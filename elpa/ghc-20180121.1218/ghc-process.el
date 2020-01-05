;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-process.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  9, 2014
;; License: BSD-3-clause

;;; Code:

(require 'ghc-func)

(defvar ghc-debug-options nil)
;; (setq ghc-debug-options '("-v9"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-process-running nil)
(defvar ghc-process-file-mapping nil)

(defvar-local ghc-process-process-name nil)
(defvar-local ghc-process-original-buffer nil)
(defvar-local ghc-process-original-file nil)
(defvar-local ghc-process-root nil)

(defvar ghc-command "ghc-mod")

(defvar ghc-report-errors t "Report GHC errors to *GHC Error* buffer")
(defvar ghc-error-buffer "*GHC Error*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-get-project-root ()
  (ghc-run-ghc-mod '("root")))

(defun ghc-with-process (cmd async-after-callback &optional sync-before-hook)
  (unless ghc-process-process-name
    (setq ghc-process-process-name (ghc-get-project-root)))
  (when (and ghc-process-process-name (not ghc-process-running))
    (setq ghc-process-running t)
    (if sync-before-hook (funcall sync-before-hook))
    (let* ((cbuf (current-buffer))
	   (name ghc-process-process-name)
	   (root (file-name-as-directory ghc-process-process-name))
	   (buf (get-buffer-create (concat " ghc-mod:" name)))
	   (file (buffer-file-name))
	   (cpro (get-process name)))
      ;; setting root in the original buffer, sigh
      (setq ghc-process-root root)
      (ghc-with-current-buffer buf
        (setq ghc-process-original-buffer cbuf)
	(setq ghc-process-original-file file)
	(setq ghc-process-root root)
	(let ((pro (ghc-get-process cpro name buf root))
	      (map-cmd (format "map-file %s\n" file)))
;	       (unmap-cmd (format "unmap-file %s\n" file)))
	  (when (buffer-modified-p cbuf)
	    (setq ghc-process-file-mapping t)
	    (setq ghc-process-async-after-callback nil)
	    (erase-buffer)
	    (when ghc-debug
	      (ghc-with-debug-buffer
	       (insert (format "%% %s" map-cmd))
	       (insert "CONTENTS + EOT\n")))
	    (process-send-string pro map-cmd)
	    (with-current-buffer cbuf
	      (save-restriction
		(widen)
		(process-send-region pro (point-min) (point-max))))
	    (process-send-string pro "\n\004\n")
	    (condition-case nil
		(let ((inhibit-quit nil))
		  (while ghc-process-file-mapping
		    (accept-process-output pro 0.1 nil t)))
	      (quit
	       (setq ghc-process-running nil)
	       (setq ghc-process-file-mapping nil))))
	  ;; command
	  (setq ghc-process-async-after-callback async-after-callback)
	  (erase-buffer)
	  (when ghc-debug
	    (ghc-with-debug-buffer
	     (insert (format "%% %s" cmd))))
	  (process-send-string pro cmd)

	  ;;; this needs to be done asyncrounously after the command actually
	  ;;; finished, gah
	  ;; (when do-map-file
	  ;;   (when ghc-debug
	  ;;	 (ghc-with-debug-buffer
	  ;;	  (insert (format "%% %s" unmap-cmd))))
	  ;;   (process-send-string pro unmap-cmd))

	  pro)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-get-process (cpro name buf root)
  (cond
   ((not cpro)
    (ghc-start-process name buf root))
   ((not (eq (process-status cpro) 'run))
    (delete-process cpro)
    (ghc-start-process name buf root))
   (t cpro)))

(defun ghc-start-process (name buf root)
  (let* ((default-directory root)
	 (process-connection-type nil) ;; using PIPE due to ^D
	 (opts (append ghc-debug-options
		       '("-b" "\n" "-l" "--line-prefix=O: ,E: ")
		       (ghc-make-ghc-options)
		       '("legacy-interactive")))
	 (pro (apply 'start-process name buf ghc-command opts)))
    (set-process-filter pro 'ghc-process-filter)
    (set-process-sentinel pro 'ghc-process-sentinel)
    (set-process-query-on-exit-flag pro nil)
    pro))

(defun ghc-process-filter (process string)
  (let* ((pbuf (process-buffer process))
	 (tbufname (concat " tmp " (buffer-name pbuf)))
	 tbuf)
    (if (not (get-buffer pbuf))
	(setq ghc-process-running nil) ;; just in case
      (ghc-with-current-buffer pbuf
	(when ghc-debug
	  (ghc-with-debug-buffer
	   (insert string)))
	(with-current-buffer (get-buffer-create tbufname)
	  (setq tbuf (current-buffer))
	  (goto-char (point-max))
	  (insert string)
	  (goto-char (point-min))
	  (let ((cont t) end out)
	    (while (and cont (not (eobp)) ghc-process-running)
	      (cond
	       ((looking-at "^O: ")
		(setq out t))
	       ((looking-at "^E: ")
		(setq out nil))
	       (t
		(setq cont nil)))
	      (when cont
		(forward-line)
		(unless (bolp) (setq cont nil)))
	      (when cont
		(delete-region 1 4)
		(setq end (point))
		(if out
		    (with-current-buffer pbuf
		      (goto-char (point-max))
		      (insert-buffer-substring tbuf 1 end))
		  (when ghc-report-errors
                    (with-current-buffer (get-buffer-create ghc-error-buffer)
		    (setq buffer-read-only t)
		    (let* ((buffer-read-only nil)
			   (inhibit-read-only t)
			   (cbuf (current-buffer))
			   cwin)
		      (unless (get-buffer-window cbuf) (display-buffer cbuf))
		      (setq cwin (get-buffer-window cbuf))
		      (with-selected-window cwin
			(goto-char (point-max))
			(insert-buffer-substring tbuf 1 end)
			(set-buffer-modified-p nil))
		      (redisplay)))))
		(delete-region 1 end)))))
	(goto-char (point-max))
	(forward-line -1)
	(cond
	 ((looking-at "^OK$")
	  (delete-region (point) (point-max))
	  (setq ghc-process-file-mapping nil)
	  (when ghc-process-async-after-callback
	    (goto-char (point-min))
	    (funcall ghc-process-async-after-callback 'ok)
	    (setq ghc-process-running nil)))
	 ((looking-at "^NG ")
	  (funcall ghc-process-async-after-callback 'ng)
	  (setq ghc-process-running nil)))))))

(defun ghc-process-sentinel (_process _event)
  (setq ghc-process-running nil)
  (setq ghc-process-file-mapping nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-process-rendezvous nil)
(defvar ghc-process-num-of-results nil)
(defvar ghc-process-results nil)

(defun ghc-sync-process (cmd &optional n)
  (unless ghc-process-running
    (setq ghc-process-rendezvous nil)
    (setq ghc-process-results nil)
    (setq ghc-process-num-of-results (or n 1))
    (let ((pro (ghc-with-process cmd 'ghc-sync-process-callback nil)))
      ;; ghc-process-running is now t.
      ;; But if the process exits abnormally, it is set to nil.
      (condition-case nil
	  (let ((inhibit-quit nil))
	    (while (and (null ghc-process-rendezvous) ghc-process-running)
	      (accept-process-output pro 0.1 nil t)))
	(quit
	 (setq ghc-process-running nil))))
    ghc-process-results))

(defun ghc-sync-process-callback (status)
  (cond
   ((eq status 'ok)
    (let* ((n ghc-process-num-of-results)
	   (ret (if (= n 1)
		    (ghc-read-lisp-this-buffer)
		  (ghc-read-lisp-list-this-buffer n))))
      (setq ghc-process-results ret)))
   (t
    (setq ghc-process-results nil)))
  (setq ghc-process-num-of-results nil)
  (setq ghc-process-rendezvous t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-kill-process ()
  (interactive)
  (when (eq major-mode 'haskell-mode)
    (let* ((name ghc-process-process-name)
	   (cpro (if name (get-process name))))
      (if (not cpro)
	  (message "No ghc-mod process")
	(delete-process cpro)
	(message "ghc-mod process was killed")))))

(provide 'ghc-process)
