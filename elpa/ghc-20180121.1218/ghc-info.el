;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-info.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 15, 2010
;; License: BSD-3-clause

;;; Code:

(require 'ghc-func)
(require 'ghc-process)

(defun ghc-show-info (&optional ask)
  (interactive "P")
  (let* ((expr0 (ghc-things-at-point))
	 (expr (if (or ask (not expr0)) (ghc-read-expression expr0) expr0))
	 (info (ghc-get-info expr)))
    (when info
      (ghc-display
       nil
       (lambda () (insert info))))))

(defun ghc-get-info (expr)
  (let* ((file (buffer-file-name))
	 (cmd (format "info %s %s\n" file expr)))
    (ghc-sync-process cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; type
;;;

(defvar ghc-type-overlay nil)

(make-variable-buffer-local 'ghc-type-overlay)

(defun ghc-type-set-ix (n)
  (overlay-put ghc-type-overlay 'ix n))

(defun ghc-type-get-ix ()
  (overlay-get ghc-type-overlay 'ix))

(defun ghc-type-set-point (pos)
  (overlay-put ghc-type-overlay 'pos pos))

(defun ghc-type-get-point ()
  (overlay-get ghc-type-overlay 'pos))

(defun ghc-type-set-types (types)
  (overlay-put ghc-type-overlay 'types types))

(defun ghc-type-get-types ()
  (overlay-get ghc-type-overlay 'types))

(ghc-defstruct tinfo beg-line beg-column end-line end-column info)

(defun ghc-type-init ()
  (setq ghc-type-overlay (make-overlay 0 0))
  (overlay-put ghc-type-overlay 'face 'region)
  (ghc-type-clear-overlay)
  (setq after-change-functions
	(cons 'ghc-type-clear-overlay after-change-functions))
  (add-hook 'post-command-hook 'ghc-type-post-command-hook))

(defun ghc-type-clear-overlay (&optional _beg _end _len)
  (when (overlayp ghc-type-overlay)
    (ghc-type-set-ix 0)
    (ghc-type-set-point 0)
    (move-overlay ghc-type-overlay 0 0)))

(defun ghc-type-post-command-hook ()
  (when (and (eq major-mode 'haskell-mode)
	     (overlayp ghc-type-overlay)
	     (/= (ghc-type-get-point) (point)))
    (ghc-type-clear-overlay)))

(defun ghc-show-type ()
  (interactive)
  (let ((buf (current-buffer))
	(tinfos (ghc-type-get-tinfos)))
    (if (null tinfos)
	(progn
	  (ghc-type-clear-overlay)
	  (message "Cannot determine type"))
      (let* ((tinfo (nth (ghc-type-get-ix) tinfos))
	     (type (ghc-tinfo-get-info tinfo))
	     (beg-line (ghc-tinfo-get-beg-line tinfo))
	     (beg-column (ghc-tinfo-get-beg-column tinfo))
	     (end-line (ghc-tinfo-get-end-line tinfo))
	     (end-column (ghc-tinfo-get-end-column tinfo))
	     (left (ghc-get-pos buf beg-line beg-column))
	     (right (ghc-get-pos buf end-line end-column)))
	(move-overlay ghc-type-overlay (- left 1) (- right 1) buf)
	(message type)))))

(defun ghc-type-get-tinfos ()
  (if (= (ghc-type-get-point) (point))
      (ghc-type-set-ix
       (mod (1+ (ghc-type-get-ix)) (length (ghc-type-get-types))))
    (let ((types (ghc-type-obtain-tinfos)))
      (if (not (listp types)) ;; main does not exist in Main
	  (ghc-type-set-types nil)
	(ghc-type-set-types types)
	(ghc-type-set-point (point))
	(ghc-type-set-ix 0))))
  (ghc-type-get-types))

(defun ghc-type-obtain-tinfos ()
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (1+ (current-column))))
	 (file (buffer-file-name))
	 (cmd (format "type %s %s %s\n" file ln cn)))
    (ghc-sync-process cmd nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expanding Template Haskell
;;;

(defun ghc-expand-th ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (cmds (list "-b" "\n" "expand" file))
	 (source (ghc-run-ghc-mod cmds)))
    (when source
      (ghc-display
       'fontify
       (lambda () (insert source))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun ghc-get-pos (buf line col)
  (save-excursion
    (set-buffer buf)
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)
    (point)))

(defun ghc-read-expression (default)
  (if default
      (let ((prompt (format "Expression (%s): " default)))
	(read-string prompt default nil))
    (read-string "Expression: ")))

(defun ghc-find-module-name ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^module[ ]+\\([^ \n]+\\)" nil t)
	(match-string-no-properties 1))))

(provide 'ghc-info)
