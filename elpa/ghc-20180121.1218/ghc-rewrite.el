;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-rewrite.el
;;;

;; Author:  Alejandro Serrano <trupill@gmail.com>
;; Created: Jun 17, 2014
;; License: BSD-3-clause

;;; Code:

(require 'ghc-func)
(require 'ghc-process)
(require 'button)
;(require 'dropdown-list)

(defvar ghc-auto-info nil)
(defvar ghc-auto-buffer nil)

;; Common code for case splitting and refinement

(defun ghc-perform-rewriting (info)
  "Replace code with new string obtained from ghc-mod"
  (let* ((current-line    (line-number-at-pos))
	 (begin-line      (ghc-sinfo-get-beg-line info))
	 (begin-line-diff (+ 1 (- begin-line current-line)))
	 (begin-line-pos  (line-beginning-position begin-line-diff))
	 (begin-pos       (- (+ begin-line-pos (ghc-sinfo-get-beg-column info)) 1))
	 (end-line        (ghc-sinfo-get-end-line info))
	 (end-line-diff   (+ 1 (- end-line current-line)))
	 (end-line-pos    (line-beginning-position end-line-diff))
	 (end-pos         (- (+ end-line-pos (ghc-sinfo-get-end-column info)) 1)) )
    (delete-region begin-pos end-pos)
    (insert (ghc-sinfo-get-info info)) )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Case splitting
;;;

(ghc-defstruct sinfo beg-line beg-column end-line end-column info)

(defun ghc-case-split ()
  "Split the variable at point into its possible constructors"
  (interactive)
  (when (null (ghc-try-case-split))
        (message "Cannot split into cases")))

(defun ghc-try-case-split ()
  (let ((info (ghc-obtain-case-split)))
    (if (null info)
	'()
        (ghc-perform-rewriting info)) ))

(defun ghc-obtain-case-split ()
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (1+ (current-column))))
	 (file (buffer-file-name))
	 (cmd (format "split %s %s %s\n" file ln cn)))
    (ghc-sync-process cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refinement
;;;

(defun ghc-refine ()
  "Refine a hole using a user-specified function"
  (interactive)
  (when (null (ghc-try-refine))
        (message "Cannot refine")))

(defun ghc-try-refine ()
  (let ((info (ghc-obtain-refine (read-string "Refine with: "))))
    (if (null info)
	'()
        (ghc-perform-rewriting info)) ))

(defun ghc-obtain-refine (expr)
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (1+ (current-column))))
	 (file (buffer-file-name))
	 (cmd (format "refine %s %s %s %s\n" file ln cn expr)))
    (ghc-sync-process cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auto
;;;

(defun ghc-perform-rewriting-auto (info msg)
  "Replace code with new string obtained from ghc-mod from auto mode"
  (let* ((current-line    (line-number-at-pos))
	 (begin-line      (ghc-sinfo-get-beg-line info))
	 (begin-line-diff (+ 1 (- begin-line current-line)))
	 (begin-line-pos  (line-beginning-position begin-line-diff))
	 (begin-pos       (- (+ begin-line-pos (ghc-sinfo-get-beg-column info)) 1))
	 (end-line        (ghc-sinfo-get-end-line info))
	 (end-line-diff   (+ 1 (- end-line current-line)))
	 (end-line-pos    (line-beginning-position end-line-diff))
	 (end-pos         (- (+ end-line-pos (ghc-sinfo-get-end-column info)) 1)) )
    (delete-region begin-pos end-pos)
    (insert msg)))

;; Option 1: using button

(defun ghc-auto-completion-window ()
   (get-buffer-window ghc-error-buffer-name 0))

(defun auto-button (button)
  (let ((text (buffer-substring (button-start button) (button-end button))))
    (with-current-buffer ghc-auto-buffer
       (ghc-perform-rewriting-auto ghc-auto-info text))
    (quit-restore-window)))

(define-button-type 'auto-button
  'follow-link t
  'help-echo "mouse-2, RET: Insert this completion"
  'action #'auto-button)

(defun ghc-show-auto-messages (info)
  (let ((buf (current-buffer)))
    (setq ghc-auto-info   info)
    (setq ghc-auto-buffer buf)
    (ghc-display nil
      (lambda ()
        (insert "Possible completions:\n")
        (mapc
          (lambda (_x)
            (let ((pos-begin (point))
                  (pos-end   (point)))
              (make-button pos-begin pos-end :type 'auto-button)))
          (ghc-sinfo-get-info info))))
    (select-window (ghc-auto-completion-window))))

;; Option 2: using dropdown-list

;; (defun ghc-show-auto-messages (info)
;;   (let* ((completions (ghc-sinfo-get-info info))
;;          (selected    (dropdown-list completions)))
;;     (when selected
;;           (ghc-perform-rewriting-auto info (nth selected completions)))))

;; Option 3: using minibuffer

;; (defvar ghc-auto-completion-buffer-name "*Djinn Completions*")

;; (defun ghc-auto-completion-window ()
;;   (get-buffer-window ghc-auto-completion-buffer-name 0))

;; (defun ghc-show-auto-messages (info)
;;   (let* ((completions (ghc-sinfo-get-info info))
;;          (buf         (generate-new-buffer "djinn-completion-temp")))
;;     (with-current-buffer
;;       (progn
;;         (with-output-to-temp-buffer ghc-auto-completion-buffer-name
;;           (display-completion-list completions))
;;         (select-window (ghc-auto-completion-window))
;;         (buffer-string)))))

(defun ghc-auto ()
  "Try to automatically fill the contents of a hole"
  (interactive)
  (let ((info (ghc-obtain-auto)))
    (if (null info)
	(message "No automatic completions found")
        (if (= (length (ghc-sinfo-get-info info)) 1)
            (ghc-perform-rewriting-auto info (car (ghc-sinfo-get-info info)))
            (ghc-show-auto-messages info)))))

(defun ghc-obtain-auto ()
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (1+ (current-column))))
	 (file (buffer-file-name))
	 (cmd (format "auto %s %s %s\n" file ln cn)))
    (ghc-sync-process cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initial code from signature
;;;

(ghc-defstruct icsinfo sort pos fns)

(defun ghc-initial-code-from-signature ()
  "Refine a hole using a user-specified function"
  (interactive)
  (when (null (ghc-try-initial-code-from-signature))
        (message "Cannot obtain initial code")))

(defun ghc-try-initial-code-from-signature ()
  "Include initial code from a function signature or instance declaration"
  (interactive)
  (let ((info (ghc-obtain-initial-code-from-signature)))
    (if (null info)
	'()
	(let* ((ln-current (line-number-at-pos))
	       (sort (ghc-icsinfo-get-sort info))
	       (pos (ghc-icsinfo-get-pos info))
	       (ln-end (ghc-sinfo-get-end-line pos))
	       (ln-diff (+ 1 (- ln-end ln-current)))
	       (fns-to-insert (ghc-icsinfo-get-fns info)))
	  (goto-char (line-end-position ln-diff))
	  (dolist (fn-to-insert fns-to-insert)
	    (if (equal sort "function")
		(newline)
	        (newline-and-indent))
	    (insert fn-to-insert))))))

(defun ghc-obtain-initial-code-from-signature ()
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (1+ (current-column))))
	 (file (buffer-file-name))
	 (cmd (format "sig %s %s %s\n" file ln cn)))
    (ghc-sync-process cmd)))

(provide 'ghc-rewrite)
