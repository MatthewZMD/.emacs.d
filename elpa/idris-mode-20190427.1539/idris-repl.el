;;; idris-repl.el --- Run an Idris interpreter using S-Expression communication protocol.-*- lexical-binding: t -*-

;; Copyright (C) 2013 Hannes Mehnert and David Raymond Christiansen

;; Author: Hannes Mehnert <hannes@mehnert.org>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)
(require 'prop-menu)

(require 'idris-core)
(require 'idris-settings)
(require 'inferior-idris)
(require 'idris-common-utils)
(require 'idris-prover)
(require 'idris-highlight-input)

(eval-when-compile (require 'cl))


(defvar idris-prompt-string "Idris"
  "The prompt shown in the REPL.")

(defvar idris-repl-buffer-name (idris-buffer-name :repl)
  "The name of the Idris REPL buffer.")

(defvar-local idris-prompt-start nil
  "Marker for the start of the Idris prompt.")

(defvar-local idris-input-start nil
  "Marker for the start of user input for Idris.")

(defun idris-repl-welcome-message ()
  "The message to display as part of the Idris banner, if applicable."
  "Welcome to the Idris REPL!")

(defun idris-repl-get-logo ()
  "Return the path to the Idris logo if it exists, or `nil' if not."
  (let ((logo-path (concat idris-mode-path "logo-small.png")))
    (if (file-readable-p logo-path)
        logo-path
      nil)))

(defun idris-repl-insert-logo ()
  "Attempt to insert a graphical logo.
Returns non-`nil' on success, `nil' on failure."
  (let ((logo (idris-repl-get-logo)))
    (if (and (display-graphic-p)
             (image-type-available-p 'png)
             logo)
        (progn (insert-image (create-image logo)
                             (idris-repl-welcome-message))
               t)
      nil)))

(defun idris-repl-animate-banner ()
  "Insert a text banner using animation.
Returns non-`nil' on success, `nil' on failure."
  (animate-string (idris-repl-welcome-message) 0 0)
  t)

(defun idris-repl-text-banner ()
  "Insert a text banner with no animation.
Returns non-`nil' on success, `nil' on failure."
  (insert (idris-repl-welcome-message))
  t)

(defun idris-repl-insert-banner ()
  "Insert Idris banner into buffer."
  (when (zerop (buffer-size))
    ;; If a banner is inserted, add a newline too
    (when (run-hook-with-args-until-success 'idris-repl-banner-functions)
      (insert "\n"))
    (let ((version-string (idris-get-idris-version-string)))
      (when (and idris-repl-show-idris-version
                 version-string)
        (insert (propertize (concat "Idris " version-string)
                            'face 'italic)
                "\n\n")))))

(defun idris-repl-insert-prompt (&optional always-insert)
  "Insert or update Idris prompt in buffer.
If ALWAYS-INSERT is non-nil, always insert a prompt at the end of the buffer."
  ;; Put the prompt at the end, if no active prompt is present.
  (when always-insert
    (set-marker idris-prompt-start (point-max))
    (set-marker idris-input-start (point-max)))
  (goto-char idris-prompt-start)
  (let ((inhibit-read-only 'idris-repl-prompt))
    (delete-region idris-prompt-start idris-input-start))
  (unless (bolp) (insert "\n"))
  (let ((prompt (if (and (equal idris-repl-prompt-style 'short)
                         (not idris-prover-currently-proving))
                    "λΠ> "
                  (format "%s> " idris-prompt-string))))
    (set-marker idris-prompt-start (point))
    (idris-propertize-region
        `(face idris-repl-prompt-face
               read-only idris-repl-prompt
               intangible t
               idris-repl-prompt t
               help-echo ,idris-prompt-string
               rear-nonsticky (idris-repl-prompt read-only face intangible))
      (let ((inhibit-read-only t))
        (insert prompt)))
    (set-marker idris-input-start (point-max))
    (goto-char idris-input-start)))


(defun idris-repl-update-prompt (new-prompt)
  "Update prompt string to NEW-PROMPT."
  (unless (equal idris-prompt-string new-prompt)
    (setq idris-prompt-string new-prompt)
    (with-current-buffer (idris-repl-buffer)
      (idris-repl-insert-prompt))))


(defun idris-repl-buffer ()
  "Return or create the Idris REPL buffer."
  (or (get-buffer idris-repl-buffer-name)
      (let ((buffer (get-buffer-create idris-repl-buffer-name)))
        (save-selected-window
          (when idris-repl-show-repl-on-startup
            (pop-to-buffer buffer t))
          (with-current-buffer buffer
            (idris-repl-mode)
            (idris-repl-buffer-init))
          buffer))))

(defun idris-repl-clear-buffer ()
  "Clear prior output from the Idris REPL buffer."
  (interactive)
  (with-current-buffer (idris-repl-buffer)
    (let ((inhibit-read-only t)
          (current-input (idris-repl-current-input)))
      (erase-buffer)
      (idris-repl-insert-prompt)
      (insert current-input))))

(defun idris-switch-to-output-buffer ()
  "Select the output buffer and scroll to bottom."
  (interactive)
  (pop-to-buffer (idris-repl-buffer))
  (goto-char (point-max)))

;;;###autoload
(defun idris-repl ()
  (interactive)
  (idris-run)
  (idris-switch-to-output-buffer))

(defvar idris-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>") 'idris-repl-return)
    ;; (define-key map (kbd "<TAB>") ...) makes the debugger complain, and
    ;; suggests this method of binding instead.
    (define-key map "\t" 'completion-at-point)
    (define-key map (kbd "<home>") 'idris-repl-begin-of-prompt)
    (define-key map (kbd "C-a") 'idris-repl-begin-of-prompt)
    (define-key map (kbd "M-p") 'idris-repl-backward-history)
    (define-key map (kbd "<C-up>") 'idris-repl-backward-history)
    (define-key map (kbd "M-n") 'idris-repl-forward-history)
    (define-key map (kbd "<C-down>") 'idris-repl-forward-history)
    (define-key map (kbd "C-c M-o") 'idris-repl-clear-buffer)
    (cl-loop for keyer
             in '(idris-define-docs-keys
                  idris-define-general-keys
                  idris-define-active-term-keys)
             do (funcall keyer map))
    map)
  "Keymap used in Idris REPL mode.")

(easy-menu-define idris-repl-mode-menu idris-repl-mode-map
  "Menu for the Idris REPL mode"
  `("Idris REPL"
    ("Interpreter options" :active idris-process
     ["Show implicits" (idris-set-option :show-implicits t)
      :visible (not (idris-get-option :show-implicits))]
     ["Hide implicits" (idris-set-option :show-implicits nil)
      :visible (idris-get-option :show-implicits)]
     ["Show error context" (idris-set-option :error-context t)
      :visible (not (idris-get-option :error-context))]
     ["Hide error context" (idris-set-option :error-context nil)
      :visible (idris-get-option :error-context)])
    ["Show term interaction widgets" idris-add-term-widgets t]
    ["Customize idris-mode" (customize-group 'idris) t]
    ["Quit inferior idris process" idris-quit t]
    ))

(define-derived-mode idris-repl-mode fundamental-mode "Idris-REPL"
  "Major mode for interacting with Idris.
    \\{idris-repl-mode-map}
Invokes `idris-repl-mode-hook'."
                                        ;syntax-table?
  :group 'idris-repl
  (set (make-local-variable 'indent-tabs-mode) nil)
  (add-hook 'idris-event-hooks 'idris-repl-event-hook-function)
  (add-hook 'kill-buffer-hook 'idris-repl-remove-event-hook-function nil t)
  (when idris-repl-history-file
    (idris-repl-safe-load-history)
    (add-hook 'kill-buffer-hook
              'idris-repl-safe-save-history nil t))
  (add-hook 'kill-emacs-hook 'idris-repl-save-all-histories)
  (set (make-local-variable 'completion-at-point-functions) '(idris-repl-complete))
  (setq mode-name `("Idris-REPL" (:eval (if idris-rex-continuations "!" ""))))
  (set (make-local-variable 'prop-menu-item-functions)
       '(idris-context-menu-items)))

(defun idris-repl-remove-event-hook-function ()
  (setq idris-prompt-string "Idris")
  (remove-hook 'idris-event-hooks 'idris-repl-event-hook-function))

(defun idris-repl-event-hook-function (event)
  (pcase event
    (`(:write-string ,output ,_target)
     (idris-repl-write-string output)
     t)
    (`(:set-prompt ,prompt ,_target)
     (idris-repl-update-prompt prompt)
     t)
    (`(:warning ,output ,_target)
     (when (member 'warnings-repl idris-warnings-printing)
       (idris-repl-write-string (format "Error: %s line %d (col %d):\n%s" (nth 0 output) (nth 1 output) (if (eq (safe-length output) 3) 0 (nth 2 output)) (car (last output))))))
    (`(:run-program ,file ,_target)
     (idris-execute-compiled-program file))
    (_ nil)))

(defun idris-execute-compiled-program (filename)
  (let* ((name (concat "idris-" filename))
         (buffer (make-comint-in-buffer name nil filename)))
    (pop-to-buffer buffer)))

(defun idris-repl-update-banner ()
  (idris-repl-insert-banner)
  (goto-char (point-max))
  (idris-repl-insert-prompt t))

(defun idris-repl-buffer-init ()
  (dolist (markname '(idris-prompt-start
                      idris-input-start))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point)))
  (idris-repl-update-banner))

(defun idris-repl-return ()
  "Send command over to Idris."
  (interactive)
  (goto-char (point-max))
  (let ((end (point)))
    (idris-repl-add-to-input-history (buffer-substring idris-input-start end))
    (let ((overlay (make-overlay idris-input-start end)))
      (overlay-put overlay 'face 'idris-repl-input-face)))
  (let ((input (idris-repl-current-input))
        (input-start (marker-position idris-input-start)))
    (goto-char (point-max))
    (if (string-match-p "^\\s-*$" input)
        (delete-region (point) idris-input-start)
      (insert "\n")
      (set-marker idris-prompt-start (point))
      (set-marker idris-input-start (point))
      (idris-repl-eval-string input input-start))))

(defun idris-repl-complete ()
  "Completion of the current input"
  (let* ((input (idris-repl-current-input))
         (result (idris-eval `(:repl-completions ,input))))
    (cl-destructuring-bind (completions partial) (car result)
      (if (null completions)
          nil
        (list (+ idris-input-start (length partial)) (point-max) completions)))))

(defun find-common-prefix (input slist)
  "Finds longest common prefix of all strings in list."
  (let ((first (car slist))
        (ilen (length input)))
    (if (> (length first) ilen)
        (progn
          (let ((next (substring first 0 (1+ ilen))))
            (if (cl-every (lambda (p) (string-prefix-p next p)) slist)
                (find-common-prefix next slist)
              input)))
      input)))

(defun idris-repl-begin-of-prompt ()
  "Got to the beginning of linke or the prompt."
  (interactive)
  (cond ((and (>= (point) idris-input-start)
              (idris-same-line-p (point) idris-input-start))
         (goto-char idris-input-start))
        (t (beginning-of-line 1))))

(defun idris-repl-current-input ()
  "Return the current input as string."
  (buffer-substring-no-properties idris-input-start (point-max)))

(defun idris-repl-highlight-input (start-pos start-line start-col end-line end-col props)
  "Apply semantic highlighting to the REPL input beginning at START-POS using the Idris location information START-LINE, START-COL, END-LINE, and END-COL and semantic annotations PROPS."
  (let ((buffer (get-buffer (idris-buffer-name :repl))))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let* ((input-line (save-excursion
                             (goto-char start-pos)
                             (beginning-of-line)
                             (count-lines (point-min) start-pos)))
               (input-col (save-excursion
                            (goto-char start-pos)
                            (current-column)))
               (start-line-repl (+ input-line start-line -1))
               (start-col-repl (+ input-col start-col))
               (end-line-repl (+ input-line end-line -1))
               (end-col-repl (+ input-col end-col)))
          (idris-highlight-input-region buffer
                                        start-line-repl start-col-repl
                                        end-line-repl end-col-repl
                                        props))))))

(defun idris-repl-eval-string (string start)
  "Evaluate STRING on the inferior Idris, where input was at position START."
  (idris-rex (start) (list :interpret string) t
    ((:ok result &optional spans)
     (pcase result
       (`(:highlight-source ,hs) ;; Semantic highlighting
        (when start
          (dolist (h hs)
            ;; Compute positions relative to the input start for
            ;; semantic highlighting
            (pcase h
              (`(((:filename ,_fn)
                  (:start ,start-line ,start-col)
                  (:end ,end-line ,end-col))
                 ,props)
               (idris-repl-highlight-input
                start start-line start-col end-line end-col props))))))
       (_ (idris-repl-insert-result result spans)))) ;; The actual result
    ((:error condition &optional spans)
     (idris-repl-show-abort condition spans))))

(defun idris-repl-show-abort (condition &optional highlighting)
  (with-current-buffer (idris-repl-buffer)
    (save-excursion
      (goto-char idris-prompt-start)
      (idris-propertize-spans (idris-repl-semantic-text-props highlighting)
        (insert-before-markers condition)))
    (idris-repl-insert-prompt)
    (idris-repl-show-maximum-output)))


(defun idris-repl-write-string (string)
  "Append STRING to output."
  (with-current-buffer (idris-repl-buffer)
    (save-excursion
      (goto-char idris-prompt-start)
      (idris-propertize-region
          `(face idris-repl-output-face
            read-only idris-repl-output
            rear-nonsticky (face read-only))
        (insert-before-markers string))
      (when (and (= (point) idris-prompt-start)
                 (not (bolp)))
        (insert-before-markers "\n")))
    (idris-repl-insert-prompt)
    (idris-repl-show-maximum-output)))


(defun idris-repl-insert-result (string &optional highlighting)
  "Insert STRING and mark it asg evaluation result.
Optional argument HIGHLIGHTING is a collection of semantic
highlighting information from Idris."
  (with-current-buffer (idris-repl-buffer)
    (save-excursion
      (goto-char (point-max))
      (when (and (not (bolp))
                 (not (string-equal string "")))
        (insert-before-markers "\n"))
      (idris-propertize-region '(read-only idris-repl-output
                                 rear-nonsticky (face read-only))
        (idris-propertize-spans (idris-repl-semantic-text-props highlighting)
          (idris-propertize-region
              '(face idris-repl-result-face
                rear-nonsticky (face))
            (insert-before-markers string)))))
    (idris-repl-insert-prompt)
    (idris-repl-show-maximum-output)))


(defun idris-repl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (if (eq (window-buffer) (current-buffer))
                   (selected-window)
                 (get-buffer-window (current-buffer) t))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1)
          (goto-char idris-input-start))))))


;;; history

(defvar-local idris-repl-input-history '()
  "History list of strings entered into the REPL buffer.")

(defun idris-repl-add-to-input-history (string)
  "Adds input to history."
  (unless (equal string "")
    (setq idris-repl-input-history
          (remove string idris-repl-input-history)))
  (unless (equal string (car idris-repl-input-history))
      (push string idris-repl-input-history)))

(defvar-local idris-repl-input-history-position -1
  "Newer items have smaller indices.")

(defun idris-repl-delete-current-input ()
  "Delete all text from the prompt."
  (interactive)
  (delete-region idris-input-start (point-max)))

(defun idris-repl-replace-input (string)
  (idris-repl-delete-current-input)
  (insert-and-inherit string))

(defun idris-repl-history-replace (direction)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list)."
  (let* ((min-pos -1)
         (max-pos (length idris-repl-input-history))
         (prefix (idris-repl-history-prefix))
         (pos0 (if (idris-repl-history-search-in-progress-p)
                   idris-repl-input-history-position
                 min-pos))
         (pos (idris-repl-position-in-history pos0 direction prefix))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (idris-repl-replace-input (nth pos idris-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          (t
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (message "%s (prefix is: %s)" msg prefix)
    (setq idris-repl-input-history-position pos)
    (setq this-command 'idris-repl-history-replace)))

(defvar-local idris-repl-history-prefix-data ""
  "Current history prefix.")

(defun idris-repl-history-prefix ()
  "Return the prefix we want to look for in the history."
  (if (idris-repl-history-search-in-progress-p)
      idris-repl-history-prefix-data
    (setq idris-repl-history-prefix-data (idris-repl-current-input))
    idris-repl-history-prefix-data))

(defun idris-repl-history-search-in-progress-p ()
  (eq last-command 'idris-repl-history-replace))

(defun idris-repl-position-in-history (start-pos direction prefix)
  "Return the position of the history item matching the PREFIX.
Return -1 resp. the length of the history if no item matches."
  ;; Loop through the history list looking for a matching line
  (let* ((step (cl-ecase direction
                 (forward -1)
                 (backward 1)))
         (history idris-repl-input-history)
         (len (length history)))
    (cl-loop for pos = (+ start-pos step) then (+ pos step)
             if (< pos 0) return -1
             if (<= len pos) return len
             for history-item = (nth pos history)
             if (string-prefix-p prefix history-item)
             return pos)))

(defun idris-repl-backward-history ()
  "Cycle backward through history."
  (interactive)
  (idris-repl-history-replace 'backward))

(defun idris-repl-forward-history ()
  "Cycle forward through history."
  (interactive)
  (idris-repl-history-replace 'forward))


;; persistent history
(defun idris-repl-save-all-histories ()
  "Save the history in each repl buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'idris-repl-mode)
        (idris-repl-safe-save-history)))))

(defun idris-repl-safe-save-history ()
  (idris-repl-call-with-handler
   #'idris-repl-save-history
   "%S while saving the history. Continue? "))

(defun idris-repl-safe-load-history ()
  (idris-repl-call-with-handler
   #'idris-repl-load-history
   "%S while loading the history. Continue? "))

(defun idris-repl-call-with-handler (fun query)
  "Call FUN in the context of an error handler.
The handler will use qeuery to ask the use if the error should be ingored."
  (condition-case err
      (funcall fun)
    (error
     (if (y-or-n-p (format query (error-message-string err)))
         nil
       (signal (car err) (cdr err))))))

(defun idris-repl-read-history-filename ()
  (read-file-name "Use Idris REPL history from file: "
                  idris-repl-history-file))

(defun idris-repl-load-history (&optional filename)
  "Set the current Idris REPL history.
It can be read either from FILENAME or `idris-repl-history-file' or
from a user defined filename."
  (interactive (list (idris-repl-read-history-filename)))
  (let ((file (or filename idris-repl-history-file)))
    (setq idris-repl-input-history (idris-repl-read-history file))))

(defun idris-repl-read-history (&optional filename)
  "Read and return the history from FILENAME.
The default value for FILENAME is `idris-repl-history-file'."
  (let ((file (or filename idris-repl-history-file)))
    (cond ((not (file-readable-p file)) '())
          (t (with-temp-buffer
               (insert-file-contents file)
               (read (current-buffer)))))))

(defun idris-repl-save-history (&optional filename history)
  "Simply save the current Idris REPL history to a file.
When Idris is setup to always load the old history and one uses only
one instance of idris all the time, there is no need to merge the
files and this function is sufficient."
  (interactive (list (idris-repl-read-history-filename)))
  (let ((file (or filename idris-repl-history-file))
        (hist (or history idris-repl-input-history)))
    (unless (file-writable-p file)
      (error (format "History file not writable: %s" file)))
    (let ((hist (cl-subseq hist 0 (min (length hist) idris-repl-history-size))))
      (with-temp-file file
        (let ((cs idris-repl-history-file-coding-system)
              (print-length nil) (print-level nil))
          (setq buffer-file-coding-system cs)
          (insert (format ";; -*- coding: %s -*-\n" cs))
          (insert ";; History for Idris REPL. Automatically written.\n"
                  ";; Edit only if you know what you're doing\n")
          (prin1 (mapcar #'substring-no-properties hist) (current-buffer)))))))

(provide 'idris-repl)
;;; idris-repl.el ends here
