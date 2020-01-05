;;; idris-prover.el --- Prover mode for Idris -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014, Hannes Mehnert and David Raymond Christiansen
;; Author: Hannes Mehnert <hannes@mehnert.org>, David Raymond Christiansen <david@davidchristiansen.dk>

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

(require 'idris-core)
(require 'idris-common-utils)
(require 'idris-warnings)
(require 'idris-settings)
(require 'inferior-idris)

(eval-when-compile (require 'cl))

; consisting of three buffers:
; ------------------------------
; | proof obligations          |
; |----------------------------|
; | proof shell | proof script |
; ------------------------------

(defgroup idris-prover nil "Idris Prover" :prefix 'idris :group 'idris)

(defface idris-prover-processed-face
  '((t (:background "PaleGreen1")))
  "Face for Idris proof script which is already processed."
  :group 'idris-faces)

(defface idris-prover-processing-face
  '((t (:background "gold")))
  "Face for Idris proof script which is currently processing."
  :group 'idris-faces)

(defcustom idris-prover-restore-window-configuration t
  "When non-nil, restore the window configuration after exiting
the prover."
  :type 'boolean
  :group 'idris-prover)

(defvar idris-prover-obligations-buffer-name (idris-buffer-name :proof-obligations)
  "The name of the Idris proof obligation buffer.")

(defvar idris-prover-shell-buffer-name (idris-buffer-name :proof-shell)
  "The name of the Idris proof shell buffer.")

(defvar idris-prover-script-buffer-name (idris-buffer-name :proof-script)
  "The name of the Idris proof script buffer.")

(defvar idris-prover-currently-proving nil
  "The hole that Idris has open in the interactive prover, or nil
if Idris is not proving anything.")

(defconst idris-prover-error-message-prefix "Prover error: "
  "A prefix to show on minibuffer error messages that originate
  in the prover.")

(defun idris-prover-obligations-buffer ()
  (or (get-buffer idris-prover-obligations-buffer-name)
      (let ((buffer (get-buffer-create idris-prover-obligations-buffer-name)))
        (with-current-buffer buffer
          (setq buffer-read-only t))
        buffer)))

(defun idris-prover-show-obligations ()
  (display-buffer (idris-prover-obligations-buffer)
     '(display-buffer-pop-up-window . nil)))

(defun idris-prover-write-goals (goals)
  "Write GOALS to the goal buffer.
If GOALS is a string, it is treated as undecorated text.
Otherwise, it must be a two-element list whose car is a goal
string and whose cadr is highlighting information."
  (with-current-buffer (idris-prover-obligations-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (when idris-show-help-text
        (setq header-line-format
              "This is a read-only view of your proof state. Prove the lemma in the script buffer."))
      (let ((goals-string (car goals))
            (goals-spans (cadr goals)))
        (idris-propertize-spans (idris-repl-semantic-text-props goals-spans)
          (insert goals-string)))))
  (idris-prover-show-obligations))

(defvar idris-prover-saved-window-configuration nil
  "The saved window configuration from before running the prover.")

(defvar-local idris-prover-script-processed nil
  "Marker for the processed part of proof script")

(defvar-local idris-prover-script-processed-overlay nil
  "Overlay for processed proof script")

(defvar-local idris-prover-script-processing nil
  "Marker for the processing part of proof script")

(defvar-local idris-prover-script-processing-overlay nil
  "Overlay for processing proof script")

(defvar-local idris-prover-script-warning-overlay nil
  "Overlay for warning in proof script")

; invariant: point-min <= idris-prover-script-processed <= idris-prover-script-processing <= point-max

(defvar-local idris-prover-prove-step 0
  "Step counter of the proof")

(defvar idris-prover-script-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'idris-prover-script-ret)
    (define-key map (kbd "M-n") 'idris-prover-script-forward)
    (define-key map (kbd "M-p") 'idris-prover-script-backward)
    (define-key map (kbd "C-c C-q") 'idris-prover-script-qed)
    (define-key map (kbd "C-c C-k") 'idris-prover-abandon)
    ;; Using (kbd "<TAB>") in place of "\t" makes emacs angry, and suggests
    ;; using the latter form.
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap used in Idris proof script mode.")


(defun idris-prover-complete ()
  "Completion of the current input."
  (let* ((start (save-excursion (beginning-of-line) (point)))
         (input (buffer-substring-no-properties
                 start
                 (point)))
         (result (idris-eval `(:repl-completions ,input))))
    (cl-destructuring-bind (completions _partial) (car result)
      (if (null completions)
          nil
        (list start (point) completions)))))

(defun idris-prover-find-tactic (start-pos)
  "Use some layout heuristics to find the tactic beginning at
START-POS, returning a pair consisting of the start and end
positions of the tactic. Tactics are required to begin at the
left margin."
  (let (tactic-start tactic-end)
    (save-excursion
      (goto-char start-pos)

      ;; Ensure that we're at the next line beginning
      (beginning-of-line)
      (unless (= (point) start-pos)
        (forward-line))

      ;; Go forward until the current line begins a tactic
      (while (and (not (eobp)) (not (looking-at-p "[a-zA-Z]")))
        (forward-line))

      (if (eobp) ;; if at end of buffer, no tactic to be found!
          nil
        (setq tactic-start (point))

        ;; Go forward until end of buffer or non-blank line at left margin
        (forward-line)
        (while (and (not (eobp)) (not (looking-at-p "[a-zA-Z]")))
          (forward-line))

        ;; Go backward until a non-whitespace char is found - it is the end of
        ;; the tactic
        (backward-char)
        (while (looking-at-p "\\s-\\|$") (backward-char))
        (forward-char)
        (setq tactic-end (point))

        (cons tactic-start tactic-end)))))



(defun idris-prover-script-backward ()
  "Backward one piece of proof script"
  (interactive)
  (idris-eval-async (list :interpret (if idris-enable-elab-prover ":undo" "undo"))
                    #'(lambda (_result) t)
                    #'(lambda (condition)
                        (message (concat idris-prover-error-message-prefix condition)))))

(defun idris-prover-script-forward ()
  "Forward one piece of proof script."
  (interactive)
  (when (eobp) (newline)) ;; There must be a newline at the end
  (when idris-prover-script-warning-overlay
    (delete-overlay idris-prover-script-warning-overlay)
    (setq idris-prover-script-warning-overlay nil))
  (goto-char (+ 1 idris-prover-script-processed))
  (let ((prior-processed-position (marker-position idris-prover-script-processed)) ; to restore on error
        (next-tactic (idris-prover-find-tactic
                      idris-prover-script-processed)))
    (if (null next-tactic)
        (error "At the end of the proof script")
      (let* ((tactic-start (car next-tactic))
             (tactic-end (cdr next-tactic))
             (tactic-text (buffer-substring-no-properties tactic-start
                                                          tactic-end)))
        (set-marker idris-prover-script-processed tactic-start)
        (set-marker idris-prover-script-processing tactic-end)
        (let ((overlay (make-overlay idris-prover-script-processed
                                     idris-prover-script-processing)))
          (overlay-put overlay 'face 'idris-prover-processing-face)
          (setq idris-prover-script-processing-overlay overlay))
        (with-no-warnings
          (let ((tactic-cmd (replace-regexp-in-string
                             "\\`[ \t\n]*" ""
                             ;; replace Windows newlines with a space
                             (replace-regexp-in-string "" " " tactic-text))))
            (idris-rex () (list :interpret tactic-cmd) nil
              ((:ok _result)
               (with-current-buffer (idris-prover-script-buffer)
                 (when idris-prover-script-processing-overlay
                   (delete-overlay idris-prover-script-processing-overlay)
                   (setq idris-prover-script-processing-overlay nil))
                 ;; Delete the region because it will be or has been
                 ;; written with the proof state.
                 (delete-region idris-prover-script-processed
                                idris-prover-script-processing)
                 ;; Put point at a useful spot for the next tactic
                 (when (eql (marker-position idris-prover-script-processed) (point-max))
                   (goto-char idris-prover-script-processed)
                   (let ((inhibit-read-only t)) (insert "\n")))
                 (goto-char (1+ (marker-position idris-prover-script-processed)))
                 (recenter)))
              ((:error condition &optional _spans)
               (with-current-buffer (idris-prover-script-buffer)
                 (when idris-prover-script-processing-overlay
                   (delete-overlay idris-prover-script-processing-overlay)
                   (setq idris-prover-script-processing-overlay nil))
                 (setq idris-prover-script-warning-overlay
                       (idris-warning-create-overlay idris-prover-script-processed
                                                     idris-prover-script-processing
                                                     condition))
                 ;; Restore the original position of the marker for
                 ;; the processed region to prevent Emacs and Idris
                 ;; from getting out of sync RE proof script contents
                 (set-marker idris-prover-script-processed prior-processed-position))
               (message (concat idris-prover-error-message-prefix condition))
               t))))))))

(defun idris-prover-script-ret ()
  "Insert a newline at the end of buffer, even if it's read-only."
  (interactive)
  (if (equal (point) (marker-position idris-prover-script-processed))
      (let ((inhibit-read-only t)) (insert "\n"))
    (newline)))

(defun idris-prover-script-qed ()
  "Send a QED command to Idris."
  (interactive)
  (if idris-prover-currently-proving
      (idris-eval-async (list :interpret (if idris-enable-elab-prover ":qed" "qed"))
                        #'(lambda (_result) t)
                        #'(lambda (condition)
                            (message (concat idris-prover-error-message-prefix condition))))
    (error "No proof in progress")))

(easy-menu-define idris-prover-script-mode-menu idris-prover-script-mode-map
  "Menu for Idris prover scripts"
  `("Idris Proof"
    ["Advance" idris-prover-script-forward t]
    ["Retract" idris-prover-script-backward t]
    ["QED" idris-prover-script-qed t]
    ["Abandon" idris-prover-abandon t])
  )

(define-derived-mode idris-prover-script-mode prog-mode "Idris-Proof-Script"
  "Major mode for interacting with Idris proof script.
    \\{idris-prover-script-mode-map}
Invokes `idris-prover-script-mode-hook'."
  :group 'idris-prover
  (set (make-local-variable 'completion-at-point-functions)
       '(idris-prover-complete))
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun idris-prover-script-buffer ()
  (or (get-buffer idris-prover-script-buffer-name)
      (let ((buffer (get-buffer-create idris-prover-script-buffer-name)))
        (with-current-buffer buffer
          (idris-prover-script-mode)
          (setq idris-prover-script-processing (make-marker))
          (setq idris-prover-script-processed (make-marker))
          (set-marker idris-prover-script-processing (point))
          (set-marker idris-prover-script-processed (point)))
        buffer)))

(defun idris-prover-reset-prover-script-buffer ()
  "Erase or initialize a proof script buffer, resetting all the
special prover state."
  (with-current-buffer (idris-prover-script-buffer)
    (when idris-prover-script-processed-overlay
      (delete-overlay idris-prover-script-processed-overlay)
      (setq idris-prover-script-processed-overlay nil))
    (when idris-prover-script-processing-overlay
      (delete-overlay idris-prover-script-processing-overlay)
      (setq idris-prover-script-processing-overlay nil))
    (setq idris-prover-prove-step 0)
    (erase-buffer)
    (when idris-show-help-text
      (setq header-line-format
            (let ((fwd (where-is-internal 'idris-prover-script-forward))
                  (bak (where-is-internal 'idris-prover-script-backward))
                  (qed (where-is-internal 'idris-prover-script-qed)))
              (concat " Write your proof script here."
                      (if (and fwd bak qed)
                          (format "Use %s to advance and %s to retract.  %s saves a completed proof."
                                  (key-description (car fwd))
                                  (key-description (car bak))
                                  (key-description (car qed)))
                        "")))))
    (unless idris-prover-script-processing
      (setq idris-prover-script-processing (make-marker)))
    (unless idris-prover-script-processed
      (setq idris-prover-script-processed (make-marker)))
    (set-marker idris-prover-script-processing (point))
    (set-marker idris-prover-script-processed (point))))

(defun idris-prover-write-script (script count)
  "Put the proof state recieved from Idris into the proof script buffer.
SCRIPT is the list of tactics in the proof state, and COUNT is
the length reported by Idris."
  (interactive)
  (with-current-buffer (idris-prover-script-buffer)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point-max) 'read-only nil))
    (cond ((< count idris-prover-prove-step)
           ;; this is actually the (count - 1) == Idris-prover-prove-step case!
           ;; in other words, we are undoing the final step.
           ;; can the other case(s) happen??
           (goto-char idris-prover-script-processed)
           (if (= (forward-line -1) 0)
               ;; we went back
               (end-of-line)
             ;; we did not go back - are thus at top of buffer and must
             ;; retract whole script
             (goto-char (point-min)))
           (set-marker idris-prover-script-processed (point)))
          ((> count idris-prover-prove-step)
           ;; Here we are inserting a newly-checked proof step.
           (goto-char idris-prover-script-processed)
           (while (< idris-prover-prove-step count)
             (let ((lelem (nth idris-prover-prove-step script)))
               (insert-before-markers lelem))
             (newline)
             (setq idris-prover-prove-step (1+ idris-prover-prove-step))))
          (t nil))
    (setq idris-prover-prove-step count)
    (unless (null idris-prover-script-processed-overlay)
      (delete-overlay idris-prover-script-processed-overlay))
    (let ((overlay (make-overlay 0 idris-prover-script-processed)))
      (overlay-put overlay 'face 'idris-prover-processed-face)
      (setq idris-prover-script-processed-overlay overlay))
    (let ((inhibit-read-only t))
      (put-text-property (point-min) idris-prover-script-processed 'read-only t))))

(defun idris-prover-abandon ()
  "Abandon an in-progress proof."
  (interactive)
  ;; Ask for confirmation when called interactively
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p "Abandon proof and discard script? "))
    (if idris-prover-currently-proving
        (idris-eval (list :interpret (if idris-enable-elab-prover ":abandon" "abandon")) t)
      (error "No proof in progress"))))

(defun idris-prover-end ()
  "Get rid of left over buffers from proof mode and unset global state related to the prover."
  (interactive)
  (setq idris-prover-currently-proving nil)
  (let ((obligations (idris-prover-obligations-buffer))
        (script (idris-prover-script-buffer)))
    (when obligations
      (delete-windows-on obligations)
      (kill-buffer obligations))
    (when script (kill-buffer script)))
  (when (and idris-prover-restore-window-configuration
             (window-configuration-p
              idris-prover-saved-window-configuration))
    (set-window-configuration idris-prover-saved-window-configuration))
  (setq idris-prover-saved-window-configuration nil))


(autoload 'idris-repl-write-string "idris-repl.el")
(defun idris-prover-event-hook-function (event)
  "Process an EVENT returned from Idris when the prover is running."
  (pcase event
    (`(:start-proof-mode ,name ,_target)
     (setq idris-prover-currently-proving name)
     (setq idris-prover-saved-window-configuration
           (current-window-configuration))
     (idris-prover-reset-prover-script-buffer)
     (idris-repl-write-string (format "Start proof of %s" name))
     (let* ((obligations-window (idris-prover-show-obligations))
            (script-window (split-window obligations-window)))
       (set-window-buffer script-window (idris-prover-script-buffer)))
     t)
    (`(:end-proof-mode ,msg ,_target)
     (let ((name (car msg))
           (proof (cadr msg)))
       (idris-perhaps-insert-proof-script proof)
       (idris-prover-end)
       (idris-repl-write-string (concat "End proof of " name))
       (run-hooks 'idris-prover-success-hook))
     t)
    (`(:write-proof-state ,msg ,_target)
     (cl-destructuring-bind (script count) msg
       (idris-prover-write-script script count))
     t)
    (`(:write-goal ,goal ,_target)
     (idris-prover-write-goals goal)
     t)
    (`(:abandon-proof ,_msg ,_target)
     (when (get-buffer idris-prover-script-buffer-name)
       (with-current-buffer idris-prover-script-buffer-name
         (copy-region-as-kill (point-min) (point-max))
         (message "Proof saved to kill ring")))
     (idris-prover-end)
     (idris-repl-write-string "Abandoned proof")
     t)
    (_ nil)))

(defcustom idris-prover-success-hook '(idris-list-holes-on-load)
  "Functions to call when completing a proof"
  :type 'hook
  :options '(idris-list-holes-on-load)
  :group 'idris-prover)

(defun idris-perhaps-insert-proof-script (proof)
  "Prompt the user as to whether PROOF should be inserted into the buffer."
  (save-window-excursion
    (pop-to-buffer idris-currently-loaded-buffer)
    (delete-other-windows)
    (let ((proof-buffer (get-buffer-create "*idris-finished-proof*")))
      (unwind-protect
          (progn
            (pop-to-buffer proof-buffer)
            (insert proof)
            (if (y-or-n-p "Keep this proof script?")
                (idris-insert-proof-script idris-currently-loaded-buffer proof)
              (kill-new proof)
              (message "Proof saved to kill ring")))
        (kill-buffer proof-buffer)))))

(defconst idris-proof-script-insertion-marker "---------- Proofs ----------"
  "Look for this marker to insert proofs. Should agree with the
  one in the Idris compiler source.")

(defun idris-insert-proof-script (buffer proof)
  "Insert PROOF into BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward idris-proof-script-insertion-marker nil t)
        (when (re-search-forward "\\(\\s-*\n\\)*\\'")
          (replace-match (concat "\n\n" idris-proof-script-insertion-marker "\n") nil nil)))
      (newline)
      (insert proof)
      (newline))))

(provide 'idris-prover)
