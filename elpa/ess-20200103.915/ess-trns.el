;;; ess-trns.el --- Support for manipulating S transcript files  -*- lexical-binding: t; -*-

;; Copyright (C) 1989--1994 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2012 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/


;;; Commentary:

;; Code for dealing with ESS transcripts.

;;; Code:

 ; Requires and autoloads

(require 'ess-mode)
(require 'ess-inf)
(require 'comint)

(declare-function ess-display-help-on-object "ess-help" (object &optional command))

 ; ess-transcript-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The major mode ess-transcript-mode
;;;; * Commands for ess-transcript-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom ess-transcript-mode-hook nil
  "Hook for customizing ESS transcript mode."
  :group 'ess-hooks
  :type 'hook)

;;*;; Major mode definition
(defvar ess-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" #'ess-switch-process)
    (define-key map "\C-c\C-r" #'ess-eval-region)
    (define-key map "\C-c\M-r" #'ess-eval-region-and-go)
    (define-key map "\C-c\C-k" #'ess-force-buffer-current)
    (define-key map "\C-c\C-q" #'ess-quit)
    (define-key map "\C-c\C-j" #'ess-transcript-send-command)
    (define-key map "\C-c\M-j" #'ess-transcript-send-command-and-move)
    (define-key map "\M-\C-a"  #'ess-goto-end-of-function-or-para)
    (define-key map "\M-\C-e"  #'ess-goto-end-of-function-or-para)
    (define-key map "\C-c\C-y" #'ess-switch-to-ESS)
    (define-key map "\C-c\C-z" #'ess-switch-to-end-of-ESS)
    (define-key map "\C-c\C-v" #'ess-display-help-on-object)
    (define-key map "\C-c\C-d" #'ess-dump-object-into-edit-buffer)
    (define-key map "\C-a"     #'comint-bol)
    (define-key map "\M-\t"    #'comint-replace-by-expanded-filename)
    (define-key map "\M-?"     #'comint-dynamic-list-completions)
    (define-key map "\C-c\C-k" #'ess-request-a-process)
    (define-key map "{"        #'skeleton-pair-insert-maybe)
    (define-key map "}"        #'skeleton-pair-insert-maybe)
    (define-key map "\e\C-h"   #'ess-mark-function-or-para)
    (define-key map "\e\C-q"   #'ess-indent-exp)
    (define-key map "\t"       #'ess-indent-command)
    (define-key map "\C-c\C-p" #'comint-previous-prompt)
    (define-key map "\C-c\C-n" #'comint-next-prompt)
    (define-key map "\r"       #'ess-transcript-send-command-and-move)
    (define-key map "\M-\r"    #'ess-transcript-send-command)
    (define-key map "\C-c\r"   #'ess-transcript-copy-command)
    (define-key map "\C-c\C-w" #'ess-transcript-DO-clean-region)
    (define-key map "\C-c\M-c" #'ess-transcript-clean-buffer)
    map)
  "Keymap for `ess-transcript-mode'.")

(easy-menu-define
  ess-transcript-mode-menu ess-transcript-mode-map
  "Menu for use in S transcript mode."
  '("ESS-trans"
    ["Describe"         describe-mode                   t]
    ["About"           (ess-goto-info "Transcript Mode") t]
    ["Send bug report"  ess-submit-bug-report           t]
    "------"
    ["Mark cmd group"   mark-paragraph          t]
    ["Previous prompt"  comint-previous-prompt  t]
    ["Next prompt"      comint-next-prompt      t]
    "------"
    ["Send and move" ess-transcript-send-command-and-move t]
    ["Copy command"  ess-transcript-copy-command	t]
    ["Send command"  ess-transcript-send-command	t]
    ["Clean Region"  ess-transcript-DO-clean-region	t]
    ["Clean Whole Buffer" ess-transcript-clean-buffer	t]
    ["Switch S process" ess-switch-process		t]
    ))

;;;###autoload
(define-derived-mode ess-transcript-mode ess-mode "ESS Transcript"
  "Major mode for transcript files.

Type \\[ess-transcript-send-command] to send a command in the
transcript to the current inferior process. \\[ess-transcript-copy-command]
copies the command but does not execute it, allowing you to edit it in
the process buffer first.

Type \\[ess-transcript-clean-region] to delete all outputs and prompts
in the region, leaving only the commands."
  :group 'ess
  (setq buffer-read-only t
        ess-local-process-name nil
        mode-line-process '(" [" ess-local-process-name "]"))
  ;; TODO: Set this more normally in various major modes
  (unless inferior-ess-prompt ;; For S languages it is set in custom-alist
    (setq inferior-ess-prompt
          ;; Do not anchor to bol with `^'
          (concat "\\("
                  inferior-ess-primary-prompt
                  "\\|"
                  inferior-ess-secondary-prompt
                  "\\)")))
  (setq-local paragraph-start (concat "^" inferior-ess-prompt "\\|^\^L"))
  (setq-local paragraph-separate "^\^L")
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-regexp (concat "^" inferior-ess-prompt))
  (setq font-lock-defaults '(ess-build-font-lock-keywords
                             nil nil ((?\. . "w") (?\_ . "w") (?' . ".")))))

;;*;; Commands used in S transcript mode

(defun ess-transcript-send-command ()
  "Send the command at point in the transcript to the ESS process.
The line should begin with a prompt.  The ESS process buffer is displayed if it
is not already."
  (interactive)
  (let* ((proc (or ess-local-process-name
                   (ess-request-a-process "Evaluate into which process? " t)))
         (ess-buf (ess-get-process-buffer proc)))
    (setq ess-local-process-name proc)
    (if (get-buffer-window ess-buf) nil
      (display-buffer ess-buf t))
    (let ((input (inferior-ess-get-old-input)))
      (with-current-buffer ess-buf
        (goto-char (point-max))
        (ess-eval-linewise input)))))

(defun ess-transcript-send-command-and-move ()
  "Send the command on this line, and move point to the next command."
  (interactive)
  (let* ((proc (or ess-local-process-name
                   (ess-request-a-process "Evaluate into which process? " t)))
         (ess-buf (ess-get-process-buffer proc)))
    (setq ess-local-process-name proc)
    (if (get-buffer-window ess-buf) nil
      (display-buffer ess-buf t))
    (let ((input (inferior-ess-get-old-input)))
      (with-current-buffer ess-buf
        (goto-char (point-max))
        (ess-eval-linewise input nil nil nil 1))))
  (comint-next-prompt 1))

(defun ess-transcript-copy-command ()
  "Copy the command at point to the command line of the ESS process."
  (interactive)
  (let* ((proc (or ess-local-process-name
                   (ess-request-a-process "Evaluate into which process? " t)))
         (ess-buf (process-buffer (get-process proc)))
         (input (inferior-ess-get-old-input)))
    (setq ess-local-process-name proc)
    (if (get-buffer-window ess-buf) nil
      (display-buffer ess-buf t))
    (with-current-buffer ess-buf
      (goto-char (point-max))
      (insert input)))
  (ess-switch-to-end-of-ESS))

(defun ess-transcript-clean-region (beg end even-if-read-only)
  "Strip the transcript in the region, leaving only (R/S/Lsp/..) commands.
Deletes any lines not beginning with a prompt, and then removes the
prompt from those lines that remain.  Prefix argument means to
clean even if the buffer is \\[read-only]."
  (interactive "r\nP")
  (unless inferior-ess-prompt
    (error "Cannot clean ESS transcript region in this mode!
 That only works in ess-transcript-mode or inferior-ess-mode ('*R*' etc)."
           ;; Maybe call ess-clean-region-in-new-transcript ?"))
           ))
  (let ((do-toggle (and buffer-read-only even-if-read-only))
        (ess-prompt-rx (if inferior-ess-secondary-prompt
                           (concat "^\\(\\("
                                   inferior-ess-prompt
                                   "\\)\\|\\("
                                   inferior-ess-secondary-prompt
                                   "\\)\\)")
                         (concat "^" inferior-ess-prompt))))
    (save-excursion
      (if do-toggle (setq buffer-read-only nil))
      (save-restriction
        (deactivate-mark)
        (narrow-to-region beg end)
        (goto-char (point-min))
        (delete-non-matching-lines ess-prompt-rx)
        (goto-char (point-min))
        ;; (replace-regexp  *  * ) :
        (while (re-search-forward ess-prompt-rx nil t)
          (replace-match "" nil nil)))

      (if do-toggle (setq buffer-read-only t)))))

(defun ess-transcript-DO-clean-region (beg end)
  "Clean the current via \\[ess-transcript-clean-region] even if the buffer is read-only."
  (interactive "r")
  (ess-transcript-clean-region beg end 'In-ANY-case))

(defun ess-transcript-clean-buffer ()
  "Cleanup the whole buffer.
Use point-min/max to obey `narrow-to-region'."
  (interactive)
  (ess-transcript-clean-region (point-min) (point-max) 'In-ANY-case))

(provide 'ess-trns)

;;; ess-trns.el ends here
