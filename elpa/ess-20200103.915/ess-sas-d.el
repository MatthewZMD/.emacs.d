;;; ess-sas-d.el --- SAS customization

;; Copyright (C) 1997--2001 Richard M. Heiberger and A. J. Rossini
;; Copyright (C) 2002--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Richard M. Heiberger <rmh@temple.edu>
;; Created: 20 Aug 1997
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS.

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

;; This file defines all the SAS customizations for ESS behaviors.  See
;; ess-sas-l and ess-sas-a for the underlying general modifications.

;;; Code:

;;; Autoloads:

(require 'shell)
(require 'ess-sas-l)

(defcustom SAS-mode-hook nil
  "Hook to run when entering SAS mode."
  :type 'hook
  :group 'ess-sas)

(defvar inferior-SAS-args "-stdio -linesize 80 -noovp -nosyntaxcheck"
  "Arguments to use for starting SAS.")

(defvar inferior-SAS-args-temp nil
  "Hack variable, needed for args preprocessing.
Better logic needed!  (see 2 uses, in this file).")

(defvar SAS-mode-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?\\ "."  tab)  ;; backslash is punctuation
    (modify-syntax-entry ?+  "."  tab)
    (modify-syntax-entry ?-  "."  tab)
    (modify-syntax-entry ?=  "."  tab)
    (modify-syntax-entry ?%  "w"  tab)
    (modify-syntax-entry ?<  "."  tab)
    (modify-syntax-entry ?>  "."  tab)
    (modify-syntax-entry ?&  "w"  tab)
    (modify-syntax-entry ?|  "."  tab)
    (modify-syntax-entry ?\' "\"" tab)
    (modify-syntax-entry ?*  ". 23"  tab) ; comment character
    (modify-syntax-entry ?\; "."  tab)
    (modify-syntax-entry ?_  "w"  tab)
    (modify-syntax-entry ?<  "."  tab)
    (modify-syntax-entry ?>  "."  tab)
    (modify-syntax-entry ?/  ". 14"  tab) ; comment character
    (modify-syntax-entry ?.  "w"  tab)
    tab)
  "Syntax table for `SAS-mode'.")

(defun ess-SAS-pre-run-hook (temp-ess-dialect)
  "Set up log and list files for interactive SAS."

  (let* ((ess-shell-buffer-name-flag (get-buffer "*shell*"))
         ess-shell-buffer-name
         ;; isn't pretty yet.
         ;;  ess-local-process-name is defined after this function.
         ;;  it needs to be defined prior to this function.
         (tmp-procname (let ((ntry 0)
                             (done nil))
                         ;; find a non-existent process
                         (while (not done)
                           (setq ntry (1+ ntry)
                                 done (not
                                       (get-process (ess-proc-name
                                                     ntry
                                                     temp-ess-dialect)))))
                         (ess-proc-name ntry temp-ess-dialect)))
         ;; Following was tmp-local-process-name.  Stolen from inferior-ess
         (ess-sas-lst-bufname (concat "*" tmp-procname ".lst*"))
         (ess-sas-log-bufname (concat "*" tmp-procname ".log*"))
         (explicit-shell-file-name "/bin/sh")
         inferior-SAS-redirect-args
         ess-sas-lst
         ess-sas-log)

    (ess-write-to-dribble-buffer
     (format "(ess-SAS-pre-run-hook 1): ess-lang=%s, ess-dialect=%s, temp-dialect=%s, buf=%s \n"
             ess-language
             ess-dialect
             temp-ess-dialect
             (current-buffer)))
    ;; If someone is running a *shell* buffer, rename it to avoid
    ;; inadvertent nuking.
    (if ess-shell-buffer-name-flag
        (with-current-buffer "*shell*"
          (setq ess-shell-buffer-name
                (rename-buffer "*ess-shell-regular*" t))))

    ;; Construct the LST buffer for output
    (if (get-buffer ess-sas-lst-bufname)
        nil
      (shell)
      (accept-process-output (get-buffer-process (current-buffer)))
      (sleep-for 2) ; need to wait, else working too fast!
      (setq ess-sas-lst (ess-insert-accept "tty"))
      (SAS-listing-mode)
      (shell-mode)
      (ess-listing-minor-mode t)
      (rename-buffer ess-sas-lst-bufname t))

    ;; Construct the LOG buffer for output
    (if (get-buffer  ess-sas-log-bufname)
        nil
      (shell)
      (accept-process-output (get-buffer-process (current-buffer)))
      (sleep-for 2) ; need to wait, else working too fast!
      (setq ess-sas-log (ess-insert-accept "tty"))
                                        ;(SAS-log-mode)
      (shell-mode)
      (ess-transcript-minor-mode t)
      (rename-buffer ess-sas-log-bufname t))

    (setq inferior-SAS-redirect-args (concat " "
                                             ess-sas-lst
                                             " "
                                             ess-sas-log
                                             " ")
          inferior-SAS-args-temp (concat inferior-SAS-redirect-args
                                         inferior-SAS-args))

    ;; Restore the *shell* buffer
    (if ess-shell-buffer-name-flag
        (with-current-buffer ess-shell-buffer-name
          (rename-buffer "*shell*")))

    (delete-other-windows)
    (split-window-vertically)
    (split-window-vertically)
    (switch-to-buffer (nth 2 (buffer-list)))
    (other-window 2)
    (switch-to-buffer ess-sas-log-bufname)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer ess-sas-lst-bufname)
    (other-window 2)

    ;;workaround
    (setq inferior-SAS-program
          (concat (file-name-as-directory ess-etc-directory)
                  "ess-sas-sh-command"))
    (setq inferior-ess-program inferior-SAS-program)))

(defun ess-insert-accept (command)
  "Submit command to process, get next line."
  (interactive)
  (goto-char (point-max))
  (insert command)
  (comint-send-input)
  (accept-process-output (get-buffer-process (current-buffer)))
  (forward-line -1)
  (let* ((beg (point))
         (ess-tty-name (progn (end-of-line) (buffer-substring beg (point)))))
    (goto-char (point-max))
    ess-tty-name))


(defvar SAS-customize-alist
  '((ess-local-customize-alist     . 'SAS-customize-alist)
    (ess-language                  . "SAS")
    (ess-dialect                   . "SAS")
    (inferior-ess-program          . inferior-SAS-program)
    (ess-help-sec-regex            . "^[A-Z. ---]+:$")
    (ess-help-sec-keys-alist       . " ")
    (ess-object-name-db-file       . "ess-sas-namedb.el")
    (inferior-ess-objects-command  . "objects(%d)");;FIXME
    (inferior-ess-help-command     . "help(\"%s\",pager=\"cat\",window=F)\n");;FIXME
    (inferior-ess-exit-command     . "endsas;\n")
    (ess-loop-timeout              .  500000 )
    (inferior-ess-primary-prompt   . "^")
    (inferior-ess-secondary-prompt . "^")
    (comint-use-prompt-regexp      . t)
    (inferior-ess-start-args       . inferior-SAS-args-temp)
    ;; (ess-pre-run-hook              . 'ess-SAS-pre-run-hook)
    ;; (ess-local-process-name        . nil)
    )
  "Variables to customize for SAS")

;;; The functions of interest (mode, inferior mode)

;;;###autoload
(define-derived-mode SAS-mode ess-mode "[SAS]"
  "Major mode for editing SAS source.  See `ess-mode' for more help."
  :group 'ess-sas
  (ess-setq-vars-local SAS-customize-alist)
  (setq ess-local-customize-alist SAS-customize-alist)
  (setq-local sentence-end ";[\t\n */]*")
  (setq-local paragraph-start "^[ \t]*$")
  (setq-local paragraph-separate "^[ \t]*$")
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'sas-indent-line)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/[*]")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[*]/")
  (setq-local comment-column 40)
  (setq-local ess-local-process-name nil)
  (setq-local tab-stop-list ess-sas-tab-stop-list)
  (setq-local font-lock-keywords-case-fold-search t)
  ;; Local map settings, AFTER initialization (only if not yet defined)
  (unless sas-mode-local-map
    (setq sas-mode-local-map (copy-keymap (current-local-map)))
    (ess-sas-edit-keys-set ess-sas-edit-keys-toggle)
    (if ess-sas-local-unix-keys (ess-sas-local-unix-keys))
    (if ess-sas-local-pc-keys (ess-sas-local-pc-keys))
    (if ess-sas-global-unix-keys (ess-sas-global-unix-keys))
    (if ess-sas-global-pc-keys (ess-sas-global-pc-keys)))
  (define-key sas-mode-local-map ";" 'ess-electric-run-semicolon)
  (define-key sas-mode-local-map (kbd "\C-c\C-w") 'ess-multi-frame-SAS)
  ;; this is a mess
  ;; interactive and batch commands share sas-mode-local-map,
  ;; but the associated commands are very different
  ;; what would be better is two maps like
  ;; sas-batch-mode-local-map and sas-interactive-mode-local-map
  ;; or smart function definitions that would do the appropriate
  ;; thing for either batch or interactive sessions
  ;; however, neither of these solutions are planned
  ;; therefore, no key definitions can be shared between
  ;; batch and interactive at this time, hence the lines that
  ;; are commented below:  uncomment at your own risk
  ;;  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path)
  ;;  (define-key sas-mode-local-map "\C-c\C-b" 'ess-sas-submit)
  ;;  (define-key sas-mode-local-map "\C-c\C-r" 'ess-sas-submit-region)
  ;;  (define-key sas-mode-local-map "\C-c\C-x" 'ess-sas-goto-log)
  ;;  (define-key sas-mode-local-map "\C-c\C-y" 'ess-sas-goto-lst)

  (use-local-map sas-mode-local-map)

  (setq font-lock-defaults
        ;; KEYWORDS KEYWORDS-ONLY CASE-FOLD .....
        '(SAS-mode-font-lock-defaults nil t)))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Ss][Aa][Ss]\\'" . SAS-mode))

;; rmh Jul 10 2003
(defun ess-electric-run-semicolon (arg)
  "Insert character.  If the line contains \"run;\" or \"quit;\" and nothing else then indent line."
  (interactive "P")
  (if ess-sas-edit-keys-toggle (insert ";") (let (insertpos)
                                              (if (and (not arg)
                                                       (eolp)
                                                       (save-excursion
                                                         (skip-chars-backward " \t")
                                                         (backward-word 1)
                                                         (and (looking-at "run\\|quit")
                                                              (progn
                                                                (skip-chars-backward " \t")
                                                                (bolp)))))
                                                  (progn
                                                    (insert last-command-event)
                                                    (funcall indent-line-function)
                                                    (save-excursion
                                                      (if insertpos (goto-char (1+ insertpos)))
                                                      (delete-char -1))))
                                              (if insertpos
                                                  (save-excursion
                                                    (goto-char insertpos)
                                                    (self-insert-command (prefix-numeric-value arg)))
                                                (self-insert-command (prefix-numeric-value arg))))))

(defun SAS-menu ()
  "Start SAS from the menu."
  (interactive)
  (if ess-microsoft-p
      ;; replace with other choices for starting SAS?
      (error "SAS cannot be started this way in ESS on Windows.")
    (SAS)))

(defun SAS ()
  "Call 'SAS', from SAS Institute."
  (interactive)
  (let* ((temp-dialect "SAS")) ;(cdr (rassoc ess-dialect SAS-customize-alist))))
    (ess-write-to-dribble-buffer
     (format "(SAS): ess-dial=%s, temp-dial=%s\n"
             ess-dialect
             temp-dialect))
    (ess-SAS-pre-run-hook temp-dialect)
    (setq ess-eval-visibly-p nil)
    ;; FIXME: `inferior-SAS-args' is defined from
    ;; `inferior-SAS-args-temp' in `ess-SAS-pre-run-hook'
    (let ((inf-buf (inferior-ess nil SAS-customize-alist)))
      (with-current-buffer inf-buf
        (use-local-map sas-mode-local-map))
      inf-buf)))

(defun ess-multi-frame-SAS ()
  "Put running SAS buffers into separate frames.
Load this function M-x load-file essx-sas.el RET.
Then find-file myfile.sas.  If myfile.sas is already in a buffer, kill-buffer
it and then find-file it again.
Place the cursor in a myfile.sas buffer.  Run SAS with M-x SAS,
Return the cursor to the myfile.sas buffer,
then enter C-c C-w to put *SAS* *SAS.log* *SAS.lst* buffers into
their own frames."
  (interactive)
  (delete-other-windows)
  (with-current-buffer "*SAS*"
    (make-frame))
  (with-current-buffer "*SAS.log*"
    (make-frame))
  (with-current-buffer "*SAS.lst*"
    (make-frame)))

(defun ess-num-or-zero (arg)
  "If a number, then return that number, otherwise return 0."
  (or (and (numberp arg) arg) 0))

 ; Provide package

(provide 'ess-sas-d)

;;; ess-sas-d.el ends here
