;;; ess-stata-mode.el --- Stata customization  -*- lexical-binding: t; -*-

;; Copyright (C) 1997--1999 A. J. Rossini, Thomas Lumley
;; Copyright (C) 1997--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 9 Sep 1998
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

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

;; This file defines all the Stata customizations for ess-mode. It is somewhat
;; based on Stata-mode by Thomas Lumley <thomas@biostat.washington.edu>.

;;; Code:

(require 'ess-mode)
(require 'ess-stata-lang)

(defvar STA-dialect-name "stata"
  "Name of 'dialect' for Stata.");easily changeable in a user's .emacs

(defvar ess-stata-mode-syntax-table
  (let ((tbl (copy-syntax-table ess-mode-syntax-table)))
    (modify-syntax-entry ?\\ "." tbl) ;nullify escape meaning
    (modify-syntax-entry ?\$ "." tbl)
    (modify-syntax-entry ?` "(\'" tbl)
    (modify-syntax-entry ?\' ")`" tbl)
    (modify-syntax-entry ?/  ". 124b" tbl)
    ;; asterisk at bol comments taken care of by
    ;; `syntax-propertize-function'.
    (modify-syntax-entry ?*  ". 23b"   tbl)
    (modify-syntax-entry ?\n ">"  tbl)
    (modify-syntax-entry ?+ "." tbl)
    (modify-syntax-entry ?- "." tbl)
    (modify-syntax-entry ?= "." tbl)
    (modify-syntax-entry ?% "." tbl)
    (modify-syntax-entry ?< "." tbl)
    (modify-syntax-entry ?> "." tbl)
    (modify-syntax-entry ?& "." tbl)
    (modify-syntax-entry ?| "." tbl)
    (modify-syntax-entry ?~ "." tbl)
    tbl)
  "Syntax table for `ess-stata-mode'.")

(defvar STA-customize-alist
  '((ess-local-customize-alist     . 'STA-customize-alist)
    (ess-language                  . "STA")
    (ess-dialect                   . STA-dialect-name)
    (ess-suffix                    . "ado")
    (ess-mode-editing-alist        . STA-editing-alist)
    (ess-help-sec-regex            . ess-help-STA-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-STA-sec-keys-alist)
    (ess-loop-timeout              . 500000 )
    (ess-object-name-db-file       . "ess-sta-namedb.el" )
    (inferior-ess-program          . inferior-STA-program)
    (inferior-ess-objects-command  . "describe\n")
    (inferior-ess-help-command     . "help %s\n") ;; assumes set more off
    (inferior-ess-exit-command     . "exit\n")
    ;; --more-- is necessary here (hangs otherwise if startup stata.msg is big)
    (inferior-ess-primary-prompt   . "[.:] \\|--more--")
    (inferior-ess-secondary-prompt . ">\\|--more--")
    (inferior-ess-prompt           . "\\([.:>] \\|--more--\\)")
    (comint-use-prompt-regexp      . t)
    (inferior-ess-search-list-command   . "set more off\n search()\n")
    (ess-execute-screen-options-command . "set linesize %s\n")
    (ess-getwd-command             . "pwd\n")
    (ess-setwd-command             . "cd \"%s\"\n")
    (ess-load-command              . "run \"%s\"\n"))
  "Variables to customize for Stata.")

(cl-defmethod ess--help-web-search-override (cmd &context (ess-dialect "stata"))
  "Browse the web for documentation about CMD in Stata."
  (browse-url
   (format
    "https://www.stata.com/search/?q=%s&restrict=&btnG=Search&client=stata&num=&output=xml_no_dtd&site=stata&ie=&oe=UTF-8&sort=&proxystylesheet=stata"
    cmd)))

;;;###autoload
(define-derived-mode ess-stata-mode ess-mode "ESS[STA]"
  "Major mode for editing Stata source."
  :group 'ess-Stata
  (ess-setq-vars-local STA-customize-alist)
  (setq-local comint-use-prompt-regexp t)
  (setq-local comment-column 40)
  (setq-local comment-end " */")
  (setq-local comment-start "/* ")
  (setq-local comment-start-skip "/\\*+ *")
  (setq-local comment-use-syntax t)
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ("^\\*" (0 "<"))))
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-separate (concat  "[ \t\f]*$\\|" page-delimiter))
  (setq-local paragraph-start (concat "[ \t\f]*$\\|" page-delimiter))
  (setq font-lock-defaults '(ess-STA-mode-font-lock-defaults nil nil ((?\. . "w")))))

(defalias 'STA-mode 'ess-stata-mode)
(defalias 'stata-mode 'ess-stata-mode)
(defalias 'Stata-mode 'ess-stata-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.do\\'" . ess-stata-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ado\\'" . ess-stata-mode))


(defun ess-sta-remove-comments (string)
  "Remove one-line comments before sending the STRING to process.

This function is placed in `ess-presend-filter-functions'."
  (replace-regexp-in-string "/\\*.*\\*/\\|^//.*$" "" string))

;; (ess-sta-remove-comments "aaa /* sdfdsf */ bbb
;; sdfsd
;;  ccc
;; // sdfsf
;; sdf /* sdfdsf */
;; sdfsf
;; " )


(defvar ess-stata-post-run-hook nil
  "Functions run in process buffer after the initialization of stata process.")

(defun stata (&optional start-args)
  "Call Stata with START-ARGS."
  (interactive "P")
  (ess-write-to-dribble-buffer
   (format "(STA): ess-dialect=%s , buf=%s \n"
           ess-dialect
           (current-buffer)))
  (let* ((sta-start-args
          (concat inferior-STA-start-args
                  (when start-args (read-string "Starting Args [possibly -k####] ? "))))
         (inf-buf (inferior-ess sta-start-args STA-customize-alist))
         (inf-proc (get-buffer-process inf-buf)))
    (while (process-get inf-proc 'sec-prompt)
      ;; get read of all --more-- if stata.msg is too long.
      (ess-send-string inf-proc "q")
      (ess-wait-for-process inf-proc t))
    (ess-send-string inf-proc "set more off")
    (goto-char (point-max))
    (with-current-buffer inf-buf
      (add-hook 'ess-presend-filter-functions 'ess-sta-remove-comments nil 'local)
      (run-mode-hooks 'ess-stata-post-run-hook))
    inf-buf))

(defvar inferior-ess-stata-mode-syntax-table
  (let ((tab (copy-syntax-table ess-stata-mode-syntax-table)))
    tab)
  "Syntax table for `inferior-ess-stata-mode'.")

(define-derived-mode inferior-ess-stata-mode inferior-ess-mode "iESS"
  "Inferior `stata' mode."
  :group 'ess-proc
  (ess-setq-vars-local STA-customize-alist)
  (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))
  (setq-local comint-use-prompt-regexp t)
  (setq font-lock-defaults '(ess-STA-mode-font-lock-defaults nil nil ((?\. . "w")))))


(define-derived-mode ess-stata-transcript-mode ess-transcript-mode "ESS Transcript"
  "Stata transcript mode."
  :group 'ess-Stata
  :syntax-table ess-stata-mode-syntax-table
  (ess-setq-vars-local STA-customize-alist)
  (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))
  (setq-local comint-use-prompt-regexp t)
  (setq-local comment-column 40)
  (setq-local comment-end " */")
  (setq-local comment-start "/* ")
  (setq-local comment-start-skip "/\\*+ *")
  (setq-local comment-use-syntax t)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-separate (concat  "[ \t\f]*$\\|" page-delimiter))
  (setq-local paragraph-start (concat "[ \t\f]*$\\|" page-delimiter))
  (setq font-lock-defaults '(ess-STA-mode-font-lock-defaults nil nil ((?\. . "w")))))

(defalias 'STA-transcript-mode 'ess-stata-mode-syntax-table)

(defun ess--STA-retrive-topics-from-search ()
  (with-current-buffer (ess-command inferior-ess-search-list-command)
    (goto-char (point-min))
    (let (topics)
      (while (re-search-forward "(help \\(.+?\\)\\( if installed\\| for replacement.*\\)?)$" nil t)
        (setq topics
              (nconc (split-string (match-string-no-properties 1) ",\\|; +")
                     topics)))
      (nreverse (delete-dups topics))
      )))

(cl-defmethod ess-help-get-topics (proc &context (ess-dialect "stata"))
  "Return a list of current STA help topics associated with process PROC."
  (or (ess-process-get 'help-topics proc)
      (progn
        (ess-process-put 'help-topics (ess--STA-retrive-topics-from-search))
        (ess-process-get 'help-topics))))

(provide 'ess-stata-mode)

;;; ess-stata-mode.el ends here
