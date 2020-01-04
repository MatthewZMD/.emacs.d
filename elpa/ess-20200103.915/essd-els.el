;;; essd-els.el --- S-PLUS 3.x at another location customization  -*- lexical-binding: t; -*-

;; Copyright (C) 1998 Richard M. Heiberger
;; Copyright (C) 1999--2005 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Richard M. Heiberger <rmh@temple.edu>
;; Created: December 1998

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

;; This file defines all the S-PLUS 3.x customizations for ess-mode.

;;; Code:

(require 'ess-julia)
(require 'ess-s-lang)
(require 'ess-sp6-d)
(require 'ess-sas-d)
(require 'ess-stata-mode)
(require 'ess-trns)
(require 'ess-utils)

;; Easily changeable in a user's .emacs
(defvar S+elsewhere-dialect-name "S+6"
  "Name of 'dialect' for S-PLUS at another location.")

(defvar S+elsewhere-start-args "-i"
  "Arguments for `S+elsewhere-dialect-name'.")

(defcustom inferior-ess-remote-pager nil
  "Remote pager to use for reporting help files and similar things.
The default value is nil."
  :group 'ess-proc
  :type '(choice (const nil) string))


(defvar S+elsewhere-customize-alist
  (append
   '((ess-local-customize-alist         . 'S+elsewhere-customize-alist)
     (ess-dialect                       . S+elsewhere-dialect-name)
     (ess-object-name-db-file           . "ess-spelsewhere-namedb.el" )
     (inferior-ess-program              . inferior-S-elsewhere-program)
     (inferior-ess-help-command         . "help(\"%s\", pager=\"cat\", window=F)\n")
     (ess-STERM  . "iESS")
     )
   S+common-cust-alist)
  "Variables to customize for S+elsewhere.")

(defun S+elsewhere (&optional _proc-name)
  "Call 'S-PLUS 3.x', the 'Real Thing'  from StatSci."
  ;; git commit 104c4d7c56bc239ea245562763caa317bc3a1a84
  (declare (obsolete ess-remote "2000"))
  (ess-write-to-dribble-buffer
   (format "\n(S+elsewhere): ess-dialect=%s, buf=%s\n" ess-dialect
           (current-buffer)))
  (let ((inf-buf (inferior-ess nil S+elsewhere-customize-alist)))
    (when inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start))
    inf-buf))

(defun S+elsewhere-mode (&optional _proc-name)
  "Major mode for editing S+3 source.  See `ess-mode' for more help."
  (declare (obsolete ess-remote "2000"))
  (setq-local ess-local-customize-alist S+elsewhere-customize-alist)
  (ess-mode))

(define-obsolete-function-alias 'S+elsewhere-transcript-mode #'S-transcript-mode "2000")

;; This REALLY shouldn't need an editing mode.  Just a transcript and
;; an inferior process handler.

(defun ess-select-alist-dialect (&optional dialect)
  "Query user for an ESS DIALECT and return the matching customize-alist."
  (interactive)
  (let* ((dialects '("R" "S+" "julia" "arc" "vst" "omg" "s3" "s4" "stata" "sp3" "sp4"
                     "sqpe4" "sp5" "sqpe" "XLS" "SAS"))
         (dialect (or dialect
                      (ess-completing-read "Dialect" dialects nil t))))
    (cond
     ((string= dialect "julia") ess-julia-customize-alist)
     ((string= dialect "stata") STA-customize-alist)
     ((string= dialect "R")     ess-r-customize-alist)
     ((string= dialect "SAS")   SAS-customize-alist)
     (t                         S+elsewhere-customize-alist))))

(defun ess-add-ess-process ()
  "Execute this command from within a buffer running a process to add
the process to `ess-process-name-alist' and to make it the
`ess-current-process-name'.  This command will normally be run in a
telnet buffer connected to another computer or in a shell or comint
buffer on the local computer."
  (interactive)
  (let ((proc (get-buffer-process (buffer-name))))
    (if (not proc)
        (error "No process is associated with this buffer")
      (set-process-filter proc 'inferior-ess-output-filter)
      (setq ess-current-process-name (process-name proc))
      (add-to-list 'ess-process-name-list
                   (list ess-current-process-name)))))

(defvar ess-remote nil
  "Indicator, t in remote buffers.")

;;;###autoload
(defun ess-remote (&optional proc-name dialect)
  "Execute this command from within a buffer running a process.
It runs `ess-add-ess-process' to add the PROC-NAME to
`ess-process-name-alist' and to make it the
`ess-current-process-name'. It then prompts the user for an ESS
language and sets the editing characteristics appropriately.

To use this command, first start a process on a remote computer by
manual use of telnet, rlogin, ssh, or some other protocol.  Start the
relevant program (\"S\" or \"R\" or \"sas -stdio\") in that buffer.  Once
you are talking to S or R or SAS, then do \\[ess-remote] to make
the current buffer an inferior-ess buffer with the right behavior for
the language you are currently working with.  With S and R, use C-c
C-n to send lines over.  With SAS, use C-c i
`ess-eval-line-and-step-invisibly' to send lines over invisibly.

DIALECT is the desired ess-dialect. If nil, ask for dialect"

  (interactive)
  (ess-add-ess-process)
  ;; Need to select a remote-customize-alist
  (let ((customize-alist (ess-select-alist-dialect dialect)))
    (ess-write-to-dribble-buffer
     (format "\n(ESS-remote): ess-dialect=%s, buf=%s\n" ess-dialect
             (current-buffer)))
    (ess-setq-vars-local customize-alist)
    (inferior-ess--set-major-mode ess-dialect)
    (setq-local ess-remote t)
    (setq ess-local-process-name (or proc-name ess-current-process-name))

    (goto-char (point-max))

    (when (equal ess-dialect "R")
      ;; ugly fix for evn variable. What can we do :(
      (ess-eval-linewise (format "options(pager='%s')\n"
                                 (or inferior-ess-remote-pager inferior-ess-pager))
                         nil nil nil 'wait)
      (ess-r-load-ESSR))

    (when (equal ess-dialect "S+")
      (ess-command ess-S+--injected-code))

    (when (equal ess-language "SAS")
      (font-lock-mode 0)
      (SAS-log-mode)
      (shell-mode)
      (setq buffer-read-only nil)
      (font-lock-mode 1))

    (ess-process-put 'funargs-cache (make-hash-table :test 'equal))
    (ess-process-put 'funargs-pre-cache nil)
    ;; auto-complete
    (ess--setup-auto-complete ess-r-ac-sources t)
    ;; company
    (ess--setup-company ess-r-company-backends t)

    (when inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start
                         nil nil nil 'wait-prompt))))


 ; Provide package

(provide 'essd-els)

;;; essd-els.el ends here
