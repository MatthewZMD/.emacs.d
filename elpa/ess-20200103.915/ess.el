;;; ess.el --- Emacs Speaks Statistics  -*- lexical-binding: t; -*-
;;
;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;;         A.J. Rossini <blindglobe@gmail.com>
;;         Richard M. Heiberger <rmh@temple.edu>
;;         Kurt Hornik <Kurt.Hornik@R-project.org>
;;         Martin Maechler <maechler@stat.math.ethz.ch>
;;         Rodney A. Sparapani <rsparapa@mcw.edu>
;;         Stephen Eglen <stephen@gnu.org>
;;         Sebastian P. Luque <spluque@gmail.com>
;;         Henning Redestig <henning.red@googlemail.com>
;;         Vitalie Spinu <spinuvit@gmail.com>
;;         Lionel Henry <lionel.hry@gmail.com>
;;         J. Alexander Branham <alex.branham@gmail.com>
;;
;; Maintainer: ESS Core Team <ESS-core@r-project.org>
;; Copyright (C) 1997-2018 ESS Core Team <ESS-core@r-project.org>
;; Created: 7 Jan 1994
;; Version: 18.10.3snapshot
;; URL: https://ess.r-project.org/
;; Package-Requires: ((emacs "25.1") (julia-mode "0.3"))
;; ESSR-Version: 1.5
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/
;;
;;; Commentary:
;;
;; Emacs Speaks Statistics (ESS) is a package designed to support editing of
;; scripts and interaction with various statistical analysis programs such as R,
;; S-Plus, SAS, Stata and OpenBUGS/JAGS. For more details please visit ESS home
;; page at https://ess.r-project.org/
;;
;;; Code:

(require 'ess-utils)
(require 'cl-generic)

(defvar reporter-prompt-for-summary-p)


;; Versions

(defconst ess-version (eval-when-compile
                        (require 'lisp-mnt)
                        (lm-version (or load-file-name buffer-file-name)))
  "Version of ESS currently loaded.")

(defconst essr-version (eval-when-compile
                         (require 'lisp-mnt)
                         (save-excursion (lm-header "ESSR-Version")))
  "Version of ESSR package.")

(defvar ess-revision nil
  "The revision and date of ESS.
Is set  by \\[ess-version-string].")

;;;###autoload
(defun ess-version ()
  "Return a string with ESS version information."
  (interactive)
  (message (format "ess-version: %s (loaded from %s)"
                   (ess-version-string)
                   (file-name-directory ess-lisp-directory))))

(defun ess-version-string ()
  (let* ((ess-dir (file-name-directory ess-lisp-directory)) ; if(<from source>) the top-level 'ess/'
         (is-release (file-exists-p (concat ess-etc-directory ".IS.RELEASE")))
         (rel-string (if is-release "Released "))
         (git-ref-fn (concat ess-dir ".git/HEAD"))
         (git-ref (when (file-exists-p git-ref-fn)
                    (with-current-buffer (find-file-noselect git-ref-fn)
                      (goto-char (point-min))
                      (when (re-search-forward "ref: \\(.*\\)\n" nil t)
                        (match-string 1)))))
         (git-fname (if git-ref
                        (concat ess-dir ".git/" git-ref)
                      ;; For release
                      (concat ess-etc-directory "git-ref")))
         (git-rev (when (file-exists-p git-fname)
                    (with-current-buffer (find-file-noselect git-fname)
                      (goto-char (point-min))
                      (concat "git: "(buffer-substring 1 (point-at-eol))))))
         (elpa-fname (concat ess-dir "ess-pkg.el"))
         (elpa-rev (when (file-exists-p elpa-fname)
                     ;; Get it from ELPA dir name, (probably won't work if installed manually)
                     (concat "elpa: "
                             (replace-regexp-in-string "ess-" ""
                                                       (file-name-nondirectory
                                                        (substring ess-dir 1 -1)))))))
    ;; Set the "global" ess-revision:
    (setq ess-revision (format "%s%s%s"
                               (or rel-string "")
                               (or git-rev "")
                               (or elpa-rev "")))
    (when (string= ess-revision "")
      (setq ess-revision "<unknown>"))
    (concat ess-version " [" ess-revision "]")))


;;; Bug Reporting

;;;###autoload
(defun ess-submit-bug-report ()
  "Submit a bug report to the ESS maintainers."
  (interactive)
  (let ((reporter-prompt-for-summary-p 't))
    (reporter-submit-bug-report
     "ess-bugs@r-project.org"
     (concat "ess-mode " (ess-version-string))
     (list 'ess-language
           'ess-dialect
           'ess-ask-for-ess-directory
           'ess-ask-about-transfile
           'default-directory
           'ess-keep-dump-files
           'ess-source-directory
           'ess-use-ido
           'ess-use-eldoc
           'ess-use-tracebug
           'ess-use-auto-complete
           'ess-use-company
           'ess-eval-visibly-p
           'ess-can-eval-in-background
           'ess-local-process-name)
     nil
     (lambda ()
       ;;(goto-char (point-max))
       (rfc822-goto-eoh)
       (forward-line 1)
       (insert "\n\n-------------------------------------------------------\n")
       (insert "This bug report will be sent to the ESS bugs email list\n")
       (insert "Press C-c C-c when you are ready to send your message.\n")
       (insert "-------------------------------------------------------\n\n")
       (insert (with-current-buffer ess-dribble-buffer
                 (goto-char (point-max))
                 (forward-line -100)
                 (buffer-substring-no-properties (point) (point-max))))))))



;;; Timer

(defcustom ess-idle-timer-interval 1
  "Number of idle seconds to wait before running function in `ess-idle-timer-functions'."
  :type '(integer)
  :group 'ess)

(defvar ess-idle-timer-functions nil
  "A list of functions to run each `ess-idle-timer-interval' idle seconds.
If your function calls the process, you better use
`ess-when-new-input' to wrap your call. If you call the
subprocess please respect `ess-can-eval-in-background' variable.

These functions are run with `run-hooks'. Use `add-hook' to add
symbols to this variable.

Most likely you will need a local hook. Then you should specify
the LOCAL argument to `add-hook' and initialize it in
`ess-mode-hook' or `ess-post-run-hook', or one of the more
specialized hooks `ess-r-post-run-hook',`ess-stata-post-run-hook'
etc.")

(defun ess--idle-timer-function nil
  "Internal function executed by `ess--idle-timer'."
  (run-hooks 'ess-idle-timer-functions))

(require 'timer)
(defvar ess--idle-timer
  (run-with-idle-timer ess-idle-timer-interval 'repeat 'ess--idle-timer-function)
  "Timer used to run `ess-idle-timer-functions'.")


;;; Dispatch on ess-dialect
;; Inspired by major-mode specializer in cl-generic.el

;; FIXME: Implement a notion of derived dialects. major-mode specializer cannot
;; be used here as we need same dialect in different major-modes.

;; Two parts:
;; 1) first define a specializer (ess-dialect= DIALECT) to match symbols
;;    representing ess dialects.
;; 2) then define a context-rewriter so you can write
;;   `&context (ess-dialect "R")` rather than
;;   `&context (ess-dialect (ess-dialect= "R"))`.

(cl-generic-define-generalizer ess--generic-dialect-generalizer
  95
  (lambda (name &rest _) `(if (stringp ,name) (intern ,name)
                            (if (symbolp ,name) ,name)))
  (lambda (tag &rest _) `((ess-dialect= ,tag))))

(cl-defmethod cl-generic-generalizers ((_specializer (head ess-dialect=)))
  "Support for (ess-dialect DIALECT) context specializer."
  (list ess--generic-dialect-generalizer))

(cl-generic-define-context-rewriter ess-dialect (dialect)
  `(ess-dialect (ess-dialect= ,(if (stringp dialect)
                                   (intern dialect)
                                 dialect))))

;; (cl-defgeneric ess-print-dialect ()
;;   (error "unknown dialect %s" ess-dialect))
;; (cl-defmethod ess-print-dialect (&context (ess-dialect "R"))
;;   (message "dialect: R"))
;; (cl-defmethod ess-print-dialect (&context (ess-dialect "julia"))
;;   (message "dialect: julia"))

(provide 'ess)

;;; ess.el ends here
