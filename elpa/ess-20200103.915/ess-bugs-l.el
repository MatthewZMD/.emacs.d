;;; ess-bugs-l.el --- ESS[BUGS] languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2006-2011 Rodney Sparapani

;; Author: Rodney Sparapani
;; Created: 16 August 2006
;; Maintainer: ESS-help <ess-help@r-project.org>

;; This file is part of ESS

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


;;; Code:

(require 'shell)
(require 'ess-utils)
(defvar ess-bugs-command)
(defvar ess-bugs-chains)
(defvar ess-jags-command)
(defvar ess-jags-chains)
(defvar ess-bugs-default-bins)

(declare-function ess-bugs-na-bug "ess-bugs-d")
(declare-function ess-jags-na-bug "ess-jags-d")
(declare-function ess-bugs-na-bmd "ess-bugs-d")
(declare-function ess-jags-na-jmd "ess-jags-d")

(defgroup ess-bugs nil
  "ESS: BUGS."
  :group 'ess
  :prefix "ess-")

(defcustom ess-bugs-batch-method
  (if (and ess-microsoft-p
           (fboundp 'w32-shell-dos-semantics)
           (w32-shell-dos-semantics))
      'dos
    'sh)
  "Method used by `ess-bugs-batch'.
The default is based on the value of the Emacs variable `system-type'
and, on Windows machines, the function `w32-shell-dos-semantics'.
'sh           if *shell* runs a Bourne-like or a C-like Unix shell
'dos          if *shell* runs a DOS-like Windows shell

Unix users will get 'sh by default.

Windows users running a DOS-like *shell* will get 'dos by default,
while those running a Unix-like *shell* will get 'sh by default.

Users whose default is not 'sh, but are accessing a remote machine with
`telnet' or `ssh', should have the following in their init file:
   (setq-default ess-bugs-batch-method 'sh)"
  :group 'ess-bugs
  :type '(choice (const 'sh :tag "Bourne/C-like Unix Shell")
                 (const 'dos :tag "DOS-like Windows shell")))

(defcustom ess-bugs-batch-post-command
  (if (equal ess-bugs-batch-method 'sh) "&" " ")
  "ESS[BUGS]: Modifiers at the end of the batch BUGS command line."
  :group 'ess-bugs
  :type  'string
  )

(defcustom ess-bugs-batch-pre-command
  (if (equal ess-bugs-batch-method 'sh) "nohup nice time"
    (if ess-microsoft-p "start"))
  "ESS[BUGS]: Modifiers at the beginning of the batch BUGS command line."
  :group 'ess-bugs
  :type  'string
  )


(defcustom ess-bugs-default-burn-in "500"
  "ESS[BUGS]: Burn-in iterations to discard."
  :group 'ess-bugs
  :type  'string
  )

(defcustom ess-bugs-default-update "1000"
  "ESS[BUGS]: Iterations to store."
  :group 'ess-bugs
  :type  'string
  )

(defvar ess-bugs-batch-command ";"
  "ESS[BUGS]: The name of the command to run BUGS in batch mode."
  )

(defvar ess-bugs-file "."
  "ESS[BUGS]: BUGS file with PATH.")

(defvar ess-bugs-file-root "."
  "ESS[BUGS]: Root of BUGS file.")

(defvar ess-bugs-file-suffix "."
  "ESS[BUGS]: Suffix of BUGS file.")

(defvar ess-bugs-file-dir "."
  "ESS[BUGS]: Directory of BUGS file.")

(defvar ess-bugs-file-data "..."
  "ESS[BUGS]: BUGS data file.")

(defcustom ess-bugs-inits-suffix ".in"
  "ESS[BUGS]: BUGS init file suffix."
  :group 'ess-bugs
  :type  'string
  )

(defcustom ess-bugs-data-suffix ".dat"
  "ESS[BUGS]: BUGS data file suffix."
  :group 'ess-bugs
  :type  'string
  )

(defcustom ess-bugs-mode-hook nil
  "ESS[BUGS]: List of functions to call upon entering mode."
  :group 'ess-bugs
  :type 'hook)

(defvar ess-bugs-monitor-vars " "
  "ESS[BUGS]: List of BUGS variables to be written out to a file.")

(defvar ess-bugs-stats-vars " "
  "ESS[BUGS]: List of BUGS variables to be summarized with statistics.")

(defvar ess-bugs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (quote [f2])  #'ess-revert-wisely)
    (define-key map "\C-c\C-c" #'ess-bugs-next-action)
    (define-key map "=" #'ess-bugs-hot-arrow)
    (define-key map "_" #'ess-bugs-hot-arrow)
    map)
  "ESS[BUGS]: Keymap for mode.")

(defvar ess-bugs-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\)  ")(" table)
    (modify-syntax-entry ?. "w" table)
    table)
  "ESS[BUGS]: Syntax table for mode.")

(defun ess-bugs-file ()
  "ESS[BUGS]: Set internal variables dealing with BUGS files.
Set `ess-bugs-file', `ess-bugs-file-root', `ess-bugs-file-suffix'
and `ess-bugs-file-dir'."
  (let ((ess-bugs-temp-string (buffer-name)))
    (setq ess-bugs-file (expand-file-name ess-bugs-temp-string))
    (setq ess-bugs-file-dir
          (convert-standard-filename (file-name-directory ess-bugs-file)))
    (setq ess-bugs-file-root
          (file-name-nondirectory (file-name-sans-extension ess-bugs-file)))

    (if (fboundp 'file-name-extension)
        (setq ess-bugs-file-suffix (file-name-extension ess-bugs-temp-string))
      ;;else
      (setq ess-bugs-file-suffix (car (last (split-string ess-bugs-temp-string "[.]")))))

    (setq ess-bugs-file-suffix
          (downcase (car (split-string (concat "." ess-bugs-file-suffix) "[<]"))))

    (setq ess-bugs-file (concat ess-bugs-file-dir ess-bugs-file-root ess-bugs-file-suffix))
    )
  )

(defun ess-bugs-exit-notify-sh (string)
  "ESS[BUGS]: Detect completion or failure of submitted job and notify the user."
  (let* ((exit-done "\\[[0-9]+\\] *\\+* *\\(Exit\\|Done\\)[^\r\n]*")
         (beg (string-match exit-done string)))
    (if beg (message "%s" (substring string beg (match-end 0))))))

(defun ess-bugs-hot-arrow ()
  "ESS[BUGS]: Substitute <- for = key press"
  (interactive)
  (insert " <- "))

(defun ess-bugs-next-action ()
  "ESS[BUGS/JAGS]: Perform the appropriate next action."
  (interactive)
  (ess-bugs-file)

  (cond ((equal ".bug" ess-bugs-file-suffix) (ess-bugs-na-bug))
        ((equal ".jag" ess-bugs-file-suffix) (ess-jags-na-bug))
        ((equal ".bmd" ess-bugs-file-suffix)
         (ess-save-and-set-local-variables)
         (ess-bugs-na-bmd ess-bugs-command ess-bugs-chains))
        ((equal ".jmd" ess-bugs-file-suffix)
         (ess-save-and-set-local-variables)
         (ess-jags-na-jmd ess-jags-command ess-jags-chains)))
  )

(defun ess-bugs-sci-to-round-4-dp ()
  "ESS[BUGS]: round output from +/-0.000E+/-0 to 4 decimal places."
  (interactive)
  (setq buffer-read-only nil)
  (save-excursion (goto-char 0)
                  (save-match-data (let ((ess-bugs-replacement-string nil)
                                         (ess-bugs-replacement-9 0)
                                         (ess-bugs-replacement-diff 0))
                                     (while (search-forward-regexp "-?[0-9][.][0-9][0-9][0-9]E[+-][0-9]" nil t)
                                       (setq ess-bugs-replacement-string
                                             (int-to-string (string-to-number (match-string 0))))
                                       (setq ess-bugs-replacement-diff (- (match-end 0) (match-beginning 0)))
                                       (save-match-data
                                         (setq ess-bugs-replacement-9
                                               (string-match "99999999999$" ess-bugs-replacement-string))

                                         (if (not ess-bugs-replacement-9)
                                             (setq ess-bugs-replacement-9
                                                   (string-match "000000000001$" ess-bugs-replacement-string))))

                                       (if ess-bugs-replacement-9
                                           (setq ess-bugs-replacement-string
                                                 (substring ess-bugs-replacement-string 0 ess-bugs-replacement-9)))

                                       (setq ess-bugs-replacement-diff
                                             (- ess-bugs-replacement-diff (string-width ess-bugs-replacement-string)))

                                       (while (> ess-bugs-replacement-diff 0)
                                         (setq ess-bugs-replacement-string (concat ess-bugs-replacement-string " "))
                                         (setq ess-bugs-replacement-diff (- ess-bugs-replacement-diff 1)))

                                       (replace-match ess-bugs-replacement-string))))))

;;; ESS[BUGS-Shell] for running BUGS interactively
(defgroup ess-bugs-shell nil
  "ESS: BUGS-Shell."
  :group 'ess-bugs
  :prefix "ess-")

(defcustom ess-bugs-shell-buffer-name "BUGS"
  "ESS[BUGS-Shell]: The name of the BUGS-Shell buffer."
  :group 'ess-bugs-shell
  :type  'string)

(defcustom ess-bugs-shell-command "OpenBUGS"
  "ESS[BUGS-Shell]: The name of the command to run BUGS interactively.

Set to the name of the batch BUGS script that comes with ESS or
to the name of BUGS command. Make sure it is in your PATH or
add path to the command name."
  :group 'ess-bugs-shell
  :type  'string)

(defcustom ess-bugs-shell-default-output-file-root "bugs"
  "ESS[BUGS-Shell]: Default value for the root of output files."
  :group 'ess-bugs-shell
  :type  'string)

(defcustom ess-bugs-shell-mode-hook nil
  "ESS[BUGS-Shell]: List of functions to call upon entering mode."
  :group 'ess-bugs-shell
  :type 'hook)

(defun ess-bugs-shell ()
  "Create a buffer with BUGS running as a subprocess."
  (interactive)
  (pop-to-buffer-same-window (concat "*" ess-bugs-shell-buffer-name "*"))
  (make-comint ess-bugs-shell-buffer-name ess-bugs-shell-command nil
               ess-bugs-default-bins ess-bugs-shell-default-output-file-root)
  (comint-mode)
  (setq shell-dirtrackp t
        major-mode 'bugs-shell-mode
        mode-name "ESS[BUGS-Shell]"
        comint-prompt-regexp "^Bugs> *")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ess-bugs-font-lock-keywords nil t))
  (run-mode-hooks 'ess-bugs-shell-mode-hook)
  )

(provide 'ess-bugs-l)

;;; ess-bugs-l.el ends here
