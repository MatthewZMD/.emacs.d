;;; ess-jags-d.el --- ESS[JAGS] dialect  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2011 Rodney Sparapani

;; Author: Rodney Sparapani
;; Created: 13 March 2008
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

(require 'ess-bugs-d)
(require 'ess-utils)
(require 'ess-inf)
(require 'ess-mode)

(defvar ess-jags-command "jags" "Default JAGS program in PATH.")
(make-local-variable 'ess-jags-command)

(defvar ess-jags-monitor '("") "Default list of variables to monitor.")
(make-local-variable 'ess-jags-monitor)

(defvar ess-jags-thin 1 "Default thinning parameter.")
(make-local-variable 'ess-jags-thin)

(defvar ess-jags-chains 1 "Default number of chains.")
(make-local-variable 'ess-jags-chains)

(defvar ess-jags-burnin 10000 "Default burn-in.")
(make-local-variable 'ess-jags-burnin)

(defvar ess-jags-update 10000 "Default number of updates after burnin.")
(make-local-variable 'ess-jags-update)

(defvar ess-jags-system t "Default whether JAGS recognizes the system command.")

(defvar ess-jags-font-lock-keywords
  (list
   ;; .jag files
   (cons "#.*\n"                        font-lock-comment-face)

   (cons "^[ \t]*\\(model\\|var\\)\\>"
         font-lock-keyword-face)

   (cons (concat "\\<d\\(bern\\|beta\\|bin\\|cat\\|chisq\\|"
                 "dexp\\|dirch\\|exp\\|\\(gen[.]\\)?gamma\\|hyper\\|"
                 "interval\\|lnorm\\|logis\\|mnorm\\|mt\\|multi\\|"
                 "negbin\\|norm\\(mix\\)?\\|par\\|pois\\|sum\\|t\\|"
                 "unif\\|weib\\|wish\\)[ \t\n]*(")
         font-lock-constant-face)

   (cons (concat "\\<\\(abs\\|cos\\|dim\\|\\(i\\)?cloglog\\|equals\\|"
                 "exp\\|for\\|inprod\\|interp[.]line\\|inverse\\|length\\|"
                 "\\(i\\)?logit\\|logdet\\|logfact\\|loggam\\|max\\|mean\\|"
                 "mexp\\|min\\|phi\\|pow\\|probit\\|prod\\|rank\\|round\\|"
                 "sd\\|sin\\|sort\\|sqrt\\|step\\|sum\\|t\\|trunc\\|T\\)[ \t\n]*(")
         font-lock-function-name-face)

   ;; .jmd files
   (cons (concat "\\<\\(adapt\\|cd\\|clear\\|coda\\|data\\|dir\\|"
                 "exit\\|in\\(itialize\\)?\\|load\\|model\\|monitors\\|parameters\\|"
                 "pwd\\|run\\|s\\(amplers\\|ystem\\)\\|to\\|update\\)[ \t\n]")
         font-lock-keyword-face)

   (cons "\\<\\(compile\\|monitor\\)[, \t\n]"
         font-lock-keyword-face)

   (cons "[, \t\n]\\(by\\|chain\\|nchains\\|stem\\|thin\\|type\\)[ \t\n]*("
         font-lock-function-name-face)
   )
  "ESS[JAGS]: Font lock keywords."
  )

(defun ess-jags-switch-to-suffix (suffix &optional jags-chains jags-monitor jags-thin
                                         jags-burnin jags-update)
  "ESS[JAGS]: Switch to file with suffix."
  (find-file (concat ess-bugs-file-dir ess-bugs-file-root suffix))

  (if (equal 0 (buffer-size)) (progn
                                (if (equal ".jag" suffix) (progn
                                                            (insert "var ;\n")
                                                            (insert "model {\n")
                                                            (insert "    for (i in 1:N) {\n    \n")
                                                            (insert "    }\n")
                                                            (insert "}\n")
                                                            (insert "#Local Variables" ":\n")
                                                            (insert "#ess-jags-chains:1\n")
                                                            (insert "#ess-jags-monitor:(\"\")\n")
                                                            (insert "#ess-jags-thin:1\n")
                                                            (insert "#ess-jags-burnin:10000\n")
                                                            (insert "#ess-jags-update:10000\n")
                                                            (insert "#End:\n")
                                                            ))

                                (if (equal ".jmd" suffix) (let
                                                              ((ess-jags-temp-chains "") (ess-jags-temp-monitor "") (ess-jags-temp-chain ""))

                                                            (if jags-chains (setq ess-jags-chains jags-chains))
                                                            (if jags-monitor (setq ess-jags-monitor jags-monitor))
                                                            (if jags-thin (setq ess-jags-thin jags-thin))

                                                            (setq ess-jags-temp-chains
                                                                  (concat "compile, nchains(" (format "%d" ess-jags-chains) ")\n"))

                                                            (setq jags-chains ess-jags-chains)

                                                            (while (< 0 jags-chains)
                                                              (setq ess-jags-temp-chains
                                                                    (concat ess-jags-temp-chains
                                                                            "parameters ## \"" ess-bugs-file-root
                                                                            ".##" (format "%d" jags-chains) "\", chain("
                                                                            (format "%d" jags-chains) ")\n"))
                                                              (setq jags-chains (- jags-chains 1)))

                                                            (setq ess-jags-temp-monitor "")

                                                            (while (and (listp ess-jags-monitor) (consp ess-jags-monitor))
                                                              (if (not (string-equal "" (car ess-jags-monitor)))
                                                                  (setq ess-jags-temp-monitor
                                                                        (concat ess-jags-temp-monitor "monitor "
                                                                                (car ess-jags-monitor) ", thin(" (format "%d" ess-jags-thin) ")\n")))
                                                              (setq ess-jags-monitor (cdr ess-jags-monitor)))

                                                            (insert "model in \"" ess-bugs-file-root ".jag\"\n")
                                                            (insert "data in \"" ess-bugs-file-root ".jdt\"\n")
                                                            (insert (ess-replace-in-string ess-jags-temp-chains "##" "in"))
                                                            (insert "initialize\n")
                                                            ;(insert "update " (format "%d" (* jags-thin jags-burnin)) "\n")
                                                            (insert "update " (format "%d" jags-burnin) "\n")
                                                            (insert ess-jags-temp-monitor)
                                                            (insert "update " (format "%d" (* jags-thin jags-update)) "\n")
                                                            (insert (ess-replace-in-string
                                                                     (ess-replace-in-string ess-jags-temp-chains
                                                                                            "compile, nchains([0-9]+)" "#") "##" "to"))
                                                            (insert "coda "
                                                                    ;(if ess-microsoft-p (if (w32-shell-dos-semantics) "*" "\\*") "\\*")
                                                                    "*, stem(\"" ess-bugs-file-root "\")\n")

                                                            (if ess-jags-system (progn
                                                                                  (insert "system rm -f " ess-bugs-file-root ".ind\n")
                                                                                  (insert "system ln -s " ess-bugs-file-root "index.txt " ess-bugs-file-root ".ind\n")

                                                                                  (setq jags-chains ess-jags-chains)

                                                                                  (while (< 0 jags-chains)
                                                                                    (setq ess-jags-temp-chain (format "%d" jags-chains))

                                        ;.txt not recognized by BOA and impractical to over-ride
                                                                                    (insert "system rm -f " ess-bugs-file-root ess-jags-temp-chain ".out\n")
                                                                                    (insert "system ln -s " ess-bugs-file-root "chain" ess-jags-temp-chain ".txt "
                                                                                            ess-bugs-file-root ess-jags-temp-chain ".out\n")
                                                                                    (setq jags-chains (- jags-chains 1)))))

                                                            (insert "exit\n")
                                                            (insert "Local Variables" ":\n")
                                                            (insert "ess-jags-chains:" (format "%d" ess-jags-chains) "\n")
                                                            (insert "ess-jags-command:\"jags\"\n")
                                                            (insert "End:\n")
                                                            ))
                                ))
  )

(defun ess-jags-na-jmd (jags-command)
  "ESS[JAGS]: Perform the Next-Action for .jmd."
                                        ;(ess-save-and-set-local-variables)
  (if (equal 0 (buffer-size)) (ess-jags-switch-to-suffix ".jmd")
                                        ;else
    (shell)
    (ess-sleep)

    (if (when (fboundp 'w32-shell-dos-semantics)
          (w32-shell-dos-semantics))
        (if (string-equal ":" (substring ess-bugs-file 1 2))
            (progn
              (insert (substring ess-bugs-file 0 2))
              (comint-send-input)
              )
          )
      )

    (insert "cd \"" ess-bugs-file-dir "\"")
    (comint-send-input)

                                        ;    (let ((ess-jags-temp-chains ""))
                                        ;
                                        ;       (while (< 0 jags-chains)
                                        ;           (setq ess-jags-temp-chains
                                        ;               (concat (format "%d " jags-chains) ess-jags-temp-chains))
                                        ;           (setq jags-chains (- jags-chains 1)))

    (insert ess-bugs-batch-pre-command " " jags-command " "
            ess-bugs-file-root ".jmd "

            (if (or (equal shell-file-name "/bin/csh")
                    (equal shell-file-name "/bin/tcsh")
                    (equal shell-file-name "/bin/zsh")
                    (equal shell-file-name "/bin/bash"))
                (concat ">& " ess-bugs-file-root ".jog ")
                                        ;else
              "> " ess-bugs-file-root ".jog 2>&1 ")

                                        ;               ;.txt not recognized by BOA and impractical to over-ride
                                        ;               "&& (rm -f " ess-bugs-file-root ".ind; "
                                        ;               "ln -s " ess-bugs-file-root "index.txt " ess-bugs-file-root ".ind; "
                                        ;               "for i in " ess-jags-temp-chains "; do; "
                                        ;               "rm -f " ess-bugs-file-root "$i.out; "
                                        ;               "ln -s " ess-bugs-file-root "chain$i.txt " ess-bugs-file-root "$i.out; done) "

            ess-bugs-batch-post-command)

    (comint-send-input)
    ))

(defun ess-jags-na-bug ()
  "ESS[JAGS]: Perform Next-Action for .jag"

  (if (equal 0 (buffer-size)) (ess-jags-switch-to-suffix ".jag")
                                        ;else
    (ess-save-and-set-local-variables)
    (ess-jags-switch-to-suffix ".jmd"
                               ess-jags-chains ess-jags-monitor ess-jags-thin ess-jags-burnin ess-jags-update))
  )

;;;###autoload
(define-derived-mode ess-jags-mode ess-bugs-mode "ESS[JAGS]"
  "Major mode for JAGS."
  (setq-local comment-start "#")
  (setq font-lock-defaults '(ess-jags-font-lock-keywords nil t))
  (setq ess-language "S") ; mimic S for ess-smart-underscore
  (unless (and (fboundp 'w32-shell-dos-semantics)
               (w32-shell-dos-semantics))
    (add-hook 'comint-output-filter-functions 'ess-bugs-exit-notify-sh))
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode))

(provide 'ess-jags-d)

;;; ess-jags-d.el ends here
