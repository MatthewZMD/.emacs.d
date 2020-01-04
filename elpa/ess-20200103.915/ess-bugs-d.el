;;; ess-bugs-d.el --- ESS[BUGS] dialect  -*- lexical-binding: t; -*-

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

(require 'ess-bugs-l)
(require 'ess-utils)
(require 'ess-inf)
(require 'ess-mode)

(defvar ess-bugs-command "OpenBUGS" "Default BUGS program in PATH.")
(make-local-variable 'ess-bugs-command)

(defvar ess-bugs-monitor '("") "Default list of variables to monitor.")
(make-local-variable 'ess-bugs-monitor)

(defvar ess-bugs-thin 1 "Default thinning parameter.")
(make-local-variable 'ess-bugs-thin)

(defvar ess-bugs-chains 1 "Default number of chains.")
(make-local-variable 'ess-bugs-chains)

(defvar ess-bugs-burnin 10000 "Default burn-in.")
(make-local-variable 'ess-bugs-burnin)

(defvar ess-bugs-update 10000 "Default number of updates after burnin.")
(make-local-variable 'ess-bugs-update)

(defvar ess-bugs-system nil "Default whether BUGS recognizes the system command.")

(defvar ess-bugs-font-lock-keywords
  (list
   ;; .bug files
   (cons "#.*\n"                        font-lock-comment-face)

   (cons "^[ \t]*\\(model\\|var\\)\\>"
         font-lock-keyword-face)

   (cons (concat "\\<d\\(bern\\|beta\\|bin\\|cat\\|chisq\\|"
                 "dexp\\|dirch\\|exp\\|\\(gen[.]\\)?gamma\\|hyper\\|"
                 "interval\\|lnorm\\|logis\\|mnorm\\|mt\\|multi\\|"
                 "negbin\\|norm\\(mix\\)?\\|par\\|pois\\|sum\\|t\\|"
                 "unif\\|weib\\|wish\\)[ \t\n]*(")
         font-lock-constant-face)

   (cons (concat "\\<\\(abs\\|cos\\|C\\|dim\\|\\(i\\)?cloglog\\|equals\\|"
                 "exp\\|for\\|inprod\\|interp[.]line\\|inverse\\|length\\|"
                 "\\(i\\)?logit\\|logdet\\|logfact\\|loggam\\|max\\|mean\\|"
                 "mexp\\|min\\|phi\\|pow\\|probit\\|prod\\|rank\\|round\\|"
                 "sd\\|sin\\|sort\\|sqrt\\|step\\|sum\\|t\\|trunc\\|T\\)[ \t\n]*(")
         font-lock-function-name-face)

   ;; .bmd files
   (cons (concat (regexp-opt '(
                               "dicClear" "dicSet" "dicStats"
                               "infoMemory" "infoModules" "infoNodeMethods"
                               "infoNodeTypes" "infoNodeValues"
                               "infoUpdatersbyDepth" "infoUpdatersbyName"
                               "modelCheck" "modelCompile" "modelData"
                               "modelDisable" "modelEnable" "modelGenInits"
                               "modelInits" "modelPrecision" "modelQuit"
                               "modelSaveState" "modelSetAP" "modelSetIts"
                               "modelSetOR" "modelSetRN" "modelUpdate"
                               "ranksClear" "ranksSet" "ranksStats"
                               "samplesAutoC" "samplesBgr" "samplesCoda"
                               "samplesDensity" "samplesHistory" "samplesSet"
                               "sampleStats" "samplesThin"
                               "summaryClear" "summarySet" "summaryStats"
                               ) 'words) "(")
         font-lock-function-name-face)

   (cons (concat (regexp-opt '("Local Variables" "End") 'words) ":")
         font-lock-keyword-face)
   )
  "ESS[BUGS]: Font lock keywords."
  )

(defun ess-bugs-switch-to-suffix (suffix &optional bugs-chains bugs-monitor bugs-thin
                                         bugs-burnin bugs-update)
  "ESS[BUGS]: Switch to file with suffix."
  (find-file (concat ess-bugs-file-dir ess-bugs-file-root suffix))

  (if (equal 0 (buffer-size)) (progn
                                (if (equal ".bug" suffix) (progn
                                        ;(insert "var ;\n")
                                                            (insert "model {\n")
                                                            (insert "    for (i in 1:N) {\n    \n")
                                                            (insert "    }\n")
                                                            (insert "}\n")
                                                            (insert "#Local Variables" ":\n")
                                        ;           (insert "#enable-local-variables: :all\n")
                                                            (insert "#ess-bugs-chains:1\n")
                                                            (insert "#ess-bugs-monitor:(\"\")\n")
                                                            (insert "#ess-bugs-thin:1\n")
                                                            (insert "#ess-bugs-burnin:10000\n")
                                                            (insert "#ess-bugs-update:10000\n")
                                                            (insert "#End:\n")
                                                            ))

                                (if (equal ".bmd" suffix) (let
                                                              ((ess-bugs-temp-chains "") (ess-bugs-temp-monitor ""))

                                                            (if bugs-chains (setq ess-bugs-chains bugs-chains))
                                                            (if bugs-monitor (setq ess-bugs-monitor bugs-monitor))
                                                            (if bugs-thin (setq ess-bugs-thin bugs-thin))

                                                            (setq ess-bugs-temp-chains
                                                                  (concat "modelCompile(" (format "%d" ess-bugs-chains) ")\n"))

                                                            (setq bugs-chains ess-bugs-chains)

                                                            (while (< 0 bugs-chains)
                                                              (setq ess-bugs-temp-chains
                                                                    (concat ess-bugs-temp-chains
                                                                            "modelInits('" ess-bugs-file-root
                                                                            ".##" (format "%d" bugs-chains) "', "
                                                                            (format "%d" bugs-chains) ")\n"))
                                                              (setq bugs-chains (- bugs-chains 1)))

                                                            (setq ess-bugs-temp-monitor "")

                                                            (while (and (listp ess-bugs-monitor) (consp ess-bugs-monitor))
                                                              (if (not (string-equal "" (car ess-bugs-monitor)))
                                                                  (setq ess-bugs-temp-monitor
                                                                        (concat ess-bugs-temp-monitor "samplesSet('"
                                                                                (car ess-bugs-monitor)
                                        ;", thin(" (format "%d" ess-bugs-thin)
                                                                                "')\n")))
                                                              (setq ess-bugs-monitor (cdr ess-bugs-monitor)))

                                                            (insert "modelCheck('" ess-bugs-file-root ".bug')\n")
                                                            (insert "modelData('" ess-bugs-file-root ".bdt')\n")
                                                            (insert (ess-replace-in-string ess-bugs-temp-chains "##" "in"))
                                                            (insert "modelGenInits()\n")
                                                            (insert "modelUpdate(" (format "%d" bugs-burnin) ")\n")
                                        ;(insert "modelUpdate(" (format "%d" (* bugs-thin bugs-burnin)) ")\n")
                                                            (insert ess-bugs-temp-monitor)
                                                            (insert "modelUpdate(" (format "%d" (* bugs-thin bugs-update)) ")\n")
                                        ;           (insert (ess-replace-in-string
                                        ;               (ess-replace-in-string ess-bugs-temp-chains
                                        ;                   "modelCompile([0-9]+)" "#") "##" "to"))

                                                            (if (< 1 bugs-thin) (insert "samplesThin(" (format "%d" bugs-thin) ")\n"))

                                                            (insert "samplesCoda('*', '" ess-bugs-file-root "')\n")

                                        ;           (if ess-bugs-system (progn
                                        ;               (insert "system rm -f " ess-bugs-file-root ".ind\n")
                                        ;               (insert "system ln -s " ess-bugs-file-root "index.txt " ess-bugs-file-root ".ind\n")

                                        ;               (setq bugs-chains ess-bugs-chains)

                                        ;               (while (< 0 bugs-chains)
                                        ;                   (setq ess-bugs-temp-chain (format "%d" bugs-chains))

                                        ;                   ;.txt not recognized by BOA and impractical to over-ride
                                        ;                   (insert "system rm -f " ess-bugs-file-root ess-bugs-temp-chain ".out\n")
                                        ;                   (insert "system ln -s " ess-bugs-file-root "chain" ess-bugs-temp-chain ".txt "
                                        ;                       ess-bugs-file-root ess-bugs-temp-chain ".out\n")
                                        ;                   (setq bugs-chains (- bugs-chains 1)))))

                                                            (insert "modelQuit()\n")
                                                            (insert "Local Variables" ":\n")
                                        ;           (insert "enable-local-variables: :all\n")
                                                            (insert "ess-bugs-chains:" (format "%d" ess-bugs-chains) "\n")
                                                            (insert "ess-bugs-command:\"" ess-bugs-command "\"\n")
                                                            (insert "End:\n")
                                                            ))
                                ))
  )

(defun ess-bugs-na-bmd (bugs-command)
  "ESS[BUGS]: Perform the Next-Action for .bmd."
                                        ;(ess-save-and-set-local-variables)
  (if (equal 0 (buffer-size)) (ess-bugs-switch-to-suffix ".bmd")
                                        ;else
    (shell)
    (ess-sleep)

    (when (and (when (fboundp 'w32-shell-dos-semantics)
                 (w32-shell-dos-semantics))
               (string-equal ":" (substring ess-bugs-file 1 2)))
      (insert (substring ess-bugs-file 0 2)))

    (comint-send-input)
    (insert "cd \"" ess-bugs-file-dir "\"")
    (comint-send-input)

                                        ;    (let ((ess-bugs-temp-chains ""))
                                        ;
                                        ;       (while (< 0 bugs-chains)
                                        ;           (setq ess-bugs-temp-chains
                                        ;               (concat (format "%d " bugs-chains) ess-bugs-temp-chains))
                                        ;           (setq bugs-chains (- bugs-chains 1)))

    ;; (insert "echo '"
    ;;       ess-bugs-batch-pre-command " " bugs-command " < "
    ;;       ess-bugs-file-root ".bmd > " ess-bugs-file-root ".bog 2>&1 "
    ;;       ess-bugs-batch-post-command "' > " ess-bugs-file-root ".bsh")
    ;; (comint-send-input)

    ;; (insert "at -f " ess-bugs-file-root ".bsh now")

    ;; (comint-send-input)

    (insert "echo '"
            ess-bugs-batch-pre-command " " bugs-command " < "
            ess-bugs-file-root ".bmd > " ess-bugs-file-root ".bog 2>&1 "
            ess-bugs-batch-post-command "' | at now")

    (comint-send-input)
    ))

(defun ess-bugs-na-bug ()
  "ESS[BUGS]: Perform Next-Action for .bug"

  (if (equal 0 (buffer-size)) (ess-bugs-switch-to-suffix ".bug")
                                        ;else
    (ess-save-and-set-local-variables)
    (ess-bugs-switch-to-suffix ".bmd"
                               ess-bugs-chains ess-bugs-monitor ess-bugs-thin ess-bugs-burnin ess-bugs-update))
  )

;;;###autoload
(define-derived-mode ess-bugs-mode ess-mode "ESS[BUGS]"
  "Major mode for BUGS."
  (setq-local comment-start "#")
  (setq font-lock-defaults '(ess-bugs-font-lock-keywords nil t))
  (setq ess-language "S") ; mimic S for ess-smart-underscore
  (unless (when (fboundp 'w32-shell-dos-semantics)
            (w32-shell-dos-semantics))
    (add-hook 'comint-output-filter-functions 'ess-bugs-exit-notify-sh)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode))

(defun ess-sci-to-dec ()
  "For BUGS/S family: Express +/-0.000E+/-0 or +/-0.0e+/-00 as a decimal."
  (interactive)
  (setq buffer-read-only nil)
  (save-excursion (goto-char 0)
                  (save-match-data (let ((ess-temp-replacement-string nil)
                                         (ess-temp-replacement-9 0)
                                         (ess-temp-replacement-diff 0))
                                     (while (search-forward-regexp "-?[0-9][.][0-9][0-9]?[0-9]?[Ee][+-][0-9][0-9]?" nil t)
                                       (setq ess-temp-replacement-string
                                             (int-to-string (string-to-number (match-string 0))))
                                       (setq ess-temp-replacement-diff (- (match-end 0) (match-beginning 0)))
                                       (save-match-data
                                         (setq ess-temp-replacement-9
                                               (string-match "99999999999$" ess-temp-replacement-string))

                                         (if (not ess-temp-replacement-9)
                                             (setq ess-temp-replacement-9
                                                   (string-match "000000000001$" ess-temp-replacement-string))))

                                       (if ess-temp-replacement-9
                                           (setq ess-temp-replacement-string
                                                 (substring ess-temp-replacement-string 0 ess-temp-replacement-9)))

                                       (setq ess-temp-replacement-diff
                                             (- ess-temp-replacement-diff (string-width ess-temp-replacement-string)))

                                       (while (> ess-temp-replacement-diff 0)
                                         (setq ess-temp-replacement-string (concat ess-temp-replacement-string " "))
                                         (setq ess-temp-replacement-diff (- ess-temp-replacement-diff 1)))

                                       (replace-match ess-temp-replacement-string))))))

(provide 'ess-bugs-d)

;;; ess-bugs-d.el ends here
