;;; ess-r-a.el -- Possible local customizations for R with ESS.  -*- lexical-binding: t; -*-

;; Copyright (C) 1997--2005 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <blindglobe@gmail.com>
;; Created: 17 November 1999
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

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

;;; Commentary:

;; The purpose of this file is to demonstrate some of the extras that
;; have been constructed for the ESS R mode; if they prove
;; interesting, then they might be migrated to ess-r-mode, the primary
;; ESS R mode tools.

;;; Code:
(require 'ess-inf)
(require 'ess-r-mode)

;; you can invoke ESS/R from emacs by typing
;;      C-u M-x essr
;; with vsize set to (for example) 40M, and nsize set to 600000.

;; Undefined on non-apple devices
(declare-function ns-do-applescript "nsfns.m" (script))
(declare-function do-applescript "ess-r-a" (script))
(unless (fboundp 'do-applescript)
  (defalias 'do-applescript 'ns-do-applescript))

(defalias 'essr
  (read-kbd-macro
   "C-u M-x R RET - - vsize = 40M SPC - - nsize = 600000 2*RET"))

(defun ess-r-do-region (start end)
  "Send from START to END to R via AppleScript."
  (interactive "r\nP")
  (message "Starting evaluation...")
  (do-applescript (concat
                   "try\n"
                   "tell application \"R\"\n"
                   "activate\n"
                   "with timeout of 0 seconds\n"
                   "cmd \"" (buffer-substring start end)
                   "\"\n"
                   "end timeout\n"
                   "end tell\n"
                   "end try\n"))
  (message "Finished evaluation"))

(defun ess-r-do-line ()
  "Send the current line to R via AppleScript."
  (interactive) ;; "r\nP")
  (message "Starting evaluation...")
  (save-excursion
    (let ((end (point)))
      (move-to-column 0)
      (do-applescript (concat
                       "try\n"
                       "tell application \"R\"\n"
                       "activate\n"
                       "with timeout of 0 seconds\n"
                       "cmd \"" (buffer-substring (point) end)
                       "\"\n"
                       "end timeout\n"
                       "end tell\n"
                       "end try\n"))))
  (message "Finished evaluation"))

(defun ess-r-var (beg end)
  "Load the current region of numbers into an R variable.
Prompts for a variable name. If none is given, it uses a default
variable name, e. BEG and END denote the region in the current
buffer to be sent."
  (interactive "r")
  (save-window-excursion
    (let ((tmp-file (make-temp-file "ess-r-var"))
          cmd
          var)
      (write-region beg end tmp-file)

      ;; Decide on the variable name to use in R; could use completion.
      (setq var (read-string "R Variable name (default e): "))
      (if (equal var "")
          (setq var "e"))

      ;; Command to send to the R process.  Get R to delete the file
      ;; rather than Emacs in case it takes R a long time to run the
      ;; scan command.
      (setq cmd (concat var " <- scan(\""  tmp-file "\"); "
                        "unlink(\"" tmp-file "\")" ))

      ;; Put the output from the scan command into the process buffer so
      ;; the user has a record of it.
      (ess-execute cmd 'buffer))))


;;; Peter Dalgaard's code.
;;; This needs to be cleaned and validated!

(defun pd::set-up-demo ()
  (run-ess-r)
  (split-window-vertically 6)
  (find-file "demos.R")

  ;; Don't need to run this as a function -- ought to be fine if set
  ;; just once.

  (defun ajr::scroll-to-end::peterD (emacs)
    "Goal: map prompt to bottom of the screen after every command.
Alternatively, use the scroll-in-place package, not sure where that
is)."
    (interactive)
    (other-buffer 1)
    (if (= emacs "emacs")
        (setq scroll-up-aggressively t)
      (setq scroll-conservatively -4)) ;; <- change this
    (other-buffer -1))

  (defun show-max-other-window ()
    (interactive)
    (other-window 1)
    (comint-show-maximum-output)
    (other-window -1))

  ;; call this once
  ;; (ajr::scroll-to-end::peterD "emacs")

  (global-set-key [f11] 'show-max-other-window)
  (global-set-key [f12] 'ess-eval-line-visibly-and-step))


 ; Provide package

(provide 'ess-r-a)

;;; ess-r-a.el ends here
