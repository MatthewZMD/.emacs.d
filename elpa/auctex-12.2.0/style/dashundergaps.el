;;; dashundergaps.el --- AUCTeX style for `dashundergaps.sty' (v2.0d)

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2018-11-24
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `dashundergaps.sty' v2.0d from
;; 2018/11/18.  `dashundergaps.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(defvar LaTeX-dashundergaps-key-val-options
  `(;; 2.1.1 Gap modes
    ("teacher-mode" ("true" "false"))
    ("gap-mode"     ("true" "false"))
    ("teachermode"  ("true" "false"))
    ;; 2.1.2 Gap formatting
    ("gap-format"        ("underline" "double-underline"
			  "dash" "dot" "wave" "blank"))
    ("gap-format-adjust"  ("true" "false"))
    ("teacher-gap-format" ("underline" "double-underline"
			   "dash" "dot" "wave" "blank"))
    ("gap-font" ,(mapcar (lambda (x)
			   (concat TeX-esc x))
			 '("rmfamily" "sffamily" "ttfamily" "mdseries" "bfseries"
			   "upshape" "itshape" "slshape" "scshape"
			   "tiny"  "scriptsize" "footnotesize"
			   "small" "normalsize" "large"
			   "Large" "LARGE" "huge" "Huge" "normalfont")))
    ("dash")
    ("dot")
    ;; 2.1.3 Gap numbers
    ("gap-numbers"        ("true" "false"))
    ("gap-number-format")
    ("numbers")
    ("display-total-gaps" ("true" "false"))
    ("displaynbgaps")
    ;; 2.1.4 Gap widening
    ("gap-widen"          ("true" "false"))
    ("gap-extend-minimum" ,(mapcar (lambda (x)
				     (concat TeX-esc x))
				   (mapcar #'car (LaTeX-length-list))))
    ("gap-extend-percent")
    ("widen"))
  "Key=value options for dashundergaps macro.")

(TeX-add-style-hook
 "dashundergaps"
 (lambda ()

   ;; 2 The user interface
   (TeX-add-symbols
    '("gap"  [ TeX-arg-key-val LaTeX-dashundergaps-key-val-options ] t)
    '("gap*" [ TeX-arg-key-val LaTeX-dashundergaps-key-val-options ] t)

    '("TeacherModeOn" 0)
    '("TeacherModeOff" 0)

    '("dashundergapssetup"
      (TeX-arg-key-val LaTeX-dashundergaps-key-val-options)))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("gap" "*[{"))
			      'textual)
     (font-latex-add-keywords '(("dashundergapssetup" "{")
				("TeacherModeOn"      "")
				("TeacherModeOff"     ""))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-dashundergaps-package-options nil
  "Package options for the dashundergaps package.")

;;; dashundergaps.el ends here
