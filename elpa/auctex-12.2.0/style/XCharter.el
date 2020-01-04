;;; XCharter.el --- AUCTeX style for `XCharter.sty' (v1.094)

;; Copyright (C) 2014, 2017, 2018 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-10-30
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

;; This file adds support for `XCharter.sty' (v1.094) from 2017/08/08.
;; `XCharter.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(TeX-add-style-hook
 "XCharter"
 (lambda ()

   ;; Run style hook for various packages loaded by XCharter
   (TeX-run-style-hooks "textcomp" "fontaxes")

   ;; New symbols
   (TeX-add-symbols

    ;; Only preamble commands
    '("useosf"  0)
    '("useosfI" 0)

    ;; Text commands
    '("textsu"     t)   ; superior figures
    '("sustyle"   -1)   ;
    '("textin"     t)   ; inferior figures
    '("instyle"   -1)   ;
    '("textlf"     t)   ; lining figures
    '("lfstyle"   -1)   ;
    '("textosf"    t)   ; oldstyle figures
    '("textosfI"   t)   ; oldstyle figures alternate
    '("osfstyle"  -1)   ; whatever oldstyle option is in force
    '("textnumerator"   t) ; numerators
    '("textnu"          t) ;
    '("textdenominator" t) ; denominators
    '("textde"          t) ;
    '("textfrac"        2))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("textsu"    "{")
				("textin"    "{")
                                ("textlf"    "{")
                                ("textosf"   "{")
                                ("textosfI"  "{")
				("textnumerator"   "{")
				("textnu"          "{")
				("textdenominator" "{")
				("textde"          "{")
				("textfrac"        "{{"))
                              'type-command)
     (font-latex-add-keywords '(("sustyle"   "")
				("instyle"   "")
                                ("lfstyle"   "")
                                ("osfstyle"  ""))
                              'type-declaration)))
 LaTeX-dialect)

(defvar LaTeX-XCharter-package-options
  '("lining" "lf" "oldstyle" "osf" "oldstyleI" "osfI"
    "scaled" "sups" "scosf")
  "Package options for the XCharter package.")

;;; XCharter.el ends here
