;;; bidi.el --- AUCTeX style for the (XeLaTeX) bidi package

;; Copyright (C) 2016--2018 Free Software Foundation, Inc.

;; Author: Uwe Brauer <oub@mat.ucm.es>
;; Created: 2016-03-06
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

;; This file adds support for the bidi package version 31.7

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(defvar LaTeX-bidi-package-options
  '("RTLdocument" "rldocument" "extrafootnotefeatures")
  "Package options for the bidi package.")

(defun LaTeX-env-bidi-bib (environment)
  "Insert bidi-ENVIRONMENT and a bibitem."
  (LaTeX-insert-environment environment)
  (end-of-line 0)
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

(TeX-add-style-hook
 "bidi"
 (lambda ()
   ;; bidi.sty requires xelatex, so set the engine
   (TeX-check-engine-add-engines 'xetex)

   ;; 1.4 Turning TeX--XeT features on and off
   (TeX-add-symbols
    '("TeXXeTOn" 0)
    '("TeXXeTOff" 0))

   ;; 1.5 Options of The Package: These macros rely on package option
   ;; "extrafootnotefeatures".  So check for it first
   (when (LaTeX-provided-package-options-member "bidi" "extrafootnotefeatures")
     (TeX-add-symbols
      '("normalfootnotes" 0)
      '("twocolumnfootnotes" 0)
      '("threecolumnfootnotes" 0)
      '("fourcolumnfootnotes" 0)
      '("fivecolumnfootnotes" 0)
      '("sixcolumnfootnotes" 0)
      '("sevencolumnfootnotes" 0)
      '("eightcolumnfootnotes" 0)
      '("ninecolumnfootnotes" 0)
      '("tencolumnfootnotes" 0)
      '("RTLcolumnfootnotes" 0)
      '("LTRcolumnfootnotes" 0)
      '("paragraphfootnotes" 0)
      '("setLTRparagraphfootnotes" 0)
      '("setRTLparagraphfootnotes" 0)))

   (TeX-add-symbols
    ;; 1.6 Paragraph Switching Commands
    '("setLTR" 0)
    '("setRTL" 0)
    '("setLR" 0)
    '("setRL" 0)
    '("unsetRL" 0)
    '("unsetRTL" 0)
    '("unsetLTR" 0)

    ;; 1.8 Typesetting Short LTR and RTL Texts
    '("LR" 1)
    '("LRE" 1)
    '("RLE" 1)
    '("RL" 1)

    ;; 1.9 Footnotes
    '("LTRfootnote"
      (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
      t)
    '("RTLfootnote"
      (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
      t)
    '("setfootnoteRL" 0)
    '("setfootnoteLR" 0)
    '("unsetfootnoteRL" 0)
    '("LTRthanks" 1)
    '("RTLthanks" 1)
    '("LTRfootnotetext"
      (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
      t)
    '("RTLfootnotetext"
      (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
      t)

    ;; 1.9.1 Footnote Rule
    '("autofootnoterule"  0)
    '("rightfootnoterule" 0)
    '("leftfootnoterule" 0)
    '("LRfootnoterule" 0)
    '("textwidthfootnoterule" 0)
    '("SplitFootnoteRule" 0)
    '("debugfootnotedirection" 0)

    ;; 1.10 Two Column Typesetting
    '("RTLdblcol" 0)
    '("LTRdblcol" 0)

    ;; 1.11 RTL cases
    '("RTLcases" t)

    ;; 1.12 Typesetting Logos
    '("XeTeX" 0)
    '("XeLaTeX" 0)

    ;; 1.13 Separation Mark
    '("SepMark" "Mark")

    ;; 1.20 \setLTRbibitems, \setRTLbibitems, and \setdefaultbibitems
    ;; commands
    '("setLTRbibitems" 0)
    '("setRTLbibitems" 0)
    '("setdefaultbibitems" 0)

    ;; 1.21 Typesetting margin par
    '("setRTLmarginpar" 0)
    '("setLTRmarginpar" 0)
    '("setdefaultmarginpar" 0)
    '("LTRmarginpar" [ "Left margin text" ] "Text")
    '("RTLmarginpar" [ "Left margin text" ] "Text"))

   (LaTeX-add-environments
    ;; 1.7 Pargraph Switching Environments
    "LTR"
    "RTL"

    ;; 1.18 LTRitems and RTLitems Environments
    '("RTLitems" LaTeX-env-item)
    '("LTRitems" LaTeX-env-item)

    ;; 1.19 LTRbibitems and RTLbibitems Environments
    '("LTRbibitems"  LaTeX-env-bidi-bib)
    '("RTLbibitems"  LaTeX-env-bidi-bib))

   ;; Append "LTRbibitems" & "RTLbibitems" to `LaTeX-item-list':
   (make-local-variable 'LaTeX-item-list)
   (dolist (env '("LTRbibitems" "RTLbibitems"))
     (add-to-list 'LaTeX-item-list `(,env . LaTeX-item-bib) t))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("LR"        "{")
				("LRE"       "{")
				("RLE"       "{")
				("RL"        "{")
				("LTRthanks" "{")
				("RTLthanks" "{"))
			      'textual)
     (font-latex-add-keywords '(("LTRfootnote"     "[{")
				("RTLfootnote"     "[{")
				("LTRfootnotetext" "[{")
				("RTLfootnotetext" "[{")
				("LTRmarginpar"    "[{")
				("RTLmarginpar"    "[{"))
			      'reference)
     (font-latex-add-keywords '(("XeTeX"   "")
				("XeLaTeX" "")
				("SepMark" "{"))
			      'function)))
 LaTeX-dialect)

;;; bidi.el ends here
