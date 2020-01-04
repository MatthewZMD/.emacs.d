;;; arabxetex.el --- AUCTeX style for `arabxetex.sty' (v1.2.1)

;; Copyright (C) 2017, 2018 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2017-08-12
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

;; This file adds support for `arabxetex.sty' (v1.2.1) from 2015/09/04.
;; `arabxetex.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(TeX-add-style-hook
 "arabxetex"
 (lambda ()

   ;; Run the style hooks for packages required by arabxetex:
   (TeX-run-style-hooks "amsmath" "fontspec" "bidi")

   ;; We need xelatex, so check for the engine here:
   (TeX-check-engine-add-engines 'xetex)

   ;; New macros & environments:
   (let ((langs '("arab"
		  "farsi" "persian"
		  "urdu"
		  "sindhi"
		  "pashto"
		  "ottoman" "turk"
		  "kurdisch"
		  "kashmiri"
		  "malay" "jawi"
		  "uighur")))
     ;; Add \text<language>[option]{...}
     (mapc #'TeX-add-symbols
	   (mapcar
	    (lambda (symbol)
	      (list symbol
		    [ TeX-arg-eval completing-read
				   (TeX-argument-prompt optional nil "Mode")
				   LaTeX-arabxetex-package-options ]
		    t))
	    (mapcar (lambda (lang) (concat "text" lang)) langs)))
     ;;
     ;; Add \begin{<language>}[option] ... \end{<language>}
     (mapc #'LaTeX-add-environments
	   (mapcar
	    (lambda (environment)
	      (list environment
		    #'LaTeX-env-args
		    [ TeX-arg-eval completing-read
				   (TeX-argument-prompt optional nil "Mode")
				   LaTeX-arabxetex-package-options ]))
	    langs))
     ;;
     ;; Fontification
     (when (and (featurep 'font-latex)
		(eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords (mapcar (lambda (lang)
					  (list (concat "text" lang) "[{"))
					langs)
				'textual)))

   ;; Other macros:
   (TeX-add-symbols
    '("textLR" t)
    '("aemph" t)

    ;; 3.3 Transliteration
    '("SetTranslitConvention"
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Mapping")
		    '("dmg" "loc")))
    '("SetTranslitStyle" "Style"))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("textLR" "{"))
			      'textual)
     (font-latex-add-keywords '(("aemph"  "{"))
			      'italic-command)
     (font-latex-add-keywords '(("SetTranslitConvention" "{")
				("SetTranslitStyle"      "{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-arabxetex-package-options
  '("novoc" "voc" "fullvoc" "trans" "utf")
  "Package options for the arabxetex package.")

;;; arabxetex.el ends here
