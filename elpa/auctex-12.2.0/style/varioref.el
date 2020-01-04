;;; varioref.el --- AUCTeX style for `varioref.sty' (v1.6b)

;; Copyright (C) 1999, 2013, 2015, 2018, 2019 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@strw.leidenuniv.nl>
;;         Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org

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

;; This file adds support for `varioref.sty' (v1.6b) from 2019/09/08.
;; `varioref.sty' is a standard LaTeX package and part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(TeX-add-style-hook
 "varioref"
 (lambda ()

   (TeX-add-symbols
    ;; 3 The user interface
    '("vref" TeX-arg-ref)
    '("vpageref" [ "Same page text" ] [ "Different page text" ] TeX-arg-ref)
    '("vrefrange" [ "Same page text" ] TeX-arg-ref TeX-arg-ref)
    '("vpagerefrange" [ "Same page text" ] TeX-arg-ref TeX-arg-ref)
    "vpagerefnum"
    '("vpagerefcompare" 4)
    '("vpagerefnearby"  3)
    '("vref*" TeX-arg-ref)
    '("vpageref*" [ "Same page text" ] [ "Different page text" ] TeX-arg-ref)
    '("vrefrange*" [ "Same page text" ] TeX-arg-ref TeX-arg-ref)
    '("vpagerefrange*" [ "Same page text" ] TeX-arg-ref TeX-arg-ref)

    '("Vref" TeX-arg-ref)
    '("Vref*" TeX-arg-ref)

    ;; 5 Customization
    "reftextbefore" "reftextfacebefore"
    "reftextafter"  "reftextfaceafter"
    "reftextfaraway" "vreftextvario"
    "reftextpagerange" "reftexlabelrange"
    "vrefwarning"  "vrefshowerrors"
    '("fullref" TeX-arg-ref))

   ;; Install completion for labels.  Only offer completion for
   ;; commands that take only one reference as an argument
   ;; FIXME: The first 3 entries can be reduced to
   ;; ("\\\\[Vv]ref\\*?{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")  ???
   (setq TeX-complete-list
	 (append
	  '(("\\\\[Vv]ref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
	    ("\\\\vref\\*?{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
	    ("\\\\vref\\*{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
	    ("\\\\fullref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
	    ("\\\\vpageref\\*?\\(\\[[^]]*\\]\\)*{\\([^{}\n\r\\%,]*\\)"
	     2 LaTeX-label-list "}"))
	  TeX-complete-list))

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(;; vref is already in font-latex.el,
				;; so don't add it here again
				("vpageref"      "*[[{")
				("vrefrange"     "*[{{")
				("vpagerefrange" "*[{{")
				("Vref"          "*{")
				("fullref"       "{"))
			      'reference))

   ;; Activate RefTeX reference style.
   (and LaTeX-reftex-ref-style-auto-activate
	(fboundp 'reftex-ref-style-activate)
	(reftex-ref-style-activate "Varioref")))
 LaTeX-dialect)

(defvar LaTeX-varioref-package-options
  '("afrikaans" "american" "arabic" "austrian" "naustrian" "basque"
    "bahasam" "brazil" "breton" "bulgarian" "catalan" "croatian"
    "czech" "danish" "dutch" "english" "esperanto" "finnish" "french"
    "galician" "german" "ngerman" "greek" "icelandic" "italian" "magyar"
    "norsk" "nynorsk" "polish" "portuges" "romanian" "russian"
    "slovak" "slovene" "spanish" "swedish" "turkish" "ukrainian"
    "francais" "germanb" "draft" "final" "space" "nospace")
  "Package options for the varioref package.")

;;; varioref.el ends here
