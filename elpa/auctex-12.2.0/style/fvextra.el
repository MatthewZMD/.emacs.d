;;; fvextra.el --- AUCTeX style for `fvextra.sty' (v1.4)

;; Copyright (C) 2017, 2019 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2017-03-05
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

;; This file adds support for `fvextra.sty' (v1.4) from 2019/02/04.
;; `fvextra.sty' is part of TeXLive.

;;; Code:

;; Needed for compiling `cl-pushnew':
(eval-when-compile
  (require 'cl-lib))

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(declare-function font-latex-update-font-lock
		  "font-latex"
		  (&optional syntactic-kws))

(declare-function LaTeX-color-definecolor-list "color" ())
(declare-function LaTeX-xcolor-definecolor-list "xcolor" ())

;; Defined in fancyvrb.el:
(defvar LaTeX-fancyvrb-key-val-options-local)

(defvar LaTeX-fvextra-key-val-options
  '(;; 3 General options
    ("beameroverlays" ("true" "false"))
    ("curlyquotes" ("true" "false"))
    ("extra" ("true" "false"))
    ("fontencoding" (;; Reset to default document font encoding
		     "none"
		     ;; 128+ glyph encodings (text)
		     "OT1" "OT2" "OT3" "OT4" "OT6"
		     ;; 256 glyph encodings (text)
		     "T1" "T2A" "T2B" "T2C" "T3" "T4" "T5"
		     ;; 256 glyph encodings (text extended)
		     "X2"
		     ;; Other encodings
		     "LY1" "LV1" "LGR"))
    ("highlightcolor")
    ("highlightlines")
    ("linenos" ("true" "false"))
    ("mathescape" ("true" "false"))
    ("numberfirstline" ("true" "false"))
    ;; ("numbers" ("none" "left" "right" "both"))
    ("retokenize" ("true" "false"))
    ("space" ("\\textvisiblespace"))
    ("spacecolor" ("none"))
    ("stepnumberfromfirst" ("true" "false"))
    ("stepnumberoffsetvalues" ("true" "false"))
    ("tab" ("\\FancyVerbTab"))
    ("tabcolor" ("none"))
    ;; 7.1 Line breaking options
    ("breakafter" ("none"))
    ("breakaftergroup" ("true" "false"))
    ("breakaftersymbolpre")
    ("breakaftersymbolpost")
    ("breakanywhere" ("true" "false"))
    ("breakanywheresymbolpre")
    ("breakanywheresymbolpost")
    ("breakautoindent" ("true" "false"))
    ("breakbefore")
    ("breakbeforegroup" ("true" "false"))
    ("breakbeforesymbolpre")
    ("breakbeforesymbolpost")
    ("breakindent")
    ("breakindentnchars")
    ("breaklines" ("true" "false"))
    ("breaksymbol")
    ("breaksymbolleft")
    ("breaksymbolright")
    ("breaksymbolindent")
    ("breaksymbolindentnchars")
    ("breaksymbolindentleft")
    ("breaksymbolindentleftnchars")
    ("breaksymbolindentright")
    ("breaksymbolindentrightnchars")
    ("breaksymbolsep")
    ("breaksymbolsepnchars")
    ("breaksymbolsepleft")
    ("breaksymbolsepleftnchars")
    ("breaksymbolsepright")
    ("breaksymbolseprightnchars"))
  "Key=value options for fvextra macros and environments.")

(defun LaTeX-fvextra-update-key-val ()
  "Update `LaTeX-fancyvrb-key-val-options-local' with key=vals from \"fvextra.sty\"."
  ;; Delete the key "numbers" from `LaTeX-fancyvrb-key-val-options-local':
  (setq LaTeX-fancyvrb-key-val-options-local
	(assq-delete-all (car (assoc "numbers" LaTeX-fancyvrb-key-val-options-local))
			 LaTeX-fancyvrb-key-val-options-local))
  ;; Add the key with "both" value:
  (add-to-list 'LaTeX-fancyvrb-key-val-options-local
	       '("numbers" ("none" "left" "right" "both")))
  ;; Add color values to resp. keys:
  (when (or (member "xcolor" (TeX-style-list))
	    (member "color" (TeX-style-list)))
    (let* ((colorcmd (if (member "xcolor" (TeX-style-list))
			 #'LaTeX-xcolor-definecolor-list
		       #'LaTeX-color-definecolor-list))
	   (keys '("highlightcolor"
		   "rulecolor"
		   "fillcolor"
		   "tabcolor"
		   "spacecolor"))
	   (tmp (copy-alist LaTeX-fancyvrb-key-val-options-local)))
      (dolist (x keys)
	(setq tmp (assq-delete-all (car (assoc x tmp)) tmp))
	(if (string= x "highlightcolor")
	    (cl-pushnew (list x (mapcar #'car (funcall colorcmd))) tmp :test #'equal)
	  (cl-pushnew (list x (append '("none") (mapcar #'car (funcall colorcmd)))) tmp :test #'equal)))
      (setq LaTeX-fancyvrb-key-val-options-local
	    (copy-alist tmp)))))

(add-hook 'TeX-auto-cleanup-hook #'LaTeX-fvextra-update-key-val t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "fvextra"
 (lambda ()

   ;; Run the style hook for "fancyvrb"
   (TeX-run-style-hooks "fancyvrb")

   ;; Append `LaTeX-fvextra-key-val' to `LaTeX-fancyvrb-key-val-options-local':
   (setq LaTeX-fancyvrb-key-val-options-local
	 (append LaTeX-fvextra-key-val-options
		 LaTeX-fancyvrb-key-val-options-local))

   (TeX-add-symbols
    ;; 4.1 Inline formatting with \fvinlineset
    '("fvinlineset" (TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local))

    ;; 4.2 Line and text formatting
    "FancyVerbFormatText"

    ;; 6 New commands and environments
    ;; 6.1 \EscVerb
    '("EscVerb"
      [ TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local ] "Text")
    '("EscVerb*"
      [ TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local ] "Text")

    ;; 7.3.2 Breaks within macro arguments
    "FancyVerbBreakStart"
    "FancyVerbBreakStop"

    ;; 7.3.3 Customizing break behavior
    "FancyVerbBreakAnywhereBreak"
    "FancyVerbBreakBeforeBreak"
    "FancyVerbBreakAfterBreak")

   ;; Add \EscVerb*? to `LaTeX-verbatim-macros-with-braces-local':
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local
		"EscVerb" t)
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local
		"EscVerb*" t)

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-update-font-lock)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fvinlineset" "{"))
			      'function)
     (font-latex-add-keywords '(("EscVerb"     "*["))
			      'textual)
     (font-latex-update-font-lock t)) )
 LaTeX-dialect)

(defvar LaTeX-fvextra-package-options nil
  "Package options for the fvextra package.")

;;; fvextra.el ends here
