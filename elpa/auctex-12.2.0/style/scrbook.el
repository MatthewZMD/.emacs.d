;;; scrbook.el --- AUCTeX style for scrbook.cls

;; Copyright (C) 2002, 2005, 2018 Free Software Foundation

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Created: 2002-09-26
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

;; This file adds support for `scrbook.cls'. This file needs
;; `scrbase.el'.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(TeX-add-style-hook "scrbook"
  (lambda ()
    (LaTeX-largest-level-set "chapter")
    ;; load basic definitons
    (TeX-run-style-hooks "scrbase")
    (TeX-add-symbols
     "chapapp"
     "raggeddictum"
     '("chapappifchapterprefix" "Additional text")
     '("setpartpreamble" [ TeX-arg-KOMA-setpreamble ] [ "Width" ] t)
     '("setchapterpreamble" [ TeX-arg-KOMA-setpreamble ] [ "Width" ] t)
     '("dictum" [ "Author" ] t))
    (LaTeX-section-list-add-locally '("addchap" 1))
    (make-local-variable 'LaTeX-section-label)
    (setq LaTeX-section-label (append
			       LaTeX-section-label
			       '(("addchap" . nil))))
    ;; Definitions for font-latex
    (when (and (featurep 'font-latex)
	       (eq TeX-install-font-lock 'font-latex-setup))
      ;; Textual keywords
      (font-latex-add-keywords '(("addchap" "[{")
				 ("setpartpreamble" "[[{")
				 ("setchapterpreamble" "[[{")
				 ("dictum" "[{"))
			       'textual)
      ;; Sectioning keywords
      (font-latex-add-keywords '(("addchap" "[{")) 'sectioning-1)))
  LaTeX-dialect)

;;; scrbook.el ends here
