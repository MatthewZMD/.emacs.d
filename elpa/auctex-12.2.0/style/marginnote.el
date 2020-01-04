;;; marginnote.el --- AUCTeX style for `marginnote.sty' (v1.4)

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2018-07-07
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

;; This file adds support for `marginnote.sty' (v1.4) from 2018/07/01.
;; `marginnote.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(TeX-add-style-hook
 "marginnote"
 (lambda ()

   (TeX-add-symbols
    '("marginnote" [ "Left margin text" ] "Text"
      [ TeX-arg-length "Vertical offset" ] )

    '("marginnotetextwidth" 0)

    '("marginnotevadjust" 0)

    '("raggedleftmarginnote" 0)

    '("raggedrightmarginnote" 0)

    '("marginfont" 0))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("marginnote"  "[{["))
			      'reference)))
 LaTeX-dialect)

(defvar LaTeX-marginnote-package-options
  '("fulladjust" "heightadjust" "depthadjust" "noadjust"
    "parboxrestore" "noparboxrestore")
  "Package options for the marginnote package.")

;;; marginnote.el ends here
