;;; alltt.el --- AUCTeX style for `alltt.sty'

;; Copyright (C) 2004, 2005, 2014, 2016, 2018 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-04-30
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

;; This file adds support for `alltt.sty'.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-update-font-lock
		  "font-latex"
		  (&optional syntactic-kws))

(TeX-add-style-hook
 "alltt"
 (lambda ()
   (LaTeX-add-environments "alltt")
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
		'("alltt" current-indentation) t)
   (add-to-list 'LaTeX-verbatim-environments-local "alltt")
   ;; Fontification
   (when (and (fboundp 'font-latex-update-font-lock)
	      (eq TeX-install-font-lock 'font-latex-setup))
     ;; Tell font-lock about the update.
     (font-latex-update-font-lock t)))
 LaTeX-dialect)

(defvar LaTeX-alltt-package-options nil
  "Package options for the alltt package.")

;;; alltt.el ends here
