;;; nameref.el --- AUCTeX style for `nameref.sty'

;; Copyright (C) 2013, 2015, 2018 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
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

;; This file adds support for `nameref.sty'

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(TeX-add-style-hook
 "nameref"
 (lambda ()
   (TeX-add-symbols
    '("nameref" TeX-arg-ref)
    '("nameref*" TeX-arg-ref)
    '("Nameref" TeX-arg-ref))

   (setq TeX-complete-list
	 (append
	  '(("\\\\\\(?:N\\|n\\)ameref\\*?{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}"))
	  TeX-complete-list))

   ;, Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-set-syntactic-keywords)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("nameref" "*{")
				("Nameref" "{"))
			      'reference)))
 LaTeX-dialect)

(defvar LaTeX-nameref-package-options nil
  "Package options for nameref.")

;; nameref.el ends here
