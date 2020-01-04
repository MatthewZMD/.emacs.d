;;; url.el --- AUCTeX style for `url.sty'

;; Copyright (C) 2004, 2005, 2018 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-10-13
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

;; This file adds support for `url.sty'.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(declare-function font-latex-update-font-lock
		  "font-latex"
		  (&optional syntactic-kws))

(TeX-add-style-hook
 "url"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    "Url"
    "UrlBigBreakPenalty"
    "UrlBigBreaks"
    "UrlBreakPenalty"
    "UrlBreaks"
    "UrlFont"
    "UrlLeft"
    "UrlNoBreaks"
    "UrlOrds"
    "UrlRight"
    "UrlSpecials"
    '("path" (TeX-arg-verb-delim-or-brace "Path"))
    ;; "hyperref" redefines \url so that the argument is only in
    ;; braces.  We check here if hyperref is loaded:
    '("url" (TeX-arg-conditional (member "hyperref" (TeX-style-list))
				 ("Url")
			       ((TeX-arg-verb-delim-or-brace "Url"))))
    "urldef"
    '("urlstyle" TeX-arg-urlstyle))

   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   ;; hyperref.el has some code to remove "url" from
   ;; `LaTeX-verbatim-macros-with-delims-local', but we check here as
   ;; well if "hyperref" is already loaded:
   (unless (member "hyperref" (TeX-style-list))
     (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url"))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-update-font-lock)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("path" "") ("url" "")) 'reference)
     (font-latex-add-keywords '(("Url" "")
				("UrlBigBreakPenalty" "")
				("UrlBigBreaks" "")
				("UrlBreakPenalty" "")
				("UrlBreaks" "")
				("UrlFont" "")
				("UrlLeft" "")
				("UrlNoBreaks" "")
				("UrlOrds" "")
				("UrlRight" "")
				("UrlSpecials" "")
				("urldef" "")
				("urlstyle" "{"))
			      'variable)
     ;; Tell font-lock about the update.
     (font-latex-update-font-lock t)))
 LaTeX-dialect)

(defun TeX-arg-urlstyle (optional &optional prompt)
  "Prompt for style used in \\urlstyle with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Style")
		    (mapcar 'list '("rm" "same" "sf" "tt"))
		    nil t)
   optional))

(defvar LaTeX-url-package-options '("hyphens" "obeyspaces" "spaces" "LY1"
				    "T1" "allowmove")
  "Package options for the url package.")

;;; url.el ends here
