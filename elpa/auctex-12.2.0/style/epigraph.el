;;; epigraph.el --- AUCTeX style for `epigraph.sty' v1.5c

;; Copyright (C) 2012, 2017, 2018 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2012-02-11
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

;; This file adds support for `epigraph.sty' v1.5c from 2009/09/02.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(TeX-add-style-hook
 "epigraph"
 (lambda ()
   (TeX-add-symbols
    ;; 2.1 The epigraph command
    '("epigraph" 2)
    ;; 2.2 The epigraphs environment
    '("qitem" 2)
    ;; 2.3 General
    "textflush"
    "epigraphflush"
    "sourceflush"
    "epigraphsize"
    ;; 2.4 Epigraphs before chapter headings
    '("epigraphhead" [ "Distance (a number)" ] t)
    '("dropchapter" TeX-arg-length)
    "undodrop"
    ;; \cleartoevenpage takes an optional argument.  Don't query for
    ;; it, just insert the macro and leave the rest to the user
    '("cleartoevenpage" 0))

   ;; 2.2 The epigraphs environment
   (LaTeX-add-environments
    '("epigraphs" LaTeX-env-item))

   ;; The value of these lengths can be changed with \setlength
   (LaTeX-add-lengths "epigraphwidth" "epigraphrule"
		      "beforeepigraphskip"
		      "afterepigraphskip")

   ;; Append epigraphs to `LaTeX-item-list':
   (add-to-list 'LaTeX-item-list
		'("epigraphs" . LaTeX-epigraph-qitem) t)

   ;; Append qitem to `LaTeX-item-regexp':
   (unless (string-match "qitem" LaTeX-item-regexp)
     (set (make-local-variable 'LaTeX-item-regexp)
	  (concat
	   LaTeX-item-regexp
	   "\\|"
	   "qitem\\b"))
     (LaTeX-set-paragraph-start))

   ;; Fontification:
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("epigraph"     "{{")
				("qitem"        "{{")
				("epigraphhead" "[{"))
			      'textual)
     (font-latex-add-keywords '(("dropchapter"  "{")
				("undodrop"     ""))
			      'variable)
     (font-latex-add-keywords '("cleartoevenpage")
			      'warning)))
 LaTeX-dialect)

(defvar LaTeX-epigraph-package-options nil
  "Package options for the epigraph package.")

;; adapted from latex.el:`LaTeX-item-bib'
(defun LaTeX-epigraph-qitem ()
  "Insert a new qitem for use in the epigraphs environment."
  (TeX-insert-macro "qitem"))

;;; epigraph.el ends here
