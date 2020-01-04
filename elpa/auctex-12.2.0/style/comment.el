;;; comment.el --- AUCTeX style for `comment.sty'

;; Copyright (C) 2007, 2018 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2007-03-18
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
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary:

;; This file adds support for `comment.sty'.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(declare-function font-latex-update-font-lock
		  "font-latex"
		  (&optional syntactic-kws))

(defvar font-latex-syntactic-keywords-extra)

(TeX-add-style-hook
 "comment"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    '("includecomment" "Name")
    '("excludecomment" "Name")
    '("specialcomment" "Name" "Before commands" "After commands")
    '("processcomment" "Name" "Each-line commands"
      "Before commands" "After commands"))
   ;; New environments
   (mapc 'LaTeX-add-environments LaTeX-comment-env-list)
   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-update-font-lock)
	      (eq TeX-install-font-lock 'font-latex-setup))
     ;; For syntactic fontification.
     (add-to-list 'font-latex-syntactic-keywords-extra
		  ;; \begin is supposed to start at the beginning of a line.
		  `(,(format "^\\\\begin *{%s}.*\\(\n\\)"
			     (regexp-opt LaTeX-comment-env-list))
		    (1 "!" t)))
     (add-to-list 'font-latex-syntactic-keywords-extra
		  ;; \end is supposed to start at the beginning of a line.
		  `(,(format "^\\(\\\\\\)end *{%s}"
			     (regexp-opt LaTeX-comment-env-list))
		    (1 "!" t)))
     (font-latex-add-keywords '(("includecomment" "{")
				("excludecomment" "{")
				("specialcomment" "{{{")
				("processcomment" "{{{{"))
			      'variable)
     ;; Tell font-lock about the update.
     (font-latex-update-font-lock t)))
 LaTeX-dialect)

;;; comment.el ends here
