;;; lettrine.el --- AUCTeX style for `lettrine.sty' (v2.21)

;; Copyright (C) 2011, 2018 Free Software Foundation, Inc.

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

;; This file adds support for `lettrine.sty' (v2.21) from 2018/08/28.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(defvar LaTeX-lettrine-key-val-options
  '(("lines")
    ("depth")
    ("lhang")
    ("loversize")
    ("lraise")
    ("findent")
    ("nindent")
    ("slope")
    ("ante")
    ("image" ("true"))
    ("grid" ("true"))
    ("novskip")
    ("realheight" ("true"))
    ("refstring"))
  "Key=value options for \\lettrine marco.")

(TeX-add-style-hook
 "lettrine"
 (lambda ()
   (TeX-add-symbols
    '("lettrine" [ TeX-arg-key-val LaTeX-lettrine-key-val-options ]
      "Letter" "Text")
    '("DefaultLoversize" 0)
    '("DefaultLraise" 0)
    '("DefaultLhang" 0)
    '("LettrineImageFalse" 0)
    '("LettrineOnGridfalse" 0)
    '("LettrineRealHeightfalse" 0)
    '("LettrineSelfReffalse" 0)
    '("LettrineFont" 0)
    '("LettrineFontHook" 0)
    '("LettrineTextFont" 0)
    ;; above settings can also be input a file, and pointed to with
    ;; \renewcommand
    '("DefaultOptionsFile" TeX-arg-file-name))

   ;; Counters:
   (LaTeX-add-counters "DefaultLines" "DefaultDepth")

   ;; Lengths and dimensions:
   (LaTeX-add-lengths "DefaultFindent"
		      "DefaultNindent"
		      "DefaultSlope"
		      "DiscardVskip"
		      "LettrineWidth"
		      "LettrineHeight"
		      "LettrineDepth")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("lettrine" "[{{")) 'textual)))
 LaTeX-dialect)

(defvar LaTeX-lettrine-package-options nil
  "Package options for the lettrine package.")

;;; lettrine.el ends here
