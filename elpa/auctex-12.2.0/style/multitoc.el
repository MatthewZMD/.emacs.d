;;; multitoc.el --- AUCTeX style for `multitoc.sty' (v2.01)

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2019-09-07
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

;; This file adds support for `multitoc.sty' (v2.01) from 1999/06/08.
;; `multitoc.sty' is part of TeXLive.

;;; Code:

(TeX-add-style-hook
 "multitoc"
 (lambda ()
   (TeX-run-style-hooks "multicol" "ifthen")
   (TeX-add-symbols
    "multicolumntoc"
    "multicolumnlot"
    "multicolumnlof"))
 LaTeX-dialect)

(defvar LaTeX-multitoc-package-options
  '("toc" "lof" "lot")
  "Package options for the multitoc package.")

;;; multitoc.el ends here
