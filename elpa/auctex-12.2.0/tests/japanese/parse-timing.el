;;; parse-timing.el --- tests for parse timing

;; Copyright (C) 2017 Free Software Foundation, Inc.

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
;; In AUCTeX, style hooks must not be executed too early.  In
;; particular, they should not be called within the major mode hook
;; because the required settings are sometimes not ready at that
;; timing.  A difficult point is that many AUCTeX functions implicitly
;; call `TeX-update-style', which eventually calls style hooks.  Thus
;; a complicated hook sometimes triggers style hooks unintentionally.
;; Such cases suits for detection via regression tests.

;;; Code:

(require 'ert)
(require 'tex-jp)

(AUCTeX-set-ert-path
 'parse-timing
 "parse-timing-test.tex")

(ert-deftest japanese-TeX-style-hook-timing ()
  "Test style hooks are not called too early."
  (let ((TeX-parse-self t)
	(TeX-master t)
	(LaTeX-mode-hook '(japanese-latex-mode-initialization
			   turn-on-reftex)))
    (find-file parse-timing)
    (should (memq 'AMSTeX
		  (get reftex-docstruct-symbol
		       'reftex-label-alist-style)))
    (kill-buffer)))

;;; parse-timing.el ends here
