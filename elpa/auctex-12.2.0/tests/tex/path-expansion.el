;;; path-expansion.el --- tests for path expansion

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

;;; Code:

(require 'ert)

(ert-deftest TeX-variable-truncation ()
  "Check whether list variable is not truncated as side effect."
  (let ((var '("str1" "str2"))
	(TeX-kpathsea-path-delimiter nil)
	(TeX-search-files-type-alist
	 '((abc "${dummy}" ("str2" var) TeX-file-extensions))))
    (TeX-search-files-by-type 'abc 'global)
    (should (equal var '("str1" "str2")))))

;;; path-expansion.el ends here
