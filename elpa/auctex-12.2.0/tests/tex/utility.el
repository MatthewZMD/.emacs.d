;;; utility.el --- tests for AUCTeX utility functions

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

(ert-deftest TeX-infinite-loop ()
  "Check whether functions don't fall into infinite loop."
  (should (TeX-delete-duplicate-strings '("nil")))
  (should (TeX-delete-dups-by-car '(("nil" . 1)))))

;;; utility.el ends here
