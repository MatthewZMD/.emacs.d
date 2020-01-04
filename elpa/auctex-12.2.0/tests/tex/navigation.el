;;; navigation.el --- tests for navigation function in TeX buffer

;; Copyright (C) 2019 Free Software Foundation, Inc.

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
(require 'tex)

(defun TeX-check-f-m-e-h (string &optional position)
  "Check whether `TeX-find-macro-end-helper' works for exceptional case."
  (with-temp-buffer
    (insert string)
    (should (= (or position (point-max))
	       (TeX-find-macro-end-helper (point-min))))))

(ert-deftest TeX-find-macro-end-helper-single ()
  ;; single macro ending at EOB
  (TeX-check-f-m-e-h "\\foo"))

(ert-deftest TeX-find-macro-end-helper-curly ()
  ;; curly braces ending at EOB
  (TeX-check-f-m-e-h "\\foo{bar}"))

(ert-deftest TeX-find-macro-end-helper-curly-fail ()
  ;; curly brace failing to close at EOB
  (TeX-check-f-m-e-h "\\foo{bar"))

(ert-deftest TeX-find-macro-end-helper-square ()
  ;; square brackets ending at EOB
  (TeX-check-f-m-e-h "\\foo{bar}[baz]"))

(ert-deftest TeX-find-macro-end-helper-square-fail ()
  ;; square bracket failing to close at EOB
  (TeX-check-f-m-e-h "\\foo{bar}[baz" (1+ (length "\\foo{bar}"))))

;;; navigation.el ends here
