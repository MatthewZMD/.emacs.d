;;; preview-latex.el --- tests for preview-latex compatibility

;; Copyright (C) 2018 Free Software Foundation, Inc.

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
(require 'preview)

(ert-deftest preview-error-quote-utf-8 ()
  "`preview-error-quote' is robust against partial ^^-quoting or not.
If a utf-8 byte sequence is partially ^^-quoted in latex output, we have
to decode ^^ab as raw 8-bit character first and decode in the sense of
emacs' coding system later."
  (let (case-fold-search
	(buffer-file-coding-system 'utf-8))
    (dolist (str '("primárias"
		   ;; Unicode character á is encoded in utf-8 as
		   ;; a byte sequence \xC3 \xA1.
		   "prim\xC3\xA1rias" "prim^^c3\xA1rias" "prim^^c3^^a1rias"))
      (should (string-match (preview-error-quote str) "primárias")))))

(ert-deftest preview-decode-^^ab-utf-8 ()
  "Test mixture of raw 8-bit byte and byte with ^^-quoting."
  (dolist (str '("prim\xC3\xA1rias" "prim^^c3\xA1rias" "prim^^c3^^a1rias"))
    (should (string= (preview--decode-^^ab str 'utf-8) "primárias"))))

;;; preview-latex-test.el ends here

;; Local Variables:
;; coding: utf-8
;; End:
