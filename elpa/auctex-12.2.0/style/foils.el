;;; foils.el - Special code for FoilTeX.

;; Copyright (C) 1994-2014 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'timezone)

(TeX-add-style-hook "foils"
 (function
  (lambda ()
    (add-hook 'LaTeX-document-style-hook 'LaTeX-style-foils)
    (setq LaTeX-default-style "foils")
    (setq LaTeX-default-options '("landscape"))
    (TeX-add-symbols
     '("foilhead" [ "Rubric-body separation" ] "Foil rubric"))))
 LaTeX-dialect)

(defun LaTeX-style-foils nil
  "Prompt for and insert foiltex options."
  (let* ((date (timezone-parse-date (current-time-string)))
	 (year   (string-to-number (aref date 0)))
	 (month  (string-to-number (aref date 1)))
	 (day    (string-to-number (aref date 2)))
	 (title (TeX-read-string "Title: ")))
    (save-excursion
      (goto-char (point-max))
      (re-search-backward ".begin.document.")
      (insert TeX-esc "title"
	      TeX-grop title TeX-grcl "\n")
      (insert TeX-esc "author"
	      TeX-grop (user-full-name) TeX-grcl "\n")
      (insert TeX-esc "date" TeX-grop
	      (format "%d-%02d-%02d" year month day)
	      TeX-grcl "\n")
      (insert "" TeX-esc "MyLogo" TeX-grop TeX-grcl "\n")
      (insert "%" TeX-esc "Restriction" TeX-grop TeX-grcl "\n")
      (insert "%" TeX-esc "rightfooter" TeX-grop TeX-grcl "\n")
      (insert "%" TeX-esc "leftheader" TeX-grop TeX-grcl "\n")
      (insert "%" TeX-esc "rightheader" TeX-grop TeX-grcl "\n\n")
      (re-search-forward ".begin.document.")
      (end-of-line)
      (newline-and-indent)
      (insert "" TeX-esc "maketitle\n\n"))
    (forward-line -1)))

;;; foils.el ends here
