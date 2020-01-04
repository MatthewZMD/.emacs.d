;;; xltabular.el --- AUCTeX style for `xltabular.sty' (v0.05)

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2017-11-03
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

;; This file adds support for `xltabular.sty' (v0.05) from 2017/10/26.
;; `xltabular.sty' is part of TeXLive.

(defvar LaTeX-xltabular-skipping-regexp
  (concat "[ \t]*" (regexp-opt '("[l]" "[r]" "[c]" "")) "[ \t]*{[^}]*}[ \t]*")
  "Regexp matching between \\begin{xltabular} and column specification.
For xltabular environment only.  See `LaTeX-insert-ampersands' for detail.

This regexp assumes that the width specification contains neither
nested curly brace pair nor escaped \"}\".")

(defun LaTeX-env-xltabular (environment)
  "Insert a xltabular ENVIRONMENT with spec, caption and label."
  ;; xltabular has the following syntax:
  ;;   \begin{xltabular}[hPos]{width}{ l X ...}
  ;; Optional <hPos> comes before <width>, hence we cannot use
  ;; `LaTeX-env-tabular*' here and has to cook our own function which
  ;; is a combination of `LaTeX-env-tabular*' and
  ;; `LaTeX-env-longtable':
  (let* ((pos (completing-read (TeX-argument-prompt t nil "Position")
			       '("l" "r" "c")))
	 (width (TeX-read-string "Width: " LaTeX-default-width))
	 (fmt (TeX-read-string "Format: " LaTeX-default-format))
	 (caption (TeX-read-string "Caption: "))
	 (short-caption (when (>= (length caption) LaTeX-short-caption-prompt-length)
			  (TeX-read-string "(Optional) Short caption: "))))
    (setq LaTeX-default-format fmt)
    (LaTeX-insert-environment environment
			      (concat
			       (unless (zerop (length pos))
				 (concat LaTeX-optop pos LaTeX-optcl))
			       (concat TeX-grop width TeX-grcl)
			       (concat TeX-grop fmt TeX-grcl)))
    ;; top caption -- do nothing if user skips caption
    (unless (zerop (length caption))
      ;; insert `\caption[short-caption]{caption':
      (insert TeX-esc "caption")
      (when (and short-caption (not (string= short-caption "")))
	(insert LaTeX-optop short-caption LaTeX-optcl))
      (insert TeX-grop caption)
      ;; ask for a label and insert it
      (LaTeX-label environment 'environment)
      ;; the longtable `\caption' is equivalent to a
      ;; `\multicolumn', so it needs a `\\' at the
      ;; end of the line.  Prior to that, add } to
      ;; close `\caption{'
      (insert TeX-grcl "\\\\")
      ;; fill the caption
      (LaTeX-fill-paragraph)
      ;; Insert a new line and indent
      (LaTeX-newline)
      (indent-according-to-mode))
    ;; Insert suitable number of &'s, suppress line break
    (LaTeX-item-xltabular t)))

(defun LaTeX-item-xltabular (&optional suppress)
  "Insert line break macro on the last line and suitable number of &'s.
For xltabular environment only.

If SUPPRESS is non-nil, do not insert line break macro."
  (unless suppress
    (save-excursion
      (end-of-line 0)
      (just-one-space)
      (TeX-insert-macro "\\")))
  (LaTeX-insert-ampersands
   LaTeX-xltabular-skipping-regexp #'LaTeX-array-count-columns))

(TeX-add-style-hook
 "xltabular"
 (lambda ()
   ;; ltablex loads both tabularx and longtable
   (TeX-run-style-hooks "ltablex")

   ;; Add xltabular with `LaTeX-env-xltabular'::
   (LaTeX-add-environments '("xltabular" LaTeX-env-xltabular))

   ;; Use the enhanced table formatting.  Append to
   ;; `LaTeX-indent-environment-list' in order not to override custom settings.
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
		'("xltabular" LaTeX-indent-tabular) t)

   ;; Append xltabular to `LaTeX-label-alist', in order not to
   ;; override possible custome values.
   (add-to-list 'LaTeX-label-alist '("xltabular" . LaTeX-table-label) t)

   ;; Append xltabular to `LaTeX-item-list' with `LaTeX-item-xltabular'
   (add-to-list 'LaTeX-item-list '("xltabular" . LaTeX-item-xltabular) t)

   ;; Tell RefTeX -- This is the same entry as for "longtable" in
   ;; `reftex-label-alist-builtin':
   (when (fboundp 'reftex-add-label-environments)
     (reftex-add-label-environments
      '(("xltabular" ?t nil nil caption)))))
 LaTeX-dialect)

(defvar LaTeX-xltabular-package-options nil
  "Package options for the xltabular package.")

;;; xltabular.el ends here
