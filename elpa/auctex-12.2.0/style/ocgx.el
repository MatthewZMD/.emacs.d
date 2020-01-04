;;; ocgx.el --- AUCTeX style for `ocgx.sty' (v0.5)

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2018-08-05
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

;; This file adds support for `ocgx.sty' v0.5 from 2012/11/14.
;; `ocgx.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(TeX-add-style-hook
 "ocgx"
 (lambda ()

   ;; Run style hook for ocg-p package:
   (TeX-run-style-hooks "ocg-p")

   ;; 1.2 Manage the visibility of OCGs
   (TeX-add-symbols
    '("switchocg" LaTeX-arg-ocgp-layer-id "Action button")

    '("showocg" LaTeX-arg-ocgp-layer-id "Action button")

    '("hideocg" LaTeX-arg-ocgp-layer-id "Action button")

    '("actionsocg"
      (LaTeX-arg-ocgp-layer-id "Toggle layer id ('s space separated)")
      (LaTeX-arg-ocgp-layer-id "Show layer id ('s space separated)")
      (LaTeX-arg-ocgp-layer-id "Hide layer id ('s space separated)")
      "Action button"))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("switchocg"  "{{")
				("showocg"    "{{")
				("hideocg"    "{{")
				("actionsocg" "{{{{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-ocgx-package-options nil
  "Package options for the ocgx package.")

;;; ocgx.el ends here
