;;; ocg-p.el --- AUCTeX style for `ocg-p.sty' (v0.4)

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

;; This file adds support for `ocg-p.sty' v0.4 from 2013/01/10.
;; `ocg-p.sty' is part of TeXLive.

;;; Code:

;; Needed for auto-parsing.
(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

;; Setup for layer id's defined with
;; \begin{ocg}[<opt-arg>]{<layer name>}{<layer id>}{<initial visibility>}:

(TeX-auto-add-type "ocgp-ocg-layer-id" "LaTeX")

(defvar LaTeX-ocgp-ocg-layer-id-regexp
  `(,(concat "\\\\begin{ocg}"
	     "[ \t\n\r%]*"
	     "\\(?:\\[[^]]*\\]\\)?"
	     "[ \t\n\r%]*"
	     "\\(?:{[^}]+}\\)"
	     "[ \t\n\r%]*"
	     "{\\([^}]+\\)}")
    1 LaTeX-auto-ocgp-ocg-layer-id))

(defun LaTeX-ocgp-auto-prepare ()
  "Clear `LaTeX-auto-ocgp-ocg-layer-id' before parsing."
  (setq LaTeX-auto-ocgp-ocg-layer-id nil))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-ocgp-auto-prepare t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defvar LaTeX-ocgp-env-key-val-options
  '(("printocg"      ("always" "never" "ifvisible"))
    ("exportocg"     ("always" "never" "ifvisible"))
    ("listintoolbar" ("always" "never" "iffirstuse")))
  "Key=value options for ocg environment from ocg-p package.")

(defun LaTeX-env-ocgp-ocg (optional)
  "Insert arguments for ocg environment from ocg-p package."
  ;; The optional key-val argument:
  (let ((TeX-arg-opening-brace LaTeX-optop)
	(TeX-arg-closing-brace LaTeX-optcl))
    (TeX-argument-insert
     (TeX-read-key-val t LaTeX-ocgp-env-key-val-options)
     t))
  ;; Layer Name:
  (TeX-argument-insert
   (TeX-read-string
    (TeX-argument-prompt optional nil "Layer name"))
   optional)
  ;;
  ;; Layer id: Use completing read to show which id's are available.
  ;; Then add the new id to list of know id's and insert it
  (TeX-argument-insert
   (let ((id (completing-read
	      (TeX-argument-prompt optional nil "Layer id")
	      (LaTeX-ocgp-ocg-layer-id-list))))
     (LaTeX-add-ocgp-ocg-layer-ids id)
     id)
   optional)
  ;;
  ;; Initial visibility: Insert 0 or 1
  (TeX-argument-insert
   (TeX-read-string
    (TeX-argument-prompt optional nil "Initial visibility (0 or 1)"))
   optional))

(defvar LaTeX-ocgp-mac-key-val-options
  '(("triggerocg" ("onareaenter" "onareaexit" "onmousedown"
		   "onmouseup"   "allactions")))
  "Key=value options for macros provided by ocg-p package.")

(defun LaTeX-arg-ocgp-layer-id (optional &optional prompt)
  "Insert (multiple) defined layer id's for various macros from ocg-p package.
<SPC> key binding in minibuffer is removed temporarily.
Completion is still available with <TAB> key."
  (let* ((crm-separator "[ \t]+")
	 (crm-local-completion-map
	  (remove (assoc 32 crm-local-completion-map) crm-local-completion-map))
	 (ids (mapconcat #'identity
			 (TeX-completing-read-multiple
			  (TeX-argument-prompt optional prompt "Layer id ('s space separated)")
			  (LaTeX-ocgp-ocg-layer-id-list))
			 " ")))
    (TeX-argument-insert ids optional)))

(defun LaTeX-env-ocgp-ocgtabular (environment)
  "Insert ocgtabular ENVIRONMENT with position, column spec's and 2 more arguments.
Just like array and tabular."
  (let ((pos (and LaTeX-default-position ; LaTeX-default-position can
					; be nil, i.e. do not prompt
		  (TeX-read-string "(Optional) Position: " LaTeX-default-position)))
	(fmt (TeX-read-string "Format: " LaTeX-default-format))
	(dbase (TeX-read-string "Database name: "))
	(opts (TeX-read-string "Additional options: ")))
    (setq LaTeX-default-position pos)
    (setq LaTeX-default-format fmt)
    (LaTeX-insert-environment environment
			      (concat
			       (unless (zerop (length pos))
				 (concat LaTeX-optop pos LaTeX-optcl))
			       (concat TeX-grop fmt TeX-grcl)
			       (concat TeX-grop dbase TeX-grcl)
			       (concat TeX-grop opts TeX-grcl)))
    (LaTeX-item-array t)))

(TeX-add-style-hook
 "ocg-p"
 (lambda ()

   ;; Add ocg-p to the parser
   (TeX-auto-add-regexp LaTeX-ocgp-ocg-layer-id-regexp)

   ;; 2.3 The ocg environment
   (LaTeX-add-environments
    '("ocg" LaTeX-env-args LaTeX-env-ocgp-ocg))

   ;; 2.4 The commands of the package
   (TeX-add-symbols
    '("toggleocgs"
      [ TeX-arg-key-val LaTeX-ocgp-mac-key-val-options ]
      LaTeX-arg-ocgp-layer-id
      "Action button")

    '("showocgs"
      [ TeX-arg-key-val LaTeX-ocgp-mac-key-val-options ]
      LaTeX-arg-ocgp-layer-id
      "Action button")

    '("hideocgs"
      [ TeX-arg-key-val LaTeX-ocgp-mac-key-val-options ]
      LaTeX-arg-ocgp-layer-id
      "Action button")

    '("setocgs"
      [ TeX-arg-key-val LaTeX-ocgp-mac-key-val-options ]
      (LaTeX-arg-ocgp-layer-id "Toggle layer id ('s space separated)")
      (LaTeX-arg-ocgp-layer-id "Show layer id ('s space separated)")
      (LaTeX-arg-ocgp-layer-id "Hide layer id ('s space separated)")
      "Action button"))

   ;; 2.5 The ocgtabular environment
   (when (LaTeX-provided-package-options-member "ocg-p" "ocgtabular")
     (LaTeX-add-environments
      '("ocgtabular" LaTeX-env-ocgp-ocgtabular))

     (TeX-add-symbols
      '("setocgtabularheader" "Column name" "Displayed header")))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("toggleocgs"          "[{{")
				("showocgs"            "[{{")
				("hideocgs"            "[{{")
				("setocgs"             "[{{{{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-ocg-p-package-options '("ocgtabular")
  "Package options for the ocg-p package.")

;;; ocg-p.el ends here
