;;; thm-restate.el --- AUCTeX style for `thm-restate.sty' (v66)

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2018-07-07
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

;; This file adds support for `thm-restate.sty'.  `thm-restate.sty' is
;; part of `thmtools' package (v66) from 2014/04/21.  `thmtools.sty'
;; is part of TeXLive.

;;; Code:

;; Needed for auto-parsing.
(require 'tex)

;; Silence the parser:
(declare-function LaTeX-thmtools-declaretheorem-list
		  "thmtools" ())

;; Setup for macro names defined with
;; \begin{restatable}[<Heading>]{<env-name>}{<macro name>}:

(TeX-auto-add-type "thmrestate-restatable-macro" "LaTeX")

(defvar LaTeX-thmrestate-restatable-marco-regexp
  `(,(concat "\\\\begin{restatable}"
	     "[ \t\n\r%]*"
	     "\\(?:\\[[^]]*\\]\\)?"
	     "[ \t\n\r%]*"
	     "\\(?:{[^}]+}\\)"
	     "[ \t\n\r%]*"
	     "{\\([^}]+\\)}")
    1 LaTeX-auto-thmrestate-restatable-macro))

(defun LaTeX-thmrestate-auto-prepare ()
  "Clear `LaTeX-auto-thmrestate-restatable-macro' before parsing."
  (setq LaTeX-auto-thmrestate-restatable-macro nil))

(defun LaTeX-thmrestate-auto-cleanup ()
  "Process parsed elements from thm-restate package."
  (dolist (newmac (mapcar #'car (LaTeX-thmrestate-restatable-macro-list)))
    (TeX-add-symbols `(,newmac 0)
		     `(,(concat newmac "*") 0))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-thmrestate-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-thmrestate-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-env-thmrestate-restatable (optional)
  "Insert arguments for restatable environment from thm-restate package."
  ;; The optional heading argument:
  (let ((TeX-arg-opening-brace LaTeX-optop)
	(TeX-arg-closing-brace LaTeX-optcl))
    (TeX-argument-insert
     (TeX-read-string
      (TeX-argument-prompt t nil "Heading"))
     t))
  ;; Name of the environment we are referring to; this can be defined
  ;; via amsthm.sty, ntheorem.sty or thmtools.sty:
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional nil "Environment")
    (append
     ;; Cater for environments defined with amsthm's \newtheorem
     (when (and (fboundp 'LaTeX-amsthm-newtheorem-list)
		(LaTeX-amsthm-newtheorem-list))
       (LaTeX-amsthm-newtheorem-list))
     ;; Cater for environments defined with ntheorem's \newtheorem
     (when (and (fboundp 'LaTeX-ntheorem-newtheorem-list)
		(LaTeX-ntheorem-newtheorem-list))
       (LaTeX-ntheorem-newtheorem-list))
     ;; Environments defined with \declaretheorem
     (LaTeX-thmtools-declaretheorem-list)))
   optional)
  (let ((mac (TeX-read-string
	      (TeX-argument-prompt optional nil "Macro"))))
    (TeX-add-symbols `(,mac 0)
		     `(,(concat mac "*") 0))
    (TeX-argument-insert mac optional)))

(TeX-add-style-hook
 "thm-restate"
 (lambda ()

   ;; Run the style hook `thmtools.el':
   (TeX-run-style-hooks "thmtools")

   ;; Add thm-restate to the parser
   (TeX-auto-add-regexp LaTeX-thmrestate-restatable-marco-regexp)

   ;; Provide restatable\\*? environment
   (LaTeX-add-environments
    '("restatable"  LaTeX-env-args LaTeX-env-thmrestate-restatable)
    '("restatable*" LaTeX-env-args LaTeX-env-thmrestate-restatable)))
 LaTeX-dialect)

;;; thm-restate.el ends here
