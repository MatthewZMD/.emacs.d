;;; thmtools.el --- AUCTeX style for `thmtools.sty' (v67)

;; Copyright (C) 2018, 2019 Free Software Foundation, Inc.

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

;; This file adds support for `thmtools.sty' (v67) from 2019/07/31.
;; `thmtools.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

;; Needed for auto-parsing:
(require 'tex)
(require 'latex)

;; Setup for \declaretheoremstyle:
(TeX-auto-add-type "thmtools-declaretheoremstyle" "LaTeX")

(defvar LaTeX-thmtools-declaretheoremstyle-regexp
  `(,(concat "\\\\declaretheoremstyle"
	     "[ \t\n\r%]*"
	     "\\(?:"
	     (LaTeX-extract-key-value-label 'none)
	     "\\)?"
	     "[ \t\n\r%]*"
	     "{\\([^}]+\\)}")
    1 LaTeX-auto-thmtools-declaretheoremstyle)
  "Matches the argument of \\declaretheoremstyle from thmtools package.")

;; Setup for \declaretheorem:
(TeX-auto-add-type "thmtools-declaretheorem" "LaTeX")

(defvar LaTeX-thmtools-declaretheorem-regexp
  `(,(concat "\\\\declaretheorem"
	     "[ \t\n\r%]*"
	     "\\(?:"
	     (LaTeX-extract-key-value-label 'none)
	     "\\)?"
	     "[ \t\n\r%]*"
	     "{\\([^}]+\\)}")
    1 LaTeX-auto-thmtools-declaretheorem)
  "Matches the argument of \\declaretheorem from thmtools package.")

(defun LaTeX-thmtools-auto-prepare ()
  "Clear `LaTeX-auto-thmtools-*' before parsing."
  (setq LaTeX-auto-thmtools-declaretheoremstyle nil
	LaTeX-auto-thmtools-declaretheorem      nil))

(defun LaTeX-thmtools-auto-cleanup ()
  "Process parsed elements from thmtools package."
  (dolist (newthm (mapcar #'car (LaTeX-thmtools-declaretheorem-list)))
    (LaTeX-add-environments `(,newthm LaTeX-thmtools-env-label))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-thmtools-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-thmtools-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-thmtools-declaretheoremstyle-key-val (optional &optional prompt)
  "Query and return a key=val string for \\declaretheoremstyle macro.
If OPTIONAL is non-nil, indicate an optional argument in
minibuffer.  PROMPT replaces the standard one."
  (let ((lengths (mapcar (lambda (x)
			   (concat TeX-esc x))
			 (mapcar #'car (LaTeX-length-list))))
	(fonts (mapcar (lambda (x)
			 (concat TeX-esc x))
		       '("rmfamily" "sffamily" "ttfamily" "mdseries" "bfseries"
			 "upshape" "itshape" "slshape" "scshape"
			 "tiny"  "scriptsize" "footnotesize"
			 "small" "normalsize" "large"
			 "Large" "LARGE" "huge" "Huge" "normalfont"))))
    (TeX-read-key-val
     optional
     `(("spaceabove" ,lengths)
       ("spacebelow" ,lengths)
       ("headfont" ,fonts)
       ("notefont" ,fonts)
       ("bodyfont" ,fonts)
       ("headpunct")
       ("notebraces")
       ("postheadspace" ,lengths)
       ("headformat" ("margin" "swapnumber" "\\NUMBER" "\\NAME" "\\NOTE"))
       ("headindent" ,lengths))
     prompt)))

(defun LaTeX-arg-thmtools-declaretheoremstyle (optional &optional prompt)
  "Insert the key=val and style name defined by \\declaretheoremstyle.
If OPTIONAL is non-nil, also insert the second argument in square
brackets.  PROMPT replaces the standard one for the second
argument."
  (let ((TeX-arg-opening-brace "[")
	(TeX-arg-closing-brace "]"))
    (TeX-argument-insert
     (LaTeX-thmtools-declaretheoremstyle-key-val t)
     t))
  (let ((style (TeX-read-string
		(TeX-argument-prompt optional prompt "Style"))))
    (LaTeX-add-thmtools-declaretheoremstyles style)
    (TeX-argument-insert style optional)))

(defun LaTeX-thmtools-declaretheorem-key-val (optional &optional prompt)
  "Query and return a key=val string for \\declaretheorem macro.
If OPTIONAL is non-nil, indicate an optional argument in
minibuffer.  PROMPT replaces the standard one."
  (let ((counters (mapcar #'car (LaTeX-counter-list))))
    (TeX-read-key-val
     optional
     `(("parent" ,counters)
       ("numberwithin" ,counters)
       ("within" ,counters)
       ("sibling" ,counters)
       ("numberlike" ,counters)
       ("sharenumber" ,counters)
       ("title")
       ("name")
       ("heading")
       ("numbered" ("yes" "no" "unless unique"))
       ("style"
	,(append
	  ;; check for \newtheoremstyle from amsthm.sty:
	  (when (and (fboundp 'LaTeX-amsthm-newtheoremstyle-list)
		     (LaTeX-amsthm-newtheoremstyle-list))
	    (mapcar #'car (LaTeX-amsthm-newtheoremstyle-list)))
	  ;; check for \newtheoremstyle from ntheorem.sty:
	  (when (and (fboundp 'LaTeX-ntheorem-newtheoremstyle-list)
		     (LaTeX-ntheorem-newtheoremstyle-list))
	    (mapcar #'car (LaTeX-ntheorem-newtheoremstyle-list)))
	  ;; thmtools version is called \declaretheoremstyle:
	  (mapcar #'car (LaTeX-thmtools-declaretheoremstyle-list))))
       ("preheadhook")
       ("postheadhook")
       ("prefoothook")
       ("postfoothook")
       ("refname")
       ("Refname")
       ("shaded" ("textwidth" "bgcolor" "rulecolor" "rulewidth" "margin"))
       ("thmbox" ("L" "M" "S"))))))

(defun LaTeX-arg-thmtools-declaretheorem (optional &optional prompt)
  "Insert the key=val and environment name defined by \\declaretheorem.
If OPTIONAL is non-nil, also insert the second argument in square
brackets.  PROMPT replaces the standard one for the second
argument."
  (let ((TeX-arg-opening-brace "[")
	(TeX-arg-closing-brace "]"))
    (TeX-argument-insert
     (LaTeX-thmtools-declaretheorem-key-val t)
     t))
  (let ((env (TeX-read-string
	      (TeX-argument-prompt optional prompt "Environment"))))
    (LaTeX-add-environments `(,env LaTeX-thmtools-env-label))
    (TeX-argument-insert env optional)))

(defun LaTeX-thmtools-listoftheorems-key-val (optional &optional prompt)
  "Query and return a key=val string for \\listoftheorems macro.
If OPTIONAL is non-nil, indicate an optional argument in
minibuffer.  PROMPT replaces the standard one."
  (let ((lengths (mapcar (lambda (x)
			   (concat TeX-esc x))
			 (mapcar #'car (LaTeX-length-list))))
	(thms (append
	       ;; check for \newtheorem from amsthm.sty:
	       (when (and (fboundp 'LaTeX-amsthm-newtheorem-list)
			  (LaTeX-amsthm-newtheorem-list))
		 (mapcar #'car (LaTeX-amsthm-newtheorem-list)))
	       ;; check for \newtheorem from ntheorem.sty:
	       (when (and (fboundp 'LaTeX-ntheorem-newtheorem-list)
			  (LaTeX-ntheorem-newtheorem-list))
		 (mapcar #'car (LaTeX-ntheorem-newtheorem-list)))
	       ;; thmtools version is called \declaretheorem:
	       (mapcar #'car (LaTeX-thmtools-declaretheorem-list)))))
    (TeX-read-key-val
     optional
     `(("numwidth" ,lengths)
       ("ignore" ,thms)
       ("onlynamed" ,thms)
       ("show" ,thms)
       ("ignoreall" ("true" "false"))
       ("showall" ("true" "false"))
       ("title")))))

(defun LaTeX-arg-thmtools-listoftheorems (optional &optional prompt)
  "Insert the key=val to \\listoftheorems macro.
If OPTIONAL is non-nil, insert the result square brackets.
OPTIONAL and PROMPT are passed to `LaTeX-thmtools-listoftheorems-key-val'."
  (TeX-argument-insert
   (LaTeX-thmtools-listoftheorems-key-val optional prompt)
   optional))

(defun LaTeX-thmtools-env-label (environment)
  "Insert thmtools ENVIRONMENT, query for an optional argument and label.
AUCTeX users should add ENVIRONMENT to `LaTeX-label-alist' via
customize or in init-file with:

  (add-to-list \\='LaTeX-label-alist \\='(\"theorem\" . \"thm:\"))

RefTeX users should customize or add ENVIRONMENT to
`LaTeX-label-alist' and `reftex-label-alist', e.g.

  (add-to-list \\='LaTeX-label-alist \\='(\"theorem\" . \"thm:\"))
  (add-to-list \\='reftex-label-alist
	       \\='(\"theorem\" ?m \"thm:\" \"~\\ref{%s}\"
		 nil (\"Theorem\" \"theorem\") nil))"
  (let* ((choice (read-char
		  (TeX-argument-prompt nil nil "Heading (h), Key=val (k), Empty (RET)")))
	 (opthead (cond ((= choice ?h)
			 (TeX-read-string
			  (TeX-argument-prompt t nil "Heading")))
			((= choice ?k)
			 (TeX-read-key-val
			  t
			  `(("name")
			    ("continues" ,(mapcar #'car (LaTeX-label-list)))
			    ("restate" ,(mapcar #'car (LaTeX-label-list)))
			    ;; We don't offer a label key here: It is
			    ;; marked "experimental" in the manual and
			    ;; inserting and parsing \label{foo} is
			    ;; much easier for AUCTeX and RefTeX
			    ;; ("label")
			    ("listhack" ("true" "false")))))
			(t ""))))
    (LaTeX-insert-environment environment
			      (when (and opthead
					 (not (string= opthead "")))
				(format "[%s]" opthead))))
  (when (LaTeX-label environment 'environment)
    (LaTeX-newline)
    (indent-according-to-mode)))

(TeX-add-style-hook
 "thmtools"
 (lambda ()

   ;; Add thmtools to the parser.
   (TeX-auto-add-regexp LaTeX-thmtools-declaretheoremstyle-regexp)
   (TeX-auto-add-regexp LaTeX-thmtools-declaretheorem-regexp)

   (TeX-add-symbols
    '("declaretheoremstyle" LaTeX-arg-thmtools-declaretheoremstyle)
    '("declaretheorem" LaTeX-arg-thmtools-declaretheorem)

    '("listoftheorems"  [ LaTeX-arg-thmtools-listoftheorems ])
    '("ignoretheorems"
      (TeX-arg-eval mapconcat #'identity
		    (TeX-completing-read-multiple
		     (TeX-argument-prompt optional nil "Environment(s)")
		     (append
		      ;; check for \newtheorem from amsthm.sty:
		      (when (and (fboundp 'LaTeX-amsthm-newtheorem-list)
				 (LaTeX-amsthm-newtheorem-list))
			(mapcar #'car (LaTeX-amsthm-newtheorem-list)))
		      ;; check for \newtheorem from ntheorem.sty:
		      (when (and (fboundp 'LaTeX-ntheorem-newtheorem-list)
				 (LaTeX-ntheorem-newtheorem-list))
			(mapcar #'car (LaTeX-ntheorem-newtheorem-list)))
		      ;; thmtools version is called \declaretheorem:
		      (mapcar #'car (LaTeX-thmtools-declaretheorem-list))))
		    ","))
    '("listtheoremname" 0))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("declaretheoremstyle"  "[{")
				("declaretheorem"       "[{")
				("listoftheorems"       "[")
				("ignoretheorems"       "{"))
			      'function)))
 LaTeX-dialect)

;; The package has only one option `debug'.  We ignore that in order
;; to make loading faster:
(defvar LaTeX-thmtools-package-options nil
  "Package options for the thmtools package.")

;;; thmtools.el ends here
