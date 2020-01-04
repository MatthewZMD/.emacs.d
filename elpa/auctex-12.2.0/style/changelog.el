;;; changelog.el --- AUCTeX style for `changelog.sty' (v2.0.0)

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2019-05-05
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

;; This file adds support for `changelog.sty' (v2.0.0).
;; `changelog.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(defvar LaTeX-changelog-env-key-val-options
  '(("section" ("true" "false"))
    ("title"))
  "Key=value options for changelog environment.
The keys sectioncmd and label are added in the function
`LaTeX-env-changelog'.")

(defvar LaTeX-changelog-version-env-key-val-options
  '(("version")
    ("v")
    ("author")
    ("date")
    ("yanked" ("true" "false"))
    ("simple" ("true" "false"))
    ("short"  ("true" "false")))
  "key=value options for version environment.")

(defun LaTeX-env-changelog (environment)
  "Insert ENVIRONMENT, ask for optional argument and insert a label."
  (let* ((seccmds (mapcar #'car LaTeX-section-list))
	 ;; Collect the key=vals acc. to environment & documentclass
	 (opts (TeX-read-key-val
		t
		(if (string= environment "changelog")
		    (append
		     `(("sectioncmd"
			,(if (< (LaTeX-largest-level) 2)
			     (append
			      (mapcar (lambda (cmd) (concat TeX-esc cmd))
				      seccmds)
			      (mapcar (lambda (cmd) (concat TeX-esc cmd "*"))
				      seccmds))
			   (append
			    (mapcar (lambda (cmd) (concat TeX-esc cmd))
				    (remove "chapter" seccmds))
			    (mapcar (lambda (cmd) (concat TeX-esc cmd "*"))
				    (remove "chapter" seccmds))))))
		     LaTeX-changelog-env-key-val-options
		     LaTeX-changelog-version-env-key-val-options)
		  LaTeX-changelog-version-env-key-val-options)))
	 ;; Extract the chosen sectioning command
	 (sec (progn
		(string-match "sectioncmd=\\\\\\([a-z]+\\)\\(\\*?\\)" opts)
		(match-string-no-properties 1 opts)))
	 ;; Temp. re-bind `LaTeX-label-alist' and pick the label
	 ;; prefix from `LaTeX-section-label'
	 (LaTeX-label-alist
	  (when (and (string= environment "changelog")
		     (match-string 2 opts)
		     (not (string= (match-string 2 opts) "*")))
	    `(,(cons environment
		     (cdr (assoc sec LaTeX-section-label))))))
	 ;; Temp. re-bind `reftex-label-alist' as well and make
	 ;; `reftex-label' DTRT:
	 (reftex-label-alist
	  (when (and (boundp 'reftex-label-alist)
		     LaTeX-label-alist
		     (string= environment "changelog"))
	    `((,environment ?s ,(cdr (assoc sec LaTeX-section-label)) nil t)))))
    (LaTeX-insert-environment
     environment
     (when (and opts (not (string= opts "")))
       (concat LaTeX-optop opts LaTeX-optcl)))
    ;; Add a label into the opt. argument
    (when (string= environment "changelog")
      (LaTeX-env-label-as-keyval nil "sectioncmd" nil environment))
    ;; Add an \item in version environment
    (when (string= environment "version")
      (TeX-insert-macro "item")
      (indent-according-to-mode))))

(TeX-add-style-hook
 "changelog"
 (lambda ()

   (LaTeX-add-environments
    '("changelog" LaTeX-env-changelog)
    '("version"   LaTeX-env-changelog))

   (TeX-add-symbols
    '("added"      0)
    '("changed"    0)
    '("deprecated" 0)
    '("removed"    0)
    '("fixed"      0)
    '("security"   0)
    `("shortversion" (TeX-arg-key-val
		      ,(append
			'(("changes"))
			LaTeX-changelog-version-env-key-val-options))))

   ;; Tell RefTeX that the optional arg of changelog env. can contain a label:
   (when (and (boundp 'reftex-label-regexps)
	      (fboundp 'reftex-compile-variables)
	      (not (string-match  "\\bchangelog\\b"
				  (mapconcat #'identity
					     reftex-label-regexps
					     "|"))))
     (add-to-list (make-local-variable 'reftex-label-regexps)
		  (concat "\\\\begin{changelog}"
			  (LaTeX-extract-key-value-label nil 1))
		  t)
     (reftex-compile-variables))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("added"        "")
				("changed"      "")
				("deprecated"   "")
				("removed"      "")
				("fixed"        "")
				("security"     "")
				("shortversion" "{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-changelog-package-options nil
  "Package options for the changelog package.")

;;; changelog.el ends here
