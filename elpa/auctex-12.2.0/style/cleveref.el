;;; cleveref.el --- AUCTeX style for `cleveref.sty' (v0.21.4)

;; Copyright (C) 2014--2019 Free Software Foundation, Inc.

;; Author: Matthew Leach <matthew@mattleach.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 13/10/2014

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

;; This file adds support for `cleveref.sty' (v0.21.4), dated
;; 2018/03/27.

;;; Code

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(defun TeX-arg-cleveref-multiple-labels (optional &optional prompt)
  "Prompt for a series of labels completing with known labels.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (if (and (fboundp 'reftex-arg-label)
	   (fboundp 'reftex-plug-flag)
	   (reftex-plug-flag 2))
      ;; Use RefTeX when enabled
      (TeX-arg-ref optional)
    ;; Use AUCTeX interface
    (let* ((labels (TeX-completing-read-multiple
		    (TeX-argument-prompt optional prompt "Keys")
		    (LaTeX-label-list)))
	   (labels-string (mapconcat #'identity labels ",")))
      (TeX-argument-insert labels-string optional))))

(defun TeX-arg-cleveref-crossref-type (optional &optional prompt)
  "Insert the cross-reference type for macros of cleveref package.
If OPTIONAL is non-nil, insert the resulting value in brackets.
Use PROMPT as the prompt string."
  (let* ((type (mapcar #'list
		       '("appendix" "subappendix" "subsubappendix"
			 "subsubsubappendix" "subfigure" "subtable"
			 "subequation")))
	 (types (append (LaTeX-counter-list) type)))
    (TeX-argument-insert
     (completing-read (TeX-argument-prompt optional prompt "Type") types)
     optional)))

(defvar LaTeX-cleveref-label-regexp
  '("\\\\label\\[[^]]*\\]{\\([^\n\r%\\{}]+\\)}" 1 LaTeX-auto-label)
  "Regexp matching a \\label incl. an optional argument.")

(TeX-add-style-hook
 "cleveref"
 (lambda ()

   (TeX-add-symbols
    ;; 4 Typesetting Cross-References
    '("cref" TeX-arg-cleveref-multiple-labels)
    '("Cref" TeX-arg-cleveref-multiple-labels)
    '("crefrange" (TeX-arg-ref "Key (first)") (TeX-arg-ref "Key (last)"))
    '("Crefrange" (TeX-arg-ref "key (first)") (TeX-arg-ref "Key (last)"))
    '("cref*" TeX-arg-cleveref-multiple-labels)
    '("Cref*" TeX-arg-cleveref-multiple-labels)
    '("crefrange*" (TeX-arg-ref "Key (first)") (TeX-arg-ref "Key (last)"))
    '("Crefrange*" (TeX-arg-ref "Key (first)") (TeX-arg-ref "Key (last)"))
    '("cpageref" TeX-arg-cleveref-multiple-labels)
    '("Cpageref" TeX-arg-cleveref-multiple-labels)
    '("cpagerefrange" (TeX-arg-ref "Key (first)") (TeX-arg-ref "Key (last)"))
    '("Cpagerefrange" (TeX-arg-ref "Key (first)") (TeX-arg-ref "Key (last)"))
    '("namecref" TeX-arg-ref)
    '("nameCref" TeX-arg-ref)
    '("lcnamecref" TeX-arg-ref)
    '("namecrefs" TeX-arg-ref)
    '("nameCrefs" TeX-arg-ref)
    '("lcnamecrefs" TeX-arg-ref)
    '("labelcref" TeX-arg-cleveref-multiple-labels)
    '("labelcpageref" TeX-arg-cleveref-multiple-labels)
    ;; 6 Overriding the Cross-Reference Type
    '("crefalias" TeX-arg-counter "Type")
    '("label" [ TeX-arg-cleveref-crossref-type ] TeX-arg-define-label)

    ;; 8.1.1 Global Customisation
    '("crefdefaultlabelformat" t)

    ;; 8.1.2 Customising Individual Cross-Reference Types
    '("crefname" TeX-arg-cleveref-crossref-type
      "Singular name" "Plural name")
    '("Crefname" TeX-arg-cleveref-crossref-type
      "Singular name" "Plural name")
    '("creflabelformat" TeX-arg-cleveref-crossref-type t)
    '("crefrangelabelformat" TeX-arg-cleveref-crossref-type t)

    ;; 8.2.1 Single Cross-References
    '("crefformat" TeX-arg-cleveref-crossref-type t)
    '("Crefformat" TeX-arg-cleveref-crossref-type t)

    ;; 8.2.2 Reference Ranges
    '("crefrangeformat" TeX-arg-cleveref-crossref-type t)
    '("Crefrangeformat" TeX-arg-cleveref-crossref-type t)

    ;; 8.2.3 Multiple Cross-References
    '("crefmultiformat"      TeX-arg-cleveref-crossref-type 4)
    '("Crefmultiformat"      TeX-arg-cleveref-crossref-type 4)
    '("crefrangemultiformat" TeX-arg-cleveref-crossref-type 4)
    '("Crefrangemultiformat" TeX-arg-cleveref-crossref-type 4))

   ;; These macros aren't used particularly often during the course of
   ;; normal referencing.
   (TeX-declare-expert-macros
    "cleveref"
    "namecref" "nameCref" "lcnamecref" "namecrefs" "nameCrefs"
    "lcnamecrefs" "labelcref" "labelcpageref"
    "crefdefaultlabelformat"
    "crefname" "Crefname" "creflabelformat" "crefrangelabelformat"
    "crefformat" "Crefformat"
    "crefrangeformat" "Crefrangeformat"
    "crefmultiformat" "Crefmultiformat"
    "crefrangemultiformat" "Crefrangemultiformat")

   ;; Add \label[type]{label} to AUCTeX parser
   (TeX-auto-add-regexp LaTeX-cleveref-label-regexp)

   ;; Tell RefTeX.  Check if `reftex-label-regexps' is bound and use a
   ;; local version of it.  Check if the regexp is already added in
   ;; order not to run `reftex-compile-variables' every time the style
   ;; hook runs
   (when (and (boundp 'reftex-label-regexps)
	      (fboundp 'reftex-compile-variables))
     (let ((regexp "\\\\label\\[[^]]*\\]{\\(?1:[^\n\r%\\{}]+\\)}"))
       (unless (member regexp reftex-label-regexps)
	 (add-to-list (make-local-variable 'reftex-label-regexps)
		      regexp t)
	 (reftex-compile-variables))))

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("cref"          "*{")
				("Cref"          "*{")
				("crefrange"     "*{{")
				("Crefrange"     "*{{")
				("cpageref"      "{")
				("Cpageref"      "{")
				("cpagerefrange" "{{")
				("Cpagerefrange" "{{")
				("namecref"      "{")
				("nameCref"      "{")
				("lcnamecref"    "{")
				("namecrefs"     "{")
				("nameCrefs"     "{")
				("lcnamecrefs"   "{")
				("labelcref"     "{")
				("labelcpageref" "{")
				("label"         "[{"))
			      'reference)
     (font-latex-add-keywords '(("crefalias"              "{{")
				("crefname"               "{{{")
				("Crefname"               "{{{")
				("creflabelformat"        "{{")
				("crefrangelabelformat"   "{{")
				("crefdefaultlabelformat" "{")
				("crefformat"             "{{")
				("Crefformat"             "{{")
				("crefrangeformat"        "{{")
				("Crefrangeformat"        "{{")
				("crefmultiformat"        "{{{{{")
				("Crefmultiformat"        "{{{{{")
				("crefrangemultiformat"   "{{{{{")
				("Crefrangemultiformat"   "{{{{{"))
			      'function))

   ;; Activate RefTeX reference style.
   (and LaTeX-reftex-ref-style-auto-activate
	(fboundp 'reftex-ref-style-activate)
	(reftex-ref-style-activate "Cleveref")))
 LaTeX-dialect)

(defvar LaTeX-cleveref-package-options
  '("capitalise" "nameinlink" "noabbrev" "poorman")
  "Package options for the cleveref package.")

;;; cleveref.el ends here.
