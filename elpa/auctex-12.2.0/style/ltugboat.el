;;; ltugboat.el --- AUCTeX style for `ltugboat.cls' (v2.21)

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2019-05-11
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

;; This file adds support for `ltugboat.cls' (v2.21) from 2018/12/14.
;; `ltugboat.cls' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(defun LaTeX-env-ltugboat-verbatim (environment)
  "Insert verbatim environment with an optional argument."
  (let* ((crm-separator (regexp-quote TeX-esc))
	 (opts (mapconcat #'identity
			  (TeX-completing-read-multiple
			   (TeX-argument-prompt t nil "command(s)")
			   '("\\tiny"  "\\scriptsize" "\\footnotesize"
			     "\\small" "\\normalsize" "\\large"
			     "\\Large" "\\LARGE"      "\\huge"
			     "\\Huge"  "\\makevmeta"  "\\ruled")
			   nil nil TeX-esc)
			  TeX-esc)))
    (LaTeX-insert-environment environment
			      (when (and opts
					 (not (string= opts "")))
				(concat LaTeX-optop opts LaTeX-optcl)))))

(TeX-add-style-hook
 "ltugboat"
 (lambda ()

   ;; Run the style hook for mflogo in order to define the macros \MF
   ;; and \MP:
   (TeX-run-style-hooks "mflogo")

   ;; Preliminaries: ltugboat.cls suppresses \part & \subparagraph
   (LaTeX-largest-level-set "section")
   (LaTeX-add-counters "section" "subsection" "subsubsection" "paragraph"
		       "figure" "table")

   ;; 6 Divisions of the paper
   (TeX-add-symbols
    '("nameref" TeX-arg-ref))

   (setq TeX-complete-list
	 (append
	  '(("\\\\nameref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}"))
	  TeX-complete-list))

   ;; 6.1 Abstracts
   (LaTeX-add-environments '("abstract")
			   '("longabstract"))

   ;; 6.2 Appendices: Cater for appendix environment and don't indent
   ;; the content
   (LaTeX-add-environments '("appendix"))

   (unless (string-match-p "appendix" LaTeX-document-regexp)
     (set (make-local-variable 'LaTeX-document-regexp)
	  (concat LaTeX-document-regexp "\\|" "appendix")))

   (TeX-add-symbols
    ;; 7 Titles, addresses and so on
    '("shortTitle"  "Short title")
    '("shortAuthor" LaTeX-arg-author)
    '("address"     "Address")
    '("netaddress"  "Email address")
    '("personalURL" "Web page")
    '("ORCID"       "Digital identifier")

    ;; 7.1 Compilation articles
    '("contributor" "Contributor")
    '("makesignature" 0))

   ;; 8 Verbatim text
   (LaTeX-add-environments
    '("verbatim" LaTeX-env-ltugboat-verbatim))

   ;; 10.1 Acronyms and logos
   (TeX-add-symbols
    '("acro" "Acronym")
    "AMS"
    "AmS"
    "AmSLaTeX"
    "AmSTeX"
    "ANSI"
    "API"
    "ASCII"
    "aw"
    "AW"
    "BibLaTeX"
    "BibTeX"
    "BSD"
    "CandT"
    "ConTeXt"
    "CMkIV"
    "Cplusplus"
    "CPU"
    "CSczabbr"
    "CSS"
    "CSTUG"
    "CSV"
    "CTAN"
    "DTD"
    "DTK"
    "DVD"
    "DVI"
    "DVIPDFMx"
    "DVItoVDU"
    "ECMS"
    "EPS"
    "eTeX"
    "ExTeX"
    "FAQ"
    "FTP"
    "Ghostscript"
    "GNU"
    "GUI"
    "Hawaii"
    "HTML"
    "HTTP"
    "IDE"
    "IEEE"
    "ISBN"
    "ISO"
    "ISSN"
    "JPEG"
    "JTeX"
    "JoT"
    "KOMAScript"
    "LAMSTeX"
    "LyX"
    "MacOSX"
    "MathML"
    "mf"
    "MFB"
    "MkIV"
    "mp"
    "NTG"
    "NTS"
    "OMEGA"
    "OCP"
    "OOXML"
    "OTF"
    "OTP"
    "mtex"
    "Pas"
    "pcMF"
    "PCteX"
    "pcTeX"
    "PDF"
    "PGF"
    "PHP"
    "PiCTeX"
    "plain"
    "PNG"
    "POBox"
    "PS"
    "PSTricks"
    "RTF"
    "SC"
    "SGML"
    "SliTeX"
    "SQL"
    "stTeX"
    "STIX"
    "SVG"
    "TANGLE"
    "TB"
    "TIFF"
    "TP"
    "TeXhax"
    "TeXMaG"
    "TeXtures"
    "Textures"
    "TeXworks"
    "TeXXeT"
    "TFM"
    "Thanh"
    "TikZ"
    "ttn"
    "TTN"
    "TUB"
    "TUG"
    "tug"
    "UG"
    "UNIX"
    "VAX"
    "VnTeX"
    "VorTeX"
    "XML"
    "WEB"
    "WEAVE"
    "WYSIWYG"
    "XeTeX"
    "XeLaTeX"
    "XHTML"
    "XSL"
    "XSLFO"
    "XSLT"

    ;; 10.2 Other special typesetting
    '("Dash" 0)
    '("cs" (TeX-arg-eval let ((macro (completing-read
				      (TeX-argument-prompt optional nil
							   "Command")
				      (TeX-symbol-list))))
			 (format "%s" macro)))
    '("env" (TeX-arg-eval let ((env (completing-read
				     (TeX-argument-prompt optional nil
							  "Environment")
				     (LaTeX-environment-list))))
			  (format "%s" env)))
    '("meta"      "Text")
    '("tubbraced" "Text")
    '("nth"       "Number")

    ;; 12 Bibliography
    '("SetBibJustification"
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Justification")
		    '("\\raggedright"  "\\sloppy"))))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("shortTitle"   "{")
				("shortAuthor"  "{")
				("netaddress"   "{")
				("personalURL"  "{")
				("ORCID"        "{")
				("contributor"  "{")
				("acro"         "{")
				("cs"           "{")
				("env"          "{")
				("meta"         "{")
				("tubbraced"    "{")
				("nth"          "{"))
			      'textual)
     (font-latex-add-keywords '(("makesignature"   "")
				("SetBibJustification"  "{"))
			      'function)
     (font-latex-add-keywords '(("nameref" "{"))
			      'reference)))
 LaTeX-dialect)

(defvar LaTeX-ltugboat-class-options
  '("draft" "final" "preprint"
    "extralabel" "harvardcite" "noextralabel" "nonumber" "numbersec"
    "onecolumn" "rawcite" "runningfull" "runningminimal" "runningoff"
    "a4paper" "a5paper" "b5paper" "letterpaper" "legalpaper" "executivepaper"
    "titlepage" "notitlepage" "twocolumn" "leqno" "fleqn" "openbib")
  "Package options for the ltugboat class.")

;;; ltugboat.el ends here
