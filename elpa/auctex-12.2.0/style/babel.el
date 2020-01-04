;;; babel.el --- AUCTeX style for `babel.sty' version 3.31.

;; Copyright (C) 2005, 2007, 2013-2019 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2005-05-29
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

;; This file adds support for `babel.sty' version 3.31 from 2019/05/04.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; Needed for auto-parsing.
(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(defvar LaTeX-babel-language-list
  '("afrikaans"
    "azerbaijani"
    "bahasa" "indonesian" "indon" "bahasai" "bahasam" "malay" "meyalu"
    "basque"
    "breton"
    "bulgarian"
    "catalan"
    "croatian"
    "czech"
    "danish"
    "dutch"
    "english" "USenglish" "american" "UKenglish" "british"  "canadian"
    "australian" "newzealand"
    "esperanto"
    "estonian"
    "finnish"
    "french" "francais" "canadien" "acadian"
    "galician"
    "austrian" "german" "germanb" "ngerman" "naustrian"
    "greek" "polutonikogreek"
    "hebrew"
    "icelandic"
    "interlingua"
    "irish"
    "italian"
    "latin"
    "lowersorbian"
    "samin"
    "norsk" "nynorsk"
    "polish"
    "portuges" "portuguese" "brazilian" "brazil"
    "romanian"
    "russian"
    "scottish"
    "spanish"
    "slovak"
    "slovene"
    "swedish"
    "serbian"
    "turkish"
    "ukrainian"
    "uppersorbian"
    "welsh"
    ;; Extra languages mentioned in the `babel' manual.
    "albanian" "hindi" "thai" "thaicjk" "latvian" "turkmen" "hungarian" "magyar"
    "mongolian" "romansh" "lithuanian" "spanglish" "vietnamese" "japanese"
    "pinyin" "arabinc" "farsi" "ibygreek" "bgreek" "serbianic" "frenchle"
    "ethiop" "friulan")
  "List of languages supported by the babel LaTeX package.")

(defvar LaTeX-babel-babelprovide-key-val-options
  `(("import")
    ("captions")
    ("hyphenrules" ,(append '("+") LaTeX-babel-language-list))
    ("main")
    ("script")
    ("language")
    ("mapfont")
    ("intraspace")
    ("intrapenalty"))
  "Key=value options for `\\babelprovide' macro from `babel' package.")

(defun LaTeX-babel-active-languages ()
  "Return a list of languages used in the document."
  (let (main-language active-languages)
    ;; Loop over options provided to class and `babel' package at load time.
    (dolist (elt (append
		  ;; In most cases there is only one element in the alist, if
		  ;; there is more than one element, the first one should
		  ;; contain the class options of the current buffer.  So we can
		  ;; take the car of `LaTeX-provided-class-options'.
		  (cdr (car LaTeX-provided-class-options))
		  (cdr (assoc "babel" LaTeX-provided-package-options))))
      (setq elt (TeX-split-string "=" elt))
      (if (equal (car elt) "main")
	  ;; Starting from version 3.9 of `babel' package, languages can be set
	  ;; with the following syntax:
	  ;;   \usepackage[latin.medieval,main=danish,spanish.notilde]{babel}
	  ;; with `danish' being the default language.  When the default
	  ;; language is set with the `main' option, we record it and append to
	  ;; the list at the end.
	  (setq main-language (car (cdr elt)))
	;; Get rid of the modifiers (`medieval' and `notilde' in the above
	;; example).
	(setq elt (car (TeX-split-string "\\." (car elt))))
	(if (member elt LaTeX-babel-language-list)
	    ;; Append element to `active-languages' to respect loading order.
	    ;; `babel' package uses as default language the last loaded one,
	    ;; except if it is set with the `main' option.
	    (cl-pushnew elt active-languages :test #'equal))))
    (if main-language
	(cl-pushnew main-language active-languages :test #'equal))
    (nreverse active-languages)))

;; Setup for \babeltags: Note that the macro is \babeltags, we use
;; the version without `s' in order to reduce the hassle with AUCTeX
;; auto-generating the plural form:
(TeX-auto-add-type "babel-babeltag" "LaTeX")

(defvar LaTeX-babel-babeltags-regexp
  '("\\\\babeltags{\\([^}]+\\)}" 1 LaTeX-auto-babel-babeltag)
  "Matches the argument of `\\babeltags' from `babel' package.")

(defun LaTeX-babel-cleanup-babeltags ()
  "Parse defined babel tags and add them to AUCTeX."
  ;; Check if we parsed something at all
  (when (LaTeX-babel-babeltag-list)
    (let (results tag tags cmds)
      ;; Clean up the parsed results from characters we don't want;
      ;; also remove possible comment lines
      (setq results
	    (replace-regexp-in-string
	     "\\(%.*$\\|[ \n\r\t]\\)" ""
	     (mapconcat #'car (LaTeX-babel-babeltag-list) ",")))
      ;; Look if \babeltags was issued once with multiple entries or
      ;; more than once in the document:
      (if (string-match-p "," results)
	  (progn
	    (dolist (elt (split-string results "," t))
	      (setq tag (car (split-string elt "=" t)))
	      (push tag tags)
	      (push (list (concat "text" tag) t) cmds)
	      (push (list tag -1) cmds)))
	;; One \babeltags with one entry only
	(setq tag (car (split-string results "=" t)))
	(push tag tags)
	(push (list (concat "text" tag) t) cmds)
	(push (list tag -1) cmds))
      (mapc #'TeX-add-symbols cmds)
      (mapc #'LaTeX-add-environments tags)
      ;; Fontification
      (when (and (featurep 'font-latex)
		 (eq TeX-install-font-lock 'font-latex-setup))
	(font-latex-add-keywords (mapcar (lambda (x)
					   (list (concat "text" x)  "{"))
					 tags)
				 'textual)
	(font-latex-add-keywords (mapcar (lambda (x)
					   (list x  ""))
					 tags)
				 'type-declaration)))))

;; Setup for \babelfont:
(TeX-auto-add-type "babel-babelfont" "LaTeX")

(defvar LaTeX-babel-babelfont-regexp
  '("\\\\babelfont\\(?:\\[[^]]*\\]\\)?[ \t\n\r%]*{\\([^}]+\\)}"
    1 LaTeX-auto-babel-babelfont)
  "Matches the <font-family> argument of `\\babelfont' from `babel' package.")

(defun LaTeX-babel-cleanup-babelfont ()
  "Parse defined font-families and add them to AUCTeX."
  (when (LaTeX-babel-babelfont-list)
    (dolist (elt (mapcar #'car (LaTeX-babel-babelfont-list)))
      ;; Don't do anything for standard font-families:
      (unless (member elt '("rm" "sf" "tt"))
	;; Define \<font>family, \<font>default and \text<font>:
	(let ((fam (concat elt "family"))
	      (def (concat elt "default"))
	      (mac (concat "text" elt)))
	  (apply #'TeX-add-symbols
		 `((,fam -1)
		   (,def -1)
		   (,mac t)))
	  ;; Cater for fontification:
	  (when (and (featurep 'font-latex)
		     (eq TeX-install-font-lock 'font-latex-setup))
	    (font-latex-add-keywords `((,fam "")
				       (,def ""))
				     'type-declaration)
	    (font-latex-add-keywords `((,mac "{"))
				     'type-command)))))))

(defun LaTeX-babel-auto-prepare ()
  "Clear `LaTeX-auto-babel-babel*' before parsing."
  (setq LaTeX-auto-babel-babeltag  nil
	LaTeX-auto-babel-babelfont nil))

(defun LaTeX-babel-auto-cleanup ()
  "Process parsed elements."
  (LaTeX-babel-cleanup-babeltags)
  (LaTeX-babel-cleanup-babelfont))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-babel-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-babel-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun TeX-arg-babel-lang (optional &optional prompt)
  "Prompt for a language with completion and insert it as an argument."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Language")
    (LaTeX-babel-active-languages))
   optional))

(defun LaTeX-env-babel-lang (env)
  "Prompt for a language and insert it as an argument of ENV."
  (LaTeX-insert-environment
   env (format "{%s}" (completing-read "Language: "
				       (LaTeX-babel-active-languages)))))

(defun LaTeX-babel-load-languages ()
  "Load style files of babel active languages."
  ;; Run style hooks for every active language in loading order, so
  ;; `TeX-quote-language' will be correctly set.
  (mapc #'TeX-run-style-hooks (LaTeX-babel-active-languages)))

(TeX-add-style-hook
 "babel"
 (lambda ()
   (LaTeX-babel-load-languages)
   (add-hook 'LaTeX-after-usepackage-hook 'LaTeX-babel-load-languages nil t)

   ;; Add babel to the parser.
   (TeX-auto-add-regexp LaTeX-babel-babeltags-regexp)
   (TeX-auto-add-regexp LaTeX-babel-babelfont-regexp)

   ;; New symbols
   (TeX-add-symbols

    ;; 1.7 Basic language selectors
    '("selectlanguage" TeX-arg-babel-lang)
    '("foreignlanguage" TeX-arg-babel-lang t)

    ;; 1.9 More on selection
    '("babeltags" t)
    '("babelensure" (TeX-arg-key-val
		     (("include") ("exclude")
		      ("fontenc" (;; 128+ glyph encodings (text)
				  "OT1" "OT2" "OT3" "OT4" "OT6"
				  ;; 256 glyph encodings (text)
				  "T1" "T2A" "T2B" "T2C" "T3" "T4" "T5"
				  ;; 256 glyph encodings (text extended)
				  "X2"
				  ;; Other encodings
				  "LY1" "LV1" "LGR"))))
      TeX-arg-babel-lang)
    ;; 1.10 Shorthands
    '("shorthandon"    "Shorthands list")
    '("shorthandoff"   "Shorthands list")
    '("shorthandoff*"  "Shorthands list")
    '("useshorthands"  "Character")
    '("useshorthands*" "Character")
    '("defineshorthand"
      [ TeX-arg-eval mapconcat #'identity
		     (TeX-completing-read-multiple
		      (TeX-argument-prompt optional nil "Language(s)")
		      (LaTeX-babel-active-languages)) ""]
      t nil)
    '("aliasshorthand"   "Original" "Alias")
    '("languageshorthands" TeX-arg-babel-lang)
    '("babelshorthand"   "Short hand")
    '("ifbabelshorthand" "Character" t nil)

    ;; 1.12 The base option
    '("AfterBabelLanguage"
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Language")
		    LaTeX-babel-language-list)
      t)

    ;; 1.14 Selecting fonts
    '("babelfont"
      [ TeX-arg-eval mapconcat #'identity
		     (TeX-completing-read-multiple
		      (TeX-argument-prompt optional nil "Language(s)")
		      LaTeX-babel-language-list)
		     "," ]
      (TeX-arg-eval let ((fontfam (completing-read
				   (TeX-argument-prompt optional nil "font family")
				   '("rm" "sf" "tt"))))
		    ;; Make sure `tex-buf.el' is also loaded otherwise
		    ;; `TeX-check-engine-add-engines' is not defined.
		    ;; Then load `fontspec.el' and make sure the
		    ;; key-vals are up to date.
		    (unless (member "fontspec" (TeX-style-list))
		      (require 'tex-buf)
		      (TeX-check-engine-add-engines 'luatex 'xetex)
		      (TeX-run-style-hooks "fontspec")
		      (LaTeX-fontspec-auto-cleanup))
		    (LaTeX-add-babel-babelfonts fontfam)
		    (LaTeX-babel-cleanup-babelfont)
		    (format "%s" fontfam))
      [ TeX-arg-key-val LaTeX-fontspec-font-features-local]
      LaTeX-fontspec-arg-font)

    ;; 1.16 Creating a language
    '("babelprovide"
      [ TeX-arg-key-val LaTeX-babel-babelprovide-key-val-options ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Language")
		    LaTeX-babel-language-list))

    ;; 1.18 Getting the current language name
    '("languagename" 0)
    '("iflanguage" TeX-arg-babel-lang t nil)

    ;; 1.19 Hyphenation and line breaking
    '("babelhyphen"
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Type/Text")
		    '("soft" "hard" "repeat" "empty")))
    '("babelhyphen*"
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Type/Text")
		    '("soft" "hard" "repeat" "empty")))

    '("babelhyphenation"
      [ TeX-arg-eval mapconcat #'identity
		     (TeX-completing-read-multiple
		      (TeX-argument-prompt optional nil "Language(s)")
		      LaTeX-babel-language-list)
		     "," ]
      t)

    ;; 1.20 Selecting scripts
    '("ensureascii" "Text")

    ;; 1.22 Language attributes
    '("languageattribute" TeX-arg-babel-lang t))

   ;; New environments: 1.8 Auxiliary language selectors
   (LaTeX-add-environments
    '("otherlanguage" LaTeX-env-babel-lang)
    '("otherlanguage*" LaTeX-env-babel-lang)
    '("hyphenrules" LaTeX-env-babel-lang))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("selectlanguage"     "{")
				("foreignlanguage"    "{{")
				("babeltags"          "{")
				("babelensure"        "{{")
				("shorthandon"        "{")
				("shorthandoff"       "*{")
				("useshorthands"      "*{")
				("languageshorthands" "{")
				("babelshorthand"     "{")
				("AfterBabelLanguage" "{")
				("babelfont"          "[{[{")
				("babelprovide"       "[{")
				("languagename"       "")
				("iflanguage"         "{{{")
				("babelhyphen"        "*{")
				("babelhyphenation"   "[{")
				("ensureascii"        "{"))
			      'function)
     (font-latex-add-keywords '(("defineshorthand"    "[{{")
				("aliasshorthand"     "{{")
				("languageattribute"  "{{"))
			      'variable)))
 LaTeX-dialect)

(defun LaTeX-babel-package-options ()
  "Prompt for package options for the babel package."
  (TeX-read-key-val
   t
   (append
    '(("KeepShorthandsActive")
      ("activeacute")
      ("activegrave")
      ("shorthands" ("off"))
      ("safe" ("none" "ref" "bib"))
      ("math" ("active" "normal"))
      ("config")
      ("main" LaTeX-babel-language-list)
      ("headfoot" LaTeX-babel-language-list)
      ("noconfigs")
      ("nocase")
      ("silent")
      ("showlanguages")
      ("nocase")
      ("silent")
      ("strings" ("generic" "unicode" "encoded"
		  "OT1" "OT2" "OT3" "OT4" "OT6"
		  "T1"  "T2A" "T2B" "T2C" "T3" "T4" "T5"
		  "X2"  "LY1" "LV1" "LGR"))
      ("hyphenmap" ("off" "main" "select" "other" "other*"))
      ("bidi" ("default" "basic" "basic-r" "bidi-l" "bidi-r"))
      ("layout" ("sectioning" "counters" "lists" "captions"
		 "contents" "footnotes" "columns" "extras"))
      ("base"))
    (mapcar #'list LaTeX-babel-language-list))))

;;; babel.el ends here
