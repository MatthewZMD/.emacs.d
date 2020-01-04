;;; listings.el --- AUCTeX style for `listings.sty'

;; Copyright (C) 2004, 2005, 2009, 2013--2019 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-10-17
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

;; This file adds support for `listings.sty'.
;;
;; May 2015: The style detects new environments defined with
;; `\lstnewenvironment'.  Users need to invoke `C-c C-n' for this.
;;
;; October 2015: The style detects new "styles" defined with
;; `\lstdefinestyle' and offers them during key-value query.
;;
;; January 2017: Put label in opt. argument of environment.
;;
;; October 2018: Extract label context for RefTeX.
;;
;; FIXME: Please make me more sophisticated!

;;; Code:

;; Needed for compiling `cl-pushnew':
(eval-when-compile
  (require 'cl-lib))

;; Needed for auto-parsing:
(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(declare-function font-latex-update-font-lock
		  "font-latex"
		  (&optional syntactic-kws))

;; The following are options taken from chapter 4 of the listings
;; manual (2007/02/22 Version 1.4).
(defvar LaTeX-listings-key-val-options
  '(;; Space and placement
    ("float" ("t" "b" "p" "h")) ; Support [*] as an optional prefix and that
				; tbph are not exclusive.
    ("floatplacement" ("t" "b" "p" "h"))
    ("aboveskip")
    ("belowskip")
    ("lineskip")
    ("boxpos" ("b" "c" "t"))
    ;; The printed range
    ("print" ("true" "false"))
    ("firstline")
    ("lastline")
    ("linerange")
    ("showlines" ("true" "false"))
    ("emptylines")
    ("gobble")
    ;; Languages and styles
    ("style")
    ("language")
    ("alsolanguage")
    ("defaultdialect")
    ("printpod" ("true" "false"))
    ("usekeywordsintag" ("true" "false"))
    ("tagstyle")
    ("markfirstintag")
    ("makemacrouse" ("true" "false"))
    ;; Figure out the appearance
    ("basicstyle")
    ("identifierstyle")
    ("commentstyle")
    ("stringstyle")
    ("keywordstyle")
    ("classoffset")
    ("texcsstyle")
    ("directivestyle")
    ("emph")
    ("moreemph")
    ("deleteemph")
    ("emphstyle")
    ("delim")
    ("moredelim")
    ("deletedelim")
    ;; Getting all characters right
    ("extendedchars" ("true" "false"))
    ("inputencoding") ; Could make use of `latex-inputenc-coding-alist'.
    ("upquote" ("true" "false"))
    ("tabsize")
    ("showtabs" ("true" "false"))
    ("tab")
    ("showspaces" ("true" "false"))
    ("showstringspaces" ("true" "false"))
    ("formfeed")
    ;; Line numbers
    ("numbers" ("none" "left" "right"))
    ("stepnumber")
    ("numberfirstline" ("true" "false"))
    ("numberstyle")
    ("numbersep")
    ("numberblanklines" ("true" "false"))
    ("firstnumber" ("auto" "last")) ; Can also take a number.
    ("name")
    ;; Captions
    ("title")
    ("caption") ; Insert braces?
    ;; Label is inserted as part of environment insertion; see below
    ;; for "lstlisting" in style hook
    ;; ("label")
    ("nolol" ("true" "false"))
    ("numberbychapter" ("true" "false"))
    ("captionpos" ("t" "b")) ; Can be a subset of tb.
    ("abovecaptionskip")
    ("belowcaptionskip")
    ;; Margins and line shape
    ("linewidth")
    ("xleftmargin")
    ("xrightmargin")
    ("resetmargins" ("true" "false"))
    ("breaklines" ("true" "false"))
    ("breakatwhitespace" ("true" "false"))
    ("prebreak")
    ("postbreak")
    ("breakindent")
    ("breakautoindent" ("true" "false"))
    ;; Frames
    ("frame" ("none" "leftline" "topline" "bottomline" "lines" "single"
	      "shadowbox"
	      ;; Alternative to the above values.  A subset of trblTRBL can be
	      ;; given.
	      "t" "r" "b" "l" "T" "R" "B" "L"))
    ("frameround" ("t" "f")) ; The input actually has to be four times {t,f}.
    ("framesep")
    ("rulesep")
    ("framerule")
    ("framexleftmargin")
    ("framexrightmargin")
    ("framextopmargin")
    ("framebottommargin")
    ("backgroundcolor")
    ("rulecolor")
    ("fillcolor")
    ("fulesepcolor")
    ("frameshape")
    ;; Indexing
    ("index")
    ("moreindex")
    ("deleteindex")
    ("indexstyle")
    ;; Column alignment
    ("columns" ("fixed" "flexible" "fullflexible" "spaceflexible")) ;
					; Also supports an optional
					; argument with {c,l,r}.
    ("flexiblecolumns" ("true" "false"))
    ("keepspaces" ("true" "false"))
    ("basewidth")
    ("fontadjust" ("true" "false"))
    ;; Escaping to LaTeX
    ("texcl" ("true" "false"))
    ("mathescape" ("true" "false"))
    ("escapechar")
    ("escapeinside")
    ("escapebegin")
    ("escapeend")
    ;; Interface to fancyvrb
    ("fancyvrb" ("true" "false"))
    ("fvcmdparams")
    ("morefvcmdparams")
    ;; Language definitions
    ("keywordsprefix")
    ("keywords")
    ("morekeywords")
    ("deletekeywords")
    ("texcs")
    ("moretexcs")
    ("deletetexcs")
    ("directives")
    ("moredirectives")
    ("deletedirectives")
    ("sensitive" ("true" "false"))
    ("alsoletter")
    ("alsodigit")
    ("alsoother")
    ("otherkeywords")
    ("tag")
    ("string")
    ("morestring")
    ("deletestring")
    ("comment")
    ("morecomment")
    ("deletecomment")
    ("keywordcomment")
    ("morekeywordcomment")
    ("deletekeywordcomment")
    ("keywordcommentsemicolon")
    ("podcomment" ("true" "false"))
    ;; The following are all options from chapter 5, which are
    ;; experimental
    ;; Export of identifiers
    ("procnamekeys")
    ("moreprocnamekeys")
    ("deleteprocnamekeys")
    ("procnamestyle")
    ("indexprocnames" ("true" "false"))
    ;; Hyperlink references
    ("hyperref")
    ("morehyperref")
    ("deletehyperref")
    ("hyperanchor")
    ("hyperlink")
    ;; Literate programming
    ("literate") ;; three arguments: replace,replacement text,length
    ;; LGrind definitions
    ("lgrindef")
    ;; Arbitrary linerange markers
    ("rangebeginprefix")
    ("rangebeginsuffix")
    ("rangeendprefix")
    ("rangeendsuffix")
    ("rangeprefix")
    ("rangesuffix")
    ("includerangemarker" ("true" "false"))
    ;; Multicolumn Listing
    ("multicolumn"))
  "Key=value options for listings macros and environments.")

(defvar LaTeX-listings-key-val-options-local nil
  "Buffer-local Key=value options for listings macros and environments.")
(make-variable-buffer-local 'LaTeX-listings-key-val-options-local)

;; Setup for \lstnewenvironment:
(defvar LaTeX-auto-listings-lstnewenvironment nil
  "Temporary for parsing the arguments of `\\lstnewenvironment'
from `listings' package.")

(defvar LaTeX-listings-lstnewenvironment-regexp
  `(,(concat "\\\\lstnewenvironment"
	     "[ \t\n\r]*{\\([A-Za-z0-9]+\\)}%?"
	     "[ \t\n\r]*\\[?\\([0-9]?\\)\\]?%?"
	     "[ \t\n\r]*\\(\\[\\)?")
    (1 2 3) LaTeX-auto-listings-lstnewenvironment)
  "Matches the argument of `\\lstnewenvironment' from `listings.sty'.")

;; Setup for \lstdefinestyle:
(TeX-auto-add-type "listings-lstdefinestyle" "LaTeX")

(defvar LaTeX-listings-lstdefinestyle-regexp
  '("\\\\lstdefinestyle{\\([^}]+\\)}"
    1 LaTeX-auto-listings-lstdefinestyle)
  "Matches the argument of \"\\lstdefinestyle\" from
\"listings\" package.")

;; Setup for parsing the labels inside optional arguments:

(defvar LaTeX-listings-key-val-label-regexp
  `(,(concat
      "\\\\begin{lstlisting}" (LaTeX-extract-key-value-label))
    1 LaTeX-auto-label)
  "Matches the label inside an optional argument after \\begin{lstlisting}.")

(defun LaTeX-listings-update-style-key ()
  "Update the \"style\" key from `LaTeX-listings-key-val-options-local'
with user-defined values via the \"lstdefinestyle\" macro."
  (let* ((elt (assoc "style" LaTeX-listings-key-val-options-local))
	 (key (car elt))
	 (temp (copy-alist LaTeX-listings-key-val-options-local))
	 (opts (assq-delete-all (car (assoc key temp)) temp)))
    (cl-pushnew (list key (TeX-delete-duplicate-strings
			   (mapcar #'car (LaTeX-listings-lstdefinestyle-list))))
		opts :test #'equal)
    (setq LaTeX-listings-key-val-options-local
	  (copy-alist opts))))

(defun LaTeX-listings-auto-prepare ()
  "Clear temporary variable from `listings.sty' before parsing."
  (setq LaTeX-auto-listings-lstnewenvironment nil)
  (setq LaTeX-auto-listings-lstdefinestyle    nil))

(defun LaTeX-listings-auto-cleanup ()
  "Process the parsed results of \"\\lstnewenvironment\" and
\"\\lstdefinestyle\"."
  (dolist (env-args LaTeX-auto-listings-lstnewenvironment)
    (let ((env  (car   env-args))
	  (args (cadr  env-args))
	  (opt  (nth 2 env-args)))
      (cond (;; opt. 1st argument and mandatory argument(s)
	     (and args (not (string-equal args ""))
		  opt  (not (string-equal opt  "")))
	     (LaTeX-add-environments
	      `(,env
		LaTeX-env-args
		[TeX-arg-key-val LaTeX-listings-key-val-options-local]
		(LaTeX-env-label-as-keyval "caption")
		,(1- (string-to-number args)))))
	    (;; mandatory argument(s) only
	     (and args (not (string-equal args ""))
		  (string-equal opt ""))
	     (LaTeX-add-environments
	      (list env (string-to-number args))))
	    (t ; No args
	     (LaTeX-add-environments (list env))))
      (add-to-list 'LaTeX-indent-environment-list `(,env current-indentation) t)
      (add-to-list 'LaTeX-verbatim-environments-local env t)
      (add-to-list 'LaTeX-label-alist `(,env . LaTeX-listing-label) t)
      ;; Add new env to parser for labels in opt. argument:
      (TeX-auto-add-regexp `(,(concat "\\\\begin{" env "}"
				      (LaTeX-extract-key-value-label))
			     1 LaTeX-auto-label))
      ;; Tell RefTeX
      (when (fboundp 'reftex-add-label-environments)
	(reftex-add-label-environments
	 `((,env ?l "lst:" "~\\ref{%s}"
		 LaTeX-listings-reftex-label-context-function
		 (regexp "[Ll]isting")))))
      ;; Fontification
      (when (and (fboundp 'font-latex-add-keywords)
		 (fboundp 'font-latex-update-font-lock)
		 (eq TeX-install-font-lock 'font-latex-setup))
	;; Tell font-lock about the update.
	(font-latex-update-font-lock t))
      ;; Add new env's to `ispell-tex-skip-alist': skip the entire env
      (TeX-ispell-skip-setcdr `(,(cons env (concat "\\\\end{" env "}"))))))
  (when (LaTeX-listings-lstdefinestyle-list)
    (LaTeX-listings-update-style-key)))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-listings-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-listings-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-listings-reftex-label-context-function (env)
  "Extract and return a context string for RefTeX.
The context string is the value given to the caption key.  If no
caption key is found, an error is issued."
  (let* ((envstart (save-excursion
		     (re-search-backward (concat "\\\\begin{" env "}")
					 nil t)))
	 (capt-key (save-excursion
		     (re-search-backward "caption[ \t\n\r%]*=[ \t\n\r%]*"
					 envstart t)))
	 capt-start capt-end)
    (if capt-key
	(save-excursion
	  (goto-char capt-key)
	  (re-search-forward
	   "caption[ \t\n\r%]*=[ \t\n\r%]*" nil t)
	  (cond (;; Short caption inside [] is available, extract it only
		 (looking-at-p (regexp-quote (concat TeX-grop LaTeX-optop)))
		 (forward-char)
		 (setq capt-start (1+ (point)))
		 (setq capt-end (1- (progn (forward-sexp) (point)))))
		;; Extract the entire caption which is enclosed in braces
		((looking-at-p TeX-grop)
		 (setq capt-start (1+ (point)))
		 (setq capt-end (1- (progn (forward-sexp) (point)))))
		;; Extract everything to next comma ,
		(t
		 (setq capt-start (point))
		 (setq capt-end (progn (skip-chars-forward "^,") (point)))))
	  ;; Return the extracted string
	  (buffer-substring-no-properties capt-start capt-end))
      (error "No caption found"))))

(TeX-add-style-hook
 "listings"
 (lambda ()

   ;; Add to parser:
   (TeX-auto-add-regexp LaTeX-listings-lstnewenvironment-regexp)
   (TeX-auto-add-regexp LaTeX-listings-lstdefinestyle-regexp)
   (TeX-auto-add-regexp LaTeX-listings-key-val-label-regexp)

   ;; Local version of key-val options:
   (setq LaTeX-listings-key-val-options-local
	 (copy-alist LaTeX-listings-key-val-options))

   ;; New symbols
   (TeX-add-symbols
    '("lstalias" ["Alias dialect"] "Alias" ["Dialect"] "Language")
    '("lstdefinestyle"
      (TeX-arg-eval
       (lambda ()
	 (let ((name (TeX-read-string "Style name: ")))
	   (LaTeX-add-listings-lstdefinestyles name)
	   (LaTeX-listings-update-style-key)
	   (format "%s" name))))
      (TeX-arg-key-val LaTeX-listings-key-val-options-local))
    '("lstinline" [TeX-arg-key-val LaTeX-listings-key-val-options-local]
      TeX-arg-verb-delim-or-brace)
    '("lstinputlisting" [TeX-arg-key-val LaTeX-listings-key-val-options-local]
      TeX-arg-file)
    "lstlistoflistings"
    '("lstnewenvironment" "Name" ["Number or arguments"] ["Default argument"]
      "Starting code" "Ending code")
    '("lstset" (TeX-arg-key-val LaTeX-listings-key-val-options-local))
    '("lstloadlanguages" t)
    ;; 4.17 Short Inline Listing Commands
    '("lstMakeShortInline" [ "Options" ] "Character")
    '("lstDeleteShortInline" "Character")

    "lstgrinddeffile" "lstaspectfiles" "lstlanguagefiles"
    "lstlistingname" "lstlistlistingname")

   ;; New environments
   (LaTeX-add-environments
    '("lstlisting" LaTeX-env-args
      [TeX-arg-key-val LaTeX-listings-key-val-options-local]
      (LaTeX-env-label-as-keyval "caption")))

   ;; Append "lstlisting" to `LaTeX-label-alist':
   (add-to-list 'LaTeX-label-alist '("lstlisting" . LaTeX-listing-label) t)

   ;; Filling
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
		'("lstlisting" current-indentation) t)
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")

   ;; RefTeX support lstlistings environment via
   ;; `reftex-label-alist-builtin'.  We add the same thing here only
   ;; with our function as 5th element:
   (when (fboundp 'reftex-add-label-environments)
     (reftex-add-label-environments
      '(("lstlisting" ?l "lst:" "~\\ref{%s}"
	 LaTeX-listings-reftex-label-context-function
	 (regexp "[Ll]isting")))))

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-update-font-lock)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("lstnewenvironment" "{[[{{")) 'function)
     (font-latex-add-keywords '(("lstinputlisting" "[{")) 'reference)
     (font-latex-add-keywords '(("lstinline" "[")
				("lstlistoflistings" ""))
			      'textual)
     (font-latex-add-keywords '(("lstalias" "{{")
				("lstdefinestyle" "{{")
				("lstset" "{"))
			      'variable)
     ;; Tell font-lock about the update.
     (font-latex-update-font-lock t)))
 LaTeX-dialect)

(defvar LaTeX-listings-package-options '("draft" "final" "savemem"
					 "noaspects"
					 ;; procnames is mentioned in
					 ;; Section 5.2
					 "procnames")
  "Package options for the listings package.")

;;; listings.el ends here
