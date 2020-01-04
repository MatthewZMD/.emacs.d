;;; pythontex.el --- AUCTeX style for `pythontex.sty' (v0.16)

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2018-12-01
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

;; This file adds support for `pythontex.sty' v0.16 from
;; 2017/07/20.  `pythontex.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
;; `LaTeX-fancyvrb-key-val-options-local' will be defined after
;; loading `fvextra.el' which loads `fancyvrb.el' in return:
(defvar LaTeX-fancyvrb-key-val-options-local)

;; These are provided by `font-latex.el':
(defvar font-latex-syntactic-keywords-extra)
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))
(declare-function font-latex-update-font-lock
		  "font-latex"
		  (&optional syntactic-kws))

;; The next two are provided by `newfloat.el':
(declare-function LaTeX-add-newfloat-DeclareFloatingEnvironments
		  "newfloat"
		  (&rest newfloat-declarefloatingenvironments))
(declare-function LaTeX-newfloat-auto-cleanup
		  "newfloat" ())

;; Needed for auto-parsing:
(require 'tex)

(defvar LaTeX-pythontex-pygmentize-program (executable-find "pygmentize")
  "Path to pygmentize executable.")

(defvar LaTeX-pythontex-language-list nil
  "List containing languages provided by pymentize program.")

(defun LaTeX-pythontex-language-list (&rest _ignored)
  "Return a list of languages provided by pymentize program.
Update the variable `LaTeX-pythontex-language-list' if still nil."
  (or LaTeX-pythontex-language-list
      (when LaTeX-pythontex-pygmentize-program
	(with-temp-buffer
	  (shell-command (concat LaTeX-pythontex-pygmentize-program " -L lexers")
			 (current-buffer))
	  (goto-char (point-min))
	  (let (languages)
	    (while (re-search-forward "^\\*[[:space:]]\\([^:]+\\):" nil t)
	      (dolist (lang (split-string (match-string 1) "[[:space:],]" t))
		(push lang languages)))
	    (setq LaTeX-pythontex-language-list languages))))))

(defvar LaTeX-pythontex-package-options-list
  `(("usefamily"         ("py" "sympy" "pylab" "rb" "ruby" "jl" "julia" "octave"))
    ("gobble"            ("none" "auto"))
    ("beta"              ("true" "false"))
    ("runall"            ("true" "false"))
    ("rerun"             ("never" "modified" "errors" "warnings" "always"))
    ("hashdependencies"  ("true" "false"))
    ("autoprint"         ("true" "false"))
    ("autostdout"        ("true" "false"))
    ("debug")
    ("makestderr"        ("true" "false"))
    ("stderrfilename"    ("full" "session" "genericfile" "genericscript"))
    ("pyfuture"          ("none" "all" "default"))
    ("pyconfuture"       ("none" "all" "default"))
    ("upquote"           ("true" "false"))
    ("fixlr"             ("true" "false"))
    ("keeptemps"         ("all" "code" "none"))
    ("prettyprinter"     ("pygments" "fancyvrb"))
    ("prettyprintinline" ("true" "false"))
    ("pygments"          ("true" "false"))
    ("pyginline"         ("true" "false"))
    ("pyglexer"          ,(LaTeX-pythontex-language-list))
    ("pygopt"            ("style" "texcomments" "mathescape"))
    ("fvextfile")
    ("pyconbanner"       ("none" "standard" "default" "pyversion"))
    ("pyconfilename"     ("stdin" "console"))
    ("depythontex"       ("true" "false")))
  "Package options for the pythontex package.")

(defun LaTeX-pythontex-package-options ()
  "Prompt for package options for the pythontex package."
  (TeX-read-key-val t LaTeX-pythontex-package-options-list))

(defvar LaTeX-pythontex-family-list
  '("py" "sympy" "pylab" "rb" "ruby" "jl" "julia" "octave")
  "List of language families provided by pythontex package.")

(defun LaTeX-env-pythontex (environment)
  "Insert ENVIRONMENT provided by pythontex package."
  (let ((session (TeX-read-string
		  (TeX-argument-prompt t nil "Session")))
	(fvkeyval (TeX-read-key-val t LaTeX-fancyvrb-key-val-options-local)))
    (LaTeX-insert-environment environment
			      (concat
			       (when (and session (not (string= session "")))
				 (concat LaTeX-optop session LaTeX-optcl))
			       ;; We need an extra pair of brackets
			       ;; when no session is given but
			       ;; key=vals are available
			       (when (and session (string= session "")
					  fvkeyval (not (string= fvkeyval "")))
				 (concat LaTeX-optop LaTeX-optcl))
			       (when (and fvkeyval (not (string= fvkeyval "")))
				 (concat LaTeX-optop fvkeyval LaTeX-optcl))))))

;; Setup for \saveprintpythontex & \savestdoutpythontex &
;; \savestderrpythontex
(TeX-auto-add-type "pythontex-savecontent" "LaTeX")

(defvar LaTeX-pythontex-savecontent-regexp
  `(,(concat "\\\\"
	     (regexp-opt '("saveprintpythontex"
			   "savestdoutpythontex"
			   "savestderrpythontex")
			 "\\(?:")
	     "{\\([^}]+\\)}")
    1 LaTeX-auto-pythontex-savecontent)
  "Matches the argument of \\save(print|stdout|stderr)pythontex macros.")

;; Setup for \setpythontexlistingenv:
(TeX-auto-add-type "pythontex-setpythontexlistingenv" "LaTeX")

(defvar LaTeX-pythontex-setpythontexlistingenv-regexp
  '("\\\\setpythontexlistingenv{\\([^}]+\\)}"
    1 LaTeX-auto-pythontex-setpythontexlistingenv)
  "Matches the argument of \\setpythontexlistingenv macro.")

(defun LaTeX-pythontex-auto-prepare ()
  "Clear various `LaTeX-auto-pythontex-*' before parsing."
  (setq LaTeX-auto-pythontex-savecontent nil
	LaTeX-auto-pythontex-setpythontexlistingenv nil))

(defun LaTeX-pythontex-auto-cleanup ()
  "Process the parsing results for \\setpythontexlistingenv macro."
  ;; Use `LaTeX-add-newfloat-DeclareFloatingEnvironments' on parsed
  ;; elements and then run `LaTeX-newfloat-auto-cleanup'.
  (dolist (env (mapcar #'car (LaTeX-pythontex-setpythontexlistingenv-list)))
    (LaTeX-add-newfloat-DeclareFloatingEnvironments `(,env "verbatim")))
  (LaTeX-newfloat-auto-cleanup))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-pythontex-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-pythontex-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-pythontex-add-syntactic-keywords-extra (type macro)
  "Add MACRO from pythontex.sty to `font-latex-syntactic-keywords-extra'.
TYPE is one of the symbols `brace' or `delim' indicating how
verbatim text is enclosed after the macro.  MACRO is a string or
a list of strings."
  (let ((syntax (if (eq type 'brace)
		    '((1 "|") (2 "|"))
		  '((1 "\"") (2 ".") (3 "\""))))
	regexp)
    (when (listp macro)
      (setq macro (regexp-opt macro "\\(?:")))
    (setq regexp `(,(concat
		     ;; The backslash
		     (regexp-quote TeX-esc)
		     ;; Name of the macro(s)
		     macro
		     ;; The first mandatory argument is the lexer
		     "\\(?:{[^}]+}\\)"
		     ;; With 'brace, allow braced sub-groups otherwise
		     ;; we stop matching too early.  With 'delim, copy
		     ;; font-latex.el:
		     (if (eq type 'brace)
			 (concat "\\({\\)"
				   "\\(?:[^}{]*"
				     "\\(?:{[^}{]*"
				       "\\(?:{[^}{]*"
					 "\\(?:{[^}{]*}[^}{]*\\)*"
				       "}[^}{]*\\)*"
				     "}[^}{]*\\)*"
				   "\\)"
				 "\\(}\\)")
		       (concat
			;; Opening delimiter
			"\\([^a-z@*\n\f{]\\).*?"
			;; Closing delimiter
			"\\(" (regexp-quote TeX-esc) "*\\)\\(\\1\\)")))))
    (add-to-list 'font-latex-syntactic-keywords-extra (append regexp syntax))))

(TeX-add-style-hook
 "pythontex"
 (lambda ()

   ;; Load only the relevant style hooks within AUCTeX
   (TeX-run-style-hooks "fvextra" "newfloat")

   ;; Add pythontex to the parser:
   (TeX-auto-add-regexp LaTeX-pythontex-savecontent-regexp)
   (TeX-auto-add-regexp LaTeX-pythontex-setpythontexlistingenv-regexp)

   ;; We need this for Filling:
   (make-local-variable 'LaTeX-indent-environment-list)

   ;; 4.2.4 Default families
   (let* ((verb-macs '(;; python
		       "py" "pyc" "pys" "pyv" "pyb"
		       "pycon" "pyconc" "pyconv"
		       ;; Python + pylab (matplotlib module)
		       "pylab" "pylabc" "pylabs" "pylabv" "pylabb"
		       "pylabcon" "pylabconc" "pylabconv"
		       ;; Python + SymPy
		       "sympy" "sympyc" "sympys" "sympyv" "sympyb"
		       "sympycon" "sympyconc" "sympyconv"))
	  (verb-envs '(;;python
		       "pycode" "pysub" "pyverbatim" "pyblock"
		       "pyconsole" "pyconcode" "pyconverbatim"
		       ;; Python + pylab (matplotlib module)
		       "pylabcode" "pylabsub" "pylabverbatim" "pylabblock"
		       "pylabconsole" "pylabconcode" "pylabconverbatim"
		       ;; Python + SymPy
		       "sympycode" "sympysub" "sympyverbatim" "sympyblock"
		       "sympyconsole" "sympyconcode" "sympyconverbatim"))
	  (verb-envs-regexp (regexp-opt verb-envs "\\(?:")))
     (apply #'TeX-add-symbols
	    (mapcar (lambda (mac)
		      (list mac [ "Session" ] 'TeX-arg-verb-delim-or-brace))
		    verb-macs))
     (apply #'LaTeX-add-environments
	    (mapcar (lambda (env)
		      (list env 'LaTeX-env-pythontex))
		    verb-envs))
     ;; Filling:
     (dolist (mac verb-macs)
       (add-to-list 'LaTeX-verbatim-macros-with-delims-local mac)
       (add-to-list 'LaTeX-verbatim-macros-with-braces-local mac))
     (dolist (env verb-envs)
       (add-to-list 'LaTeX-indent-environment-list
		    `(,env current-indentation) t))
     ;; Fontification
     (when (and (fboundp 'font-latex-add-keywords)
		(fboundp 'font-latex-update-font-lock)
		(boundp 'font-latex-syntactic-keywords-extra)
		(eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords (mapcar (lambda (mac)
					  (list mac "["))
					verb-macs)
				'textual)
       ;; We can't use the fontification provided when verbatim
       ;; environments are added to
       ;; `LaTeX-verbatim-environments-local' -- pythontex
       ;; environments have 2 optional arguments and `font-latex.el'
       ;; recognizes only 1 optional which breaks the fontification.
       ;; We add the envs to `font-latex-syntactic-keywords-extra' and
       ;; define a customized regexp to match 2 optional arguments.
       (add-to-list 'font-latex-syntactic-keywords-extra
		    `(,(concat
			"^[ \t]*\\\\begin *{\\(?:"
			verb-envs-regexp
			"\\)}"
			"[ \t]*\\(?:%.*\n[ \t]*\\)?"
			"\\(?:\\[[^][]*\\(?:\\[[^][]*\\][^][]*\\)*\\]\\)\\{0,2\\}"
			"\\(\n\\|.\\)")
		      (1 "|" t)))
       (add-to-list 'font-latex-syntactic-keywords-extra
		    `(,(concat "\\(\\\\\\)end *{\\(?:"
			       verb-envs-regexp
			       "\\)}")
		      (1 "|" t))))
     ;; Tell font-lock about the update.
     (font-latex-update-font-lock t))

   (TeX-add-symbols
    ;; 4.2.5 Custom code
    ;; pythontexcustomc[<position>]{<family>}{<code>}
    '("pythontexcustomc"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Position")
		     '("begin" "end") ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Family")
		    LaTeX-pythontex-family-list)
      t)

    ;; 4.2.7 Formatting of typeset code
    ;; \setpythontexfv[<family>]{<fancyvrb settings>}
    '("setpythontexfv"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Family")
		     LaTeX-pythontex-family-list ]
      (TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local))

    ;; \setpythontexprettyprinter[<family>]{<printer>}
    '("setpythontexprettyprinter"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Family")
		     (cons "auto" LaTeX-pythontex-family-list) ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Printer")
		    '("text" "bw" "fancyvrb" "pygments")))

    ;; \setpythontexpyglexer[<family>]{<pygments lexer>}
    '("setpythontexpyglexer"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Family")
		     LaTeX-pythontex-family-list ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Pygments lexer")
		    (LaTeX-pythontex-language-list)))

    ;; \setpythontexpygopt[<family>]{<pygments options>}
    '("setpythontexpygopt"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Family")
		     LaTeX-pythontex-family-list ]
      (TeX-arg-key-val
       (("style") ("texcomments") ("mathescape"))))

    ;; 4.2.8 Access to printed content (stdout)
    ;; \printpythontex[<mode>][<options>]
    '("printpythontex"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Mode")
		     '("raw" "verb" "verbatim") ]
      [ TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local ] )

    ;; \stdoutpythontex[<mode>][<options>]
    '("stdoutpythontex"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Mode")
		     '("raw" "verb" "verbatim") ]
      [ TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local ] )

    ;;\saveprintpythontex{<name>}
    '("saveprintpythontex"
      (TeX-arg-eval (lambda ()
		      (let ((name (TeX-read-string
				   (TeX-argument-prompt optional nil "Name"))))
			(LaTeX-add-pythontex-savecontents name)
			(format "%s" name)))))

    ;;\savestdoutpythontex{<name>}
    '("savestdoutpythontex"
      (TeX-arg-eval (lambda ()
		      (let ((name (TeX-read-string
				   (TeX-argument-prompt optional nil "Name"))))
			(LaTeX-add-pythontex-savecontents name)
			(format "%s" name)))))

    ;; \useprintpythontex[<verbatim options>][<fancyvrb options>]{<name>}
    ;; I assume <verbatim options> is meant to be <mode>
    '("useprintpythontex"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Mode")
		     '("raw" "verb" "verbatim") ]
      [ TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Name")
		    (LaTeX-pythontex-savecontent-list)))

    ;; \usestdoutpythontex[<verbatim options>][<fancyvrb options>]{<name>}
    ;; I assume <verbatim options> is meant to be <mode>
    '("usestdoutpythontex"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Mode")
		     '("raw" "verb" "verbatim") ]
      [ TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Name")
		    (LaTeX-pythontex-savecontent-list)))

    ;; \stderrpythontex[<mode>][<fancyvrb options>]
    '("stderrpythontex"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Mode")
		     '("raw" "verb" "verbatim") ]
      [ TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local ] )


    ;;\savestderrpythontex{<name>}
    '("savestderrpythontex"
      (TeX-arg-eval (lambda ()
		      (let ((name (TeX-read-string
				   (TeX-argument-prompt optional nil "Name"))))
			(LaTeX-add-pythontex-savecontents name)
			(format "%s" name)))))

    ;; \usestderrpythontex[<mode>][<fancyvrb options>]{<name>}
    '("usestderrpythontex"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Mode")
		     '("raw" "verb" "verbatim") ]
      [ TeX-arg-key-val LaTeX-fancyvrb-key-val-options-local ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Name")
		    (LaTeX-pythontex-savecontent-list)))

    ;;\setpythontexautoprint{<boolean>}
    '("setpythontexautoprint"
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Boolean value")
		    '("true" "false")))

    ;; \setpythontexautostdout{<boolean>}
    '("setpythontexautostdout"
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Boolean value")
		    '("true" "false")))

    ;; 4.3 Pygments commands and environments
    ;; \pygment{<lexer>}<opening delim><code><closing delim>
    '("pygment"
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Lexer")
		    (LaTeX-pythontex-language-list))
      TeX-arg-verb-delim-or-brace)

    ;; \inputpygments[<fancyvrb settings>]{<lexer>}{<external file>}
    '("inputpygments"
      [ TeX-arg-eval LaTeX-fancyvrb-key-val-options-local ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Lexer")
		    (LaTeX-pythontex-language-list))
      TeX-arg-file-name)

    ;; \setpygmentsfv[<lexer>]{<fancyvrb settings>}
    '("setpygmentsfv"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Lexer")
		     (LaTeX-pythontex-language-list) ]
      (TeX-arg-eval LaTeX-fancyvrb-key-val-options-local))

    ;; \setpygmentspygopt[<lexer>]{<pygments options>}
    '("setpygmentspygopt"
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Lexer")
		     (LaTeX-pythontex-language-list) ]
      (TeX-arg-key-val
       (("style") ("texcomments") ("mathescape"))))

    ;; \setpygmentsprettyprinter{<printer>}
    '("setpygmentsprettyprinter"
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Printer")
		    '("text" "bw" "fancyvrb" "pygments")))

    ;; 4.5  Advanced PythonTeX usage
    ;; \setpythontexcontext{<key-value pairs>}
    '("setpythontexcontext" t)

    ;; \restartpythontexsession{<counter value(s)>}
    '("restartpythontexsession" t)

    ;; \setpythontexoutputdir{<output directory>}
    '("setpythontexoutputdir" t)

    ;; \setpythontexworkingdir{<working directory>}
    '("setpythontexworkingdir" t)

    ;; 4.5  Advanced PythonTeX usage
    '("setpythontexcontext" t)
    '("restartpythontexsession" TeX-arg-counter)
    '("setpythontexoutputdir" t)
    '("setpythontexworkingdir" t) )

   ;; 4.4.1 Listings float
   ;; Only add it if not already defined somewhere else.
   (unless (assoc-string "listing" (LaTeX-environment-list))
     (LaTeX-add-newfloat-DeclareFloatingEnvironments
      '("listing" "verbatim")))

   ;; Cater for \setpythontexlistingenv:
   (TeX-add-symbols
    '("setpythontexlistingenv"
      (TeX-arg-eval
       (lambda ()
	 (let ((name (TeX-read-string
		      (TeX-argument-prompt optional nil "Listing environment name"))))
	   (LaTeX-add-newfloat-DeclareFloatingEnvironments `(,name "verbatim"))
	   (LaTeX-newfloat-auto-cleanup)
	   (format "%s" name))))))

   (LaTeX-add-environments
    ;; 4.2.5 Custom code
    '("pythontexcustomcode" LaTeX-env-args
      [ TeX-arg-eval completing-read
		     (TeX-argument-prompt optional nil "Position")
		     '("begin" "end") ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Family")
		    LaTeX-pythontex-family-list))

    ;; \begin{pygments}[<fancyvrb settings>]{<lexer>}
    '("pygments" LaTeX-env-args
      [ TeX-arg-eval LaTeX-fancyvrb-key-val-options-local ]
      (TeX-arg-eval completing-read
		    (TeX-argument-prompt optional nil "Lexer")
		    (LaTeX-pythontex-language-list))) )

   ;; Filling
   (add-to-list 'LaTeX-indent-environment-list
		'("pythontexcustomcode" current-indentation) t)
   (add-to-list 'LaTeX-indent-environment-list
		'("pygments" current-indentation) t)
   (add-to-list 'LaTeX-verbatim-environments-local "pythontexcustomcode")
   (add-to-list 'LaTeX-verbatim-environments-local "pygments")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-update-font-lock)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("pythontexcustomc"         "[{{")
				("setpythontexfv"           "[{")
				("setpythontexprettyprinter" "[{")
				("setpythontexpyglexer"     "[{")
				("setpythontexpygopt"       "[{")
				("printpythontex"           "[[")
				("stdoutpythontex"          "[[")
				("saveprintpythontex"       "{")
				("savestdoutpythontex"      "{")
				("useprintpythontex"        "[[{")
				("usestdoutpythontex"       "[[{")
				("stderrpythontex"          "[[")
				("savestderrpythontex"      "{")
				("usestderrpythontex"       "[[{")
				("setpythontexautoprint"    "{")
				("setpythontexautostdout"   "{")
				("inputpygments"            "[{{")
				("setpygmentsfv"            "[{")
				("setpygmentspygopt"        "[{")
				("setpygmentsprettyprinter" "{")
				("setpythontexcontext"      "{")
				("restartpythontexsession"  "{")
				("setpythontexoutputdir"    "{")
				("setpythontexworkingdir"   "{")
				("setpythontexlistingenv"   "{")
				("setpythontexcontext"      "{")
				("restartpythontexsession"  "{")
				("setpythontexoutputdir"    "{")
				("setpythontexworkingdir"   "{"))
			      'function)
     (font-latex-add-keywords '(("pygment" "{"))
			      'textual)
     (LaTeX-pythontex-add-syntactic-keywords-extra 'brace "pygment")
     (LaTeX-pythontex-add-syntactic-keywords-extra 'delim "pygment")
     ;; Tell font-lock about the update.
     (font-latex-update-font-lock t)))
 LaTeX-dialect)

;;; pythontex.el ends here
