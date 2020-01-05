;;; tuareg.el --- OCaml mode for Emacs.  -*- coding: utf-8; lexical-binding:t -*-

;; Copyright (C) 1997-2006 Albert Cohen, all rights reserved.
;; Copyright (C) 2009-2010 Jane Street Holding, LLC.
;; Licensed under the GNU General Public License.

;; Author: Albert Cohen <Albert.Cohen@inria.fr>
;;      Sam Steingold <sds@gnu.org>
;;      Christophe Troestler <Christophe.Troestler@umons.ac.be>
;;      Till Varoquaux <till@pps.jussieu.fr>
;;      Sean McLaughlin <seanmcl@gmail.com>
;;      Stefan Monnier <monnier@iro.umontreal.ca>
;; Created: 8 Jan 1997
;; Version: 2.3.0
;; Package-Requires: ((caml "3.12.0.1") (emacs "24.4"))
;; Keywords: ocaml languages
;; URL: https://github.com/ocaml/tuareg
;; EmacsWiki: TuaregMode

;;; Commentary:
;; Description:
;; Tuareg helps editing OCaml code, to highlight important parts of
;; the code, to run an OCaml REPL, and to run the OCaml debugger
;; within Emacs.

;; Installation:
;; If you have permissions to the local `site-lisp' directory, you
;; only have to copy `tuareg.el', `ocamldebug.el'
;; and `tuareg-site-file.el'.  Otherwise, copy the previous files
;; to a local directory and add the following line to your `.emacs':
;;
;; (add-to-list 'load-path "DIR")

;;; Usage:
;; Tuareg allows you to run batch OCaml compilations from Emacs (using
;; M-x compile) and browse the errors (C-x `). Typing C-x ` sets the
;; point at the beginning of the erroneous program fragment, and the
;; mark at the end.  Under Emacs, the program fragment is temporarily
;; hilighted.
;;
;; M-x tuareg-run-ocaml (or simply `run-ocaml') starts an OCaml
;; REPL (aka toplevel) with input and output in an Emacs buffer named
;; `*OCaml*.  This gives you the full power of Emacs to edit
;; the input to the OCaml REPL.  This mode is based on comint so
;; you get all the usual comint features, including command history. A
;; hook named `tuareg-interactive-mode-hook' may be used for
;; customization.
;;
;; Typing C-c C-e in a buffer in tuareg mode sends the current phrase
;; (containing the point) to the OCaml REPL, and evaluates it.  If
;; you type one of these commands before M-x tuareg-run-ocaml, the
;; REPL will be started automatically.
;;
;; M-x ocamldebug FILE starts the OCaml debugger ocamldebug on the
;; executable FILE, with input and output in an Emacs buffer named
;; *ocamldebug-FILE*.  It is similar to April 1996 version, with minor
;; changes to support XEmacs, Tuareg and OCaml. Furthermore, package
;; `thingatpt' is not required any more.

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'easymenu)
(require 'find-file)

(defconst tuareg-mode-revision
  (eval-when-compile
    (with-temp-buffer
      (if (file-directory-p ".git")
           (progn
             (insert "git: ")
             (call-process "git" nil t nil "log" "--pretty=%h" "-1")))
      (unless (zerop (buffer-size))
        (buffer-substring-no-properties
         (point-min) (1- (point-max))))))
  "Tuareg revision from the control system used.")

(defconst tuareg-mode-version
  (let ((version "Tuareg Version 2.2.0"))
    (if (null tuareg-mode-revision)
        version
      (concat version " (" tuareg-mode-revision ")")
      ))
  "         Copyright (C) 1997-2006 Albert Cohen, all rights reserved.
         Copyright (C) 2009-2010 Jane Street Holding, LLC.
         Copyright (C) 2011- Stefan Monnier & Christophe Troestler
         Copying is covered by the GNU General Public License.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Compatibility functions

(defun tuareg-editing-ls3 ()
  "Tell whether we are editing Lucid Synchrone syntax."
  (string-match-p "\\.ls\\'" (or buffer-file-name (buffer-name))))

(defun tuareg-editing-ocamllex ()
  "Tell whether we are editing OCamlLex syntax."
  (string-match-p "\\.mll\\'" (or buffer-file-name (buffer-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Import types and help features

(defvar tuareg-with-caml-mode-p
  (and (require 'caml-types nil t) (require 'caml-help nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       User customizable variables

(require 'smie)

;; Use the standard `customize' interface or `tuareg-mode-hook' to
;; Configure these variables

(require 'custom)

(defgroup tuareg nil
  "Support for the OCaml language."
  :link '(url-link "https://github.com/ocaml/tuareg")
  :group 'languages)

;; Indentation defaults

(defcustom tuareg-default-indent 2
  "*Default indentation.

Global indentation variable (large values may lead to indentation overflows).
When no governing keyword is found, this value is used to indent the line
if it has to."
  :group 'tuareg :type 'integer)

(defcustom tuareg-support-camllight nil
  "*If true, handle Caml Light character syntax (incompatible with labels)."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (when (boundp 'tuareg-mode-syntax-table)
           (modify-syntax-entry ?` (if val "\"" ".")
                                tuareg-mode-syntax-table))))

(defcustom tuareg-support-metaocaml nil
  "*If true, handle MetaOCaml syntax."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (ignore-errors
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when (derived-mode-p 'tuareg-mode 'tuareg-interactive-mode)
                 (tuareg--install-font-lock)))))))

(defcustom tuareg-in-indent 0 ; tuareg-default-indent
  "*How many spaces to indent from a `in' keyword.
Upstream <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
recommends 0, and this is what we default to since 2.0.1
instead of the historical `tuareg-default-indent'."
  :group 'tuareg :type 'integer)

(defcustom tuareg-with-indent 0
  "*How many spaces to indent from a `with' keyword.
The examples at <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
show the '|' is aligned with 'match', thus 0 is the default value."
  :group 'tuareg :type 'integer)

(defcustom tuareg-match-clause-indent 1
  "*How many spaces to indent a clause of match after a pattern `| ... ->'
or `... ->' (pattern without preceding `|' in the first clause of a matching).
To respect <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
the default is 1."
  :type 'integer)

(defcustom tuareg-match-when-indent (+ 4 tuareg-match-clause-indent)
  "*How many spaces from `|' to indent `when' in a pattern match
   | patt
        when cond ->
      clause"
  :type 'integer)

(defcustom tuareg-match-patterns-aligned nil
  "Non-nil means that the pipes for multiple patterns of a single case
are aligned instead of being slightly shifted to spot the multiple
patterns better.
         function          v.s.        function
         | A                           | A
           | B -> ...                  | B -> ...
         | C -> ...                    | C -> ... "
  :group 'tuareg :type 'boolean)

;; Tuareg-Interactive
;; Configure via `tuareg-mode-hook'

;; Automatic indentation

(make-obsolete-variable 'tuareg-use-abbrev-mode
                        "Use `electric-indent-mode' instead." "2.2.0")

(defcustom tuareg-electric-indent nil
  "Whether to automatically indent the line after typing one of
the words in `tuareg-electric-indent-keywords'.  Lines starting
with `|', `)', `]`, and `}' are always indented when the
`electric-indent-mode' is turned on."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-electric-close-vector t
  "*Non-nil means electrically insert `|' before a vector-closing `]' or
`>' before an object-closing `}'.

Many people find electric keys irritating, so you can disable them by
setting this variable to nil.  You should probably have this on,
though, if you also have `tuareg-electric-indent' on."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-highlight-all-operators nil
  "If t, highlight all operators (as opposed to unusual ones).
This is not turned on by default because this makes font-lock
much less efficient."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-other-file-alist
  '(("\\.mli\\'" (".ml" ".mll" ".mly"))
    ("\\.ml\\'" (".mli"))
    ("\\.mll\\'" (".mli"))
    ("\\.mly\\'" (".mli"))
    ("\\.eliomi\\'" (".eliom"))
    ("\\.eliom\\'" (".eliomi")))
  "Associative list of alternate extensions to find.
See `ff-other-file-alist'."
  :group 'tuareg
  :type '(repeat (list regexp (choice (repeat string) function))))


(defcustom tuareg-interactive-scroll-to-bottom-on-output nil
  "*Controls when to scroll to the bottom of the interactive buffer
upon evaluating an expression.

See `comint-scroll-to-bottom-on-output' for details."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (when (boundp 'comint-scroll-to-bottom-on-output)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when (derived-mode-p 'tuareg-interactive-mode)
                 (setq-local comint-scroll-to-bottom-on-output val)))))))

(defcustom tuareg-skip-after-eval-phrase t
  "*Non-nil means skip to the end of the phrase after evaluation in the
OCaml REPL."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-read-only-input nil
  "*Non-nil means input sent to the OCaml REPL is read-only."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-echo-phrase t
  "*Non-nil means echo phrases in the REPL buffer when sending
them to the OCaml REPL."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-input-font-lock t
  "*Non nil means Font-Lock for REPL input phrases."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-output-font-lock t
  "*Non nil means Font-Lock for REPL output messages."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-error-font-lock t
  "*Non nil means Font-Lock for REPL error messages."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-display-buffer-on-eval t
  "*Non nil means pop up the OCaml REPL when evaluating code."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-manual-url
  "http://caml.inria.fr/pub/docs/manual-ocaml/"
  "*URL to the OCaml reference manual."
  :group 'tuareg :type 'string)

(defcustom tuareg-browser 'browse-url
  "*Name of function that displays the OCaml reference manual.
Valid names are `browse-url', `browse-url-firefox', etc."
  :group 'tuareg :type 'function)

(defcustom tuareg-library-path "/usr/local/lib/ocaml/"
  "*Path to the OCaml library."
  :group 'tuareg :type 'string)

(defvar tuareg-options-list
  `(["Prettify symbols" prettify-symbols-mode
      :style toggle :selected prettify-symbols-mode :active t])
  "*List of menu-configurable Tuareg options.")

(defvar tuareg-interactive-options-list
  '(("Skip phrase after evaluation" . 'tuareg-skip-after-eval-phrase)
    ("Echo phrase in interactive buffer" . 'tuareg-interactive-echo-phrase)
    "---"
    ("Font-lock interactive input" . 'tuareg-interactive-input-font-lock)
    ("Font-lock interactive output" . 'tuareg-interactive-output-font-lock)
    ("Font-lock interactive error" . 'tuareg-interactive-error-font-lock)
    "---"
    ("Read only input" . 'tuareg-interactive-read-only-input))
  "*List of menu-configurable Tuareg options.")

(defvar tuareg-interactive-program "ocaml -nopromptcont"
  "*Default program name for invoking an OCaml REPL (aka toplevel) from Emacs.")
;; Could be interesting to have this variable buffer-local
;;   (e.g., ocaml vs. metaocaml buffers)
;; (make-variable-buffer-local 'tuareg-interactive-program)

(defgroup tuareg-faces nil
  "Special faces for the Tuareg mode."
  :group 'tuareg)

(defconst tuareg-faces-inherit-p
  (and (boundp 'face-attribute-name-alist)
       (assq :inherit face-attribute-name-alist)))

(defface tuareg-font-lock-governing-face
  '((((class color) (type tty)) (:bold t))
    (((background light)) (:foreground "black" :bold t))
    (t (:foreground "wheat" :bold t)))
  "Face description for governing/leading keywords."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-governing-face
  'tuareg-font-lock-governing-face)

(defface tuareg-font-lock-multistage-face
  '((((background light))
     (:foreground "darkblue" :background "lightgray" :bold t))
    (t (:foreground "steelblue" :background "darkgray" :bold t)))
  "Face description for MetaOCaml staging operators."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-multistage-face
  'tuareg-font-lock-multistage-face)

(defface tuareg-font-lock-line-number-face
  '((((background light)) (:foreground "dark gray"))
    (t (:foreground "gray60")))
  "Face description for line numbering directives."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-line-number-face
  'tuareg-font-lock-line-number-face)

(defface tuareg-font-lock-operator-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face description for all operators."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-operator-face
  'tuareg-font-lock-operator-face)

(defface tuareg-font-lock-module-face
  '((t (:inherit font-lock-type-face))); backward compatibility
  "Face description for modules and module paths."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-module-face
  'tuareg-font-lock-module-face)

(defface tuareg-font-lock-constructor-face
  '((t (:inherit default)))
  "Face description for constructors of (polymorphic) variants and exceptions."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-constructor-face
  'tuareg-font-lock-constructor-face)

(defface tuareg-font-lock-label-face
  '((t (:inherit font-lock-constant-face keep)))
  "Face description for labels."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-label-face
  'tuareg-font-lock-label-face)

(defface tuareg-font-double-semicolon-face
  '((t (:foreground "OrangeRed")))
  "Face description for ;; which is not needed in standard code."
  :group 'tuareg-faces)
(defvar tuareg-font-double-semicolon-face
  'tuareg-font-double-semicolon-face)

(defface tuareg-font-lock-error-face
  '((t (:foreground "yellow" :background "red" :bold t)))
  "Face description for all errors reported to the source."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-error-face
  'tuareg-font-lock-error-face)

(defface tuareg-font-lock-interactive-output-face
  '((((background light))
     (:foreground "blue4"))
    (t (:foreground "grey")))
  "Face description for all outputs in the REPL."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-interactive-output-face
  'tuareg-font-lock-interactive-output-face)

(defface tuareg-font-lock-interactive-error-face
  (if tuareg-faces-inherit-p
      '((t :inherit font-lock-warning-face))
    '((((background light)) (:foreground "red3"))
      (t (:foreground "red2"))))
  "Face description for all REPL errors."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-interactive-error-face
  'tuareg-font-lock-interactive-error-face)

(defface tuareg-font-lock-interactive-directive-face
  '((((background light)) (:foreground "slate gray"))
    (t (:foreground "light slate gray")))
  "Face description for all REPL directives such as #load."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-interactive-directive-face
  'tuareg-font-lock-interactive-directive-face)

(defface tuareg-font-lock-attribute-face
  (if tuareg-faces-inherit-p
      '((t :inherit font-lock-preprocessor-face))
    '((((background light)) (:foreground "DodgerBlue2"))
      (t (:foreground "LightSteelBlue"))))
  "Face description for OCaml attribute annotations."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-attribute-face
  'tuareg-font-lock-attribute-face)

(defface tuareg-font-lock-infix-extension-node-face
  (if tuareg-faces-inherit-p
      '((t :inherit font-lock-preprocessor-face))
    '((((background light)) (:foreground "Orchid"))
      (((background dark)) (:foreground "LightSteelBlue"))
      (t (:foreground "LightSteelBlue"))))
  "Face description for OCaml the infix extension node."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-infix-extension-node-face
  'tuareg-font-lock-infix-extension-node-face)

(defface tuareg-font-lock-extension-node-face
  (if tuareg-faces-inherit-p
      '((t :inherit tuareg-font-lock-infix-extension-node-face
           :background "gray92"))
    '((((background light)) (:foreground "Orchid" :background "gray92"))
      (((background dark)) (:foreground "LightSteelBlue" :background "gray92"))
      (t (:foreground "LightSteelBlue"))))
  "Face description for OCaml extension nodes."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-extension-node-face
  'tuareg-font-lock-extension-node-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Support definitions

;; This function is different from the standard in that it does NOT signal
;; errors at beginning-of-buffer.
(defun tuareg-backward-char (&optional step)
  (if step (goto-char (- (point) step))
    (goto-char (1- (point)))))

(defun tuareg-in-indentation-p ()
  "Return non-nil if all chars between beginning of line and point are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun tuareg-in-literal-or-comment-p (&optional pos)
  "Return non-nil if point is inside an OCaml literal or comment."
  (nth 8 (syntax-ppss pos)))

(defun tuareg-backward-up-list ()
  ;; FIXME: not clear if moving out of a string/comment should count as 1 or no.
  (condition-case nil
      (backward-up-list)
    (scan-error (goto-char (point-min)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Font-lock in Emacs

;; Originally by Stefan Monnier

(defcustom tuareg-font-lock-symbols nil
  "*Display fun and -> and such using symbols in fonts.
This may sound like a neat trick, but note that it can change the
alignment and can thus lead to surprises.  On recent Emacs >= 24.4,
use `prettify-symbols-mode'."
  :group 'tuareg :type 'boolean)

(when (fboundp 'prettify-symbols-mode)
  (make-obsolete-variable 'tuareg-font-lock-symbols
                          'prettify-symbols-mode "Emacs-24.4"))

(defcustom tuareg-prettify-symbols-full nil
  "If non-nil, add fun and -> and such to be prettified with symbols.
This may sound like a neat trick, but note that it can change the
alignment and can thus lead to surprises.  By default, only symbols that
do not perturb in essential ways the alignment are used.  See
`tuareg-prettify-symbols-basic-alist' and
`tuareg-prettify-symbols-extra-alist'."
  :group 'tuareg :type 'boolean)

(defvar tuareg-prettify-symbols-basic-alist
  `(("sqrt" . ?√)
    ("&&" . ?∧)        ; 'LOGICAL AND' (U+2227)
    ("||" . ?∨)        ; 'LOGICAL OR' (U+2228)
    ("+." . ?∔)        ;DOT PLUS (U+2214)
    ("-." . ?∸)        ;DOT MINUS (U+2238)
    ;;("*." . ?×)
    ("*." . ?∙)   ; BULLET OPERATOR
    ("/." . ?÷)
    ("<-" . ?←)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("<>" . ?≠)
    ("==" . ?≡)
    ("!=" . ?≢)
    ("<=>" . ?⇔)
    ("infinity" . ?∞)
    ;; Some greek letters for type parameters.
    ("'a" . ?α)
    ("'b" . ?β)
    ("'c" . ?γ)
    ("'d" . ?δ)
    ("'e" . ?ε)
    ("'f" . ?φ)
    ("'i" . ?ι)
    ("'k" . ?κ)
    ("'m" . ?μ)
    ("'n" . ?ν)
    ("'o" . ?ω)
    ("'p" . ?π)
    ("'r" . ?ρ)
    ("'s" . ?σ)
    ("'t" . ?τ)
    ("'x" . ?ξ)))

(defvar tuareg-prettify-symbols-extra-alist
  `(("fun" . ?λ)
    ("not" . ?¬)
    ;;("or" . ?∨); should not be used as ||
    ("[|" . ?〚)        ;; 〚
    ("|]" . ?〛)        ;; 〛
    ("->" . ?→)
    (":=" . ?⇐)
    ("::" . ?∷)))

(defun tuareg--prettify-symbols-compose-p (start end match)
  "Return true iff the symbol MATCH should be composed.
See `prettify-symbols-compose-predicate'."
  ;; Refine `prettify-symbols-default-compose-p' so as not to compose
  ;; symbols for errors,...
  (and (prettify-symbols-default-compose-p start end match)
       (not (memq (get-text-property start 'face)
                  '(tuareg-font-lock-error-face
                    tuareg-font-lock-interactive-output-face
                    tuareg-font-lock-interactive-error-face)))))

(defun tuareg-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((mbegin (match-beginning 0))
         (mend (match-end 0))
         (syntax (char-syntax (char-after mbegin))))
    (if (or (eq (char-syntax (or (char-before mbegin) ?\ )) syntax)
            (eq (char-syntax (or (char-after mend) ?\ )) syntax)
            (memq (get-text-property mbegin 'face)
                  '(tuareg-doc-face
                    font-lock-string-face
                    font-lock-comment-face
                    tuareg-font-lock-error-face
                    tuareg-font-lock-interactive-output-face
                    tuareg-font-lock-interactive-error-face)))
        ;; No composition for you. Let's actually remove any composition
        ;;   we may have added earlier and which is now incorrect.
        (remove-text-properties mbegin mend '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region mbegin mend (cdr (assoc (match-string 0) alist)))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun tuareg-font-lock-symbols-keywords ()
  (let ((alist (if tuareg-prettify-symbols-full
                   (append tuareg-prettify-symbols-basic-alist
                           tuareg-prettify-symbols-extra-alist)
                 tuareg-prettify-symbols-basic-alist)))
    (dolist (x alist)
      (when (and (if (fboundp 'char-displayable-p)
                     (char-displayable-p (cdr x))
                   t)
                 (not (assoc (car x) alist))) ; not yet in alist.
        (push x alist)))
    (when alist
      `((,(regexp-opt (mapcar #'car alist) t)
         (0 (tuareg-font-lock-compose-symbol ',alist)))))))

(defvar tuareg-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?. "'" st)     ;Make qualified names a single symbol.
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?? ". p" st)
    (modify-syntax-entry ?~ ". p" st)
    ;; See http://caml.inria.fr/pub/docs/manual-ocaml/lex.html.
    (dolist (c '(?! ?$ ?% ?& ?+ ?- ?/ ?: ?< ?= ?> ?@ ?^ ?|))
      (modify-syntax-entry c "." st))
    (modify-syntax-entry ?' "_" st) ; ' is part of symbols (for primes).
    (modify-syntax-entry
     ;; ` is punctuation or character delimiter (Caml Light compatibility).
     ?` (if tuareg-support-camllight "\"" ".") st)
    (modify-syntax-entry ?\" "\"" st) ; " is a string delimiter
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?*  ". 23" st)
    (condition-case nil
        (progn
          (modify-syntax-entry ?\( "()1n" st)
          (modify-syntax-entry ?\) ")(4n" st))
      (error               ;XEmacs signals an error instead of ignoring `n'.
       (modify-syntax-entry ?\( "()1" st)
       (modify-syntax-entry ?\) ")(4" st)))
    st)
  "Syntax table in use in Tuareg mode buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Font-Lock

(defvar tuareg-doc-face 'font-lock-doc-face)

(defconst tuareg-font-lock-syntactic-keywords
  ;; Char constants start with ' but ' can also appear in identifiers.
  ;; Beware not to match things like '*)hel' or '"hel' since the first '
  ;; might be inside a string or comment.
  ;; Note: for compatibility with Emacs<23, we use "\\<" rather than "\\_<",
  ;; which depends on tuareg-font-lock-syntax turning all "_" into "w".
  '(("\\<\\('\\)\\([^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
     (1 '(7)) (3 '(7)))))

(defun tuareg-syntax-propertize (start end)
  (goto-char start)
  (tuareg--syntax-quotation end)
  (funcall
   (syntax-propertize-rules
    ;; When we see a '"', knowing whether it's a literal char (as opposed to
    ;; the end of a string followed by the beginning of a literal char)
    ;; requires checking syntax-ppss as in:
    ;; ("\\_<\\('\"'\\)"
    ;;  (1 (unless (nth 3 (save-excursion (syntax-ppss (match-beginning 0))))
    ;;       (string-to-syntax "\""))))
    ;; Not sure if it's worth the trouble since adding a space between the
    ;; string and the literal char is easy enough and is the usual
    ;; style anyway.
    ;; For all other cases we don't need to check syntax-ppss because, if the
    ;; first quote is within a string (or comment), the whole match is within
    ;; the string (or comment), so the syntax-properties don't hurt.
    ;;
    ;; Note: we can't just use "\\<" here because syntax-propertize is also
    ;; used outside of font-lock.
    ("\\_<\\('\\)\\(?:[^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
     (1 "\"") (2 "\""))
    ("\\({\\)[a-z_]*|"
     (1 (prog1 "|" (tuareg--syntax-quotation end))))
    )
   (point) end))

(defun tuareg--syntax-quotation (end)
  (let ((ppss (syntax-ppss)))
    (when (eq t (nth 3 ppss))
      (pcase (char-after (nth 8 ppss))
        (`?<
         ;; We're indeed inside a quotation.
         (when (re-search-forward ">>" end 'move)
           (put-text-property (1- (point)) (point)
                              'syntax-table (string-to-syntax "|"))))
        (`?\{
         ;; We're inside a quoted string
         ;; http://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec244
         (let ((id (save-excursion
                     (goto-char (1+ (nth 8 ppss)))
                     (buffer-substring (point)
                                       (progn (skip-chars-forward "a-z_")
                                              (point))))))
	   (when (search-forward (concat "|" id "}") end 'move)
             (put-text-property (1- (point)) (point)
                                'syntax-table (string-to-syntax "|")))))
        (c (error "Unexpected char '%c' starting delimited string" c))))))

(defun tuareg-font-lock-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Tuareg."
  (if (nth 3 state)
      font-lock-string-face
    (let ((start (nth 8 state)))
      (if (and (> (point-max) (+ start 2))
               (eq (char-after (+ start 2)) ?*)
               (not (eq (char-after (+ start 3)) ?*)))
          ;; This is a documentation comment
          tuareg-doc-face
        font-lock-comment-face))))

;; Initially empty, set in `tuareg--install-font-lock-1'
(defvar tuareg-font-lock-keywords ()
  "Font-Lock patterns for Tuareg mode (basic level).")

(defvar tuareg-font-lock-keywords-1 ()
  "Font-Lock patterns for Tuareg mode (intermediate level).")

(defvar tuareg-font-lock-keywords-2 ()
  "Font-Lock patterns for Tuareg mode (maximum level).")

(defconst tuareg-font-lock-syntax
  ;; Note: as a general rule, changing syntax-table during font-lock
  ;; is a potential problem for syntax-ppss.
  `((?_ . "w") (?' . "w"))
  "Syntax changes for Font-Lock.")

(defconst tuareg--whitespace-re
  ;; QUESTION: Why not just "[ \t\n]*"?
  ;; It used to be " *[\t\n]? *" but this is inefficient since it can match
  ;; N spaces in N+1 different ways :-(
  " *\\(?:[\t\n] *\\)?")

(defconst tuareg--id-re "\\_<[A-Za-z_][A-Za-z0-9_']*\\_>"
  "Regular expression for identifiers.")
(defconst tuareg--lid-re "\\_<[a-z_][A-Za-z0-9_']*\\_>"
  "Regular expression for variable names.")
(defconst tuareg--uid-re "\\_<[A-Z][A-Za-z0-9_']*\\_>"
  "Regular expression for module and constructor names.")

(defun tuareg--install-font-lock (&optional interactive-p)
  "Setup `font-lock-defaults'.  INTERACTIVE-P says whether it is
for the interactive mode."
  (let* ((id tuareg--id-re)
         (lid tuareg--lid-re)
         (uid tuareg--uid-re)
	 (attr-id1 "\\<[A-Za-z_][A-Za-z0-9_']*\\>")
	 (attr-id (concat attr-id1 "\\(?:\\." attr-id1 "\\)*"))
	 (maybe-infix-extension (concat "\\(?:%" attr-id "\\)?")); at most 1
         ;; Matches braces balanced on max 3 levels.
         (balanced-braces
          (let ((b "\\(?:[^()]\\|(")
                (e ")\\)*"))
            (concat b b b "[^()]*" e e e)))
         (balanced-braces-no-string
          (let ((b "\\(?:[^()\"]\\|(")
                (e ")\\)*"))
            (concat b b b "[^()\"]*" e e e)))
         (balanced-braces-no-end-operator ; non-empty
          (let* ((b "\\(?:[^()]\\|(")
                 (e ")\\)*")
                 (braces (concat b b "[^()]*" e e))
                 (end-op (concat "\\(?:[^()!$%&*+-./:<=>?@^|~]\\|("
                                 braces ")\\)")))
            (concat "\\(?:[^()!$%&*+-./:<=>?@^|~]"
                    ;; Operator not starting with ~
                    "\\|[!$%&*+-./:<=>?@^|][!$%&*+-./:<=>?@^|~]*" end-op
                    ;; Operator or label starting with ~
                    "\\|~\\(?:[!$%&*+-./:<=>?@^|~]+" end-op
                    "\\|[a-z][a-zA-Z0-9]*[: ]\\)"
                    "\\|(" braces e)))
	 (balanced-brackets
          (let ((b "\\(?:[^][]\\|\\[")
                (e "\\]\\)*"))
            (concat b b b "[^][]*" e e e)))
	 (maybe-infix-attribute
	  (concat "\\(?:\\[@" attr-id balanced-brackets "\\]\\)*"))
	 (maybe-infix-ext+attr
	  (concat maybe-infix-extension maybe-infix-attribute))
	 ;; FIXME: module paths with functor applications
         (module-path (concat uid "\\(?:\\." uid "\\)*"))
         (typeconstr (concat "\\(?:" module-path "\\.\\)?" lid))
         (extended-module-name
          (concat uid "\\(?: *([ A-Z]" balanced-braces ")\\)*"))
         (extended-module-path
          (concat extended-module-name
                  "\\(?: *\\. *" extended-module-name "\\)*"))
         (modtype-path (concat "\\(?:" extended-module-path "\\.\\)*" id))
         (typevar "'[A-Za-z_][A-Za-z0-9_']*\\>")
         (typeparam (concat "\\(?:[+-]?" typevar "\\|_\\)"))
         (typeparams (concat "\\(?:" typeparam "\\|( *"
                             typeparam " *\\(?:, *" typeparam " *\\)*)\\)"))
         (typedef (concat "\\(?:" typeparams " *\\)?" lid))
         ;; Define 2 groups: possible path, variables
         (let-ls3 (regexp-opt '("clock" "node" "static"
                                "present" "automaton" "where" "match"
                                "with" "do" "done" "unless" "until"
                                "reset" "every")))
         (before-operator-char "[^-!$%&*+./:<=>?@^|~#?]")
         (operator-char         "[-!$%&*+./:<=>?@^|~]")
         (operator-char-no>     "[-!$%&*+./:<=?@^|~]"); for "->"
         (binding-operator-char
          (concat "\\(?:[-$&*+/<=>@^|]" operator-char "*\\)"))
         (let-binding-g4 ; 4 groups
          (concat "\\_<\\(?:\\(let\\_>" binding-operator-char "?\\)"
                  "\\(" maybe-infix-ext+attr
		  "\\)\\(?: +\\(" (if (tuareg-editing-ls3) let-ls3 "rec\\_>")
		  "\\)\\)?\\|\\(and\\_>" binding-operator-char "?\\)\\)"))
         ;; group for possible class param
         (gclass-gparams
          (concat "\\(\\_<class\\(?: +type\\)?\\(?: +virtual\\)?\\_>\\)"
                  " *\\(\\[ *" typevar " *\\(?:, *" typevar " *\\)*\\] *\\)?"))
         ;; font-lock rules common to all levels
         (common-keywords
          `(("^#[0-9]+ *\\(?:\"[^\"]+\"\\)?"
             0 tuareg-font-lock-line-number-face t)
            ;; cppo
            (,(concat "^ *#"
                      (regexp-opt '("define" "undef" "if" "ifdef" "ifndef"
				    "else" "elif" "endif" "include"
				    "warning" "error" "ext" "endext")
				  'symbols))
             . font-lock-preprocessor-face)
            ;; Directives
            ,@(if interactive-p
                  `((,(concat "^# +\\(#" lid "\\)")
                     1 tuareg-font-lock-interactive-directive-face)
                    (,(concat "^ *\\(#" lid "\\)")
                     1 tuareg-font-lock-interactive-directive-face))
                `((,(concat "^\\(#" lid "\\)")
                   . tuareg-font-lock-interactive-directive-face)))
            (,(concat (if interactive-p "^ *#\\(?: +#\\)?" "^#")
                      "show\\(?:_module\\)? +\\(" uid "\\)")
             1 tuareg-font-lock-module-face)
            (";;+" 0 tuareg-font-double-semicolon-face)
            ;; Attributes (`keep' to highlight except strings & chars)
            (,(concat "\\[@\\(?:@@?\\)?" attr-id balanced-brackets "\\]")
             0 tuareg-font-lock-attribute-face keep)
            ;; Extension nodes.
            (,(concat "\\(\\[%%?" attr-id "\\)" balanced-brackets "\\(\\]\\)")
             (1 tuareg-font-lock-extension-node-face)
             (2 tuareg-font-lock-extension-node-face))
            (,(concat "[^;];\\(" maybe-infix-extension "\\)")
             1 tuareg-font-lock-infix-extension-node-face)
            (,(concat "\\_<\\(function\\)\\_>\\(" maybe-infix-ext+attr "\\)"
	              tuareg--whitespace-re "\\(" lid "\\)?")
             (1 font-lock-keyword-face)
             (2 tuareg-font-lock-infix-extension-node-face keep)
             (3 font-lock-variable-name-face nil t))
            (,(concat "\\_<\\(fun\\|match\\)\\_>\\(" maybe-infix-ext+attr "\\)")
             (1 font-lock-keyword-face)
             (2 tuareg-font-lock-infix-extension-node-face keep))
            ;; "type" to introduce a local abstract type considered a keyword
            (,(concat "( *\\(type\\) +\\(" lid " *\\)+)")
             (1 font-lock-keyword-face)
             (2 font-lock-type-face))
            (":[\n]? *\\(\\<type\\>\\)"
             (1 font-lock-keyword-face))
            ;; (lid: t), before function definitions
            (,(concat "(" lid " *:\\(['_A-Za-z]"
                      balanced-braces-no-string "\\))")
             1 font-lock-type-face keep)
            ;; "module type of" module-expr (here "of" is a governing
            ;; keyword).  Must be before the modules highlighting.
            (,(concat "\\<\\(module +type +of\\)\\>\\(?: +\\("
                      module-path "\\)\\)?")
             (1 tuareg-font-lock-governing-face keep)
             (2 tuareg-font-lock-module-face keep t))
            ;; First class modules.  In these contexts, "val" and "module"
            ;; are not considered as "governing" (main structure of the code).
            (,(concat "( *\\(module\\) +\\(" module-path "\\) *\\(?:: *\\("
	              balanced-braces-no-string "\\)\\)?)")
             (1 font-lock-keyword-face)
             (2 tuareg-font-lock-module-face)
             (3 tuareg-font-lock-module-face keep t))
            (,(concat "( *\\(val\\) +\\("
                      balanced-braces-no-end-operator "\\): +\\("
	              balanced-braces-no-string "\\))")
             (1 font-lock-keyword-face)
             (2 tuareg-font-lock-module-face)
             (3 tuareg-font-lock-module-face))
            (,(concat "\\_<\\(module\\)\\(" maybe-infix-ext+attr "\\)"
	              "\\(\\(?: +type\\)?\\(?: +rec\\)?\\)\\>\\(?: *\\("
                      uid "\\)\\)?")
             (1 tuareg-font-lock-governing-face)
             (2 tuareg-font-lock-infix-extension-node-face)
             (3 tuareg-font-lock-governing-face)
             (4 tuareg-font-lock-module-face keep t))
            ("\\_<let +exception\\_>" . tuareg-font-lock-governing-face)
            (,(concat (regexp-opt '("sig" "struct" "functor" "inherit"
                                    "initializer" "object" "begin")
                                  'symbols)
                      "\\(" maybe-infix-ext+attr "\\)")
             (1 tuareg-font-lock-governing-face)
             (2 tuareg-font-lock-infix-extension-node-face keep))
            (,(regexp-opt '("constraint" "in" "end") 'symbols)
             . tuareg-font-lock-governing-face)
            ,@(if (tuareg-editing-ls3)
                  `((,(concat "\\<\\(let[ \t]+" let-ls3 "\\)\\>")
                     . tuareg-font-lock-governing-face)))
            ;; "with type": "with" treated as a governing keyword
            (,(concat "\\<\\(\\(?:with\\|and\\) +type\\(?: +nonrec\\)?\\_>\\) *"
                      "\\(" typeconstr "\\)?")
             (1 tuareg-font-lock-governing-face keep)
             (2 font-lock-type-face keep t))
            (,(concat "\\<\\(\\(?:with\\|and\\) +module\\>\\) *\\(?:\\("
                      module-path "\\) *\\)?\\(?:= *\\("
                      extended-module-path "\\)\\)?")
             (1 tuareg-font-lock-governing-face keep)
             (2 tuareg-font-lock-module-face keep t)
             (3 tuareg-font-lock-module-face keep t))
            ;; "!", "mutable", "virtual" treated as governing keywords
            (,(concat "\\<\\(\\(?:val\\(" maybe-infix-ext+attr "\\)"
	              (if (tuareg-editing-ls3) "\\|reset\\|do")
                      "\\)!? +\\(?:mutable\\(?: +virtual\\)?\\_>"
                      "\\|virtual\\(?: +mutable\\)?\\_>\\)"
	              "\\|val!\\(" maybe-infix-ext+attr "\\)\\)"
                      "\\(?: *\\(" lid "\\)\\)?")
             (2 tuareg-font-lock-infix-extension-node-face keep t)
             (3 tuareg-font-lock-infix-extension-node-face keep t)
             (1 tuareg-font-lock-governing-face keep t)
             (4 font-lock-variable-name-face nil t))
            ;; "val" without "!", "mutable" or "virtual"
            (,(concat "\\_<\\(val\\)\\_>\\(" maybe-infix-ext+attr "\\)"
	              "\\(?: +\\(" lid "\\)\\)?")
             (1 tuareg-font-lock-governing-face keep)
             (2 tuareg-font-lock-infix-extension-node-face keep)
             (3 font-lock-function-name-face keep t))
            ;; "private" treated as governing keyword
            (,(concat "\\(\\<method!?\\(?: +\\(?:private\\(?: +virtual\\)?"
                      "\\|virtual\\(?: +private\\)?\\)\\)?\\>\\)"
                      " *\\(" lid "\\)?")
             (1 tuareg-font-lock-governing-face keep t)
             (2 font-lock-function-name-face keep t)); method name
            (,(concat "\\<\\(open\\(?:! +\\|\\> *\\)\\)\\(" module-path "\\)?")
             (1 tuareg-font-lock-governing-face)
             (2 tuareg-font-lock-module-face keep t))
            ;; (expr: t) and (expr :> t) If `t' is longer then one
            ;; word, require a space before.  Not only this is more
            ;; readable but it also avoids that `~label:expr var` is
            ;; taken as a type annotation when surrounded by
            ;; parentheses.  Done last so that it does not apply if
            ;; already highlighted (let x : t = u in ...) but before
            ;; module paths (expr : X.t).
            (,(concat "(" balanced-braces-no-end-operator ":>? *\\(?:\n *\\)?"
                      "\\(['_A-Za-z]" balanced-braces-no-string
                      "\\|(" balanced-braces-no-string ")"
                      balanced-braces-no-string"\\))")
             1 font-lock-type-face)
            ;; module paths A.B.
            (,(concat module-path "\\.") . tuareg-font-lock-module-face)
            ,@(and tuareg-support-metaocaml
                   '(("[^-@^!*=<>&/%+~?#]\\(\\(?:\\.<\\|\\.~\\|!\\.\\|>\\.\\)+\\)"
                      1 tuareg-font-lock-multistage-face)))
            ;; External function declaration
            (,(concat "\\_<\\(external\\)\\_>\\(?: +\\(" lid "\\)\\)?")
             (1 tuareg-font-lock-governing-face)
             (2 font-lock-function-name-face keep t))
            ;; Binding operators
            (,(concat "( *\\(\\(?:let\\|and\\)\\_>"
                      binding-operator-char "\\) *)")
             1 font-lock-function-name-face)
            ;; Highlight "let" and function names (their argument
            ;; patterns can then be treated uniformly with variable bindings)
            (,(concat let-binding-g4 " *\\(?:\\(" lid "\\) *"
                      "\\(?:[^ =,:a]\\|a\\(?:[^s]\\|s[^[:space:]]\\)\\)\\)?")
             (1 tuareg-font-lock-governing-face keep t)
             (2 tuareg-font-lock-infix-extension-node-face keep t)
             (3 tuareg-font-lock-governing-face keep t)
             (4 tuareg-font-lock-governing-face keep t)
             (5 font-lock-function-name-face keep t))
            (,(concat "\\_<\\(include\\)\\_>\\(?: +\\("
                      extended-module-path "\\|( *"
                      extended-module-path " *: *" balanced-braces " *)\\)\\)?")
             (1 tuareg-font-lock-governing-face)
             (2 tuareg-font-lock-module-face keep t))
            ;; module type A = B
            (,(concat "\\_<\\(module +type\\)\\_>\\(?: +" id
                      " *= *\\(" modtype-path "\\)\\)?")
             (1 tuareg-font-lock-governing-face)
             (2 tuareg-font-lock-module-face keep t))
            ;; "class [params] name"
            (,(concat gclass-gparams "\\(" lid "\\)?")
             (1 tuareg-font-lock-governing-face keep)
             (2 font-lock-type-face keep t)
             (3 font-lock-function-name-face keep t))
            ;; "type lid" anywhere (e.g. "let f (type t) x =")
            ;; introduces a new type
            (,(concat "\\_<\\(type\\_>\\)\\(" maybe-infix-ext+attr
                      "\\)\\(?: +\\(nonrec\\_>\\)\\)?\\(?:"
                      tuareg--whitespace-re
                      "\\(" typedef "\\)\\)?")
             (1 tuareg-font-lock-governing-face)
             (2 tuareg-font-lock-infix-extension-node-face keep)
             (3 tuareg-font-lock-governing-face keep t)
             (4 font-lock-type-face keep t))))
         tuareg-font-lock-keywords-1-extra)
    (setq
     tuareg-font-lock-keywords
     (append
      common-keywords
      `(;; Basic way of matching functions
        (,(concat let-binding-g4 " *\\("
                  lid "\\) *= *\\(fun\\(?:ction\\)?\\)\\>")
         (5 font-lock-function-name-face)
         (6 font-lock-keyword-face))
        )))
    (setq
     tuareg-font-lock-keywords-1-extra
     `((,(regexp-opt '("true" "false" "__LOC__" "__FILE__" "__LINE__"
                       "__MODULE__" "__POS__" "__LOC_OF__" "__LINE_OF__"
                       "__POS_OF__")
                     'symbols)
        . font-lock-constant-face)
       (,(let ((kwd '("as" "do" "done" "downto" "else" "for" "if"
                      "then" "to" "try" "when" "while" "new"
                      "lazy" "assert" "exception")))
           (if (tuareg-editing-ls3)
               (progn (push "reset" kwd)  (push "merge" kwd)
                      (push "emit" kwd)  (push "period" kwd)))
           (regexp-opt kwd 'symbols))
        . font-lock-keyword-face)
       (,(concat "\\_<exception +\\(" uid "\\)")
        1 tuareg-font-lock-constructor-face)
       ;; (M: S) -- only color S here (may be "A.T with type t = s")
       (,(concat "( *" uid " *: *\\("
                 modtype-path "\\(?: *\\_<with\\_>"
                 balanced-braces "\\)?\\) *)")
        1 tuareg-font-lock-module-face keep)
       ;; module A(B: _)(C: _) : D = E, including "module A : E"
       (,(concat "\\_<module +" uid tuareg--whitespace-re
                 "\\(\\(?:( *" uid " *: *"
                 modtype-path "\\(?: *\\_<with\\_>" balanced-braces "\\)?"
                 " *)" tuareg--whitespace-re "\\)*\\)\\(?::"
                 tuareg--whitespace-re "\\(" modtype-path
                 "\\) *\\)?\\(?:=" tuareg--whitespace-re
                 "\\(" extended-module-path "\\)\\)?")
        (1 font-lock-variable-name-face keep); functor (module) variable
        (2 tuareg-font-lock-module-face keep t)
        (3 tuareg-font-lock-module-face keep t))
       (,(concat "\\_<functor\\> *( *\\(" uid "\\) *: *\\("
                 modtype-path "\\) *)")
        (1 font-lock-variable-name-face keep); functor (module) variable
        (2 tuareg-font-lock-module-face keep))
       ;; Other uses of "with", "mutable", "private", "virtual"
       (,(regexp-opt '("of" "with" "mutable" "private" "virtual") 'symbols)
        . font-lock-keyword-face)
       ;; labels
       (,(concat "\\([?~]" lid "\\)" tuareg--whitespace-re ":[^:>=]")
        1 tuareg-font-lock-label-face keep)
       ;; label in a type signature
       (,(concat "\\(?:->\\|:[^:>=]\\)" tuareg--whitespace-re
                 "\\(" lid "\\)[ \t]*:[^:>=]")
        1 tuareg-font-lock-label-face keep)
       ;; Polymorphic variants (take precedence on builtin names)
       (,(concat "`" id) . tuareg-font-lock-constructor-face)
       (,(regexp-opt '("failwith" "failwithf" "exit" "at_exit" "invalid_arg"
                       "parser" "raise" "raise_notrace" "ref" "ignore"
		       "Match_failure" "Assert_failure" "Invalid_argument"
		       "Failure" "Not_found" "Out_of_memory" "Stack_overflow"
		       "Sys_error" "End_of_file" "Division_by_zero"
		       "Sys_blocked_io" "Undefined_recursive_module")
                     'symbols)
        . font-lock-builtin-face)
       ("\\[[ \t]*\\]" . tuareg-font-lock-constructor-face) ; []
       ("[])a-zA-Z0-9 \t]\\(::\\)[[(a-zA-Z0-9 \t]" ; :: (not not ::…)
        1 tuareg-font-lock-constructor-face)
       ;; Constructors
       (,(concat "\\(" uid "\\)[^.]")  1 tuareg-font-lock-constructor-face)
       (,(concat "\\_<let +exception +\\(" uid "\\)")
        1 tuareg-font-lock-constructor-face)
       ;; let-bindings (let f : type = fun)
       (,(concat let-binding-g4 " *\\(" lid "\\) *\\(?:: *\\([^=]+\\)\\)?= *"
                 "fun\\(?:ction\\)?\\>")
        (5 font-lock-function-name-face nil t)
        (6 font-lock-type-face keep t))
       ;; let binding variables
       (,(concat "\\(?:" let-binding-g4 "\\|" gclass-gparams "\\)")
        (tuareg--pattern-vars-matcher (tuareg--pattern-pre-form-let) nil
                                      (0 font-lock-variable-name-face keep))
        (tuareg--pattern-maybe-type-matcher nil nil ; def followed by type
                                            (1 font-lock-type-face keep)))
       (,(concat "\\_<fun\\_>" maybe-infix-ext+attr)
        (tuareg--pattern-vars-matcher (tuareg--pattern-pre-form-fun) nil
                                      (0 font-lock-variable-name-face keep)))
       (,(concat "\\_<method!? +\\(" lid "\\)")
        (1 font-lock-function-name-face keep t); method name
        (tuareg--pattern-vars-matcher (tuareg--pattern-pre-form-let) nil
                                      (0 font-lock-variable-name-face keep))
        (tuareg--pattern-maybe-type-matcher nil nil ; method followed by type
                                            (1 font-lock-type-face keep)))
       (,(concat "\\_<object *(\\(" lid "\\) *\\(?:: *\\("
                 balanced-braces "\\)\\)?)")
        (1 font-lock-variable-name-face)
        (2 font-lock-type-face keep t))
       (,(concat "\\_<object *( *\\(" typevar "\\|_\\) *)")
        1 font-lock-type-face)
       ,@(and tuareg-font-lock-symbols
              (tuareg-font-lock-symbols-keywords))))
    (setq
     tuareg-font-lock-keywords-1
     (append common-keywords
             tuareg-font-lock-keywords-1-extra))
    (setq
     tuareg-font-lock-keywords-2
     `(,@common-keywords
       ;; https://caml.inria.fr/pub/docs/manual-ocaml/lex.html#infix-symbol
       (,(concat "( *\\([-=<>@^|&+*/$%!]" operator-char
                 "*\\|[#?~]" operator-char "+\\) *)")
        1 font-lock-function-name-face)
       ;; By default do no highlight relation operators (=, <, >) nor
       ;; arithmetic operators because it is slow.  However,
       ;; optionally allow it by popular demand.
       ,@(if tuareg-highlight-all-operators
             ;; Highlight "@", "+",... after "let…[@…]" but before
             ;; "let" rules remove the highlighting of "=".
             `((,(concat before-operator-char
                         "\\([=<>@^&+*/$%!]" operator-char "*\\|:=\\|"
                         "[|#?~]" operator-char "+\\)")
                1 tuareg-font-lock-operator-face)
               ;; "-" is special: avoid "->" and "-13"
               (,(concat "\\(-\\)\\(?:[^0-9>]\\|\\("
                         operator-char-no> operator-char "*\\)\\)")
                (1 tuareg-font-lock-operator-face)
                (2 tuareg-font-lock-operator-face keep t))
               (,(regexp-opt '("type" "module" "module type"
                               "val" "val mutable")
                             'symbols)
                (tuareg--pattern-equal-matcher nil nil nil)))
           `((,(concat "[@^&$%!]" operator-char "*\\|"
                       "[|#?~]" operator-char "+")
              . tuareg-font-lock-operator-face)))
       (,(regexp-opt
          (if (tuareg-editing-ls3)
              '("asr" "asl" "lsr" "lsl" "or" "lor" "and" "land" "lxor"
                "not" "lnot" "mod" "fby" "pre" "last" "at")
            '("asr" "asl" "lsr" "lsl" "or" "lor" "land"
              "lxor" "not" "lnot" "mod"))
          'symbols)
        1 tuareg-font-lock-operator-face)
       ,@tuareg-font-lock-keywords-1-extra)))
  (setq font-lock-defaults
        `((tuareg-font-lock-keywords
           tuareg-font-lock-keywords-1
           tuareg-font-lock-keywords-2)
          nil nil
          ,tuareg-font-lock-syntax nil
          (font-lock-syntactic-face-function
           . tuareg-font-lock-syntactic-face-function)))
  ;; (push 'smie-backward-sexp-command font-lock-extend-region-functions)
  )

(defvar tuareg--pattern-matcher-limit 0
  "Limit for the matcher of function arguments")
(make-variable-buffer-local 'tuareg--pattern-matcher-limit)

(defvar tuareg--pattern-matcher-type-limit 0
  "Limit for the type of a let bound definition.")
(make-variable-buffer-local 'tuareg--pattern-matcher-type-limit)

(defun tuareg--font-lock-in-string-or-comment ()
  "Returns t if the point is inside a string or a comment.
This based on the fontification and is faster than calling `syntax-ppss'."
  (let* ((face (get-text-property (point) 'face)))
    (and (symbolp face)
         (memq face '(font-lock-comment-face
                      font-lock-comment-delimiter-face
                      font-lock-doc-face
                      font-lock-string-face)))))

(defun tuareg--pattern-pre-form-let ()
  "Return the position of \"=\" marking the end of \"let\"."
  (if (or (tuareg--font-lock-in-string-or-comment)
          (looking-at "[ \t\n]*open\\_>")       ; "let open"
          (looking-at "[ \t\n]*exception\\_>")) ; "let exception"
      (progn ; bail out
        (setq tuareg--pattern-matcher-limit (point))
        (setq tuareg--pattern-matcher-type-limit (point)))

    (let* ((opoint (point))
           (limit (+ opoint 800))
           pos)
      (setq tuareg--pattern-matcher-limit nil)
      (while (and
              (setq pos (re-search-forward "[=({:]" limit t))
              (progn
                (backward-char)
                (cond
                 ((memq (char-after) '(?\( ?\{))
                  ;; Skip balanced braces
                  (if (ignore-errors (forward-list))
                      t
                    (goto-char (1- pos))
                    nil)) ; If braces are not balanced, stop.
                 ((char-equal ?: (char-after))
                  ;; Make sure it is not a label
                  (skip-chars-backward "a-zA-Z0-9_'")
                  (if (not (memq (char-before) '(?~ ??)))
                      (setq tuareg--pattern-matcher-limit (1- pos)))
                  (goto-char pos)
                  t)
                 (t nil)))))
      (setq tuareg--pattern-matcher-type-limit (1+ (point))); include "="
      (unless tuareg--pattern-matcher-limit
        (setq tuareg--pattern-matcher-limit (point)))
      ;; Remove any possible highlighting on "="
      (unless (eobp)
        (put-text-property (point) (1+ (point)) 'face nil))
      ;; move the point back for the sub-matcher
      (goto-char opoint))
    (put-text-property (point) tuareg--pattern-matcher-limit
                       'font-lock-multiline t)
    tuareg--pattern-matcher-limit))

(defun tuareg--pattern-pre-form-fun ()
  "Return the position of \"->\" marking the end of \"fun\"."
  (if (tuareg--font-lock-in-string-or-comment)
      (setq tuareg--pattern-matcher-limit (point))

    (let* ((opoint (point))
           (limit (+ opoint 800))
           pos)
      (while (and
              (setq pos (re-search-forward "[-({]" limit t))
              (cond
               ((or (char-equal ?\( (char-before))
                    (char-equal ?{  (char-before)))
                (backward-char)
                (if (ignore-errors (forward-list))
                    t
                  (goto-char (1- pos))
                  nil)) ; If braces are not balanced, stop.
              (t (not (char-equal ?> (char-after)))))))
      (setq tuareg--pattern-matcher-limit (point))
      ;; move the point back for the sub-matcher
      (goto-char opoint)
      (put-text-property (point) tuareg--pattern-matcher-limit
                         'font-lock-multiline t))
    tuareg--pattern-matcher-limit))

(defun tuareg--pattern-equal-matcher (limit)
  "Find \"=\" and \"+=\" and remove its highlithing."
  (unless (tuareg--font-lock-in-string-or-comment)
    (let (pos)
      (while (and
              (<= (point) limit)
              (setq pos (re-search-forward "[+=({[]" limit t))
              (progn
                (backward-char)
                (cond
                 ((or (char-equal ?\( (char-after))
                      (char-equal ?{  (char-after))
                      (char-equal ?\[ (char-after)))
                  ;; Skip balanced braces
                  (if (ignore-errors (forward-list))
                      t
                    (goto-char (1- pos))
                    nil)) ; If braces are not balanced, stop.
                 ((char-equal ?+ (char-after))
                  (if (char-equal ?= (char-after pos))
                      (put-text-property (point) (1+ pos) 'face nil)
                    (forward-char)
                    t))
                 (t
                  (put-text-property (point) (1+ (point)) 'face nil))))))
      nil)))

(defun tuareg--pattern-vars-matcher (limit)
  "Match a variable name after the point.
If it succeeds, it moves the point after the variable name and set
`match-data'.  See e.g., `font-lock-keywords'."
  (when (and (<= (point) tuareg--pattern-matcher-limit)
             (re-search-forward tuareg--lid-re tuareg--pattern-matcher-limit t))
    (skip-chars-forward " \t\n")
    (if (>= (point) tuareg--pattern-matcher-limit)
        t
      (cond
       ((char-equal ?= (char-after))
        ;; Remove possible fontification of "=" (e.g. as an operator)
        (put-text-property (point) (1+ (point)) 'face nil)
        ;; Decide whether it is ?(v =...) or {x = v; x = v}
        (goto-char (match-beginning 0))
        (skip-chars-backward " \t\n")
        (if (char-equal (char-before) ?\() ; (var = expr)
            (progn (up-list) ; keep match, skip expr
                   t)
          ;; This is a label record, variable after
          (goto-char (match-end 0))
          (re-search-forward tuareg--lid-re tuareg--pattern-matcher-limit t)))
       ((char-equal ?: (char-after))
        (let ((beg-ty (1+ (point))))
          (goto-char (match-beginning 0))
          (skip-chars-backward " \t\n")
          (if (char-equal (char-before) ?\() ; (var : t)
              (progn
                (up-list) ; keep match, skip type
                (put-text-property beg-ty (1- (point)) 'face
                                   'font-lock-type-face)
                t)
            (goto-char (match-end 0)))))
       (t t)))))

(defun tuareg--pattern-maybe-type-matcher (limit)
  "Match a possible type after a let binding.
Run only once."
  ;; This function is needed because we want to ensure that the search
  ;; is bounded by the detected "=".
  (when (and (<= (point) tuareg--pattern-matcher-type-limit)
             (re-search-forward "[ \t\n]*:\\([^=]+\\)="
                                tuareg--pattern-matcher-type-limit t))
    (goto-char limit) ; Do not run a second time
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Keymap

(defvar tuareg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-q" 'tuareg-indent-phrase)
    (define-key map "\C-c\C-q" 'tuareg-indent-phrase)
    ;; Don't bother: it's the global default anyway.
    ;;(define-key map "\M-\C-\\" 'indent-region)
    (define-key map "\C-c\C-a" 'tuareg-find-alternate-file)
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-c\C-w" 'tuareg-opam-update-env)
    (define-key map "\C-xnd" 'tuareg-narrow-to-phrase)
    (define-key map "\M-\C-x" 'tuareg-eval-phrase)
    (define-key map "\C-x\C-e" 'tuareg-eval-phrase)
    (define-key map "\C-c\C-e" 'tuareg-eval-phrase)
    (define-key map "\C-c\C-r" 'tuareg-eval-region)
    (define-key map "\C-c\C-b" 'tuareg-eval-buffer)
    (define-key map "\C-c\C-s" 'tuareg-run-ocaml)
    (define-key map "\C-c\C-i" 'tuareg-interrupt-ocaml)
    (define-key map "\C-c\C-k" 'tuareg-kill-ocaml)
    (define-key map "\C-c\C-n" 'tuareg-next-phrase)
    (define-key map "\C-c\C-p" 'tuareg-previous-phrase)
    (define-key map [(backspace)] 'backward-delete-char-untabify)
    (define-key map [(control c) (home)]
      'tuareg-move-inside-module-or-class-opening)
    (define-key map "\C-c`" 'tuareg-interactive-next-error-source)
    (define-key map "\C-c?" 'tuareg-interactive-next-error-source)
    (define-key map "\C-c.c" 'tuareg-insert-class-form)
    (define-key map "\C-c.b" 'tuareg-insert-begin-form)
    (define-key map "\C-c.f" 'tuareg-insert-for-form)
    (define-key map "\C-c.w" 'tuareg-insert-while-form)
    (define-key map "\C-c.i" 'tuareg-insert-if-form)
    (define-key map "\C-c.l" 'tuareg-insert-let-form)
    (define-key map "\C-c.m" 'tuareg-insert-match-form)
    (define-key map "\C-c.t" 'tuareg-insert-try-form)
    (when tuareg-with-caml-mode-p
      ;; Trigger caml-types
      (define-key map [?\C-c ?\C-t] 'caml-types-show-type)  ; "type"
      (define-key map [?\C-c ?\C-f] 'caml-types-show-call)  ; "function"
      (define-key map [?\C-c ?\C-l] 'caml-types-show-ident) ; "let"
      ;; To prevent misbehavior in case of error during exploration.
      (define-key map [?\C-c mouse-1] 'caml-types-mouse-ignore)
      (define-key map [?\C-c down-mouse-1] 'caml-types-explore)
      ;; Trigger caml-help
      (define-key map [?\C-c ?\C-i] 'ocaml-add-path)
      (define-key map [?\C-c ?\[] 'ocaml-open-module)
      (define-key map [?\C-c ?\]] 'ocaml-close-module)
      (define-key map [?\C-c ?\C-h] 'caml-help)
      (define-key map [?\C-c ?\t] 'tuareg-complete))
    map)
  "Keymap used in Tuareg mode.")

(defvar tuareg-electric-indent-keywords
  '("module" "class" "functor" "object" "type" "val" "inherit"
    "include" "virtual" "constraint" "exception" "external" "open"
    "method" "and" "initializer" "to" "downto" "do" "done" "else"
    "begin" "end" "let" "in" "then" "with"))

(defun tuareg--electric-indent-predicate (char)
  "Check whether we should auto-indent.
For use on `electric-indent-functions'."
  (save-excursion
    (tuareg-backward-char);; Go before the inserted char.
    (let ((syntax (char-syntax char)))
      (if (tuareg-in-indentation-p)
          (or (eq char ?|) (eq syntax ?\)))
        (or (pcase char
              (`?\) (char-equal ?* (preceding-char)))
              (`?\} (and (char-equal ?> (preceding-char))
                         (progn (tuareg-backward-char)
                                (tuareg-in-indentation-p))))
              (`?\] (and (char-equal ?| (preceding-char))
                         (progn (tuareg-backward-char)
                                (tuareg-in-indentation-p)))))
            (and tuareg-electric-indent
                 (not (eq syntax ?w))
                 (let ((end (point)))
                   (skip-syntax-backward "w_")
                   (member (buffer-substring (point) end)
                           tuareg-electric-indent-keywords))
                 (tuareg-in-indentation-p)))))))

(defun tuareg--electric-close-vector ()
  ;; Function for use on post-self-insert-hook.
  (when tuareg-electric-close-vector
    (let ((inners (cdr (assq last-command-event
                             '((?\} ?> "{<") (?\] ?| "\\[|"))))))
      (and inners
           (eq (char-before) last-command-event) ;; Sanity check.
           (not (eq (car inners) (char-before (1- (point)))))
           (not (tuareg-in-literal-or-comment-p))
           (save-excursion
             (when (ignore-errors (backward-sexp 1) t)
               (looking-at (nth 1 inners))))
           (save-excursion
             (goto-char (1- (point)))
             (insert (car inners)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				 SMIE

;; TODO:
;; - Obey tuareg-*-indent customization variables.
;; - Fix use of tuareg-indent-command in tuareg-auto-fill-insert-leading-star.
;; - Use it by default (when possible).
;; - Move the old indentation code to a separate file.

(defconst tuareg-smie-grammar
  ;; Problems:
  ;; - "let D in E" expression vs "let D" declaration.  This is solved
  ;;   by making the lexer return "d-let" for the second case.
  ;; - FIXME: SMIE assumes that concatenation binds tighter than
  ;;   everything else, whereas OCaml gives tighter precedence to ".".
  ;; - "x : t1; (y : (t2 -> t3)); z : t4" but
  ;;   "when (x1; x2) -> (z1; z2)".  We solve this by distinguishing
  ;;   the two kinds of arrows, using "t->" for the type arrow.
  ;; - The "with" in modules's "with type" has different precedence.
  ;; - Big problem with "if...then": because of SMIE's transitivity of the
  ;;   precedence relation, we can't properly parse both "if A then B; C" and
  ;;   "if A then let x = E in B; C else D" (IOW I think a non-transitive OPG
  ;;   could do it).  We could try and fix the problem in the lexer, but it's
  ;;   far from obvious how (we'd probably end up having to pre-parse the text
  ;;   in the lexer to decide which kind of "if" and "then" we're looking
  ;;   at).  A good solution could be found maybe if SMIE let us disambiguate
  ;;   lexemes late, i.e. at a time where we have access to the relevant parse
  ;;   stack.  Or maybe by allowing smie-grammar to use a non-transitive
  ;;   precedence relation.  But until that happens, we will live with an
  ;;   incorrect parse, and instead we try to patch up the result with ad-hoc
  ;;   hacks in tuareg-smie-rules.
  ;; - The "<module-type> with <mod-constraints>" syntax introduces many
  ;;   conflicts:
  ;;      "... with module M = A with module B = C"
  ;;   vs      "... module M = A with module B = C"
  ;;   In the first, the second "with" should either have the first "with" as
  ;;   sibling, or have some earlier construct as parent, whereas in the second
  ;;   the "with" should have the first "=" (or maybe the first "module", tho
  ;;   that would not correspond to the actual language syntax and would
  ;;   probably break other cases) as parent.  Other problems in this
  ;;   mod-constraints syntax: we need a precedence along the lines of
  ;;   "with" < "and" < "module/type", whereas the rest of the syntax wants
  ;;   "module/type" < "and" < "with", so basically all the keywords involved
  ;;   in mod-constraints need to be handled specially in the lexer :-(
  ;; - and then some...
  (let ((bnfprec2
         (smie-bnf->prec2
          '((decls (decls "type" decls) (decls "d-let" decls)
                   (decls "and" decls) (decls ";;" decls)
                   (decls "exception" decls)
                   (decls "module" decls)
                   (decls "class" decls)
                   (decls "val" decls) (decls "external" decls)
                   (decls "open" decls) (decls "include" decls)
                   (exception)
                   (def)
                   ;; Hack: at the top-level, a "let D in E" can appear in
                   ;; decls as well, but the lexer classifies it as "d-let",
                   ;; so we need to make sure that "d-let D in E" doesn't
                   ;; end up matching the "in" with some far away thingy.
                   (def-in-exp))
            (def-in-exp (defs "in" exp))
            (def (var "d=" exp) (id "d=" datatype) (id "d=" module))
            (idtype (id ":" type))
            (var (id) ("m-type" var) ("d-type" var) ("rec" var)
                 ("private" var) (idtype)
                 ("l-module" var) ("l-class" var))
            (exception (id "of" type))
            (datatype ("{" typefields "}") (typebranches)
                      (typebranches "with" id))
            (typebranches (typebranches "|" typebranches) (id "of" type))
            (typefields (typefields ";" typefields) (idtype))
            (type (type "*…" type) (type "t->" type)
                  ;; ("<" ... ">") ;; FIXME!
                  (type "as" id))
            (id)
            (module ("struct" decls "end")
                    ("sig" decls "end")
                    ("functor" id "->" module)
                    (module "m-with" mod-constraints))
            (simpledef (id "c=" type))
            (mod-constraints (mod-constraints "m-and" mod-constraints)
                             ("w-type" simpledef)
                             ("w-module" simpledef))
            ;; http://caml.inria.fr/pub/docs/manual-ocaml/expr.html
            ;; exp1 is "all exps except for `if exp then'".
            (exp1 ("begin" exp "end")
                  ("(" exp:type ")")
                  ("[|" exp "|]")
                  ("{" fields "}")
                  ("if" exp "then" exp1 "else" exp1)
                  ;; ("if" exp "then" exp)
                  ("while" exp "do" exp "done")
                  ("for" forbounds "do" exp "done")
                  (exp1 ";" exp1)
                  ("match" exp "with" branches)
                  ("function" branches)
                  ("fun" patterns* "->" exp1)
                  ("try" exp "with" branches)
                  ("let" defs "in" exp1)
                  ("let" "exception-let" exception "in" exp1)
                  ("object" class-body "end")
                  ("(" exp:>type ")")
                  ("{<" fields ">}")
                  ;; MetaOCaml thingies.
                  ;; Let's not do anything special for .~ for now,
                  ;; as for !. it's deprecated anyway!
                  (".<" exp ">."))
            ;; Like `exp' but additionally allow if-then without else.
            (exp (exp1) ("if" exp "then" exp))
            (forbounds (iddef "to" exp) (iddef "downto" exp))
            (defs (def) (defs "and" defs) ("l-open" id))
            (exp:>type (exp:type ":>" type))
            (exp:type (exp)) ;; (exp ":" type)
            (fields (fields1) (exp "with" fields1))
            (fields1 (fields1 ";" fields1) (iddef))
            (iddef (id "f=" exp1))
            (branches (branches "|" branches) (branch))
            (branch (patterns "->" exp1))
            (patterns* ("-dlpd-" patterns*) (patterns)) ;See use of "-dlpd-".
            (patterns (pattern) (pattern "when" exp1)
                      ;; Since OCaml 4.02, `match' expressions allow
                      ;; `exception' branches.
                      ("exception-case" pattern))
            (pattern (id) (pattern "as" id) (pattern "|-or" pattern)
                     (pattern "," pattern))
            (class-body (class-body "inherit" class-body)
                        (class-body "method" class-body)
                        (class-body "initializer" class-body)
                        (class-body "val" class-body)
                        (class-body "constraint" class-body)
                        (class-field))
            (class-field (exp) ("mutable" idtype)
                         ("virtual" idtype) ("private" idtype))
            ;; We get cyclic dependencies between ; and | because things like
            ;; "branches | branches" implies that "; > |" whereas "exp ; exp"
            ;; implies "| > ;" and while those two do not directly conflict
            ;; because they're constraints on precedences of different sides,
            ;; they do introduce a cycle later on because those operators are
            ;; declared associative, which adds a constraint that both sides
            ;; must be of equal precedence.  So we declare here a dummy rule
            ;; to force a direct conflict, that we can later resolve with
            ;; explicit precedence rules.
            (foo1 (foo1 ";" foo1) (foo1 "|" foo1))
            ;; "mutable x : int ; y : int".
            (foo2 ("mutable" id) (foo2 ";" foo2))
            )
          ;; Type precedence rules.
          ;; http://caml.inria.fr/pub/docs/manual-ocaml/types.html
          '((nonassoc "as") (assoc "t->") (assoc "*…"))
          ;; Pattern precedence rules.
          ;; http://caml.inria.fr/pub/docs/manual-ocaml/patterns.html
          '((nonassoc "as") (assoc "|-or") (assoc ",") (assoc "::"))
          ;; Resolve "{a=(1;b=2)}" vs "{(a=1);(b=2)}".
          '((nonassoc ";") (nonassoc "f="))
          ;; Resolve "(function a -> b) | c -> d".
          '((nonassoc "function") (nonassoc "|"))
          ;; Resolve "when (function a -> b) -> c".
          '((nonassoc "function") (nonassoc "->"))
          ;; Resolve ambiguity "(let d in e2); e3" vs "let d in (e2; e3)".
          '((nonassoc "in" "match" "->" "with") (nonassoc ";"))
          ;; Resolve "(if a then b else c);d" vs "if a then b else (c; d)".
          '((nonassoc ";") (nonassoc "else")) ;; ("else" > ";")
          ;; Resolve "match e1 with a → (match e2 with b → e3 | c → e4)"
          ;;      vs "match e1 with a → (match e2 with b → e3) | c → e4"
          '((nonassoc "with") (nonassoc "|"))
          ;; Resolve "functor A -> (M with MC)".
          '((nonassoc "->") (nonassoc "m-with"))
          ;; Resolve the conflicts caused by "when" and by SMIE's assumption
          ;; that all non-terminals can match the empty string.
          '((nonassoc "with") (nonassoc "->")) ; "when (match a with) -> e"
          '((nonassoc "|") (nonassoc "->"))    ; "when (match a with a|b) -> e"
          ;; Fix up conflict between (decls "and" decls) and (defs "in" exp).
          '((nonassoc "in") (nonassoc "and"))
          ;; Resolve the "artificial" conflict introduced by the `foo1' rule.
          '((assoc "|") (assoc ";"))
          ;; Fix up associative declaration keywords.
          '((assoc "type" "d-let" "exception" "module" "val" "open"
                   "external" "include" "class" ";;")
            (assoc "and"))
          '((assoc "val" "method" "inherit" "constraint" "initializer"))
          ;; Declare associativity of remaining sequence separators.
          '((assoc ";")) '((assoc "|")) '((assoc "m-and")))))
    ;; (dolist (pair '()) ;; ("then" . "|") ("|" . "then")
    ;;   (display-warning 'prec2 (format "%s %s %s"
    ;;                                   (car pair)
    ;;                                   (gethash pair bnfprec2)
    ;;                                   (cdr pair))))
    ;; SMIE takes for granted that all non-terminals can match the empty
    ;; string, which can lead to the addition of unnecessary constraints.
    ;; Let's remove the ones that cause cycles without causing conflicts.
    (progn
      ;; This comes from "exp ; exp" and "function branches", where
      ;; SMIE doesn't realize that `branches' has to have a -> before ;.
      (cl-assert (eq '> (gethash (cons "function" ";") bnfprec2)))
      (remhash (cons "function" ";") bnfprec2))
    (smie-prec2->grammar
     (smie-merge-prec2s
      bnfprec2
      (smie-precs->prec2
       ;; Precedence of operators.
       ;; http://caml.inria.fr/pub/docs/manual-ocaml/expr.html
       (reverse
        '((nonassoc ".")
          ;; function application, constructor application, assert, lazy
          ;; - -. (prefix)    –
          (right "**…" "lsl" "lsr" "asr")
          (nonassoc "*…" "/…" "%…" "mod" "land" "lor" "lxor")
          (left "+…" "-…")
          (assoc "::")
          (right "@…" "^…")
          (left "=…" "<…" ">…" "|…" "&…" "$…")
          (right "&" "&&")
          (right "or" "||")
          (assoc ",")
          (right "<-" ":=")
          (assoc ";"))))))))

(defun tuareg-smie--search-backward (tokens)
  (let (tok)
    (while (progn
             (setq tok (tuareg-smie--backward-token))
             (if (not (zerop (length tok)))
                 (not (member tok tokens))
	       (unless (bobp)
		 (condition-case err
		     (progn (backward-sexp) t)
		   (scan-error
		    (setq tok (buffer-substring (nth 3 err) (1+ (nth 3 err))))
		    nil))))))
    tok))

(defun tuareg-smie--search-forward (tokens)
  (let (tok)
    (while (progn
             (setq tok (tuareg-smie--forward-token))
             (if (not (zerop (length tok)))
                 (not (member tok tokens))
	       (unless (eobp)
		 (condition-case err
		     (progn (forward-sexp) t)
		   (scan-error
		    (setq tok (buffer-substring (nth 2 err) (nth 3 err)))
		    nil))))))
    tok))

(defun tuareg-skip-blank-and-comments ()
  (forward-comment (point-max)))

(defconst tuareg-smie--type-label-leader
  '("->" ":" "=" ""))

(defconst tuareg-smie--exp-operator-leader
  (delq nil (mapcar (lambda (x) (if (numberp (nth 2 x)) (car x)))
                    tuareg-smie-grammar)))

(defconst tuareg-smie--float-re "[0-9]+\\(?:\\.[0-9]*\\)?\\(?:e[-+]?[0-9]+\\)")

(defun tuareg-smie--forward-token ()
  (tuareg-skip-blank-and-comments)
  (let ((start (point))
        (end nil))
    (if (zerop (skip-syntax-forward "."))
              (let ((start (point)))
                (skip-syntax-forward "w_'")
                ;; Watch out for floats!
                (and (memq (char-after) '(?- ?+))
                     (eq (char-before) ?e)
                     (save-excursion
                       (goto-char start)
                       (looking-at tuareg-smie--float-re))
                     (goto-char (match-end 0))))
            ;; The "." char is given symbol property so that "M.x" is
            ;; considered as a single symbol, but in reality, it's part of the
            ;; operator chars, since "+." and friends are operators.
            (while (not (and (zerop (skip-chars-forward "."))
                             (zerop (skip-syntax-forward ".")))))
            (when (and (eq (char-before) ?%)
                       (looking-at "[[:alpha:]]+"))
              ;; Infix extension nodes, bug#121
              (setq end (1- (point)))
              (goto-char (match-end 0))))
    (buffer-substring-no-properties start (or end (point)))))

(defun tuareg-smie--backward-token ()
  (forward-comment (- (point)))
  (let ((end (point)))
    (if (and (zerop (skip-chars-backward "."))
             (zerop (skip-syntax-backward ".")))
        (progn
          (skip-syntax-backward "w_'")
          ;; Watch out for floats!
          (pcase (char-before)
            ((or `?- `?+)
             (and (memq (char-after) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
                  (save-excursion
                    (forward-char -1) (skip-syntax-backward "w_")
                    (looking-at tuareg-smie--float-re))
                  (>= (match-end 0) (point))
                  (goto-char (match-beginning 0))))
            (`?%                        ;extension node, bug#121
             (let ((pos (point)))
               (forward-char -1)
               (if (and (zerop (skip-chars-backward "."))
                        (zerop (skip-syntax-backward ".")))
                   (goto-char pos)
                 (setq end (1- pos)))))))
      (cond
       ((memq (char-after) '(?\; ?,)) nil) ; ".;" is not a token.
       ((and (eq (char-after) ?\.)
             (memq (char-before) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)))
        (skip-chars-backward "0-9"))    ; A float number!
       (t ;; The "." char is given symbol property so that "M.x" is
        ;; considered as a single symbol, but in reality, it's part of
        ;; the operator chars, since "+." and friends are operators.
        (while (not (and (zerop (skip-chars-backward "."))
                         (zerop (skip-syntax-backward "."))))))))
    (buffer-substring-no-properties (point) end)))

(defun tuareg-smie-forward-token ()
  "Move point to the end of the next token and return its SMIE name."
  (let ((tok (tuareg-smie--forward-token)))
    (cond
     ((zerop (length tok))
      (if (not (looking-at "{<\\|\\[|"))
          tok
        (goto-char (match-end 0))
        (match-string 0)))
     ((and (equal tok "|") (looking-at-p "\\]")) (forward-char 1) "|]")
     ((and (equal tok ">") (looking-at-p "}")) (forward-char 1) ">}")
     ((and (equal tok ".") (memq (char-after) '(?< ?~)))
      (forward-char 1) (string ?. (char-before)))
     ((or (member tok '("let" "=" "->"
                        "module" "class" "open" "type" "with" "and"
                        "exception"))
          ;; http://caml.inria.fr/pub/docs/manual-ocaml/expr.html lists
          ;; the tokens whose precedence is based on their prefix.
          (memq (aref tok 0) '(?* ?/ ?% ?+ ?- ?@ ?^ ?= ?< ?> ?| ?& ?$)))
      ;; When indenting, the movement is mainly backward, so it's OK to make
      ;; the forward tokenizer a bit slower.
      (save-excursion (tuareg-smie-backward-token)))
     ((and (member tok '("~" "?"))
           (looking-at "[[:alpha:]_][[:alnum:]'_]*:"))
      (goto-char (match-end 0))
      "label:")
     ((and (looking-at-p ":\\(?:[^:]\\|\\'\\)")
           (string-match-p "\\`[[:alpha:]_]" tok)
           (save-excursion
             (tuareg-smie--backward-token) ;Go back.
             (member (tuareg-smie--backward-token)
                     tuareg-smie--type-label-leader)))
      (forward-char 1)
      "label:")
     ((string-match-p "\\`[[:alpha:]_].*\\.\\'"  tok)
      (forward-char -1) (substring tok 0 -1))
     (t tok))))

(defconst tuareg-smie--exp-leaders
  ;; (let ((leaders ()))
  ;;   (dolist (cat tuareg-smie-bnf)
  ;;     (dolist (rule (cdr cat))
  ;;       (setq rule (reverse rule))
  ;;       (while (setq rule (cdr (cl-member 'dummy rule
  ;;                                         :test (lambda (_ x)
  ;;                                                 (memq x '(exp exp1))))))
  ;;         (push (car rule) leaders))))
  ;;   (prin1-to-string (sort (delete-dups leaders) #'string-lessp)))
  ;; BEWARE: In let-disambiguate, we compare this against the output of
  ;; tuareg-smie--backward-token which never returns refined tokens like "d=",
  ;; so we manually replace those with just "=" here!
  '("->" ".<" ";" "[|" "begin" "=" "do" "downto" "else" "if" "in"
    "match" "then" "to" "try" "when" "while"))

(defun tuareg-smie--let-disambiguate ()
  "Return \"d-let\" if \"let\" at point is a decl, or just \"let\" if it's an exp."
  (save-excursion
    (let ((prev (tuareg-smie--backward-token)))
      (if (or (member prev tuareg-smie--exp-leaders)
              (if (zerop (length prev))
                  (and (not (bobp))
                       ;; See if prev char has open-paren syntax.
                       (eq 4 (mod (car (syntax-after (1- (point)))) 256)))
                (and (eq ?. (char-syntax (aref prev 0)))
                     (and (not (equal prev ";;"))
                          (let ((tokinfo (assoc prev smie-grammar)))
                            ;; Check that prev is not a closing token like ">."
                            (or (null tokinfo)
                                (integerp (nth 2 tokinfo))))))))
          "let"
        "d-let"))))

(defun tuareg-smie--label-colon-p ()
  (and (not (zerop (skip-chars-backward "[[:alnum:]]_")))
       (or (not (zerop (skip-chars-backward "?~")))
           (save-excursion
             (member (tuareg-smie--backward-token)
                     tuareg-smie--type-label-leader)))))

(defun tuareg-smie--=-disambiguate ()
  "Return which kind of \"=\" we've just found.
Point is not moved and should be right in front of the equality.
Return values can be
  \"f=\" for field definition,
  \"d=\" for a normal definition,
  \"c=\" for a type equality constraint, and
  \"=…\" for an equality test."
  (save-excursion
    (let* ((pos (point))
           (telltale '("type" "let" "module" "class" "and" "external"
                       "val" "method" "=" ":="
                       "if" "then" "else" "->" ";" ))
           (nearest (tuareg-smie--search-backward telltale)))
      (cond
       ((and (member nearest '("{" ";"))
             (let ((field t))
               (while
                   (let ((x (tuareg-smie--forward-token)))
                     (and (< (point) pos)
                          (cond
                           ((zerop (length x)) (setq field nil))
                           ((memq (char-syntax (aref x 0)) '(?w ?_)))
                           ((member x '("." ";")))
                           (t (setq field nil))))))
               field))
        "f=")
       ((progn
          (while (and (equal nearest "->")
                      (save-excursion
                        (forward-char 2)
                        (equal (tuareg-smie-backward-token) "t->")))
            (setq nearest (tuareg-smie--search-backward telltale)))
          nil))
       ((and (member nearest '("=" ":="))
             (member (tuareg-smie--search-backward telltale)
                     '("type" "module")))
        ;; Second equality in "type t = M.t = C" or after mod-constraint
        "d=")
       ((not (member nearest '("type" "let" "module" "class" "and"
                               "external" "val" "method")))
        "=…")
       ((and (member nearest '("type" "module"))
             ;; Maybe a module's type equality constraint?
             (or (member (tuareg-smie--backward-token) '("with" "and"))
                 ;; Or maybe an alias as part of a definition?
                 (and (equal nearest "type")
                      (goto-char (1+ pos)) ;"1+" to skip the `=' itself!
                      (let ((tok (tuareg-smie--search-forward
                                  (cons "=" (mapcar #'car
                                                    tuareg-smie-grammar)))))
                        (equal tok "=")))))
        "c=")
       (t "d=")))))

(defun tuareg-smie--:=-disambiguate ()
  "Return which kind of \":=\" we've just found.
Point is not moved and should be right in front of the equality.
Return values can be
  \":=\" for assignment definition,
  \"c=\" for destructive equality constraint."
  (save-excursion
    (let* ((telltale '("type" "let" "module" "class" "and" "external"
                       "val" "method" "=" ":="
                       "if" "then" "else" "->" ";" ))
           (nearest (tuareg-smie--search-backward telltale)))
      (cond				;Issue #7
       ((and (member nearest '("type" "module"))
             (member (tuareg-smie--backward-token) '("with" "and"))) "c=")
       (t ":=")))))

(defun tuareg-smie--|-or-p ()
  "Return non-nil if we're just in front of an or pattern \"|\"."
  (save-excursion
    (let ((tok (tuareg-smie--search-backward
                ;; Stop at the first | or any token which should
                ;; never appear between a "|" and a "|-or".
                '("|" "[" "->" "with" "function" "=" "of" "in" "then"))))
      (cond
       ((equal tok "(") t)
       ((equal tok "|")
        ;; Maybe we have a "|-or".  Then again maybe not.  We should make sure
        ;; that `tok' is really either a "|-or" or the | of a match (and not
        ;; the | of a datatype definition).
        (while
            (equal "|"
                   (setq tok
                         (tuareg-smie--search-backward
                          '("|" "with" "function" "=" "of" "in" "then")))))
        (cond
         ((equal tok "=")
          (not (equal (tuareg-smie--=-disambiguate) "d=")))
         ((equal tok "of") nil)
         ((member tok '("[" "{" "(")) nil)
         (t t)))))))

(defun tuareg-smie-backward-token ()
  "Move point to the beginning of the next token and return its SMIE name."
  (let ((tok (tuareg-smie--backward-token)))
    (cond
     ;; Distinguish a let expression from a let declaration.
     ((equal tok "let") (tuareg-smie--let-disambiguate))
     ((equal ".<.~" tok) (forward-char 2) ".~") ;FIXME: Likely too ad-hoc!
     ;; Handle "let module" and friends.
     ((member tok '("module" "class" "open"))
      (let ((prev (save-excursion (tuareg-smie--backward-token))))
        (cond
         ((equal prev "let") (concat "l-" tok))
         ((and (member prev '("with" "and")) (equal tok "module")) "w-module")
         (t tok))))
     ;; Distinguish a "type ->" from a "case ->".
     ((equal tok "->")
      (save-excursion
        (let (nearest)
          (while (progn
                   (setq nearest (tuareg-smie--search-backward
                                  '("with" "|" "fun" "function" "functor"
				    "type" ":" "of")))
                   (and (equal nearest ":")
                        (tuareg-smie--label-colon-p))))
          (if (member nearest '("with" "|" "fun" "function" "functor"))
              tok "t->"))))
     ;; Handle "module type", mod-constraint's "with/and type" and
     ;; polymorphic syntax.
     ((equal tok "type")
      (save-excursion
        (let ((prev (tuareg-smie--backward-token)))
          (cond ((equal prev "module") "m-type")
                ((member prev '("and" "with")) "w-type")
                ((equal prev ":") "d-type"); ": type a. ..."
                (t tok)))))
     ;; Disambiguate mod-constraint's "and" and "with".
     ((member tok '("with" "and"))
      (save-excursion
        (tuareg-smie--forward-token)
        (if (member (tuareg-smie--forward-token) '("type" "module"))
            (concat "m-" tok) tok)))
     ;; Distinguish a defining = from a comparison-=.
     ((equal tok "=")
      (tuareg-smie--=-disambiguate))
     ((equal tok ":=") (tuareg-smie--:=-disambiguate))
     ((zerop (length tok))
      (if (not (and (memq (char-before) '(?\} ?\]))
                    (save-excursion (forward-char -2)
                                    (looking-at ">}\\||\\]"))))
          tok
        (goto-char (match-beginning 0))
        (match-string 0)))
     ((and (equal tok "|") (eq (char-before) ?\[)) (forward-char -1) "[|")
     ((and (equal tok "<") (eq (char-before) ?\{)) (forward-char -1) "{<")
     ((equal tok "|")
      ;; Check if it's the | of an or-pattern, since it has a slightly
      ;; different precedence (see Issue #71 for an example).
      (if (tuareg-smie--|-or-p) "|-or" "|"))
     ;; Some infix operators get a precedence based on their prefix, so we
     ;; collapse them into a canonical representative.
     ;; See http://caml.inria.fr/pub/docs/manual-ocaml/expr.html.
     ((memq (aref tok 0) '(?* ?/ ?% ?+ ?- ?@ ?^ ?= ?< ?> ?| ?& ?$))
      (cond
       ((member tok '("|" "||" "&" "&&" "<-" "->" ">.")) tok)
       ((and (eq (aref tok 0) ?*) (> (length tok) 1) (eq (aref tok 1) ?*))
        "**…")
       (t (string (aref tok 0) ?…))))
     ((equal tok ":")
      (let ((pos (point)))
        (if (tuareg-smie--label-colon-p)
            "label:"
          (goto-char pos)
          tok)))
     ((equal tok "exception")
      (let ((back-tok (save-excursion (tuareg-smie--backward-token))))
	(cond
	 ((member back-tok '("|" "with")) "exception-case")
	 ((equal back-tok "let") "exception-let")
	 (t tok))))
     ((string-match-p "\\`[[:alpha:]_].*\\.\\'"  tok)
      (forward-char (1- (length tok))) ".")
     (t tok))))

(defun tuareg-smie-rules (kind token)
  ;; FIXME: Handling of "= |", "with |", "function |", and "[ |" is
  ;; problematic.
  ;; FIXME: Start with (pcase (cons kind token) ...) so Edebug jumps
  ;; straight to the appropriate branch!
  (cond
   ;; Special indentation for module fields.
   ((and (eq kind :after) (member token '("." ";"))
         (smie-rule-parent-p "with")
         (tuareg-smie--with-module-fields-rule)))
   ((and (eq kind :after) (equal token ";;"))
    0)
   ;; Special indentation for monadic >>>, >>|, >>=, and >|= operators.
   ((and (eq kind :before) (tuareg-smie--monadic-rule token)))
   ((and (equal token "and") (smie-rule-parent-p "type"))
    0)
   ((member token '(";" "|" "," "and" "m-and"))
    (cond
     ((and (eq kind :before) (member token '("|" ";"))
           (smie-rule-parent-p "then")
           ;; We have misparsed the code: TOKEN is not a child of `then' but
           ;; should have closed the "if E1 then E2" instead!
           (tuareg-smie--if-then-hack token)))
     ;; FIXME: smie-rule-separator doesn't behave correctly when the separator
     ;; is right after the parent (on another line).
     ((and (smie-rule-bolp) (smie-rule-prev-p "d=" "with" "[" "function"))
      (if (and (eq kind :before) (smie-rule-bolp)
               (smie-rule-prev-p "[" "d=" "function"))
          0 tuareg-with-indent))
     ((and (equal token "|") (smie-rule-bolp) (not (smie-rule-prev-p "d="))
           (smie-rule-parent-p "d="))
      ;; FIXME: Need a comment explaining what this tries to do.
      ;; FIXME: Should this only apply when (eq kind :before)?
      ;; FIXME: Don't use smie--parent.
      (when (bound-and-true-p smie--parent)
        (goto-char (cadr smie--parent))
        (smie-indent-forward-token)
        (tuareg-skip-blank-and-comments)
        `(column . ,(- (current-column) 2))))
     (t (smie-rule-separator kind))))
   (t
    (pcase kind
      (`:elem (cond
               ((eq token 'basic) tuareg-default-indent)
               ;; The default tends to indent much too deep.
               ((eq token 'empty-line-token) ";;")))
      (`:list-intro (member token '("fun")))
      (`:close-all t)
      (`:before
       (cond
        ((equal token "d=") (smie-rule-parent 2))
        ((member token '("fun" "match"))
         (and (not (smie-rule-bolp))
              (cond ((smie-rule-prev-p "d=")
                     (smie-rule-parent tuareg-default-indent))
                    ((smie-rule-prev-p "begin") (smie-rule-parent)))))
        ((equal token "then") (smie-rule-parent))
        ((equal token "if") (if (and (not (smie-rule-bolp))
                                     (smie-rule-prev-p "else"))
                                (smie-rule-parent)))
        ((and (equal token "with") (smie-rule-parent-p "{"))
         (smie-rule-parent))
        ((and (equal token "with") (smie-rule-parent-p "d="))
         (let ((td (smie-backward-sexp "with")))
           (cl-assert (equal (nth 2 td) "d="))
           (goto-char (nth 1 td))
           (setq td (smie-backward-sexp "d="))
           ;; Presumably (equal (nth 1 td) "type").
           (goto-char (nth 1 td))
           `(column . ,(smie-indent-virtual))))
        ;; Align the "with" of "module type A = B \n with ..." w.r.t "module".
        ((and (equal token "m-with") (smie-rule-parent-p "d="))
         (save-excursion
           (smie-backward-sexp token)
           (goto-char (nth 1 (smie-backward-sexp 'halfsexp)))
           (cons 'column (+ 2 (current-column)))))
        ;; Treat purely syntactic block-constructs as being part of their
        ;; parent, when the opening statement is hanging.
        ((member token '("let" "(" "[" "{" "sig" "struct" "begin"))
         (when (and (smie-rule-hanging-p)
                    (apply #'smie-rule-prev-p
                           tuareg-smie--exp-operator-leader))
           (if (let ((openers '("{" "(" "{<" "[" "[|")))
                 (or (apply #'smie-rule-prev-p openers)
                     (not (apply #'smie-rule-parent-p openers))))
               (let ((offset (if (and (member token '("(" "struct" "sig"))
                                      (not (smie-rule-parent-p "let" "d-let")))
                                 0
                               tuareg-default-indent)))
                 (smie-rule-parent offset))
             ;; In "{ a = (", "{" and "a =" are not part of the same
             ;; syntax rule, so "(" is part of "a =" but not of the
             ;; surrounding "{".
             (save-excursion
               (smie-backward-sexp 'halfsexp)
               (cons 'column (smie-indent-virtual))))))
	((and tuareg-match-patterns-aligned
	      (equal token "|-or") (smie-rule-parent-p "|"))
	 (smie-rule-parent))
        ;; If we're looking at the first class-field-spec
        ;; in a "object(type)...end", don't rely on the default behavior which
        ;; will treat (type) as a previous element with which to align.
        ((tuareg-smie--object-hanging-rule token))
        ;; Apparently, people like their `| pattern when test -> body' to have
        ;;  the `when' indented deeper than the body.
        ((equal token "when") (smie-rule-parent tuareg-match-when-indent))))
      (`:after
       (cond
        ((equal token "d=")
         (and (smie-rule-parent-p "type")
              (not (smie-rule-next-p "["))
              0))
        ((equal token "->")
         (cond
          ((smie-rule-parent-p "with")
           ;; Align with "with" but only if it's the only branch (often
           ;; the case in try..with), since otherwise subsequent
           ;; branches can't be both indented well and aligned.
           (if (save-excursion
                 (and (not (equal "|" (nth 2 (smie-forward-sexp "|"))))
                      ;; Since we may misparse "if..then.." we need to
                      ;; double check that smie-forward-sexp indeed got us
                      ;; to the right place.
                      (equal (nth 2 (smie-backward-sexp "|")) "with")))
               (smie-rule-parent 2)
             ;; Align with other clauses, even with no preceding "|"
             tuareg-match-clause-indent))
          ((smie-rule-parent-p "function")
           ;; Similar to the previous rule but for "function"
           (if (save-excursion
                 (and (not (equal "|" (nth 2 (smie-forward-sexp "|"))))
                      (equal (nth 2 (smie-backward-sexp "|")) "function")))
               (smie-rule-parent tuareg-default-indent)
             tuareg-match-clause-indent))
          ((smie-rule-parent-p "|") tuareg-match-clause-indent)
          ;; Special case for "CPS style" code.
          ;; https://github.com/ocaml/tuareg/issues/5.
          ((smie-rule-parent-p "fun")
           (save-excursion
             (smie-backward-sexp "->")
             (if (eq ?\( (char-before))
                 (cons 'column
                       (+ tuareg-default-indent
                          (progn
                            (backward-char 1)
                            (smie-indent-virtual))))
               0)))
          (t 0)))
        ((equal token ":")
         (cond
          ((smie-rule-parent-p "val" "external") (smie-rule-parent 2))
          ((smie-rule-parent-p "module") (smie-rule-parent))
          (t 2)))
        ((equal token "in") tuareg-in-indent) ;;(if (smie-rule-hanging-p)
        ((equal token "with")
         (cond
          ;; ((smie-rule-next-p "|") 2)
          ((smie-rule-parent-p "{") nil)
          (t (+ 2 tuareg-with-indent))))
        ((or (member token '("." "t->" "]"))
             (consp (nth 2 (assoc token tuareg-smie-grammar)))) ;; Closer.
         nil)
        ((member token '("{" "("))
         ;; The virtual indent after ( can be higher than the actual one
         ;; because it might be "column + tuareg-default-indent", whereas
         ;; the token only occupies a single column.  So make sure we don't
         ;; get caught in this trap.
         (let ((vi (smie-indent-virtual)))
           (forward-char 1)             ;Skip paren.
           (skip-chars-forward " \t")
           (unless (eolp)
             `(column
               . ,(min (current-column)
                       (+ tuareg-default-indent vi))))))
        (t tuareg-default-indent)))))))

(defun tuareg-smie--with-module-fields-rule ()
  ;; Indentation of fields after "{ E with Module." where the "Module."
  ;; syntactically only applies to the first field, but has
  ;; semantically a higher position since it applies to all fields.
  (save-excursion
    (forward-char 1)
    (smie-backward-sexp 'halfsexp)
    (when (looking-at "\\(?:\\sw\\|\\s_\\)+\\.[ \t]*$")
      (smie-backward-sexp 'halfsexp)
      (cons 'column (current-column)))))

(defconst tuareg-smie--monadic-operators '(">>|" ">>=" ">>>" ">|=")
  "Monadic infix operators")

(defconst tuareg-smie--monadic-op-re
  (regexp-opt tuareg-smie--monadic-operators))

(defun tuareg-smie--monadic-rule (token)
  ;; When trying to indent a >>=, try to look back to find any earlier
  ;; >>= in a sequence of "monadic steps".
  (or (and (equal token ">…") (looking-at tuareg-smie--monadic-op-re)
           (save-excursion
             (tuareg-smie--forward-token)
             (let ((indent nil))
               (while
                   (let ((parent-data (smie-backward-sexp 'halfsexp)))
                     (cond
                      ((car parent-data) (member (nth 2 parent-data) '("->")))
                      ((member (nth 2 parent-data) '(";" "d=")) nil)
                      ((member (nth 2 parent-data) '("fun" "function"))
                       (if (member (tuareg-smie--backward-token)
                                   tuareg-smie--monadic-operators)
                           (progn
                             (setq indent (cons 'column
                                                (smie-indent-virtual)))
                             nil)
                         t)))))
               indent)))
      ;; In "foo >>= fun x -> bar" indent `bar' relative to `foo'.
      (and (member token '("fun" "function")) (not (smie-rule-bolp))
           (save-excursion
             (let ((prev (tuareg-smie-backward-token)))
               ;; FIXME: Should we use the same loop as above?
               (and (equal prev ">…") (looking-at tuareg-smie--monadic-op-re)
                    (progn (smie-backward-sexp prev)
                           (cons 'column (current-column)))))))))

(defun tuareg-smie--object-hanging-rule (token)
  ;; If we're looking at the first class-field-spec
  ;; in a "object(type)...end", don't rely on the default behavior which
  ;; will treat (type) as a previous element with which to align.
  (cond
   ;; An important role of this first condition is to call smie-indent-virtual
   ;; so that we get called back to compute the (virtual) indentation of
   ;; "object", thus making sure we get called back to apply the second rule.
   ((and (member token '("inherit" "val" "method" "constraint"))
         (smie-rule-parent-p "object"))
    (save-excursion
      (forward-word 1)
      (goto-char (nth 1 (smie-backward-sexp 'halfsexp)))
      (let ((col (smie-indent-virtual)))
        `(column . ,(+ tuareg-default-indent col)))))
   ;; For "class foo = object(type)...end", align object...end with class.
   ((and (equal token "object") (smie-rule-parent-p "class"))
    (smie-rule-parent))))

(defun tuareg-smie--if-then-hack (token)
  ;; Getting SMIE's parser to properly parse "if E1 then E2" is difficult, so
  ;; instead we live with a confused parser and try to work around the mess
  ;; here, although it clearly won't help other uses of the parser
  ;; (e.g. navigation).
  (save-excursion
    (let (pd)
      (while (equal (nth 2 (setq pd (smie-backward-sexp token))) "then")
        (let ((pdi (smie-backward-sexp 'halfsexp)))
          (cl-assert (equal (nth 2 pdi) "if"))))
      (cond
       ((equal (nth 2 pd) token)
        (goto-char (nth 1 pd))
        (cons 'column (smie-indent-virtual)))
       ((and (equal token "|") (equal (nth 2 pd) "with")
             (not (smie-rule-bolp)))
        (goto-char (nth 1 pd))
        (cons 'column (+ 3 (current-column))))
       (t (cons 'column (current-column)))))))

(defun tuareg-smie--inside-string ()
  (when (nth 3 (syntax-ppss))
    (save-excursion
      (goto-char (1+ (nth 8 (syntax-ppss))))
      (current-column))))

(defcustom tuareg-indent-align-with-first-arg nil
  "Non-nil if indentation should try to align arguments on the first one.
With a non-nil value you get

    let x = List.map (fun x -> 5)
                     my list

whereas with a nil value you get

    let x = List.map (fun x -> 5)
              my list"
  :type 'boolean)

(defun tuareg-smie--args ()
  ;; FIXME: This is largely copy&pasted from smie.el.  SMIE should offer a way
  ;; to hook into smie-indent-exps in order to control that behavior.
  (unless (or tuareg-indent-align-with-first-arg
              (nth 8 (syntax-ppss))
              (looking-at comment-start-skip)
              (looking-at "[ \t]*$") ;; bug#179
              (numberp (nth 1 (save-excursion (smie-indent-forward-token))))
              (numberp (nth 2 (save-excursion (smie-indent-backward-token)))))
    (save-excursion
      (let ((positions nil)
            arg)
        (while (and (null (car (smie-backward-sexp)))
                    (push (point) positions)
                    (not (smie-indent--bolp))))
        (save-excursion
          ;; Figure out if the atom we just skipped is an argument rather
          ;; than a function.
          (setq arg
                (or (null (car (smie-backward-sexp)))
                    (funcall smie-rules-function :list-intro
                             (funcall smie-backward-token-function)))))
        (cond
         ((null positions)
          ;; We're the first expression of the list.  In that case, the
          ;; indentation should be (have been) determined by its context.
          nil)
         (arg
          ;; There's a previous element, and it's not special (it's not
          ;; the function), so let's just align with that one.
          (goto-char (car positions))
          (if (fboundp 'smie-indent--current-column)
              (smie-indent--current-column)
            (current-column)))
         (t
          ;; There's no previous arg at BOL.  Align with the function.
          (goto-char (car positions))
          (+ (smie-indent--offset 'args)
             ;; We used to use (smie-indent-virtual), but that
             ;; doesn't seem right since it might then indent args less than
             ;; the function itself.
             (if (fboundp 'smie-indent--current-column)
                 (smie-indent--current-column)
               (current-column)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Phrase movements and indentation

(defun tuareg--skip-backward-comment ()
  "Skip backward a single comment and at most one preceding newline.
Return a non-nil value if a comment was skipped."
  ;; We do not want to skip newlines after the comment (as
  ;; `forward-comment'), so we skip spaces by hand and check we are at
  ;; the end of a comment.
  (skip-chars-backward " \t")
  (let ((opoint (point)))
    (if (and (char-equal (preceding-char) ?\)) (forward-comment -1))
        (progn
          (skip-chars-backward " \t")
          (skip-chars-backward "\n" (1- (point)))
          t)
      (goto-char opoint)
      nil)))

(defun tuareg--skip-backward-comments-semicolon ()
  "Skip 'sticky' comments and ';;' after a definition."
  ;; Comments after the definition not separated by a blank like
  ;; ("sticking") are considered part of the definition.
  (when (looking-at-p "[ \t]*(\\*")
    (skip-chars-backward " \t")
    (skip-chars-backward "\n" (1- (point))))
  (while (tuareg--skip-backward-comment))
  (skip-chars-backward " \t;"))

(defun tuareg--skip-forward-comment ()
  "Skip forward a single comment and at most one subsequent newline.
Return a non-nil value if a comment was skipped."
  (skip-chars-forward " \t")
  (let ((opoint (point)))
    (skip-chars-forward "\n" (1+ (point)))
    (skip-chars-forward " \t")
    (if (and (char-equal (following-char) ?\() (forward-comment 1))
        t
      (goto-char opoint)
      nil)))

(defun tuareg--skip-forward-comments-semicolon ()
  "Skip ';;' and then 'sticky' comments after a definition."
  (skip-chars-forward " \t;")
  (while (tuareg--skip-forward-comment)))

(defconst tuareg-starters-syms
  '("type" "d-let" "exception" "module" "class" "val" "external" "open"))

(defun tuareg-backward-beginning-of-defun ()
  "Move the point backward to the beginning of a definition.
Return the token starting the phrase (`nil' if it is an expression)."
  (let ((state (syntax-ppss)))
    (if (nth 3 state); in a string
        (goto-char (nth 8 state))
      ;; If on a word (e.g., "let" or "end"), move to the end of it.
      ;; In particular, even if at the beginning of the "let" of a
      ;; definition, one will not jump to the previous one.
      (or (/= (skip-syntax-forward "w_") 0)
          (tuareg--skip-backward-comments-semicolon))))
  (let (td tok
        (opoint (point)))
    (setq td (smie-backward-sexp ";;")); for expressions
    (cond
     ((and (car td) (member (nth 2 td) tuareg-starters-syms))
      (goto-char (nth 1 td)) (setq tok (nth 2 td)))
     ((and (car td) (string= (nth 2 td) ";;")))
     (t
      (goto-char opoint)
      (while (progn
               (setq td (smie-backward-sexp 'halfsexp))
               (cond
                ((and (car td)
                      (member (nth 2 td) tuareg-starters-syms))
                 (goto-char (nth 1 td)) (setq tok (nth 2 td)) nil)
                ((and (car td) (string= (nth 2 td) ";;"))
                 nil)
                ((and (car td) (not (numberp (car td))))
                 (unless (bobp)
                   (goto-char (nth 1 td))
                   ;; Make sure there is not a preceding ;;
                   (setq opoint (point))
                   (let ((tok (tuareg-smie-backward-token)))
                     (goto-char opoint)
                     (not (string= tok ";;")))))
                (t t))))))
    tok))

(defun tuareg--skip-double-semicolon ()
  (tuareg-skip-blank-and-comments)
  (when (looking-at ";;[ \t\n]*")
    (goto-char (match-end 0))))

(defun tuareg-end-of-defun ()
  "Assuming that we are at the beginning of a definition, move to its end.
See variable `end-of-defun-function'."
  (interactive)
  (let ((td (smie-forward-sexp ";;"))) ; for expressions
    (when (member (nth 2 td) tuareg-starters-syms)
      (smie-forward-sexp 'halfsexp)))
  (tuareg--skip-forward-comments-semicolon))

(defun tuareg-beginning-of-defun (&optional arg)
  "Move point backward to the beginning of a definition.
See variable `beginning-of-defun-function'."
  (interactive "^P")
  (unless arg (setq arg 1))
  (cond
   ((> arg 0)
    (while (and (> arg 0) (not (bobp)))
      (tuareg-backward-beginning-of-defun)
      (cl-decf arg)))
   (t
    (tuareg-backward-beginning-of-defun)
    (unless (bobp) (tuareg-end-of-defun))
    (while (and (< arg 0) (not (eobp)))
      (tuareg--skip-double-semicolon)
      (smie-forward-sexp 'halfsexp)
      (cl-incf arg))
    (tuareg-backward-beginning-of-defun)))
  t); whether an experssion or a def, we found something.

(defun tuareg-skip-siblings ()
  (while (and (not (bobp))
              (let ((td (smie-backward-sexp)))
                (or (null (car td))
                    (and (string= (nth 2 td) ";;")
                         (tuareg-smie-backward-token)))))
    (tuareg-backward-beginning-of-defun)
    (forward-comment (- (point))))
  (when (looking-at-p "in")
    ;; Skip over `local...in' and continue.
    (forward-word 1)
    (smie-backward-sexp 'halfsexp)
    (tuareg-skip-siblings)))

(defun tuareg--current-fun-name ()
  (when (tuareg-backward-beginning-of-defun)
    (save-excursion (tuareg-smie-forward-token)
                    (tuareg-skip-blank-and-comments)
                    (let ((name (tuareg-smie-forward-token)))
                      (if (not (member name '("rec" "type")))
                          name
                        (tuareg-skip-blank-and-comments)
                        (tuareg-smie-forward-token))))))

(defcustom tuareg-max-name-components 3
  "Maximum number of components to use for the current function name."
  :type 'integer)

(defun tuareg-current-fun-name ()
  (save-excursion
    (let ((count tuareg-max-name-components)
          fullname name)
      (end-of-line)
      (while (and (> count 0)
                  (setq name (tuareg--current-fun-name)))
        (cl-decf count)
        (setq fullname (if fullname (concat name "." fullname) name))
        ;; Skip all other declarations that we find at the same level.
        (tuareg-skip-siblings))
      fullname)))

(define-obsolete-function-alias 'tuareg--beginning-of-phrase
  'tuareg-backward-beginning-of-defun
  "Apr 10, 2019")

(defun tuareg-region-of-defun (&optional pos)
  "Return a couple (BEGIN . END) for the OCaml phrase around POS,
including comments after the phrase.  In case of error, move the
point at the beginning of the error and return `nil'."
  (let ((complete-phrase t)
        begin end)
    (save-excursion
      (if pos (goto-char pos))
      (tuareg-backward-beginning-of-defun)
      (setq begin (point))
      (tuareg-end-of-defun) ; OK as point is as beginning of defun
      (setq end (point))
      ;; Check if we were not stuck (after POS) because the phrase was
      ;; not well parenthesized.
      (when (and complete-phrase (< (point) (point-max)))
        (smie-forward-sexp 'halfsexp)
        (when (= end (point)); did not move
          (setq complete-phrase nil))))
    (if complete-phrase
        (cons begin end)
      (goto-char end)
      nil)))

(defun tuareg-discover-phrase (&optional pos)
  "Return a triplet (BEGIN END END-WITH-COMMENTS) for the OCaml
phrase around POS.  In case of error, move the point at the
beginning of the error and return `nil'."
  (let ((r (tuareg-region-of-defun pos))
        end-without-comments)
    (when r
      ;; Remove possible comments after the phrase.
      (goto-char (cdr r))
      (forward-comment (- (point)))
      (setq end-without-comments (point))
      (list (car r) end-without-comments (cdr r)))))

(defun tuareg--string-boundaries ()
  "Assume point is inside a string and return (START . END), the
positions delimiting the string (including its delimiters)."
  (save-excursion
    (let ((start (nth 8 (syntax-ppss)))
          end)
      (goto-char start)
      (smie-forward-sexp)
      (setq end (1- (point)))
      (cons start end))))

(defun tuareg--fill-string ()
  "Assume the point is inside a string delimited by \" and jusfify it.
This function moves the point."
  ;; FIXME: be more subtle: detect lists and @param
  (let* ((start-end (tuareg--string-boundaries))
         (start (set-marker (make-marker) (car start-end)))
         (end   (set-marker (make-marker) (cdr start-end)))
         fill-prefix
         (fill-individual-varying-indent t)
         (use-hard-newlines t))
    (indent-region (marker-position start) (marker-position end))
    ;; Delete all backslash protected newlines except those without
    ;; a preceding space that serve to cut a long word.
    (goto-char (marker-position start))
    ;;(indent-according-to-mode)
    (setq fill-prefix (make-string (1+ (current-column)) ?\ ))
    (if (looking-at "\"\\\\ *[\n\r] *")
        (replace-match "\""))
    (while (re-search-forward " +\\\\ *[\n\r] *" (marker-position end) t)
      (replace-match " "))
    (set-hard-newline-properties (marker-position start)
                                 (marker-position end))
    ;; Do not include the final \" not to remove space before it:
    (fill-region (marker-position start) (1- (marker-position end)))
    ;; Protect all soft newlines
    (goto-char (marker-position start))
    (end-of-line)
    (while (< (point) (marker-position end))
      (unless (get-char-property (point) 'hard)
        (insert " \\"))
      (forward-char)
      (end-of-line))
    (set-marker start nil)
    (set-marker end nil)))

(defun tuareg--fill-comment ()
  "Assumes the point is inside a comment and justify it.
This function moves the point."
  (let* ((start (set-marker (make-marker) (nth 8 (syntax-ppss))))
         (end (make-marker))
         fill-prefix
         (use-hard-newlines t))
    (goto-char (marker-position start))
    (indent-according-to-mode)
    (setq fill-prefix (make-string (+ 3 (current-column)) ?\ ))
    (forward-comment 1)
    (set-marker end (point))
    (goto-char (marker-position start))
    (let ((e (marker-position end)))
      (while (re-search-forward "\n\n" e t)
        (put-text-property (match-beginning 0) (match-end 0) 'hard 't)))
    (fill-region start end)
    (remove-text-properties (marker-position start) (marker-position end)
                            '(hard))
    (set-marker start nil)
    (set-marker end nil)))

(defun tuareg-indent-phrase ()
  "Depending of the context: justify and indent a comment,
or indent all lines in the current phrase."
  (interactive)
  (save-excursion
    (let ((ppss (syntax-ppss)))
      (cond
       ((equal ?\" (nth 3 ppss))
        (tuareg--fill-string))
       ((nth 4 ppss)
        (tuareg--fill-comment))
       (t (let ((phrase (tuareg-region-of-defun)))
            (if phrase
                (indent-region (car phrase) (cdr phrase)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Commenting

(defun tuareg--end-of-string-or-comment (state)
  "Return the end position of the comment or string given by STATE."
  (save-excursion
    (goto-char (nth 8 state))
    (if (nth 4 state) (comment-forward 1) (forward-sexp))
    (point)))

(defun tuareg-comment-or-uncomment-region (beg end &optional arg)
  "Replacement for `comment-or-uncomment-region' tailored for OCaml."
  (interactive "*r\nP")
  (comment-normalize-vars)
  (setq beg (save-excursion (goto-char beg)
                            (skip-chars-forward " \t\r\n")
                            (point)))
  (setq end (save-excursion (goto-char end)
                            (skip-chars-backward " \t\r\n")
                            (point)))
  (let (state pos)
    ;; Include the comment or string to which BEG possibly belongs
    (setq pos (nth 8 (syntax-ppss beg)))
    (if pos (setq beg pos))
    ;; Include the comment or string to which END possibly belongs
    (setq state (syntax-ppss end))
    (if (nth 8 state)
        (setq end (tuareg--end-of-string-or-comment state)))
    (if (comment-only-p beg end)
        (uncomment-region beg end arg)
      (comment-region beg end arg))))

(defun tuareg-comment-dwim (&optional arg)
  "Replacement for `comment-dwim' tailored for OCaml."
  (interactive "*P")
  (comment-normalize-vars)
  (if (use-region-p)
      (save-excursion
        (tuareg-comment-or-uncomment-region (region-beginning)
                                            (region-end) arg))
    (let ((state (syntax-ppss)))
      (cond
       ((nth 4 state)
        ;; Point inside a comment.  Uncomment just as if a region
        ;; inside the comment was active.
        (uncomment-region (nth 8 state)
                          (tuareg--end-of-string-or-comment state) arg))
       ((nth 3 state); Point inside a string.
        (comment-region (nth 8 state)
                        (tuareg--end-of-string-or-comment state) arg))
       ((looking-at-p "[ \t]*$"); at end of line and not in string
        (comment-dwim arg))
       (t (save-excursion
            (tuareg-comment-or-uncomment-region (line-beginning-position)
                                                (line-end-position) arg)))))))

(define-key tuareg-mode-map [?\C-c ?\C-\;] 'tuareg-comment-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              The major mode

(defalias 'tuareg-find-alternate-file 'ff-get-other-file)

(defun tuareg--switch-outside-build ()
  "If the current buffer refers to a file under a _build
directory and a corresponding file exists outside the _build
directory, propose the user to switch to it.  Return t if the
switch was made."
  (let ((fpath (buffer-file-name))
	(p nil)
        (in-build nil)
        base b)
    (when fpath
      ;; Inspired by `locate-dominating-file'.
      (setq fpath (abbreviate-file-name fpath))
      (setq base (file-name-nondirectory fpath))
      (setq fpath (file-name-directory fpath))
      (while (not (or in-build
                      (null fpath)
                      (string-match-p locate-dominating-stop-dir-regexp fpath)))
        (setq b (file-name-nondirectory (directory-file-name fpath)))
        (if (string= b "_build")
            (setq in-build t)
          (push (file-name-as-directory b) p)
          (if (equal fpath (setq fpath (file-name-directory
                                        (directory-file-name fpath))))
              (setq fpath nil))))
      (when in-build
	;; Make `fpath' the path without _build.
	(setq fpath (file-name-directory (directory-file-name fpath)))
        ;; jbuilder prefixes the path with a <context> dir, not ocamlbuild
        (let* ((context (pop p))
               (rel-fpath (concat (apply #'concat p) base))
               (alt0 (concat fpath rel-fpath)); jbuilder
               (alt1 (concat fpath context rel-fpath)); ocamlbuild
               (alt (if (file-readable-p alt0) alt0))
               (alt (or alt (if (file-readable-p alt1) alt1))))
          (if (and alt
                   (y-or-n-p "File in _build.  Switch to corresponding \
file outside _build? "))
              (progn
                (kill-buffer)
                (find-file alt)
                t)
            (read-only-mode)
            (message "File in _build.  C-x C-q to edit.")
            nil))))))

(defmacro tuareg--eval-when-macrop (f form)
  "Execute FORM but only when F is `fboundp' (because it's a macro).
If F is not bound yet, then keep the code un-expanded and perform the
expansion at run-time, if the run-time version of Emacs does know this macro."
  (declare (debug (symbolp body)) (indent 1))
  (if (fboundp f) form                  ;Macro expanded at compile-time.
    `(if (fboundp ',f) (eval ',form)))) ;Macro expanded at run-time.

(defun tuareg--hanging-eolp-advice ()
  "Recognize \"fun ..args.. ->\" at EOL as being hanging."
  (when (looking-at "fun\\_>")
    (smie-indent-forward-token)
    ;; We define a special "-dlpd-" token
    ;; ("-dummy-left-pattern-delimiter-") in the grammar
    ;; specifically so as to be able to make the right
    ;; call to smie-forward-sexp here.
    (if (equal "->" (nth 2 (smie-forward-sexp "-dlpd-")))
        (smie-indent-forward-token))))

(defun tuareg--common-mode-setup ()
  (setq-local syntax-propertize-function #'tuareg-syntax-propertize)
  (setq-local parse-sexp-ignore-comments t)
  (smie-setup tuareg-smie-grammar #'tuareg-smie-rules
              :forward-token #'tuareg-smie-forward-token
              :backward-token #'tuareg-smie-backward-token)
  (tuareg--eval-when-macrop add-function
    (when (boundp 'smie--hanging-eolp-function)
      ;; FIXME: As its name implies, smie--hanging-eolp-function
      ;; is not to be used by packages like us, but SMIE's maintainer
      ;; hasn't provided any alternative so far :-(
      (add-function :before (local 'smie--hanging-eolp-function)
                    #'tuareg--hanging-eolp-advice)))
  (add-hook 'smie-indent-functions #'tuareg-smie--args nil t)
  (add-hook 'smie-indent-functions #'tuareg-smie--inside-string nil t)
  (setq-local add-log-current-defun-function #'tuareg-current-fun-name)
  (setq prettify-symbols-alist
        (if tuareg-prettify-symbols-full
            (append tuareg-prettify-symbols-basic-alist
                    tuareg-prettify-symbols-extra-alist)
          tuareg-prettify-symbols-basic-alist))
  (setq prettify-symbols-compose-predicate
        #'tuareg--prettify-symbols-compose-p)
  (setq-local open-paren-in-column-0-is-defun-start nil)

  (add-hook 'completion-at-point-functions #'tuareg-completion-at-point nil t)

  (add-hook 'electric-indent-functions
            #'tuareg--electric-indent-predicate nil t)
  (add-hook 'post-self-insert-hook #'tuareg--electric-close-vector nil t))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . tuareg-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.eliomi?\\'" . tuareg-mode))
;;;###autoload(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"
;;;###autoload                ".annot" ".cmt" ".cmti"))
;;;###autoload  (add-to-list 'completion-ignored-extensions ext))

(defvar compilation-first-column)

(defvar compilation-error-screen-columns)

;;;###autoload
(define-derived-mode tuareg-mode prog-mode "Tuareg"
  "Major mode for editing OCaml code.

Dedicated to Emacs and XEmacs, version 21 and higher.  Provides
automatic indentation and compilation interface.  Performs font/color
highlighting using Font-Lock.  It is designed for OCaml but handles
Caml Light as well.

The Font-Lock minor-mode is used according to your customization
options.

You have better byte-compile tuareg.el.

For customization purposes, you should use `tuareg-mode-hook'
\(run for every file) or `tuareg-load-hook' (run once) and not patch
the mode itself.  You should add to your configuration file something like:
  (add-hook 'tuareg-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

`dot-emacs.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x ocamldebug' FILE starts the OCaml debugger ocamldebug on the executable
FILE, with input and output in an Emacs buffer named *ocamldebug-FILE*.

A Tuareg Interactive Mode to evaluate expressions in a REPL (aka toplevel) is
included.  Type `M-x tuareg-run-ocaml' or simply `M-x run-ocaml' or see
special-keys below.

Short cuts for the Tuareg mode:
\\{tuareg-mode-map}

Short cuts for interactions with the REPL:
\\{tuareg-interactive-mode-map}"

  (unless (tuareg--switch-outside-build)
    ;; Initialize the Tuareg menu
    (tuareg-build-menu)

    (setq-local paragraph-start (concat "^[ \t]*$\\|\\*)$\\|" page-delimiter))
    (setq-local paragraph-separate paragraph-start)
    (setq-local require-final-newline mode-require-final-newline)
    (setq-local comment-start "(* ")
    (setq-local comment-end " *)")
    (setq-local comment-start-skip "(\\*+[ \t]*")
    (setq-local comment-style 'multi-line)
    ;; `ocamlc' counts columns from 0, contrary to other tools which start at 1.
    (setq-local compilation-first-column 0)
    (setq-local compilation-error-screen-columns nil)
    ;; TABs should NOT be used in OCaml files:
    (setq indent-tabs-mode nil)
    (setq ff-search-directories '(".")
          ff-other-file-alist tuareg-other-file-alist)
    (tuareg--common-mode-setup)
    (tuareg--install-font-lock)
    (setq-local beginning-of-defun-function #'tuareg-beginning-of-defun)
    (setq-local end-of-defun-function #'tuareg-end-of-defun)

    (setq imenu-create-index-function #'tuareg-imenu-create-index)
    (run-mode-hooks 'tuareg-load-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Error processing

(require 'compile)

;; In some versions of Emacs, the regexps in
;; compilation-error-regexp-alist do not match the error messages when
;; the language is not English.  Hence we add a regexp.

(defconst tuareg--error-regexp
  "^ *\\(File \\(\"?\\)\\([^,\" \n\t<>]+\\)\\2, \
lines? \\([0-9]+\\)-?\\([0-9]+\\)?\
\\(?:, characters? \\([0-9]+\\)-?\\([0-9]+\\)?\\)?:\\)\
\\(?:\n[ \t]*\\(?:\\(?:[0-9]+ | .*\\|\\^+\\)\n[ \t]*\\)*\
\\(Warning\\(?: [0-9]+\\)?\\):\\)?"
  "Regular expression matching the error messages produced by ocamlc/ocamlopt.")

(when (boundp 'compilation-error-regexp-alist-alist)
  (push `(ocaml ,tuareg--error-regexp 3 (4 . 5) (6 . 7) (8) 1
                (8 font-lock-function-name-face))
        compilation-error-regexp-alist-alist))

(when (boundp 'compilation-error-regexp-alist)
  (push 'ocaml compilation-error-regexp-alist)

  (eval-after-load 'caml
    ;; caml-mode also changes `compilation-error-regexp-alist' with a
    ;; too simple regexp.  Make sure the one above comes first.
    #'(lambda()
        (setq compilation-error-regexp-alist
              (delete 'ocaml compilation-error-regexp-alist))
        (push 'ocaml compilation-error-regexp-alist))))


;; Wrapper around next-error.

;; itz 04-21-96 instead of defining a new function, use defadvice
;; that way we get our effect even when we do \C-x` in compilation buffer

;; smclaughlin 07-19-11 defadvice is to be avoided.  It makes debugging
;; much more difficult.  If you really want this behavior, write your
;; own next-error-function.  In particular, it breaks when omake is
;; used.

;; (defadvice next-error (after tuareg-next-error activate)
;;  "Read the extra positional information provided by the OCaml compiler.

;; Puts the point and the mark exactly around the erroneous program
;; fragment. The erroneous fragment is also temporarily highlighted if
;; possible."
;;  (when (eq major-mode 'tuareg-mode)
;;    (let ((beg nil) (end nil))
;;      (with-current-buffer compilation-last-buffer
;;        (save-excursion
;;          (goto-char (window-point (get-buffer-window (current-buffer) t)))
;;          (when (looking-at tuareg-error-chars-regexp)
;;            (setq beg (string-to-number (match-string-no-properties 1))
;;                  end (string-to-number (match-string-no-properties 2))))))
;;      (beginning-of-line)
;;      (when beg
;;        (setq beg (+ (point) beg) end (+ (point) end))
;;        (goto-char beg) (push-mark end t t)))))

(autoload 'ocaml-module-alist "caml-help")
(autoload 'ocaml-visible-modules "caml-help")
(autoload 'ocaml-module-symbols "caml-help")

(defun tuareg-completion-at-point ()
  (let ((beg (save-excursion (skip-syntax-backward "w_") (point)))
        (end (save-excursion (skip-syntax-forward "w_") (point)))
        (table
         (lambda (string pred action)
           (let ((dot (string-match-p "\\.[^.]*\\'" string))
                 ;; ocaml-module-symbols contains an unexplained call to
                 ;; pop-to-buffer within save-window-excursion.  Let's try and
                 ;; avoid it pops up a stupid frame.
                 (display-buffer-alist
                  (cons '("^\\*caml-help\\*$"
                          (display-buffer-reuse-window
                           display-buffer-pop-up-window)
                          (reusable-frames . nil); only the selected frame
                          (window-height . 0.25))
                        display-buffer-alist)))
             (if (eq (car-safe action) 'boundaries)
                 `(boundaries ,(if dot (1+ dot) 0)
                              ,@(string-match-p "\\." (cdr action)))
               (if (null dot)
                   (complete-with-action
                    action (apply #'append
                                  (mapcar (lambda (mod) (concat (car mod) "."))
                                          (ocaml-module-alist))
                                  (mapcar #'ocaml-module-symbols
                                          (ocaml-visible-modules)))
                    string pred)
                 (completion-table-with-context
                  (substring string 0 (1+ dot))
                  (ocaml-module-symbols
                   (assoc (substring string 0 dot) (ocaml-module-alist)))
                  (substring string (1+ dot)) pred action)))))))
    (unless (or (eq beg end)
                (not tuareg-with-caml-mode-p))
      (list beg end table))))


(autoload 'caml-complete "caml-help")

(defun tuareg-complete (arg)
  "Completes qualified ocaml identifiers."
  (interactive "p")
  (modify-syntax-entry ?_ "w" tuareg-mode-syntax-table)
  (unwind-protect
      (caml-complete arg)
    (modify-syntax-entry ?_ "_" tuareg-mode-syntax-table)))

(define-skeleton tuareg-insert-class-form
  "Insert a nicely formatted class-end form, leaving a mark after end."
  nil
  \n "class " @ " = object (self)" > \n
  "inherit " > _ " as super" \n "end;;" > \n)

(define-skeleton tuareg-insert-begin-form
  "Insert a nicely formatted begin-end form, leaving a mark after end."
  nil
  \n "begin" > \n _ \n "end" > \n)

(define-skeleton tuareg-insert-for-form
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  nil
  \n "for " - " do" > \n _ \n "done" > \n)

(define-skeleton tuareg-insert-while-form
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  nil
  \n "while " - " do" > \n _ \n "done" > \n)

(define-skeleton tuareg-insert-if-form
  "Insert a nicely formatted if-then-else form, leaving a mark after else."
  nil
  \n "if" > \n _ \n "then" > \n @ \n "else" \n @)

(define-skeleton tuareg-insert-match-form
  "Insert a nicely formatted math-with form, leaving a mark after with."
  nil
  \n "match" > \n _ \n "with" > \n)

(define-skeleton tuareg-insert-let-form
  "Insert a nicely formatted let-in form, leaving a mark after in."
  nil
  \n "let " > _ " in" > \n)

(define-skeleton tuareg-insert-try-form
  "Insert a nicely formatted try-with form, leaving a mark after with."
  nil
  \n "try" > \n _ \n "with" > \n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               OPAM

(require 'tuareg-opam)
(when (and tuareg-opam-insinuate tuareg-opam)
  (setq tuareg-interactive-program
        (concat tuareg-opam " config exec -- ocaml"))

  (advice-add 'compile :before #'tuareg--compile-opam)

  (defvar merlin-command)               ;Silence byte-compiler.
  (setq merlin-command 'opam)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Tuareg interactive mode

;; Augment Tuareg mode with an OCaml REPL.

(require 'comint)

(defvar tuareg-interactive-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-c\C-i" 'tuareg-interrupt-ocaml)
    (define-key map "\C-c\C-k" 'tuareg-kill-ocaml)
    (define-key map "\C-c`" 'tuareg-interactive-next-error-repl)
    (define-key map "\C-c?" 'tuareg-interactive-next-error-repl)
    (define-key map "\C-m" 'tuareg-interactive-send-input)
    (define-key map [(shift return)]
      'tuareg-interactive-send-input-end-of-phrase)
    (define-key map [(ctrl return)]
      'tuareg-interactive-send-input-end-of-phrase)
    (define-key map [kp-enter] 'tuareg-interactive-send-input-end-of-phrase)
    map))

(defconst tuareg-interactive-buffer-name "*OCaml*")

(defconst tuareg-interactive-error-range-regexp
  "[ \t]*Characters \\([0-9]+\\)-\\([1-9][0-9]*\\):\n"
  "Regexp matching the char numbers in OCaml REPL's error messages.")

(defconst tuareg-interactive-error-regexp
  "\n\\(Error: [^#]*\\)")

(defconst tuareg-interactive-exception-regexp
  "\\(Exception: [^#]*\\)")

(defvar tuareg-interactive-last-phrase-pos-in-source 0)

(defvar tuareg-interactive-last-phrase-pos-in-repl 0)

(defun tuareg-interactive-filter (_text)
  (when (eq major-mode 'tuareg-interactive-mode)
    (save-excursion
      (when (>= comint-last-input-end comint-last-input-start)
        (when tuareg-interactive-read-only-input
          (add-text-properties
           comint-last-input-start comint-last-input-end
           (list 'read-only t)))
        (when (and font-lock-mode tuareg-interactive-input-font-lock)
          (font-lock-fontify-region comint-last-input-start
                                    comint-last-input-end))
        (when tuareg-interactive-output-font-lock
          (save-excursion
            (goto-char (point-max))
            (re-search-backward comint-prompt-regexp
                                comint-last-input-end t)
            (add-text-properties
             comint-last-input-end (point)
             '(font-lock-face tuareg-font-lock-interactive-output-face))))
        (when tuareg-interactive-error-font-lock
          (save-excursion
            (goto-char comint-last-input-end)
            (cond
             ((looking-at tuareg-interactive-error-range-regexp)
              (let ((beg (string-to-number (match-string-no-properties 1)))
                    (end (string-to-number (match-string-no-properties 2))))
                (put-text-property
                 (+ comint-last-input-start beg)
                 (+ comint-last-input-start end)
                 'font-lock-face 'tuareg-font-lock-error-face))
              (goto-char comint-last-input-end)
              (when (re-search-forward tuareg-interactive-error-regexp nil t)
                (let ((errbeg (match-beginning 1))
                      (errend (match-end 1)))
                (put-text-property
                 errbeg errend
                 'font-lock-face 'tuareg-font-lock-interactive-error-face))))
             ((looking-at tuareg-interactive-exception-regexp)
              (let ((errbeg (match-beginning 1))
                    (errend (match-end 1)))
                (put-text-property
                 errbeg errend
                 'font-lock-face 'tuareg-font-lock-interactive-error-face)))
             )))))))

(easy-menu-define
  tuareg-interactive-mode-menu tuareg-interactive-mode-map
  "Tuareg Interactive Mode Menu."
  '("Tuareg"
    ("Interactive Mode"
     ["Run OCaml REPL" tuareg-run-ocaml t]
     ["Interrupt OCaml REPL" tuareg-interrupt-ocaml
      :active (comint-check-proc tuareg-interactive-buffer-name)]
     ["Kill OCaml REPL" tuareg-kill-ocaml
      :active (comint-check-proc tuareg-interactive-buffer-name)]
     ["Evaluate Region" tuareg-eval-region :active (region-active-p)]
     ["Evaluate Phrase" tuareg-eval-phrase t]
     ["Evaluate Buffer" tuareg-eval-buffer t])
    "---"
    ["Customize Tuareg Mode..." (customize-group 'tuareg) t]
    ("Tuareg Options" ["Dummy" nil t])
    ("Tuareg Interactive Options" ["Dummy" nil t])
    "---"
    ["About" tuareg-about t]
    ["Help" tuareg-interactive-help t]))

(define-derived-mode tuareg-interactive-mode comint-mode "Tuareg-Interactive"
  "Major mode for interacting with an OCaml process.
Runs an OCaml REPL as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in tuareg mode.

Short cuts for interactions with the REPL:
\\{tuareg-interactive-mode-map}"
  (add-hook 'comint-output-filter-functions #'tuareg-interactive-filter)
  (setq comint-prompt-regexp "^#  *")
  (setq comint-process-echoes nil)
  (setq comint-get-old-input 'tuareg-interactive-get-old-input)
  (setq comint-scroll-to-bottom-on-output
        tuareg-interactive-scroll-to-bottom-on-output)
  (set-syntax-table tuareg-mode-syntax-table)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comint-prompt-read-only t)

  (tuareg--common-mode-setup)
  (tuareg--install-font-lock t)
  (when (or tuareg-interactive-input-font-lock
            tuareg-interactive-output-font-lock
            tuareg-interactive-error-font-lock)
    (font-lock-mode 1))

  (easy-menu-add tuareg-interactive-mode-menu)
  (tuareg-update-options-menu))

;;;###autoload
(defun tuareg-run-ocaml ()
  "Run an OCaml REPL process.  I/O via buffer `*OCaml*'."
  (interactive)
  (tuareg-run-process-if-needed)
  (display-buffer tuareg-interactive-buffer-name))

;;;###autoload
(defalias 'run-ocaml 'tuareg-run-ocaml)

;;;###autoload
(add-to-list 'interpreter-mode-alist '("ocamlrun" . tuareg-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("ocaml" . tuareg-mode))

(defun tuareg-run-process-if-needed (&optional cmd)
  "Run an OCaml REPL process if needed, with an optional command name.
I/O via buffer `*OCaml*'."
  (if cmd
      (setq tuareg-interactive-program cmd)
    (unless (comint-check-proc tuareg-interactive-buffer-name)
      (setq tuareg-interactive-program
            (read-shell-command "OCaml REPL to run: "
                                tuareg-interactive-program))))
  (unless (comint-check-proc tuareg-interactive-buffer-name)
    (let ((cmdlist (tuareg--split-args tuareg-interactive-program))
          (process-connection-type t))
      (set-buffer (apply (function make-comint) "OCaml"
                         (car cmdlist) nil (cdr cmdlist)))
      (tuareg-interactive-mode)
      (sleep-for 1))))

(defun tuareg--split-args (args)
  (condition-case nil
      (split-string-and-unquote args)
      (error (progn
               (message "Arguments ‘%s’ ill quoted.  Ignored." args)
               nil))))

(defun tuareg-interactive-get-old-input ()
  (save-excursion
    (let ((end (point)))
      (re-search-backward comint-prompt-regexp (point-min) t)
      (when (looking-at comint-prompt-regexp)
        (re-search-forward comint-prompt-regexp))
      (buffer-substring-no-properties (point) end))))


(defconst tuareg-interactive--send-warning
  "Note: REPL processing requires a terminating `;;', or use S-return.")

(defun tuareg-interactive--indent-line ()
  (insert "\n")
  (indent-according-to-mode)
  (message tuareg-interactive--send-warning))

(defun tuareg-interactive-send-input ()
  "Send the current phrase to the OCaml REPL or insert a newline.
If the point is next to \";;\", the phrase is sent to the REPL,
otherwise a newline is inserted and the lines are indented."
  (interactive)
  (cond
   ((tuareg-in-literal-or-comment-p) (tuareg-interactive--indent-line))
   ((or (equal ";;" (save-excursion (nth 2 (smie-backward-sexp))))
        (looking-at-p "[ \t\n\r]*;;"))
    (comint-send-input))
   (t (tuareg-interactive--indent-line))))

(defun tuareg-interactive-send-input-end-of-phrase ()
  (interactive)
  (goto-char (point-max))
  (unless (equal ";;" (save-excursion (nth 2 (smie-backward-sexp))))
    (insert ";;"))
  (comint-send-input))

(defun tuareg-interactive--send-region (start end)
  "Send the region between START and END to the OCaml REPL.
It is assumed that the range START-END delimit valid OCaml phrases."
  (save-excursion (tuareg-run-process-if-needed))
  (comint-preinput-scroll-to-bottom)
  (let* ((phrases (buffer-substring-no-properties start end))
         (phrases (replace-regexp-in-string "[ \t\n]*\\(;;[ \t\n]*\\)?\\'" ""
                                            phrases))
         (phrases-semicolon (concat phrases ";;")))
    (if (string= phrases "")
        (message "Cannot send empty commands to OCaml REPL!")
      (with-current-buffer tuareg-interactive-buffer-name
        (goto-char (point-max))
        (setq tuareg-interactive-last-phrase-pos-in-repl (point))
        (comint-send-string tuareg-interactive-buffer-name phrases-semicolon)
        (let ((pos (point)))
          (comint-send-input)
          (when tuareg-interactive-echo-phrase
            (save-excursion
              (goto-char pos)
              (insert phrases-semicolon)))))))
  (when tuareg-display-buffer-on-eval
    (display-buffer tuareg-interactive-buffer-name)))

(defun tuareg-eval-region (start end)
  "Eval the current region in the OCaml REPL."
  (interactive "r")
  (setq tuareg-interactive-last-phrase-pos-in-source start)
  (save-excursion
    (goto-char start)
    (tuareg-backward-beginning-of-defun)
    (setq start (point))
    (setq end (cdr (tuareg-region-of-defun end)))
    (if end
        (tuareg-interactive--send-region start end)
      (message "The expression after the point is not well braced."))))

(define-obsolete-function-alias 'tuareg-narrow-to-phrase 'narrow-to-defun
  "Apr 10, 2019")

(defun tuareg-eval-phrase ()
  "Eval the surrounding OCaml phrase (or block) in the OCaml REPL."
  (interactive)
  (let ((opoint (point)))
    ;; Move before the comment, if we are in one.
    (let ((ppss (syntax-ppss)))
      (if (nth 4 ppss) (goto-char (- (nth 8 ppss) 1))))
    (let ((phrase (tuareg-discover-phrase)))
      (if phrase
          (progn
            (tuareg-interactive--send-region (car phrase) (cadr phrase))
            (if tuareg-skip-after-eval-phrase
              (progn (goto-char (caddr phrase))
                     (tuareg-skip-blank-and-comments))
              (goto-char opoint)))
        (message "The expression after the point is not well braced.")))))

(defun tuareg-eval-buffer ()
  "Send the buffer to the Tuareg Interactive process."
  (interactive)
  (tuareg-interactive--send-region (point-min) (point-max)))

(defvar tuareg-interactive-next-error-olv (make-overlay 1 1))

(overlay-put tuareg-interactive-next-error-olv
             'face 'tuareg-font-lock-error-face)

(delete-overlay tuareg-interactive-next-error-olv)

(defun tuareg-interactive-next-error-source ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (with-current-buffer tuareg-interactive-buffer-name
      (goto-char tuareg-interactive-last-phrase-pos-in-repl)
      (setq error-pos
            (re-search-forward tuareg-interactive-error-range-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (match-string-no-properties 1))
              end (string-to-number (match-string-no-properties 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ tuareg-interactive-last-phrase-pos-in-source beg)
            end (+ tuareg-interactive-last-phrase-pos-in-source end))
      (goto-char beg)
      (move-overlay tuareg-interactive-next-error-olv beg end)
      (unwind-protect
          (sit-for 60 t)
        (delete-overlay tuareg-interactive-next-error-olv))
      )))

(defun tuareg-interactive-next-error-repl ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (goto-char tuareg-interactive-last-phrase-pos-in-repl)
      (setq error-pos
            (re-search-forward tuareg-interactive-error-range-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (match-string-no-properties 1))
              end (string-to-number (match-string-no-properties 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ tuareg-interactive-last-phrase-pos-in-repl beg)
            end (+ tuareg-interactive-last-phrase-pos-in-repl end))
      (move-overlay tuareg-interactive-next-error-olv beg end)
      (unwind-protect
          (sit-for 60 t)
        (delete-overlay tuareg-interactive-next-error-olv))
      (goto-char beg))))

(defun tuareg-interrupt-ocaml ()
  (interactive)
  (when (comint-check-proc tuareg-interactive-buffer-name)
    (with-current-buffer tuareg-interactive-buffer-name
      (comint-interrupt-subjob))))

(defun tuareg-kill-ocaml ()
  (interactive)
  (when (comint-check-proc tuareg-interactive-buffer-name)
    (with-current-buffer tuareg-interactive-buffer-name
      (comint-kill-subjob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Menu support

(defun tuareg-about ()
  (interactive)
  (describe-variable 'tuareg-mode-version))

(defun tuareg-short-cuts ()
  "Short cuts for the Tuareg mode:
\\{tuareg-mode-map}

Short cuts for interaction within the REPL:
\\{tuareg-interactive-mode-map}"
  (interactive)
  (describe-function 'tuareg-short-cuts))

(defun tuareg-help ()
  (interactive)
  (describe-function 'tuareg-mode))

(defun tuareg-interactive-help ()
  (interactive)
  (describe-function 'tuareg-interactive-mode))

(defun tuareg-build-menu ()
  (easy-menu-define
   tuareg-mode-menu (list tuareg-mode-map)
   "Tuareg Mode Menu."
   '("Tuareg"
     ("Interactive Mode"
      ["Run OCaml REPL" tuareg-run-ocaml t]
      ["Interrupt OCaml REPL" tuareg-interrupt-ocaml
       :active (comint-check-proc tuareg-interactive-buffer-name)]
      ["Kill OCaml REPL" tuareg-kill-ocaml
       :active (comint-check-proc tuareg-interactive-buffer-name)]
      ["Evaluate Region" tuareg-eval-region
       ;; Region-active-p for XEmacs and mark-active for Emacs
       :active mark-active]
      ["Evaluate Phrase" tuareg-eval-phrase t]
      ["Evaluate Buffer" tuareg-eval-buffer t])
     ("OCaml Forms"
      ["try .. with .." tuareg-insert-try-form t]
      ["match .. with .." tuareg-insert-match-form t]
      ["let .. in .." tuareg-insert-let-form t]
      ["if .. then .. else .." tuareg-insert-if-form t]
      ["while .. do .. done" tuareg-insert-while-form t]
      ["for .. do .. done" tuareg-insert-for-form t]
      ["begin .. end" tuareg-insert-begin-form t])
     ["Switch .ml/.mli" tuareg-find-alternate-file t]
     "---"
     ["Compile..." compile t]
     ["Reference Manual..." tuareg-browse-manual t]
     ["OCaml Library..." tuareg-browse-library t]
     "---"
     [ "Show type at point" caml-types-show-type
       tuareg-with-caml-mode-p]
     [ "Show fully qualified ident at point" caml-types-show-ident
       tuareg-with-caml-mode-p]
     [ "Show the kind of call at point" caml-types-show-call
       tuareg-with-caml-mode-p]
     "---"
     [ "Complete identifier" caml-complete
       tuareg-with-caml-mode-p]
     [ "Help for identifier" caml-help
       tuareg-with-caml-mode-p]
     [ "Add path for documentation" ocaml-add-path
       tuareg-with-caml-mode-p]
     [ "Open module for documentation" ocaml-open-module
       tuareg-with-caml-mode-p]
     [ "Close module for documentation" ocaml-close-module
       tuareg-with-caml-mode-p]
     "---"
     ["Customize Tuareg Mode..." (customize-group 'tuareg) t]
     ("Tuareg Options" ["Dummy" nil t])
     ("Tuareg Interactive Options" ["Dummy" nil t])
     "---"
     ["About" tuareg-about t]
     ["Short Cuts" tuareg-short-cuts]
     ["Help" tuareg-help t]))
  (easy-menu-add tuareg-mode-menu)
  (tuareg-update-options-menu))

(defun tuareg-toggle-option (symbol)
  (interactive)
  (set symbol (not (symbol-value symbol)))
  (tuareg-update-options-menu))

(defun tuareg-update-options-menu ()
  (easy-menu-change
   '("Tuareg") "Tuareg Options"
   (mapcar (lambda (pair)
             (if (consp pair)
                 (vector (car pair)
                         (list 'tuareg-toggle-option (cdr pair))
                         ':style 'toggle
                         ':selected (nth 1 (cdr pair))
                         ':active t)
               pair))
           tuareg-options-list))
  (easy-menu-change
   '("Tuareg") "Tuareg Interactive Options"
   (mapcar (lambda (pair)
             (if (consp pair)
                 (vector (car pair)
                         (list 'tuareg-toggle-option (cdr pair))
                         ':style 'toggle
                         ':selected (nth 1 (cdr pair))
                         ':active t)
               pair))
           tuareg-interactive-options-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Manual

;; From M. Quercia

(defun tuareg-browse-manual ()
  "*Browse OCaml reference manual."
  (interactive)
  (setq tuareg-manual-url (read-from-minibuffer "URL: " tuareg-manual-url))
  (funcall tuareg-browser tuareg-manual-url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Library

;; From M. Quercia

(defvar tuareg-library-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [return] 'tuareg-library-find-file)
    (define-key map [mouse-2] 'tuareg-library-mouse-find-file)
    map))

(defun tuareg-browse-library ()
  "Browse the OCaml library."
  (interactive)
  (let ((buf-name "*ocaml-library*") (opoint)
        (dir (read-from-minibuffer "Library path: " tuareg-library-path)))
    (when (and (file-directory-p dir) (file-readable-p dir))
      (setq tuareg-library-path dir)
      ;; List *.ml and *.mli files
      (with-output-to-temp-buffer buf-name
        (buffer-disable-undo standard-output)
        (with-current-buffer buf-name
          (kill-all-local-variables)
          (setq-local tuareg-library-path dir)
          ;; Help
          (insert "Directory \"" dir "\".\n")
          (insert "Select a file with middle mouse button or RETURN.\n\n")
          (insert "Interface files (.mli):\n\n")
          (insert-directory (concat dir "/*.mli") "-C" t nil)
          (insert "\n\nImplementation files (.ml):\n\n")
          (insert-directory (concat dir "/*.ml") "-C" t nil)
          ;; '.', '-' and '_' are now letters
          (modify-syntax-entry ?. "w")
          (modify-syntax-entry ?_ "w")
          (modify-syntax-entry ?- "w")
          ;; Every file name is now mouse-sensitive
          (goto-char (point-min))
          (while (< (point) (point-max))
            (re-search-forward "\\.ml.?\\>")
            (setq opoint (point))
            (re-search-backward "\\<" (point-min) 1)
            (put-text-property (point) opoint 'mouse-face 'highlight)
            (goto-char (+ 1 opoint)))
          ;; Activate tuareg-library mode
          (setq major-mode 'tuareg-library-mode)
          (setq mode-name "tuareg-library")
          (use-local-map tuareg-library-mode-map)
          (setq buffer-read-only t))))))

(defun tuareg-library-find-file ()
  "Load the file whose name is near point."
  (interactive)
  (when (text-properties-at (point))    ;FIXME: Why??
    (save-excursion
      (let (beg)
        (re-search-backward "\\<") (setq beg (point))
        (re-search-forward "\\>")
        (find-file-read-only (expand-file-name (buffer-substring-no-properties
                                                beg (point))
                                               tuareg-library-path))))))

(defun tuareg-library-mouse-find-file (event)
  "Visit the file name you click on."
  (interactive "e")
  (let ((owindow (selected-window)))
    (mouse-set-point event)
    (tuareg-library-find-file)
    (select-window owindow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			    Imenu support

(defun tuareg-imenu-create-index ()
  "Create an index alist for OCaml files using `merlin-imenu' or `caml-mode'.
See `imenu-create-index-function'."
  (or (require 'merlin-imenu nil t)
      (let (abbrevs-changed)            ;Workaround for tuareg#146
        (require 'caml nil t)))
  (cond
   ((fboundp 'merlin-imenu-create-index)
    (merlin-imenu-create-index))
   ((fboundp 'caml-create-index-function)
    (caml-create-index-function))
   (t
    (message "Install Merlin or caml-mode.")
    ;; Cannot return the empty list `nil' because imenu will issue its
    ;; own warning.
    '(("Install Merlin or caml-mode" . 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Related files & modes

(eval-when-compile
  (autoload 'speedbar-add-supported-extension "speedbar")
  (defvar speedbar-obj-alist))

(when (require 'speedbar nil t)
  (speedbar-add-supported-extension
   '(".ml" ".mli" ".mll" ".mly" ".mlp" ".ls"))
  (push '("\\.mli$" . ".cmi") speedbar-obj-alist)
  (push '("\\.ml$"  . ".cmo") speedbar-obj-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Hooks and Exit

(provide 'tuareg)

;;; tuareg.el ends here
