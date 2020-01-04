;;; ess-s-lang.el --- Support for editing S source code  -*- lexical-binding: t; -*-

;; Copyright (C) 1989-1997 D. Bates, Kademan, Ritter, D.M. Smith, K. Hornik,
;;      R.M. Heiberger, M. Maechler, and A.J. Rossini.
;; Copyright (C) 1998-2015 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 26 Aug 1997
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS (Emacs Speaks Statistics).

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/

;;; Commentary:

;; Code for general editing S source code (specializes to S, S+, R).

;;; Code:

 ; Requires and autoloads

(require 'ess-mode)
(require 'ess-help)
(require 'ess-inf)

(declare-function speedbar-add-supported-extension "speedbar" (extension))

 ; Configuration variables

(defvar S-syntax-table
  (let ((S-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" S-syntax-table)
    (modify-syntax-entry ?+  "."  S-syntax-table)
    (modify-syntax-entry ?-  "."  S-syntax-table)
    (modify-syntax-entry ?=  "."  S-syntax-table)
    (modify-syntax-entry ?%  "."  S-syntax-table)
    (modify-syntax-entry ?<  "."  S-syntax-table)
    (modify-syntax-entry ?>  "."  S-syntax-table)
    (modify-syntax-entry ?&  "."  S-syntax-table)
    (modify-syntax-entry ?|  "."  S-syntax-table)
    (modify-syntax-entry ?\' "\"" S-syntax-table)
    (modify-syntax-entry ?\" "\"" S-syntax-table)
    (modify-syntax-entry ?#  "<"  S-syntax-table) ; open comment
    (modify-syntax-entry ?\n ">"  S-syntax-table) ; close comment
    ;;(modify-syntax-entry ?.  "w"  S-syntax-table) ; "." used in S obj names
    (modify-syntax-entry ?.  "_"  S-syntax-table) ; see above/below,
                                        ; plus consider separation.
    (modify-syntax-entry ?$  "_"  S-syntax-table); foo$comp = 1 symbol(completion)
    (modify-syntax-entry ?@  "_"  S-syntax-table); foo@slot = 1 symbol(completion)
    (modify-syntax-entry ?_  "_"  S-syntax-table)
    (modify-syntax-entry ?:  "_"  S-syntax-table)
    (modify-syntax-entry ?*  "."  S-syntax-table)
    (modify-syntax-entry ?<  "."  S-syntax-table)
    (modify-syntax-entry ?>  "."  S-syntax-table)
    (modify-syntax-entry ?/  "."  S-syntax-table)
    S-syntax-table)
  "Syntax table for S code."
  )

(defvar S-editing-alist
  '((paragraph-start              . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-separate           . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    ;;(comment-indent-function  . 'S-comment-indent)
    ;;(ess-comment-indent           . 'S-comment-indent)
    ;;(ess-calculate-indent           . 'ess-calculate-indent)
    ;;(ess-keep-dump-files          . 'ask)
    ;; For Changelog add, require ' ' before <- : "attr<-" is a function name :
    (add-log-current-defun-header-regexp . "^\\(.+\\)\\s-+<-[ \t\n]*function"))
  "General options for S and S+ source files.")

(defvar inferior-S-language-start
  '(concat "options("
           "STERM='"    ess-STERM  "'"
           ", str.dendrogram.last=\"'\""
           (if ess-editor (concat ", editor='" ess-editor "'"))
           (if ess-pager  (concat ", pager='"  ess-pager  "', help.pager='"  ess-pager  "'"))
           ", show.error.locations=TRUE"
           ")")
  "S language expression for startup -- default for all S dialects.")

(defconst S-common-cust-alist
  '((ess-language                  . "S")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-language-start   . (eval inferior-S-language-start))
    (comint-use-prompt-regexp      . t)  ;;use fields if nil
    (comint-process-echoes	   . t)
    ;; these prompt are the same for all S-languages As long as custom prompt
    ;; ends in inferior-ess-primary-prompt everything should work as expected.
    (inferior-ess-primary-prompt   . "> ")
    ;; (inferior-ess-secondary-prompt . "[+:] ") ;; catch Selection: and alike
    (inferior-ess-secondary-prompt . "+ ") ;; catch Selection: and alike
    (comment-start                . "#")
    (comment-add                  . 1)
    (comment-start-skip           . "#+ *")
    (comment-use-syntax           . t)  ; see log for bug report 2013-06-07
    (comment-column               . 40)
    (ess-no-skip-regexp           . (concat "^ *@\\|" (default-value 'ess-no-skip-regexp)))
    ;; inferior-ess-prompt is used by comint for navigation, only if
    ;; comint-use-prompt-regexp is t; (transcript-mode also relies on this regexp)
    (inferior-ess-prompt           . inferior-S-prompt)
    (ess-getwd-command          . "getwd()\n")
    (ess-setwd-command          . "setwd('%s')\n")
    (ess-funargs-command        . ".ess_funargs(\"%s\")\n")
    (fill-nobreak-predicate     . 'ess-inside-string-p)
    (ess-execute-screen-options-command . "options(width=%d, length=99999)\n")
    (font-lock-defaults           . '(ess-build-font-lock-keywords
                                      nil nil ((?\. . "w") (?\_ . "w")))))
  "S-language common settings for all <dialect>-customize-alist.")

(defconst S+common-cust-alist
  (append
   '((ess-suffix                . "S")
     (ess-help-sec-regex        . ess-help-S+-sec-regex)
     (ess-help-sec-keys-alist   . ess-help-S+sec-keys-alist)
     (ess-change-sp-regexp      . ess-S+-change-sp-regexp)
     (ess-function-pattern      . ess-s-function-pattern)
     (ess-function-template     . " <- \n#\nfunction()\n{\n\n}\n")
     (ess-dump-filename-template . (replace-regexp-in-string
                                    "S$" ess-suffix ; in the one from custom:
                                    ess-dump-filename-template-proto))
     (ess-traceback-command     . "traceback()\n")
     (ess-mode-editing-alist    . S-editing-alist)

     (ess-dumped-missing-re
      . "\\(\\(<-\\|=\\)\nDumped\n\\'\\)\\|\\(\\(<-\\|=\\)\\(\\s \\|\n\\)*\\'\\)")
     (ess-syntax-error-re
      . "\\(Syntax error: .*\\) at line \\([0-9]*\\), file \\(.*\\)$")
     (inferior-ess-objects-command  . inferior-Splus-objects-command)
     (ess-describe-object-at-point-commands . 'ess-S-describe-object-at-point-commands)
     (ess-editor . S-editor)
     (ess-pager  . S-pager))
   S-common-cust-alist)
  "Common settings for all S+<*>-customize-alist."
  )

;;; Changes from S to S-PLUS 3.x.  (standard S3 should be in ess-s-lang!).

(defconst ess-help-S+sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?d . "DESCRIPTION:")
    (?D . "DETAILS:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
    (?O . "OPTIONAL ARGUMENTS:")
    (?R . "REQUIRED ARGUMENTS:")
    (?r . "REFERENCES:")
    (?s . "SEE ALSO:")
    (?S . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Alist of (key . string) pairs for use in section searching.")
;;; `key' indicates the keystroke to use to search for the section heading
;;; `string' in an S help file. `string' is used as part of a
;;; regexp-search, and so specials should be quoted.

;; S ver.3 (NOT S-Plus)
(defconst ess-help-S3-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?d . "DESCRIPTION:")
    (?D . "DETAILS:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
    (?r . "REFERENCES:")
    (?s . "SEE ALSO:")
    (?S . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Help section keys for S ver.3.")

;; S ver.4 (NOT S-Plus)
(defconst ess-help-S4-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?d . "DESCRIPTION:")
    (?D . "DETAILS:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
    (?r . "REFERENCES:")
    (?s . "SEE ALSO:")
    (?S . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Help section keys for S4.")


(defconst ess-help-S+-sec-regex "^[A-Z.]+:$"
  "Reg(ular) Ex(pression) of section headers in help file.")

 ; Function Definitions

(defun S-comment-indent ()
  "Indentation for S comments."
  (if (or (looking-at "###")
          (and (looking-at "#!") (= 1 (line-number-at-pos))))
      (current-column)
    (if (looking-at "##")
        (let ((tem (when ;; FIXME ess-calculate-indent is R specific
                       (fboundp 'ess-calculate-indent)
                     (ess-calculate-indent))))
          (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

;;*;; S/R  Pretty-Editing

(defun ess-fix-comments (&optional dont-query verbose)
  "Fix buffer so that single-line comments start with at least '##',
and ensure space before subsequent text."
  (interactive "P")
  (ess-replace-regexp-dump-to-src "#\\([A-Za-z0-9]\\)" "# \\1" nil verbose)
  (ess-replace-regexp-dump-to-src "^\\([ \t]*#\\)\\([^#]\\)"
                                  "\\1#\\2" dont-query verbose))

(defun ess-dump-to-src (&optional dont-query verbose)
  "Make the change in an S - dump() file to improve human readability.
Optional arguments DONT-QUERY and VERBOSE are passed to
`ess-replace-regexp-dump-to-src'."
  (interactive "P")
  (ess-replace-regexp-dump-to-src  "^\"\\([a-z.][a-z.0-9]*\\)\" *<-\n"
                                   "\n\\1 <- "
                                   dont-query verbose))

(defun ess-num-var-round (&optional dont-query verbose)
  "Round endings like 000000 and 99999.
Optional argument DONT-QUERY means do not query.
Optional argument VERBOSE gives more verbose output."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (let ((num 0)
          (str "")
          (rgxp "000000+[1-9]?[1-9]?\\>")
          (to   ""))
      (if dont-query
          (ess-rep-regexp     rgxp to nil nil verbose)
        (query-replace-regexp rgxp to nil))
      (while (< num 9)
        (setq str (concat (int-to-string num) "999999+[0-8]*"))
        (if (and (numberp verbose) (> verbose 1))
            (message (format "\nregexp: '%s'" str)))
        (goto-char (point-min))
        (ess-rep-regexp str (int-to-string (1+ num))
                        'fixedcase 'literal verbose)
        (setq num (1+ num))))))

(defun ess-fix-dot (before-chars &optional dont-query verbose)
  "Remove trailing decimal '.' (\"dot\"), before BEFORE-CHARS.
Optional argument DONT-QUERY and VERBOSE get passed to `ess-replace-regexp-dump-to-src'."
  ;; typically, before-chars =  "]:" or more
  (ess-replace-regexp-dump-to-src
   (concat "\\([0-9]\\)\\.\\( *[" before-chars "]\\)")
   ;;           111      ^
   "\\1\\2" dont-query verbose))

(defun ess-fix-dot-1 (&optional do-query verbose)
  "Remove trailing decimal '.' (\"dot\"), before ':' or ']', i.e.,
in cases where it's ugly and nonsense.  DO-QUERY(prefix) asks before replacing."
  (interactive "P")
  (ess-fix-dot "]:" (not do-query) verbose))

(defun ess-fix-dot-more (&optional dont-query verbose)
  "Remove trailing decimal '.' (\"dot\", typically from S+) in more cases
than `ess-fix-dot-1'."
  (interactive "P")
  (ess-fix-dot-1 nil verbose)
  (ess-fix-dot ",)" dont-query verbose))

(defun ess-fix-EQ-assign (&optional dont-query verbose not-all)
  "Replace \"=\" by \"<-\" in places where it 'might make sense', e.g.,
for function assignments and lines not ending in \",\".
Be *careful* for list()s of functions and when argument not-all is
nil (as by default) !"
  ;;TODO: "in the few places we can be very sure.."
  ;;---- is hard in general: local functions: ok; but functions in
  ;;  list(a = function(x) abs(x), b= function(y) bound(y))  *NOT* ok!
  (interactive "P")
  (ess-replace-regexp-dump-to-src
   "^\\( *[a-z.][_a-z.0-9]*\\) *= *\\(function *(\\)"
   "\\1 <- \\2" dont-query verbose)
  (unless not-all
    ;; "too" aggressive {proposing to replace function argument specs}:
    (ess-replace-regexp-dump-to-src ;; all those *not* ending in ","
     ;; including  Mat[ i, ] = ...,
     ;; but not `names(x) = "..."' for that is "confused" with plot(x=x,..)
     "^\\( *[a-z.][][, \"_a-z.0-9]*\\) *= *\\([a-z.0-9({]\\(.*[^,]\\)? *$\\)"
     "\\1 <- \\2" nil ;; always query - often has many "false positives"
     verbose)))

;;; All of the above three :
(defun ess-MM-fix-src (&optional dont-query verbose)
  "Clean up ess-source code which has been produced by dump(..), and other
code typically produced by other tools.  Produces more readable code,
and one that is well formatted in Emacs ess-mode."
  (interactive "P")
  ;; each of the following does a save-excursion:
  (ess-dump-to-src dont-query)
  (ess-fix-comments dont-query)
  (ess-num-var-round dont-query verbose)
  (ess-fix-dot-more dont-query verbose)
  (ess-fix-EQ-assign dont-query verbose 'not-all))

(defun ess-fix-miscellaneous (&optional from verbose)
  "Fix Miscellaneous S/R `ill-formation's from current \\[point].
Particularly use \"<-\"and put spaces around operators."
  (interactive "d\nP"); Defaults: point and prefix (C-u)
  ;; activate by (setq ess-verbose t)
  (ess-if-verbose-write
   (format "ess-fix-misc begin (from = %s, verbose = %s)\n" from verbose))
  (save-excursion

    (when (and (string= ess-dialect "R")
               (fboundp 'ess-r-fix-T-F))
      (ess-r-fix-T-F from (not verbose)))

    ;; activate by (setq ess-verbose t)
    (ess-if-verbose-write "ess-fix-misc: after fix-T-F\n");___D___

    ;; former C and matlab programmers leave trailing  ";" :
    ;; (goto-char from) (ess-rep-regexp "; *$" "" nil 'literal verbose)
    ;; (ess-if-verbose-write "ess-fix-misc: after trailing ';'\n");___D___
    (goto-char from) (ess-rep-regexp ";\\( *\\)#" "\\1#" nil nil verbose)
    (ess-if-verbose-write "ess-fix-misc: after ';' before #\n");___D___

    ;;from R 1.9.x "_" is valid in names; here assume no initial / trailing '_'
    ;; BUG: The following changes "beta_ " or " _abc"
    ;; (goto-char from) (ess-rep-regexp " +_ *" " <- " nil 'literal verbose)
    ;; (goto-char from) (ess-rep-regexp   "_ +" " <- " nil 'literal verbose)

    (ess-if-verbose-write "ess-fix-misc: before 'around \"<-\"' :\n");___D___
    ;; ensure space around  "<-"  ---- but only replace if necessary:
    (goto-char from)
    (ess-rep-regexp "\\([^< \t\n]\\)\\(<<?-\\)" "\\1 \\2" nil nil verbose)
    (goto-char from)(ess-rep-regexp "<-\\([^ \t\n]\\)" "<- \\1" nil nil verbose)
    ;; ensure space around  "<" (not in "<-","<=","<<-")  and ">" (not ">=") :
    (goto-char from);; --> " <", care with "->":
    (ess-rep-regexp "\\([^-< \t\n]\\)\\([<>]\\)" "\\1 \\2" nil nil verbose)
    ;; ">" -> "> " , for "<", don't split "<-" nor "<<-":
    (goto-char from)
    (ess-rep-regexp "\\(>=?\\)\\([^= \t\n]\\)" "\\1 \\2" nil nil verbose)
    (goto-char from)
    (ess-rep-regexp "\\(<=?\\)\\([^-<= \t\n]\\)" "\\1 \\2" nil nil t)

    (ess-if-verbose-write "ess-fix-misc: before \"=\" \"==\" .. :\n");___D___
    ;; -- ensure space around "=", "==", "!=" :
    (goto-char from) ;; --> " ="
    (ess-rep-regexp "\\([^=!<> ]\\)\\([=!]?\\)=" "\\1 \\2=" nil nil verbose)
    (goto-char from) (ess-rep-regexp "=\\([^= ]\\)" "= \\1" nil nil verbose)

    (goto-char from) ;; add a space between "{" and surrounding ..char:
    (ess-rep-regexp "{\\([.A-Za-z()]\\)" "{ \\1" 'fix nil verbose)
    (ess-rep-regexp "\\([()]\\){" "\\1 {" 'fix nil verbose)
    (goto-char from) ;; add a space between "}" and a preceding wordchar:
    (ess-rep-regexp "\\([A-Za-z0-9()]\\)}" "\\1 }" 'fix nil verbose)
    (ess-space-around "else" from verbose)

    (ess-if-verbose-write "ess-fix-misc: after \"{ ... }\" :\n");___D___
    (goto-char from) ;; add a space inside "){"
    (ess-rep-regexp "){" ") {" 'fix nil verbose)

    ;; add a newline and indent before a "}"
    ;; --- IFF there's NO "{" or "#" AND some NON-white text on the same line:
    ;;D (if verbose (message "\t R-fix-misc..: Hard.. '}'"))
    (goto-char from)
    (ess-rep-regexp "^\\([^#{\n]*[^#{ \t\n]+[ \t]*\\)}[ \t]*$"
                    "\\1\n}" 'fix nil verbose)
    (ess-if-verbose-write "ess-fix-misc __end__\n");___D___
    ))

(defun ess-cycle-assign ()
  "Cycle between assignment symbols in `ess-assign-list'.
On consecutive calls, replace the assignment symbol before point
with the next symbol from that list. This function sets the last
keypress to repeat it, so if it is bound to \"C-c C-=\" pressing
\"=\" again cycles to the next assignment."
  (interactive)
  (if (eq last-command this-command)
      (let ((slist ess-assign-list)
            str)
        ;; The or statements in the setq allow cycling past the end of
        ;; ess-assign-list.
        (while (and (setq str (or (car slist) (car ess-assign-list))
                          slist (or (cdr slist) ess-assign-list))
                    (not (and (re-search-backward str
                                                  (- (point) (length str)) t)
                              (not (replace-match (car slist))))))))
    (insert (car ess-assign-list)))
  (set-transient-map
   (let ((map (make-sparse-keymap))
         (key (format "%c" (event-basic-type last-input-event))))
     (define-key map (kbd key) #'ess-cycle-assign)
     map)))

(defun ess-insert-assign (arg)
  "Insert the first element of `ess-assign-list' unless in string or comment.
If the character before point is the first element of
`ess-assign-list', replace it with the last character typed.

If `ess-language' is not \"S\", call `self-insert-command' with ARG."
  (interactive "p")
  (if (string= ess-language "S")
      (let* ((assign (car ess-assign-list))
             (event (event-basic-type last-input-event))
             (char (ignore-errors (format "%c" event))))
        (cond ((and char (ess-inside-string-or-comment-p))
               (insert char))
              ((re-search-backward assign (- (point) (length assign)) t)
               (if (and char (numberp event))
                   (replace-match char t t)
                 (replace-match "")))
              (t (insert assign))))
    (funcall #'self-insert-command arg)))

;; In case people had this in their config don't cause errors:
(define-obsolete-function-alias 'ess-smart-S-assign 'ess-insert-assign "ESS 18.10")
(define-obsolete-function-alias 'ess-disable-smart-S-assign #'ignore "ESS 18.10")

(defun ess-add-MM-keys ()
  "Define MM's user keys."
  (declare (obsolete "Setup your own keybindings." "ESS 19.04"))
  (define-key inferior-ess-mode-map "\C-cw" #'ess-execute-screen-options)
  (define-key ess-mode-map          [?\M--] #'ess-insert-assign)
  (define-key inferior-ess-mode-map [?\M--] #'ess-insert-assign))

(defun ess-dump-args-and-go (Sfunc)
  "Dump the function name, with arguments, to a buffer for editing.

Currently, this needs to:
   1. set the buffer to the right mode, with the right settings
   2. format the statement,
   3. c/function/Sfunc/
and I need to relearn Emacs lisp (but I had to, anyway."
  (interactive "sFunction ? ")
  (declare (obsolete 'ess-execute "ESS 19.04"))
  (let* ((buffname "ess-complete.R"))
    (ess-execute (format "args(%s)" Sfunc) t buffname)
    (pop-to-buffer (concat "*" buffname "*"))
    (while (search-forward "function" nil t)
      (replace-match Sfunc nil t))
    (when (fboundp 'ess-r-mode)
      (ess-r-mode))))

;;; S imenu support

;; don't use syntax classes, bad for etags
(defvar ess-imenu-S-generic-expression
  '(("Functions" "^\\([^ \t\n]+\\)[ \t\n]*\\(?:<-\\|=\\)[ \t\n]*function[ ]*(" 1)
    ("Classes" "^.*setClass(\\(.*\\)," 1)
    ("Coercions" "^.*setAs(\\([^,]+,[^,]*\\)," 1) ; show from and to
    ("Generics" "^.*setGeneric(\\([^,]*\\)," 1)
    ("Methods" "^.*set\\(Group\\|Replace\\)?Method(\\([^,]+,[^,]*\\)" 2)
    ("Package" "^.*\\(library\\|require\\)(\\([^)]*\\)" 2)
    ("Data" "^\\(.+\\)[ \t\n]-*\\(?:<-\\|=\\)[ \t\n]*\\(read\\|.*data\\.frame\\).*(" 1))
  "Imenu generic expression for S modes.
See `imenu-generic-expression'.")

(defun ess-imenu-S (&optional _arg)
  "S Language Imenu support for ESS.
ARG is ignored."
  (declare (obsolete "It is set automatically in major modes" "ESS 19.04"))
  (imenu-add-to-menubar "Imenu-S"))

(define-obsolete-function-alias 'ess-imenu-R 'ess-imenu-S "ESS 19.04")


 ;;; Speedbar stuff.

(eval-after-load "speedbar"
  '(progn
     (speedbar-add-supported-extension ".R")
     (speedbar-add-supported-extension ".S")
     (speedbar-add-supported-extension ".s")
     (speedbar-add-supported-extension ".q")))

(cl-defmethod ess-help-get-topics (proc &context (ess-dialect "R"))
  "Return a list of current S help topics associated with process PROC.
If 'sp-for-help-changed?' process variable is non-nil or
`ess-help-topics-list' is nil, (re)-populate the latter and
return it.  Otherwise, return `ess-help-topics-list'."
  (with-ess-process-buffer nil
    (cond
     ;; (Re)generate the list of topics
     ((or (not ess-help-topics-list)
          (ess-process-get 'sp-for-help-changed?))
      (ess-process-put 'sp-for-help-changed? nil)
      (setq ess-help-topics-list
            (delete-dups
             (append (ess-get-object-list proc 'exclude-1st)
                     (ess-get-help-files-list)
                     (ess-get-help-aliases-list)))))
     (t
      ess-help-topics-list))))

(defalias 'S 'S+)
(defalias 's-mode 'S+-mode)
(defalias 's-transcript-mode 'S+-transcript-mode)
(defalias 'S-transcript-mode 's-transcript-mode)
(defalias 'S-mode 's-mode)


(define-obsolete-function-alias 'ess-toggle-S-assign-key #'ignore "ESS 18.10")
(define-obsolete-function-alias 'ess-smart-underscore 'ess-insert-assign "ESS 18.10")
(define-obsolete-function-alias 'ess-insert-S-assign 'ess-insert-assign "ESS 18.10")

(define-obsolete-function-alias 'ess-toggle-underscore 'ess-disable-smart-S-assign "ESS 18.10")
(define-obsolete-function-alias 'ess-toggle-S-assign 'ess-disable-smart-S-assign "ESS 18.10")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Ss]t\\'" . S-transcript-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.Sout" . S-transcript-mode))

(provide 'ess-s-lang)

;;; ess-s-lang.el ends here
