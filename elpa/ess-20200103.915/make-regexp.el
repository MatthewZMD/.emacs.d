;;; make-regexp.el --- generate efficient regexps to match strings.

;; Copyright (C) 1994, 1995 Simon Marshall.

;; Author: Simon Marshall <simon@gnu.ai.mit.edu>
;; Keywords: lisp, matching
;; Version: 1.02

;; LCD Archive Entry:
;; make-regexp|Simon Marshall|simon@gnu.ai.mit.edu|
;; Generate efficient regexps to match strings.|
;; 11-Jul-1995|1.02|~/functions/make-regexp.el.gz|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/

;;; Commentary:

;; Purpose:
;;
;; To make efficient regexps from lists of strings.

;; For example:
;;
;; (let ((strings '("cond" "if" "while" "let\\*?" "prog1" "prog2" "progn"
;;                  "catch" "throw" "save-restriction" "save-excursion"
;;                  "save-window-excursion" "save-match-data"
;;                  "unwind-protect" "condition-case" "track-mouse")))
;;   (concat "(" (make-regexp strings t)))
;;
;;      => "(\\(c\\(atch\\|ond\\(\\|ition-case\\)\\)\\|if\\|let\\*?\\|prog[12n]\\|save-\\(excursion\\|match-data\\|restriction\\|window-excursion\\)\\|t\\(hrow\\|rack-mouse\\)\\|unwind-protect\\|while\\)"
;;
;; To search for the above regexp takes about 70% of the time as for the simple
;; (concat "(\\(" (mapconcat 'identity strings "\\|") "\\)") regexp.
;;
;; Obviously, the more the similarity between strings, the faster the regexp:
;;
;; (make-regexp '("abort" "abs" "accept" "access" "array" "begin" "body" "case"
;;                "constant" "declare" "delay" "delta" "digits" "else" "elsif"
;;                "entry" "exception" "exit" "function"  "generic" "goto" "if"
;;                "others" "limited" "loop" "mod" "new" "null" "out" "subtype"
;;                "package" "pragma" "private" "procedure" "raise" "range"
;;                "record" "rem" "renames" "return" "reverse" "select"
;;                "separate" "task" "terminate" "then" "type" "when" "while"
;;                "with" "xor"))
;;
;;     => "a\\(b\\(ort\\|s\\)\\|cce\\(pt\\|ss\\)\\|rray\\)\\|b\\(egin\\|ody\\)\\|c\\(ase\\|onstant\\)\\|d\\(e\\(clare\\|l\\(ay\\|ta\\)\\)\\|igits\\)\\|e\\(ls\\(e\\|if\\)\\|ntry\\|x\\(ception\\|it\\)\\)\\|function\\|g\\(eneric\\|oto\\)\\|if\\|l\\(imited\\|oop\\)\\|mod\\|n\\(ew\\|ull\\)\\|o\\(thers\\|ut\\)\\|p\\(ackage\\|r\\(agma\\|ivate\\|ocedure\\)\\)\\|r\\(a\\(ise\\|nge\\)\\|e\\(cord\\|m\\|names\\|turn\\|verse\\)\\)\\|s\\(e\\(lect\\|parate\\)\\|ubtype\\)\\|t\\(ask\\|erminate\\|hen\\|ype\\)\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\|xor"
;;
;; To search for the above regexp takes less than 60% of the time of the simple
;; mapconcat equivalent.
;;
;; But even small regexps may be worth it:
;;
;; (make-regexp '("and" "at" "do" "end" "for" "in" "is" "not" "of" "or" "use"))
;;     => "a\\(nd\\|t\\)\\|do\\|end\\|for\\|i[ns]\\|not\\|o[fr]\\|use"
;;
;; as this is 10% faster than the mapconcat equivalent.

;; Installation:
;;
;; (autoload 'make-regexp "make-regexp"
;;   "Return a regexp to match a string item in STRINGS.")
;;
;; (autoload 'make-regexps "make-regexp"
;;   "Return a regexp to REGEXPS.")
;;
;; Since these functions were written to produce efficient regexps, not regexps
;; efficiently, it is probably not a good idea to in-line too many calls in
;; your code, unless you use the following neat trick with `eval-when-compile':
;;
;; (defvar definition-regexp
;;   (let ((regexp (eval-when-compile
;;                   (make-regexp '("defun" "defsubst" "defmacro" "defalias"
;;                                  "defvar" "defconst" "defadvice") t))))
;;     (concat "^(" regexp)))
;;
;; The `byte-compile' code will be as if you had defined the variable thus:
;;
;; (defvar definition-regexp
;;   "^(\\(def\\(a\\(dvice\\|lias\\)\\|const\\|macro\\|subst\\|un\\|var\\)\\)")

;; Feedback:
;;
;; Originally written for font-lock, from an idea from Stig's hl319.
;; Please don't tell me that it doesn't produce optimal regexps; I know that
;; already.  But (ideas or) code to improve things (are) is welcome.  Please
;; test your code and tell me the speed up in searching an appropriate buffer.
;;
;; Please send me bug reports, bug fixes, and extensions, etc.
;; Simon Marshall <simon@gnu.ai.mit.edu>

;; History:
;;
;; 1.00--1.01:
;; - Made `make-regexp' take `lax' to force top-level parentheses.
;; - Fixed `make-regexps' for MATCH bug and new `font-lock-keywords'.
;; - Added `unfontify' to user timing functions.
;; 1.01--1.02:
;; - Made `make-regexp' `let' a big `max-lisp-eval-depth'.

;; The basic idea is to find the shortest common non-"" prefix each time, and
;; squirrel it out.  If there is no such prefix, we divide the list into two so
;; that (at least) one half will have at least a one-character common prefix.

;; In addition, we (a) delay the addition of () parenthesis as long as possible
;; (until we're sure we need them), and (b) try to squirrel out one-character
;; sequences (so we can use [] rather than ()).

;;; Code:

(defun make-regexp (strings &optional paren lax)
  "Return a regexp to match a string item in STRINGS.
If optional PAREN non-nil, output regexp parentheses around returned regexp.
If optional LAX non-nil, don't output parentheses if it doesn't require them.
Merges keywords to avoid backtracking in Emacs' regexp matcher."
  (let* ((max-lisp-eval-depth (* 1024 1024))
         (strings (let ((l strings))    ; Paranoia---make strings unique!
                    (while l (setq l (setcdr l (delete (car l) (cdr l)))))
                    (sort strings 'string-lessp)))
         (open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" ""))
         (open-lax (if lax "" open-paren)) (close-lax (if lax "" close-paren))
         (completion-ignore-case nil))
    (cond
     ;; If there's only one string, just return it.
     ((= (length strings) 1)
      (concat open-lax (car strings) close-lax))
     ;; If there's an empty string, pull it out.
     ((string= (car strings) "")
      (if (and (= (length strings) 2) (= (length (nth 1 strings)) 1))
          (concat open-lax (nth 1 strings) "?" close-lax)
        (concat open-paren "\\|" (make-regexp (cdr strings)) close-paren)))
     ;; If there are only one-character strings, make a [] list instead.
     ((= (length strings) (apply '+ (mapcar 'length strings)))
      (concat open-lax "[" (mapconcat 'identity strings "") "]" close-lax))
     (t
      ;; We have a list of strings.  Is there a common prefix?
      (let ((prefix (try-completion "" (mapcar 'list strings))))
        (if (> (length prefix) 0)
            ;; Common prefix!  Squirrel it out and recurse with the suffixes.
            (let* ((len (length prefix))
                   (sufs (mapcar (lambda (str) (substring str len)) strings)))
              (concat open-paren prefix (make-regexp sufs t t) close-paren))
          ;; No common prefix.  Is there a one-character sequence?
          (let ((letters (let ((completion-regexp-list '("^.$")))
                           (all-completions "" (mapcar 'list strings)))))
            (if (> (length letters) 1)
                ;; Do the one-character sequences, then recurse on the rest.
                (let ((rest (let ((completion-regexp-list '("^..+$")))
                              (all-completions "" (mapcar 'list strings)))))
                  (concat open-paren
                          (make-regexp letters) "\\|" (make-regexp rest)
                          close-paren))
              ;; No one-character sequence, so divide the list into two by
              ;; dividing into those that start with a particular letter, and
              ;; those that do not.
              (let* ((char (substring (car strings) 0 1))
                     (half1 (all-completions char (mapcar 'list strings)))
                     (half2 (nthcdr (length half1) strings)))
                (concat open-paren
                        (make-regexp half1) "\\|" (make-regexp half2)
                        close-paren))))))))))

;; This stuff is realy for font-lock...

;; Ahhh, the wonders of lisp...
(defun regexp-span (regexp &optional start)
  "Return the span or depth of REGEXP.
This means the number of \"\\\\(...\\\\)\" pairs in REGEXP, optionally from START."
  (let ((match (string-match (regexp-quote "\\(") regexp (or start 0))))
    (if (not match) 0 (1+ (regexp-span regexp (match-end 0))))))

;; The basic idea is to concat the regexps together, keeping count of the span
;; of the regexps so that we can get the correct match for hilighting.
(defun make-regexps (&rest regexps)
  "Return a regexp to match REGEXPS
Each item of REGEXPS should be of the form:

 STRING                                 ; A STRING to be used literally.
 (STRING MATCH FACE DATA)               ; Match STRING at depth MATCH with FACE
                                        ; and highlight according to DATA.
 (STRINGS FACE DATA)                    ; STRINGS is a list of strings FACE is
                                        ; to highlight according to DATA.

Returns a list of the form:

 (REGEXP (MATCH FACE DATA) ...)

For example:

 (make-regexps \"^(\"
               '((\"defun\" \"defalias\" \"defsubst\" \"defadvice\") keyword)
               \"[ \t]*\"
               '(\"\\\\([a-zA-Z-]+\\\\)?\" 1 function-name nil t))

     =>

 (\"^(\\\\(def\\\\(a\\\\(dvice\\\\|lias\\\\)\\\\|subst\\\\|un\\\\)\\\\)[        ]*\\\\([a-zA-Z-]+\\\\)?\"
  (1 keyword) (4 function-name nil t))

Uses `make-regexp' to make efficient regexps."
  (let ((regexp "") (data ()))
    (while regexps
      (cond ((stringp (car regexps))
             (setq regexp (concat regexp (car regexps))))
            ((stringp (nth 0 (car regexps)))
             (setq data (cons (cons (+ (regexp-span regexp)
                                       (nth 1 (car regexps)))
                                    (nthcdr 2 (car regexps)))
                              data)
                   regexp (concat regexp (nth 0 (car regexps)))))
            (t
             (setq data (cons (cons (1+ (regexp-span regexp))
                                    (cdr (car regexps)))
                              data)
                   regexp (concat regexp (make-regexp (nth 0 (car regexps))
                                                      t)))))
      (setq regexps (cdr regexps)))
    (cons regexp (nreverse data))))

;; timing functions removed due to name collisions with Gnus

(provide 'make-regexp)

;;; make-regexp.el ends here
