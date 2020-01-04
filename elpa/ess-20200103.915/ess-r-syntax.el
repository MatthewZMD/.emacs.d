;;; ess-r-syntax.el --- Utils to work with R code

;; Copyright (C) 2015 Lionel Henry

;; Author: Lionel Henry <lionel.hry@gmail.com>
;; Created: 12 Oct 2015
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

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

;; API is not yet stable.

;;; Code:

(require 'ess-utils)
(require 'regexp-opt)

(eval-when-compile
  (require 'cl-lib))


;;*;; Utils

;; The three following wrappers return t if successful, nil on error
(defun ess-backward-sexp (&optional N)
  (ess-forward-sexp (- (or N 1))))

(defun ess-forward-sexp (&optional N)
  (or N (setq N 1))
  (condition-case nil
      (prog1 t
        (goto-char (or (scan-sexps (point) N)
                       (buffer-end N))))
    (error nil)))

(defun ess-up-list (&optional N)
  (condition-case nil
      (let (forward-sexp-function)
        (progn (up-list N) t))
    (error nil)))

(defun ess-backward-up-list (&optional N)
  (ess-up-list (- (or N 1))))

(defun ess-forward-char (&optional N)
  (unless (= (point) (point-max))
    (forward-char (or N 1))
    t))

(defun ess-backward-char (&optional N)
  (unless (bobp)
    (forward-char (- (or N 1)))
    t))

(defun ess-goto-char (pos)
  "Go to POS if it is non-nil.
If POS is nil, return nil.  Otherwise return position itself."
  (when pos
    (goto-char pos)))

(defun ess-looking-at (regex &optional newlines)
  "Like `looking-at' but consumes blanks and comments first."
  (save-excursion
    (ess-skip-blanks-forward newlines)
    (looking-at regex)))

(defmacro ess-save-excursion-when-nil (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  `(let ((orig-point (point)))
     (cond ((progn ,@body))
           (t (prog1 nil
                (goto-char orig-point))))))

(defmacro ess-while (test &rest body)
  "Like `while' for TEST but return t when BODY gets executed once."
  (declare (indent 1)
           (debug (&rest form)))
  `(let (executed)
     (while ,test
       (setq executed t)
       ,@body)
     executed))

(defmacro ess-at-indent-point (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  `(save-excursion
     (goto-char indent-point)
     (back-to-indentation)
     (progn ,@body)))

(defvar containing-sexp)
(defmacro ess-at-containing-sexp (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  '(when (not (bound-and-true-p containing-sexp))
     (error "Internal error: containing-sexp is nil or undefined"))
  `(save-excursion
     (goto-char containing-sexp)
     (progn ,@body)))

(defmacro ess-any (&rest forms)
  "Evaluate all arguments and return non-nil if one of the arguments is non-nil.
This is useful to trigger side-effects. FORMS follows the same
syntax as arguments to `cond'."
  (declare (indent 0) (debug nil))
  `(let ((forms (list ,@(mapcar (lambda (form) `(progn ,@form)) forms))))
     (cl-some 'identity (mapcar 'eval forms))))

(defun ess-char-syntax (string)
  (char-to-string (char-syntax (string-to-char string))))


;;*;; Tokenisation

(defun ess-token-type (token) (car (nth 0 token)))
(defun ess-token-value (token) (cdr (nth 0 token)))
(defun ess-token-start (token) (car (nth 1 token)))
(defun ess-token-end (token) (cdr (nth 1 token)))

(defun ess-token-refined-type (token)
  (ess-token-type (ess-refine-token token)))

(defun ess-token-after (&optional token)
  "Return next TOKEN.
Cons cell containing the token type and string representation."
  (save-excursion
    (when token
      (goto-char (ess-token-end token)))
    (ess-jump-token)))

(defun ess-token-before (&optional token)
  "Return previous TOKEN.
Cons cell containing the token type and string representation."
  (save-excursion
    (when token
      (goto-char (ess-token-start token)))
    (ess-climb-token)))

(defun ess-climb-token (&optional type string)
  (ess-save-excursion-when-nil
    (ess-escape-comment)
    (ess-skip-blanks-backward t)
    (let ((token (or (ess-climb-token--back)
                     (ess-climb-token--back-and-forth)
                     (progn (forward-char -1) (ess-token-after)))))
      (if (or type string)
          (when (ess-token= token type string)
            token)
        token))))

(defun ess-token--cons (type value)
  (if (eq type 'self)
      (cons value nil)
    (cons type value)))

(defun ess-climb-token--back ()
  (let* ((token-end (point))
         (token-type (if (bobp)
                         "buffer-start"
                       (ess-climb-token--operator)))
         (token-value (buffer-substring-no-properties (point) token-end)))
    (unless (null token-type)
      (list (ess-token--cons token-type token-value)
            (cons (point) token-end)))))

(defsubst ess-climb-token--char (&rest chars)
  (ess-while (and chars
                  (eq (char-before) (car chars))
                  (ess-backward-char))
    (setq chars (cdr chars))))

;; Difficult to use regexps here because we want to match greedily
;; backward
(defun ess-climb-token--operator ()
  (when (pcase (char-before)
          ((or `?+ `?/ `?^ `?~ `?? `?!)
           (ess-backward-char))
          (`?=
           (prog1 (ess-backward-char)
             (or (ess-climb-token--char ?=)
                 (ess-climb-token--char ?!)
                 (ess-climb-token--char ?:)
                 (ess-climb-token--char ?>)
                 (ess-climb-token--char ?<))))
          ((or `?& `?| `?* `?@ `?$)
           (prog1 (ess-backward-char)
             (ess-climb-token--char (char-after))))
          (`?<
           (ess-backward-char))
          (`?>
           (prog1 (ess-backward-char)
             (or (ess-climb-token--char ?-)
                 (and (looking-back "->" (- (point) 2))
                      (goto-char (- (point) 2))))))
          (`?-
           (prog1 (ess-backward-char)
             (ess-climb-token--char ?< ?<)))
          (`?:
           (prog1 (ess-backward-char)
             (ess-climb-token--char ?: ?:))))
    'self))

(defun ess-climb-token--back-and-forth ()
  (let ((limit (point)))
    (when (ess-skip-token-backward)
      (save-restriction
        (narrow-to-region (point) limit)
        (ess-token-after)))))

(defun ess-skip-token-backward ()
  (ess-save-excursion-when-nil
    (cond
     ;; Punctuation
     ((memq (char-before) '(?, ?\;))
      (ess-backward-char))
     ;; Quoting delimiters
     ((memq (char-syntax (char-before)) '(?\" ?$))
      (ess-backward-sexp))
     ;; Syntaxic delimiters
     ((memq (char-syntax (char-before)) '(?\( ?\)))
      (prog1 (ess-backward-char)
        ;; Also skip double brackets
        (ess-save-excursion-when-nil
          (when (let ((current-delim (char-after)))
                  (ess-skip-blanks-backward)
                  (and (memq (char-before) '(?\[ ?\]))
                       (eq current-delim (char-before))))
            (ess-backward-char)))))
     ;; Identifiers and numbers
     ((/= (skip-syntax-backward "w_") 0)))))

(defun ess-jump-token (&optional type string)
  "Consume a token forward.
Return a cons cell containing the token type and the token string
content. Return nil when the end of the buffer is reached."
  (ess-save-excursion-when-nil
    (ess-skip-blanks-forward t)
    (let* ((token-start (point))
           (token-type (or (ess-jump-token--regexps)
                           (ess-jump-token--literal)
                           (ess-jump-token--infix-op)
                           (ess-jump-token--punctuation)
                           (progn (forward-char) "unknown")))
           (token-value (buffer-substring-no-properties token-start (point))))
      (let ((token (list (ess-token--cons token-type token-value)
                         (cons token-start (point)))))
        (if (or type string)
            (when (ess-token= token type string)
              token)
          token)))))

(defun ess-jump-token--literal ()
  (cond
   ;; Simply assume anything starting with a digit is a number. May be
   ;; too liberal but takes care of fractional numbers, integers such
   ;; as 10L, etc. False positives are not valid R code anyway.
   ((looking-at "[0-9]")
    (ess-forward-sexp)
    "number")
   ((or (looking-at "\\sw\\|\\s_")
        (eq (char-after) ?`))
    (ess-forward-sexp)
    "identifier")
   ((memq (char-after) '(?\" ?\'))
    (ess-forward-sexp)
    "string")))

(defun ess-jump-token--punctuation ()
  (or (when (= (point) (point-max))
        "buffer-end")
      (pcase (char-after)
        (`?\;
         (forward-char)
         'self)
        (`?,
         (forward-char)
         ;; Treat blanks after comma as part of an argument
         (ess-skip-blanks-forward t)
         ","))))

(defvar ess-r-prefix-keywords-list
  '("if" "for" "while" "function"))

(defvar ess-r-keywords-list
  (append ess-r-prefix-keywords-list '("else")))

(defvar ess-r-delimiters-list
  '("(" ")" "{" "}" "[" "]" "[[" "]]"))

(defvar ess-r-operators-list
  '("+" "-" "*" "/" "%%" "**" "^"
    "&" "&&" "|" "||" "!" "?" "~"
    "==" "!=" "<" "<=" ">=" ">"
    "=" "<-" "<<-" "->" "->>"
    "$" "@" ":" "::" ":::" ":="))

(defvar ess-r-keywords-re
  (concat (regexp-opt ess-r-keywords-list) "\\_>"))

(defvar ess-r-delimiters-re
  (regexp-opt ess-r-delimiters-list))

(defvar ess-r-operators-re
  (regexp-opt ess-r-operators-list))

(defun ess-jump-token--regexps ()
  (when (or (looking-at ess-r-keywords-re)
            (looking-at ess-r-delimiters-re)
            (looking-at ess-r-operators-re))
    (goto-char (match-end 0))
    'self))

(defun ess-jump-token--infix-op ()
  (or (when (looking-at ess-r-operators-re)
        (goto-char (match-end 0))
        'self)
      (when (eq (char-after) ?%)
        (ess-forward-sexp)
        "%infix%")))

(defun ess-escape-token ()
  (ess-escape-comment)
  (ess-skip-blanks-forward)
  (or (ess-escape-string)
      (when (ess-token-delimiter-p (ess-token-after))
        (prog1 t
          (mapc (lambda (delims)
                  (while (and (ess-token-after= nil delims)
                              (eq (char-before) (string-to-char
                                                 (car delims))))
                    (ess-backward-char)))
                '(("[" "[[") ("]" "]]")))))
      (ess-token-after= '("," ";"))
      (and (ess-token-after= "identifier")
           (not (memq (char-syntax (char-before)) '(?w ?_))))
      (progn (skip-syntax-backward ".")
             (ess-token-operator-p (ess-token-after)))
      (/= (skip-syntax-backward "w_") 0)))

(defun ess-refine-token (token)
  (let ((refined-type
         (pcase (ess-token-type token)
           ;; Parameter assignment
           (`"="
            (save-excursion
              (goto-char (ess-token-start token))
              (let ((containing-sexp (ess-containing-sexp-position)))
                (when (and containing-sexp
                           (ess-at-containing-sexp
                             (and (ess-token-after= "(")
                                  (ess-token-before= '("identifier" "string"))))
                           (save-excursion
                             (and (ess-climb-token)
                                  (ess-token-before= '("," "(")))))
                  "param-assign"))))
           ;; Quoted identifiers
           (`"string"
            (when (or
                   ;; Quoted parameter names
                   (ess-refined-token= (ess-token-after) "param-assign")
                   ;; Quoted call names
                   (ess-token-after= "("))
              "identifier"))
           ((or `"(" `")")
            (or (save-excursion
                  (if (ess-token-close-delimiter-p token)
                      (ess-climb-paired-delims nil token)
                    (goto-char (ess-token-start token)))
                  (when (ess-token-keyword-p (ess-token-before))
                    "prefixed-expr-delimiter"))
                ;; Fixme: probably too crude. Better handled in parser
                (when (ess-token= token ")")
                  (save-excursion
                    (ess-climb-paired-delims ")" token)
                    (when (ess-token-before= '("identifier" "string" ")" "]" "]]" "}"))
                      "argslist-delimiter")))))
           ((or `"{" `"}")
            (save-excursion
              (unless (ess-climb-paired-delims "}" token)
                (goto-char (ess-token-start token)))
              (when (ess-refined-token= (ess-token-before) "prefixed-expr-delimiter")
                "prefixed-expr-delimiter"))))))
    (if refined-type
        (list (cons refined-type (ess-token-value token))
              (nth 1 token))
      token)))

(defun ess-token-balancing-delim (token)
  (pcase (ess-token-type token)
    (`"(" ")")
    (`")" "(")
    (`"[" "]")
    (`"]" "[")
    (`"[[" "]]")
    (`"]]" "[[")))


;;;*;;; Token predicates

(defun ess-token= (token &optional type string)
  (when (and (null type)
             (null string))
    (error "No condition supplied"))
  (let ((type (if (stringp type) (list type) type))
        (string (if (stringp string) (list string) string)))
    (and (if type (member (ess-token-type token) type) t)
         (if string (member (ess-token-value token) string) t))))

(defun ess-refined-token= (token type &optional string)
  (ess-token= (ess-refine-token token) type string))

(defun ess-token-after= (type &optional string)
  (ess-token= (ess-token-after) type string))

(defun ess-token-before= (type &optional string)
  (ess-token= (ess-token-before) type string))

(defun ess-token-open-delimiter-p (token)
  (string= (ess-char-syntax (ess-token-type token)) "("))

(defun ess-token-close-delimiter-p (token)
  (string= (ess-char-syntax (ess-token-type token)) ")"))

(defun ess-token-delimiter-p (token)
  (or (ess-token-open-delimiter-p token)
      (ess-token-close-delimiter-p token)))

(defun ess-token-operator-p (token &optional strict)
  (and (or (member (ess-token-type token) ess-r-operators-list)
           (string= (ess-token-type token) "%infix%"))
       (or (null strict)
           (not (ess-refined-token= token "param-assign")))))

(defun ess-token-keyword-p (token)
  (member (ess-token-type token) ess-r-keywords-list))


;;;*;;; Tokens properties and accessors

(defun ess-token-make-hash (&rest specs)
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (spec)
            ;; alist
            (if (listp (cdr spec))
                (mapc (lambda (cell)
                      (puthash (car cell) (cdr cell) table))
                    spec)
              ;; Cons cell
              (mapc (lambda (token)
                        (puthash token (cdr spec) table))
                      (car spec))))
          specs)
    table))

(defvar ess-token-r-powers-delimiters
  '(("("  . 100)
    ("["  . 100)
    ("[[" . 100)))

(defvar ess-token-r-powers-operator
  '(("?"       .  5)
    ("else"    .  8)
    ("<-"      . 10)
    ("<<-"     . 10)
    ("="       . 15)
    ("->"      . 20)
    ("->>"     . 20)
    ("~"       . 25)
    ("|"       . 30)
    ("||"      . 30)
    ("&"       . 35)
    ("&&"      . 35)
    ("!"       . 40)
    ("<"       . 45)
    (">"       . 45)
    ("<="      . 45)
    (">="      . 45)
    ("=="      . 45)
    ("+"       . 50)
    ("-"       . 50)
    ("*"       . 55)
    ("/"       . 55)
    ("%infix%" . 60)
    (":"       . 65)
    ("^"       . 70)
    ("$"       . 75)
    ("@"       . 75)
    ("::"      . 80)
    (":::"     . 80)))

(defvar ess-token-r-power-table
  (ess-token-make-hash ess-token-r-powers-operator
                       ess-token-r-powers-delimiters))

(defvar ess-token-r-right-powers-operator
  '((")"  . 1)))

(defvar ess-token-r-right-power-table
  (ess-token-make-hash ess-token-r-powers-operator
                       ess-token-r-right-powers-operator))

(defvar ess-token-r-nud-table
  (ess-token-make-hash
   '(("identifier" . identity)
     ("literal" . identity)
     ("number" . identity)
     ("function" . identity)
     ("if" . identity)
     ("while" . identity)
     ("for" . identity))
   '(("(" . ess-parser-nud-block)
     ("{" . ess-parser-nud-block))))

(defvar ess-token-r-rnud-table
  (ess-token-make-hash
   '(("identifier" . identity)
     ("literal" . identity)
     ("number" . identity))
   '((")" . ess-parser-rnud-paren)
     ("}" . ess-parser-nud-block))))

(defvar ess-token-r-leds-operator
  (let ((operators-list (append '("%infix%" "else") ess-r-operators-list)))
    (cons operators-list #'ess-parser-led-lassoc)))

(defvar ess-token-r-leds-delimiter
  '(("(" . ess-parser-led-funcall)
    ("[" . ess-parser-led-funcall)
    ("[[" . ess-parser-led-funcall)))

(defvar ess-token-r-led-table
  (ess-token-make-hash ess-token-r-leds-operator
                       ess-token-r-leds-delimiter))

(defvar ess-token-r-rid-table
  (ess-token-make-hash
   '((")" . ess-parser-rid-expr-prefix))))


;;;*;;; Nud, led and rid functions

(defun ess-parser-nud-block (prefix-token)
  (let ((right (list (cons "TODO" nil))))
    (ess-parser-advance-pair nil prefix-token)
    (ess-node (cons "block" nil)
              (cons (ess-token-start prefix-token) (point))
              (list prefix-token right))))

(defun ess-parser-led-lassoc (start infix-token)
  (let* ((power (ess-parser-power infix-token))
         (end (ess-parse-expression power)))
    (ess-node (cons "binary-op" nil)
              (cons (ess-parser-token-start start) (point))
              (list start infix-token end))))

(defun ess-parser-led-funcall (left infix-token)
  (when (ess-token= left (append '("identifier" "string" "node")
                                 ess-r-prefix-keywords-list))
    (let* ((power (ess-parser-power infix-token))
           (right (ess-parse-arglist power infix-token))
           (type (if (ess-token= left ess-r-prefix-keywords-list)
                     "prefixed-expr"
                   "funcall")))
      (when (string= type "prefixed-expr")
        (setq right (list right (ess-parse-expression 0))))
      (ess-node (cons type nil)
                (cons (ess-parser-token-start left) (point))
                (list left right)))))

(defun ess-parser-rid-expr-prefix (right suffix-token)
  (when (ess-refined-token= suffix-token "prefixed-expr-delimiter")
    (ess-parser-rnud-paren suffix-token right)))

(defun ess-parser-rnud-paren (suffix-token &optional prefixed-expr)
  (let* ((infix-token (save-excursion
                        (ess-parser-advance-pair nil suffix-token)))
         (power (ess-parser-power infix-token))
         (args (ess-parse-arglist power suffix-token))
         (left (if prefixed-expr
                   (ess-parser-advance)
                 (ess-parse-expression power)))
         (type (cond (prefixed-expr "prefixed-expr")
                     (left "funcall")
                     (t "enclosed-expr"))))
    (when prefixed-expr
      (setcdr (car prefixed-expr) (list infix-token suffix-token)))
    (ess-node (cons type nil)
              (cons (ess-parser-token-start suffix-token) (point))
              (if prefixed-expr
                  (list prefixed-expr args left)
                (list args left)))))


;;;*;;; Parsing

(defun ess-parser-advance (&optional type value)
  (if (bound-and-true-p ess-parser--backward)
      (ess-climb-token type value)
    (ess-jump-token type value)))

(defun ess-parser-advance-pair (&optional type token)
  (if (bound-and-true-p ess-parser--backward)
      (ess-climb-paired-delims type token)
    (ess-jump-paired-delims type token)))

(defun ess-parser-next-token ()
  (if (bound-and-true-p ess-parser--backward)
      (ess-token-before)
    (ess-token-after)))

(defun ess-parser-token-start (token)
  (if (bound-and-true-p ess-parser--backward)
      (ess-token-end token)
    (ess-token-start token)))

(defun ess-parser-power (token)
  (or (if (bound-and-true-p ess-parser--backward)
          (gethash (ess-token-type token) ess-token-r-right-power-table)
        (gethash (ess-token-type token) ess-token-r-power-table))
      0))

(defun ess-node (type pos contents)
  (let ((pos (if (bound-and-true-p ess-parser--backward)
                 (cons (cdr pos) (car pos))
               pos))
        (contents (if (bound-and-true-p ess-parser--backward)
                      (nreverse contents)
                    contents)))
    (list type pos contents)))

(defalias 'ess-node-start #'ess-token-start)
(defalias 'ess-node-end #'ess-token-end)

(defun ess-parse-start-token (token)
  (let* ((table (if (bound-and-true-p ess-parser--backward)
                    ess-token-r-rnud-table
                  ess-token-r-nud-table))
         (nud (gethash (ess-token-type token) table)))
    (when (fboundp nud)
      (funcall nud token))))

(defun ess-parse-infix-token (infix-token left)
  (let ((infix-power (ess-parser-power infix-token))
        (led (or (when (bound-and-true-p ess-parser--backward)
                   (gethash (ess-token-type infix-token) ess-token-r-rid-table))
                 (gethash (ess-token-type infix-token) ess-token-r-led-table))))
    (funcall led left infix-token)))

(defun ess-parse-expression (&optional power)
  (let ((current (ess-parse-start-token (ess-parser-advance)))
        (power (or power 0))
        (next (ess-parser-next-token))
        (last-sucessful-pos (point))
        last-success)
    (setq last-success current)
    (while (and current (< power (ess-parser-power next)))
      (ess-parser-advance)
      (when (setq current (ess-parse-infix-token next current))
        (setq last-sucessful-pos (point))
        (setq last-success current))
      (setq next (ess-parser-next-token)))
    (goto-char last-sucessful-pos)
    last-success))

(defun ess-parse-arglist (power start-token)
  (let ((start-pos (point))
        (arg-start-pos (point))
        (arglist (list start-token))
        (closing-delim (ess-token-balancing-delim start-token))
        expr)
    (while (and (setq expr (ess-parse-expression))
                (push (ess-node (cons "arg" nil)
                                (cons arg-start-pos (point))
                                (list expr))
                      arglist)
                (ess-parser-advance ","))
      (setq arg-start-pos (point)))
    (push (ess-parser-advance closing-delim) arglist)
    (ess-node (cons "arglist" nil)
              (cons start-pos (1- (point)))
              (nreverse arglist))))

(defun forward-ess-r-expr ()
  (interactive)
  (ess-save-excursion-when-nil
    (ess-escape-token)
    (ess-parse-expression)))

(defun forward-ess-r-sexp ()
  (interactive)
  (ess-save-excursion-when-nil
    (ess-escape-token)
    (let* ((orig-token (ess-token-after))
           (tree (ess-parse-expression))
           (sexp-node (ess-parser-tree-assoc orig-token tree)))
      (when sexp-node
        (goto-char (ess-token-end sexp-node))
        sexp-node))))

(defun backward-ess-r-expr ()
  (interactive)
  (let ((ess-parser--backward t))
    (ess-parse-expression)))

(defun backward-ess-r-sexp ()
  (interactive)
  (error "Todo"))

(defun ess-parser-tree-assoc (key tree)
  (let ((next tree)
        stack last-node result)
    (while (and next (null result))
      (cond ((eq next 'node-end)
             (pop last-node))
            ((nth 2 next)
             (push 'node-end stack)
             (dolist (node (nth 2 next))
               (push node stack))
             (push next last-node))
            ((equal next key)
             (setq result (car last-node))))
      (setq next (pop stack)))
    result))


;;*;; Point predicates

(defun ess-inside-call-p (&optional call)
  "Return non-nil if point is in a function or indexing call."
  (let ((containing-sexp (or (bound-and-true-p containing-sexp)
                             (ess-containing-sexp-position))))
    (save-excursion
      (and (prog1 (ess-goto-char containing-sexp)
             (ess-climb-chained-delims))
           (save-excursion
             (forward-char)
             (ess-up-list))
           (or (ess-behind-call-opening-p "(")
               (looking-at "\\["))
           (ess-inside-call-name-p call)))))

(defun ess-inside-continuation-p ()
  (unless (or (looking-at ",")
              (ess-behind-call-opening-p "[[(]"))
    (or (save-excursion
          (ess-jump-object)
          (and (not (ess-ahead-param-assign-p))
               (ess-behind-operator-p)))
        (save-excursion
          (ess-climb-object)
          (ess-climb-operator)
          (and (ess-behind-operator-p)
               (not (ess-ahead-param-assign-p)))))))

(defun ess-inside-call-name-p (&optional call)
  (save-excursion
    (ess-climb-call-name call)))

(defun ess-inside-prefixed-block-p (&optional call)
  "Return non-nil if point is in a prefixed block.
Prefixed blocks refer to the blocks following function
declarations, control flow statements, etc.

If CALL is not nil, check if the prefix corresponds to CALL. If
nil, return the prefix."
  (save-excursion
    (ess-escape-prefixed-block call)))


;;*;; Syntactic Travellers and Predicates

;;;*;;; Blanks, Characters, Comments and Delimiters

(defun ess-skip-blanks-backward (&optional newlines)
  "Skip blanks and newlines backward, taking end-of-line comments into account."
  (ess-any ((ess-skip-blanks-backward-1))
           ((when newlines
              (ess-while (and (not (bobp))
                              (= (point) (line-beginning-position)))
                (forward-line -1)
                (goto-char (ess-code-end-position))
                (ess-skip-blanks-backward-1))))))

(defun ess-skip-blanks-backward-1 ()
  (and (not (bobp))
       (/= 0 (skip-syntax-backward " "))))

(defun ess-skip-blanks-forward (&optional newlines)
  "Skip blanks and newlines forward, taking end-of-line comments into account."
  (ess-any ((/= 0 (skip-syntax-forward " ")))
           ((ess-while (and newlines
                            (= (point) (ess-code-end-position))
                            (when (ess-save-excursion-when-nil
                                    ;; Handles corner cases such as point being on last line
                                    (let ((orig-point (point)))
                                      (forward-line)
                                      (back-to-indentation)
                                      (> (point) orig-point)))
                              (skip-chars-forward " \t")
                              t))))))

(defun ess-jump-char (char)
  (ess-save-excursion-when-nil
    (ess-skip-blanks-forward t)
    (when (looking-at char)
      (goto-char (match-end 0)))))

(defun ess-escape-comment ()
  (when (ess-inside-comment-p)
    (prog1 (comment-beginning)
     (skip-chars-backward "#+[ \t]*"))))

(defun ess-ahead-closing-p ()
  (memq (char-before) '(?\] ?\} ?\))))

(defun ess-ahead-boundary-p ()
  (looking-back "[][ \t\n(){},]" (1- (point))))

(defun ess-escape-string ()
  (and (nth 3 (syntax-ppss))
       (ess-goto-char (nth 8 (syntax-ppss)))))

(defun ess-climb-paired-delims (&optional type token)
  (ess-save-excursion-when-nil
    (let ((token (or token (ess-token-before))))
      (goto-char (ess-token-end token))
      (when (if type
                (ess-token= token type)
              (ess-token-delimiter-p token))
        (and (ess-backward-sexp)
             (ess-token-after))))))

(defun ess-jump-paired-delims (&optional type token)
  (ess-save-excursion-when-nil
    (let ((token (or token (ess-token-after))))
      (goto-char (ess-token-start token))
      (when (if type
                (ess-token= token type)
              (ess-token-delimiter-p token))
        (and (ess-forward-sexp)
             (ess-token-before))))))


;;;*;;; Blocks

(defun ess-block-opening-p ()
  (save-excursion
    (cond
     ((looking-at "{"))
     ;; Opening parenthesis not attached to a function opens up a
     ;; block too. Only pick up those that are last on their line
     ((ess-behind-block-paren-p)))))

(defun ess-block-closing-p ()
  (save-excursion
    (cond
     ((looking-at "}"))
     ((looking-at ")")
      (forward-char)
      (backward-sexp)
      (not (looking-back
            (concat ess-r-name-pattern "[[:blank:]]*")
            (line-beginning-position)))))))

(defun ess-block-p ()
  (or (save-excursion
        (when containing-sexp
          (goto-char containing-sexp)
          (ess-block-opening-p)))
      (ess-unbraced-block-p)))

;; Parenthesised expressions
(defun ess-behind-block-paren-p ()
  (and (looking-at "(")
       (not (ess-ahead-attached-name-p))))

(defun ess-climb-block (&optional ignore-ifelse)
  (ess-save-excursion-when-nil
    (cond
     ((and (not ignore-ifelse)
           (ess-climb-if-else 'to-start)))
     ((and (eq (char-before) ?\})
           (prog2
               (forward-char -1)
               (ess-up-list -1)
             (ess-climb-block-prefix)))))))

(defvar ess-prefixed-block-patterns
  (mapcar (lambda (fun) (concat fun "[ \t\n]*("))
          '("function" "if" "for" "while")))

(defun ess-behind-prefixed-block-p (&optional call)
  (if call
      (looking-at (concat call "[ \t]*("))
    (cl-some 'looking-at ess-prefixed-block-patterns)))

(defun ess-unbraced-block-p (&optional ignore-ifelse)
  "This indicates whether point is in front of an unbraced
prefixed block following a control flow statement. Returns
position of the control flow function (if, for, while, etc)."
  (save-excursion
    (and (ess-backward-sexp)
         (or (and (looking-at "else\\b")
                  (not ignore-ifelse))
             (and (looking-at "(")
                  (ess-backward-sexp)
                  (cl-some 'looking-at ess-prefixed-block-patterns)
                  (if ignore-ifelse
                      (not (looking-at "if\\b"))
                    t)))
         (point))))

(defun ess-climb-block-prefix (&optional call ignore-ifelse)
  "Climb the prefix of a prefixed block.
Prefixed blocks refer to the blocks following function
declarations, control flow statements, etc.

Should be called either in front of a naked block or in front
of the curly brackets of a braced block.

If CALL not nil, check if the prefix corresponds to CALL. If nil,
return the prefix."
  (ess-save-excursion-when-nil
    (or (and (not ignore-ifelse)
             (prog1 (and (ess-climb-if-else-call)
                         (or (null call)
                             (looking-at call)))
               (when (ess-token-after= "else")
                 (ess-climb-token "}"))))
        (let ((pos (ess-unbraced-block-p ignore-ifelse)))
          (and (ess-goto-char pos)
               (if call
                   (looking-at call)
                 (cond ((looking-at "function")
                        "function")
                       ((looking-at "for")
                        "for")
                       ((looking-at "if")
                        "if")
                       ((looking-at "else")
                        "else"))))))))

(defun ess-escape-prefixed-block (&optional call)
  "Climb outside of a prefixed block."
  (let ((containing-sexp (or (bound-and-true-p containing-sexp)
                             (ess-containing-sexp-position))))
    (or (ess-save-excursion-when-nil
          (and (ess-goto-char containing-sexp)
               (looking-at "{")
               (ess-climb-block-prefix call)))
        (ess-escape-unbraced-block call))))

(defun ess-escape-unbraced-block (&optional call)
  (ess-save-excursion-when-nil
    (while (and (not (ess-unbraced-block-p))
                (or (ess-escape-continuations)
                    (ess-escape-call))))
    (ess-climb-block-prefix call)))

(defun ess-jump-block ()
  (cond
   ;; if-else blocks
   ((ess-jump-if-else))
   ;; Prefixed blocks such as `function() {}'
   ((ess-behind-prefixed-block-p)
    (ess-jump-prefixed-block))
   ;; Naked blocks
   ((and (or (looking-at "{")
             (ess-behind-block-paren-p))
         (ess-forward-sexp)))))

(defun ess-jump-prefixed-block (&optional call)
  (ess-save-excursion-when-nil
    (when (ess-behind-prefixed-block-p call)
      (ess-forward-sexp 2)
      (ess-skip-blanks-forward t)
      (if (looking-at "{")
          (ess-forward-sexp)
        (prog1 (ess-jump-expression)
          (ess-jump-continuations))))))


;;;*;;; Calls

(defun ess-call-closing-p ()
  (save-excursion
    (when (cond ((looking-at ")")
                 (ess-up-list -1))
                ((looking-at "]")
                 (when (ess-up-list -1)
                   (prog1 t (ess-climb-chained-delims)))))
      (ess-ahead-attached-name-p))))

(defun ess-behind-call-opening-p (pattern)
  (and (looking-at pattern)
       (ess-ahead-attached-name-p)))

;; Should be called just before the opening brace
(defun ess-ahead-attached-name-p ()
  (save-excursion
    (ess-climb-object)))

(defun ess-ahead-param-assign-p ()
  "Return non-nil if looking at a function argument.
To be called just before the `=' sign."
  (ess-refined-token= (ess-token-before) "param-assign"))

(defun ess-behind-arg-p ()
  (save-excursion
    (ess-jump-arg)))

(defun ess-behind-parameter-p ()
  (save-excursion
    (ess-jump-parameter)))

(defun ess-jump-parameter ()
  (ess-save-excursion-when-nil
    (and (ess-jump-name)
         (when (looking-at "[ \t]*=\\([^=]\\)")
           (goto-char (match-beginning 1))
           (ess-skip-blanks-forward)
           t))))

(defun ess-jump-arg ()
  (ess-save-excursion-when-nil
    (ess-skip-blanks-forward t)
    (ess-any ((ess-jump-parameter))
             ((ess-jump-expression))
             ((ess-jump-continuations)))))

(defun ess-arg-bounds ()
  "Should be called in front of the argument."
  (save-excursion
    (let ((beg (point)))
      (and (ess-jump-arg)
           (list beg (point))))))

(defun ess-climb-call (&optional call)
  "Climb functions (e.g. ggplot) and parenthesised expressions."
  (or (ess-while (ess-save-excursion-when-nil
                   (ess-climb-name)
                   (and (ess-climb-chained-delims ?\])
                        ;; (ess-climb-expression)
                        (if (eq (char-before) ?\))
                            (ess-climb-call)
                          (ess-climb-name))
                        )))
      (ess-save-excursion-when-nil
        (when (and (memq (char-before) '(?\] ?\) ?\}))
                   (ess-backward-sexp))
          (if call
              (and (ess-climb-name)
                   (looking-at call)))
          (prog1 t
            (ess-climb-name))))))

(defun ess-climb-call-name (&optional call)
  (ess-save-excursion-when-nil
    (ess-jump-name)
    (ess-skip-blanks-forward)
    (and (ess-behind-call-opening-p "[[(]")
         (ess-climb-name)
         (or (null call)
             (looking-at call)))))

(defun ess-step-to-first-arg ()
  (let ((containing-sexp (ess-containing-sexp-position)))
    (cond ((ess-inside-call-p)
           (goto-char containing-sexp)
           (forward-char)
           t)
          ((ess-inside-call-name-p)
           (ess-jump-name)
           (ess-skip-blanks-forward)
           (forward-char)
           t))))

(defun ess-jump-to-next-arg ()
  (and (ess-jump-arg)
       (prog1 (ess-jump-char ",")
         (ess-skip-blanks-forward t))))

(defun ess-jump-call ()
  (ess-save-excursion-when-nil
    (or (and (ess-jump-object)
             (cond ((eq (char-before) ?\)))
                   ((looking-at "\\[")
                    (ess-jump-chained-brackets))
                   ((looking-at "(")
                    (ess-forward-sexp))))
        (and (looking-at "[ \t]*(")
             (ess-forward-sexp)))))

(defun ess-behind-call-p ()
  (save-excursion
    (ess-jump-object)
    (ess-skip-blanks-forward)
    (looking-at "[[(]")))

(defun ess-climb-chained-delims (&optional delim)
  "Should be called with point between delims, e.g. `]|['."
  (setq delim (if delim
                  (list delim)
                '(?\] ?\))))
  (ess-while (ess-save-excursion-when-nil
               (when (memq (char-before) delim)
                 (ess-backward-sexp)))))

(defun ess-jump-chained-brackets ()
  (ess-while (ess-save-excursion-when-nil
               (when (eq (char-after) ?\[)
                 (ess-forward-sexp)))))

(defun ess-escape-call (&optional call)
  (let ((containing-sexp (ess-containing-sexp-position)))
    (if (ess-inside-call-p)
        (ess-save-excursion-when-nil
          (goto-char containing-sexp)
          (ess-climb-chained-delims)
          (and (ess-climb-name)
               (or (null call)
                   (looking-at call))))
      ;; At top level or inside a block, check if point is on the
      ;; function name.
      (ess-save-excursion-when-nil
        (let ((orig-pos (point)))
          (and (ess-jump-name)
               (looking-at "[[(]")
               (ess-climb-name)
               (or (null call)
                   (looking-at call))
               (/= (point) orig-pos)))))))

(defun ess-escape-calls ()
  (ess-while (ess-escape-call)))

(defun ess-jump-inside-call ()
  (ess-save-excursion-when-nil
    (when (ess-jump-name)
      (ess-skip-blanks-forward)
      (when (looking-at "(")
        (forward-char)
        t))))

(defun ess-args-bounds (&optional marker)
  (let ((containing-sexp (ess-containing-sexp-position)))
    (when (ess-inside-call-p)
      (save-excursion
        (let ((beg (1+ containing-sexp))
              (call-beg (ess-at-containing-sexp
                          (ess-climb-name)
                          (point))))
          ;; (ess-up-list) can't find its way when point is on a
          ;; backquoted name, so start from `beg'.
          (and (goto-char beg)
               (ess-up-list)
               (prog1 t
                 (forward-char -1))
               (let ((end (if marker
                              (point-marker)
                            (point))))
                 (list beg end call-beg))))))))

(defun ess-args-alist ()
  "Return all arguments as an alist with cars set to argument
names and cdrs set to the expressions given as argument. Both
cars and cdrs are returned as strings."
  (save-excursion
    (when (ess-step-to-first-arg)
      (let (args current-arg)
        (while (and (setq current-arg (ess-cons-arg))
                    (setq args (nconc args (list current-arg)))
                    (ess-jump-to-next-arg)))
        args))))

(defun ess-cons-arg ()
  "Return a cons cell of the current argument with car set to the
parameter name (nil if not specified) and cdr set to the argument
expression."
  (save-excursion
    (ess-skip-blanks-forward t)
    (let ((param (when (ess-behind-parameter-p)
                   (buffer-substring-no-properties
                    (point)
                    (prog2
                        (ess-jump-name)
                        (point)
                      (ess-jump-char "=")
                      (ess-skip-blanks-forward)))))
          (arg (buffer-substring-no-properties
                (point)
                (progn
                  (ess-jump-arg)
                  (point)))))
      (cons param arg))))


;;;*;;; Statements

(defun ess-behind-operator-p (&optional strict)
  (ess-token-operator-p (ess-token-after) strict))

(defun ess-ahead-operator-p (&optional strict)
  (ess-token-operator-p (ess-token-before) strict))

(defun ess-climb-lhs (&optional no-fun-arg climb-line)
  (ess-save-excursion-when-nil
    (let ((start-line (line-number-at-pos)))
      (ess-climb-operator)
      (when (and (or climb-line (equal (line-number-at-pos) start-line))
                 (ess-behind-definition-op-p no-fun-arg))
        (prog1 t
          (ess-climb-expression))))))

(defun ess-jump-lhs ()
  (ess-save-excursion-when-nil
    (and (ess-jump-name)
         (ess-behind-definition-op-p)
         (ess-jump-operator))))

(defun ess-climb-operator ()
  (when (ess-token-operator-p (ess-token-before))
    (prog1 (ess-climb-token)
      (ess-skip-blanks-backward))))

;; Currently doesn't check that the operator is not binary
(defun ess-climb-unary-operator ()
  (ess-save-excursion-when-nil
    (let ((token (ess-climb-token)))
      (member (ess-token-type token) '("+" "-" "!" "?" "~")))))

;; Currently returns t if we climbed lines, nil otherwise.
(defun ess-climb-continuations (&optional cascade ignore-ifelse)
  (let* ((start-line (line-number-at-pos))
         (state (list :start-line start-line
                      :last-line start-line
                      :moved 0
                      :last-pos (point)
                      :prev-point nil
                      :def-op nil
                      :expr nil)))
    (when (ess-while (and (<= (plist-get state :moved) 1)
                          (or (ess-save-excursion-when-nil
                               (and (ess-climb-operator)
                                    (ess-climb-continuations--update-state state cascade 'op)
                                    (ess-climb-expression ignore-ifelse)))
                              (ess-climb-unary-operator))
                          (/= (plist-get state :last-pos) (point)))
                     (ess-climb-continuations--update-state state cascade nil)
                     (plist-put state :last-pos (point)))
      (when (and (plist-get state :prev-point)
                 (or (= (plist-get state :moved) 3)
                     (not (plist-get state :expr))))
        (goto-char (plist-get state :prev-point)))
      (if (plist-get state :def-op)
          'def-op
        (< (line-number-at-pos) (plist-get state :start-line))))))

(defun ess-climb-continuations--update-state (state cascade &optional op)
  ;; Climbing multi-line expressions should not count as moving up
  (when op
    (plist-put state :expr (ess-ahead-closing-p)))
  (let ((cur-line (line-number-at-pos)))
    (when (and (plist-get state :last-line)
               (< cur-line (plist-get state :last-line))
               (or cascade (not (plist-get state :expr))))
      (plist-put state :moved (1+ (plist-get state :moved)))
      (plist-put state :last-line cur-line)))
  ;; Don't update counter after climbing operator or climbing too high
  (when (and (not op)
             (<= (plist-get state :moved) 1))
    (plist-put state :prev-point (point)))
  (when (and (ess-behind-definition-op-p)
             (<= (plist-get state :moved) 1))
    (plist-put state :def-op t))
  t)

(defun ess-jump-operator ()
  (when (ess-behind-operator-p)
    (ess-jump-token)
    (ess-skip-blanks-forward t)
    t))

(defun ess-jump-continuation ()
  (and (ess-jump-operator)
       (ess-jump-expression)))

(defun ess-jump-continuations ()
  (let (last-pos)
    (when (ess-while (and (or (null last-pos)
                              (/= (point) last-pos))
                          (setq last-pos (point))
                          (ess-jump-continuation)))
      ;; In calls, operators can start on newlines
      (let ((start-line (line-number-at-pos)))
        (when (ess-save-excursion-when-nil
                (and (ess-inside-call-p)
                     (ess-skip-blanks-forward t)
                     (/= (line-number-at-pos) start-line)
                     (ess-behind-operator-p)))
          (ess-jump-continuations)))
      t)))

(defun ess-ahead-continuation-p (&optional or-parameter)
  (or (ess-token-operator-p (ess-token-before) (not or-parameter))
      (save-excursion
        (ess-climb-block-prefix))
      (ess-token-after= "else")
      (save-excursion
        (ess-climb-if-else-call))))

(defun ess-token-definition-op-p (token strict)
  (and (ess-token= token '("<-" "<<-" ":=" "~" "="))
       (if strict
           (not (ess-refined-token= token "param-assign"))
         t)))

(defun ess-behind-definition-op-p (&optional strict)
  (ess-token-definition-op-p (ess-token-after) strict))

(defun ess-ahead-definition-op-p (&optional strict)
  (ess-token-definition-op-p (ess-token-before) strict))

(defun ess-behind-assignment-op-p ()
  (let ((token (ess-token-after)))
    (and (ess-token= token '("<-" "="))
         (not (ess-refined-token= token "param-assign")))))

(defun ess-escape-continuations ()
  (ess-any ((unless (ess-ahead-boundary-p)
              (ess-climb-expression)))
           ((ess-while (ess-climb-continuations)))))

(defun ess-continuations-bounds (&optional marker)
  (save-excursion
    (let ((beg (progn
                 (ess-escape-continuations)
                 (point))))
      (when beg
        (ess-jump-expression)
        (ess-jump-continuations)
        (let ((end (if marker
                       (point-marker)
                     (point))))
          (list beg end))))))

(defun ess-climb-to-top-level ()
  (while (ess-goto-char (ess-containing-sexp-position)))
  (ess-escape-continuations))


;;;*;;; Statements: Control Flow

(defun ess-climb-if-else-call (&optional multi-line)
  "Climb if, else, and if else calls."
  (ess-save-excursion-when-nil
    (cond ((ess-climb-paired-delims ")")
           (when (ess-climb-token "if")
             ;; Check for `else if'
             (prog1 t
               (ess-save-excursion-when-nil
                 (let ((orig-line (line-number-at-pos)))
                   (and (ess-climb-token "else")
                        (or multi-line
                            (eq orig-line (line-number-at-pos)))))))))
          ((ess-climb-token "else")))))


(defun ess-climb-if-else-body (&optional from-else)
  (cond
   ;; Climb braced body
   ((ess-save-excursion-when-nil
      (and (when (progn (ess-skip-blanks-backward t)
                        (eq (char-before) ?\}))
             (prog1 t (forward-char -1)))
           (ess-up-list -1))))
   ;; Climb unbraced body
   ((when from-else
      (ess-save-excursion-when-nil
        (ess-skip-blanks-backward t)
        (prog1 (ess-climb-expression 'ignore-ifelse)
          (or (ess-climb-continuations nil 'ignore-ifelse)
              (ess-climb-block-prefix nil 'ignore-ifelse))))))))

(defun ess-climb-if-else (&optional to-start)
  "Climb horizontal as well as vertical if-else chains, with or
without curly braces."
  ;; Don't climb if we're atop the current chain of if-else
  (unless (ess-token-after= "if")
    (ess-save-excursion-when-nil
      (let ((from-else (ess-token-after= "else")))
        (when (and (ess-climb-if-else-body from-else)
                   (ess-climb-if-else-call to-start))
          ;; If we start from a final else and climb to another else, we
          ;; are in the wrong chain of if-else. In that case,
          ;; climb-recurse to the top of the current chain and climb
          ;; again to step in the outer chain.
          (when (save-excursion (and from-else
                                     (ess-jump-token "else")
                                     (not (ess-jump-token "if"))))
            (ess-climb-if-else 'to-start)
            (ess-climb-continuations)
            (ess-climb-block-prefix nil 'ignore-ifelse)
            (ess-climb-if-else-call nil))
          (ess-maybe-climb-broken-else)
          (when to-start
            (ess-climb-if-else to-start))
          t)))))

;; Broken else: if \n else
(defun ess-maybe-climb-broken-else (&optional same-line)
  (ess-save-excursion-when-nil
    ;; Don't record current line if not needed (expensive operation)
    (let ((cur-line (when same-line (line-number-at-pos))))
      (and (ess-climb-token "else")
           (if same-line
               (= cur-line (line-number-at-pos))
             t)))))

(defun ess-skip-curly-backward ()
  (re-search-backward "}[ \t]*" (line-beginning-position) t))

(defun ess-jump-if-else ()
  (let (from)
    (ess-while (ess-save-excursion-when-nil
                 (ess-skip-blanks-forward t)
                 (cond
                  ((and (not (eq from 'if))
                        (ess-jump-if)
                        (setq from 'if)))
                  ((looking-at "else")
                   (ess-forward-sexp)
                   (or (ess-jump-if)
                       (progn
                         (ess-skip-blanks-forward t)
                         (ess-jump-expression)))
                   (setq from 'else))
                  (t
                   nil))))))

(defun ess-jump-if ()
  (ess-save-excursion-when-nil
    (ess-skip-blanks-forward t)
    (and (looking-at "if[ \t\n]*(")
         (ess-forward-sexp 2)
         (progn
           (ess-skip-blanks-forward t)
           (ess-jump-expression)))))


;;;*;;; Function Declarations

(defun ess-behind-defun-p ()
  (or (looking-at "function[ \t]*(")
      (ess-behind-enclosed-defun-p)))

(defun ess-behind-enclosed-defun-p ()
  (save-excursion
    (and (ess-behind-call-p)
         (ess-jump-inside-call)
         (cl-some (lambda (arg)
                    (string-match "^function\\b"
                                  (cdr arg)))
                  (ess-args-alist)))))


;;;*;;; Names / Objects / Expressions

;; Should  climb any names, including backquoted ones or those
;; containing `@' or `$'. Difficult to achieve with regexps, but
;; skipping chars is faster anyway.
(defun ess-climb-object ()
  (ess-save-excursion-when-nil
    (let (climbed)
      (ess-skip-blanks-backward)
      ;; Backquoted names can contain any character
      (if (and (memq (char-before) '(?` ?\" ?\'))
               (ess-backward-sexp))
          (setq climbed t)
        (while (cl-some (apply-partially '/= 0)
                        `(,(skip-syntax-backward "w_")
                          ,(skip-chars-backward "\"'")))
          (setq climbed t)))
      ;; Recurse if we find an indexing char
      (let ((tok (ess-token-before)))
        (when (member (ess-token-type tok) '("$" "@" "::" ":::"))
          (goto-char (ess-token-start tok))
          (ess-climb-object)))
      climbed)))

;; Todo: split name and object climbing
(defun ess-climb-name ()
  (ess-climb-object))

;; This jumps both object names and atomic objects like strings or
;; numbers.
(defun ess-jump-object ()
  (cond
   ;; Jump over object names
   ((ess-jump-name))
   ;; Jump over strings))
   ((ess-save-excursion-when-nil
      (skip-chars-forward " \t")
      (memq (char-after) '(?\" ?\')))
    (ess-forward-sexp))))

(defun ess-jump-name ()
  (ess-save-excursion-when-nil
    (let (climbed)
      (skip-chars-forward " \t")
      ;; Jump over backquoted names
      (cond ((and (eq (char-after) ?`)
                  (looking-back ess-r-symbol-pattern
                                (1- (point))))
             (forward-char)
             (setq climbed t))
            ((eq (char-after) ?`)
             (forward-char)
             (when (ess-while (not (memq (char-after) '(?` ?\C-J)))
                     (forward-char))
               (setq climbed t)
               (forward-char)))
            ;; Jump over regular names
            ((when (/= 0 (skip-syntax-forward "w_"))
               ;; Maybe point was inside backticks
               (when (eq (char-after) ?`)
                 (forward-char))
               (setq climbed t))))
      climbed)))

(defun ess-climb-expression (&optional ignore-ifelse)
  (ess-save-excursion-when-nil
    (or (ess-climb-block ignore-ifelse)
        (ess-climb-call)
        (ess-climb-object))))

(defun ess-jump-expression ()
  (or (ess-jump-block)
      (ess-jump-call)
      (ess-jump-object)))

(provide 'ess-r-syntax)

;;; ess-r-syntax.el ends here
