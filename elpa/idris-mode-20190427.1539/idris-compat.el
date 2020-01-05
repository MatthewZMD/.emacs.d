;;; idris-compat.el --- compatibility functions for Emacs 24.1 -*- lexical-binding: t -*-

;;; Commentary:
;; This file defines defvar-local, which was introduced in Emacs 24.3, and string-suffix-p, from Emacs 24.4.

;;; Code:
(require 'subr-x nil 'no-error)   ; Additional utilities, Emacs 24.4 and upwards

(eval-and-compile
  (unless (featurep 'subr-x)
    ;; `subr-x' function for Emacs 24.3 and below
    (defsubst string-blank-p (string)
      "Check whether STRING is either empty or only whitespace."
      (string-match-p "\\`[ \t\n\r]*\\'" string))))

(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

;;; The following function is copyright FSF, from GNU Emacs 24.4 source code.
(unless (fboundp 'string-suffix-p)
  (defun string-suffix-p (suffix string  &optional ignore-case)
    "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
    (let ((start-pos (- (length string) (length suffix))))
      (and (>= start-pos 0)
           (eq t (compare-strings suffix nil nil
                                  string start-pos nil ignore-case))))))

(provide 'idris-compat)
;;; idris-compat.el ends here
