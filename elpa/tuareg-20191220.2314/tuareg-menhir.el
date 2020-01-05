;;; tuareg-menhir.el --- Support for Menhir (and Ocamlyacc) source code  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode to edit Menhir (and Ocamlyacc) source files.

;; Currently provides:
;; - Font-lock highlighting
;; - Automatic indentation
;; - Imenu

;;; Code:

(require 'cl-lib)
(require 'tuareg)

(defgroup tuareg-menhir ()
  "Major mode to edit Menhir source files."
  :group 'tuareg)

(defvar tuareg-menhir-mode-syntax-table
  (let ((st (make-syntax-table tuareg-mode-syntax-table)))
    ;; Menhir comments are hellish: can be C, C++, or OCaml style!
    ;; FIXME: C/C++ style comments aren't allowed inside the OCaml part of the code.
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st))

(defun tuareg-menhir--in-ocaml-p ()
  "Return non-nil if point is within OCaml code."
  (let ((pos (car (nth 9 (syntax-ppss)))))
    (and pos (eq ?\{ (char-after pos)))))

(defconst tuareg-menhir--keywords
  '("parameter" "token" "nonassoc" "left" "right" "type" "start" "on_error_reduce"))

;;;; Indentation

(defcustom tuareg-menhir-basic-indent 2
  "Default basic indentation step for Menhir files."
  :type 'integer)

(defcustom tuareg-menhir-rule-indent tuareg-menhir-basic-indent
  "Indentation column of rules."
  :type 'integer)

(defcustom tuareg-menhir-action-indent tuareg-menhir-basic-indent
  "Indentation action w.r.t rules."
  :type 'integer)

(defun tuareg-menhir--indent-column ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ((looking-at "\\(?:\\sw\\|\\s_\\)+:") 0)
     ((looking-at "|") tuareg-menhir-rule-indent)
     ((looking-at "{")
      (+ tuareg-menhir-rule-indent tuareg-menhir-action-indent))
     (t 0))))

(defun tuareg-menhir--indent-ocaml ()
  (let ((smie-rules-function #'tuareg-smie-rules)
        (smie-grammar tuareg-smie-grammar)
        (smie-forward-token-function #'tuareg-smie-forward-token)
        (smie-backward-token-function #'tuareg-smie-backward-token))
    (smie-indent-line)))

(defun tuareg-menhir--indent (&optional _)
  (if (save-excursion (beginning-of-line)
                      (tuareg-menhir--in-ocaml-p))
      (tuareg-menhir--indent-ocaml)
    (let ((col (tuareg-menhir--indent-column)))
      (if (save-excursion (skip-chars-backward " \t") (bolp))
          (indent-line-to col)
        (save-excursion (indent-line-to col))))))

;;;; Font-lock

(defvar tuareg-menhir-font-lock-keywords
  `(("^\\(\\(?:\\sw\\|\\s_\\)+\\):" (1 font-lock-function-name-face))
    (,(concat "^%\\(?:%\\|" (regexp-opt tuareg-menhir--keywords) "\\_>\\)")
     (0 font-lock-builtin-face))
    ("%\\(?:prec\\|public\\|inline\\)\\_>"
     (0 (unless (tuareg-menhir--in-ocaml-p) font-lock-builtin-face)))))

;;;; Imenu

(defvar tuareg-menhir-imenu-generic-expression
  '((nil "^\\(\\(?:\\sw\\|\\s_\\)+\\):" 1)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-menhir-mode))

;;;###autoload
(define-derived-mode tuareg-menhir-mode prog-mode "Menhir"
  "Major mode to edit Menhir (and Ocamlyacc) files."
  (setq-local indent-line-function #'tuareg-menhir--indent)
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq-local comment-start-skip "\\(?:[(/]\\*+\\|//+\\)[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\\*+[/)]\\)?")
  (setq-local font-lock-defaults '(tuareg-menhir-font-lock-keywords))
  (setq-local imenu-generic-expression tuareg-menhir-imenu-generic-expression)
  )

(provide 'tuareg-menhir)
;;; tuareg-menhir.el ends here
