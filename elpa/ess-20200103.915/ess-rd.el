;; ess-rd.el --- Support for editing R documentation (Rd) source  -*- lexical-binding: t; -*-

;; Copyright (C) 1997--2005  A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
;; Created: 25 July 1997
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS (Emacs Speaks Statistics).

;; This file is free software; you may redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; A copy of the GNU General Public License is available on the World
;; Wide Web at https://www.gnu.org/copyleft/gpl.html.  You can also
;; obtain it by writing to the Free Software Foundation, Inc., 675 Mass
;; Ave, Cambridge, MA 02139, USA.


;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'subr-x))

(require 'ess-help)
(require 'ess-inf)
;; Silence the byte compiler, see TODO below; can we remove these?
(defvar ess-help-r-sec-regex)
(defvar ess-help-r-sec-keys-alist)
(defvar ess-r-customize-alist)

(defcustom Rd-mode-hook nil
  "Hook to be run when Rd mode is entered."
  :type 'hook
  :group 'ess-R
  :group 'ess-hooks)

(define-abbrev-table 'Rd-mode-skeleton-abbrev-table
  '(("`ag" "\\arguments" nil :system t)
    ("`al" "\\alias" nil :system t)
    ("`au" "\\author" nil :system t)
    ("`bf" "\\bold" nil :system t)
    ("`co" "\\code" nil :system t)
    ("`de" "\\describe" nil :system t)
    ("`dn" "\\description" nil :system t)
    ("`dt" "\\details" nil :system t)
    ("`em" "\\emph" nil :system t)
    ("`en" "\\enumerate" nil :system t)
    ("`ex" "\\examples" nil :system t)
    ("`fi" "\\file" nil :system t)
    ("`fo" "\\format" nil :system t)
    ("`it" "\\item" nil :system t)
    ("`iz" "\\itemize" nil :system t)
    ("`kw" "\\keyword" nil :system t)
    ("`li" "\\link" nil :system t)
    ("`me" "\\method" nil :system t)
    ("`na" "\\name" nil :system t)
    ("`no" "\\note" nil :system t)
    ("`re" "\\references" nil :system t)
    ("`sa" "\\seealso" nil :system t)
    ("`se" "\\section" nil :system t)
    ("`so" "\\source" nil :system t)
    ("`ss" "\\subsection" nil :system t)
    ("`sy" "\\synopsis" nil :system t)
    ("`ta" "\\tabular" nil :system t)
    ("`ti" "\\title" nil :system t)
    ("`us" "\\usage" nil :system t)
    ("`va" "\\value" nil :system t))
  "Abbrev table for R documentation keywords.
All Rd mode abbrevs start with a grave accent (`)."
  :case-fixed t)

(define-abbrev-table 'Rd-mode-abbrev-table ()
  "Abbrev table for Rd mode."
  :parents (list Rd-mode-skeleton-abbrev-table))

(defvar Rd-mode-syntax-table
  (let ((tab (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\\ "\\" tab)
    (modify-syntax-entry ?\{ "(}" tab)
    (modify-syntax-entry ?\} "){" tab)
    ;; Nice for editing, not for parsing ...
    (modify-syntax-entry ?\( "()" tab)
    (modify-syntax-entry ?\) ")(" tab)
    (modify-syntax-entry ?\[ "(]" tab)
    (modify-syntax-entry ?\] ")[" tab)
    ;; To get strings right
    ;; (modify-syntax-entry ?\' "\"" Rd-mode-syntax-table)
    (modify-syntax-entry ?\" "\"" tab)
    ;; To make abbrevs starting with a grave accent work ...
    (modify-syntax-entry ?\` "w" tab)
    ;; Comments
    (modify-syntax-entry ?\% "<" tab)
    (modify-syntax-entry ?\n ">" tab)
    tab)
  "Syntax table for `Rd-mode'.")

(defvar Rd-mode-parse-syntax-table
  (let ((tab (copy-syntax-table Rd-mode-syntax-table)))
    ;; To make parse-partial-sexps do the thing we want for computing
    ;; indentations
    (modify-syntax-entry ?\( "_" tab)
    (modify-syntax-entry ?\) "_" tab)
    (modify-syntax-entry ?\[ "_" tab)
    (modify-syntax-entry ?\] "_" tab)
    tab)
  "Syntax table for parsing Rd mode.")

(defvar Rd-section-names
  '("Rdversion" "arguments" "alias" "author" "concept" "describe" "description"
    "details" "docType" "encoding" "enumerate" "examples" "format"
    "itemize" "keyword" "name" "note" "preformatted" "references"
    "seealso" "section" "source" "subsection" "synopsis"
    "tabular" "title" "usage"
    "value"))

(defvar Rd-keywords
  '(
    ;; the next two lines: only valid in R <= 2.8.1
    ;; commented out on 2011-01-14 for ESS version 5.13:
    ;; "Alpha" "Gamma" "alpha" "beta" "epsilon" "lambda" "mu" "pi" "sigma"
    ;; "ge" "le" "left" "right"
    ;;
    "R" "RdOpts" "S3method" "S4method" "Sexpr" "acronym"
    "bold" "cite" "code" "command" "cr" "dQuote" "deqn" "dfn" "dontrun"
    "dontshow" "donttest" "dots" "email" "emph" "enc" "env" "eqn" "figure" "file"
    "href" "if" "ifelse"
    "item" "kbd" "ldots" "linkS4class" "link" "method"
    "newcommand" "option" "out"
    "pkg" "sQuote" "renewcommand"
    "samp" "strong" "tab" "url" "var" "verb"
    ;; System macros (from <R>/share/Rd/macros/system.Rd ):
    "CRANpkg" "PR" "sspace" "doi"
    "packageTitle" "packageDescription" "packageAuthor"
    "packageMaintainer" "packageDESCRIPTION" "packageIndices"
    ))

(defvar Rd-font-lock-keywords
  (list
   (cons
    (concat "\\\\\\("
            (mapconcat 'identity Rd-section-names "\\|")
            "\\>\\)")
    'font-lock-reference-face) ; Rd-bold-face
   (cons
    (concat "\\\\\\("
            (mapconcat 'identity Rd-keywords "\\|")
            "\\>\\)")
    'font-lock-keyword-face)
   '("^#\\(ifn?def\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-builtin-face)
     (2 font-lock-variable-name-face nil t))
   '("^#\\(endif\\)" 1 font-lock-builtin-face))
  "Additional Rd expressions to highlight.")

(defvar Rd-indent-level 2
  "Indentation of Rd code with respect to containing blocks.")

(defvar Rd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" #'indent-according-to-mode)
    (define-key map "\C-j" #'reindent-then-newline-and-indent)
    (define-key map "\C-m" #'reindent-then-newline-and-indent)
    (define-key map "\C-c\C-p" #'Rd-preview-help)
    (define-key map "\C-c\C-j" #'Rd-mode-insert-item)
    (define-key map "\C-c\C-e" #'Rd-mode-insert-skeleton)
    (define-key map "\C-c\C-f" #'Rd-font)
    (define-key map "\C-c\C-s" #'Rd-mode-insert-section)
    (define-key map "\C-c\C-n" #'ess-eval-line-visibly-and-step)
    (define-key map "\C-c\C-r" #'ess-eval-region)
    (define-key map "\C-c\C-c" #'ess-eval-region-or-function-or-paragraph-and-step)
    (define-key map "\C-\M-x"  #'ess-eval-region-or-function-or-paragraph)
    (define-key map "\C-c\C-v" #'ess-display-help-on-object)
    (define-key map "\C-c\C-w" #'ess-switch-process)
    (define-key map "\C-c\C-y" #'ess-switch-to-ESS)
    (define-key map "\C-c\C-z" #'ess-switch-to-end-of-ESS)
    map)
  "Keymap used in Rd mode.")

(defvar Rd-mode-menu
  (list "Rd"
        ["Markup [word]"                Rd-font t]
        ["Insert Item"                  Rd-mode-insert-item t]
        ["Insert Section"               Rd-mode-insert-section t]
        ["Insert Skeleton"              Rd-mode-insert-skeleton t]
        "-"
        ["Preview"                      Rd-preview-help t]
        "-"
        ["Eval Line"                    ess-eval-line-visibly-and-step t]
        ["Eval Region"                  ess-eval-region t]
        ["Switch to ESS Process"        ess-switch-to-ESS t]
        ["Switch the ESS Process"       ess-switch-process t]
        ["Switch to end{ESS Pr}"        ess-switch-to-end-of-ESS t]
        "-"
        ["Toggle Abbrev Mode"           abbrev-mode t]
        ["Toggle Auto-Fill Mode"        auto-fill-mode t]
        "-"
        ["Submit Bug Report"            ess-submit-bug-report t]
        "-"
        ["Describe Rd Mode"             describe-mode t])
  "Menu used in Rd mode.")

(defvar Rd-to-help-command "R CMD Rd2txt"
  "Shell command for converting R documentation source to help text.")

(defvar Rd-font-list
  '((?\C-b "\\bold{"    "}")
    (?\C-c "\\code{"    "}")
    (?\C-e "\\emph{"    "}")
    (?\C-l "\\link{"    "}")
    (?l "\\code{\\link{" "}}")
    (?\C-m "\\email{"   "}")
    (?\C-q "\\eqn{"     "}")
    (?\C-u "\\url{"     "}")
    )
  "List of \"fonts\" used by `Rd-font'.
Each entry is a list. The first element is the key to activate
the font. The second element is the string to insert before
point, and the third element is the string to insert after
point.")


;;;###autoload
(define-derived-mode Rd-mode text-mode "Rd"
  "Major mode for editing R documentation source files.

Type \\[list-abbrevs] to display the built-in abbrevs for Rd
keywords.To automatically turn on the abbrev(iate) features, add
the following to your Emacs configuration file:

  (add-hook 'Rd-mode-hook #'abbrev-mode)"
  (setq ess-language "S" ess-dialect  "R")
  (require 'ess-r-mode)
  (ess-setq-vars-local ess-r-customize-alist)

  (setq-local indent-line-function 'Rd-mode-indent-line)
  (setq fill-column 72)
  (setq-local comment-start-skip "\\s<+\\s-*")
  (setq-local comment-start "% ")
  (setq-local comment-end "")
  (setq font-lock-defaults
        '(Rd-font-lock-keywords nil nil))

  ;; Here is a workaround for an Emacs bug related to indirect buffers and
  ;; spurious lockfiles that rears its ugly head with .Rd files
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2013-02/msg01368.html
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=14328
  (setq-local create-lockfiles nil)

  (easy-menu-define Rd-mode-menu-map Rd-mode-map
    "Menu keymap for Rd mode." Rd-mode-menu)

  (turn-on-auto-fill))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.Rd\\'" . Rd-mode))

(defun Rd-describe-major-mode ()
  "Describe the current major mode."
  (declare (obsolete describe-mode "ESS 19.04"))
  (describe-function major-mode))

(defun Rd-mode-in-verbatim-p ()
  "Return non-nil if in a usage, examples, or synopsis."
  (let ((pos (point)))
    (save-excursion
      (if (and (re-search-backward
                "\\\\\\(usage\\|examples\\|synopsis\\)" nil t)
               (re-search-forward "\\s(" nil t))
          (condition-case ()
              (progn
                (up-list 1)
                (< pos (point)))
            (error t))
        nil))))

(defun Rd-mode-in-preprocessor-line-p ()
  "Return non-nil if in a preprocessor line."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*#\\(ifdef\\|endif\\)")))

(defun Rd-mode-calculate-indent ()
  "Return appropriate indentation for current line in Rd mode."
  (save-excursion
    (beginning-of-line)
    (cond
     ((Rd-mode-in-verbatim-p)
      ;; Don't do anything in verbatims
      nil)
     ((Rd-mode-in-preprocessor-line-p)
      ;; Indent to 0
      0)
     (t
      (let ((p (progn
                 (re-search-forward "[ \t]*\\s)*" (point-at-eol) t)
                 (point))))
        (if (or (< (forward-line -1) 0)
                (Rd-mode-in-verbatim-p))
            0
          (set-syntax-table Rd-mode-parse-syntax-table)
          (while (and (or (looking-at "[ \t]*$")
                          (Rd-mode-in-preprocessor-line-p))
                      (not (bobp)))
            (forward-line -1))
          (re-search-forward "[ \t]*\\s)*" (point-at-eol) t)
          (prog1
              (+ (current-indentation)
                 (* (car (parse-partial-sexp (point) p))
                    Rd-indent-level))
            (set-syntax-table Rd-mode-syntax-table))))))))

(defun Rd-mode-indent-line ()
  "Indent current line as Rd source."
  (when-let ((ic (Rd-mode-calculate-indent))
             (rp (- (current-column) (current-indentation))))
    (when (< ic 0)
      (error "Unmatched parenthesis"))
    (indent-line-to ic)
    (when (> rp 0)
      (move-to-column (+ ic rp)))))

(defun Rd-mode-insert-item ()
  "Insert \\item{ on a newline."
  (interactive)
  (reindent-then-newline-and-indent)
  (insert "\\item{")
  )

(defun Rd-mode-insert-section ()
  "Insert a section from `Rd-section-names'."
  (interactive)
  (let ((s (ess-completing-read
            "Insert section: "
            (mapcar (lambda (x) (cons x x)) Rd-section-names)
            nil t)))
    (if (string= s "")
        (progn (insert "\\section{}{") (backward-char 2))
      (insert (format "\\%s{" s)))))

(defun Rd-mode-insert-skeleton ()
  "Insert several empty Rd fields."
  (interactive)
  ;; Hmm: in theory this should be kept in sync with prompt()
  ;; ---  maybe using prompt() [or promptClass()...] would be better anyway?!
  (insert "\\name{}\n")
  (insert "\\alias{}\n")
  (insert "\\title{}\n")
  (insert "\\description{\n}\n")
  (insert "\\usage{\n}\n")
  (insert "\\arguments{\n}\n")
  (insert "\\value{\n}\n")
  (insert "\\details{\n}\n")
  (insert "\\references{\n}\n")
  (insert "\\seealso{\n}\n")
  (insert "\\examples{\n}\n")
  (insert "\\author{}\n")
  (insert "\\keyword{}\n"))

;; This is an `easy' version of (defun TeX-font ..) in AUCtex's  tex.el ;
;;  see TeX-font-list and also LaTeX-font-list in latex.el

(defun Rd-font (what)
  "Insert template for font command.
WHAT determines the font to use, as specified by `Rd-font-list'."
  (interactive "c")
  ;;TeX had : (Rd-update-style)
  (let* ((entry (assoc what Rd-font-list))
         (before (nth 1 entry))
         (after (nth 2 entry)))
    (cond ((null entry) ;; help on possibilities :
           (let ((help
                  (concat
                   "Rd Markup (available from C-c C-f):\n\n\t"
                   "KEY          Rd-Markup\n\n"
                   (mapconcat
                    (lambda (entry)
                      ;; A textual description of an ENTRY in TeX-font-list.
                      (concat (format "%11s  "
                                      (key-description
                                       (char-to-string (nth 0 entry))))
                              (format "%14s %-3s"
                                      (nth 1 entry) (nth 2 entry))))
                    Rd-font-list "\n"))))
             (with-output-to-temp-buffer "*Help*"
               (set-buffer "*Help*")
               (insert help))))

          ((region-active-p)
           (save-excursion
             (cond ((> (mark) (point))
                    (insert before)
                    (goto-char (mark))
                    (insert after))
                   (t
                    (insert after)
                    (goto-char (mark))
                    (insert before)))))
          (t
           (insert before)
           (save-excursion
             (insert after))))))

(defun Rd-preview-help (&optional via-shell)
  "Preview the current Rd buffer contents as help.
If the current buffer is not associated with a file, create a
temporary one in variable `temporary-file-directory'."
  (declare (advertised-calling-convention () "ESS 19.04"))
  (interactive "P")                     ; If optional VIA-SHELL is set, using `Rd-to-help-command'.
  (let ((file buffer-file-name)
        (pbuf (get-buffer-create "R Help Preview"))
        del-p)
    (unless file
      (setq file (make-temp-file "RD_" nil ".Rd"))
      (write-region (point-min) (point-max) file)
      (setq del-p t))

    (if via-shell ;; FIXME eventually get rid of this option
        ;; only method in ESS <= 14.09 -- calls "R" even if in "R-devel"; slower
        (let ((shcmd (format "%s '%s'" Rd-to-help-command file)))
          (set-buffer pbuf)
          (erase-buffer)
          (ess-write-to-dribble-buffer
           (format "Rd-preview-help: (shell-command |%s| t)" shcmd))
          (shell-command shcmd t))
      ;; else directly:
      (ess-force-buffer-current "R process to use: ")
      (ess-command (format ".ess_Rd2txt(\"%s\")\n" file) pbuf)
      (set-buffer pbuf))

    ;; FIXME(2): once got rid of via-shell, consider
    ;; (ess--flush-help-into-current-buffer file "tools::Rd2txt(\"%s\")\n")
    ;; instead of all this :
    (ess-setq-vars-local ess-r-customize-alist)
    ;; mostly cut'n'paste from ess--flush-help* (see FIXME(2)):
    (ess-help-underline)
    (ess--help-major-mode)
    ;; FIXME: Is this really needed?
    (setq ess-help-sec-regex ess-help-r-sec-regex
          ess-help-sec-keys-alist ess-help-r-sec-keys-alist)
    (goto-char (point-min))
    (set-buffer-modified-p 'nil)
    (setq buffer-read-only t)
    (setq truncate-lines nil)
    (when del-p (delete-file file))
    (unless (get-buffer-window pbuf 'visible)
      (display-buffer pbuf t))))

(define-obsolete-function-alias 'Rd-submit-bug-report 'ess-submit-bug-report "2018-08-16")

(provide 'ess-rd)

;;; ess-rd.el ends here
