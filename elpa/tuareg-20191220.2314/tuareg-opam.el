;;; tuareg-opam.el --- Mode for editing opam files   -*- coding: utf-8; lexical-binding:t -*-

;; Copyright (C) 2017- Christophe Troestler

;; This file is not part of GNU Emacs.

;; Permission to use, copy, modify, and distribute this software for
;; any purpose with or without fee is hereby granted, provided that
;; the above copyright notice and this permission notice appear in
;; all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


(defvar tuareg-opam-mode-hook nil)

(defvar tuareg-opam-indent-basic 2
  "The default amount of indentation.")

(defvar tuareg-opam-flymake nil
  "It t, use flymake to lint OPAM files.")

(defvar tuareg-opam-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for tuareg-opam mode")

(defgroup tuareg-opam nil
  "Support for the OPAM files."
  :group 'languages)

;; TODO this is wrong, and doesn't respect OPAMROOT. It should probably just
;; removed.
(defconst tuareg-opam-compilers
  (when (file-directory-p "~/.opam")
    (let ((c (directory-files "~/.opam" t "[0-9]+\\.[0-9]+\\.[0-9]+")))
      (if (file-directory-p "~/.opam/system")
	        (cons "~/.opam/system" c)
	      c)))
  "The list of OPAM directories for the installed compilers.")

(defvar tuareg-opam
  (let ((opam (executable-find "opam")))
    (if opam opam
      (let ((opam (locate-file "bin/opam" tuareg-opam-compilers)))
        (if (and opam (file-executable-p opam)) opam)))) ; or nil
  "The full path of the opam executable or `nil' if opam wasn't found.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Syntax highlighting

(defface tuareg-opam-error-face
  '((t (:inherit error)))
  "Face for constructs considered as errors (e.g. deprecated constructs)."
  :group 'tuareg-opam)

(defvar tuareg-opam-error-face 'tuareg-opam-error-face
  "Face for constructs considered as errors (e.g. deprecated constructs).")

(defface tuareg-opam-pkg-variable-name-face
  '((t (:inherit font-lock-variable-name-face :slant italic)))
  "Face for package specific variables."
  :group 'tuareg-opam)

(defvar tuareg-opam-pkg-variable-name-face 'tuareg-opam-pkg-variable-name-face
  "Face for package specific variables.")

(defconst tuareg-opam-keywords
  '("opam-version" "name" "version" "maintainer" "authors"
    "license" "homepage" "doc" "bug-reports" "dev-repo"
    "tags" "patches" "substs" "build" "install" "run-test"
    "remove" "depends" "depopts" "conflicts" "conflict-class"
    "depexts" "messages" "post-messages" "available"
    "flags" "features" "synopsis" "description" "url" "setenv"
    "build-env" "extra-files" "pin-depends")
  "Kewords in OPAM files.")

(defconst tuareg-opam-keywords-regex
  (regexp-opt tuareg-opam-keywords 'symbols))

(defconst tuareg-opam-variables-regex
  (regexp-opt '("opam-version" "root" "jobs" "make" "arch"
                "os" "os-distribution" "os-family" "os-version"
                "switch" "prefix" "lib" "bin" "sbin" "share" "doc"
                "etc" "man" "toplevel" "stublibs" "user" "group"
                "name" "version" "pinned")
              'symbols)
  "Variables declared in OPAM.")

(defconst tuareg-opam-pkg-variables-regex
  (regexp-opt '("name" "version" "depends" "installed" "enable" "pinned"
                "bin" "sbin" "lib" "man" "doc" "share" "etc" "build"
                "hash" "dev" "build-id")
              'symbols)
  "Package variables in OPAM.")

(defconst tuareg-opam-scopes-regex
  (regexp-opt '("build" "with-test" "with-doc"
                "pinned"
                "true" "false")
              'symbols)
  "Package scopes")

(defconst tuareg-opam-deprecated-regex
  (eval-when-compile (regexp-opt '("build-test" "build-doc") 'symbols)))

(defvar tuareg-opam-font-lock-keywords
  `((,tuareg-opam-deprecated-regex . tuareg-opam-error-face)
    (,(concat "^" tuareg-opam-keywords-regex ":")
     1 font-lock-keyword-face)
    ("^\\(extra-source\\)\\_>" 1 font-lock-keyword-face)
    (,(concat "^\\(x-[[:alnum:]]+\\):")
     1 font-lock-keyword-face)
    (,tuareg-opam-scopes-regex . font-lock-constant-face)
    (,tuareg-opam-variables-regex . font-lock-variable-name-face)
    (,(concat "%{" tuareg-opam-variables-regex "\\(?:}%\\|?\\)")
     (1 font-lock-variable-name-face t))
    (,(concat "%{\\([a-zA-Z_][a-zA-Z0-9_+-]*\\):"
              tuareg-opam-pkg-variables-regex "\\(?:}%\\|?\\)")
     (1 font-lock-type-face t)
     (2 font-lock-variable-name-face t t))
    (,(concat "%{\\([a-zA-Z_][a-zA-Z0-9_+-]*\\):"
              "\\([a-zA-Z][a-zA-Z0-9_+-]*\\)\\(?:}%\\|?\\)")
     (1 font-lock-type-face t)
     (2 tuareg-opam-pkg-variable-name-face t t))
    ;; "package-name:var-name" anywhere (do not force)
    (,(concat "\\_<\\([a-zA-Z_][a-zA-Z0-9_+-]*\\):"
              tuareg-opam-pkg-variables-regex)
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face))
    ("\\_<\\([a-zA-Z_][a-zA-Z0-9_+-]*\\):\\([a-zA-Z][a-zA-Z0-9_+-]*\\)\\_>"
     (1 font-lock-type-face)
     (2 tuareg-opam-pkg-variable-name-face)))
  "Highlighting for OPAM files")


(defvar tuareg-opam-prettify-symbols
  `(("&" . ,(decode-char 'ucs 8743)); 'LOGICAL AND' (U+2227)
    ("|" . ,(decode-char 'ucs 8744)); 'LOGICAL OR' (U+2228)
    ("<=" . ,(decode-char 'ucs 8804))
    (">=" . ,(decode-char 'ucs 8805))
    ("!=" . ,(decode-char 'ucs 8800)))
  "Alist of symbol prettifications for OPAM files.
See `prettify-symbols-alist' for more information.")

(defvar tuareg-opam-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Tuareg-opam syntax table.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               SMIE

(require 'smie)

(defvar tuareg-opam-smie-grammar
  (let* ((decl-of-kw (lambda(kw) `(decls ,kw ":" list)))
         (bnfprec2
          (smie-bnf->prec2
           `((decls . ,(mapcar decl-of-kw tuareg-opam-keywords) )
             (list ("[" list "]")
                   (value))
             (value (string "{" filter "}")
                    (string))
             (string)
             (filter)))))
    (smie-prec2->grammar
     (smie-merge-prec2s
      bnfprec2
      (smie-precs->prec2
       '((right "&" "|")
         (left "=" "!=" ">" ">=" "<" "<=")))
      ))))

(defun tuareg-opam-smie-rules (kind token)
  (cond
   ((and (eq kind :before) (member token tuareg-opam-keywords))
    0)
   ((and (eq kind :before) (equal token "[") (smie-rule-hanging-p))
    0)
   ((and (eq kind :before) (equal token "{"))
    0)
   (t tuareg-opam-indent-basic)))


(defvar tuareg-opam-smie-verbose-p t
  "Emit context information about the current syntax state.")

(defmacro tuareg-opam-smie-debug (message &rest format-args)
  `(progn
     (when tuareg-opam-smie-verbose-p
       (message (format ,message ,@format-args)))
     nil))

(defun verbose-tuareg-opam-smie-rules (kind token)
  (let ((value (tuareg-opam-smie-rules kind token)))
    (tuareg-opam-smie-debug
     "%s '%s'; sibling-p:%s parent:%s prev-is-[:%s hanging:%s = %s"
     kind token
     (ignore-errors (smie-rule-sibling-p))
     (ignore-errors smie--parent)
     (ignore-errors (smie-rule-prev-p "["))
     (ignore-errors (smie-rule-hanging-p))
     value)
    value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Linting

(require 'flymake)

(defalias 'tuareg-opam--flymake-proc-init-create-temp-buffer-copy
  (if (functionp #'flymake-proc-init-create-temp-buffer-copy)
      'flymake-proc-init-create-temp-buffer-copy
    'flymake-init-create-temp-buffer-copy))

(defalias 'tuareg-opam--proc-create-temp-inplace
  (if (functionp #'flymake-proc-create-temp-inplace)
      'flymake-proc-create-temp-inplace
    'flymake-create-temp-inplace))

(defun tuareg-opam-flymake-init ()
  (let ((fname (tuareg-opam--flymake-proc-init-create-temp-buffer-copy
                #'tuareg-opam--proc-create-temp-inplace)))
    (list "opam" (list "lint" fname))))

(defvaralias 'tuareg-opam--flymake-proc-allowed-file-name-masks
  (if (boundp 'flymake-proc-allowed-file-name-masks)
      'flymake-proc-allowed-file-name-masks
    'flymake-allowed-file-name-masks))

(defvar tuareg-opam--allowed-file-name-masks
  '("[./]opam_?\\'" tuareg-opam-flymake-init)
  "Flymake entry for OPAM files.  See `flymake-allowed-file-name-masks'.")

(defvaralias 'tuareg-opam--flymake-proc-err-line-patterns
  (if (boundp 'flymake-proc-err-line-patterns)
      'flymake-proc-err-line-patterns
    'flymake-err-line-patterns))

(defvar tuareg-opam--err-line-patterns
  '(("File \"\\([^\"]+\\)\", line \\([0-9]+\\), \
characters \\([0-9]+\\)-\\([0-9]+\\): +\\([^\n]*\\)$"
     1 2 3 5))
  "Value of `flymake-proc-err-line-patterns' for OPAM files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Skeleton

(define-skeleton tuareg-opam-insert-opam-form
  "Insert a minimal opam file."
  nil
  "opam-version: \"2.0\"" > \n
  "maintainer: \"" _ "\"" > \n
  "authors: [" _ "]" > \n
  "tags: [" _ "]" > \n
  "license: \"" _ "\"" > \n
  "homepage: \"" _ "\"" > \n
  "dev-repo: \"" _ "\"" > \n
  "bug-reports: \"" _ "\"" > \n
  "doc: \"" _ "\"" > \n
  "build: [" > \n
  "[\"dune\" \"subst\" ] {pinned}" > \n
  "[\"dune\" \"build\" \"-p\" name \"-j\" jobs]" > \n
  "[\"dune\" \"build\" \"-p\" name \"-j\" jobs \"@doc\"] {with-doc}" > \n
  "[\"dune\" \"runtest\" \"-p\" name \"-j\" jobs] {with-test}" > \n
  "]" > \n
  "depends: [" > \n
  "\"ocaml\" {>= \"4.02\"}" > \n
  "\"dune\"" > \n
  "]" > \n
  "synopsis: \"\"" > \n
  "description: \"\"\"" > \n
  "\"\"\"" > ?\n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tuareg-opam-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c.o" 'tuareg-opam-insert-opam-form)
    map)
  "Keymap used in Tuareg-opam mode.")

(defun tuareg-opam-build-menu ()
  (easy-menu-define
    tuareg-opam-mode-menu  (list tuareg-opam-mode-map)
    "Tuareg-opam mode menu."
    '("OPAM"
      ["Skeleton" tuareg-opam-insert-opam-form t]))
  (easy-menu-add tuareg-opam-mode-menu))


;;;###autoload
(define-derived-mode tuareg-opam-mode prog-mode "Tuareg-opam"
  "Major mode to edit opam files."
  (setq font-lock-defaults '(tuareg-opam-font-lock-keywords))
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local prettify-symbols-alist tuareg-opam-prettify-symbols)
  (setq indent-tabs-mode nil)
  (setq-local require-final-newline mode-require-final-newline)
  (smie-setup tuareg-opam-smie-grammar #'tuareg-opam-smie-rules)
  (push tuareg-opam--allowed-file-name-masks
        tuareg-opam--flymake-proc-allowed-file-name-masks)
  (setq-local tuareg-opam--flymake-proc-err-line-patterns
              tuareg-opam--err-line-patterns)
  (when (and tuareg-opam-flymake buffer-file-name)
    (flymake-mode t))
  (tuareg-opam-build-menu)
  (run-mode-hooks 'tuareg-opam-mode-hook))

(defun tuareg-opam-config-env (&optional switch)
  "Get the opam environment for the given switch (or the default
switch if none is provied) and return a list of lists of the
form (n v) where n is the name of the environment variable and v
its value (both being strings).  If opam is not found or the
switch is not installed, `nil' is returned."
  (let* ((switch (if switch (concat " --switch " switch)))
	 (get-env (concat tuareg-opam " config env --sexp" switch))
	 (opam-env (tuareg--shell-command-to-string get-env)))
    (if opam-env
	(car (read-from-string opam-env)))))

(defcustom tuareg-opam-insinuate nil
  "By default, Tuareg will use the environment that Emacs was
launched in.  That environment may not contain an OCaml
compiler (say, because Emacs was launched graphically and the
path is set in ~/.bashrc) and will remain unchanged when one
issue an \"opam switch\" in a shell.  If this variable is set to
t, Tuareg will try to use opam to set the right environment for
`compile', `run-ocaml' and `merlin-mode' based on the current
opam switch at the time the command is run (provided opam is
found).  You may also use `tuareg-opam-update-env' to set the
environment for another compiler from within emacs (without
changing the opam switch).  Beware that setting it to t causes
problems if you compile under tramp."
  :group 'tuareg :type 'boolean)

(defun tuareg--shell-command-to-string (command)
  "Similar to shell-command-to-string, but returns nil when the
process return code is not 0 (shell-command-to-string returns the
error message as a string)."
  (let* ((return-value 0)
         (return-string
          (with-output-to-string
            (with-current-buffer standard-output
              (setq return-value
                    (process-file shell-file-name nil '(t nil)
                                  nil shell-command-switch command))))))
    (if (= return-value 0) return-string nil)))

(defun tuareg-opam-config-env (&optional switch)
  "Get the opam environment for the given switch (or the default
switch if none is provied) and return a list of lists of the
form (n v) where n is the name of the environment variable and v
its value (both being strings).  If opam is not found or the
switch is not installed, `nil' is returned."
  (let* ((switch (if switch (concat " --switch " switch)))
	 (get-env (concat tuareg-opam " config env --sexp" switch))
	 (opam-env (tuareg--shell-command-to-string get-env)))
    (if opam-env
	(car (read-from-string opam-env)))))

(defun tuareg-opam-installed-compilers ()
  (let* ((cmd1 (concat tuareg-opam " switch list -i -s"))
         (cmd2 (concat tuareg-opam " switch list -s")); opam2
	 (cpl (or (tuareg--shell-command-to-string cmd1)
                  (tuareg--shell-command-to-string cmd2))))
    (if cpl (split-string cpl "[ \f\t\n\r\v]+" t) '())))

(defun tuareg-opam-current-compiler ()
  (let* ((cmd (concat tuareg-opam " switch show -s"))
	 (cpl (tuareg--shell-command-to-string cmd)))
    (when cpl
      (replace-regexp-in-string "[ \t\n]*" "" cpl))))

;;;###autoload
(defun tuareg-opam-update-env (switch)
  "Update the environment to follow current OPAM switch configuration."
  (interactive
   (let* ((compl (tuareg-opam-installed-compilers))
	  (current (tuareg-opam-current-compiler))
	  (default (if current current "current"))
	  (prompt (format "opam switch (default: %s): " default)))
     (list (completing-read prompt compl))))
  (let* ((switch (if (string= switch "") nil switch))
	 (env (tuareg-opam-config-env switch)))
    (if env
	(dolist (v env)
	  (setenv (car v) (cadr v))
	  (when (string= (car v) "PATH")
	    (setq exec-path (split-string (cadr v) path-separator))))
      (message "Switch %s does not exist (or opam not found)" switch))))

;; OPAM compilation
(defun tuareg--compile-opam (&rest _)
  "Advice to update the OPAM environment to sync it with the OPAM
switch before compiling."
  (let* ((env (tuareg-opam-config-env)))
    (when env
      (setq-local compilation-environment
                  (mapcar (lambda(v) (concat (car v) "=" (cadr v)))
                          (tuareg-opam-config-env))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("[./]opam_?\\'" . tuareg-opam-mode))


(provide 'tuareg-opam)
