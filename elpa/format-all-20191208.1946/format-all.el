;;; format-all.el --- Auto-format C, C++, JS, Python, Ruby and 40 other languages -*- lexical-binding: t -*-
;;
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-format-all-the-code
;; Package-Version: 20191208.1946
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Keywords: languages util
;; SPDX-License-Identifier: MIT
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Lets you auto-format source code in many languages using the same
;; command for all languages, instead of learning a different Emacs
;; package and formatting command for each language.
;;
;; Just do M-x format-all-buffer and it will try its best to do the
;; right thing.  To auto-format code on save, use the minor mode
;; format-all-mode.  Please see the documentation for that function
;; for instructions.
;;
;; Supported languages:
;;
;; - Angular/Vue (prettier)
;; - Assembly (asmfmt)
;; - Bazel Starlark (buildifier)
;; - BibTeX (emacs)
;; - C/C++/Objective-C (clang-format)
;; - Clojure/ClojureScript (node-cljfmt)
;; - CMake (cmake-format)
;; - Crystal (crystal tool format)
;; - CSS/Less/SCSS (prettier)
;; - D (dfmt)
;; - Dart (dartfmt)
;; - Dhall (dhall format)
;; - Dockerfile (dockfmt)
;; - Elixir (mix format)
;; - Elm (elm-format)
;; - Emacs Lisp (emacs)
;; - Fish Shell (fish_indent)
;; - Go (gofmt)
;; - GraphQL (prettier)
;; - Haskell (brittany)
;; - HTML/XHTML/XML (tidy)
;; - Java (clang-format)
;; - JavaScript/JSON/JSX (prettier)
;; - Kotlin (ktlint)
;; - LaTeX (latexindent)
;; - Ledger (ledger-mode)
;; - Lua (lua-fmt)
;; - Markdown (prettier)
;; - Nix (nixfmt)
;; - OCaml (ocp-indent)
;; - Perl (perltidy)
;; - PHP (prettier plugin-php)
;; - Protocol Buffers (clang-format)
;; - Python (black)
;; - R (styler)
;; - Ruby (rufo)
;; - Rust (rustfmt)
;; - Shell script (shfmt)
;; - SQL (sqlformat)
;; - Swift (swiftformat)
;; - Terraform (terraform fmt)
;; - TypeScript/TSX (prettier)
;; - Verilog (iStyle)
;; - YAML (prettier)
;;
;; You will need to install external programs to do the formatting.
;; If `format-all-buffer` can't find the right program, it will try to
;; tell you how to install it.
;;
;; There are currently no customize variables, since it's not clear
;; what approach should be taken.  Please see
;; https://github.com/lassik/emacs-format-all-the-code/issues for
;; discussion.
;;
;; Many of the external formatters support configuration files in the
;; source code directory to control their formatting.  Please see the
;; documentation for each formatter.
;;
;; New external formatters can be added easily if they can read code
;; from standard input and format it to standard output.  Feel free to
;; submit a pull request or ask for help in GitHub issues.
;;
;;; Code:

(defvar format-all-debug nil
  "When non-nil, format-all writes debug info using `message'.")

(defvar format-all-after-format-functions nil
  "Hook run after each time `format-all-buffer' has formatted a buffer.

The value is a list of hook functions.  Use `add-hook' to add a
function.  The function is called with two arguments: (FORMATTER
STATUS).  FORMATTER is a symbol naming the formatter, as given to
`define-format-all-formatter'.  STATUS is one of the following
keywords:

* :reformatted -- The formatter made changes to the buffer.

* :already-formatted -- The buffer was already formatted
  correctly so the formatter didn't make any changes to it.

* :error -- The formatter encountered an error (usually a syntax
  error).  The buffer contents are the same as before formatting.

The current buffer is the buffer that was just formatted.  Point
is not guaranteed to be in any particular place, so `goto-char'
before editing the buffer.  Narrowing may be in effect unless
STATUS is :reformatted.")

(eval-when-compile
  (defconst format-all--system-type
    (cl-case system-type
      (windows-nt 'windows)
      (cygwin     'windows)
      (darwin     'macos)
      (gnu/linux  'linux)
      (berkeley-unix
       (save-match-data
         (let ((case-fold-search t))
           (cond ((string-match "freebsd" system-configuration) 'freebsd)
                 ((string-match "openbsd" system-configuration) 'openbsd)
                 ((string-match "netbsd"  system-configuration) 'netbsd))))))
    "Current operating system according to the format-all package."))

(eval-when-compile
  (defun format-all--resolve-system (choices)
    "Get first choice matching `format-all--system-type' from CHOICES."
    (cl-dolist (choice choices)
      (cond ((atom choice)
             (cl-return choice))
            ((eql format-all--system-type (car choice))
             (cl-return (cadr choice)))))))

(defun format-all--fix-trailing-whitespace ()
  "Fix trailing whitespace since some formatters don't do that."
  (save-match-data
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match ""))
    (goto-char (point-max))
    (delete-region
     (if (re-search-backward "[^ \t\n]" nil t) (match-end 0) (point-min))
     (point-max))
    (unless (= (point-min) (point-max))
      (goto-char (point-max))
      (insert "\n"))))

(defun format-all--remove-ansi-color (string)
  "Internal helper function to remove terminal color codes from STRING."
  (save-match-data (replace-regexp-in-string "\x1b\\[[0-9]+m" "" string t)))

(defun format-all--flatten-once (list)
  "Internal helper function to remove nested lists in LIST."
  (mapcan (lambda (x) (if (listp x) x (list x)))
          list))

(defun format-all--buffer-extension-p (&rest extensions)
  "Internal helper function to test file name EXTENSIONS."
  (and (buffer-file-name)
       (save-match-data
         (let ((case-fold-search t))
           (cl-some (lambda (ext)
                      (string-match (concat "\\." (regexp-quote ext) "\\'")
                                    (buffer-file-name)))
                    extensions)))))

(defun format-all--buffer-thunk (thunk)
  "Internal helper function to implement formatters.

THUNK is a function that implements a particular formatter.  It
takes INPUT (the unformatted source code as a string).  THUNK is
invoked such that the current buffer is an empty temp buffer.  It
should call the formatter on INPUT and write the formatted source
code output to the temp buffer.  It should return (ERRORP
ERRPUT).  ERRORP is a boolean indicating whether the formatter
caused an error and hence the contents of the temp buffer should
be discarded.  ERRPUT is a string containing all error/warning
output from the formatter.

Note that in some cases we can use the output of the formatter
even if it produced warnings.  Not all warnings are errors."
  (save-excursion
    (save-restriction
      (widen)
      (let ((inbuf (current-buffer))
            (input (buffer-string)))
        (with-temp-buffer
          (cl-destructuring-bind (errorp errput) (funcall thunk input)
            (let* ((no-chg (or errorp
                               (= 0 (let ((case-fold-search nil))
                                      (compare-buffer-substrings
                                       inbuf nil nil nil nil nil)))))
                   (output (cond (errorp nil)
                                 (no-chg t)
                                 (t (buffer-string)))))
              (list output errput))))))))

(defun format-all--buffer-native (mode &rest funcs)
  "Internal helper function to implement formatters.

In a new temp buffer, switches to MODE then calls FUNCS in order
to format the code. MODE and FUNCS should be symbols instead of
functions to avoid warnings from the Emacs byte compiler."
  (format-all--buffer-thunk
   (lambda (input)
     (funcall mode)
     (insert input)
     (mapc #'funcall funcs)
     (format-all--fix-trailing-whitespace)
     (list nil ""))))

(defun format-all--locate-default-directory (root-files)
  "Internal helper function to find working directory for formatter.

ROOT-FILES is a list of strings which are the filenames to look
for using `locate-dominating-file'.  Details in documentation for
`format-all--buffer-hard'."
  (let ((found-dirs
         (when (and root-files (buffer-file-name))
           (mapcan (lambda (root-file)
                     (let ((found-file (locate-dominating-file
                                        (buffer-file-name) root-file)))
                       (when found-file
                         (list (file-name-directory found-file)))))
                   root-files))))
    (or (car (sort found-dirs (lambda (a b) (> (length a) (length b)))))
        (and (buffer-file-name) (file-name-directory (buffer-file-name)))
        default-directory)))

(defun format-all--buffer-hard
    (ok-statuses error-regexp root-files executable &rest args)
  "Internal helper function to implement formatters.

Runs the external program EXECUTABLE.  The program shall read
unformatted code from stdin, write its formatted equivalent to
stdout, and write errors/warnings to stderr.

The program should exit with status zero for the formatting to be
considered successful.  If a list of OK-STATUSES is given, all of
those are actually considered successful.  But if ERROR-REGEXP is
given, and the program's stderr contains that regexp, then the
formatting is considered failed even if the exit status is in
OK-STATUSES.  OK-STATUSES and ERROR-REGEXP are hacks to work
around formatter programs that don't make sensible use of their
exit status.

If ARGS are given, those are arguments to EXECUTABLE. They should
not be shell-quoted.

If ROOT-FILES are given, the working directory of the formatter
will be the deepest directory (starting from the file being
formatted) containing one of these files.  If ROOT-FILES is nil,
or none of ROOT-FILES are found in any parent directories, the
working directory will be the one where the formatted file is.
ROOT-FILES is ignored for buffers that are not visiting a file."
  (let ((ok-statuses (or ok-statuses '(0)))
        (args (format-all--flatten-once args))
        (default-directory (format-all--locate-default-directory root-files)))
    (when format-all-debug
      (message "Format-All: Running: %s"
               (mapconcat #'shell-quote-argument (cons executable args) " "))
      (message "Format-All: Directory: %s" default-directory))
    (format-all--buffer-thunk
     (lambda (input)
       (let* ((errfile (make-temp-file "format-all-"))
              (status (apply #'call-process-region input nil
                             executable nil (list t errfile)
                             nil args))
              (errput (with-temp-buffer
                        (insert-file-contents errfile)
                        (delete-file errfile)
                        (buffer-string)))
              (errorp (or (not (member status ok-statuses))
                          (and error-regexp
                               (save-match-data
                                 (string-match error-regexp errput))))))
         (list errorp errput))))))

(defun format-all--buffer-easy (executable &rest args)
  "Internal helper function to implement formatters.

Runs the external program EXECUTABLE.  The program shall read
unformatted code from stdin, write its formatted equivalent to
stdout, write errors/warnings to stderr, and exit zero/non-zero
on success/failure.

If ARGS are given, those are arguments to EXECUTABLE.  They don't
need to be shell-quoted."
  (apply 'format-all--buffer-hard nil nil nil executable args))

(defvar format-all--executable-table (make-hash-table)
  "Internal table of formatter executable names for format-all.")

(defvar format-all--install-table (make-hash-table)
  "Internal table of formatter install commands for format-all.")

(defvar format-all--mode-table (make-hash-table)
  "Internal table of major mode formatter lists for format-all.")

(defvar format-all--format-table (make-hash-table)
  "Internal table of formatter formatting functions for format-all.")

(defun format-all--pushhash (key value table)
  "Push VALUE onto the list under KEY in hash table TABLE."
  (puthash key (cons value (remove value (gethash key table))) table))

(defmacro define-format-all-formatter (formatter &rest body)
  "Define a new source code formatter for use with format-all.

FORMATTER is a symbol naming the formatter.  The name of the
command used to run the formatter is usually a good choice.

Consult the existing formatters for examples of BODY."
  (let (executable install modes format)
    (cl-assert
     (equal (mapcar 'car body) '(:executable :install :modes :format)))
    (cl-dolist (part body)
      (cl-ecase (car part)
        (:executable
         (setq executable
               (unless (null (cdr part))
                 (or (format-all--resolve-system (cdr part))
                     (error "Executable not specified for %S system %S"
                            formatter format-all--system-type)))))
        (:install
         (setq install (format-all--resolve-system (cdr part))))
        (:modes
         (setq modes
               (mapcan
                (lambda (modex)
                  (let ((modex (if (listp modex) modex (list modex))))
                    (cl-destructuring-bind (mmodes &optional probex) modex
                      (let* ((mmodes (if (listp mmodes) mmodes (list mmodes)))
                             (probe (when probex `(lambda () ,probex))))
                        (mapcar
                         (lambda (mmode)
                           `(format-all--pushhash ',mmode
                                                  (cons ',formatter ,probe)
                                                  format-all--mode-table))
                         mmodes)))))
                (cdr part))))
        (:format
         (setq format `(lambda (executable mode-result)
                         (ignore mode-result
                                 ,@(unless executable '(executable)))
                         ,(cadr part))))))
    `(progn (puthash ',formatter ,executable format-all--executable-table)
            (puthash ',formatter ,install format-all--install-table)
            ,@modes
            (puthash ',formatter ,format format-all--format-table)
            ',formatter)))

(define-format-all-formatter asmfmt
  (:executable "asmfmt")
  (:install)
  (:modes asm-mode nasm-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter bibtex-mode
  (:executable)
  (:install)
  (:modes bibtex-mode)
  (:format (format-all--buffer-native
            'bibtex-mode 'bibtex-reformat 'bibtex-sort-buffer)))

(define-format-all-formatter black
  (:executable "black")
  (:install "pip install black")
  (:modes python-mode)
  (:format (format-all--buffer-easy
            executable "-q"
            (when (format-all--buffer-extension-p "pyi") "--pyi")
            "-")))

(define-format-all-formatter buildifier
  (:executable "buildifier")
  (:install
   (macos "brew install buildifier")
   "go get github.com/bazelbuild/buildtools/buildifier")
  (:modes bazel-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter brittany
  (:executable "brittany")
  (:install "stack install brittany")
  (:modes haskell-mode literate-haskell-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter clang-format
  (:executable "clang-format")
  (:install
   (macos "brew install clang-format")
   (windows "scoop install llvm"))
  (:modes
   (c-mode ".c")
   (c++-mode ".cpp")
   (java-mode ".java")
   (objc-mode ".m")
   (protobuf-mode ".proto"))
  (:format
   (format-all--buffer-easy
    executable
    (let ((assume-filename (or (buffer-file-name) mode-result)))
      (when assume-filename (concat "-assume-filename=" assume-filename))))))

(define-format-all-formatter cljfmt
  (:executable "cljfmt")
  (:install "npm install --global node-cljfmt")
  (:modes clojure-mode clojurec-mode clojurescript-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter cmake-format
  (:executable "cmake-format")
  (:install "pip install cmake-format")
  (:modes cmake-mode)
  (:format (format-all--buffer-easy executable "-")))

(define-format-all-formatter crystal
  (:executable "crystal")
  (:install (macos "brew install crystal"))
  (:modes crystal-mode)
  (:format (format-all--buffer-easy executable "tool" "format" "-")))

(define-format-all-formatter dartfmt
  (:executable "dartfmt")
  (:install (macos "brew tap dart-lang/dart && brew install dart"))
  (:modes dart-mode)
  (:format
   (format-all--buffer-easy
    executable
    (when (buffer-file-name)
      (list "--stdin-name" (buffer-file-name))))))

(define-format-all-formatter dfmt
  (:executable "dfmt")
  (:install (macos "brew install dfmt"))
  (:modes d-mode)
  (:format
   (format-all--buffer-hard nil (regexp-quote "[error]") nil executable)))

(define-format-all-formatter dhall
  (:executable "dhall")
  (:install (macos "brew install dhall"))
  (:modes dhall-mode)
  (:format (format-all--buffer-easy executable "format")))

(define-format-all-formatter dockfmt
  (:executable "dockfmt")
  (:install "go get github.com/jessfraz/dockfmt")
  (:modes dockerfile-mode)
  (:format (format-all--buffer-easy executable "fmt")))

(define-format-all-formatter elm-format
  (:executable "elm-format")
  (:install (macos "brew install elm"))
  (:modes elm-mode)
  (:format
   (cl-destructuring-bind (output errput)
       (format-all--buffer-hard nil nil '("elm.json" "elm-package.json")
                                executable "--yes" "--stdin")
     (let ((errput (format-all--remove-ansi-color errput)))
       (list output errput)))))

(define-format-all-formatter emacs-lisp
  (:executable)
  (:install)
  (:modes emacs-lisp-mode lisp-interaction-mode)
  (:format
   (format-all--buffer-native
    'emacs-lisp-mode
    (lambda () (indent-region (point-min) (point-max))))))

(define-format-all-formatter fish-indent
  (:executable "fish_indent")
  (:install (macos "brew install fish OR port install fish"))
  (:modes fish-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter gofmt
  (:executable "gofmt")
  (:install
   (macos "brew install go")
   (windows "scoop install go"))
  (:modes go-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter html-tidy
  (:executable "tidy")
  (:install
   (macos "brew install tidy-html5")
   (windows "scoop install tidy"))
  (:modes
   html-helper-mode html-mode mhtml-mode nxhtml-mode
   nxml-mode xml-mode
   (web-mode
    (and (equal "none" (symbol-value 'web-mode-engine))
         (car (member (symbol-value 'web-mode-content-type)
                      '("xml" "html"))))))
  (:format
   (format-all--buffer-hard
    '(0 1) nil nil
    executable
    "-q"
    "--tidy-mark" "no"
    "-indent"
    (when (member major-mode '(nxml-mode xml-mode))
      "-xml"))))

(define-format-all-formatter istyle-verilog
  (:executable "iStyle")
  (:install)
  (:modes verilog-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter ktlint
  (:executable "ktlint")
  (:install (macos "brew install ktlint"))
  (:modes kotlin-mode)
  (:format (format-all--buffer-easy executable "--format" "--stdin")))

(define-format-all-formatter latexindent
  (:executable "latexindent")
  (:install)
  (:modes latex-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter ledger-mode
  (:executable)
  (:install)
  (:modes ledger-mode)
  (:format
   (format-all--buffer-native 'ledger-mode 'ledger-mode-clean-buffer)))

(define-format-all-formatter lua-fmt
  (:executable "luafmt")
  (:install "npm install --global lua-fmt")
  (:modes lua-mode)
  (:format (format-all--buffer-easy executable "--stdin")))

(define-format-all-formatter mix-format
  (:executable "mix")
  (:install (macos "brew install elixir"))
  (:modes elixir-mode)
  (:format
   (format-all--buffer-hard
    nil nil '("mix.exs")
    executable
    "format"
    (let* ((file ".formatter.exs")
           (dir (and (buffer-file-name)
                     (locate-dominating-file (buffer-file-name) file))))
      (when dir (list "--dot-formatter" (concat dir file))))
    "-")))

(define-format-all-formatter nixfmt
  (:executable "nixfmt")
  (:install "nix-env -f https://github.com/serokell/nixfmt/archive/master.tar.gz -i")
  (:modes nix-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter ocp-indent
  (:executable "ocp-indent")
  (:install "opam install ocp-indent")
  (:modes caml-mode tuareg-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter perltidy
  (:executable "perltidy")
  (:install "cpan install Perl::Tidy")
  (:modes perl-mode cperl-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter prettier
  (:executable "prettier")
  (:install "npm install --global prettier @prettier/plugin-php")
  (:modes
   (angular-html-mode "angular")
   ((js-mode js2-mode js3-mode)
    (if (and (boundp 'flow-minor-mode)
             (not (null (symbol-value 'flow-minor-mode))))
        "flow"
      "babel"))
   ((js2-jsx-mode jsx-mode rjsx-mode) "babel")
   ((typescript-mode typescript-tsx-mode) "typescript")
   (json-mode "json")
   (vue-mode "vue")
   (css-mode "css")
   (scss-mode "scss")
   (less-css-mode "less")
   (graphql-mode "graphql")
   ((gfm-mode markdown-mode) "markdown")
   (php-mode "php")
   (web-mode
    (let ((ct (symbol-value 'web-mode-content-type))
          (en (symbol-value 'web-mode-engine)))
      (cond ((equal ct "css") "css")
            ((or (equal ct "javascript") (equal ct "jsx"))
             (if (format-all--buffer-extension-p "ts" "tsx")
                 "typescript"
               "babel"))
            ((equal ct "json") "json")
            ((equal ct "html")
             (cond ((equal en "angular") "angular")
                   ((equal en "vue") "vue")
                   ;; TODO: Use html-tidy instead of prettier for
                   ;; plain HTML. Enable prettier's HTML support once
                   ;; we have multi-formatter support.
                   ;;
                   ;; ((equal en "none") "html")
                   (t nil)))
            (t nil))))
   (yaml-mode "yaml"))
  (:format
   (let ((parser mode-result))
     (format-all--buffer-easy
      executable
      (when parser
        (list "--parser" parser))
      (when (buffer-file-name)
        (list "--stdin-filepath" (buffer-file-name)))))))

(define-format-all-formatter rufo
  (:executable "rufo")
  (:install "gem install rufo")
  (:modes ruby-mode enh-ruby-mode)
  (:format
   (format-all--buffer-easy
    executable
    "--simple-exit"
    (when (buffer-file-name)
      (list "--filename" (buffer-file-name))))))

(define-format-all-formatter rustfmt
  (:executable "rustfmt")
  (:install "cargo install rustfmt")
  (:modes rust-mode rustic-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter shfmt
  (:executable "shfmt")
  (:install
   (macos "brew install shfmt")
   (windows "scoop install shfmt"))
  (:modes sh-mode)
  (:format
   (format-all--buffer-easy
    executable
    "-ln" (cl-case (and (boundp 'sh-shell) (symbol-value 'sh-shell))
            (bash "bash") (mksh "mksh") (t "posix")))))

(define-format-all-formatter sqlformat
  (:executable "sqlformat")
  (:install "pip install sqlparse")
  (:modes sql-mode)
  (:format
   (let* ((ic (car default-process-coding-system))
          (oc (cdr default-process-coding-system))
          (ienc (symbol-name (or (coding-system-get ic :mime-charset)
                                 'utf-8)))
          (oenc (symbol-name (or (coding-system-get oc :mime-charset)
                                 'utf-8)))
          (process-environment (cons (concat "PYTHONIOENCODING=" oenc)
                                     process-environment)))
     (format-all--buffer-easy
      executable
      "--keywords" "upper"
      "--reindent_aligned"
      "--encoding" ienc
      "-"))))

(define-format-all-formatter styler
  (:executable "Rscript")
  (:install "Rscript -e 'install.packages(\"styler\")'")
  (:modes ess-r-mode)
  (:format
   (format-all--buffer-easy
    executable "--vanilla"
    "-e" (concat
          "options(styler.colored_print.vertical=FALSE);"
          " con <- file(\"stdin\");"
          " out <- styler::style_text(readLines(con));"
          " close(con);"
          " out"))))

(define-format-all-formatter swiftformat
  (:executable "swiftformat")
  (:install (macos "brew install swiftformat"))
  (:modes swift-mode swift3-mode)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter terraform-fmt
  (:executable "terraform")
  (:install (macos "brew install terraform"))
  (:modes terraform-mode)
  (:format (format-all--buffer-easy executable "fmt" "-no-color" "-")))

(defun format-all--please-install (executable installer)
  "Internal helper function for error about missing EXECUTABLE and INSTALLER."
  (concat (format "You need the %S command." executable)
          (if (not installer) ""
            (format " You may be able to install it via %S." installer))))

(defun format-all--probe ()
  "Internal helper function to get the formatter for the current buffer."
  (cl-dolist (pair (gethash major-mode format-all--mode-table) (list nil nil))
    (cl-destructuring-bind (formatter . probe) pair
      (let ((mode-result (if probe (funcall probe) t)))
        (when mode-result (cl-return (list formatter mode-result)))))))

(defun format-all--formatter-executable (formatter)
  "Internal helper function to get the external program for FORMATTER."
  (let ((executable (gethash formatter format-all--executable-table)))
    (when executable
      (or (executable-find executable)
          (error (format-all--please-install
                  executable
                  (gethash formatter format-all--install-table)))))))

(defun format-all--show-or-hide-errors (error-output)
  "Internal helper function to update *format-all-errors* with ERROR-OUTPUT."
  (save-selected-window
    (with-current-buffer (get-buffer-create "*format-all-errors*")
      (erase-buffer)
      (cond ((not (= 0 (length error-output)))
             (insert error-output)
             (display-buffer (current-buffer)))
            (t
             (let ((error-window (get-buffer-window (current-buffer))))
               (when error-window (quit-window nil error-window))))))))

(defun format-all--save-line-number (thunk)
  "Internal helper function to run THUNK and go back to the same line."
  (let ((old-line-number (line-number-at-pos))
        (old-column (current-column)))
    (funcall thunk)
    (goto-char (point-min))
    (forward-line (1- old-line-number))
    (let ((line-length (- (point-at-eol) (point-at-bol))))
      (goto-char (+ (point) (min old-column line-length))))))

(defun format-all-buffer--with (formatter mode-result)
  "Internal helper function to format the current buffer.

Relies on FORMATTER and MODE-RESULT from `format-all--probe'."
  (when format-all-debug
    (message "Format-All: Formatting %s using %S"
             (buffer-name) (list formatter mode-result)))
  (let ((f-function (gethash formatter format-all--format-table))
        (executable (format-all--formatter-executable formatter)))
    (cl-destructuring-bind (output errput)
        (funcall f-function executable mode-result)
      (let ((status (cond ((null output) :error)
                          ((equal t output) :already-formatted)
                          (t :reformatted))))
        (when (equal :reformatted status)
          (widen)
          (format-all--save-line-number
           (lambda ()
             (erase-buffer)
             (insert output))))
        (format-all--show-or-hide-errors errput)
        (run-hook-with-args 'format-all-after-format-functions
                            formatter status)
        (message (cl-ecase status
                   (:error "Formatting error")
                   (:already-formatted "Already formatted")
                   (:reformatted "Reformatted!")))))))

(defun format-all-buffer--from-hook ()
  "Internal helper function to auto-format current buffer from a hook.

Format-All installs this function into `before-save-hook' to
format buffers on save. This is a lenient version of
`format-all-buffer' that silently succeeds instead of signaling
an error if the current buffer has no formatter."
  (cl-destructuring-bind (formatter mode-result) (format-all--probe)
    (when formatter
      (format-all-buffer--with formatter mode-result))))

;;;###autoload
(defun format-all-buffer ()
  "Auto-format the source code in the current buffer.

No disk files are touched - the buffer doesn't even need to be
saved.  If you don't like the results of the formatting, you can
use ordinary undo to get your code back to its previous state.

You will need to install external programs to do the formatting.
If the command can't find the program that it needs, it will try
to tell you how you might be able to install it on your operating
system. Only BibTeX, Emacs Lisp and Ledger are formatted without an
external program.

A suitable formatter is selected according to the `major-mode' of
the buffer.  Many popular programming languages are supported.
It is fairly easy to add new languages that have an external
formatter.

If any errors or warnings were encountered during formatting,
they are shown in a buffer called *format-all-errors*."
  (interactive)
  (cl-destructuring-bind (formatter mode-result) (format-all--probe)
    (if formatter
        (format-all-buffer--with formatter mode-result)
      (error "Don't know how to format %S code" major-mode))))

;;;###autoload
(define-minor-mode format-all-mode
  "Toggle automatic source code formatting before save.

When this minor mode (FmtAll) is enabled, `format-all-buffer' is
automatically called to format your code each time before you
save the buffer.

The mode is buffer-local and needs to be enabled separately each
time a file is visited.  You may want to use `add-hook' to add a
function to your personal `after-change-major-mode-hook' in your
`user-init-file' to enable the mode based on the buffer's
`major-mode' and some `buffer-file-name' patterns. For example:

    (defvar my-auto-format-modes '(js-mode python-mode))
    (defvar my-auto-format-dirs '(\"foo\" \"bar\"))

    (defun my-auto-format-buffer-p ()
      (and (member major-mode my-auto-format-modes)
           (buffer-file-name)
           (save-match-data
             (let ((dir (file-name-directory (buffer-file-name))))
               (cl-some (lambda (regexp) (string-match regexp dir))
                        my-auto-format-dirs)))))

    (defun my-after-change-major-mode ()
      (format-all-mode (if (my-auto-format-buffer-p) 1 0)))

    (add-hook 'after-change-major-mode-hook 'my-after-change-major-mode)

When `format-all-mode' is called as a Lisp function, the mode is
toggled if ARG is ‘toggle’, disabled if ARG is a negative integer
or zero, and enabled otherwise."
  :lighter " FmtAll"
  :global nil
  (if format-all-mode
      (add-hook 'before-save-hook
                'format-all-buffer--from-hook
                nil 'local)
    (remove-hook 'before-save-hook
                 'format-all-buffer--from-hook
                 'local)))

(provide 'format-all)

;;; format-all.el ends here
