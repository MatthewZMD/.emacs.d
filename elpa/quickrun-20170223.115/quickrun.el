;;; quickrun.el --- Run commands quickly -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-quickrun
;; Package-Version: 20170223.115
;; Version: 2.2.8
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; quickrun.el executes editing buffer. quickrun.el selects commands to execute
;; buffer automatically. Please see https://github.com/syohex/emacs-quickrun
;; for more information.
;;
;; This package respects `quickrun.vim' developed by thinca
;;   - https://github.com/thinca/vim-quickrun
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'quickrun)
;;
;; And you call 'M-x quickrun'.
;;

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'em-banner)
(require 'eshell)

;; for warnings of byte-compile
(declare-function anything "anything")
(declare-function helm "helm")
(declare-function tramp-dissect-file-name "tramp")

(defgroup quickrun nil
  "Execute buffer quickly"
  :group 'processes
  :prefix 'quickrun)

(defcustom quickrun-timeout-seconds 10
  "Timeout seconds for running too long process"
  :type 'integer)

(defcustom quickrun-focus-p t
  "If this value is `nil`, quickrun.el does not move focus to output buffer."
  :type 'boolean)

(defcustom quickrun-input-file-extension ".qrinput"
  "Extension of input file name"
  :type '(choice (string :tag "Extension of quickrun input file")
                 (boolean :tag "Not use input file" nil)))

(defcustom quickrun-debug nil
  "Enable debug message"
  :type 'boolean)

(defconst quickrun--buffer-name "*quickrun*")
(defvar quickrun--executed-file nil)
(defvar quickrun--remove-files nil)
(defvar quickrun--compile-only-flag nil)
(defvar quickrun--original-buffer nil)
(defvar quickrun--original-outputter nil)

(defmacro quickrun--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defun quickrun--mklist (obj)
  (if (listp obj)
      obj
    (list obj)))

(defsubst quickrun--log (fmt &rest args)
  (when quickrun-debug
    (apply 'message fmt args)))

(defsubst quickrun--windows-p ()
  (memq system-type '(ms-dos windows-nt cygwin)))

;;
;; file local variable
;; Based on shadow.el. https://raw.github.com/mooz/shadow.el/master/shadow.el
;;
(defmacro quickrun--defvar (name &optional value safep doc)
  "Define buffer-local and safe-local variable."
  (declare (indent defun))
  `(progn
     (defvar ,name ,value ,doc)
     (make-variable-buffer-local (quote ,name))
     ;; Suppress file local variable warning
     ,(when safep
        `(put (quote ,name) 'safe-local-variable (quote ,safep)))))

(quickrun--defvar quickrun-option-cmd-alist
                  nil listp
                  "Specify command alist directly as file local variable")

(quickrun--defvar quickrun-option-command
                  nil stringp
                  "Specify command directly as file local variable")

(quickrun--defvar quickrun-option-cmdkey
                  nil stringp
                  "Specify language key directly as file local variable")

(quickrun--defvar quickrun-option-cmdopt
                  nil stringp
                  "Specify command option directly as file local variable")

(quickrun--defvar quickrun-option-args
                  nil stringp
                  "Specify command argument directly as file local variable")

(defun quickrun--outputter-p (_x)
  (lambda (x)
    (or (functionp x) (symbolp x) (stringp x)
        (quickrun--outputter-multi-p x))))

(quickrun--defvar quickrun-option-outputter
                  nil quickrun--outputter-p
                  "Specify format function output buffer as file local variable")

(quickrun--defvar quickrun-option-shebang
                  t booleanp
                  "Select using command from schebang as file local variable")

(quickrun--defvar quickrun-option-timeout-seconds
                  nil integerp
                  "Timeout seconds as file local variable")

(quickrun--defvar quickrun-option-default-directory
                  nil file-directory-p
                  "Default directory where command is executed")

;; hooks
(defvar quickrun-after-run-hook nil
  "Run hook after execute quickrun")

(defvar quickrun--temporary-file nil)

;; Language specific functions

(defun quickrun--gnuplot-execute ()
  (setq quickrun--temporary-file (concat (make-temp-name "quickrun-gnuplot") ".png"))
  (let ((terminal-option "set terminal png")
        (output-option (format "set output \"%s\"" quickrun--temporary-file)))
    (push quickrun--temporary-file quickrun--remove-files)
    (concat "%c -e '" terminal-option "' -e '" output-option "' %s")))

(defun quickrun--gnuplot-outputter ()
  (clear-image-cache)
  (insert-file-contents quickrun--temporary-file)
  (image-mode))

;;
;; language command parameters
;;

(defvar quickrun--language-alist
  '(("c/gcc" . ((:command . "gcc")
                (:exec    . ("%c -x c %o -o %e %s" "%e %a"))
                (:compile-only . "%c -Wall -Werror %o -o %e %s")
                (:remove . ("%e"))
                (:description . "Compile C file with gcc and execute")))

    ("c/clang" . ((:command . "clang")
                  (:exec    . ("%c -x c %o -o %e %s" "%e %a"))
                  (:compile-only . "%c -Wall -Werror %o -o %e %s")
                  (:remove  . ("%e"))
                  (:description . "Compile C file with llvm/clang and execute")))

    ("c/cl" . ((:command . "cl")
               (:exec    . ("%c /Tc %o %s /nologo /Fo%n.obj /Fe%n.exe"
                            "%n %a"))
               (:compile-only . "%c %o %s /Wall /nologo /Fo%n.obj /Fe%n.exe")
               (:remove  . ("%n.obj" "%n.exe"))
               (:description . "Compile C file with VC++/cl and execute")))

    ("c++/g++" . ((:command . "g++")
                  (:exec    . ("%c -x c++ %o -o %e %s" "%e %a"))
                  (:compile-only . "%c -Wall -Werror %o -o %e %s")
                  (:remove  . ("%e"))
                  (:description . "Compile C++ file with g++ and execute")))

    ("c++/clang++" . ((:command . "clang++")
                      (:exec    . ("%c -x c++ %o -o %e %s" "%e %a"))
                      (:compile-only . "%c -Wall -Werror %o -o %e %s")
                      (:remove  . ("%e"))
                      (:description . "Compile C++ file with llvm/clang++ and execute")))

    ("c++/cl" . ((:command . "cl")
                 (:exec    . ("%c /Tp %o %s /nologo /Fo%n.obj /Fe%n.exe"
                              "%n %a"))
                 (:compile-only . "%c %o %s /Wall /nologo /Fo%n.obj /Fe%n.exe")
                 (:remove  . ("%n.obj" "%n.exe"))
                 (:description . "Compile C++ file with VC/cl and execute")))

    ("objc" . ((:command . "gcc")
               (:exec    . ((lambda ()
                              (if (eq system-type 'darwin)
                                  "%c -x objective-c %o -o %e %s -framework foundation"
                                "%c -x objective-c %o -o %e %s -lobjc"))
                            "%e %a"))
               (:remove  . ("%e"))
               (:description . "Compile Objective-C file with gcc and execute")))

    ("c#/mono" . ((:command . "mono")
                  (:exec    . ("mcs %o %s" "%c %n.exe %a"))
                  (:remove  . ("%n.exe"))
                  (:description . "Compile C# and execute with mono(mcs)")))

    ("d" . ((:command . "dmd")
            (:exec    . ("%c %o -of%e %s" "%e %a"))
            (:remove  . ("%e" "%n.o"))
            (:description . "Compile D language file and execute")))

    ("fortran/gfortran" . ((:command . "gfortran")
                           (:exec    . ("%c %o -o %e %s" "%e %a"))
                           (:remove  . ("%e"))
                           (:description . "Compile Fortran language with gfortran")))

    ("java" . ((:command . "java")
               (:compile-only . "javac -Werror %o %s")
               (:exec    . ("javac %o %s" "%c %N %a"))
               (:remove  . ("%n.class"))
               (:tempfile . nil)
               (:description . "Compile Java file and execute")))

    ("perl" . ((:command . "perl") (:compile-only . "%c -wc %s")
               (:description . "Run Perl script")))
    ("perl6" . ((:command . "perl6") (:compile-only . "%c -c %s")
                (:description . "Run Perl6 script")))
    ("ruby/ruby" . ((:command . "ruby") (:compile-only . "%c -wc %s")
                    (:description . "Run Ruby script")))
    ("ruby/mruby" . ((:command . "mruby")
                     (:exec . ("mrbc %s" "mruby -b %N.mrb"))
                     (:compile-only . "mrbc -c %s")
                     (:remove  . ("%n.mrb"))
                     (:description . "Run mruby script")))
    ("python" . ((:command . "python") (:compile-only . "pyflakes %s")
                 (:description . "Run Python script")))
    ("php" . ((:command . "php") (:compile-only . "%c -l %s")
              (:description . "Run PHP script")))

    ("emacs" . ((:command . "emacs")
                (:exec    . "%c -q --no-site-file --batch -l %s")
                (:description . "Run Elisp as script file")))
    ("lisp/clisp" . ((:command . "clisp")
                     (:description . "Run Lisp file with clisp")))
    ("lisp/sbcl" . ((:command . "sbcl")
                    (:exec . "%c --script %s %a")
                    (:description . "Run Lisp file with sbcl")))
    ("lisp/ccl" . ((:command . "ccl")
                   (:exec . "%c --load %s --eval '(quit)'")
                   (:description . "Run Lisp file with ccl")))
    ("scheme/gosh" . ((:command . "gosh")
                      (:description . "Run Scheme file with gosh(Gauche)")))
    ("racket" . ((:command . "racket")
                 (:exec . "%c --require-script %s")
                 (:description . "Run racket script")))

    ("clojure/jark"        . ((:command . "jark")
                              (:description . "Run Clojure file with jark")))
    ("clojure/clj-env-dir" . ((:command . "clj-env-dir")
                              (:description . "Run Clojure file with clj-env-dir")))

    ("javascript/node" . ((:command . "node")
                          (:description . "Run Javascript file with node.js")))
    ("javascript/v8" . ((:command . "v8")
                        (:description . "Run Javascript file with v8")))
    ("javascript/js" . ((:command . "js")
                        (:description . "Run Javascript file with js(Rhino)")))
    ("javascript/jrunscript" . ((:command . "jrunscript")
                                (:description . "Run Javascript file with jrunscript")))
    ("javascript/phantomjs" . ((:command . "phantomjs")
                               (:description . "Run Javascript file with phantomjs")))
    ("javascript/cscript" . ((:command . "cscript")
                             (:exec . "%c //e:jscript %o %s %a")
                             (:cmdopt . "//Nologo")
                             (:description . "Run Javascript file with cscript")))

    ("coffee" . ((:command . "coffee")
                 (:compile-only . "coffee --print %s")
                 (:compile-conf . ((:compilation-mode . nil) (:mode . js-mode)))
                 (:description . "Run Coffee script")))

    ("jsx" . ((:command . "jsx")
              (:exec . "%c --run %o %s %a")
              (:compile-only . "%c %o %s %s")
              (:compile-conf . ((:compilation-mode . nil) (:mode . js-mode)))
              (:description . "Run JSX script")))

    ("typescript" . ((:command . "tsc")
                     (:exec . ("%c --target es5 --module commonjs %o %s %a" "node %n.js"))
                     (:compile-only . "%c %o %s %s")
                     (:compile-conf . ((:compilation-mode . nil) (:mode . js-mode)))
                     (:remove  . ("%n.js"))
                     (:description . "Run TypeScript script")))

    ("markdown/Markdown.pl" . ((:command . "Markdown.pl")
                               (:description . "Convert Markdown to HTML with Markdown.pl")))
    ("markdown/bluecloth"   . ((:command . "bluecloth")
                               (:cmdopt  . "-f")
                               (:description . "Convert Markdown to HTML with bluecloth")))
    ("markdown/kramdown"    . ((:command . "kramdown")
                               (:description . "Convert Markdown to HTML with kramdown")))
    ("markdown/pandoc"      . ((:command . "pandoc")
                               (:exec . "%c --from=markdown --to=html %o %s %a")
                               (:description . "Convert Markdown to HTML with pandoc")))
    ("markdown/redcarpet"   . ((:command . "redcarpet")
                               (:description . "Convert Markdown to HTML with redcarpet")))

    ("haskell" . ((:command . "runghc")
                  (:description . "Run Haskell file with runghc(GHC)")))

    ("go/go"  .  ((:command . "go")
                  (:exec    . ((lambda ()
                                 (if (string-match-p "_test\\.go\\'" (buffer-name))
                                     "%c test %o"
                                   "%c run %o %s %a"))))
                  (:compile-only . "%c build -o /dev/null %s %o %a")
                  (:tempfile . nil)
                  (:description . "Compile go file and execute with 'go'")))
    ("go/gccgo"  .  ((:command . "gccgo")
                     (:exec    . ("%c -static-libgcc %o -o %e %s"
                                  "%e %a"))
                     (:remove  . ("%e"))
                     (:description . "Compile Go file with 'gccgo'")))

    ("io" . ((:command . "io")
             (:description . "Run IO Language script")))
    ("lua" . ((:command . "lua")
              (:description . "Run Lua script")))
    ("groovy" . ((:command . "groovy")
                 (:description . "Run Groovy")))
    ("scala" . ((:command . "scala")
                (:cmdopt . "-Dfile.encoding=UTF-8")
                (:description . "Run Scala file with scala command")))

    ("haml" . ((:command . "haml")
               (:exec    . "%c %o %s")
               (:description . "Convert HAML to HTML")))
    ("sass" . ((:command . "sass")
               (:exec    . "%c %o --no-cache %s")
               (:description . "Convert SASS to CSS")))
    ("less" . ((:command . "lessc")
               (:description . "Convert LESS to CSS")))

    ("erlang" . ((:command . "escript")
                 (:description . "Run Erlang file with escript")))
    ("ocaml" . ((:command . "ocamlc")
                (:exec    . ("%c %o -o %e %s"
                             "%e %a"))
                (:remove  . ("%e" "%n.cmi" "%n.cmo"))
                (:description . "Compile Ocaml file with ocamlc and execute")))

    ("fsharp" . ((:command . "fsharpc")
                 (:exec . ("%c %o --nologo -o %n.exe %s" "%n.exe %a"))
                 (:remove . ("%n.exe"))
                 (:description . "Compile F# file with fsharpc and execute")))

    ("shellscript" . ((:command . (lambda () sh-shell))
                      (:description . "Run Shellscript file")))
    ("awk" . ((:command . "awk")
              (:exec    . "%c %o -f %s %a")
              (:description . "Run AWK script")))

    ("rust" . ((:command . "rustc")
               (:exec . ("%c %o -o %e %s" "%e %a"))
               (:compile-only . "%c %o -o %e %s")
               (:remove . ("%e"))
               (:description . "Compile rust and execute")))

    ("dart/checked" . ((:command . "dart")
                       (:cmdopt  . "--enable-type-checks")
                       (:description . "Run dart with '--enable-type-checks' option")))
    ("dart/production" . ((:command . "dart")
                          (:description . "Run dart as without '--enable-type-checks' option")))

    ("elixir" . ((:command . "elixir")
                 (:description . "Run Elixir script")))

    ("tcl" . ((:command . "tclsh")
              (:description . "Run Tcl script")))

    ("swift/swift" . ((:command . "swift")
                      (:exec    . ("%c %o %s %a"))
                      (:description . "Compile swift and execute")))

    ("swift/xcrun" . ((:command . "xcrun")
                      (:exec    . ("%c swift %o %s %a"))
                      (:description . "Compile swift and execute with xcrun")))

    ("ats" . ((:command . "patscc")
              (:exec    . ("%c -DATS_MEMALLOC_LIBC %o -o %e %s" "%e %a"))
              (:compile-only . "patsopt -o %n_dats.c --dynamic %s")
              (:remove  . ("%e" "%n_dats.c"))
              (:description . "Compile ATS2 and execute")))
    ("r" . ((:command . "Rscript")
            (:exec "Rscript --vanilla %s")
            (:description . "Run an R script")))

    ("nim" . ((:command . "nim")
              (:exec . "%c compile --run --verbosity:0 %s")
              (:remove . ("nimcache" "%n"))
              (:tempfile . nil)
              (:description . "Run nim script")))

    ("nimscript" . ((:command . "nim")
                    (:exec . "%c e --verbosity:0 %s")
                    (:tempfile . nil)
                    ;; Note that .nimle file also allows ‘.ini’ format, so
                    ;; we can’t check by file extension.
                    (:description . "Run NimScript (.nims or .nimble) file")))

    ("fish" . ((:command . "fish")
               (:description . "Run fish script")))

    ("julia" . ((:command . "julia")
                (:description . "Run julia script")))
    ("gnuplot" . ((:command . "gnuplot")
                  (:exec . (quickrun--gnuplot-execute))
                  (:outputter . quickrun--gnuplot-outputter))))

  "List of each programming languages information.
Parameter form is (\"language\" . parameter-alist). parameter-alist has
5 keys and those values , :command, :exec, :remove.
:command pair is mandatory, other pairs are optional. Associated value
should be string or a function which returns a string object.

Assosiated values are
:command = Program name which is used compiled or executed source code.
:exec    = Exec command template. If you omit this parameter, quickrun
           use default parameter \"%c %o %s %a\".
:remove  = Remove files or directories templates.
           Compiler or executor generates temporary files,
           you should specified this parameter.
           If value is List, quickrun removes each element.
Every pair should be dot-pair.

See explanation of quickrun--template-place-holders
if you set your own language configuration.
")

(defvar quickrun-file-alist
  '(("\\.c\\'" . "c")
    ("\\.\\(cpp\\|cxx\\|C\\|cc\\)\\'" . "c++")
    ("\\.m\\'" . "objc")
    ("\\.cs\\'" . "c#")
    ("\\.\\(pl\\|pm\\)\\'" . "perl")
    ("\\.p[ml]?6\\'" . "perl6")
    ("\\.rb\\'" . "ruby")
    ("\\.py\\'" . "python")
    ("\\.php\\'" . "php")
    ("\\.\\(el\\|elisp\\)\\'" . "emacs")
    ("\\.\\(lisp\\|lsp\\)\\'" . "lisp")
    ("\\.\\(scm\\|scheme\\)\\'" . "scheme")
    ("\\.rkt\\'" . "racket")
    ("\\.js\\'" . "javascript")
    ("\\.clj\\'" . "clojure")
    ("\\.erl\\'" . "erlang")
    ("\\.ml\\'" . "ocaml")
    ("\\.\\(fsx?\\|fsscript\\)\\'" . "fsharp")
    ("\\.go\\'" . "go")
    ("\\.io\\'" . "io")
    ("\\.lua\\'" . "lua")
    ("\\.hs\\'" . "haskell")
    ("\\.java\\'" . "java")
    ("\\.d\\'" . "d")
    ("\\.\\(f\\|for\\|f90\\|f95\\)\\'" . "fortran")
    ("\\.\\(md\\|markdown\\|mdown\\|mkdn\\)\\'" . "markdown")
    ("\\.coffee\\'" . "coffee")
    ("\\.jsx\\'" . "jsx")
    ("\\.ts\\'" . "typescript")
    ("\\.scala\\'" . "scala")
    ("\\.groovy\\'". "groovy")
    ("\\.haml\\'" . "haml")
    ("\\.sass\\'" . "sass")
    ("\\.less\\'" . "less")
    ("\\.\\(sh\\|bash\\|zsh\\|csh\\|csh\\)\\'" . "shellscript")
    ("\\.awk\\'" . "awk")
    ("\\.rs\\'" . "rust")
    ("\\.dart\\'" . "dart/checked")
    ("\\.exs?\\'" . "elixir")
    ("\\.tcl\\'" . "tcl")
    ("\\.swift\\'" . "swift")
    ("\\.dats\\'" . "ats")
    ("\\.\\(r\\|R\\)\\'" . "r")
    ("\\.nim\\'". "nim")
    ("\\.fish\\'" . "fish")
    ("\\.jl\\'" . "julia")
    ("\\.\\(gpi\\|plt\\)\\'" . "gnuplot"))
  "Alist of (file-regexp . key)")

(defvar quickrun--major-mode-alist
  '((c-mode . "c")
    (c++-mode . "c++")
    (objc-mode . "objc")
    (csharp-mode . "c#")
    ((perl-mode cperl-mode) . "perl")
    (perl6-mode . "perl6")
    (ruby-mode . "ruby")
    (python-mode . "python")
    (php-mode . "php")
    (emacs-lisp-mode . "emacs")
    (lisp-mode . "lisp")
    (scheme-mode . "scheme")
    (racket-mode . "racket")
    ((javascript-mode js-mode js2-mode) . "javascript")
    (clojure-mode . "clojure")
    (erlang-mode . "erlang")
    ((ocaml-mode tuareg-mode) . "ocaml")
    (fsharp-mode . "fsharp")
    (go-mode . "go")
    (io-mode . "io")
    (lua-mode . "lua")
    (haskell-mode . "haskell")
    (java-mode . "java")
    (d-mode . "d")
    (fortran-mode . "fortran")
    (markdown-mode . "markdown")
    (coffee-mode . "coffee")
    (jsx-mode . "jsx")
    (typescript-mode . "typescript")
    (scala-mode . "scala")
    (groove-mode . "groovy")
    (haml-mode . "haml")
    (sass-mode . "sass")
    ((less-mode less-css-mode) . "less")
    (sh-mode . "shellscript")
    (awk-mode . "awk")
    (rust-mode . "rust")
    (dart-mode . "dart/checked")
    (elixir-mode . "elixir")
    (tcl-mode . "tcl")
    (swift-mode . "swift")
    (ats-mode . "ats")
    (ess-mode . "r")
    (nim-mode . "nim")
    (nimscript-mode . "nimscript")
    (fish-mode . "fish")
    (julia-mode . "julia")
    (gnuplot-mode . "gnuplot"))
  "Alist of major-mode and langkey")

(defun quickrun--decide-file-type (filename)
  ;; First search by file extension, Second search by major-mode
  (or (assoc-default filename quickrun-file-alist 'string-match)
      (quickrun--find-from-major-mode-alist)))

(defun quickrun--find-from-major-mode-alist ()
  (cl-loop for (lang . lang-info) in quickrun--major-mode-alist
           for lang-lst = (quickrun--mklist lang)
           when (memq major-mode lang-lst)
           return lang-info))

(defun quickrun--command-info (lang)
  (or quickrun-option-cmd-alist
      (assoc-default lang quickrun--language-alist)
      (throw 'quickrun
             (format "not found [%s] language information" lang))))

;;
;; Compile Only
;;
(defun quickrun--check-using-compilation-mode (compile-conf)
  (if (not compile-conf)
      t
    (let ((compilation-mode (assoc :compilation-mode compile-conf)))
      (if (not compilation-mode)
          t
        (cdr compilation-mode)))))

(defun quickrun--pop-to-buffer (buf cb)
  (let ((win (selected-window)))
    (pop-to-buffer buf)
    (funcall cb)
    (unless quickrun-focus-p
      (select-window win))))

(defun quickrun--compilation-start (cmd compile-conf)
  (let ((use-compile (quickrun--check-using-compilation-mode compile-conf)))
    (cond (use-compile
           (setq compilation-finish-functions 'quickrun--compilation-finish-func)
           (compilation-start cmd t (lambda (_x) quickrun--buffer-name)))
          (t
           (with-current-buffer (get-buffer-create quickrun--buffer-name)
             (read-only-mode -1)
             (erase-buffer)
             (process-file-shell-command cmd nil t)
             (goto-char (point-min))
             (quickrun--awhen (assoc-default :mode compile-conf)
               (funcall it)
               (quickrun--pop-to-buffer
                (current-buffer) (lambda () (read-only-mode +1)))
               (read-only-mode +1)))
           (quickrun--remove-temp-files)))))

(defun quickrun--compilation-finish-func (_buffer _str)
  (quickrun--remove-temp-files))

;;
;; Execute
;;
(defvar quickrun--timeout-timer nil)
(defvar quickrun--run-in-shell nil)

(defsubst quickrun--concat-commands (cmd-lst)
  (mapconcat 'identity cmd-lst " && "))

(defsubst quickrun--stdin-file-name ()
  (concat quickrun--executed-file quickrun-input-file-extension))

(defsubst quickrun--stdin-file-regexp ()
  (concat quickrun-input-file-extension "\\'"))

(defsubst quickrun--use-stdin-file-p ()
  (string-match-p (quickrun--stdin-file-regexp)
                  (or (buffer-file-name) (buffer-name))))

(defun quickrun--send-file-as-stdin (process file)
  (let ((open-buf-func (cond ((file-exists-p file) 'find-file-noselect)
                             ((get-buffer file) 'get-buffer))))
    (when open-buf-func
      (quickrun--log "Send '%s' to STDIN of %s" file (process-name process))
      (with-current-buffer (funcall open-buf-func file)
        (process-send-region process (point-min) (point-max))
        (process-send-eof process)))))

(defun quickrun--default-filter (proc output)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (goto-char (point-max))
    (let ((start (point)))
      (insert output)
      (ansi-color-apply-on-region start (point)))))

(defun quickrun--exec (cmd-lst src mode)
  (if quickrun--run-in-shell
      (quickrun--send-to-shell cmd-lst)
    (ignore-errors
      (let* ((next-cmd  (car cmd-lst))
             (rest-cmds (cdr cmd-lst))
             (process (quickrun--exec-cmd next-cmd))
             (outputter (or quickrun-option-outputter
                            'quickrun--default-outputter)))
        (when (and (null rest-cmds) quickrun-input-file-extension)
          (let ((file (quickrun--stdin-file-name)))
            (quickrun--send-file-as-stdin process file)))
        (when (eq outputter 'quickrun--default-outputter)
          (set-process-filter process #'quickrun--default-filter))
        (set-process-sentinel process
                              (quickrun--make-sentinel rest-cmds outputter src mode))))))

(defvar quickrun--eshell-buffer-name "*eshell-quickrun*")
(defvar quickrun--shell-last-command)

(defun quickrun--eshell-finish ()
  (quickrun--remove-temp-files)
  (remove-hook 'eshell-post-command-hook 'quickrun--eshell-post-hook))

(defun quickrun--eshell-window-restore ()
  (interactive)
  (jump-to-register :quickrun-shell))

(defun quickrun--eshell-post-hook ()
  (let ((rerun-p nil)
        (prompt "Press 'r' to run again, any other key to finish"))
    (unwind-protect
        (ignore-errors
          (let ((input (read-char prompt)))
            (when (char-equal input ?r)
              (quickrun--insert-command quickrun--shell-last-command)
              (setq rerun-p t))))
      (unless rerun-p
        (quickrun--eshell-finish)
        (read-only-mode +1)
        (local-set-key (kbd "q") 'quickrun--eshell-window-restore)))))

(defun quickrun--insert-command (cmd-str)
  (goto-char (point-max))
  (eshell-kill-input)
  (insert cmd-str)
  (eshell-send-input))

(defun quickrun--send-to-shell (cmd-lst)
  (window-configuration-to-register :quickrun-shell)
  (let ((buf (get-buffer quickrun--buffer-name))
        (win (selected-window)))
    (pop-to-buffer buf)
    (let ((cmd-str (quickrun--concat-commands cmd-lst))
          (eshell-buf (get-buffer quickrun--eshell-buffer-name))
          (eshell-buffer-name quickrun--eshell-buffer-name)
          (eshell-banner-message ""))
      (when eshell-buf
        (kill-buffer eshell-buf))
      (eshell)
      (kill-buffer quickrun--buffer-name)
      (setq-local quickrun--shell-last-command cmd-str)
      (add-hook 'eshell-post-command-hook 'quickrun--eshell-post-hook)
      (quickrun--insert-command cmd-str)
      (unless quickrun-focus-p
        (select-window win)))))

(defsubst quickrun--default-directory ()
  (or quickrun-option-default-directory default-directory))

(defun quickrun--set-default-directory (cmd-key)
  (let ((cmd-info (quickrun--command-info cmd-key)))
    (quickrun--awhen (assoc-default :default-directory cmd-info)
      (let ((formatted (file-name-as-directory it)))
        (unless (file-directory-p formatted)
          (throw 'quickrun
                 (format "'%s' is not existed directory" it)))
        (setq quickrun-option-default-directory formatted)))))

(defsubst quickrun--process-connection-type (cmd)
  ;; for suppressing 'carriage return'(^M)
  (not (string-match-p "\\`php" cmd)))

(defun quickrun--exec-cmd (cmd)
  (let ((program (car (split-string cmd)))
        (buf (get-buffer quickrun--buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer))
    (let ((proc-name (format "quickrun-process-%s" program))
          (process-connection-type (quickrun--process-connection-type program))
          (default-directory (quickrun--default-directory)))
      (quickrun--log "Quickrun Execute: %s at %s" cmd default-directory)
      (let ((process (start-file-process-shell-command proc-name buf cmd)))
        (when (>= quickrun-timeout-seconds 0)
          (setq quickrun--timeout-timer
                (run-at-time quickrun-timeout-seconds nil
                             'quickrun--kill-process process)))
        process))))

(defun quickrun--kill-process (process)
  (when (eq (process-status process) 'run)
    (kill-process process))
  (let ((buf (get-buffer quickrun--buffer-name)))
    (with-current-buffer buf
      (insert (format "\nTime out %s(running over %d second)"
                      (process-name process)
                      quickrun-timeout-seconds)))
    (quickrun--remove-temp-files)
    (quickrun--pop-to-buffer buf (lambda () (read-only-mode +1)))))

(defun quickrun--remove-temp-files ()
  (quickrun--log "Quickrun remove %s" quickrun--remove-files)
  (dolist (file quickrun--remove-files)
    (cond
     ((file-directory-p file) (delete-directory file t))
     ((file-exists-p file) (delete-file file))))
  (setq quickrun--remove-files nil))

(defun quickrun--kill-running-process ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
        (message "No Process!!")
      (message "Kill process: %s" (process-name proc))
      (kill-process proc))))

(defvar quickrun--mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "C-c C-c") 'quickrun--kill-running-process)
    map))

(define-derived-mode quickrun--mode nil "Quickrun"
  ""
  (read-only-mode +1)
  (use-local-map quickrun--mode-map))

;;
;; Predefined outputter
;;

(defvar quickrun--defined-outputter-symbol
  '(
    (message  . quickrun--outputter-message)
    (browser  . quickrun--outputter-browser)
    (null     . quickrun--outputter-null)
    (replace  . quickrun--outputter-replace-region)
    (eval-print . quickrun--outputter-eval-print)
    ))

(defvar quickrun--defined-outputter-symbol-with-arg
  '(
    ("^file:"     . quickrun--outputter-file)
    ("^buffer:"   . quickrun--outputter-buffer)
    ("^variable:" . quickrun--outputter-variable)
    ))

(defun quickrun--recenter (arg)
  (with-selected-window (get-buffer-window quickrun--buffer-name)
    (recenter arg)))

(defun quickrun--default-outputter ()
  (quickrun--recenter -1))

(defun quickrun--outputter-multi-p (outputter)
  (and (not (functionp outputter)) (listp outputter)
       (eq (car outputter) 'multi)))

(defun quickrun--defined-outputter-p (outputter)
  (cond ((quickrun--outputter-multi-p outputter) t)
        ((or (symbolp outputter) (stringp outputter))
         (let ((name (or (and (symbolp outputter) (symbol-name outputter))
                         outputter)))
           (or (assoc outputter quickrun--defined-outputter-symbol)
               (assoc-default name
                              quickrun--defined-outputter-symbol-with-arg
                              'string-match))))))

(defun quickrun--outputter-file (file)
  (write-region (point-min) (point-max) file))

(defun quickrun--outputter-message ()
  (message "%s" (buffer-substring-no-properties (point-min) (point-max))))

(defun quickrun--outputter-browser ()
  (browse-url-of-region (point-min) (point-max)))

(defun quickrun--outputter-null ()
  (delete-region (point-min) (point-max))
  (kill-buffer (get-buffer quickrun--buffer-name)))

(defun quickrun--outputter-replace-region ()
  (let ((output (buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer quickrun--original-buffer
      (delete-region (region-beginning) (region-end))
      (insert output)
      (setq quickrun-option-outputter quickrun--original-outputter))))

(defun quickrun--outputter-eval-print ()
  (let ((output (buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer quickrun--original-buffer
      (forward-line 1)
      (let ((start (point)))
        (insert output)
        (comment-region start (point))
        (setq quickrun-option-outputter quickrun--original-outputter)))))

(defun quickrun--outputter-buffer (bufname)
  (let ((str (buffer-substring (point-min) (point-max))))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (insert str))))

(defun quickrun--outputter-variable (varname)
  (let ((symbol (intern varname)))
    (set symbol (buffer-substring (point-min) (point-max)))))

(defun quickrun--apply-outputter (op)
  (let ((buf (get-buffer quickrun--buffer-name))
        (origbuf (current-buffer))
        (outputters (or (and (quickrun--outputter-multi-p op) (cdr op))
                        (list op)))
        (outputter-func nil))
    (dolist (outputter outputters)
      (setq outputter-func outputter)
      (when (symbolp outputter)
        (let* ((name (symbol-name outputter))
               (func (assoc-default outputter
                                    quickrun--defined-outputter-symbol))
               (func-with-arg
                (assoc-default name
                               quickrun--defined-outputter-symbol-with-arg
                               'string-match)))
          (cond (func (setq outputter-func func))
                (func-with-arg
                 (when (string-match ":\\(.*\\)\\'" name)
                   (setq outputter-func
                         (lambda ()
                           (funcall func-with-arg
                                    (match-string 1 name)))))))))
      (with-current-buffer buf
        (let ((quickrun--original-buffer origbuf))
          (read-only-mode -1)
          (funcall outputter-func)
          (read-only-mode +1))))))

(defun quickrun--apply-compilation-mode (input-file mode)
  (when (not (string= input-file quickrun--executed-file))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (search-forward input-file nil t)
          (replace-match quickrun--executed-file)))))
  (compilation-mode mode))

(defun quickrun--apply-colorizing (input-file mode)
  (with-current-buffer (get-buffer quickrun--buffer-name)
    (read-only-mode -1)
    (when (and quickrun--executed-file input-file)
      (quickrun--apply-compilation-mode input-file mode)
      (read-only-mode -1))
    (quickrun--default-outputter)
    (goto-char (point-min))
    (read-only-mode +1)))

(defun quickrun--make-sentinel (rest-commands outputter-func input orig-mode)
  (lambda (process _event)
    ;; XXX Why reset `quickrun-option-outputter' ??
    (setq quickrun-option-outputter outputter-func)
    (when (memq (process-status process) '(exit signal))
      (and quickrun--timeout-timer (cancel-timer quickrun--timeout-timer))
      (delete-process process)
      (let* ((exit-status (process-exit-status process))
             (is-success (zerop exit-status)))
        (cond ((and is-success rest-commands)
               (quickrun--exec rest-commands input orig-mode))
              (t
               (if (not is-success)
                   (if (eq quickrun-option-outputter #'quickrun--default-outputter)
                       (quickrun--apply-colorizing input orig-mode)
                     (message "Failed: Exit Status=%d" exit-status))
                 (quickrun--apply-outputter outputter-func)
                 (run-hooks 'quickrun-after-run-hook))
               (when (eq outputter-func 'quickrun--default-outputter)
                 (cond ((> scroll-conservatively 0) (quickrun--recenter nil))
                       ((/= scroll-step 0) (quickrun--recenter -1))))
               (quickrun--remove-temp-files)))))))

;;
;; Composing command
;;
(defconst quickrun--template-place-holders
  '("%c" "%o" "%s" "%S" "%a" "%d" "%n" "%N" "%e" "%E")
  "A list of place holders of each language parameter.
Place holders are beginning with '%' and replaced by:
%c: :command parameter
%o: command options
%s: source code name
%S: source code name without extension
%a: program argument
%d: directory name
%n: absolute path of source code without extension
%N: source code path without extension
%e: absolute path of source code with executable extension(.exe, .out, .class)
%E: source code name with executable extension
")

(defun quickrun--executable-suffix (command)
  (cond ((string= command "java") ".class")
        ((quickrun--windows-p) ".exe")
        (t ".out")))

(defun quickrun--real-file-name (src)
  (let ((buffile (buffer-file-name)))
    (if (not (and buffile (file-remote-p buffile)))
        src
      (aref (tramp-dissect-file-name (buffer-file-name)) 3))))

(defun quickrun--place-holder-info (cmd cmdopt source args)
  (let* ((src (quickrun--real-file-name source))
         (without-extension (file-name-sans-extension src))
         (dirname (file-name-directory (expand-file-name src)))
         (directory (substring dirname 0 (- (length dirname) 1)))
         (executable-suffix (quickrun--executable-suffix cmd))
         (executable-name (concat without-extension executable-suffix)))
    `(("%c" . ,cmd)
      ("%o" . ,cmdopt)
      ("%s" . ,(file-name-nondirectory src))
      ("%S" . ,(file-name-nondirectory without-extension))
      ("%n" . ,(expand-file-name without-extension))
      ("%N" . ,without-extension)
      ("%d" . ,directory)
      ("%e" . ,(expand-file-name executable-name))
      ("%E" . ,executable-name)
      ("%a" . ,args))))

(defconst quickrun--default-tmpl-alist
  '((:exec . "%c %o %s %a")))

(defun quickrun--extract-template (key cmd-info &optional take-list)
  (let ((tmpl (or (assoc-default key cmd-info)
                  (assoc-default key quickrun--default-tmpl-alist))))
    (when tmpl
      (if take-list
          (mapcar 'quickrun--eval-parameter (quickrun--mklist tmpl))
        (quickrun--eval-parameter tmpl)))))

(defun quickrun--eval-parameter (param)
  (cond ((functionp param)
         (let* ((default-directory (quickrun--default-directory))
                (ret (funcall param)))
           (cond ((stringp ret) ret)
                 ((symbolp ret) (symbol-name ret))
                 (t
                  (throw 'quickrun
                         "template function should return symbol or string")))))
        (t param)))

(defun quickrun--get-shebang ()
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "#![ \t]*\\(.*\\)$")
      (match-string-no-properties 1))))

(defun quickrun--template-argument (cmd-info src)
  (let ((cmd (or quickrun-option-command
                 (and quickrun-option-shebang (quickrun--get-shebang))
                 (quickrun--eval-parameter (assoc-default :command cmd-info))
                 (throw 'quickrun "Not found :command parameter")))
        (cmd-opt (or quickrun-option-cmdopt
                     (quickrun--extract-template :cmdopt cmd-info) ""))
        (arg (or quickrun-option-args
                 (quickrun--extract-template :args cmd-info) "")))
    (quickrun--place-holder-info cmd cmd-opt src arg)))

(defun quickrun--fill-templates (cmd-key src)
  (let* ((cmd-info (quickrun--command-info cmd-key))
         (tmpl-arg (quickrun--template-argument cmd-info src))
         (info (make-hash-table)))
    ;; take one parameter
    (cl-loop for key in '(:compile-only)
             when (quickrun--extract-template key cmd-info)
             do (puthash key (quickrun--fill-template it tmpl-arg) info))
    ;; take one or more parameters
    (cl-loop for key in '(:exec :remove)
             when (quickrun--extract-template key cmd-info t)
             do
             (let ((filled-tmpls (mapcar (lambda (x)
                                           (quickrun--fill-template x tmpl-arg))
                                         it)))
               (puthash key filled-tmpls info)))
    ;; function parameter
    (dolist (key '(:outputter))
      (let ((func (assoc-default :outputter cmd-info)))
        (when (and func (or (functionp func) (symbolp func)))
          (puthash key func info))))
    info))

(defun quickrun--fill-template (tmpl info)
  (let ((place-holders quickrun--template-place-holders)
        (str tmpl)
        (case-fold-search nil))
    (dolist (holder place-holders str)
      (let ((rep (assoc-default holder info)))
        (setq str (replace-regexp-in-string holder rep str t))))))

;;
;; initialize
;;

(defconst quickrun--support-languages
  '("c" "c++" "objc" "c#" "perl" "perl6" "ruby" "python" "php" "emacs" "lisp" "scheme" "racket"
    "javascript" "clojure" "erlang" "ocaml" "fsharp" "go" "io" "haskell" "java"
    "d" "markdown" "coffee" "scala" "groovy" "sass" "less" "shellscript" "awk"
    "lua" "rust" "dart" "elixir" "tcl" "jsx" "typescript" "fortran" "haml"
    "swift" "ats" "r" "nim" "nimscript" "fish" "julia" "gnuplot")
  "Programming languages and Markup languages supported as default
by quickrun.el. But you can register your own command for some languages")

(defvar quickrun--command-key-table
  (make-hash-table :test 'equal))

;;;###autoload
(defun quickrun-set-default (lang key)
  "Set `key' as default key in programing language `lang'"
  (unless (assoc key quickrun--language-alist)
    (error "%s is not registered." key))
  (puthash lang key quickrun--command-key-table))

(defun quickrun--override-command (cmdkey cmd-alist)
  (let ((registered (assoc-default cmdkey quickrun--language-alist)))
    (unless registered
      (error (format "'%s' is not registered" cmdkey)))
    (cl-loop for old-param in registered
             do
             (let ((new-value (assoc-default (car old-param) cmd-alist)))
               (when new-value
                 (setcdr old-param new-value))))))

;;;###autoload
(cl-defun quickrun-add-command (key alist &key default mode override)
  (declare (indent defun))
  (cond ((not key) (error "Undefined 1st argument 'key'"))
        ((not alist) (error "Undefined 2nd argument 'command alist'")))
  (if override
      (quickrun--override-command key (copy-alist alist))
    (if (not (assoc :command alist))
        (error "not found :command parameter in language alist")
      (push (cons key (copy-alist alist)) quickrun--language-alist)))
  (let ((cmd-key (or default key)))
    (when default
      (puthash cmd-key key quickrun--command-key-table))
    (when mode
      (push (cons mode cmd-key) quickrun--major-mode-alist))
    key))

(defun quickrun--find-executable (candidates)
  (cl-loop for candidate in candidates
           when (executable-find candidate)
           return candidate))

(defun quickrun--set-command-key (lang candidates)
  (quickrun--awhen (quickrun--find-executable candidates)
    (puthash lang (format "%s/%s" lang it) quickrun--command-key-table)))

(defsubst quickrun--c-compiler ()
  (cond ((quickrun--windows-p) '("gcc" "clang" "cl"))
        ((eq system-type 'darwin) '("clang" "gcc"))
        (t '("gcc" "clang"))))

(defsubst quickrun--c++-compiler ()
  (cond ((quickrun--windows-p) '("g++" "clang++" "cl"))
        ((eq system-type 'darwin) '("clang++" "g++"))
        (t '("g++" "clang++"))))

(defconst quicklang/lang-candidates
  `(("c" . ,(quickrun--c-compiler))
    ("c++" . ,(quickrun--c++-compiler))
    ("c#" . ("mono"))
    ("fortran" . ("gfortran"))
    ("javascript" . ("node" "v8" "js" "jrunscript" "cscript"))
    ("ruby" . ("ruby" "mruby"))
    ("lisp" . ("clisp" "sbcl" "ccl"))
    ("scheme" . ("gosh"))
    ("swift" . ("xcrun" "swift"))
    ("markdown" . ("Markdown.pl" "kramdown" "bluecloth" "redcarpet" "pandoc"))
    ("clojure" . ("jark" "clj-env-dir"))
    ("go" . ("go" "gccgo")))
  "Candidates of language which has some compilers or interpreters")

(defun quickrun--init-command-key-table ()
  "Decide command for programing language which has multiple candidates"
  (dolist (lang quickrun--support-languages)
    (puthash lang lang quickrun--command-key-table))
  (cl-loop for (lang . candidates) in quicklang/lang-candidates
           do
           (quickrun--set-command-key lang candidates)))

(quickrun--init-command-key-table)

(defun quickrun--set-executed-file ()
  (let* ((buffer-file (buffer-file-name))
         (name (or buffer-file (buffer-name)))
         (use-stdin-file-p (quickrun--use-stdin-file-p))
         orig-file)
    (when (string-match (concat "\\(.+\\)" (quickrun--stdin-file-regexp)) name)
      (setq orig-file (match-string 1 name)))
    (if (and (not buffer-file) (not use-stdin-file-p))
        (setq quickrun--executed-file nil)
      (setq quickrun--executed-file
            (if use-stdin-file-p
                (if (not (file-exists-p orig-file))
                    (error "Can't find %s" orig-file)
                  orig-file)
              (file-name-nondirectory buffer-file))))))

;;
;; main
;;
;;;###autoload
(defun quickrun (&rest plist)
  "Run commands quickly for current buffer
   With universal prefix argument(C-u), select command-key,
   With double prefix argument(C-u C-u), run in compile-only-mode"
  (interactive)
  (quickrun--set-executed-file)
  (let ((beg (or (plist-get plist :start) (point-min)))
        (end (or (plist-get plist :end) (point-max)))
        (quickrun-option-cmd-alist (or quickrun-option-cmd-alist
                                       (plist-get plist :source)))
        (quickrun-timeout-seconds (or quickrun-option-timeout-seconds
                                      quickrun-timeout-seconds))
        (quickrun--compile-only-flag (or quickrun--compile-only-flag
                                         (and (consp current-prefix-arg)
                                              (= (car current-prefix-arg) 16)))))
    (let ((has-error (catch 'quickrun
                       (quickrun--common beg end)
                       nil)))
      (when has-error
        (message "%s" has-error)
        (quickrun--remove-temp-files)))))

(defvar quickrun--with-arg--history nil)

;;;###autoload
(defun quickrun-with-arg (arg)
  "Run commands quickly for current buffer with arguments"
  (interactive
   (list (read-string "QuickRun Arg: " nil 'quickrun--with-arg--history)))
  (let ((quickrun-option-args arg))
    (quickrun)))

(defvar quickrun--last-cmd-key nil)

(defun quickrun--prompt ()
  (let* ((default (or quickrun-option-cmdkey quickrun--last-cmd-key))
         (prompt (format "QuickRun Lang%s: "(if default
                                                (format "[Default: %s]" default)
                                              ""))))
    (completing-read prompt quickrun--language-alist nil nil nil nil default)))

(defun quickrun--region-command-common (start end)
  (deactivate-mark)
  (quickrun :start start :end end))

;;;###autoload
(defun quickrun-region (start end)
  "Run commands with specified region"
  (interactive "r")
  (quickrun--region-command-common start end))

;;;###autoload
(defun quickrun-replace-region (start end)
  "Run commands with specified region and replace"
  (interactive "r")
  (setq quickrun--original-outputter quickrun-option-outputter)
  (let ((quickrun-option-outputter 'replace))
    (quickrun--region-command-common start end)))

;;;###autoload
(defun quickrun-eval-print (start end)
  "Run commands with specified region and replace"
  (interactive "r")
  (setq quickrun--original-outputter quickrun-option-outputter)
  (let ((quickrun-option-outputter 'eval-print))
    (quickrun--region-command-common start end)))

;;;###autoload
(defun quickrun-compile-only ()
  "Exec only compilation"
  (interactive)
  (let ((quickrun--compile-only-flag t))
    (quickrun)))

;;;###autoload
(defun quickrun-shell ()
  "Run commands in shell for interactive programs"
  (interactive)
  (let ((quickrun--run-in-shell t)
        (quickrun-timeout-seconds nil))
    (quickrun)))

(defun quickrun--add-remove-files (removed-files)
  (let ((abs-paths (mapcar 'expand-file-name (quickrun--mklist removed-files))))
    (setq quickrun--remove-files (append abs-paths quickrun--remove-files))))

(defun quickrun--temp-name (src)
  (let* ((extension (file-name-extension src))
         (suffix (or (and extension (concat "." extension)) ""))
         (dir (quickrun--default-directory)))
    (expand-file-name (concat dir (make-temp-name "qr_") suffix))))

(defun quickrun--command-key (src)
  (let ((file-type (and src (quickrun--decide-file-type src)))
        (use-prefix-p (and (consp current-prefix-arg)
                           (= (car current-prefix-arg) 4))))
    (or (and use-prefix-p (quickrun--prompt))
        (and quickrun-option-cmd-alist "_user_defined") ;; setting dummy value
        quickrun-option-cmdkey
        (and (not src) (quickrun--prompt))
        (gethash file-type quickrun--command-key-table)
        file-type
        (quickrun--prompt))))

(defun quickrun--get-content (start end)
  (if (quickrun--use-stdin-file-p)
      (with-current-buffer (find-file-noselect quickrun--executed-file)
        (buffer-substring-no-properties (point-min) (point-max)))
    (buffer-substring-no-properties start end)))

(defun quickrun--copy-region-to-tempfile (start end dst)
  ;; Suppress write file message
  (let ((content (quickrun--get-content start end))
        (codec buffer-file-coding-system))
    (with-temp-file dst
      (set-buffer-file-coding-system codec)
      (insert content))
    (quickrun--add-remove-files dst)))

(defun quickrun--kill-quickrun-buffer ()
  (when (get-buffer quickrun--buffer-name)
    (kill-buffer quickrun--buffer-name)))

(defun quickrun--setup-exec-buffer (buf)
  (let ((default-dir (quickrun--default-directory)))
    (with-current-buffer buf
      (setq quickrun-option-default-directory default-dir))))

(defun quickrun--use-tempfile-p (cmd-key)
  (let ((buffile (buffer-file-name)))
    (unless (or quickrun--compile-only-flag (and buffile (file-remote-p buffile)))
      (let* ((cmdinfo (quickrun--command-info cmd-key))
             (tempfile-param (assoc :tempfile cmdinfo)))
        (if tempfile-param
            (cdr tempfile-param)
          t)))))

(defsubst quickrun--buffer-popup-p ()
  (and (not (quickrun--defined-outputter-p quickrun-option-outputter))
       (not quickrun--run-in-shell)))

(defun quickrun--common (start end)
  (let* ((orig-src quickrun--executed-file)
         (cmd-key (quickrun--command-key orig-src)))
    (quickrun--set-default-directory cmd-key)
    (quickrun--kill-quickrun-buffer)
    (unless (local-variable-p 'quickrun--last-cmd-key)
      (make-local-variable 'quickrun--last-cmd-key))
    (setq quickrun--last-cmd-key cmd-key)

    (let ((src (quickrun--temp-name (or orig-src ""))))
      (if (quickrun--use-tempfile-p cmd-key)
          (quickrun--copy-region-to-tempfile start end src)
        (setq src orig-src))
      (let ((cmd-info-hash (quickrun--fill-templates cmd-key src)))
        (quickrun--add-remove-files (gethash :remove cmd-info-hash))
        (unless quickrun-option-outputter
          (setq quickrun-option-outputter (gethash :outputter cmd-info-hash)))
        (cond (quickrun--compile-only-flag
               (let* ((cmd (gethash :compile-only cmd-info-hash))
                      (cmd-info (quickrun--command-info cmd-key))
                      (compile-conf (assoc-default :compile-conf cmd-info)))
                 (unless cmd
                   (throw 'quickrun
                          (format "%s does not support quickrun-compile-only"
                                  cmd-key)))
                 (quickrun--compilation-start cmd compile-conf)))
              (t
               (let ((buf (get-buffer-create quickrun--buffer-name)))
                 (quickrun--setup-exec-buffer buf)
                 (quickrun--exec (gethash :exec cmd-info-hash)
                                 (file-name-nondirectory src) major-mode)
                 (when (quickrun--buffer-popup-p)
                   (quickrun--pop-to-buffer buf 'quickrun--mode)))))))))

(defun quickrun--without-focus ()
  (let ((quickrun-focus-p nil))
    (quickrun)))

;;;###autoload
(define-minor-mode quickrun-autorun-mode
  "`quickrun' after saving buffer"
  :init-value nil
  :global nil
  :lighter " QAR"
  (if quickrun-autorun-mode
      (add-hook 'after-save-hook 'quickrun--without-focus nil t)
    (remove-hook 'after-save-hook 'quickrun--without-focus t)))

;;
;; helm/anything interface
;;

(defconst helm-quickrun--actions
  '(("Run this cmd-key" . quickrun--helm-action-default)
    ("Compile only" . quickrun--helm-compile-only)
    ("Run with shell" . quickrun--helm-action-shell)
    ("Run with argument" . quickrun--helm-run-with-arg)
    ("Replace region" . quickrun--helm-action-replace-region)
    ("Eval and insert as comment" . quickrun--helm-action-eval-print)))

(defvar helm-quickrun-source
  `((name . "Choose Command-Key")
    (volatile)
    (candidates . (lambda ()
                    (cl-loop for (cmd-key . cmd-info) in quickrun--language-alist
                             collect (quickrun--helm-candidate cmd-key cmd-info))))
    (action . ,helm-quickrun--actions))
  "helm/anything source of `quickrun'")

(defvar quickrun--helm-history nil)

(defvar helm-quickrun-history-source
  `((name . "Helm Quickrun History")
    (volatile)
    (candidates . quickrun--helm-history)
    (action . ,helm-quickrun--actions))
  "helm source of `quickrun' history")

(defun quickrun--helm-candidate (cmd-key cmd-info)
  (let ((description (or (assoc-default :description cmd-info) "")))
    (cons (format "%-25s %s" cmd-key description) cmd-key)))

(defun quickrun--helm-update-history (cmd-key)
  (setq quickrun--helm-history
        (cons cmd-key
              (delete cmd-key quickrun--helm-history))))

(defun quickrun--helm-action-default (cmd-key)
  (quickrun--helm-update-history cmd-key)
  (let ((quickrun-option-cmdkey cmd-key)
        start end)
    (if (use-region-p)
        (setq start (region-beginning) end (region-end))
      (setq start (point-min) end (point-max)))
    (quickrun-region start end)))

(defun quickrun--helm-run-with-arg (cmd-key)
  (quickrun--helm-update-history cmd-key)
  (let ((quickrun-option-cmdkey cmd-key))
    (call-interactively 'quickrun-with-arg)))

(defun quickrun--helm-action-shell (cmd-key)
  (quickrun--helm-update-history cmd-key)
  (let ((quickrun-option-cmdkey cmd-key))
    (quickrun-shell)))

(defun quickrun--helm-compile-only (cmd-key)
  (quickrun--helm-update-history cmd-key)
  (let ((quickrun-option-cmdkey cmd-key))
    (quickrun-compile-only)))

(defun quickrun--helm-action-replace-region (cmd-key)
  (quickrun--helm-update-history cmd-key)
  (let ((quickrun-option-cmdkey cmd-key))
    (quickrun-replace-region (region-beginning) (region-end))))

(defun quickrun--helm-action-eval-print (cmd-key)
  (quickrun--helm-update-history cmd-key)
  (let ((quickrun-option-cmdkey cmd-key))
    (quickrun-eval-print (region-beginning) (region-end))))

;;;###autoload
(defun anything-quickrun ()
  (interactive)
  (unless (featurep 'anything)
    (error "anything is not installed."))
  (anything helm-quickrun-source))

;;;###autoload
(defun helm-quickrun ()
  (interactive)
  (unless (featurep 'helm)
    (error "helm is not installed."))
  (let ((sources (if quickrun--helm-history
                     '(helm-quickrun-history-source helm-quickrun-source)
                   '(helm-quickrun-source))))
    (helm :sources sources :buffer "*helm quickrun*")))

(provide 'quickrun)
;;; quickrun.el ends here
