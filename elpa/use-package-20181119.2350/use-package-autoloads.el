;;; use-package-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "use-package-bind-key" "use-package-bind-key.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-bind-key.el

(autoload 'use-package-autoload-keymap "use-package-bind-key" "\
Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
deferred until the prefix key sequence is pressed.

\(fn KEYMAP-SYMBOL PACKAGE OVERRIDE)" nil nil)

(autoload 'use-package-normalize-binder "use-package-bind-key" "\


\(fn NAME KEYWORD ARGS)" nil nil)

(defalias 'use-package-normalize/:bind 'use-package-normalize-binder)

(defalias 'use-package-normalize/:bind* 'use-package-normalize-binder)

(defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode)

(defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode)

(autoload 'use-package-handler/:bind "use-package-bind-key" "\


\(fn NAME KEYWORD ARGS REST STATE &optional BIND-MACRO)" nil nil)

(defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder)

(defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder)

(autoload 'use-package-handler/:bind-keymap "use-package-bind-key" "\


\(fn NAME KEYWORD ARGS REST STATE &optional OVERRIDE)" nil nil)

(autoload 'use-package-handler/:bind-keymap* "use-package-bind-key" "\


\(fn NAME KEYWORD ARG REST STATE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-bind-key" '("use-package-handler/:bind*")))

;;;***

;;;### (autoloads nil "use-package-core" "use-package-core.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from use-package-core.el

(autoload 'use-package "use-package-core" "\
Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Defer loading of a package until after any of the named
                 features are loaded.
:demand          Prevent deferred loading in all cases.

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `customize-set-variable' with each variable definition.
:custom-face     Call `customize-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive.

\(fn NAME &rest ARGS)" nil t)

(function-put 'use-package 'lisp-indent-function '1)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-core" '("use-package-")))

;;;***

;;;### (autoloads nil "use-package-delight" "use-package-delight.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-delight.el

(autoload 'use-package-normalize/:delight "use-package-delight" "\
Normalize arguments to delight.

\(fn NAME KEYWORD ARGS)" nil nil)

(autoload 'use-package-handler/:delight "use-package-delight" "\


\(fn NAME KEYWORD ARGS REST STATE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-delight" '("use-package-normalize-delight")))

;;;***

;;;### (autoloads nil "use-package-diminish" "use-package-diminish.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-diminish.el

(autoload 'use-package-normalize/:diminish "use-package-diminish" "\


\(fn NAME KEYWORD ARGS)" nil nil)

(autoload 'use-package-handler/:diminish "use-package-diminish" "\


\(fn NAME KEYWORD ARG REST STATE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-diminish" '("use-package-normalize-diminish")))

;;;***

;;;### (autoloads nil "use-package-ensure" "use-package-ensure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-ensure.el

(autoload 'use-package-normalize/:ensure "use-package-ensure" "\


\(fn NAME KEYWORD ARGS)" nil nil)

(autoload 'use-package-handler/:ensure "use-package-ensure" "\


\(fn NAME KEYWORD ENSURE REST STATE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-ensure" '("use-package-")))

;;;***

;;;### (autoloads nil "use-package-jump" "use-package-jump.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from use-package-jump.el

(autoload 'use-package-jump-to-package-form "use-package-jump" "\
Attempt to find and jump to the `use-package' form that loaded
PACKAGE. This will only find the form if that form actually
required PACKAGE. If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead.

\(fn PACKAGE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-jump" '("use-package-find-require")))

;;;***

;;;### (autoloads nil "use-package-lint" "use-package-lint.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from use-package-lint.el

(autoload 'use-package-lint "use-package-lint" "\
Check for errors in use-package declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-lint" '("use-package-lint-declaration")))

;;;***

;;;### (autoloads nil nil ("use-package-pkg.el" "use-package.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; use-package-autoloads.el ends here
