;;; reformatter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "reformatter" "reformatter.el" (0 0 0 0))
;;; Generated autoloads from reformatter.el

(autoload 'reformatter-define "reformatter" "\
Define a reformatter command with NAME.

When called, the reformatter will use PROGRAM and any ARGS to
reformat the current buffer.  The contents of the buffer will be
passed as standard input to the reformatter, which should output
them to standard output.  A nonzero exit code will be reported as
failure, and the output of the command to standard error will be
displayed to the user.

The macro accepts the following keyword arguments:

:program (required)

  Provides a form which should evaluate to a string at runtime,
  e.g. a literal string, or the name of a variable which holds
  the program path.

:args

  If provided, this is a form which evaluates to a list of
  strings at runtime.  Default is the empty list.

:mode

  Unless nil, also generate a minor mode that will call the
  reformatter command from `before-save-hook' when enabled.
  Default is t.

:group

  If provided, this is the custom group used for any generated
  modes or custom variables.  Don't forget to declare this group
  using a `defgroup' form.

:lighter

  If provided, this is a mode lighter string which will be used
  for the \"-on-save\" minor mode.  It should have a leading
  space.  The supplied value will be used as the default for a
  generated custom variable which specifies the mode lighter.
  Default is nil, ie. no lighter.

:keymap

  If provided, this is the symbol name of the \"-on-save\" mode's
  keymap, which you must declare yourself.  Default is no keymap.

\(fn NAME &key PROGRAM ARGS (MODE t) LIGHTER KEYMAP GROUP)" nil t)

(function-put 'reformatter-define 'lisp-indent-function 'defun)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "reformatter" '("reformatter-replace-buffer-contents-from-file")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; reformatter-autoloads.el ends here
