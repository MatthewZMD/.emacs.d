;;; format-all-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "format-all" "format-all.el" (0 0 0 0))
;;; Generated autoloads from format-all.el

(autoload 'format-all-buffer "format-all" "\
Auto-format the source code in the current buffer.

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
they are shown in a buffer called *format-all-errors*.

\(fn)" t nil)

(autoload 'format-all-mode "format-all" "\
Toggle automatic source code formatting before save.

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
or zero, and enabled otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "format-all" '("fish-indent" "format-all-" "terraform-fmt" "shfmt" "sqlformat" "styler" "swiftformat" "rufo" "rustfmt" "perltidy" "prettier" "ocp-indent" "nixfmt" "mix-format" "latexindent" "ledger-mode" "lua-fmt" "ktlint" "istyle-verilog" "html-tidy" "gofmt" "elm-format" "emacs-lisp" "define-format-all-formatter" "dartfmt" "dfmt" "dhall" "dockfmt" "cmake-format" "crystal" "bibtex-mode" "black" "buildifier" "brittany" "asmfmt")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; format-all-autoloads.el ends here
