;;; which-key-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "which-key" "which-key.el" (0 0 0 0))
;;; Generated autoloads from which-key.el

(defvar which-key-mode nil "\
Non-nil if Which-Key mode is enabled.
See the `which-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-mode'.")

(custom-autoload 'which-key-mode "which-key" nil)

(autoload 'which-key-mode "which-key" "\
Toggle which-key-mode.

\(fn &optional ARG)" t nil)

(autoload 'which-key-setup-side-window-right "which-key" "\
Apply suggested settings for side-window that opens on right.

\(fn)" t nil)

(autoload 'which-key-setup-side-window-right-bottom "which-key" "\
Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise.

\(fn)" t nil)

(autoload 'which-key-setup-side-window-bottom "which-key" "\
Apply suggested settings for side-window that opens on
bottom.

\(fn)" t nil)

(autoload 'which-key-setup-minibuffer "which-key" "\
Apply suggested settings for minibuffer.
Do not use this setup if you use the paging commands. Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional.

\(fn)" t nil)

(autoload 'which-key-add-key-based-replacements "which-key" "\
Replace the description of KEY-SEQUENCE with REPLACEMENT.
KEY-SEQUENCE is a string suitable for use in `kbd'. REPLACEMENT
may either be a string, as in

\(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")

a cons of two strings as in

\(which-key-add-key-based-replacements \"C-x 8\"
                                        '(\"unicode\" . \"Unicode keys\"))

or a function that takes a (KEY . BINDING) cons and returns a
replacement.

In the second case, the second string is used to provide a longer
name for the keys under a prefix.

MORE allows you to specifcy additional KEY REPLACEMENT pairs.  All
replacements are added to
`which-key-key-based-description-replacement-alist'.

\(fn KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil)

(autoload 'which-key-add-major-mode-key-based-replacements "which-key" "\
Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply.

\(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil)

(autoload 'which-key-reload-key-sequence "which-key" "\
Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. If nil, KEY-SEQ defaults to
`which-key--current-key-list'. Any prefix arguments that were
used are reapplied to the new key sequence.

\(fn &optional KEY-SEQ)" nil nil)

(autoload 'which-key-show-standard-help "which-key" "\
Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

\(fn &optional _)" t nil)

(autoload 'which-key-show-next-page-no-cycle "which-key" "\
Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'.

\(fn)" t nil)

(autoload 'which-key-show-previous-page-no-cycle "which-key" "\
Show previous page of keys unless on the first page, in which
case do nothing.

\(fn)" t nil)

(autoload 'which-key-show-next-page-cycle "which-key" "\
Show the next page of keys, cycling from end to beginning
after last page.

\(fn &optional _)" t nil)

(autoload 'which-key-show-previous-page-cycle "which-key" "\
Show the previous page of keys, cycling from beginning to end
after first page.

\(fn &optional _)" t nil)

(autoload 'which-key-show-top-level "which-key" "\
Show top-level bindings.

\(fn &optional _)" t nil)

(autoload 'which-key-show-major-mode "which-key" "\
Show top-level bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. 

\(fn)" t nil)

(autoload 'which-key-undo-key "which-key" "\
Undo last keypress and force which-key update.

\(fn &optional _)" t nil)

(autoload 'which-key-C-h-dispatch "which-key" "\
Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil.

\(fn)" t nil)

(autoload 'which-key-show-keymap "which-key" "\
Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively from all available keymaps.

\(fn KEYMAP)" t nil)

(autoload 'which-key-show-full-keymap "which-key" "\
Show all bindings in KEYMAP using which-key. KEYMAP is
selected interactively from all available keymaps.

\(fn KEYMAP)" t nil)

(autoload 'which-key-show-minor-mode-keymap "which-key" "\
Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively by mode in `minor-mode-map-alist'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "which-key" '("which-key-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; which-key-autoloads.el ends here
