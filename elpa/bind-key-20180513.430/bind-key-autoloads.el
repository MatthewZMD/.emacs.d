;;; bind-key-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bind-key" "bind-key.el" (0 0 0 0))
;;; Generated autoloads from bind-key.el

(autoload 'bind-key "bind-key" "\
Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap and not a quoted symbol.
For example:

  (bind-key \"M-h\" #'some-interactive-function my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

\(fn KEY-NAME COMMAND &optional KEYMAP PREDICATE)" nil t)

(autoload 'unbind-key "bind-key" "\
Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details.

\(fn KEY-NAME &optional KEYMAP)" nil t)

(autoload 'bind-key* "bind-key" "\
Similar to `bind-key', but overrides any mode-specific bindings.

\(fn KEY-NAME COMMAND &optional PREDICATE)" nil t)

(autoload 'bind-keys "bind-key" "\
Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

\(fn &rest ARGS)" nil t)

(autoload 'bind-keys* "bind-key" "\


\(fn &rest ARGS)" nil t)

(autoload 'describe-personal-keybindings "bind-key" "\
Display all the personal keybindings defined by `bind-key'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bind-key" '("compare-keybindings" "get-binding-description" "bind-key" "personal-keybindings" "override-global-m")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bind-key-autoloads.el ends here
