;;; term-keys-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "term-keys" "term-keys.el" (0 0 0 0))
;;; Generated autoloads from term-keys.el

(autoload 'term-keys/init "term-keys" "\
Set up configured key sequences for the current terminal.

\(fn)" t nil)

(defvar term-keys-mode nil "\
Non-nil if Term-Keys mode is enabled.
See the `term-keys-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `term-keys-mode'.")

(custom-autoload 'term-keys-mode "term-keys" nil)

(autoload 'term-keys-mode "term-keys" "\
`term-keys' global minor mode.

When enabled, automatically set up configured keys for new frames
on TTY terminals.  If the current frame is on a TTY, set it up as
well.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "term-keys" '("term-keys/")))

;;;***

;;;### (autoloads nil "term-keys-konsole" "term-keys-konsole.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from term-keys-konsole.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "term-keys-konsole" '("term-keys/konsole-")))

;;;***

;;;### (autoloads nil "term-keys-linux" "term-keys-linux.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from term-keys-linux.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "term-keys-linux" '("term-keys/linux-")))

;;;***

;;;### (autoloads nil "term-keys-st" "term-keys-st.el" (0 0 0 0))
;;; Generated autoloads from term-keys-st.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "term-keys-st" '("term-keys/")))

;;;***

;;;### (autoloads nil "term-keys-terminal-app" "term-keys-terminal-app.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from term-keys-terminal-app.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "term-keys-terminal-app" '("term-keys/terminal-app-")))

;;;***

;;;### (autoloads nil "term-keys-urxvt" "term-keys-urxvt.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from term-keys-urxvt.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "term-keys-urxvt" '("term-keys/urxvt-")))

;;;***

;;;### (autoloads nil "term-keys-xterm" "term-keys-xterm.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from term-keys-xterm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "term-keys-xterm" '("term-keys/xterm-")))

;;;***

;;;### (autoloads nil nil ("term-keys-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; term-keys-autoloads.el ends here
