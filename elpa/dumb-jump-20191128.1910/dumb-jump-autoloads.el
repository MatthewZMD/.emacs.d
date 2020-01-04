;;; dumb-jump-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dumb-jump" "dumb-jump.el" (0 0 0 0))
;;; Generated autoloads from dumb-jump.el

(defvar dumb-jump-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-M-g") 'dumb-jump-go) (define-key map (kbd "C-M-p") 'dumb-jump-back) (define-key map (kbd "C-M-q") 'dumb-jump-quick-look) map))

(autoload 'dumb-jump-back "dumb-jump" "\
Jump back to where the last jump was done.

\(fn)" t nil)

(autoload 'dumb-jump-quick-look "dumb-jump" "\
Run dumb-jump-go in quick look mode.  That is, show a tooltip of where it would jump instead.

\(fn)" t nil)

(autoload 'dumb-jump-go-other-window "dumb-jump" "\
Like 'dumb-jump-go' but use 'find-file-other-window' instead of 'find-file'.

\(fn)" t nil)

(autoload 'dumb-jump-go-current-window "dumb-jump" "\
Like dumb-jump-go but always use 'find-file'.

\(fn)" t nil)

(autoload 'dumb-jump-go-prefer-external "dumb-jump" "\
Like dumb-jump-go but prefer external matches from the current file.

\(fn)" t nil)

(autoload 'dumb-jump-go-prompt "dumb-jump" "\
Like dumb-jump-go but prompts for function instead of using under point

\(fn)" t nil)

(autoload 'dumb-jump-go-prefer-external-other-window "dumb-jump" "\
Like dumb-jump-go-prefer-external but use 'find-file-other-window' instead of 'find-file'.

\(fn)" t nil)

(autoload 'dumb-jump-go "dumb-jump" "\
Go to the function/variable declaration for thing at point.
When USE-TOOLTIP is t a tooltip jump preview will show instead.
When PREFER-EXTERNAL is t it will sort external matches before
current file.

\(fn &optional USE-TOOLTIP PREFER-EXTERNAL PROMPT)" t nil)

(defvar dumb-jump-mode nil "\
Non-nil if Dumb-Jump mode is enabled.
See the `dumb-jump-mode' command
for a description of this minor mode.")

(custom-autoload 'dumb-jump-mode "dumb-jump" nil)

(autoload 'dumb-jump-mode "dumb-jump" "\
Minor mode for jumping to variable and function definitions

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dumb-jump" '("dumb-jump-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dumb-jump-autoloads.el ends here
