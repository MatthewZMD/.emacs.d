;;; super-save-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "super-save" "super-save.el" (0 0 0 0))
;;; Generated autoloads from super-save.el

(defvar super-save-mode nil "\
Non-nil if Super-Save mode is enabled.
See the `super-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `super-save-mode'.")

(custom-autoload 'super-save-mode "super-save" nil)

(autoload 'super-save-mode "super-save" "\
A minor mode that saves your Emacs buffers when they lose focus.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "super-save" '("super-save-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; super-save-autoloads.el ends here
