;;; sudo-edit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sudo-edit" "sudo-edit.el" (0 0 0 0))
;;; Generated autoloads from sudo-edit.el

(autoload 'sudo-edit-set-header "sudo-edit" "\
*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook' and `dired-file-hook'.

\(fn)" nil nil)

(defvar sudo-edit-indicator-mode nil "\
Non-nil if Sudo-Edit-Indicator mode is enabled.
See the `sudo-edit-indicator-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `sudo-edit-indicator-mode'.")

(custom-autoload 'sudo-edit-indicator-mode "sudo-edit" nil)

(autoload 'sudo-edit-indicator-mode "sudo-edit" "\
Indicates editing as root by displaying a message in the header line.

\(fn &optional ARG)" t nil)

(autoload 'sudo-edit "sudo-edit" "\
Edit currently visited file as another user, by default `sudo-edit-user'.

With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sudo-edit" '("sudo-edit-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sudo-edit-autoloads.el ends here
