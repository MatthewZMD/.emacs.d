;;; mu4e-alert-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mu4e-alert" "mu4e-alert.el" (0 0 0 0))
;;; Generated autoloads from mu4e-alert.el

(autoload 'mu4e-alert-set-default-style "mu4e-alert" "\
Set the default style for unread email notifications.

VALUE is the value to be used as the default style.

\(fn VALUE)" nil nil)

(autoload 'mu4e-alert-enable-mode-line-display "mu4e-alert" "\
Enable display of unread emails in mode-line.

\(fn)" t nil)

(autoload 'mu4e-alert-enable-notifications "mu4e-alert" "\
Enable desktop notifications for unread emails.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mu4e-alert" '("mu4e-alert-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mu4e-alert-autoloads.el ends here
