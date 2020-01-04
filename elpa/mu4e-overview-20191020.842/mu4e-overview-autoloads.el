;;; mu4e-overview-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mu4e-overview" "mu4e-overview.el" (0 0 0 0))
;;; Generated autoloads from mu4e-overview.el

(autoload 'mu4e-overview "mu4e-overview" "\
Display a buffer with a list of known mail folders.
The buffer shows a hierarchy of maildirs used by `mu4e'.

The available keybindings are:
\\{mu4e-overview-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mu4e-overview" '("mu4e-overview-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mu4e-overview-autoloads.el ends here
