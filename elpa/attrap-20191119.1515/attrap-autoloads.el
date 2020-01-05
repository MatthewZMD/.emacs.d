;;; attrap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "attrap" "attrap.el" (0 0 0 0))
;;; Generated autoloads from attrap.el

(autoload 'attrap-flymake "attrap" "\
Attempt to repair the flymake error at POS.

\(fn POS)" t nil)

(autoload 'attrap-flycheck "attrap" "\
Attempt to repair the flycheck error at POS.

\(fn POS)" t nil)

(autoload 'attrap-attrap "attrap" "\
Attempt to repair the error at POS.

\(fn POS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "attrap" '("attrap-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; attrap-autoloads.el ends here
