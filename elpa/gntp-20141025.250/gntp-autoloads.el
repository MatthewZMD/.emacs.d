;;; gntp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gntp" "gntp.el" (0 0 0 0))
;;; Generated autoloads from gntp.el

(autoload 'gntp-notify "gntp" "\
Send notification NAME with TITLE, TEXT, PRIORITY and ICON to SERVER:PORT.
PORT defaults to `gntp-server-port'

\(fn NAME TITLE TEXT SERVER &optional PORT PRIORITY ICON)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gntp" '("gntp-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gntp-autoloads.el ends here
