;;; log4e-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "log4e" "log4e.el" (0 0 0 0))
;;; Generated autoloads from log4e.el

(autoload 'log4e-mode "log4e" "\
Major mode for browsing a buffer made by log4e.

\\<log4e-mode-map>
\\{log4e-mode-map}

\(fn)" t nil)

(autoload 'log4e:insert-start-log-quickly "log4e" "\
Insert logging statment for trace level log at start of current function/macro.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "log4e" '("log4e")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; log4e-autoloads.el ends here
