;;; disk-usage-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "disk-usage" "disk-usage.el" (0 0 0 0))
;;; Generated autoloads from disk-usage.el

(autoload 'disk-usage "disk-usage" "\
Display listing of files in DIRECTORY with their size.
If DIRECTORY is nil, use current directory.

\(fn &optional DIRECTORY)" t nil)

(autoload 'disk-usage-here "disk-usage" "\
Run `disk-usage' in current directory.

\(fn)" t nil)

(autoload 'disk-usage-by-types "disk-usage" "\


\(fn &optional DIRECTORY)" t nil)

(autoload 'disk-usage-by-types-here "disk-usage" "\
Run `disk-usage-by-types' in current directory.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "disk-usage" '("disk-usage-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; disk-usage-autoloads.el ends here
