;;; json-reformat-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "json-reformat" "json-reformat.el" (0 0 0 0))
;;; Generated autoloads from json-reformat.el

(autoload 'json-reformat-region "json-reformat" "\
Reformat the JSON in the specified region.

If you want to customize the reformat style,
please see the documentation of `json-reformat:indent-width'
and `json-reformat:pretty-string?'.

\(fn BEGIN END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "json-reformat" '("json-reformat")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; json-reformat-autoloads.el ends here
