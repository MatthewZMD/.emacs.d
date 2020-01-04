;;; yasnippet-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yasnippet-snippets" "yasnippet-snippets.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from yasnippet-snippets.el

(autoload 'yasnippet-snippets-initialize "yasnippet-snippets" "\
Load the `yasnippet-snippets' snippets directory.

\(fn)" nil nil)

(eval-after-load 'yasnippet '(yasnippet-snippets-initialize))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yasnippet-snippets" '("yasnippet-snippets-")))

;;;***

;;;### (autoloads nil nil ("yasnippet-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yasnippet-snippets-autoloads.el ends here
