;;; discover-my-major-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "discover-my-major" "discover-my-major.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from discover-my-major.el

(autoload 'discover-my-major "discover-my-major" "\
Create a makey popup listing all major-mode keys with their description.
If ARG is non-nil recreate the makey popup function even if it is already defined.

\(fn ARG)" t nil)

(autoload 'discover-my-mode "discover-my-major" "\
Create a makey popup listing all MODE keys with their description.

\(fn MODE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "discover-my-major" '("dmm/")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; discover-my-major-autoloads.el ends here
