;;; dante-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dante" "dante.el" (0 0 0 0))
;;; Generated autoloads from dante.el

(autoload 'dante-mode "dante" "\
Minor mode for Dante.

`dante-mode' takes one optional (prefix) argument.
Interactively with no prefix argument, it toggles dante.
A prefix argument enables dante if the argument is positive,
and disables it otherwise.

When called from Lisp, the `dante-mode' toggles dante if the
argument is `toggle', disables dante if the argument is a
non-positive integer, and enables dante otherwise (including
if the argument is omitted or nil or a positive integer).

\\{dante-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dante" '("dante-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dante-autoloads.el ends here
