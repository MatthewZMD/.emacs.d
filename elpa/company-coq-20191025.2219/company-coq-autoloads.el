;;; company-coq-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-coq" "company-coq.el" (0 0 0 0))
;;; Generated autoloads from company-coq.el

(autoload 'company-coq-tutorial "company-coq" "\
Open the company-coq tutorial, creating a new buffer if needed.

\(fn)" t nil)

(autoload 'company-coq-describe-feature "company-coq" "\
Describe company-coq feature FEATURE.

\(fn FEATURE)" t nil)

(autoload 'company-coq-mode "company-coq" "\
Toggle company-coq-mode on or off.

Company-Coq is a collection of Proof-General extensions.  See
https://github.com/cpitclaudel/company-coq/ for a detailed
description, including screenshots and documentation.  First time
users may want to use \\[company-coq-tutorial] to open the
tutorial.

With a prefix argument ARG, enable %s if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

\\{company-coq-map}

\(fn &optional ARG)" t nil)

(autoload 'company-coq-initialize "company-coq" "\
Deprecated: Use `company-coq-mode' instead.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-coq" '("toggle-company-coq-debug" "company-coq-")))

;;;***

;;;### (autoloads nil "company-coq-abbrev" "company-coq-abbrev.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-coq-abbrev.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-coq-abbrev" '("company-coq--refman-")))

;;;***

;;;### (autoloads nil "company-coq-tg" "company-coq-tg.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from company-coq-tg.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-coq-tg" '("company-coq-")))

;;;***

;;;### (autoloads nil "company-coq-utils" "company-coq-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-coq-utils.el

(autoload 'company-coq-cite "company-coq-utils" "\
Insert BibTeX entries for Coq, PG, and company-coq.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-coq-utils" '("company-coq--")))

;;;***

;;;### (autoloads nil nil ("company-coq-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-coq-autoloads.el ends here
