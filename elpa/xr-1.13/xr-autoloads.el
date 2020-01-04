;;; xr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xr" "xr.el" (0 0 0 0))
;;; Generated autoloads from xr.el

(autoload 'xr "xr" "\
Convert a regexp string to rx notation; the inverse of `rx'.
Passing the returned value to `rx' (or `rx-to-string') yields a regexp string
equivalent to RE-STRING.  DIALECT controls the choice of keywords,
and is one of:
`verbose'       -- verbose keywords
`brief'         -- short keywords
`terse'         -- very short keywords
`medium' or nil -- a compromise (the default)

\(fn RE-STRING &optional DIALECT)" nil nil)

(autoload 'xr-skip-set "xr" "\
Convert a skip set string argument to rx notation.
SKIP-SET-STRING is interpreted according to the syntax of
`skip-chars-forward' and `skip-chars-backward' and converted to
a character class on `rx' form.
If desired, `rx' can then be used to convert the result to an
ordinary regexp.
See `xr' for a description of the DIALECT argument.

\(fn SKIP-SET-STRING &optional DIALECT)" nil nil)

(autoload 'xr-lint "xr" "\
Detect dubious practices and possible mistakes in RE-STRING.
This includes uses of tolerated but discouraged constructs.
Outright regexp syntax violations are signalled as errors.
Return a list of (OFFSET . COMMENT) where COMMENT applies at OFFSET
in RE-STRING.

\(fn RE-STRING)" nil nil)

(autoload 'xr-skip-set-lint "xr" "\
Detect dubious practices and possible mistakes in SKIP-SET-STRING.
This includes uses of tolerated but discouraged constructs.
Outright syntax violations are signalled as errors.
The argument is interpreted according to the syntax of
`skip-chars-forward' and `skip-chars-backward'.
Return a list of (OFFSET . COMMENT) where COMMENT applies at OFFSET
in SKIP-SET-STRING.

\(fn SKIP-SET-STRING)" nil nil)

(autoload 'xr-pp "xr" "\
Convert to `rx' notation and output the pretty-printed result.
This function uses `xr' to translate RE-STRING into DIALECT.
It is intended for use from an interactive elisp session.
See `xr' for a description of the DIALECT argument.

\(fn RE-STRING &optional DIALECT)" nil nil)

(autoload 'xr-skip-set-pp "xr" "\
Convert a skip set string to `rx' notation and pretty-print.
This function uses `xr-skip-set' to translate SKIP-SET-STRING
into DIALECT.
It is intended for use from an interactive elisp session.
See `xr' for a description of the DIALECT argument.

\(fn SKIP-SET-STRING &optional DIALECT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xr" '("xr-")))

;;;***

;;;### (autoloads nil nil ("xr-pkg.el" "xr-test.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xr-autoloads.el ends here
