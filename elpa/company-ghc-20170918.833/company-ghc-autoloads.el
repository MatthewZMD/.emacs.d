;;; company-ghc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-ghc" "company-ghc.el" (0 0 0 0))
;;; Generated autoloads from company-ghc.el

(autoload 'company-ghc "company-ghc" "\
`company-mode' completion back-end for `haskell-mode' via ghc-mod.
Provide completion info according to COMMAND and ARG.  IGNORED, not used.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'company-ghc-diagnose "company-ghc" "\
Show diagnostic info of the current buffer in other buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-ghc" '("company-ghc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-ghc-autoloads.el ends here
