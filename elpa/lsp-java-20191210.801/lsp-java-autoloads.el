;;; lsp-java-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-java" "lsp-java.el" (0 0 0 0))
;;; Generated autoloads from lsp-java.el
(with-eval-after-load 'lsp-mode (require 'lsp-java))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-java" '("lsp-j")))

;;;***

;;;### (autoloads nil "lsp-java-boot" "lsp-java-boot.el" (0 0 0 0))
;;; Generated autoloads from lsp-java-boot.el

(autoload 'lsp-java-boot-lens-mode "lsp-java-boot" "\
Toggle code-lens overlays.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-java-boot" '("lsp-java-boot-")))

;;;***

;;;### (autoloads nil "lsp-jt" "lsp-jt.el" (0 0 0 0))
;;; Generated autoloads from lsp-jt.el

(autoload 'lsp-jt-lens-mode "lsp-jt" "\
Toggle code-lens overlays.

\(fn &optional ARG)" t nil)

(autoload 'lsp-jt-browser "lsp-jt" "\


\(fn)" t nil)

(autoload 'lsp-jt-show-report "lsp-jt" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-jt" '("lsp-" "java-tests--roots")))

;;;***

;;;### (autoloads nil nil ("lsp-java-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-java-autoloads.el ends here
