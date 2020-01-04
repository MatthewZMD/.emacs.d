;;; julia-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "julia-mode" "julia-mode.el" (0 0 0 0))
;;; Generated autoloads from julia-mode.el

(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

(autoload 'julia-mode "julia-mode" "\
Major mode for editing julia code.

\(fn)" t nil)

(autoload 'inferior-julia "julia-mode" "\
Run an inferior instance of `julia' inside Emacs.

\(fn)" t nil)

(defalias 'run-julia #'inferior-julia "\
Run an inferior instance of `julia' inside Emacs.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "julia-mode" '("inferior-julia-" "julia-" "latexsub")))

;;;***

;;;### (autoloads nil "julia-mode-latexsubs" "julia-mode-latexsubs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from julia-mode-latexsubs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "julia-mode-latexsubs" '("julia-mode-latexsubs")))

;;;***

;;;### (autoloads nil nil ("julia-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; julia-mode-autoloads.el ends here
