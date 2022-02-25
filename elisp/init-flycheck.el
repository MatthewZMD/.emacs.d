;; Use Flycheck instead of the older Flymake built into Emacs

(use-package flycheck
  :defer t
  :diminish
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))

(provide 'init-flycheck)
