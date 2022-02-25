(use-package rbenv
  :config (global-rbenv-mode))

(use-package rubocop)

(use-package ruby-mode
  :after lsp-mode
  ;; Needed for lsp
  :ensure-system-package (solargraph . "gem install solargraph"))

(provide 'init-ruby)
