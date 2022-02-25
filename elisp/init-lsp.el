(use-package which-key
    :config
    (which-key-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; TODO: Enable this in init-ruby.el instead of here
  :hook ((ruby-mode . lsp) 
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
