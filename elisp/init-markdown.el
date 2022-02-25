;; Install markdown-mode which also a GitHub Flavour Markdown (GFM) mode too
(use-package markdown-mode
  :after flycheck
  :commands (markdown-mode gfm-mode)
  ;; Install markdownlint for Flycheck
  :ensure-system-package (markdownlint . markdownlint-cli)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :custom
  (flycheck-markdown-markdownlint-cli-config "~/.emacs.d/.markdownlintrc"))

(provide 'init-markdown)
