(use-package python-mode
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))

(use-package lsp-python-ms
  :after lsp-mode python
  :custom
  (lsp-python-executable-cmd "python3"))

(use-package pyvenv)

(provide 'init-python)
