(use-package company
  :diminish company-mode
  :hook ((prog-mode) . company-mode)
  :custom
  (company-minimum-prefix-length 3)
  (company-tooltip-align-annotations t)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  (global-company-mode 1))

(provide 'init-company)
