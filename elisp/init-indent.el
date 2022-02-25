(use-package highlight-indent-guides
  :if (display-graphic-p)
  :diminish
  :hook ((prog-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 7))

(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default fill-column 100)

(setq-default electric-indent-mode -1)

(add-hook 'prog-mode-hook
          (lambda () (electric-indent-local-mode t)))

(provide 'init-indent)
