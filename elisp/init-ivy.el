;;; -*- lexical-binding: t; -*-

;; Ivy
(def-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-on-del-error-function nil)
  (setq ivy-magic-slash-non-match-action nil)
  (setq ivy-count-format "【%d/%d】")
  (setq ivy-wrap t))

;; Amx
(def-package amx
  :after (:any ivy ido)
  :config (amx-mode))

;; Counsel
(def-package counsel
  :after ivy
  :diminish counsel-mode
  :init (counsel-mode 1))


(provide 'init-ivy)
