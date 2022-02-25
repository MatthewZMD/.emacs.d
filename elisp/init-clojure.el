(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :after (company)
  :config
  (add-hook 'clojure-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'projectile-mode)
  (require 'flycheck-clj-kondo))

(use-package cider
  :after (clojure-mode company)
  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

(provide 'init-clojure)
