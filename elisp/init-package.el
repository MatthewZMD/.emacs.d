(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			             ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; Ensure the current path is set to pick-up system packages
(use-package exec-path-from-shell
  :ensure
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :custom
  (system-packages-package-manager 'brew) ;; It tries to use apt on my Mac
  (system-packages-use-sudo nil))

(provide 'init-package)
