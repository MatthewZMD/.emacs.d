;;; init.el --- Initialise Emacs using my configuration
;;
;;; Commentary:
;;
;; This configuration will only work on Mac OS X as it is the only OS I have available for testing.
;;
;; This config is heavily based on my previous config which itself was a version of M-Emacs:
;; https://github.com/MatthewZMD/.emacs.d.  I have reconstructed it from the ground up trying to
;; keep customisations to a minimum.
;;
;;; Code:

;; Increase the default GC threshold
(setq gc-cons-threshold 104857600) ;; 100mb

;; Keep an eye on startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Add the location of the features to the load path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Lockfiles are not needed
(setq-default create-lockfiles nil)

;; Use more recent compiled files if available
(setq load-prefer-newer t)

;; TODO: Why does flycheck register an error here?
(require 'init-package)

(require 'init-ui)

(require 'init-ivy)

(require 'init-search)

(require 'init-undo-tree)

(require 'init-dired)

(require 'init-org)

;; General programming modes

(require 'init-magit)

(require 'init-projectile)

(require 'init-treemacs)

(require 'init-indent)

(require 'init-company)

(require 'init-flycheck)

(require 'init-lsp)

;; Programming modes
(require 'init-markdown)

(require 'init-yaml)

(require 'init-clojure)

(require 'init-ruby)

(require 'init-python)



;; TODO: Move these somewhere else
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(package-selected-packages
   '(rbenv rubocop clojure-mode markdown-mode flycheck toc-org org-gtd company highlight-indent-guides magit undo-tree exec-path-from-shell use-package-ensure-system-package counsel amx ivy ag diminish auto-package-update use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
