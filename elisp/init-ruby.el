;;; init-ruby.el ---
;;
;; Filename: init-ruby.el
;; Description:
;; Author: Mingde (Matthew) Zeng
;; Maintainer:
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Jul 30 09:51:50 2020 (+0100)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 29
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package rbenv
  :ensure t
  :config (rbenv-use-global))

;; (use-package enh-ruby-mode
;;   :ensure t
;;   :mode "\\.rb\\'"
;;   :interpreter "ruby")

(use-package robe
  :ensure t
  :config (global-robe-mode))
  ;;:hook ((ruby-mode enh-ruby-mode) . robe-mode))

(use-package yari
  :ensure t
  :init
  (add-hook 'ruby-mode-hook
            (lambda ()
              (local-set-key [f1] 'yari))))

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode)

;; Activate Robe and company-robe when we start ruby-mode
(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (when (derived-mode-p 'ruby-mode)
                (add-to-list 'company-backends 'company-robe))))
  (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)
  (add-hook 'ruby-mode-hook 'robe-start))



;; (use-package projectile-rails :ensure t :defer t
;;   :config
;;   (add-hook 'projectile-mode-hook 'projectile-rails-on))

;; (use-package rinari :ensure t :defer t)
;; (use-package bundler :ensure t :defer t)
;; (use-package rspec-mode
;;   :ensure t :defer t
;;   :commands rspec-mode
;;   :config
;;   (progn (rspec-install-snippets)
;;          (inf-ruby-switch-setup)
;;          (setq compilation-scroll-output t)))

(provide 'init-ruby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ruby.el ends here
