;;; init-search.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-search.el
;; Description: Initialize Packages for Searching
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 11:01:43 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Tue Jun 23 19:09:04 2020 (+0100)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d color-rg rg
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes ivy swiper counsel color-rg snails
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

(eval-when-compile
  (require 'init-global-config)
  (require 'init-const))

;; IvyPac
(use-package ivy
  :diminish
  :init
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind
  (("C-s" . swiper-isearch)
   ("C-z s" . counsel-rg)
   ("C-z b" . counsel-buffer-or-recentf)
   ("C-z C-b" . counsel-ibuffer)
   (:map ivy-minibuffer-map
         ("C-r" . ivy-previous-line-or-history)
         ("M-RET" . ivy-immediate-done))
   (:map counsel-find-file-map
         ("C-~" . counsel-goto-local-home)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (defun counsel-goto-local-home ()
      "Go to the $HOME of the local machine."
      (interactive)
    (ivy--cd "~/")))
;; -IvyPac

;; ColorRGPac
(use-package color-rg
  :load-path (lambda () (expand-file-name "site-elisp/color-rg" user-emacs-directory))
  :if *rg*
  :bind ("C-M-s" . color-rg-search-input))
;; -ColorRGPac

;; Ag
(use-package ag
  :ensure t)
;; -Ag

;; FFIPPac
(use-package find-file-in-project
  :if *find*
  :bind ("C-z o" . ffip))
;; -FFIPPac

;; SnailsPac
;; (use-package snails
;;   :load-path (lambda () (expand-file-name "site-elisp/snails/" user-emacs-directory))
;;   :if *sys/gui*
;;   :custom-face
;;   (snails-content-buffer-face ((t (:background "#111" :height 110))))
;;   (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
;;   (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
;;   :init
;;   (use-package exec-path-from-shell :if (featurep 'cocoa) :defer t)
;;   :config
;;   ;; Functions for specific backends
;;   (defun snails-current-project ()
;;     (interactive)
;;     (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd)))
;;   (defun snails-active-recent-buffers ()
;;     (interactive)
;;     (snails '(snails-backend-buffer snails-backend-recentf)))
;;   (defun snails-everywhere ()
;;     (interactive)
;;     (snails '(snails-backend-everything snails-backend-mdfind))))
;; ;; -SnailsPac

(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
