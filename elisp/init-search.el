;;; init-search.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init-search.el
;; Description: Initialize Color-RG
;; Author: Mingde (Matthew) Zeng
;; Created: Thu Mar 14 11:01:43 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Mon Jul 22 16:00:25 2019 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d color-rg rg
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes Color-RG
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

;; ColorRGPac
(use-package color-rg
  :load-path "~/.emacs.d/site-elisp/color-rg"
  :if *rg*
  :bind ("C-z s s" . color-rg-search-input))
;; -ColorRGPac

;; IvyPac
(use-package ivy
  :bind (("C-s" . swiper-isearch)
         ("M-s" . swiper-isearch-thing-at-point))
  :diminish
  :init
  (ivy-mode 1)
  :config
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (bind-key "C-r" 'ivy-previous-line-or-history ivy-minibuffer-map)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-on-del-error-function nil)
  (setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (setq ivy-count-format "【%d/%d】")
  (setq ivy-wrap t))
;; -IvyPac

;; SnailsPac
(use-package snails
  :load-path "~/.emacs.d/site-elisp/snails/"
  :bind ("C-M-s" . snails))
;; -SnailsPac


(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ag.el ends here
