;;; init-cc.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-cc.el
;; Description: Initialize C family languages
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:58:29 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d c c++ go ccls
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initialize ccls modern-cpp-font-lock go-mode
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
  (require 'init-const))

;; CPPFontLockPac
(use-package modern-cpp-font-lock
  :diminish t
  :init (modern-c++-font-lock-global-mode t))
;; -CPPFontLockPac

;; GoPac
(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :custom (gofmt-command "goimports"))
;; -GoPac

;; RustPac
(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :bind (:map rust-mode-map ("C-c C-c" . rust-run))
  :config
  (use-package flycheck-rust
    :after flycheck
    :config
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))
;; -RustPac

(provide 'init-cc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-cc.el ends here
