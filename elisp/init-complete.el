;;; init-complete.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-complete.el
;; Description: Initialize Company
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:02:00 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d company company-tabnine
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes completion frameworks
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

;; LSPPac
(use-package lsp-bridge
  :load-path (lambda () (expand-file-name "site-elisp/lsp-bridge" user-emacs-directory))
  :defer 1
  :commands (global-lsp-bridge-mode lsp-bridge-mode)
  :custom
  (acm-enable-codeium t)
  (acm-enable-tabnine nil)
  (acm-enable-yas nil)
  (acm-enable-quick-access t)
  (lsp-bridge-enable-hover-diagnostic t)
  :bind (("M-." . lsp-bridge-find-def)
         ("M-," . lsp-bridge-find-def-return)
         ("M-i" . lsp-bridge-popup-documentation)
         ("C-M-." . lsp-bridge-peek)
         :map lsp-bridge-ref-mode-map
         ("n" . lsp-bridge-ref-jump-next-keyword)
         ("p" . lsp-bridge-ref-jump-prev-keyword)
         ("M-n" . lsp-bridge-ref-jump-next-file)
         ("M-p" . lsp-bridge-ref-jump-prev-file)
         ("C-x C-q" . lsp-bridge-ref-switch-to-edit-mode)
         :map lsp-bridge-ref-mode-edit-map
         ("C-x C-q" . lsp-bridge-ref-apply-changed)
         ("C-x C-s" . lsp-bridge-ref-apply-changed)
         ("C-c C-k" . lsp-bridge-ref-quit)
         ("M-n" . lsp-bridge-ref-jump-next-file)
         ("M-p" . lsp-bridge-ref-jump-prev-file)
         :map acm-mode-map
         ([remap next-line] . nil)
         ([remap previous-line] . nil))
  :config
  (global-lsp-bridge-mode))
;; -LSPPac

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(provide 'init-complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-complete.el ends here
