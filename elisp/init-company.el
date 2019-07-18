;;; init-company.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init-company.el
;; Description: Initialize Company
;; Author: Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:02:00 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Jul 18 19:35:19 2019 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d company company-tabnine
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes company
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

;; ComPac
(use-package company
  :diminish company-mode
  :hook (prog-mode . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations 't)
  (setq company-begin-commands '(self-insert-command))
  (setq company-require-match 'never)
  ;; Don't use company in the following modes
  (setq company-global-modes '(not shell-mode))
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common))
;; -ComPac

;; CompanyLSPPac
(use-package company-lsp
  :after lsp company
  :config
  (setq company-lsp-cache-candidates 'auto))
;; -CompanyLSPPac

;; CompanyTabNinePac
(use-package company-tabnine
  :after company company-lsp
  :config
  ;; Utilize company-tabnine with lsp-mode
  (with-eval-after-load 'lsp-mode
    (require 'company-lsp)
  (setq company-backends
        (use-package-list-insert #'company-lsp company-backends
                                 'company-tabnine t)))
  (with-eval-after-load 'company
    (push #'company-tabnine company-backends)))
;; -CompanyTabNinePac

(provide 'init-company)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
