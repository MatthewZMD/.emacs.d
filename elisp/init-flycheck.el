;;; init-flycheck.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init-flycheck.el
;; Description: Initialize Flycheck
;; Author: Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:08:22 2019 (-0400)
;; Version: 1.2.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d flycheck
;; Compatibility: emacs-version >= 25.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes flycheck
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

;; FlyCheckPac
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
;; -FlyCheckPac

(provide 'init-flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
