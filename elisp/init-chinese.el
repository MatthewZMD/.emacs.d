;;; init-chinese.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-chinese.el
;; Description: Initialize Pyim
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Jun 20 00:36:05 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d init
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes pyim and youdao-dictionary suitable for Chinese users.
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

;; PyimPac
(use-package pyim
  :init
  (use-package posframe :defer t)
  :custom
  (default-input-method "pyim")
  (pyim-default-scheme 'quanpin)
  (pyim-page-tooltip 'posframe)
  (pyim-page-length 9)
  :config
  (use-package pyim-basedict
    :after pyim
    :config (pyim-basedict-enable))
  (pyim-isearch-mode 1)
  (diminish pyim-isearch-mode "")
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  :bind
  ("M-j" . pyim-convert-string-at-point)) ; M-j 强制将光标前的拼音字符串转换为中文。
;; -PyimPac

;; YoudaoPac
(use-package youdao-dictionary
  :commands (youdao-dictionary-search
             youdao-dictionary-search-at-point
             youdao-dictionary-search-at-point-posframe)
  :bind ("C-M-y" . youdao-dictionary-search-at-point-posframe))
;; -YoudaoPac

(provide 'init-chinese)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-chinese.el ends here
