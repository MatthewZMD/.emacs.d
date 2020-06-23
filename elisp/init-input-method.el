;;; init-input-method.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-input-method.el
;; Description: Initialize Pyim
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Jun 20 00:36:05 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sun Jun 21 00:52:39 2020 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d init
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes pyim
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
  (pyim-isearch-mode 1)
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  :bind
  ("M-j" . pyim-convert-string-at-point)) ; M-j 强制将光标前的拼音字符串转换为中文。
;; -PyimPac

;; PyimBaseDictPac
(use-package pyim-basedict
  :after pyim
  :config (pyim-basedict-enable))
;; -PyimBaseDictPac

;; SmartInputSourcePac
(use-package smart-input-source
  :when *fcitx5*
  :custom
  (smart-input-source-external-ism "fcitx5-remote")
  (smart-input-source-english "1")
  (smart-input-source-other "2")
  (original-cursor-background nil)
  (smart-input-source-do-get
   (lambda()
     (string-trim
      (shell-command-to-string
       smart-input-source-external-ism))))
  (smart-input-source-do-set
   (lambda(source)
     (pcase source
       ("1" (string-trim (shell-command-to-string
                          (concat smart-input-source-external-ism " -c"))))
       ("2" (string-trim (shell-command-to-string
                          (concat smart-input-source-external-ism " -o")))))))
  :config
  (add-hook 'smart-input-source-set-english-hook
            (lambda ()
              (when original-cursor-background
                (set-face-background 'cursor original-cursor-background))))
  (add-hook 'smart-input-source-set-other-hook
            (lambda ()
              (unless original-cursor-background
                (setq original-cursor-background (face-background 'cursor)))
              (set-face-background 'cursor "orange")))
  ;; enable the /respect/ mode
  (smart-input-source-global-respect-mode t)
  ;; enable the /follow context/ and /inline english/ mode for all buffers
  (smart-input-source-global-follow-context-mode t)
  (smart-input-source-global-inline-english-mode t))
;; -SmartInputSourcePac

(provide 'init-input-method)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-input-method.el ends here
