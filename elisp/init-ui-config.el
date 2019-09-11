;;; init-ui-config.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ui-config.el
;; Description: Initialize UI Configuration
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 16:12:56 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Tue Sep 10 14:08:52 2019 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d ui
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes UI configurations
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

;; TitleBar
(setq-default frame-title-format '("M-EMACS - " user-login-name "@" system-name " - %b"))
;; -TitleBar

;; YorN
(fset 'yes-or-no-p 'y-or-n-p)
;; -YorN

;; StartupScreen
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "Present Day, Present Time...")
;; -StartupScreen

;; DisLineNum
;; Hook line numbers to only when files are opened
(add-hook 'find-file-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Display column numbers in modeline
(column-number-mode 1)
;; -DisLineNum

;; DisTimeBat
(display-time-mode 1)
(display-battery-mode 1)
;; -DisTimeBat

(provide 'init-ui-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui-config.el ends here
