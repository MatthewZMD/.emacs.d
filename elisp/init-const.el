;;; init-const.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init-const.el
;; Description: Initialize Constants
;; Author: Mingde (Matthew) Zeng
;; Created: Mon Mar 18 14:20:54 2019 (-0400)
;; Version: 1.0.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d constants
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes constants
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

;; Consts
(defconst *rg* (executable-find "rg"))

(defconst *tr* (executable-find "tr"))

(defconst *mvn* (executable-find "mvn"))

(defconst *clangd* (or (executable-find "clangd")  ;; usually
                       (executable-find "/usr/local/opt/llvm/bin/clangd")))  ;; macOS

(defconst *gcc* (executable-find "gcc"))
;; -Consts

;; UserInfo
(setq user-full-name "Mingde (Matthew) Zeng")
(setq user-mail-address "matthewzmd@gmail.com")
;; -UserInfo

(provide 'init-const)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
