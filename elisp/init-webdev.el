;;; init-webdev.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init-webdev.el
;; Description: Initialize Web, Emmet, JS2, TypeScript, Tide
;; Author: Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:03:43 2019 (-0400)
;; Version: 1.2.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d web-mode emmet js2-mode typescript-mode tide
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes web-mode emmet js2-mode typescript-mode tide
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

(require 'init-package)

;; WebModePac
(def-package web-mode
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))
;; -WebModePac

;; EmmetPac
(def-package emmet-mode
  :hook web-mode
  :config
  (add-hook 'css-mode-hooktype  'emmet-mode)) ;; enable Emmet's css abbreviation
;; -EmmetPac

;; Js2Pac
(def-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")
;; -Js2Pac

;; TypeScriptPac
(def-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode))
;; -TypeScriptPac

(provide 'init-webdev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-webdev.el ends here
