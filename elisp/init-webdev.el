;;; init-webdev.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-webdev.el
;; Description: Initialize Web, Emmet, JS2, TypeScript, Tide
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:03:43 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d web-mode js2-mode typescript-mode emmet instant-rename-tag json-mode
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes web-mode js2-mode typescript-mode emmet instant-rename-tag instant-rename-tag
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

;; WebModePac
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'" "\\.[jt]sx?\\'"))
;; -WebModePac

;; Js2Pac
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")
;; -Js2Pac

;; TypeScriptPac
(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode))
;; -TypeScriptPac

;; EmmetPac
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))
;; -EmmetPac

;; InstantRenameTagPac
(use-package instant-rename-tag
  :load-path (lambda () (expand-file-name "site-elisp/instant-rename-tag" user-emacs-directory))
  :bind ("C-z <" . instant-rename-tag))
;; -InstantRenameTagPac

;; JsonPac
(use-package json-mode
  :mode "\\.json\\'")
;; -JsonPac


;;
(use-package add-node-modules-path
  :hook (web-mode . add-node-modules-path))

;; PrettierPac
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))
;; -PrettierPac


(provide 'init-webdev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-webdev.el ends here
