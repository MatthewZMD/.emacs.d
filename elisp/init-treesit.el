;;; init-treesit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-treesit.el
;; Description: Initialize Parenthesis
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:17:13 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d parenthesis smartparens delete-block
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes tree-sitter
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
  (require 'init-global-config))

;; TreesitAutoPac
(use-package treesit-auto
  :if (version<= "29" emacs-version)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (when (version<= "31" emacs-version)
    (warn "The treesit-auto package maybe obsolete!"))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (defun treesit-show-parser-used-at-point ()
    "Shows treesit parser used at point."
    (interactive)
    (if (and (fboundp 'treesit-available-p)
             (treesit-available-p))
        (message (format "%s" (treesit-language-at (point))))
      (message "treesit is not available"))))
;; -TreesitAutoPac

(provide 'init-treesit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treesit.el ends here
