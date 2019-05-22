;;; init.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init.el
;; Description: Initialize M-EMACS
;; Author: Mingde (Matthew) Zeng
;; Created: Thu Mar 14 10:15:28 2019 (-0400)
;; Version: 1.2.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d init
;; Compatibility: emacs-version >= 25.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for M-EMACS
;; Loads the README org file which contains the *REAL* meat
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

;; CheckVer
(when (version< emacs-version "25.1")
  (error "M-EMACS requires Emacs 25.1 and above!"))
;; -CheckVer

;; DisableUnnecessaryInterface
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
;; -DisableUnnecessaryInterface

;; AvoidStartupGarbageCollect
(eval-and-compile
  (defun revert-gc ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1))

  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (add-hook 'emacs-startup-hook 'revert-gc))
;; -AvoidStartupGarbageCollect

;; UnsetFNHA
(eval-and-compile
  (defun reset-file-name-handler-alist ()
    (setq file-name-handler-alist orig-file-name-handler-alist))

  (defvar orig-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (add-hook 'emacs-startup-hook 'reset-file-name-handler-alist))
;; -UnsetFNHA

;; LoadPath
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (add-to-list 'load-path base)
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (add-to-list 'load-path name))))))

(update-to-load-path "~/.emacs.d/elisp")
;; -LoadPath

;; Constants

(require 'init-const)

;; Packages

;; Package Management
(require 'init-package)

;; Global Functionalities
(require 'init-global-config)

(require 'init-dired)

(require 'init-search)

(require 'init-avy)

(require 'init-ivy)

(require 'init-shell)

(require 'init-winner)

(require 'init-which-key)

(require 'init-popup-kill-ring)

(require 'init-undo-tree)

(require 'init-discover-my-major)

(require 'init-ace-window)

;; User Interface Enhancements
(require 'init-ui-config)

(require 'init-all-the-icons)

(require 'init-theme)

(require 'init-dashboard)

(require 'init-fonts)

(require 'init-diminish)

(require 'init-scroll)

(require 'init-symbol)

;; General Programming
(require 'init-magit)

(require 'init-projectile)

(require 'init-treemacs)

(require 'init-company)

(require 'init-yasnippet)

(require 'init-flycheck)

(require 'init-dumb-jump)

(require 'init-linenum)

(require 'init-parens)

(require 'init-indent)

(require 'init-format)

(require 'init-comment)

(require 'init-iedit)

(require 'init-header)

;; (require 'init-ein)

;; Programming

(require 'init-lsp)

;; (require 'init-arduino)

;; Web Development
(require 'init-webdev)

;; Miscellaneous
(require 'init-org)

(require 'init-eww)

(require 'init-leetcode)

(require 'init-games)

(require 'init-zone)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
