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

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Disable Unnecessary Interface
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
;; -Disable Unnecessary Interface

;; Avoid Garbage Collect During Startup
(eval-and-compile
  (defun revert-gc ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1))

  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (add-hook 'emacs-startup-hook 'revert-gc))
;; -Avoid Garbage Collect During Startup

;; Unset FNHA
(eval-and-compile
  (defun reset-file-name-handler-alist ()
    (setq file-name-handler-alist orig-file-name-handler-alist))

  (defvar orig-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (add-hook 'emacs-startup-hook 'reset-file-name-handler-alist))
;; -Unset FNHA

;; Load LP
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-elisp" user-emacs-directory) load-path)
  (push (expand-file-name "elisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-elisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
;; -Load LP

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

(require 'init-global-config)

(require 'init-dired)

(require 'init-ag)

(require 'init-avy)

(require 'init-ivy)

(org-babel-load-file (expand-file-name "~/.emacs.d/inits.org"))


(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
