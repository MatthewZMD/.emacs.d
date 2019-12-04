;;; init-global-config.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-global-config.el
;; Description: Initialize Global Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 14:01:54 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Mon Nov 25 22:40:07 2019 (-0500)
;;           By: Bilaal Hussain
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file initializes global configurations
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

;; DefBindings
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Use iBuffer instead of Buffer List
(global-set-key (kbd "C-x C-b") #'ibuffer)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
;; -DefBindings
;; UTF8Coding
(if (eq system-type 'windows-nt)
    (progn
      (set-clipboard-coding-system 'utf-16-le)
      (set-selection-coding-system 'utf-16-le))
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when *sys/gui*
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; -UTF8Coding

;; EditExp
;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Make sentences end with a single space
(setq-default sentence-end-double-space nil)

;; Disable Shift mark
(setq shift-select-mode nil)

;; Replace selection on insert
(delete-selection-mode 1)

;; Merge system clipboard with Emacs
(setq-default select-enable-clipboard t)

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)
;; -EditExp

;; History
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)
;; -History

;; SmallConfigs
;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Don't Lock Files
(setq-default create-lockfiles nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defadvice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
;; -SmallConfigs

;; Registers
(if (string= (system-name) "DESKTOP-24FKRDK")
    (progn
      (set-register ?i '(file . "C:/Users/User/AppData/Roaming/.emacs.d/emacs-init/emacs-init.el"))
      (set-register ?t '(file . "C:/Users/User/Documents/Tobyx"))
      (set-register ?d '(file . "C:/Users/User/Drive"))
      (set-register ?4 '(file . "C:/Users/User/Drive/CS341"))
      (set-register ?c '(file . "C:/Users/User/Documents"))
      (set-register ?o '(file . "C:/Users/User/Downloads"))
      (set-register ?j '(file . "C:/Users/User/Drive/books/JojoPart7"))
      (set-register ?m '(file . "C:/MinGW/bin")))
  )

(if (string= (system-name) "ubuntu1804-002")
    (progn
      (set-register ?i '(file .  "~/.emacs.d/emacs-init/emacs-init.el"))
      (set-register ?b '(file .  "~/cs350-os161/root/"))))
;; -Registers

(provide 'init-global-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-global-config.el ends here
