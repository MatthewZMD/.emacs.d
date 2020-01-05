;;; idris-keys.el --- Hooks to define Idris keybindings -*- lexical-binding: t -*-

;; Copyright (C) 2014 David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; We don't need to (require 'idris-commands) because the RHS of keybindings
;;; is always just a quoted symbol

;;; Code:

(require 'idris-core)

;;; Keymap-construction hooks

;; These are provided as hooks rather than being hard-coded to facilitate
;; their replacement and cut down on copy-paste.

(defun idris-define-loading-keys (map)
  "Define the keys related to loading files in the keymap MAP."
  (define-key map (kbd "C-c C-l") 'idris-load-file)
  (define-key map (kbd "C-c C-n") 'idris-load-forward-line)
  (define-key map (kbd "C-c C-p") 'idris-load-backward-line))

(defun idris-define-docs-keys (map)
  "Define the keys related to documentation lookup in the keymap MAP."
  (define-key map (kbd "C-c C-t") 'idris-type-at-point)
  (define-key map (kbd "C-c C-d C-d") 'idris-docs-at-point)
  (define-key map (kbd "C-c C-d d") 'idris-docs-at-point)
  (define-key map (kbd "C-c C-d C-a") 'idris-apropos)
  (define-key map (kbd "C-c C-d a") 'idris-apropos)
  (define-key map (kbd "C-c C-d C-t") 'idris-type-search)
  (define-key map (kbd "C-c C-d t") 'idris-type-search))

(defun idris-define-editing-keys (map)
  "Define the keys related to editing Idris code in the keymap MAP."
  (define-key map (kbd "C-c C-c") 'idris-case-dwim)
  (define-key map (kbd "C-c C-m") 'idris-add-missing)
  (define-key map (kbd "C-c C-e") 'idris-make-lemma)
  (define-key map (kbd "C-c C-s") 'idris-add-clause)
  (define-key map (kbd "C-c C-w") 'idris-make-with-block)
  (define-key map (kbd "C-c C-a") 'idris-proof-search)
  (define-key map (kbd "C-c C-r") 'idris-refine)
  (define-key map (kbd "RET") 'idris-newline-and-indent)
  ;; Not using `kbd' due to oddness about backspace and delete
  (define-key map [delete] 'idris-delete-forward-char)
  (define-key map (kbd "C-d") 'idris-delete-forward-char)
  (define-key map (kbd "M-n") 'idris-next-error)
  (define-key map (kbd "M-p") 'idris-previous-error))

(defun idris-define-general-keys (map)
  "Define keys that are generally useful for all Idris modes in the keymap MAP."
  (define-key map (kbd "C-c C-z") 'idris-pop-to-repl)
  (define-key map (kbd "<mouse-3>") 'prop-menu-show-menu)
  (define-key map (kbd "C-c C-SPC") 'prop-menu-by-completing-read))

(defun idris-define-active-term-keys (map)
  "Define keys for manipulating active terms in the keymap MAP."
  (define-key map (kbd "C-c C-m n") 'idris-normalize-term)
  (define-key map (kbd "C-c C-m i") 'idris-show-term-implicits)
  (define-key map (kbd "C-c C-m h") 'idris-hide-term-implicits)
  (define-key map (kbd "C-c C-m c") 'idris-show-core-term))

(defun idris-define-ipkg-keys (map)
  "Define keys for working with the current package in the keymap MAP."
  (define-key map (kbd "C-c C-b b") 'idris-ipkg-build)
  (define-key map (kbd "C-c C-b C-b") 'idris-ipkg-build)
  (define-key map (kbd "C-c C-b c") 'idris-ipkg-clean)
  (define-key map (kbd "C-c C-b C-c") 'idris-ipkg-clean)
  (define-key map (kbd "C-c C-b i") 'idris-ipkg-install)
  (define-key map (kbd "C-c C-b C-i") 'idris-ipkg-install))

(defun idris-define-ipkg-editing-keys (map)
  "Define keys used only for editing packages in the keymap MAP."
  (define-key map (kbd "C-c C-f") 'idris-ipkg-insert-field))

(defun idris-define-ipkg-opening-keys (map)
  "Define keys used to find or open a package file in the keymap MAP."
  (define-key map (kbd "C-c C-b C-p") 'idris-open-package-file)
  (define-key map (kbd "C-c C-b p") 'idris-open-package-file))

(defun idris-define-evil-keys ()
  "Define keys for evil-mode."
  (when (fboundp 'evil-leader/set-key-for-mode)
    (evil-leader/set-key-for-mode 'idris-mode
                                  "r" 'idris-load-file
                                  "t" 'idris-type-at-point
                                  "d" 'idris-add-clause
                                  "l" 'idris-make-lemma
                                  "c" 'idris-case-split
                                  "w" 'idris-make-with-block
                                  "m" 'idris-add-missing
                                  "p" 'idris-proof-search
                                  "h" 'idris-docs-at-point)))

(provide 'idris-keys)
