;;; init-epaint.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-epaint.el
;; Description: Initialize epaint
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Mon Sep 16 15:47:34 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d epaint
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes epaint
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

;; EPaintPac
(use-package epaint
  :if (display-graphic-p)
  :load-path (lambda () (expand-file-name "site-elisp/epaint" user-emacs-directory))
  :commands (epaint)
  :init
  (with-eval-after-load (quote epaint-context)
    (unless (boundp (quote cl-struct-epaint-drawable))
      (defvar cl-struct-epaint-drawable (quote epaint-drawable)))
    (unless (boundp (quote cl-struct-epaint-gc))
      (defvar cl-struct-epaint-gc (quote epaint-gc)))))
;; -EPaintPac

(provide 'init-epaint)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-epaint.el ends here
