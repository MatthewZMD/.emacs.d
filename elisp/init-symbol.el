;;; init-symbol.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init-symbol.el
;; Description: Initialize Pretty Symbols
;; Author: Mingde (Matthew) Zeng
;; Created: Fri Mar 15 08:33:44 2019 (-0400)
;; Version: 1.2.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d
;; Compatibility: emacs-version >= 25.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes pretty symbols
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

;; PreSym
(global-prettify-symbols-mode 1)
  (defun add-pretty-lambda ()
    "make some word or string show as pretty Unicode symbols"
    (setq prettify-symbols-alist
          '(
            ("lambda" . 955)
            ("->" . 8594)
            ("=>" . 8658)
            ("map" . 8614)
            )))
  (add-hook 'prog-mode-hook 'add-pretty-lambda)
;; -PreSym

(provide 'init-symbol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-symbol.el ends here
