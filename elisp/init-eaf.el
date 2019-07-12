;;; init-eaf.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init-eaf.el
;; Description: Initialize Emacs Application Framework
;; Author: Mingde (Matthew) Zeng
;; Created: Tue Jun  4 00:26:09 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Jul 11 22:54:25 2019 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d pdf-tools
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes Emacs Application Framework
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

;; EAFPac
(when (and *sys/linux* *python3*)
  (use-package eaf
    :load-path "~/.emacs.d/site-elisp/emacs-application-framework"
    :config
    (setq eaf-find-alternate-file-in-dired t)
    ;; I already bind "RET", "<mouse-2>", "^" to `dired-find-alternate-file' in `init-dired.el'.
    ;; Comment this line out of you don't want to use EAF.
    (global-set-key [remap dired-find-alternate-file] #'eaf-file-open-in-dired)

    (defun eaf-open-google ()
      "Open Google using EAF."
      (interactive)
      (eaf-open-browser "https://www.google.com"))))
;; -EAFPac

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
