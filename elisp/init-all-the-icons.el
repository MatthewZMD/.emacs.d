;;; init-all-the-icons.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init-all-the-icons.el
;; Description: Initialize All-The-Icons
;; Author: Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:06:08 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Jul 11 17:44:23 2019 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d all-the-icons
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes all-the-icons, all-the-icons-dired, all-the-icons-ivy
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

;; ATIPac
(when *sys/gui*
  (use-package all-the-icons))
;; -ATIPac

;; ATIDiredPac
(when *sys/gui*
  (use-package all-the-icons-dired
    :after all-the-icons
    :diminish
    :custom-face
    (all-the-icons-dired-dir-face ((t `(:foreground ,(face-background 'default)))))
    :config (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))
;; -ATIDiredPac

(provide 'init-all-the-icons)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-all-the-icons.el ends here
