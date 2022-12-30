;;; init-games.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-games.el
;; Description: Initialize Games
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:16:53 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d tetris speed-type 2048
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes tetris, speed-type, 2048
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

;; TetrisConfig
(use-package tetris
  :ensure nil
  :commands (tetris)
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-n" . tetris-rotate-down)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-SPC" . tetris-move-bottom))
  :config
  (defadvice tetris-end-game (around zap-scores activate)
    (save-window-excursion ad-do-it)))
;; -TetrisConfig

;; SpeedTypePac
(use-package speed-type
  :commands (speed-type-text))
;; -SpeedTypePac

;; 2048Pac
(use-package 2048-game
  :commands (2048-game))
;; -2048Pac

;; SnowPac
(use-package snow
  :load-path (lambda () (expand-file-name "site-elisp/snow.el" user-emacs-directory))
  :commands (snow))
;; -SnowPac

(provide 'init-games)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-games.el ends here
