;;; init-llm.el ---
;;
;; Filename: init-llm.el
;; Description:
;; Author: Mingde (Matthew) Zeng
;; Maintainer:
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Jan  9 13:07:47 2025 (-0500)
;; Version:
;; Package-Requires: ()
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
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

;; AidermacsPac
(use-package aidermacs
  :if (executable-find "aider")
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :custom
  (aidermacs-backend 'comint)
  (aidermacs-auto-commits nil)
  (aidermacs-default-model "openrouter/google/gemini-2.5-pro-exp-03-25:free")
  (aidermacs-weak-model "openrouter/deepseek/deepseek-chat")
  :config
  (add-to-list 'display-buffer-alist
               `("\\*aidermacs.*\\*"
                 (display-buffer-pop-up-window)))
  :bind
  (("C-z a" . aidermacs-transient-menu)))
;; -AidermacsPac

;; EmigoPac
(use-package emigo
  :straight (:host github :repo "MatthewZMD/emigo" :files ("*"))
  :config
  (setq emigo-model "openrouter/deepseek/deepseek-chat-v3-0324")
  (setq emigo-base-url "https://openrouter.ai/api/v1")
  (setq emigo-api-key (getenv "OPENROUTER_API_KEY"))
  (emigo-start-process))
;; -EmigoPac

(use-package gptel)

(provide 'init-llm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-llm.el ends here
