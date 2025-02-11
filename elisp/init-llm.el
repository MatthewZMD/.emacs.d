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
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("aider.el"))
  :config
  (defun aider--get-args (model)
    "Get aider-args for specified MODEL. Ensure `exec-path-from-shell' is updated"
    (list "--model"
          (cond
           ((string= model "anthropic") "anthropic/claude-3-5-sonnet-20241022")
           ((string= model "deepseek") "r1")
           ((string= model "openai") "gpt-4o")
           ((string= model "gemini") "gemini/gemini-2.0-flash-thinking-exp"))))

  (setq aider-model "deepseek")
  (setq aider-args (aider--get-args aider-model))

  (defun aider-reload ()
    "Interactively reload aider with selected model."
    (interactive)
    (let ((model (completing-read "Select model: " '("deepseek" "anthropic" "openai" "gemini") nil t)))
      (setq aider-model model)
      (setq aider-args (aider--get-args model))
      (when (derived-mode-p 'aider-mode)
        (kill-buffer (current-buffer))
        (aider-transient-menu))))
  :bind
  (("C-z a" . aider-transient-menu)))
;; -AidermacsPac

(provide 'init-llm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-llm.el ends here
