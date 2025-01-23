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

;; AiderPac
(use-package aider
  :if (executable-find "aider")
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-model "deepseek")
  (setq aider-args
        (cond
         ((string= aider-model "anthropic")
          '("--model" "anthropic/claude-3-5-sonnet-20241022"))
         ((string= aider-model "deepseek")
          '("--model" "r1"))
         ((string= aider-model "openai")
          '("--model" "gpt-4o"))))
  (defun aider-reload ()
    "Interactively reload aider with selected model."
    (interactive)
    (let ((model (completing-read "Select model: " '("deepseek" "anthropic" "openai") nil t)))
      (setq aider-model model)
      (setq aider-args
            (cond
             ((string= model "anthropic") '("--model" "anthropic/claude-3-5-sonnet-20241022"))
             ((string= model "deepseek") '("--model" "r1"))
             ((string= model "openai") '("--model" "gpt-4o"))))
      (when (derived-mode-p 'aider-mode)
        (kill-buffer (current-buffer))
        (aider-transient-menu))))
  :bind
  (("C-z a" . aider-transient-menu)))
;; -AiderPac

(provide 'init-llm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-llm.el ends here
