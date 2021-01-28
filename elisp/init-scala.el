;;; init-scala.el ---
;;
;; Filename: init-scala.el
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


(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  ;;(substitute-key-definition
  ;;'minibuffer-complete-word
  ;; 'self-insert-command
   ;;minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))


;; :after lsp-mode

;; (use-package lsp-mode
;;   ;; Optional - enable lsp-mode automatically in scala files
;;   :hook  (scala-mode . lsp)
;;          (lsp-mode . lsp-lens-mode)
;;   :config (setq lsp-prefer-flymake nil))

(provide 'init-scala)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-scala.el ends here
