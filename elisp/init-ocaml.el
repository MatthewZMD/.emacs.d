;;; init-ocaml.el ---
;;
;; Filename: init-ocaml.el
;; Description:
;; Author: Dagnachew Argaw
;; Maintainer:
;; Copyright (C) 2019 Dagnachew Argaw
;; Created: Sun Jan  5 12:49:00 2020 (-0500)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 25
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
(use-package tuareg
  :defines (tuareg-mode-hook)
  :mode (("\\.ml[4iplg]?\\'" . tuareg-mode)
         ("[./]opam_?\\'" . tuareg-opam-mode)
         ("\\(?:\\`\\|/\\)jbuild\\'" . tuareg-jbuild-mode)
         ("\\.eliomi?\\'" . tuareg-mode))
  :init
  (defun tuareg-reset-indent ()
;;    "Reset comment style for tuareg mode"
    (setq-local comment-style 'indent))
  (add-hook 'tuareg-mode-hook #'tuareg-reset-indent)

  (push ".ml.d" completion-ignored-extensions)
  (push ".mli.d" completion-ignored-extensions))

(use-package ocp-indent
 ;;  :straight nil ; not handled by straight
  :load-path "/home/perrierjouet/.ocaml/default/share/emacs/site-lisp"
  :after tuareg
  :config (setq ocp-indent-untabify t))

(use-package merlin
  :diminish
  :defines (merlin-locate-preference)
  :commands (merlin-mode merlin-locate)
  :bind (:map merlin-mode-map ([remap merlin-locate] . my/merlin-locate))
  :init
  (add-hook 'tuareg-mode-hook #'merlin-mode)

  (defun my/invert-merlin-locate-preference ()
    (cl-case merlin-locate-preference
      ('ml 'mli)
      ('mli 'ml)))

  (defun my/merlin-locate (&optional arg)
    "Locate the identifier under point.

;; With prefix argument, invert `merlin-locate-preference'."
    (interactive "P")
    (let ((merlin-locate-preference
           (if arg (my/invert-merlin-locate-preference) merlin-locate-preference)))
      (merlin-locate)))

  :config
  ;; Disable Merlin's own error checking, we use flycheck
  (setq merlin-error-after-save nil))

(use-package flycheck-ocaml
  :after merlin
  :config
  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(use-package dune
  :load-path "/home/perrierjouet/.emacs.d/lisp/dune")

(use-package ocamlformat
 ;; :straight nil
  :load-path "/home/perrierjouet/.ocaml/default/share/emacs/site-lisp"
  :after tuareg
  :config
  (setq ocamlformat-show-errors 'echo)
  (define-key tuareg-mode-map (kbd "C-<tab>") #'ocamlformat)
  (defun my/ocamlformat-hook ()
    (add-hook 'before-save-hook #'ocamlformat-before-save))
  (add-hook 'tuareg-mode-hook #'my/ocamlformat-hook))

(provide 'init-ocaml)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ocaml.el ends here
