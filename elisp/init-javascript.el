;;; init-javascript.el ---
;;
;; Filename: init-javascript.el
;; Description:
;; Author: Mingde (Matthew) Zeng
;; Maintainer:
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Jul 10 10:49:42 2020 (+0100)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 27
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-javascript.el ends here

(setq js-indent-level 2)

(use-package flycheck-flow)
(use-package company-flow)

(use-package flow-minor-mode
  :config
  (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-flow 'flow-mode)
    (flycheck-add-mode 'javascript-eslint 'flow-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-flow))
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files ".flowconfig"))
  (with-eval-after-load 'ohai-js-web-mode
    (add-hook 'web-mode-hook 'flow-minor-enable-automatically))
  (add-to-list 'auto-mode-alist '("\\.flowconfig\\'" . conf-mode)))

(provide 'init-javascript)
