;;; init-haskell.el ---
;;
;; Filename: init-haskell.el
;; Description:
;; Author: Dagnachew Argaw
;; Maintainer:
;; Copyright (C) 2019 Dagnachew Argaw
;; Created: Sun Jan  5 10:04:02 2020 (-0500)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 11
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
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

;; Code formatting is easier with hindent.

(use-package hindent
  :ensure t)

(use-package ghc
    :ensure t
    :config
    (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package company-ghc
    :ensure t
    :config
    (add-to-list 'company-backends 'company-ghc))

(provide 'init-haskell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-haskell.el ends here
