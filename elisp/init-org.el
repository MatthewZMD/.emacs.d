;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d org toc-org htmlize ox-gfm
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes org toc-org htmlize ox-gfm
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

;; OrgPac
(use-package org
  :ensure nil
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         (:map org-mode-map (("C-c C-p" . eaf-org-export-to-pdf-and-open)
                             ("C-c ;" . nil))))
  :custom
  (org-log-done 'time)
  (calendar-latitude 43.65107) ;; Prerequisite: set it to your location, currently default: Toronto, Canada
  (calendar-longitude -79.347015) ;; Usable for M-x `sunrise-sunset' or in `org-agenda'
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-latex-listings-options '(("breaklines" "true")))
  (org-latex-listings t)
  (org-deadline-warning-days 7)
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE" "CANCELED")))
  (org-agenda-window-setup 'other-window)
  (org-latex-pdf-process
   '("pdflatex -shelnl-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :custom-face
  (org-agenda-current-time ((t (:foreground "spring green"))))
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (unless (version< org-version "9.2")
    (require 'org-tempo))
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (C . t)
     (python . t)
     (plantuml . t)))
  (defun org-export-toggle-syntax-highlight ()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
    (interactive)
    (setq-local org-latex-listings 'minted)
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

  (defun org-table-insert-vertical-hline ()
    "Insert a #+attr_latex to the current buffer, default the align to |c|c|c|, adjust if necessary."
    (interactive)
    (insert "#+attr_latex: :align |c|c|c|")))
;; -OrgPac

;; OrgRoamPac
(use-package org-roam
  :after org
  :custom
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-completion-everywhere t)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n h" . org-id-get-create))
  :config
  (when (file-directory-p "~/org/roam/")
    (setq org-roam-directory (file-truename "~/org/roam")))
  (org-roam-db-autosync-mode))
;; -OrgRoamPac

;; TocOrgPac
(use-package toc-org
  :hook (org-mode . toc-org-mode))
;; -TocOrgPac

;; HTMLIZEPac
(use-package htmlize :defer t)
;; -HTMLIZEPac

;; OXGFMPac
(use-package ox-gfm :defer t)
;; -OXGFMPac

;; PlantUMLPac
(use-package plantuml-mode
  :defer t
  :custom
  (org-plantuml-jar-path (expand-file-name "~/tools/plantuml/plantuml.jar")))
;; -PlantUMLPac

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
