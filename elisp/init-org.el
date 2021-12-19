;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Sep 17 10:24:54 2020 (+0100)
;;           By: Mingde (Matthew) Zeng
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
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  (:map org-mode-map ("C-c C-p" . org-export-as-pdf-and-open))
  :hook
  (org-mode . visual-line-mode)
  :custom
  (org-agenda-files '("~/org/inbox.org"
                      "~/org/projects.org"
                      "~/org/tickler.org"))
  (org-capture-templates '(("t" "Todo [inbox]" entry
                            (file+headline "~/org/inbox.org" "Tasks")
                            "* TODO %i%?")
                           ("T" "Tickler" entry
                            (file+headline "~/org/tickler.org" "Tickler")
                            "* %i%? \n %U")))
 (org-refile-targets '(("~/org/projects.org" :maxlevel . 2)
                       ("~/org/someday.org" :level . 1)
                       ("~/org/tickler.org" :maxlevel . 2)
                       ("~/org/coming-soon.org" :maxlevel . 2)))
 (org-agenda-custom-commands
  '(("d" "All tasks" tags "TODO={.+}")))
  (org-log-done 'time)
  (org-default-priority 70)
  (org-agenda-window-setup 'only-window)
  (org-agenda-todo-ignore-scheduled t)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-startup-indented t)
  (org-todo-keywords
   '((sequence "TODO" "WAITING" "|" "DONE")))
  :config
  (unless (version< org-version "9.2")
    (require 'org-tempo))
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))

  (defun org-export-turn-on-syntax-highlight ()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
    (interactive)
    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process
          '("pdflatex -shelnl-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

  (defun org-export-as-pdf-and-open ()
    "Run `org-latex-export-to-pdf', delete the tex file and open pdf in a new buffer."
    (interactive)
    (save-buffer)
    (let* ((pdf-path (org-latex-export-to-pdf))
           (pdf-name (file-name-nondirectory pdf-path)))
      (if (try-completion pdf-name (mapcar #'buffer-name (buffer-list)))
          (progn
            (kill-matching-buffers (concat "^" pdf-name) t t)
            (find-file-other-window pdf-name))
        (find-file-other-window pdf-name))
      (delete-file (concat (substring pdf-path 0 (string-match "[^\.]*\/?$" pdf-path)) "tex")))))
;; -OrgPac



;;(use-package org-gtd
;;  :after org
;;   :demand t
;;   :bind
;;   ("C-c d c" . org-gtd-capture)
;;   ("C-c d a" . org-agenda-list)
;;   ("C-c d p" . org-gtd-process-inbox)
;;   ("C-c d n" . org-gtd-show-all-next)
;;   ("C-c d s" . org-gtd-show-stuck-projects)
;;   :config
;;   (setq org-gtd-directory "~/org/gtd/")
;;   (setq org-gtd-process-item-hooks '(org-set-tags-command))
;;   (setq org-edna-use-inheritance t)
;;   (org-edna-mode 1))

;; (use-package org-agenda
;;    :ensure nil
;;    :after org-gtd
;;    :config
;;    (setq org-agenda-files `(,org-gtd-directory))
;;    (setq org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT"))))))

;; (use-package org-capture
;;   :ensure nil
;;   :after org-gtd
;;   :config
;;   (setq org-capture-templates
;;         `(("i" "Inbox"
;;            entry (file "~/org/gtd/inbox.org")   ;; TODO: Debug why this doesn't work,(org-gtd-inbox-path))
;;            "* %?\n%U\n\n  %i"
;;            :kill-buffer t)
;;           ("l" "Todo with link"
;;            entry (file "~/org/gtd/inbox.org")  ;; TODO debug,(org-gtd-inbox-path))
;;            "* %?\n%U\n\n  %i\n  %a"
;;            :kill-buffer t))))

;; this is used in a very specific minor mode, so you can have a pretty common keybinding.
;;(bind-key "C-c c" 'org-gtd-clarify-finalize)))

;; ;; this allows you use `(,org-gtd-directory) for your agenda files
;; (use-package org-agenda
;;   :ensure nil
;;   :after org-gtd)

;; ;; this allows you to use (org-gtd-inbox-path) for your capture destinations
;; (use-package org-capture
;;   :ensure nil
;;   :after org-gtd)

;; (use-package org-agenda-property
;;   :custom
;;   )


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
  (org-plantuml-jar-path (expand-file-name "~/tools/plantuml/plantuml.jar"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (plantuml . t))))
;; -PlantUMLPac


(use-package ox-reveal
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "screencapture" nil nil nil "-i" filename)
  (insert (concat "[[./" filename "]]"))
  (org-display-inline-images))

(load "~/.emacs.d/site-elisp/org-export-as-s5.el")

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
