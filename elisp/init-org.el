(use-package org
  :demand t
  :hook (org-mode . visual-line-mode)
  :custom
  (org-log-done 'time)
  (org-default-priority 70)
  (org-agenda-window-setup 'only-window)
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  :config
  (unless (version< org-version "9.2")
    (require 'org-tempo)))

(use-package org-gtd
  :after org
  :demand t
  :custom
  (org-gtd-directory "~/org/gtd/")
  (org-edna-use-inheritance t)
  :config
  (org-edna-mode)
  :commands (org-gtd-capture org-gtd-engage org-gtd-process-inbox)
  :init
  (bind-key "C-c d c" 'org-gtd-capture)
  (bind-key "C-c d e" 'org-gtd-engage)
  (bind-key "C-c d p" 'org-gtd-process-inbox)
  :bind
  (("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-process-map
   ("C-c c" . org-gtd-choose)))

(provide 'init-org)
