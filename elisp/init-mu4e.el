;;; init-mu4e.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-mu4e.el
;; Description: Initialize mu4e
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Mon Dec  2 15:17:14 2019 (-0500)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d mu mu4e
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes mu4e for Email clients in Emacs
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

(use-package mu4e-alert
    :defer t
    :config
    (when (executable-find "notify-send")
      (mu4e-alert-set-default-style 'libnotify))
    :hook
    ((after-init . mu4e-alert-enable-notifications)
     (after-init . mu4e-alert-enable-mode-line-display)))

(use-package mu4e-overview :defer t)

;; Mu4ePac
(use-package mu4e
  :straight (mu
             :type git
             :host github
             :repo "djcb/mu"
             :files ("build/mu4e/*.el"))
  :if (executable-find "mu")
  :commands (mu4e make-mu4e-context)
  :bind
  (("M-z m" . mu4e)
   ("M-m m" . mu4e)
   (:map mu4e-view-mode-map
         ("e" . mu4e-view-save-attachment)))
  :custom
  (mu4e-maildir (expand-file-name "~/Maildir"))
  (mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a")
  (mu4e-view-prefer-html t)
  (mu4e-update-interval 180)
  (mu4e-headers-auto-update t)
  (mu4e-compose-format-flowed t)
  (mu4e-view-show-images t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-attachment-dir "~/Downloads")
  (message-kill-buffer-on-exit t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-view-show-addresses t)
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-results-limit 1000)
  (mu4e-view-use-gnus t)
  :hook
  ((mu4e-view-mode . visual-line-mode)
   (mu4e-compose-mode . (lambda ()
                          (visual-line-mode)
                          (use-hard-newlines -1)
                          (flyspell-mode)))
   (mu4e-view-mode . (lambda() ;; try to emulate some of the eww key-bindings
                       (local-set-key (kbd "<tab>") 'shr-next-link)
                       (local-set-key (kbd "<backtab>") 'shr-previous-link)))
   (mu4e-headers-mode . (lambda ()
                          (interactive)
                          (setq mu4e-headers-fields
                                `((:human-date . 25) ;; alternatively, use :date
                                  (:flags . 6)
                                  (:from . 22)
                                  (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                                  (:size . 7))))))
  :config
  (use-package mu4e-thread-folding
    :straight (mu4e-thread-folding :type git :host github :repo "rougier/mu4e-thread-folding")
    :after mu4e
    :bind
    ((:map mu4e-headers-mode-map
           ("TAB" . mu4e-headers-toggle-at-point)
           ("C-<tab>" . mu4e-headers-toggle-fold-all))
     (:map mu4e-search-minor-mode-map
           ("S" . mu4e-kill-update-mail)))
    :custom
    (mu4e-thread-folding-default-view `folded)
    (mu4e-headers-fields '((:empty         .    2)
                           (:human-date    .   12)
                           (:flags         .    6)
                           (:mailing-list  .   10)
                           (:from          .   22)
                           (:subject       .   nil)))
    :config
    (add-to-list 'mu4e-header-info-custom
                 '(:empty . (:name "Empty"
                                   :shortname ""
                                   :function (lambda (msg) "  ")))))
  (require 'mu4e-icalendar)
  (setq mail-user-agent (mu4e-user-agent))
  (mu4e-icalendar-setup)
  (gnus-icalendar-org-setup)
  (defalias 'mu4e-add-attachment 'mail-add-attachment
    "I prefer the add-attachment function to begin wih mu4e so I can find it easily.")
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t))
;; -Mu4ePac

(provide 'init-mu4e)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mu4e.el ends here
