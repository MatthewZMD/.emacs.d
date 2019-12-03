;;; init-mu4e.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-mu4e.el
;; Description: Initialize mu4e
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Mon Dec  2 15:17:14 2019 (-0500)
;; Version: 2.0.0
;; Package-Requires: (mu4e)
;; Last-Updated:
;;           By:
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

;; Mu4ePac
(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :init
  (use-package mu4e-alert
    :defer t
    :config
    (when (executable-find "notify-send")
      (mu4e-alert-set-default-style 'libnotify))
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
  (use-package mu4e-overview :defer t)
  :config
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  ;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
  (add-hook 'mu4e-headers-mode-hook
            (defun my/mu4e-change-headers ()
	          (interactive)
	          (setq mu4e-headers-fields
	                `((:human-date . 25) ;; alternatively, use :date
		              (:flags . 6)
		              (:from . 22)
		              (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
		              (:size . 7)))))
  ;; spell check
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (visual-line-mode)
              (org-mu4e-compose-org-mode)
              (use-hard-newlines -1)
              (flyspell-mode)))
  (add-hook 'mu4e-view-mode-hook
            (lambda() ;; try to emulate some of the eww key-bindings
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))
  :bind
  ("M-z m" . mu4e)
  :custom
  (mu4e-maildir (expand-file-name "~/Maildir"))
  (mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a")
  (mu4e-view-prefer-html t)
  (mu4e-update-interval 180)
  (mu4e-headers-auto-update t)
  (mu4e-compose-signature-auto-include nil)
  (mu4e-compose-format-flowed t)
  (mu4e-view-show-images t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-change-filenames-when-moving t) ; work better for mbsync
  (mu4e-attachment-dir "~/Downloads")
  (message-kill-buffer-on-exit t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-view-show-addresses t)
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)
  (mu4e-drafts-folder "/matthewzmd-gmail/Drafts")
  (mu4e-refile-folder "/matthewzmd-gmail/Archive")
  (mu4e-sent-folder "/matthewzmd-gmail/Sent Mail")
  (mu4e-trash-folder "/matthewzmd-gmail/Trash")
  (mu4e-maildir-shortcuts
   '(("/matthewzmd-gmail/INBOX" . ?i)
     ("/matthewzmd-gmail/All Mail" . ?a)
     ("/matthewzmd-gmail/Trash" . ?d)
     ("/matthewzmd-gmail/Drafts" . ?D)
     ("/matthewzmd-gmail/Important" . ?i)
     ("/matthewzmd-gmail/Sent Mail" . ?s)
     ("/matthewzmd-gmail/Starred" . ?S))))
;; -Mu4ePac

(provide 'init-mu4e)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mu4e.el ends here
