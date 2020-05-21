;;; init-erc.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-erc.el
;; Description: Initialize ERC
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Tue Jul 30 22:15:50 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Wed Dec  4 01:57:57 2019 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d erc irc
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes erc
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

(eval-when-compile
  (require 'init-global-config)
  (require 'init-func))

;; ERCPac
(use-package erc
  :ensure nil
  :init
  ;; Prerequisite: Configure this to your IRC nickname
  (defcustom my-irc-nick ""
    "The nickname used to login into ERC")
  (use-package erc-hl-nicks :defer t)
  (use-package erc-image :defer t)
  :custom-face
  (erc-notice-face ((t (:foreground "#ababab"))))
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
  (erc-track-exclude-types '("NICK" "PART" "MODE" "324" "329" "332" "333" "353" "477"))
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-server-coding-system '(utf-8 . utf-8))
  (erc-interpret-mirc-color t)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-lurker-threshold-time 43200)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-prompt-for-password nil)
  (erc-prompt-for-nickserv-password nil)
  :config
  (add-to-list 'erc-modules 'notifications)
  (erc-track-mode t)
  (erc-services-mode 1)
  (defun erc-start-or-switch ()
    "Start ERC or switch to ERC buffer if it has started already."
    (interactive)
    (if (get-buffer "irc.freenode.net:6697")
        (erc-track-switch-buffer 1)
      (erc-tls :server "irc.freenode.net" :port 6697 :nick my-irc-nick)))

  (defun erc-count-users ()
    "Displays the number of users and ops connected on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6697")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let ((hash-table (with-current-buffer (erc-server-buffer)
                                  erc-server-users))
                    (users 0)
                    (ops 0))
                (maphash (lambda (k v)
                           (when (member (current-buffer)
                                         (erc-server-user-buffers v))
                             (cl-incf users))
                           (when (erc-channel-user-op-p k)
                             (cl-incf ops)))
                         hash-table)
                (message "%d users (%s ops) are online on %s" users ops channel))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun erc-get-ops ()
    "Displays the names of ops users on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6697")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let (ops)
                (maphash (lambda (nick cdata)
                           (if (and (cdr cdata)
                                    (erc-channel-user-op (cdr cdata)))
                               (setq ops (cons nick ops))))
                         erc-channel-users)
                (if ops
                    (message "The online ops users are: %s"  (mapconcat 'identity ops " "))
                  (message "There are no ops users online on %s" channel)))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title)))
  :bind
  (("M-z i" . erc-start-or-switch)
   ("C-c C-b" . erc-switch-to-buffer)
   (:map erc-mode-map
         ("M-RET" . newline)))
  :hook
  (erc-mode . (lambda () (electric-indent-mode 0)))
  (ercn-notify . erc-notify))
;; -ERCPac

(provide 'init-erc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-erc.el ends here
