;;; custom-set-variables.el --- 
;;
;; Filename: custom-set-variables.el
;; Description:
;; Author: Dagnachew Argaw
;; Maintainer:
;; Copyright (C) 2019 Dagnachew Argaw
;; Created: Sat Jan  4 13:51:22 2020 (-0500)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 3
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
;;; custom-set-variables.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-package-update-delete-old-versions t)
 '(auto-package-update-hide-results t)
 '(auto-package-update-interval 7)
 '(auto-package-update-prompt-before-update t)
 '(auto-revert-interval 3)
 '(auto-revert-use-notify nil)
 '(auto-revert-verbose nil)
 '(auto-save-default nil)
 '(avy-style (quote pre) t)
 '(avy-timeout-seconds 0.3 t)
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(global-auto-revert-non-file-buffers t)
 '(ivy-count-format "【%d/%d】")
 '(ivy-height 10)
 '(ivy-magic-slash-non-match-action (quote ivy-magic-slash-non-match-create))
 '(ivy-on-del-error-function nil)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(load-prefer-newer t)
 '(make-backup-files nil)
 '(multi-term-program "/usr/bin/bash" t)
 '(package-selected-packages
   (quote
    (lsp-haskell haskell-mode yasnippet-snippets which-key web-mode use-package undo-tree typescript-mode treemacs-projectile treemacs-magit toc-org term-keys super-save sudo-edit speed-type smartparens shell-here quickrun pyim posframe popup-kill-ring plantuml-mode pdf-tools ox-gfm org-edit-latex mu4e-overview mu4e-alert modern-cpp-font-lock lsp-ui lsp-python-ms lsp-java json-mode js2-mode iedit htmlize highlight-indent-guides graphql go-mode format-all flycheck exec-path-from-shell evil-nerd-commenter ess erc-image erc-hl-nicks emmet-mode dumb-jump doom-themes doom-modeline disk-usage discover-my-major diminish dashboard dap-mode crux counsel company-tabnine company-lsp company-box ccls auto-package-update amx all-the-icons-dired aio 2048-game)))
 '(recentf-auto-cleanup "05:00am")
 '(recentf-exclude
   (quote
    ((expand-file-name package-user-dir)
     ".cache" ".cask" ".elfeed" "bookmarks" "cache" "ido.*" "persp-confs" "recentf" "undo-tree-hist" "url" "COMMIT_EDITMSG\\'")))
 '(recentf-max-saved-items 200)
 '(super-save-auto-save-when-idle t)
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(which-key-prefix-prefix "+")
 '(which-key-separator " ")
 '(winner-boring-buffers
   (quote
    ("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*" "*esh command on file*"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-dir-face ((t (\` (:foreground (\, (face-background (quote default))))))))
 '(avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold))))
 '(css-selector ((t (:inherit default :foreground "#66CCFF"))))
 '(cursor ((t (:background "BlanchedAlmond"))))
 '(dashboard-banner-logo-title ((t (:family "Love LetterTW" :height 123))))
 '(erc-notice-face ((t (:foreground "#ababab"))))
 '(font-lock-comment-face ((t (:foreground "#828282"))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
 '(snails-content-buffer-face ((t (:background "#111" :height 110))))
 '(snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
 '(snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110)))))
