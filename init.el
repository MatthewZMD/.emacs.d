;;; package --- Summary
;;; This is MT`s personal init.el file for EMACS
;;; Commentary:

;; Loads README.org file which contains the real meat

;;; Code:

(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
(garbage-collect)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-arduino pacmacs company-irony-c-headers company-irony irony arduino-mode js2-mode elpy undo-tree org-bullets ag dumb-jump treemacs-projectile treemacs-magit treemacs-icons-dired treemacs projectile rjsx-mode json-mode dimmer company page-break-lines dashboard typescript-mode emmet-mode speed-type smartparens smooth-scrolling diminish web-mode flycheck magit tide web-mode-edit-element popup-kill-ring 2048-game format-all counsel ivy avy smex auto-complete which-key use-package doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-banner-logo-title-face ((t (:family "Love LetterTW" :height 125)))))

(provide 'init)
;;; init.el ends here
