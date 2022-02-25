;; General UI settings
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(column-number-mode t)
(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Set a better font
(set-face-attribute 'default nil :height 150 :family "Fira Code" :weight 'light)
(set-face-attribute 'fixed-pitch nil :height 150 :family "Fira Code" :weight 'light)
(set-face-attribute 'variable-pitch nil :height 180 :family "FiraGO" :weight 'light)

;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; But disable text-scale via the mouse
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;; Visible bell
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;; Clean-up the mode-line
;;(use-package diminish)

;; Use a line cursor
(set-default 'cursor-type  '(bar . 3))

(provide 'init-ui)
