;;; -*- lexical-binding: t; -*-

;; Bindings

(global-set-key (kbd "C-z") nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x C-!") 'toggle-truncate-lines)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd"C--") 'text-scale-decrease)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)


(provide 'init-global-config)
