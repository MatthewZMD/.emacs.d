;;; package --- Summary
;;; This is MT`s personal init.el file for EMACS
;;; Commentary:
;;; Code:

;; next few lines are from a guy on reddit basically sets font and theme for the daemon
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar nox/fonts '(("Input" . 11) ("SF Mono" . 12) ("Consolas" . 12) ("Love LetterTW" . 12.5))
  "List of fonts and sizes.  The first one available will be used.")

(defun nox/change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font nox/fonts (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))

    (if (not available-fonts)
        (error "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t)
                                       available-fonts)))
            (setq font-name (car chosen)
                  font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts)
              font-size (cdar available-fonts)))

      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))


(defun nox/setup-appearance (frame)
  "FRAME This set up the appearance of EMACS."
  (with-selected-frame frame
    (remove-hook 'after-make-frame-functions 'nox/setup-appearance)

    ;; Minimal UI
    (scroll-bar-mode -1)
    (tool-bar-mode   -1)
    (tooltip-mode    -1)
    (menu-bar-mode   -1)

    ;; Maximize the frame
    (toggle-frame-maximized)

    (nox/change-font)

    ;; Modify the next section as you wish
    (display-time-mode)
    (global-hl-line-mode)
    (blink-cursor-mode -1)

    ;; Column Number
    ;;(global-linum-mode t)

    ;; (when (version<= "26.0.50" emacs-version )
    ;;   (global-display-line-numbers-mode))

    ;; NOTE(nox): This needs to be here, else it doesn't work
    (setq-default system-time-locale "C")))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'nox/setup-appearance)
  (nox/setup-appearance (car (frame-list))))
;; add to target in the shortcut properties
;; "path to emacsclientw.exe\emacsclientw.exe" -c -n -a ""

;; hook line numbers to only when files are opened
(add-hook 'find-file-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Eval-buffer ELisp Code
(global-set-key (kbd "<f5>") 'eval-buffer)

;; Auto-rename new eww buffers
(defun xah-rename-eww-hook ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook #'xah-rename-eww-hook)

;; C-u M-x eww will force a new eww buffer
(defun modi/force-new-eww-buffer (orig-fun &rest args)
  "ORIG-FUN ARGS When prefix argument is used, a new eww buffer will be created, regardless of whether the current buffer is in `eww-mode'."
  (if current-prefix-arg
      (with-temp-buffer
        (apply orig-fun args))
    (apply orig-fun args)))
(advice-add 'eww :around #'modi/force-new-eww-buffer)

;; run-bash command
;; (defun run-bash ()
;;   (interactive)
;;   (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe"))
;;     (shell "*bash*")))

;; Resizes the window width based on the input
(defun window-resize-width (w)
  "Resizes the window width based on W."
  (interactive (list (if (> (count-windows) 1)
			 (read-number "Set the current window width (0~1): ")
		       (error "You need more than 1 window to execute this function!"))))
  (message "%s" w)
  (window-resize nil (- (truncate (* w (frame-width))) (window-total-width)) t))
(global-set-key (kbd "C-x C-|") 'window-resize-width)

;; Resizes the window height based on the input
(defun window-resize-height (h)
  "Resizes the window height based on H."
  (interactive (list (if (> (count-windows) 1)
			 (read-number "Set the current window height (0~1): ")
		       (error "You need more than 1 window to execute this function!"))))
  (message "%s" h)
  (window-resize nil (- (truncate (* h (frame-height))) (window-total-height)) nil))
(global-set-key (kbd "C-x C-_") 'window-resize-height)



;;; Package configs---------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-bullets ag dumb-jump spaceline-all-the-icons spaceline treemacs-projectile treemacs-magit treemacs-icons-dired treemacs projectile rjsx-mode json-mode dimmer company page-break-lines dashboard typescript-mode emmet-mode speed-type smartparens smooth-scrolling diminish web-mode flycheck magit tide web-mode-edit-element popup-kill-ring 2048-game format-all counsel ivy avy smex auto-complete which-key use-package doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-banner-logo-title-face ((t (:family "Love LetterTW" :height 125)))))


;; Packages

(use-package org
  :ensure t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switch))

;; Bullets for org mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

;; Doom-theme
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-spacegrey t)
  )

;; Magit
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

;; Diminish (hide minor modes from mode line
(use-package diminish :ensure t)

(use-package page-break-lines
  :ensure t
  :config
  (page-break-lines-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Present Day, Present Time...")
  (setq dashboard-startup-banner "~/.emacs.d/images/KEC_Dark_BK.png"))
;;  (setq dashboard-startup-banner "~/.emacs.d/images/KEC_Light_BK.png"))

;; init time shown on dashboard
(defun dashboard-init-time (list-size)
  "LIST-SIZE Set a dashboard item including information on package initialization time and garbage collections."
  (insert (format "Emacs ready in %.2f seconds with %d garbage collections."
		  (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
(add-to-list 'dashboard-item-generators  '(init-time . dashboard-init-time))
(add-to-list 'dashboard-items '(init-time)) ;; note adding t as 4 param adds to back of list

;;Set the Dashboard banner logo title font to Love LetterTW


;; AG Silver Searcher
(use-package ag :ensure t)


;; Avy
(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)
	 ("C-:" . avy-goto-line)))

;; Smex
(use-package smex :ensure t)

;; Ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode ;;Hide ivy in the button screen
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "【%d/%d】")
  (setq ivy-wrap t))

;; Counsel
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init (counsel-mode 1))

;; Swiper
(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

;; Projectile
(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))


;; Smooth Scrolling
(use-package smooth-scrolling
  :ensure t
  :config
  (setq scroll-margin 1
	scroll-conservatively 9999
	scroll-step 1
	mouse-wheel-scroll-amount '(2)
	mouse-wheel-progressive-speed nil))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Popup-kill-ring
(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

;; Spaceline
(use-package spaceline
  :ensure t
  :config (spaceline-spacemacs-theme))

;; All the Icons
;; Execute (all-the-icons-install-fonts) in first time
(use-package all-the-icons :ensure t)

;; Spaceline All The Icons
(use-package spaceline-all-the-icons
  :ensure t)

;; Company
(use-package company
  :ensure t
  :diminish company-mode
  :defer t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
  (setq company-idle-delay 0)                           ; decrease delay before autocompletion popup shows
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)) ; tab -> autocompletion

;; FlyCheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (flycheck-add-mode 'typescript-tslint 'js2-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))

;; Auto-complete
(use-package auto-complete
  :ensure t
  :init (global-auto-complete-mode t))

;; Format-all buffer
(use-package format-all
  :ensure t
  :init (format-all-mode))

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)


;; Dimmer mode highlights current buffer
(use-package dimmer
  :ensure t
  :init (dimmer-mode)
  :config
  (setq dimmer-fraction 0.2)
  (setq dimmer-exclusion-regexp "\\*Minibuf-[0-9]+\\*\\|\\*dashboard\\*"))

;; Treemacs shows left panel
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-git-command-pipe           ""
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-max-git-entries            5000
          treemacs-no-png-images              nil
          treemacs-no-delete-other-windows    t
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-cursor                nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config
  (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; Smartparens (customize this more)
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))

;; 2048 Game
(use-package 2048-game :ensure t)

;; Speed-type
(use-package speed-type :ensure t)

;; Web-mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

;; Angular 2+ mode
;; (use-package ng2-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.component.ts\\'" . ng2-ts-mode))
;;   (add-to-list 'auto-mode-alist '("\\.component.html\\'" . ng2-html-mode))
;;   (add-to-list 'auto-mode-alist '("\\.component.thtml\\'" . ng2-html-mode)))

;; JS2-mode
(use-package js2-mode :ensure t)

;; JSon-mode
(use-package json-mode :ensure t)

;; RJSX-mode
(use-package rjsx-mode :ensure t)

;; TypeScript mode
(use-package typescript-mode :ensure t)

;; Tide
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;;  (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
  ;;  (flycheck-add-mode 'typescript-tide 'ng2-ts-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  ;; configure javascript-tide checker to run after your default javascript checker
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))


;; Emmet-mode
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hooktype  'emmet-mode)) ;; enable Emmet's css abbreviation.)


(provide 'init)
;;; init.el ends here
