;;; package --- Summary
;;; This is MT`s personal init.el file for EMACS
;;; Commentary:
;;; Code:

;; next few lines are from a guy on reddit basically sets font and theme for the daemon
(defvar nox/fonts '(("Input" . 11) ("SF Mono" . 11) ("Consolas" . 11))
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

    ;; (when (version<= "26.0.50" emacs-version )
    ;;   (global-display-line-numbers-mode))

    ;; NOTE(nox): This needs to be here, else it doesn't work
    (setq-default system-time-locale "C")))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'nox/setup-appearance)
  (nox/setup-appearance (car (frame-list))))

;; hook line numbers to only when files are opened
(add-hook 'find-file-hook #'display-line-numbers-mode)

;; Eval-buffer ELisp Code
(global-set-key (kbd "<f5>") 'eval-buffer)


;; Column Number
;; (global-linum-mode t)


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

;; add to target in the shortcut properties
;; "path to emacsclientw.exe\emacsclientw.exe" -c -n -a ""


;; Package configs
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
    (dimmer company page-break-lines dashboard typescript-mode emmet-mode speed-type smartparens smooth-scrolling diminish web-mode flycheck magit tide web-mode-edit-element popup-kill-ring 2048-game format-all counsel ivy avy smex auto-complete which-key use-package doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Packages

;; Doom-theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-solarized-light t))

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
  (setq dashboard-startup-banner "~/.emacs.d/images/KEC.png"))

;; init time shown on dashboard
(defun dashboard-init-time (list-size)
  """Set a dashboard item including information on package initialization
   time and garbage collections."""
  (insert (format "Emacs ready in %.2f seconds with %d garbage collections."
		  (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
(add-to-list 'dashboard-item-generators  '(init-time . dashboard-init-time))
(add-to-list 'dashboard-items '(init-time)) ;; note adding t as 4 param adds to back of list

;; Avy
(use-package avy
  :ensure t
  :bind (("C-c C-SPC" . avy-goto-char-timer)
	 ("C-c C-l" . avy-goto-line)))

;; Smex
(use-package smex :ensure t)

;; Ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode ;;Hide ivy in the button screen
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "[%d/%d] ")
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

(use-package dimmer
  :ensure t
  :init (dimmer-mode)
  :config
  (setq dimmer-fraction 0.35)
  (setq dimmer-exclusion-regexp " *Minibuf-1*")
  (setq dimmer-exclusion-regexp " *Minibuf-2*")
  (setq dimmer-exclusion-regexp "*dashboard*"))

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
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  ;; configure javascript-tide checker to run after your default javascript checker
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

;; Emmet-mode
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.)


(provide 'init)
;;; init.el ends here
