;;; init-purpose.el ---
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Set-up Purpose with a default programming layout
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


;; TODO: Fix Treemacs loading

;; Adapted from https://github.com/bmag/emacs-purpose/blob/master/window-purpose-x.el

(use-package window-purpose
  :ensure t
  :commands (purpose-mode))

(purpose-mode)

;; (require 'ibuffer)
;; (require 'ibuf-ext)
;; (require 'imenu-list)


;;(nil (0 0 284 82) (t (0 0 30 82) (:purpose buffers :purpose-dedicated t :width 0.10638297872340426 :height 0.18072289156626506 :edges (0.0 0.0 0.10638297872340426 0.18072289156626506)) (:purpose treemacs :purpose-dedicated t :width 0.10638297872340426 :height 0.5783132530120482 :edges (0.0 0.18072289156626506 0.10638297872340426 0.7590361445783133)) (:purpose ilist :purpose-dedicated t :width 0.10638297872340426 :height 0.2289156626506024 :edges (0.0 0.7590361445783133 0.10638297872340426 0.9879518072289156))) (t (30 0 284 82) (nil (30 0 284 64) (:purpose edit :purpose-dedicated t :width 0.574468085106383 :height 0.7710843373493976 :edges (0.10638297872340426 0.0 0.6808510638297872 0.7710843373493976)) (:purpose repl :purpose-dedicated t :width 0.3262411347517731 :height 0.7710843373493976 :edges (0.6808510638297872 0.0 1.0070921985815602 0.7710843373493976))) (:purpose shell :purpose-dedicated t :width 0.900709219858156 :height 0.21686746987951808 :edges (0.10638297872340426 0.7710843373493976 1.0070921985815602 0.9879518072289156))))

;; (defvar purpose-programming-window-layout
;;   '(nil
;;     (0 0 364 100)
;;     (t
;;      (0 0 49 100)
;;      (:purpose buffers :purpose-dedicated t :width 0.13535911602209943 :height 0.1782178217821782
;; 	       :edges (0.0 0.0 0.13535911602209943 0.1782178217821782))
;;      (:purpose treemacs :purpose-dedicated t :width 0.13535911602209943 :height 0.5742574257425742
;; 	       :edges (0.0 0.1782178217821782 0.13535911602209943 0.7524752475247525))
;;      (:purpose ilist :purpose-dedicated t :width 0.13535911602209943 :height 0.2376237623762376
;; 	       :edges (0.0 0.7524752475247525 0.13535911602209943 0.9900990099009901)))
;;     (t
;;      (49 0 364 100)
;;      (nil
;;       (49 0 364 78)
;;       (:purpose edit :purpose-dedicated t :width 0.4889502762430939 :height 0.7722772277227723
;; 		:edges (0.13535911602209943 0.0 0.6243093922651933 0.7722772277227723))
;;       (t
;;        (226 0 364 78)
;;        (:purpose general :purpose-dedicated t :width 0.3812154696132597 :height 0.38613861386138615
;; 		 :edges (0.6243093922651933 0.0 1.0055248618784531 0.38613861386138615))
;;        (:purpose repl :purpose-dedicated t :width 0.3812154696132597 :height 0.38613861386138615
;; 		 :edges (0.6243093922651933 0.38613861386138615 1.0055248618784531 0.7722772277227723))))
;;      (:purpose shell :purpose-dedicated t :width 0.8701657458563536 :height 0.21782178217821782
;; 	       :edges (0.13535911602209943 0.7722772277227723 1.0055248618784531 0.9900990099009901)))))

;; (defvar purpose-programming-config
;;   (purpose-conf
;;                 :mode-purposes
;;                 '((ibuffer-mode . buffers)
;;                   (treemacs-mode . treemacs)
;;                   (imenu-list-major-mode . ilist)
;;                   (term-mode . shell)
;;                   (cider-repl-mode . repl)
;;                   (debugger-mode . general)
;;                   (magit-mode . edit))
;;                 :name-purposes
;;                 '(("*Org Agenda*" . edit)
;;                   ("*shell*" . shell)
;;                   ("*Python" . shell))))

;; (defvar purpose-programming-buffers-changed nil
;;   "Internal variable for use with `frame-or-buffer-changed-p'.")

;; (define-ibuffer-filter purpose-programming-ibuffer-files-only
;;     "Display only buffers that are bound to files."
;;   ()
;;   (buffer-file-name buf))

;; (defun purpose-programming--setup-ibuffer ()
;;   "Set up ibuffer settings."
;;   (add-hook 'ibuffer-mode-hook
;;             #'(lambda ()
;;                 (ibuffer-filter-by-purpose-programming-ibuffer-files-only nil)))
;;   (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
;;   (setq ibuffer-formats '((mark " " name)))
;;   (setq ibuffer-display-summary nil)
;;   (setq ibuffer-use-header-line nil)
;;   ;; not sure if we want this...
;;   ;; (setq ibuffer-default-shrink-to-minimum-size t)
;;   (when (get-buffer "*Ibuffer*")
;;     (kill-buffer "*Ibuffer*"))
;;   (save-selected-window
;;     (ibuffer-list-buffers)))

;; (defun purpose-programming--unset-ibuffer ()
;;   "Unset ibuffer settings."
;;   (remove-hook 'ibuffer-mode-hook
;;                #'(lambda ()
;;                    (ibuffer-filter-by-purpose-programming-ibuffer-files-only nil)))
;;   (remove-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
;;   (setq ibuffer-formats '((mark modified read-only " "
;;                                 (name 18 18 :left :elide)
;;                                 " "
;;                                 (size 9 -1 :right)
;;                                 " "
;;                                 (mode 16 16 :left :elide)
;;                                 " " filename-and-process)
;;                           (mark " "
;;                                 (name 16 -1)
;;                                 " " filename)))
;;   (setq ibuffer-display-summary t)
;;   (setq ibuffer-use-header-line t))


;; (defun purpose-programming-update-changed ()
;;   "Update auxiliary buffers if frame/buffer had changed.
;; Uses `frame-or-buffer-changed-p' to determine whether the frame or
;; buffer had changed."
;;   (when (frame-or-buffer-changed-p 'purpose-programming-buffers-changed)
;;     (imenu-list-update-safe)))

;; ;;;###autoload
;; (defun purpose-programming-setup ()
;;   "Setup purpose-programming config."
;;   (interactive)
;;   (purpose-set-extension-configuration :purpose-programming purpose-programming-config)
;;   (purpose-programming--setup-ibuffer)
;;   (imenu-list-minor-mode)
;;   (frame-or-buffer-changed-p 'purpose-programming-buffers-changed)
;;   (add-hook 'post-command-hook #'purpose-programming-update-changed)
;;   (purpose-set-window-layout purpose-programming-window-layout))

;; (defun purpose-programming-unset ()
;;   "Unset purpose-programming."
;;   (interactive)
;;   (purpose-del-extension-configuration :purpose-programming)
;;   (purpose-programming--unset-ibuffer)
;;   (imenu-list-minor-mode -1)
;;   (remove-hook 'post-command-hook #'purpose-programming-update-changed))


(provide 'init-purpose)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-purpose.el ends here
