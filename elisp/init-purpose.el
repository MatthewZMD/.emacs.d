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


;; TODO: Add open buffers frame
;; TODO: Fix Treemacs loading


(use-package window-purpose)
(purpose-mode)

(require 'imenu-list)

(defvar purpose-programming-window-layout
  '(nil
    (0 0 364 100)
    (t
     (0 0 49 100)
     (:purpose buffers :purpose-dedicated t :width 0.13535911602209943 :height 0.1782178217821782
	       :edges (0.0 0.0 0.13535911602209943 0.1782178217821782))
     (:purpose treemacs :purpose-dedicated t :width 0.13535911602209943 :height 0.5742574257425742
	       :edges (0.0 0.1782178217821782 0.13535911602209943 0.7524752475247525))
     (:purpose ilist :purpose-dedicated t :width 0.13535911602209943 :height 0.2376237623762376
	       :edges (0.0 0.7524752475247525 0.13535911602209943 0.9900990099009901)))
    (t
     (49 0 364 100)
     (nil
      (49 0 364 78)
      (:purpose edit :purpose-dedicated t :width 0.4889502762430939 :height 0.7722772277227723
		:edges (0.13535911602209943 0.0 0.6243093922651933 0.7722772277227723))
      (t
       (226 0 364 78)
       (:purpose general :purpose-dedicated t :width 0.3812154696132597 :height 0.38613861386138615
		 :edges (0.6243093922651933 0.0 1.0055248618784531 0.38613861386138615))
       (:purpose repl :purpose-dedicated t :width 0.3812154696132597 :height 0.38613861386138615
		 :edges (0.6243093922651933 0.38613861386138615 1.0055248618784531 0.7722772277227723))))
     (:purpose shell :purpose-dedicated t :width 0.8701657458563536 :height 0.21782178217821782
	       :edges (0.13535911602209943 0.7722772277227723 1.0055248618784531 0.9900990099009901)))))


(defvar purpose-programming-config
  (purpose-conf
                :mode-purposes
                 '((treemacs-mode . treemacs)
                   (imenu-list-major-mode . ilist)
                   (term-mode . shell)
                   (cider-repl-mode . repl)
                   (debugger-mode . general)
                   (org-agenda-mode . general))))

(defvar purpose-programming-buffers-changed nil
  "Internal variable for use with `frame-or-buffer-changed-p'.")

(defun purpose-programming-update-changed ()
  "Update auxiliary buffers if frame/buffer had changed.
Uses `frame-or-buffer-changed-p' to determine whether the frame or
buffer had changed."
  (when (frame-or-buffer-changed-p 'purpose-programming-buffers-changed)
    (imenu-list-update-safe)))

;;;###autoload
(defun purpose-programming-setup ()
  "Setup purpose-programming config."
  (interactive)
  (purpose-set-extension-configuration :purpose-programming purpose-programming-config)
  (imenu-list-minor-mode)
  (frame-or-buffer-changed-p 'purpose-programming-buffers-changed)
  (add-hook 'post-command-hook #'purpose-programming-update-changed)
  (purpose-set-window-layout purpose-programming-window-layout))

(defun purpose-programming-unset ()
  "Unset purpose-programming."
  (interactive)
  (purpose-del-extension-configuration :purpose-programming)
  (imenu-list-minor-mode -1)
  (remove-hook 'post-command-hook #'purpose-programming-update-changed))


(provide 'init-purpose)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-purpose.el ends here
