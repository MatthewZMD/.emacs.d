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

(require 'imenu-list)

(defvar purpose-programming-window-layout
  '(nil
   (0 0 364 100)
   (:purpose treemacs :purpose-dedicated t :width 0.10497237569060773 :height 0.9900990099009901
             :edges (0.0 0.0 0.10497237569060773 0.9900990099009901))
   (:purpose edit :purpose-dedicated t :width 0.45027624309392267 :height 0.9900990099009901
             :edges (0.10497237569060773 0.0 0.5552486187845304 0.9900990099009901))
   (t
    (201 0 364 100)
    (:purpose info :purpose-dedicated t :width 0.45027624309392267 :height 0.49504950495049505
              :edges (0.5552486187845304 0.0 1.0055248618784531 0.49504950495049505))
    (:purpose repl :purpose-dedicated t :width 0.45027624309392267 :height 0.49504950495049505
              :edges (0.5552486187845304 0.49504950495049505 1.0055248618784531 0.9900990099009901)))))


(defvar purpose-programming-config
  (purpose-conf
                :mode-purposes
                 '((treemacs-mode . treemacs)
                   (imenu-list-major-mode . info)
                   (cider-repl . repl))))

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
  (treemacs-mode)

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
