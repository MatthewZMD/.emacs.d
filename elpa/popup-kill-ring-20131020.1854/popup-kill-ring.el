;;; popup-kill-ring.el --- interactively insert item from kill-ring

;; Copyright (C) 2010  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; URL: https://github.com/waymondo/popup-kill-ring
;; Package-Version: 20131020.1854
;; Keywords: popup, kill-ring, pos-tip
;; Package-Requires: ((popup "0.4") (pos-tip "0.4"))

;; MELPA compatibility added by Justin Talbott

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Manage your `kill-ring' (select and paste).

;;; Requirement:
;;
;; * popup.el   http://github.com/m2ym/auto-complete
;; * pos-tip.el http://www.emacswiki.org/emacs/PosTip

;;; Setting:
;;
;; 1. Download the `popup.el', `pos-tip.el' and this file.
;; 2. Put your `load-path' directory to the `popup.el', `pos-tip.el'
;;    and this file.
;; 3. Add following settings to your .emacs.
;;
;;   (require 'popup)
;;   (require 'pos-tip)
;;   (require 'popup-kill-ring)
;;
;;   (global-set-key "\M-y" 'popup-kill-ring) ; For example.
;;
;; * If you insert a selected item interactively, add following line to
;;   your .emacs.
;;
;;   (setq popup-kill-ring-interactive-insert t)

;;; Tested:
;;
;; * Emacs
;;   * 23.1
;;   * 24.0.50
;; * popup.el
;;   * 0.4
;; * pos-tip.el
;;   * 0.4.0
;;

;;; ChangeLog:
;;
;; * 0.2.8 (2011/06/10)
;;   Added the new variable `popup-kill-ring-last-used-move-first'.  If
;;   this variable is non-nil, It means that last selected `kill-ring'
;;   item comes first of `kill-ring'. This value is `t' by default.
;;
;; * 0.2.7 (2010/05/05)
;;   If `popup-kill-ring-interactive-insert' is `t' and
;;   `C-g' was typed, clear the inserted string.
;;
;; * 0.2.6 (2010/05/05)
;;   Change `popup-kill-ring' to execute `pos-tip-hide' at all time.
;;
;; * 0.2.5 (2010/05/02)
;;   When `point' is on minibuffer, do ordinary `yank' command.
;;
;; * 0.2.4 (2010/05/01)
;;   Fixed change a place the doing `receter'.
;;
;; * 0.2.3 (2010/05/01)
;;   Add variable `popup-kill-ring-interactive-insert-face'.
;;   Now add face for interactive inserted string when
;;   `popup-kill-ring-interactive-insert-face' is `t'.
;;
;; * 0.2.2 (2010/04/29)
;;   Fix the broken `popup-menu*' overlay window when
;;   `popup-kill-ring-interactive-insert' is `t'.
;;
;; * 0.2.1 (2010/04/29)
;;   New variable `popup-kill-ring-item-size-max'.
;;   Now tested on `pos-tip' 0.3.6
;;
;; * 0.2.0 (2010/04/29)
;;   New variable `popup-kill-ring-popup-margin-left'
;;   New variable `popup-kill-ring-isearch'
;;   New variable `popup-kill-ring-item-min-width'
;;   Now `isearch' argument of `popup-menu*' is `t' by default.
;;   If the length of item of `kill-ring' was shorter than
;;   `popup-kill-ring-item-min-width', Now discards it.
;;
;; * 0.1.0
;;   New variable `popup-kill-ring-interactive-insert'.
;;
;; * 0.0.9
;;   Bug fix for `popup-kill-ring-previous'.
;;   New variable `popup-kill-ring-pos-tip-color'.
;;   Fix document of this file.
;;
;; * 0.0.8
;;   Modify keymap setting.
;;
;; * 0.0.7
;;   Added the function `popup-kill-ring-current'.
;;   Added the function `popup-kill-ring-hide'.
;;
;; * 0.0.6
;;   `up' to `popup-kill-ring-popup-previous'.
;;   `down' to `popup-kill-ring-popup-next'.
;;
;; * 0.0.5
;;   New variable `popup-kill-ring-kill-ring-show-func'.
;;   New Variable `popup-kill-ring-keymap'.
;;
;; * 0.0.4
;;   abolished the substring of menu item.
;;   set margin-right and width to `popup-menu*'.
;;
;; * 0.0.3
;;   `pos-tip-show' argument `DY' to 0.
;;
;; * 0.0.2
;;   `with-no-warnings' for variable `menu'.
;;
;; * 0.0.1:
;;   Initial version.

;;; Code:

(require 'popup)
(require 'pos-tip)
;; for `return' macro
(eval-when-compile
  (require 'cl))


;;; Variables:

(defconst popup-kill-ring-version "0.2.8"
  "Version of `popup-kill-ring'")


(defvar popup-kill-ring-popup-width 30
  "*Width of popup item.")

(defvar popup-kill-ring-popup-margin-left 2
  "*Width of `popup-menu*' margin-left.")

(defvar popup-kill-ring-popup-margin-right 2
  "*Width of `popup-menu*' margin-right.")

(defvar popup-kill-ring-timeout 1
  "*Time of displaying `pos-tip-show' help tooltip.")

(defvar popup-kill-ring-kill-ring-show-func 'popup-kill-ring-pos-tip-show
  "*Function of displaying the contents of `kill-ring'.
This function requires two arguments `str' and `pos'.
`str' is string of displaying. `pos' is point of displaying.
Default value is `popup-kill-ring-pos-tip-show'.")

(defvar popup-kill-ring-pos-tip-color nil
  "*Face for `pos-tip-show'.
See docstring of `pos-tip-show'.")

(defvar popup-kill-ring-interactive-insert nil
  "*Non-nil means that insert selected item of `popup-menu*' interactively.")

(defvar popup-kill-ring-isearch t
  "*Non-nil means that passes `t' to `isearch' option of `popup-menu*'")

(defvar popup-kill-ring-item-min-width 3
  "*The number that shows minimum width of displaying `kill-ring' item
of `popup-menu*'")

(defvar popup-kill-ring-item-size-max nil
  "*The number that means max each item size of `popup-menu'.
If item size is longer than this number, it's truncated.
Nil means that item does not be truncate.")

(defvar popup-kill-ring-interactive-insert-face 'highlight
  "*The face for interactively inserted string when
`popup-kill-ring-interactive-insert' is `t'.")

(defvar popup-kill-ring-last-used-move-first t
  "*Non-nil means that last selected `kill-ring' item comes first of
`kill-ring'.")

;; key setting for `popup-menu*'.
(defvar popup-kill-ring-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap popup-menu-keymap)
    (define-key keymap "\r" 'popup-kill-ring-select)
    (define-key keymap "\C-n" 'popup-kill-ring-next)
    (define-key keymap "\C-p" 'popup-kill-ring-previous)
    (define-key keymap [down] 'popup-kill-ring-next)
    (define-key keymap [up] 'popup-kill-ring-previous)
    (define-key keymap "\C-f" 'popup-kill-ring-current)
    (define-key keymap "\C-b" 'popup-kill-ring-hide)
    (define-key keymap [right] 'popup-kill-ring-current)
    (define-key keymap [left] 'popup-kill-ring-hide)
    keymap)
    "A keymap for `popup-menu*' of `popup-kill-ring'.")

(defvar popup-kill-ring-buffer-point-hash nil
  "The hash of buffer(key) and list of point(value).
key is buffer name.
value is list of points (start . end).
this is internal variable for `popup-kill-ring'.")

;;;###autoload
;;; Functions:

(defun popup-kill-ring ()
  "Interactively insert selected item from `key-ring' by `popup.el'
and `pos-tip.el'"
  (interactive)
  (cond
   ((minibufferp)
    (yank))
   (t (let* ((index 0)
             (summary 0)
             (kring (let (l p-max)
                      (dolist (i kill-ring)
                        (when (or (null popup-kill-ring-item-min-width)
                                  (>= (length i)
                                      popup-kill-ring-item-min-width))
                          (setq l
                                (cons
                                 (propertize
                                  (with-temp-buffer
                                    (erase-buffer)
                                    (insert (replace-regexp-in-string
                                             "[ \t]+" " "
                                             (replace-regexp-in-string
                                              "\n" " " i)))
                                    (cond
                                     ((and popup-kill-ring-item-size-max
                                           (>= (point-max)
                                               popup-kill-ring-item-size-max))
                                      (setq p-max
                                            popup-kill-ring-item-size-max))
                                     (t
                                      (setq p-max (point-max))))
                                    (buffer-substring-no-properties
                                     (point-min) p-max))
                                  'index index
                                  'summary (concat "("
                                                   (int-to-string summary)
                                                   ")"))
                                 l))
                          (setq summary (1+ summary)))
                        (setq index (1+ index)))
                      (when l
                        (nreverse l))))
             (popup-kill-ring-buffer-point-hash (make-hash-table :test 'equal))
             num item)
        (when popup-kill-ring-interactive-insert
          ;; TODO: (nth 0 kill-ring) != (nth 0 kring).
          ;; The kring discards item that lenght shorter than
          ;; `popup-kill-ring-item-min-width'.
          ;; So, The following code should be fixed.
          (popup-kill-ring-insert-item 0))
        ;; always execute `pos-tip-hide'
        ;; (The case that the item was selected.
        ;;  the case that it was typed `C-g'.)
        (unwind-protect
            (progn (setq item (popup-menu* kring
                                           :width popup-kill-ring-popup-width
                                           :keymap popup-kill-ring-keymap
                                           :margin-left popup-kill-ring-popup-margin-left
                                           :margin-right popup-kill-ring-popup-margin-right
                                           :scroll-bar t
                                           :isearch popup-kill-ring-isearch))
                   (when item
                     (setq num (popup-kill-ring-get-index item))
                     (when num
                       (let ((kill-ring-item (nth num kill-ring)))
                         (when popup-kill-ring-last-used-move-first
                           (setq kill-ring
                                 (nconc (reverse (last (reverse kill-ring) num))
                                        (cdr (nthcdr num kill-ring))))
                           (push kill-ring-item kill-ring))
                         (insert kill-ring-item)))))
          (pos-tip-hide)
          ;; If `C-g' was typed, clear the inserted string. (7 is `C-g'.)
          (when (and popup-kill-ring-interactive-insert
                     (numberp last-input-event)
                     (= last-input-event ? ))
            (popup-kill-ring-clear-inserted)))))))

(defun popup-kill-ring-pos-tip-show (str pos)
  (when (eq window-system 'x)
    (pos-tip-show str popup-kill-ring-pos-tip-color pos nil 0 nil nil nil 0)))

(defun popup-kill-ring-select ()
  (interactive)
  (let* ((m (with-no-warnings menu))
         (num (popup-cursor m))
         (lst (popup-list m))
         (item (popup-item-value-or-self (nth num lst)))
         (p (gethash (buffer-name) popup-kill-ring-buffer-point-hash)))
    (when popup-kill-ring-interactive-insert
      (goto-char (cdr (gethash (buffer-name)
                               popup-kill-ring-buffer-point-hash)))
      (when (and p (listp p))
        ;; After the `popup-isearch', these is the case that diffrence selected
        ;; item of `popup-menu*' from inserted string to `current-buffer'.
        (unless (string= (buffer-substring (car p) (cdr p)) item)
          (delete-region (car p) (cdr p))
          (return item)))
      (return))
    (return item)))

(defun popup-kill-ring-next ()
  (interactive)
  ;; Variable `menu' is contents of popup.
  ;; See: `popup-menu-event-loop'
  (let* ((m (with-no-warnings menu))
         (num (1+ (popup-cursor m)))
         (lst (popup-list m))
         (len (length lst))
         (offset (popup-offset m))
         item)
    (when (>= num len)
      (setq num 0))
    (when popup-kill-ring-interactive-insert
      (popup-kill-ring-clear-inserted))
    (setq item (popup-x-to-string (nth num lst)))
    ;; Variable `num' is converted from the index of `popup-menu*'
    ;; to the index of `kill-ring'.
    (setq num (popup-kill-ring-get-index item))
    ;; Since the every time drawing is very heavy,
    ;; `pos-tip' help is displays when timeout occured.
    (with-timeout
        (popup-kill-ring-timeout
         (progn
           ;; display selected item of kill-ring by `pos-tip-show'
           (when num
             (funcall popup-kill-ring-kill-ring-show-func
                      (format "%s" (nth num kill-ring))
                      (popup-child-point m offset)))))
      (pos-tip-hide)
      (popup-next m)
      (when popup-kill-ring-interactive-insert
        (popup-kill-ring-insert-item num))
      ;; wait for timeout
      (sit-for (+ 0.5 popup-kill-ring-timeout)))))

(defun popup-kill-ring-current ()
  (interactive)
  ;; Variable `menu' is contents of popup.
  ;; See: `popup-menu-event-loop'
  (let* ((m (with-no-warnings menu))
         (num (popup-cursor m))
         (lst (popup-list m))
         (len (length lst))
         (offset (popup-offset m))
         item)
    ;; display selected item of kill-ring by `pos-tip-show'
    (setq item (popup-x-to-string (nth num lst)))
    (setq num (popup-kill-ring-get-index item))
    (when num
      (funcall popup-kill-ring-kill-ring-show-func
               (format "%s" (nth num kill-ring))
               (popup-child-point m offset)))))

(defun popup-kill-ring-previous ()
  (interactive)
  ;; Variable `menu' is contents of popup.
  ;; See: `popup-menu-event-loop'
  (let* ((m (with-no-warnings menu))
         (num (1- (popup-cursor m)))
         (lst (popup-list m))
         (len (length lst))
         (offset (popup-offset m))
         item)
    (when popup-kill-ring-interactive-insert
      (popup-kill-ring-clear-inserted))
    (when (< num 0)
      (setq num (1- len)))
    (setq item (popup-x-to-string (nth num lst)))
    ;; Variable `num' is converted from the index of `popup-menu*'
    ;; to the index of `kill-ring'.
    (setq num (popup-kill-ring-get-index item))
    ;; Since the every time drawing is very heavy,
    ;; `pos-tip' help is displays when timeout occured.
    (with-timeout
        (popup-kill-ring-timeout
         (progn
           ;; display selected item of kill-ring by `pos-tip-show'
           (when num
             (funcall popup-kill-ring-kill-ring-show-func
                      (format "%s" (nth num kill-ring))
                      (popup-child-point m offset)))))
      (pos-tip-hide)
      (popup-previous m)
      (when popup-kill-ring-interactive-insert
        (popup-kill-ring-insert-item num))
      ;; wait for timeout
      (sit-for (+ 0.5 popup-kill-ring-timeout)))))

(defun popup-kill-ring-hide ()
  (interactive)
  (pos-tip-hide))

(defun popup-kill-ring-get-index (item)
  (with-temp-buffer
    (erase-buffer)
    (insert item)
    (get-text-property (point-min) 'index)))

(defun popup-kill-ring-insert-item (num)
    (let* ((item (format "%s" (nth num kill-ring)))
           (s (point))
           (e (+ s (length item)))
           ol)
      (puthash (buffer-name) (cons s e)
               popup-kill-ring-buffer-point-hash)
      (unwind-protect
          (with-timeout (1.0 (if ol (delete-overlay ol)))
            (insert item)
            (recenter)
            (setq ol (make-overlay s e))
            (overlay-put ol 'face popup-kill-ring-interactive-insert-face)
            (sit-for 0.5))
        (if ol (delete-overlay ol)))))

(defun popup-kill-ring-clear-inserted ()
  (when popup-kill-ring-interactive-insert
    (let* ((p (gethash (buffer-name)
                       popup-kill-ring-buffer-point-hash)))
      (when (and p (listp p))
        (delete-region (car p) (cdr p))
        (goto-char (car p))))))

(provide 'popup-kill-ring)


;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; popup-kill-ring.el ends here
