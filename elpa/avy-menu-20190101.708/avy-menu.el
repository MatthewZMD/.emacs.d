;;; avy-menu.el --- Library providing avy-powered popup menu -*- lexical-binding: t; -*-
;;
;; Copyright © 2016–2019 Mark Karpov <markkarpov92@gmail.com>
;;
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; URL: https://github.com/mrkkrp/avy-menu
;; Package-Version: 20190101.708
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3") (avy "0.3.0"))
;; Keywords: popup, menu
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The library provides Avy-powered popup menu that allows to quickly choose
;; from available options.  This is used in (at least) the following
;; packages:
;;
;; * `ace-popup-menu'
;; * `char-menu'
;; * `hasky-extensions'
;;
;; You can use it directly for your custom needs as well.

;;; Code:

(require 'avy)
(require 'cl-lib)

(defgroup avy-menu nil
  "Avy-powered popup menu."
  :group  'convenience
  :tag    "Avy Menu"
  :prefix "avy-menu-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/avy-menu"))

(defface avy-menu-title
  '((t (:inherit font-lock-function-name-face)))
  "Face used to print title of entire menu.")

(defface avy-menu-pane-header
  '((t (:inherit underline)))
  "Face used to print pane headers.")

(defface avy-menu-inactive
  '((t (:inherit shadow)))
  "Face used to print inactive menu items.")

;;;###autoload
(defun avy-menu (buffer-or-name menu &optional show-pane-header)
  "Show a popup menu in a temporary window and return user's selection.

BUFFER-OR-NAME specifies name of the buffer (or buffer itself)
that hosts menu options.  MENU itself should be a list of the
form (TITLE PANE1 PANE2 …), where each pane is a list of
form (TITLE ITEM1 ITEM2 …).  Each item is normally a cons
cell (STRING . VALUE), but a string can appear as an item—that
makes a non-selectable item in the menu.  Also, empty strings
start a new sub-section.

If SHOW-PANE-HEADER is not NIL, show pane headers (titles),
otherwise hide them.

Returned value is VALUE if user has selected something and NIL if
he has cancelled the whole menu or pressed key that does not
correspond to any available option."
  (let ((buffer (get-buffer-create buffer-or-name))
        menu-item-alist
        (first-pane t))
    (with-current-buffer buffer
      (with-current-buffer-window
       ;; buffer or name
       buffer
       ;; action (for `display-buffer')
       (cons 'display-buffer-below-selected
             '((window-height . fit-window-to-buffer)
               (preserve-size . (nil . t))))
       ;; quit-function
       (lambda (window _value)
         (with-selected-window window
           (unwind-protect
               (cdr
                (assq
                 (avy-with avy-menu
                   (avy--process (mapcar #'car menu-item-alist)
                                 #'avy--overlay-pre))
                 menu-item-alist))
             (when (window-live-p window)
               (quit-restore-window window 'kill)))))
       ;; menu generation
       (setq cursor-type nil)
       (cl-destructuring-bind (title . panes) menu
         (insert (propertize title 'face 'avy-menu-title)
                 "\n\n")
         (dolist (pane panes)
           (cl-destructuring-bind (title . items) pane
             (if first-pane
                 (setq first-pane nil)
               (insert "\n\n"))
             (when show-pane-header
               (insert (propertize title 'face 'avy-menu-pane-header)
                       "\n\n"))
             (let ((pane-alist (avy-menu--insert-strings items)))
               (if menu-item-alist
                   (nconc menu-item-alist pane-alist)
                 (setq menu-item-alist pane-alist))))))))))

(defun avy-menu--insert-strings (items)
  "Insert ITEMS much like `completion--insert-strings' in current buffer.

ITEMS should be a list, where every element is a cons of
form (STRING . VALUE), where STRING is the string to be printed
in current buffer and VALUE is used to construct result value of
this function.  ITEMS can contain plain strings, in this case
they are printed with inactive face.  Empty strings are not
printed, instead they begin new sub-section.

Return alist of values (POS . VALUE), where POS indicates
position of STRING in the buffer and VALUE is its associated
value according to ITEMS."
  (when (consp items)
    (let* ((strings (mapcar (lambda (x) (if (consp x) (car x) x))
                            items))
           (length (apply 'max
                          (mapcar #'string-width strings)))
           (window (get-buffer-window (current-buffer) 0))
           (wwidth (if window (1- (window-width window)) 79))
           (columns (min (max 2 (/ wwidth (+ 2 length)))
                         (max 1 (/ (length strings) 2))))
           (colwidth (/ wwidth columns))
           (column 0)
           (first t)
           laststring
           result)
      (dolist (str strings)
        (unless (equal laststring str)
          (setq laststring str)
          (let ((length (string-width str))
                (value  (cdr (assq str items))))
            (unless first
              (if (or (< wwidth (+ (max colwidth length) column))
                      (zerop length))
                  (progn
                    (insert "\n" (if (zerop length) "\n" ""))
                    (setq column 0))
                (insert " \t")
                (set-text-properties (1- (point)) (point)
                                     `(display (space :align-to ,column)))))
            (setq first (zerop length))
            (when value
              (push (cons (point) value) result))
            (insert (if value
                        str
                      (propertize str 'face 'avy-menu-inactive)))
            (setq column (+ column
                            (* colwidth (ceiling length colwidth)))))))
      (reverse result))))

(provide 'avy-menu)

;;; avy-menu.el ends here
