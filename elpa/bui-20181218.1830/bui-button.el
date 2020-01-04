;;; bui-button.el --- Text buttons and faces  -*- lexical-binding: t -*-

;; Copyright © 2014-2016 Alex Kost <alezost@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides general faces and some code to display buttons and
;; to work with them.

;;; Code:

(require 'cus-edit)             ; for faces
(require 'dash)
(require 'bui-utils)

(defface bui-time
  '((t :inherit font-lock-constant-face))
  "Face used for timestamps."
  :group 'bui-faces)

(defface bui-file-name
  '((t :inherit link))
  "Face used for file name buttons."
  :group 'bui-faces)

(defface bui-url
  '((t :inherit link))
  "Face used for URL buttons."
  :group 'bui-faces)

(defface bui-action-button
  '((t :inherit custom-button))
  "Face used for action buttons."
  :group 'bui-faces)

(defface bui-action-button-mouse
  '((t :inherit custom-button-mouse))
  "Mouse face used for action buttons."
  :group 'bui-faces)

(defvar bui-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "c") 'bui-button-copy-label)
    map)
  "Keymap for BUI buttons.")

(define-button-type 'bui
  'keymap bui-button-map
  'follow-link t)

(define-button-type 'bui-action
  :supertype 'bui
  'face 'bui-action-button
  'mouse-face 'bui-action-button-mouse)

(define-button-type 'bui-file
  :supertype 'bui
  'face 'bui-file-name
  'help-echo "Find file"
  'action (lambda (btn)
            (bui-find-file (or (button-get btn 'file)
                               (button-label btn)))))

(define-button-type 'bui-url
  :supertype 'bui
  'face 'bui-url
  'help-echo "Browse URL"
  'action (lambda (btn)
            (browse-url (or (button-get btn 'url)
                            (button-label btn)))))

(defun bui-button-copy-label (&optional position)
  "Copy a label of the button at POSITION into kill ring.
If POSITION is nil, use the current point position."
  (interactive)
  (--when-let (button-at (or position (point)))
    (bui-copy-as-kill (button-label it))))

(defun bui-button-type? (symbol)
  "Return non-nil, if SYMBOL is a button type."
  (and symbol
       (get symbol 'button-category-symbol)))

(defun bui-insert-button (label &optional type &rest properties)
  "Make button of TYPE with LABEL and insert it at point.
See `insert-text-button' for the meaning of PROPERTIES."
  (apply #'insert-text-button label
         :type (or type 'button)
         properties))

(defun bui-insert-action-button (label action &optional message
                                       &rest properties)
  "Make action button with LABEL and insert it at point.
ACTION is a function called when the button is pressed.  It
should accept button as the argument.
MESSAGE is a button message.
See `insert-text-button' for the meaning of PROPERTIES."
  (apply #'bui-insert-button
         label 'bui-action
         'action action
         'help-echo message
         properties))

(defun bui-buttonize (value button-type separator &rest properties)
  "Make BUTTON-TYPE button(s) from VALUE.
Return a string with button(s).

VALUE can be nil, a button name (string or symbol) or a list of
button names.  If it is a list, buttons are separated with
SEPARATOR string.

PROPERTIES are passed to `bui-insert-button'."
  (bui-get-non-nil value
    (with-temp-buffer
      (let ((labels (if (listp value) value (list value))))
        (bui-mapinsert (lambda (label)
                         (apply #'bui-insert-button
                                (if (symbolp label)
                                    (symbol-name label)
                                  label)
                                button-type properties))
                       labels
                       separator))
      (buffer-substring (point-min) (point-max)))))

(provide 'bui-button)

;;; bui-button.el ends here
