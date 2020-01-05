;;; idris-info.el --- Facilities for showing Idris help information -*- lexical-binding: t -*-

;; Copyright (C) 2014  David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: languages, tools

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

;; This module contains facilities for showing information provided by the
;; Idris compiler in a separate buffer, as well as keeping the irritation of
;; that buffer to a minimum.

;;; Code:
(require 'prop-menu)
(require 'idris-core)
(require 'idris-common-utils)


(defvar idris-info-history (list () nil ())
  "A zipper into the history for idris-info-mode.
It is a three-element list whose first element is the history,
whose second element is the current item if applicable or NIL
otherwise, and whose third element is the future.")

(defun idris-info-history-clear ()
  "Reset the history for Idris info buffers."
  (setq idris-info-history (list () nil ())))

(defun idris-info-history-insert (contents)
  "Insert CONTENTS into the Idris info history as the current node.
Following the behavior of Emacs help buffers, the future is deleted."
  (pcase-let ((`(,past ,present ,_future) idris-info-history))
    (setq idris-info-history
          (if present
              (list (cons present past) contents ())
            (list past contents ())))))

(defun idris-info-history-back ()
  "Move back in the Idris info history."
  (setq idris-info-history
        (pcase idris-info-history
          (`((,prev . ,past) ,present ,future)
           (list past prev (cons present future)))
          (`(() ,present ,future) (list () present future)))))

(defun idris-info-history-forward ()
  "Move forward in the Idris info history."
  (setq idris-info-history
        (pcase idris-info-history
          (`(,past ,present (,next . ,future))
           (list (cons present past) next future))
          (`(,past ,present ()) (list past present ())))))

(defvar idris-info-buffer-name (idris-buffer-name :info)
  "The name of the buffer containing Idris help information")

(defvar idris-info-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map) ; remove the self-inserting char commands
    (define-key map (kbd "q") 'idris-info-quit)
    ;;; Allow buttons to be clicked with the left mouse button in info buffers
    (define-key map [follow-link] 'mouse-face)
    (cl-loop for keyer
             in '(idris-define-docs-keys
                  idris-define-general-keys
                  idris-define-active-term-keys)
             do (funcall keyer map))
    map))

(easy-menu-define idris-info-mode-menu idris-info-mode-map
  "Menu for the Idris info buffer"
  `("Idris Info"
    ["Show term interaction widgets" idris-add-term-widgets t]
    ["Close Idris info buffer" idris-info-quit t]))

(define-derived-mode idris-info-mode fundamental-mode "Idris Info"
  "Major mode used for transient Idris information buffers
    \\{idris-info-mode-map}
Invokes `idris-info-mode-hook'."
  (set (make-local-variable 'prop-menu-item-functions) '(idris-context-menu-items)))
; if we use view-mode here, our key binding q would be shadowed.

(defun idris-info-buffer ()
  "Return the Idris info buffer, creating one if there is not one.
Ensure that the buffer is in `idris-info-mode'."
  (let ((buffer (get-buffer-create idris-info-buffer-name)))
    (with-current-buffer buffer
      (when (not (eq major-mode 'idris-info-mode))
        (idris-info-mode)))
    buffer))

(defun idris-info-quit ()
  (interactive)
  (idris-kill-buffer idris-info-buffer-name))

(defun idris-info-buffer-visible-p ()
  (if (get-buffer-window idris-info-buffer-name 'visible) t nil))

(defun idris-info-show ()
  "Show the Idris info buffer."
  (interactive)
  (with-current-buffer (idris-info-buffer)
    (setq buffer-read-only t)
    (pcase-let ((inhibit-read-only t)
                (`(,past ,present ,future) idris-info-history))
      (erase-buffer)
      (when present
        (insert present)
        (insert "\n\n"))
      (when past
        (insert-button "[back]" 'action #'(lambda (_) (interactive) (idris-info-history-back) (idris-info-show))))
      (when (and past future) (insert "\t"))
      (when future
        (insert-button "[forward]" 'action #'(lambda (_) (interactive) (idris-info-history-forward) (idris-info-show))))
      (when (or past future) (newline))
      (goto-char (point-min))))
  (unless (idris-info-buffer-visible-p)
    (pop-to-buffer (idris-info-buffer))
    (message "Press q to close the Idris info buffer.")))

(defmacro with-idris-info-buffer (&rest cmds)
  "Execute `CMDS' in a fresh Idris info buffer, then display it to the user."
  (declare (indent defun))
  (let ((str-info (cl-gensym "STR-INFO")))
    `(let ((,str-info (with-temp-buffer
                        ,@cmds
                        (buffer-string))))
       (idris-info-history-insert ,str-info)
       (idris-info-show))))


(defun idris-show-info (info-string &optional spans)
  "Show INFO-STRING in the Idris info buffer, obliterating its previous contents."
  (with-idris-info-buffer
    (idris-propertize-spans (idris-repl-semantic-text-props spans)
      (insert info-string)))
  info-string)



(provide 'idris-info)
;;; idris-info.el ends here
