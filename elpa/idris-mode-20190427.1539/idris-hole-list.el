;;; idris-hole-list.el --- List Idris holes in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2014 David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'cl-lib)
(require 'prop-menu)

(require 'idris-core)
(require 'idris-keys)
(require 'idris-warnings-tree)
(require 'idris-settings)

(defvar idris-hole-list-buffer-name (idris-buffer-name :holes)
  "The name of the buffer containing Idris holes")

(defun idris-hole-list-quit ()
  "Quit the Idris hole list"
  (interactive)
  (idris-kill-buffer idris-hole-list-buffer-name))

(defvar idris-hole-list-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "q") 'idris-hole-list-quit)
    (define-key map (kbd "RET") 'idris-compiler-notes-default-action-or-show-details)
    (define-key map (kbd "<mouse-2>") 'idris-compiler-notes-default-action-or-show-details/mouse)
    ;;; Allow buttons to be clicked with the left mouse button in the hole list
    (define-key map [follow-link] 'mouse-face)
    (cl-loop for keyer
             in '(idris-define-docs-keys
                  idris-define-general-keys
                  idris-define-active-term-keys)
             do (funcall keyer map))
    map))

(easy-menu-define idris-hole-list-mode-menu idris-hole-list-mode-map
  "Menu for the Idris hole list buffer"
  `("Idris Holes"
    ["Show term interaction widgets" idris-add-term-widgets t]
    ["Close hole list buffer" idris-hole-list-quit t]
    "------------------"
    ["Customize idris-hole-list-mode" (customize-group 'idris-hole-list) t]
    ["Customize fonts and colors" (customize-group 'idris-faces) t]))

(define-derived-mode idris-hole-list-mode fundamental-mode "Idris Holes"
  "Major mode used for transient Idris hole list buffers
   \\{idris-hole-list-mode-map}
Invoces `idris-hole-list-mode-hook'."
  (setq-local prop-menu-item-functions '(idris-context-menu-items)))

(defun idris-hole-list-buffer ()
  "Return the Idris hole buffer, creating one if there is not one"
  (get-buffer-create idris-hole-list-buffer-name))

(defun idris-hole-list-buffer-visible-p ()
  (if (get-buffer-window idris-hole-list-buffer-name 'visible) t nil))

(defun idris-hole-list-show (hole-info)
  (if (null hole-info)
      (progn (message "No holes found!")
             (idris-hole-list-quit))
    (with-current-buffer (idris-hole-list-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (idris-hole-list-mode)
      (insert (propertize "Holes" 'face 'idris-info-title-face) "\n\n")
      (when idris-show-help-text
        (insert "This buffer displays the unsolved holes from the currently-loaded code. ")
        (insert (concat "Press the "
                        (if idris-enable-elab-prover "[E]" "[P]")
                        " buttons to solve the holes interactively in the prover."))
        (let ((fill-column 80))
          (fill-region (point-min) (point-max)))
        (insert "\n\n"))

      (dolist (tree (mapcar #'idris-tree-for-hole hole-info))
        (idris-tree-insert tree "")
        (insert "\n\n"))
      (message "Press q to close")
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer (idris-hole-list-buffer))))

(defun idris-hole-tree-printer (tree)
  "Print TREE, formatted for holes."
  (idris-propertize-spans (idris-repl-semantic-text-props (idris-tree.highlighting tree))
    (insert (idris-tree.item tree)))
  (when (idris-tree.button tree)
    (insert " ")
    (apply #'insert-button (idris-tree.button tree))
    (insert (idris-tree.after-button tree))))


;;; Prevent circularity error
(autoload 'idris-prove-hole "idris-commands.el")

(defun idris-tree-for-hole (hole)
  "Generate a tree for HOLE.

HOLE should be a three-element list consisting of the
hole name, its premises, and its conclusion."
  (cl-destructuring-bind (name premises conclusion) hole
    (make-idris-tree :item name
                     :button (if idris-enable-elab-prover
                                 `("[E]"
                                   help-echo "Elaborate interactively"
                                   action ,#'(lambda (_)
                                               (interactive)
                                               (idris-prove-hole name t)))
                               `("[P]"
                                 help-echo "Open in prover"
                                 action ,#'(lambda (_)
                                             (interactive)
                                             (idris-prove-hole name))))
                     :highlighting `((0 ,(length name) ((:decor :metavar))))
                     :print-fn #'idris-hole-tree-printer
                     :collapsed-p (not idris-hole-list-show-expanded) ; from customize
                     :preserve-properties '(idris-tt-term)
                     :kids (list (idris-tree-for-hole-details name premises conclusion)))))

(defun idris-tree-for-hole-details (name premises conclusion)
  (let* ((name-width (1+ (apply #'max 0 (length name)
                                (mapcar #'(lambda (h) (length (car h)))
                                        premises))))
         (divider-marker nil)
         (contents (with-temp-buffer
                     (dolist (h premises)
                       (cl-destructuring-bind (name type formatting) h
                         (cl-dotimes (_ (- name-width (length name))) (insert " "))
                         (idris-propertize-spans (idris-repl-semantic-text-props
                                                  `((0 ,(length name) ((:decor :bound)))))
                           (insert name))
                         (insert " : ")
                         (let ((start (point)))
                           (idris-propertize-spans (idris-repl-semantic-text-props formatting)
                             (insert type))
                           (insert "\n")
                           ;; Indent the term to match the tree and
                           ;; its binder, if it is more than one line.
                           (let ((term-end-marker (copy-marker (point))))
                             (beginning-of-line)
                             (forward-line -1)
                             (while (< start (point))
                               ;; Preserve the term annotation, to not break active terms
                               (let ((tm (get-text-property (point) 'idris-tt-term)))
                                 (insert-before-markers
                                  (propertize (make-string (+ 3 name-width) ? )
                                              'idris-tt-term tm)))
                               (forward-line -1))
                             (goto-char term-end-marker)))))
                     (setq divider-marker (point-marker))
                     (cl-destructuring-bind (type formatting) conclusion
                       (when premises
                         (insert " ")
                         (idris-propertize-spans (idris-repl-semantic-text-props
                                                  `((0 ,(length name) ((:decor :metavar)))))
                           (insert name))
                         (insert " : "))
                       (idris-propertize-spans (idris-repl-semantic-text-props formatting)
                         (insert type)))
                     (when premises
                       (let ((width (apply #'max 0
                                           (mapcar #'length
                                                   (split-string (buffer-string) "\n")))))
                         (goto-char (marker-position divider-marker))
                         (dotimes (_ (1+ width)) (insert "-"))
                         (insert "\n")))
                     (buffer-string))))
    (make-idris-tree :item contents
                     :active-p nil
                     :highlighting '()
                     :preserve-properties '(idris-tt-term))))


(provide 'idris-hole-list)
