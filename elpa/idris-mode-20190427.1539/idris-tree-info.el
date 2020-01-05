;;; idris-info.el --- Facilities for showing Idris help information -*- lexical-binding: t -*-

;; Copyright (C) 2015  David Raymond Christiansen

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

;; This module contains facilities for showing tree-structured output
;; from Idris, because the ordinary Idris info buffers don't support
;; maintaining overlays and markers.

;;; Code:
(require 'cl-lib)
(require 'prop-menu)

(require 'idris-core)
(require 'idris-common-utils)
(require 'idris-settings)
(require 'idris-keys)
(require 'idris-warnings-tree)

(defvar idris-tree-info-buffer-name (idris-buffer-name :tree-viewer)
  "The name of the buffer that `idris-mode' uses to show general tree-structured command output.")

(defun idris-tree-info-quit ()
  "Quit the Idris tree info viewer."
  (interactive)
  (idris-kill-buffer idris-tree-info-buffer-name))

(defvar idris-tree-info-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map) ; remove the self-inserting char commands
    (define-key map (kbd "q") 'idris-tree-info-quit)
    ;;; Allow buttons to be clicked with the left mouse button in tree buffers
    (define-key map [follow-link] 'mouse-face)
    (cl-loop for keyer
             in '(idris-define-docs-keys
                  idris-define-general-keys
                  idris-define-active-term-keys)
             do (funcall keyer map))
    map))

(easy-menu-define idris-tree-info-mode-menu idris-tree-info-mode-map
  "Menu for the Idris tree viewer buffer"
  `("Idris Tree Viewer"
    ["Show term interaction widgets" idris-add-term-widgets t]
    ["Close Idris tree viewer buffer" idris-tree-info-quit t]))

(define-derived-mode idris-tree-info-mode fundamental-mode "Idris Tree"
  "Major mode used for transient Idris tree viewers
    \\{idris-tree-info-mode-map}
Invokes `idris-tree-info-mode-hook'.

This mode should be used to display tree-structured output,
because the history feature of `idris-info-mode' is incompatible
with overlays and markers, which the trees need.."
  (setq-local buffer-read-only t)
  (setq-local prop-menu-item-functions '(idris-context-menu-items)))

(defun idris-tree-info-buffer ()
  "Return the Idris tree viewer buffer, creating one if it does not exist.
Ensure that the buffer is in `idris-tree-info-mode'."
  (let ((buffer (get-buffer-create idris-tree-info-buffer-name)))
    (with-current-buffer buffer
      (when (not (eq major-mode 'idris-tree-info-mode))
        (idris-tree-info-mode)))
    buffer))

(defun idris-tree-info-buffer-visible-p ()
  "Return non-nil if the tree viewer is visible in any window."
  (if (get-buffer-window idris-tree-info-buffer-name 'visible) t nil))

(defun idris-tree-info-show-multiple (trees &optional title)
  "Show zero or more TREES in a buffer with title TITLE.

The first argument, TREES, should be an list of instances of the
struct `idris-tree'.  If non-nil, TITLE will be shown on top of
the buffer."
  (cl-assert (cl-typep trees 'list) t)
  (with-current-buffer (idris-tree-info-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when title
        (insert (propertize title 'face 'idris-info-title-face)
                "\n"))
      (dolist (tree trees)
        (cl-assert (idris-tree-p tree) t)
        (idris-tree-insert tree "")))
    (goto-char (point-min)))
  (unless (idris-tree-info-buffer-visible-p)
    (pop-to-buffer (idris-tree-info-buffer))
    (message "Press q to close the tree viewer.")))

(defun idris-tree-info-show (tree &optional title)
  "Show an instance of TREE in a buffer with title TITLE.

The first argument, `tree', should be an instance of the struct
`idris-tree'.  If non-nil, `title' will be shown on top of the
buffer."
  (cl-assert (idris-tree-p tree) t)
  (idris-tree-info-show-multiple (list tree) title))

(provide 'idris-tree-info)
;;; idris-tree-info.el ends here
