;;; tree-mode.el --- A mode to manage tree widgets
;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Package-Version: 20151104.1331
;; Package-X-Original-Version: 1.1.1.1
;; Version: $Id: tree-mode.el,v 1.1.1.1 2007-03-13 13:16:10 ywb Exp $
;; Keywords: help, convenience, widget
;; 
;; This file is part of PDE (Perl Development Environment).
;; But it is useful for generic programming.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Dependencies:
;;  no extra libraries is required

;;; Installation:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'tree-mode)

;;; Code:

(require 'tree-widget)
(eval-when-compile
  (require 'cl))

(defvar tree-mode-version "1.0")

(defvar tree-mode-list nil)

(defvar tree-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map " " 'scroll-up)
    (define-key map "\C-?" 'scroll-down)
    (define-key map "D" 'tree-mode-delete-tree)
    (define-key map "p" 'tree-mode-previous-node)
    (define-key map "n" 'tree-mode-next-node)
    (define-key map "j" 'tree-mode-next-sib)
    (define-key map "k" 'tree-mode-previous-sib)
    (define-key map "u" 'tree-mode-goto-parent)
    (define-key map "r" 'tree-mode-goto-root)
    (define-key map "g" 'tree-mode-reflesh)
    (define-key map "E" 'tree-mode-expand-level)
    (define-key map "e" 'tree-mode-toggle-expand)
    (define-key map "s" 'tree-mode-sort-by-tag)
    (define-key map "/" 'tree-mode-keep-match)
    (define-key map "!" 'tree-mode-collapse-other-except)
    ;; (define-key map "\C-s" 'tree-mode-isearch-forward)
    ;; (define-key map "\C-r" 'tree-mode-isearch-backward)
    (dotimes (i 10)
      (define-key map `[,(+ ?0 i)] 'digit-argument))
    map))

(defvar tree-mode-menu nil)
(unless tree-mode-menu
  (easy-menu-define
    tree-mode-menu tree-mode-map "Tree menu"
    '("Tree"
      ["Next tree node" tree-mode-next-node t]
      ["Previous tree node" tree-mode-previous-node t]
      ["Next sibling node" tree-mode-next-sib t]
      ["Previous sibling node" tree-mode-previous-sib t]
      ["Goto parent node" tree-mode-goto-parent t]
      ["Goto root node" tree-mode-goto-root t]
      "--"
      ["Toggle Expand" tree-mode-toggle-expand t]
      ["Expand to level 1" (lambda () (interactive)
                             (tree-mode-expand-level 1)) t]
      ["Expand to level 2" (lambda () (interactive)
                             (tree-mode-expand-level 2)) t]
      "--"
      ["Collapse other tree" tree-mode-collapse-other-except t]
      ["Sort by tag" tree-mode-sort-by-tag t]
      ["Keep match" tree-mode-keep-match t])))

(defvar tree-mode-insert-tree-hook nil
  "Hooks run after insert a tree into buffer. Each function is
passed the new tree created")

(defvar tree-mode-delete-tree-hook nil
  "Hooks run after delete a tree in the buffer. Each function is
passed the new tree created")

(defun tree-mode-nearest-widget ()
  "Return widget at point or next nearest widget."
  (or (widget-at)
      (ignore-errors
        (let ((pos (point)))
          (widget-forward 1)
          (and (< pos (point))
               (widget-at))))))

(defun tree-mode-scan-tree ()
  "Find all tree widget in current buffer."
  (save-excursion
    (goto-char (point-min))
    (setq tree-mode-list nil)
    (let ((widget (tree-mode-nearest-widget))
          parent)
      (while widget
        (if (tree-widget-p (setq parent (widget-get widget :parent)))
            (push parent tree-mode-list))
        (goto-char (widget-get (or parent widget) :to))
        (setq widget (tree-mode-nearest-widget)))
      (setq tree-mode-list (nreverse tree-mode-list)))))

;;;###autoload 
(define-minor-mode tree-minor-mode
  "More keybindings for tree-widget.

\\{tree-mode-map}"
  :lighter " Tree"
  :keymap tree-mode-map
  (when tree-minor-mode
    (make-local-variable 'tree-mode-list)
    (tree-mode-scan-tree)))

(define-derived-mode tree-mode nil "Tree"
  "A mode to manage many tree widgets"
  (make-local-variable 'tree-mode-list)
  (make-local-variable 'tree-mode-insert-tree-hook)
  (make-local-variable 'tree-mode-delete-tree-hook)
  (widget-setup))

;; put :button-icon in push-button to setup the node icon
(add-hook 'tree-widget-before-create-icon-functions
          'tree-mode-icon-create)
(defun tree-mode-icon-create (icon)
  (let ((img (widget-get (widget-get icon :node) :button-icon)))
    (if img (widget-put icon :glyph-name img))))

(defun tree-mode-insert (tree &optional before)
  "Insert tree to buffer.
If BEFORE is non-nil and is a tree in current buffer, the new
TREE will insert at position of BEFORE."
  (if (and before (memq before tree-mode-list))
      (goto-char (widget-get before :from))
    (goto-char (point-max)))
  (setq tree (widget-create tree))
  (setq tree-mode-list (append tree-mode-list (list tree)))
  (run-hook-with-args 'tree-mode-insert-tree-hook tree)
  tree)

(defun tree-mode-delete (tree)
  "Delete tree in the buffer."
  (setq tree-mode-list (delq tree tree-mode-list))
  (widget-delete tree)
  (run-hook-with-args 'tree-mode-delete-tree-hook tree))

(defun tree-mode-tree-buffer (tree)
  "Return the buffer where the TREE is inserted"
  (marker-buffer (widget-get tree :from)))

(defun tree-mode-kill-buffer (&rest ignore)
  "If no tree in current buffer, kill this buffer."
  (if (= (length tree-mode-list) 0)
      (kill-buffer (current-buffer))))

;;{{{  Predicate and others
(defun tree-mode-root-treep (tree)
  "Test if the TREE is root"
  (and (tree-widget-p tree)
       (null (widget-get tree :parent))))

(defun tree-mode-tree-linep ()
  "If there is tree-widget in current line, return t."
  (let ((wid (tree-mode-icon-current-line)))
    (and wid (not (tree-widget-leaf-node-icon-p wid)))))

(defun tree-mode-root-linep ()
  "If the root tree node in current line, return t"
  (let ((wid (tree-mode-icon-current-line)))
    (and wid (not (tree-widget-leaf-node-icon-p wid))
         (null (widget-get (widget-get wid :parent) :parent)))))

(defun tree-mode-icon-current-line ()
  "Return the icon widget in current line"
  (save-excursion
    (forward-line 0)
    (tree-mode-nearest-widget)))

(defun tree-mode-button-current-line ()
  "Return the push button in current line."
  (save-excursion
    (let ((pos (line-beginning-position))
          but)
      (goto-char (line-end-position))
      (while (and (not but) (> (point) pos))
        (setq but (get-char-property (point) 'button))
        (backward-char 1))
      but)))

(defun tree-mode-parent-current-line ()
  "If current line is root line, return the root tree, otherwise
return the parent tree"
  (let ((wid (tree-mode-icon-current-line))
        parent)
    (when wid
      (if (tree-widget-leaf-node-icon-p wid)
          (widget-get wid :parent)
        (setq parent (widget-get (widget-get wid :parent) :parent))
        (or parent (widget-get wid :parent))))))

(defun tree-mode-widget-root (wid)
  "Return tree root of the widget WID."
  (let (parent)
    (while (setq parent (widget-get wid :parent))
      (setq wid parent))
    wid))

(defun tree-mode-tree-ap (&optional pos)
  "Return the root tree at point"
  (save-excursion
    (if pos (goto-char pos))
    (ignore-errors
      (tree-mode-widget-root (tree-mode-icon-current-line)))))
;;}}}

(defun tree-mode-opened-tree (tree)
  "Find all opened tree.
Return the tag list with the same depth."
  (if (widget-get tree :open)
      (cons (widget-get (tree-widget-node tree) :tag)
            (delq nil
                  (mapcar (lambda (child)
                            (and (tree-widget-p child)
                                 (tree-mode-opened-tree child)))
                          (widget-get tree :children))))))

(defun tree-mode-open-tree (tree path)
  "Open tree using tag list given by `tree-mode-opened-tree'."
  (when path
    (if (not (widget-get tree :open))
        (widget-apply-action tree))
    (setq path (cdr path))
    (and path
         (mapc (lambda (child)
                 (and (tree-widget-p child)
                      (let* ((tag (widget-get (tree-widget-node child) :tag))
                             (subpath (assoc tag path)))
                        (if subpath
                            (tree-mode-open-tree child subpath)))))
               (widget-get tree :children)))))

(defun tree-mode-reflesh-tree (tree)
  "Redraw TREE.
If tree has attribute :dynargs, generate new :args from that function.
Otherwise use :old-args which saved by `tree-mode-backup-args'."
  (let ((path (tree-mode-opened-tree tree)))
    (if (widget-get tree :dynargs)
        (widget-put tree :args nil)
      (if (widget-get tree :old-args)
          (widget-put tree :args (widget-get tree :old-args))))
    (widget-value-set tree (widget-value tree))
    (tree-mode-open-tree tree path)))

(defun tree-mode-reflesh-parent (widget &rest ignore)
  "Put this function to :notify property of tree-widget node."
  (tree-mode-reflesh-tree (widget-get widget :parent)))

;;{{{  Movement commands
(defun tree-mode-next-node (arg)
  "Move to next node."
  (interactive "p")
  (widget-forward (* arg 2)))

(defun tree-mode-previous-node (arg)
  (interactive "p")
  (tree-mode-next-node (- arg)))

(defun tree-mode-next-sib (arg)
  "Move to next sibling node."
  (interactive "p")
  (let (me siblings sib others out-range)
    (if (tree-mode-root-linep)
        (setq me (tree-mode-tree-ap)
              siblings tree-mode-list)
      (let ((parent (tree-mode-parent-current-line)))
        (setq me (tree-mode-button-current-line))
        (if (tree-mode-tree-linep)
            (setq me (widget-get me :parent)))
        (setq siblings (widget-get parent :children))))
    (setq others (member me siblings))
    (if (> arg 0)
        (setq sib
              (if (>= arg (length others))
                  (progn
                    (setq out-range t)
                    (car (last others)))
                (nth arg others)))
      (setq sib (- (length siblings)
                   (length others)
                   (- arg))
            out-range (< sib 0))
      (setq sib (nth (max 0 sib) siblings)))
    (goto-char (widget-get sib :from))
    (if out-range
        (message "No %s sibling more!" (if (< arg 0) "previous" "next")))))

(defun tree-mode-previous-sib (arg)
  "Move to previous sibling node."
  (interactive "p")
  (tree-mode-next-sib (- arg)))

(defun tree-mode-goto-root ()
  "Move to root node"
  (interactive)
  (let ((root (tree-mode-tree-ap)))
    (if root
        (goto-char (widget-get root :from))
      (message "No Root!"))))

(defun tree-mode-goto-parent (arg)
  "Move to parent node."
  (interactive "p")
  (let ((parent (tree-mode-parent-current-line)))
    (setq arg (1- arg))
    (if parent
        (progn
          (goto-char (widget-get parent :from))
          (while (and (> arg 0)
                      (setq parent (widget-get parent :parent))
                      (goto-char (widget-get parent :from))
                      (setq arg (1- arg)))))
      (message "No parent!"))))

(defun tree-mode-find-node (tree path)
  "Find node by path.
Return a cons cell (NODE . REST). Check the rest to find if the node
is node of the full path. 
PATH is a list of node tag to search from root.
Note if the tree is not opened, It will open some node when need.
`set-buffer' to tree buffer before call this function."
  (when (and (tree-widget-p tree) path)
    (let ((children (cdr (widget-get tree :children))) ; car is root node
          ;; if last node, both push-button and tree-widget will check
          (predicate (if (= (length path) 1) 
                         'widget-type 'tree-widget-p))
          node found)
      (while (and (not found) children)
        (setq node (car children))
        (if (and (funcall predicate node)
                 (string= (tree-mode-node-tag node) (car path)))
            (progn
              (when (cdr path)
                ;; if tree is not open, open it
                (if (and (tree-widget-p node)
                         (not (widget-get node :open)))
                    (widget-apply-action node))
                (setq found (tree-mode-find-node (car children) (cdr path))))
              (or found
                  (setq found (cons (car children) (cdr path)))))
          (setq children (cdr children))))
      found)))
;;}}}

;;{{{  Expand or collapse
(defun tree-mode-collapse-other-except ()
  "Collapse other trees. If the tree at point is contract, expand it."
  (interactive)
  (let ((me (tree-mode-icon-current-line)))
    (if (tree-widget-leaf-node-icon-p me)
        (message "Not a tree under point!")
      (setq me (widget-get me :parent))
      (unless (widget-get me :open)
        (widget-apply-action me))
      (mapc (lambda (tree)
              (if (widget-get tree :open)
                  (widget-apply-action tree)))
            (remq me (if (tree-mode-root-treep me)
                         tree-mode-list
                       (widget-get (widget-get me :parent)
                                   :children)))))))

(defun tree-mode-collapse-children (tree)
  "Collapse child node"
  (mapc (lambda (child)
          (if (widget-get child :open)
              (widget-apply-action child)))
        (widget-get tree :children)))

(defun tree-mode-expand-children (tree)
  "Expand child node"
  (mapc (lambda (child)
          (if (and (tree-widget-p child)
                   (not (widget-get child :open)))
              (widget-apply-action child)))
        (widget-get tree :children)))

(defun tree-mode-toggle-expand-node (&rest ignore)
  "Put it to :notify of tree widget node."
  (tree-mode-toggle-expand))

(defun tree-mode-toggle-expand (&optional arg)
  (interactive "P")
  (let ((me (tree-mode-icon-current-line))
        expandp open)
    (if (tree-widget-leaf-node-icon-p me)
        (message "Not a tree under point!")
      (setq me (widget-get me :parent))
      (setq expandp (widget-get me :open))
      (setq open (if (null arg)
                     (not expandp)
                   (> (prefix-numeric-value arg) 0)))
      (unless (eq open expandp)
        (widget-apply-action me)))))

(defun tree-mode-expand-level (level)
  "Expand tree to LEVEL. With prefix argument 0 or negative, will
expand all leaves of the tree."
  (interactive "p")
  (let ((me (tree-mode-icon-current-line)))
    (if (tree-widget-leaf-node-icon-p me)
        (message "Not a tree under point!")
      (setq me (widget-get me :parent))
      (tree-mode-expand-level-1 me (1- level)))))

(defun tree-mode-expand-level-1 (tree level)
  (when (tree-widget-p tree)
    (if (not (widget-get tree :open))
        (widget-apply-action tree))
    (if (= level 0)
        (tree-mode-collapse-children tree)
      (mapc (lambda (child)
              (tree-mode-expand-level-1 child (1- level)))
            (widget-get tree :children)))))
;;}}}

(defun tree-mode-node-tag (node)
  "Return tag of push-button or tree-widget"
  (or (widget-get node :tag)
      (widget-get (widget-get node :node) :tag)))

;;{{{  Commands about tree nodes
(defun tree-mode-backup-args (widget)
  "Save :args of tree-widget if need."
  (unless (and (widget-get widget :dynargs)
               (null (widget-get widget :old-args)))
    ;; if widget don't have a dynamic args function
    ;; restore args to old-args for recover
    (widget-put widget :old-args (copy-sequence (widget-get widget :args)))))

(defun tree-mode-filter-children (widget filter)
  "Remove children nodes when call FILTER with the node return true."
  (tree-mode-backup-args widget)
  (widget-put widget :args
              (delq nil (mapcar (lambda (child)
                                  (if (funcall filter child)
                                      child))
                                (widget-get widget :args))))
  (widget-value-set widget (widget-value widget)))

(defun tree-mode-sort-by-nchild (wid1 wid2)
  "Sort node by which node has children"
  (widget-get wid1 :children))

(defun tree-mode-sort-children (widget sorter)
  "Sort children nodes by SORTER."
  (tree-mode-backup-args widget)
  (widget-put widget :args
              (sort (copy-sequence (widget-get widget :args)) sorter))
  (widget-value-set widget (widget-value widget)))

(defun tree-mode-sort-by-tag (arg)
  "Sort children node by tag."
  (interactive "P")
  (let ((tree (tree-mode-parent-current-line)))
    (if tree
        (tree-mode-sort-children tree
                                 (lambda (w1 w2)
                                   (or (tree-mode-sort-by-nchild w1 w2)
                                       (string< (tree-mode-node-tag w1)
                                                (tree-mode-node-tag w2)))))
      (message "No tree at point!"))))

(defun tree-mode-delete-match (regexp)
  "Remove node which tag match REGEXP."
  (interactive "sDelete node match: ")
  (let ((tree (tree-mode-parent-current-line)))
    (if tree
        (tree-mode-filter-children
         tree
         (lambda (child) (not (string-match regexp (tree-mode-node-tag child)))))
      (message "No tree at point!"))))

(defun tree-mode-keep-match (regexp)
  "Keep node which tag match REGEXP"
  (interactive "sKeep node match: ")
  (let ((tree (tree-mode-parent-current-line)))
    (if tree
        (tree-mode-filter-children
         tree
         (lambda (child) (string-match regexp (tree-mode-node-tag child))))
      (message "No tree at point!"))))

(defun tree-mode-reflesh ()
  "Reflesh parent tree."
  (interactive)
  (let ((tree (tree-mode-parent-current-line)))
    (if tree
        (tree-mode-reflesh-tree tree)
      (message "No tree at point!"))))

(defun tree-mode-delete-tree ()
  "Delete a tree from buffer."
  (interactive)
  (if (tree-mode-root-linep)
      (if (yes-or-no-p "Delete current tree? ")
          (tree-mode-delete (tree-mode-tree-ap)))
    (message "No tree at point!")))
;;}}}

(provide 'tree-mode)
;;; tree-mode.el ends here
