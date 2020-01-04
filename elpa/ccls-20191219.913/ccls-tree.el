;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani
;; Copyright (C) 2018 Fangrui Song

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'ccls-common)

(require 'xref)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defgroup ccls-tree nil
  "ccls tree."
  :group 'tools
  :group 'ccls)

(defcustom ccls-tree-initial-levels 2
  "."
  :type 'integer
  :group 'ccls-tree)

(defface ccls-tree-root-face
  '((t (:height 1.5 :line-height 2.0)))
  "."
  :group 'ccls-tree)

(defface ccls-tree-mouse-face
  '((t (:background "green")))
  "."
  :group 'ccls-tree)

(defface ccls-tree-icon-face
  '((t (:foreground "grey")))
  "."
  :group 'ccls-tree)

(defface ccls-tree-header-line-face
  '((t (:foreground "grey" :height 0.8)))
  "."
  :group 'ccls-tree)

(defface ccls-tree-mode-line-face
  '((t (:foreground "grey" :slant italic)))
  "."
  :group 'ccls-tree)

;; ---------------------------------------------------------------------
;;   Tree node
;; ---------------------------------------------------------------------

(cl-defstruct ccls-tree-node
  parent ;; parent node
  has-children ;; whether this node has children
  children ;; a list of child nodes, if already requested
  expanded ;; is this node expanded?
  location ;; (filename . (hashmap '(("line" . line) ("char" . char))). Location to go to on click
  data ;; Arbitrary, client defined data
  )

(cl-defstruct ccls-tree-client
  name ;; name of client
  mode-line-format ;; mode-line format
  header-line-format ;; header-line format
  top-line-f ;; Function to draw in front of the first line
  make-string-f ;; Function to get the string for a node. form: (node depth)
  read-node-f ;; Function to read a node from a hashmap. form: (hashmap &optional parent)
  request-children-f ;; Function to request children for a node. form: (node)
  request-init-f ;; Function to request initial nodes. form: ()
  )

(defvar-local ccls-tree--cur-client nil
  "Buffer tree client.")

(defvar-local ccls-tree-calling t
  "When non-nil, visit the node when the selected line changes.")

(defun ccls-tree--read-node (data &optional parent)
  (funcall (ccls-tree-client-read-node-f ccls-tree--cur-client) data parent))

(defun ccls-tree--make-string (node depth)
  "Propertize the name of NODE with the correct properties"
  (funcall (ccls-tree-client-make-string-f ccls-tree--cur-client) node depth))

(defun ccls-tree-node--request-children (node)
  (funcall (ccls-tree-client-request-children-f ccls-tree--cur-client) node))

(defun ccls-tree--request-init (client)
  (funcall (ccls-tree-client-request-init-f client)))

(defun ccls-tree--draw-top-line ()
  (-some-> (ccls-tree-client-top-line-f ccls-tree--cur-client)
           (funcall)
           (concat "\n")
           (insert)))

;; ---------------------------------------------------------------------
;;   Visualization
;; ---------------------------------------------------------------------

(defvar-local ccls-tree--root-nodes nil
  ".")

(defvar-local ccls-tree--visible-root nil
  ".")

(defvar-local ccls-tree--origin-buffer nil
  "Buffer that the tree was opened from.")
(defvar-local ccls-tree--origin-win nil
  "Win that the tree was opened from.")

(defvar-local ccls-tree--opoint 1
  "The original point of the buffer that the tree was opened from.")

(defun ccls-tree--refresh ()
  (let ((p (point))
        (inhibit-read-only t))
    (erase-buffer)
    (setf (ccls-tree-node-expanded ccls-tree--visible-root) t)
    (ccls-tree--draw-top-line)
    (ccls-tree--insert-node ccls-tree--visible-root 0 1 0)
    (goto-char p)))

(defun ccls-tree--insert-node (node number nchildren depth)
  (let* ((prefix (ccls-tree--make-prefix node number nchildren depth))
         (name (ccls-tree--make-string node depth)))
    (insert (if (= depth 0)
                (propertize (concat prefix name "\n")
                            'depth depth
                            'face 'ccls-tree-root-face
                            'ccls-tree-node node)
              (propertize (concat prefix name "\n")
                          'depth depth
                          'ccls-tree-node node)))
    (when (or (ccls-tree-node-expanded node))
      (when (and (ccls-tree-node-has-children node)
                 (null (ccls-tree-node-children node)))
        (setf (ccls-tree-node-children node)
              (ccls-tree-node--request-children node)))
      (let ((children (ccls-tree-node-children node)))
        (--map-indexed (ccls-tree--insert-node it it-index (length children) (+ depth 1))
                       children)))))

(defun ccls-tree--make-prefix (node number nchildren depth)
  "."
  (let* ((padding (if (= depth 0) "" (make-string (* 2 (- depth 1)) ?\ )))
         (symbol (if (= depth 0)
                     (if (ccls-tree-node-parent node)
                         "◀ "
                       "")
                   (if (ccls-tree-node-has-children node)
                       (if (ccls-tree-node-expanded node) "▶ " "▼ ")
                     (if (eq number (- nchildren 1)) "└╸" "├╸")))))
    (concat padding (propertize symbol 'face 'ccls-tree-icon-face))))

(defun ccls-tree--expand-levels (node levels)
  "Expand NODE and its children LEVELS down"
  (when (> levels 0)
    (setf (ccls-tree-node-expanded node) t)
    (--map (ccls-tree--expand-levels it (- levels 1))
           (ccls-tree-node-children node))))

(defun ccls-tree--open (client)
  "."
  (let ((opoint (point))
        (lsp-ws lsp--buffer-workspaces)
        (root-node-data (ccls-tree--request-init client))
        (orig-buf (current-buffer))
        (bufname (format "*ccls-tree %s*" (ccls-tree-client-name client))))
    (with-current-buffer (get-buffer-create bufname)
      (ccls-tree-mode)
      (setq lsp--buffer-workspaces lsp-ws
            ccls-tree--cur-client client
            ccls-tree--origin-buffer orig-buf
            ccls-tree--origin-win (get-buffer-window orig-buf)
            ccls-tree--opoint opoint
            ccls-tree--root-nodes (when root-node-data (ccls-tree--read-node root-node-data))
            ccls-tree--visible-root ccls-tree--root-nodes)
      (when (null ccls-tree--root-nodes)
        (user-error "Couldn't open tree from point"))
      (ccls-tree--refresh)
      (ccls-tree--expand-levels ccls-tree--visible-root ccls-tree-initial-levels)
      (ccls-tree--refresh)
      (setq header-line-format (ccls-tree-client-header-line-format ccls-tree--cur-client))
      (setq mode-line-format (ccls-tree-client-mode-line-format ccls-tree--cur-client))
      (goto-char 1)
      (forward-line))
    (let ((win (display-buffer-in-side-window (get-buffer bufname) '((side . right)))))
      (set-window-margins win 1)
      (select-window win)
      (set-window-start win 1)
      (set-window-dedicated-p win t))))

(defun ccls-tree--node-at-point ()
  (get-text-property (point) 'ccls-tree-node))

(defun ccls-tree--depth-at-point ()
  (get-text-property (point) 'depth))

;; ---------------------------------------------------------------------
;;   Actions
;; ---------------------------------------------------------------------

(defun ccls-tree-toggle-expand ()
  "Toggle expansion of node at point"
  (interactive)
  (-when-let* ((node (ccls-tree--node-at-point)))
    (setf (ccls-tree-node-expanded node)
          (or (not (ccls-tree-node-expanded node))
              (= 0 (ccls-tree--depth-at-point))))
    (ccls-tree--refresh)))

(defun ccls-tree-select-parent ()
  "."
  (interactive)
  (let ((depth (ccls-tree--depth-at-point)))
    (if (null depth)
        (forward-line -1)
      (if (> depth 0)
          (while (and (>= (ccls-tree--depth-at-point) depth)
                      (= 0 (forward-line -1))))
        (-when-let* ((parent (ccls-tree-node-parent (ccls-tree--node-at-point))))
          (setq ccls-tree--visible-root parent)
          (ccls-tree--refresh))))))

(defun ccls-tree-set-root ()
  "Set root to current node"
  (interactive)
  (-when-let* ((node (ccls-tree--node-at-point)))
    (when (ccls-tree-node-has-children node)
      (setq ccls-tree--visible-root node)
      (setf (ccls-tree-node-expanded node) t)
      (ccls-tree--refresh))))

(defun ccls-tree-toggle-calling ()
  "Toggle `ccls-tree-calling'."
  (interactive)
  (when (setq ccls-tree-calling (not ccls-tree-calling))
    (ccls-tree-press)))

(defun ccls-tree-press (&optional split-fn)
  "Jump to the location."
  (interactive)
  (-when-let* ((workspaces lsp--buffer-workspaces)
               (node (ccls-tree--node-at-point))
               (unused (window-live-p ccls-tree--origin-win)))
    (with-selected-window ccls-tree--origin-win
      (when split-fn
        (funcall split-fn))
      (find-file (car (ccls-tree-node-location node)))
      ;; TODO Extract lsp-ui-peek.el lsp-ui-peek--goto-xref
      (unless lsp--buffer-workspaces
        (setq lsp--buffer-workspaces workspaces)
        (lsp-mode 1)
        (dolist (workspace cur-buffer-workspaces)
          (lsp--open-in-workspace workspace)))
      (goto-char (lsp--position-to-point (cdr (ccls-tree-node-location node))))
      (recenter)
      (run-hooks 'xref-after-jump-hook))))

(defun ccls-tree-press-and-switch ()
  "Switch window and jump to the location."
  (interactive)
  (ccls-tree-press)
  (when (window-live-p ccls-tree--origin-win)
    (select-window ccls-tree--origin-win)))

(defun ccls-tree-press-and-horizontal-split ()
  "Split window horizontally and jump to the location."
  (interactive)
  (ccls-tree-press #'split-window-horizontally)
  (when (window-live-p ccls-tree--origin-win)
    (select-window ccls-tree--origin-win)))

(defun ccls-tree-press-and-vertical-split ()
  "Split window vertically and jump to the location."
  (interactive)
  (ccls-tree-press #'split-window-vertically)
  (when (window-live-p ccls-tree--origin-win)
    (select-window ccls-tree--origin-win)))

(defun ccls-tree-next-line (&optional arg)
  (interactive "p")
  (forward-line arg)
  (when ccls-tree-calling
    (ccls-tree-press)))

(defun ccls-tree-prev-line (&optional arg)
  (interactive "p")
  (forward-line (- arg))
  (when ccls-tree-calling
    (ccls-tree-press)))

(defun ccls-tree-next-sibling (&optional _arg)
  (interactive "p")
  (-when-let* ((depth (ccls-tree--depth-at-point)))
    (while (and (forward-line 1)
                (< depth (or (ccls-tree--depth-at-point) 0))))
    (when ccls-tree-calling
      (ccls-tree-press))))

(defun ccls-tree-prev-sibling (&optional _arg)
  (interactive "p")
  (-when-let* ((depth (ccls-tree--depth-at-point)))
    (while (and (forward-line -1)
                (< depth (or (ccls-tree--depth-at-point) 0))))
    (when ccls-tree-calling
      (ccls-tree-press))))

(defun ccls-tree-expand-or-set-root ()
  "If the node at point is unexpanded expand it, otherwise set it as root"
  (interactive)
  (-when-let* ((node (ccls-tree--node-at-point)))
    (when (ccls-tree-node-has-children node)
      (if (ccls-tree-node-expanded node)
          (ccls-tree-set-root)
        (ccls-tree-toggle-expand)))))

(defun ccls-tree-collapse-or-select-parent ()
  "If the node at point is expanded collapse it, otherwise select its parent."
  (interactive)
  (-when-let* ((node (ccls-tree--node-at-point)))
    (if (and (> (ccls-tree--depth-at-point) 0)
             (ccls-tree-node-expanded node))
        (ccls-tree-toggle-expand)
      (ccls-tree-select-parent))))

(defun ccls-tree-quit ()
  (interactive)
  (-when-let* ((buf ccls-tree--origin-buffer)
               (opoint ccls-tree--opoint)
               (unused (window-live-p ccls-tree--origin-win)))
    (with-selected-window ccls-tree--origin-win
      (switch-to-buffer buf)
      (goto-char opoint)))
  (quit-window))

(defun ccls-tree-yank-path ()
  (interactive)
  (--if-let (-some-> (ccls-tree--node-at-point) (ccls-tree-node-location) (car) (kill-new))
      (message (format "Yanked path: %s" (propertize it 'face 'font-lock-string-face)))
    (user-error "There is nothing to copy here")))

;; ---------------------------------------------------------------------
;;   Mode
;; ---------------------------------------------------------------------

(defvar ccls-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'ccls-tree-toggle-expand)
    (define-key map [mouse-1] #'ccls-tree-toggle-expand)
    (define-key map (kbd "c") #'ccls-tree-toggle-calling)
    (define-key map (kbd "f") #'ccls-tree-press)
    (define-key map (kbd "h") #'ccls-tree-collapse-or-select-parent)
    (define-key map (kbd "j") #'ccls-tree-next-line)
    (define-key map (kbd "k") #'ccls-tree-prev-line)
    (define-key map (kbd "J") #'ccls-tree-next-sibling)
    (define-key map (kbd "K") #'ccls-tree-prev-sibling)
    (define-key map (kbd "l") #'ccls-tree-expand-or-set-root)
    (define-key map (kbd "oh") #'ccls-tree-press-and-horizontal-split)
    (define-key map (kbd "ov") #'ccls-tree-press-and-vertical-split)
    (define-key map (kbd "oo") #'ccls-tree-press-and-switch)
    (define-key map (kbd "q") #'ccls-tree-quit)
    (define-key map (kbd "Q") #'quit-window)
    (define-key map (kbd "yy") #'ccls-tree-yank-path)
    (define-key map (kbd "RET") #'ccls-tree-press-and-switch)
    (define-key map (kbd "<left>") #'ccls-tree-collapse-or-select-parent)
    (define-key map (kbd "<right>") #'ccls-tree-expand-or-set-root)
    map)
  "Keymap for `ccls-tree-mode'.")

(define-derived-mode ccls-tree-mode special-mode "ccls-tree"
  "Mode for ccls tree buffers")

(provide 'ccls-tree)
