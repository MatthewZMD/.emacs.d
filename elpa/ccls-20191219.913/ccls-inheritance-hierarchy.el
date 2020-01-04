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
(require 'ccls-tree)


(defface ccls-inheritance-hierarchy-base-face
  '((t (:foreground "orange red")))
  "."
  :group 'ccls)

(defcustom ccls-inheritance-hierarchy-qualified t
  "Use qualified name for types in inheritance hierarchy."
  :group 'ccls
  :type 'boolean)

(cl-defstruct ccls-inheritance-hierarchy-node
  id
  kind
  name)

(defun ccls-inheritance-hierarchy--read-node (data &optional parent)
  "Construct a call tree node from hashmap DATA and give it the parent PARENT"
  (-let* ((location (gethash "location" data '(nil . nil)))
          (filename (lsp--uri-to-path (gethash "uri" location)))
          ((&hash "id" id "kind" kind "name" name) data)
          (node
           (make-ccls-tree-node
            :location (cons filename (gethash "start" (gethash "range" location)))
            :has-children (< 0 (gethash "numChildren" data))
            :parent parent
            :expanded nil
            :children nil
            :data (make-ccls-inheritance-hierarchy-node
                   :id id
                   :kind kind
                   :name name))))
    (setf (ccls-tree-node-children node)
          (--map (ccls-inheritance-hierarchy--read-node it node)
                 (gethash "children" data)))
    node))

(defun ccls-inheritance-hierarchy--request-children (derived node)
  "."
  (let ((id (ccls-inheritance-hierarchy-node-id (ccls-tree-node-data node)))
        (kind (ccls-inheritance-hierarchy-node-kind (ccls-tree-node-data node))))
    (--map (ccls-inheritance-hierarchy--read-node it node)
           (gethash "children"
                    (lsp-request
                     "$ccls/inheritance"
                     `(:id ,id :kind ,kind
                           :derived ,derived
                           :qualified ,(if ccls-inheritance-hierarchy-qualified t :json-false)
                           :levels ,ccls-tree-initial-levels
                           :hierarchy t
                           ))))))

(defun ccls-inheritance-hierarchy--request-init (derived)
  "."
  (lsp-request
   "$ccls/inheritance"
   `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
     :position ,(lsp--cur-position)

     :derived ,derived
     :qualified ,(if ccls-inheritance-hierarchy-qualified t :json-false)
     :levels 1
     :hierarchy t)))

(defun ccls-inheritance-hierarchy--make-string (node _depth)
  "Propertize the name of NODE with the correct properties"
  (let* ((data (ccls-tree-node-data node))
         (name (ccls-inheritance-hierarchy-node-name data)))
    (if (string-equal name "[[Base]]")
        (propertize "Bases" 'face 'ccls-inheritance-hierarchy-base-face)
      name)))

(defun ccls-inheritance-hierarchy (derived)
  (interactive "P")
  (let ((json-derived (if derived t :json-false)))
    (ccls-tree--open
     (make-ccls-tree-client
      :name "inheritance hierarchy"
      :mode-line-format (propertize (if derived
                                        "Inheritance Hierarchy: Subclasses"
                                      "Inheritance Hierarchy: Bases")
                                    'face 'ccls-tree-mode-line-face)
      :top-line-f (lambda () (propertize (if derived "Derive from" "Bases of") 'face 'ccls-tree-mode-line-face))
      :make-string-f 'ccls-inheritance-hierarchy--make-string
      :read-node-f 'ccls-inheritance-hierarchy--read-node
      :request-children-f (apply-partially #'ccls-inheritance-hierarchy--request-children json-derived)
      :request-init-f (lambda () (ccls-inheritance-hierarchy--request-init json-derived))))))

(provide 'ccls-inheritance-hierarchy)
;;; ccls-inheritance-hierarchy.el ends here
