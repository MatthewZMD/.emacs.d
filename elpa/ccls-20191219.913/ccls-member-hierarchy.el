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

(defcustom ccls-member-hierarchy-qualified nil
  "Use detailed name for member hierarchy"
  :group 'ccls
  :type 'boolean)

;; ---------------------------------------------------------------------
;;   Tree node
;; ---------------------------------------------------------------------

(cl-defstruct ccls-member-hierarchy-node
  name
  field-name
  id)

(defun ccls-member-hierarchy--read-node (data &optional parent)
  "Construct a call tree node from hashmap DATA and give it the parent PARENT"
  (-let* (((&hash "location" location "numChildren" nchildren "name" name "fieldName" field-name "id" id "children" children) data)
          (filename (lsp--uri-to-path (gethash "uri" location)))
          (node
           (make-ccls-tree-node
            :location (cons filename (gethash "start" (gethash "range" location)))
            ;; With a little bit of luck, this only filters out enums
            :has-children (not (or (>= 0 nchildren)
                                   (null parent)
                                   (equal id (ccls-member-hierarchy-node-id (ccls-tree-node-data parent)))))
            :parent parent
            :expanded nil
            :children nil
            :data (make-ccls-member-hierarchy-node
                   :name name
                   :field-name field-name
                   :id id))))
    (setf (ccls-tree-node-children node)
          (--map (ccls-member-hierarchy--read-node it node)
                 children))
    node))

(defun ccls-member-hierarchy--request-children (node)
  "."
  (let ((id (ccls-member-hierarchy-node-id (ccls-tree-node-data node))))
    (--map (ccls-member-hierarchy--read-node it node)
           (gethash
            "children"
            (lsp-request "$ccls/member"
                         `(:id ,id
                               :levels ,ccls-tree-initial-levels
                               :qualified ,(if ccls-member-hierarchy-qualified t :json-false)
                               :hierarchy t
                               ))))))

(defun ccls-member-hierarchy--request-init ()
  "."
  (lsp-request
   "$ccls/member"
   `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
     :position ,(lsp--cur-position)
     :levels 1
     :qualified ,(if ccls-member-hierarchy-qualified t :json-false)
     :hierarchy t)))

(defun ccls-member-hierarchy--make-string (node depth)
  "Propertize the name of NODE with the correct properties"
  (let ((data (ccls-tree-node-data node)))
    (if (= depth 0)
        (ccls-member-hierarchy-node-name data)
      (ccls-member-hierarchy-node-field-name data))))

(defun ccls-member-hierarchy ()
  (interactive)
  (ccls-tree--open
   (make-ccls-tree-client
    :name "member hierarchy"
    :mode-line-format (propertize "Member hierarchy" 'face 'ccls-tree-mode-line-face)
    :top-line-f (lambda () (propertize "Members of" 'face 'ccls-tree-mode-line-face))
    :make-string-f 'ccls-member-hierarchy--make-string
    :read-node-f 'ccls-member-hierarchy--read-node
    :request-children-f 'ccls-member-hierarchy--request-children
    :request-init-f 'ccls-member-hierarchy--request-init)))

(provide 'ccls-member-hierarchy)
;;; ccls-member-hierarchy.el ends here
