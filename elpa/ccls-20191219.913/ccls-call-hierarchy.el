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

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defface ccls-call-hierarchy-node-normal-face
  nil
  "."
  :group 'ccls)

(defface ccls-call-hierarchy-node-base-face
  '((t (:foreground "orange red")))
  "."
  :group 'ccls)

(defface ccls-call-hierarchy-node-derived-face
  '((t (:foreground "orange")))
  "."
  :group 'ccls)

(defcustom ccls-call-hierarchy-qualified t
  "Use qualified name for call hierarchy."
  :group 'ccls
  :type 'boolean)

;; ---------------------------------------------------------------------
;;   Tree node
;; ---------------------------------------------------------------------

(cl-defstruct ccls-call-hierarchy-node
  id
  name
  call-type)

(defun ccls-call-hierarchy--read-node (data &optional parent)
  "Construct a call tree node from hashmap DATA and give it the parent PARENT"
  (-let* ((location (gethash "location" data))
          (filename (lsp--uri-to-path (gethash "uri" location)))
          ((&hash "id" id "name" name "callType" call-type) data))
    (make-ccls-tree-node
     :location (cons filename (gethash "start" (gethash "range" location)))
     :has-children (< 0 (gethash "numChildren" data))
     :parent parent
     :expanded nil
     :children nil
     :data (make-ccls-call-hierarchy-node
            :id id
            :name name
            :call-type call-type))))

(defun ccls-call-hierarchy--request-children (callee node)
  "."
  (let ((id (ccls-call-hierarchy-node-id (ccls-tree-node-data node))))
    (--map (ccls-call-hierarchy--read-node it node)
           (gethash "children"
                    (lsp-request
                     "$ccls/call"
                     `(:id ,id
                           :callee ,callee
                           :callType 3
                           :levels ,ccls-tree-initial-levels
                           :qualified ,(if ccls-call-hierarchy-qualified t :json-false)
                           :hierarchy t))))))

(defun ccls-call-hierarchy--request-init (callee)
  "."
  (lsp-request
   "$ccls/call"
   `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                   :position ,(lsp--cur-position)
                   :callee ,callee
                   :callType 3
                   :qualified ,(if ccls-call-hierarchy-qualified t :json-false)
                   :hierarchy t)))

(defun ccls-call-hierarchy--make-string (node depth)
  "Propertize the name of NODE with the correct properties"
  (let ((data (ccls-tree-node-data node)))
    (if (= depth 0)
        (ccls-call-hierarchy-node-name data)
      (concat
       (propertize (ccls-call-hierarchy-node-name data)
                   'face (pcase (ccls-call-hierarchy-node-call-type data)
                           ('0 'ccls-call-hierarchy-node-normal-face)
                           ('1 'ccls-call-hierarchy-node-base-face)
                           ('2 'ccls-call-hierarchy-node-derived-face)))
       (propertize (format " (%s:%s)"
                           (file-name-nondirectory (car (ccls-tree-node-location node)))
                           (gethash "line" (cdr (ccls-tree-node-location node))))
                   'face 'ccls-tree-mode-line-face)))))

(defun ccls-call-hierarchy (callee)
  (interactive "P")
  (setq callee (if callee t :json-false))
  (ccls-tree--open
   (make-ccls-tree-client
    :name "call hierarchy"
    :mode-line-format (format " %s %s %s %s"
                              (propertize (if (eq callee t) "Callee types:" "Caller types:") 'face 'ccls-tree-mode-line-face)
                              (propertize "Normal" 'face 'ccls-call-hierarchy-node-normal-face)
                              (propertize "Base" 'face 'ccls-call-hierarchy-node-base-face)
                              (propertize "Derived" 'face 'ccls-call-hierarchy-node-derived-face))
    :top-line-f (lambda () (propertize (if (eq callee t) "Callees of " "Callers of") 'face 'ccls-tree-mode-line-face))
    :make-string-f 'ccls-call-hierarchy--make-string
    :read-node-f 'ccls-call-hierarchy--read-node
    :request-children-f (apply-partially #'ccls-call-hierarchy--request-children callee)
    :request-init-f (lambda () (ccls-call-hierarchy--request-init callee)))))

(provide 'ccls-call-hierarchy)
;;; ccls-call-hierarchy.el ends here
