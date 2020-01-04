;;; graphql.el --- GraphQL utilities                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: hypermedia, tools, lisp
;; Homepage: https://github.com/vermiculus/graphql.el
;; Package-Version: 20180912.31
;; Package-X-Original-Version: 0.1.1
;; Package-Requires: ((emacs "25"))

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

;; GraphQL.el provides a generally-applicable domain-specific language
;; for creating and executing GraphQL queries against your favorite
;; web services.

;;; Code:

(require 'pcase)

(defun graphql--encode-object (obj)
  "Encode OBJ as a GraphQL string."
  (cond
   ((stringp obj)
    obj)
   ((symbolp obj)
    (symbol-name obj))
   ((numberp obj)
    (number-to-string obj))
   ((and (consp obj)
         (not (consp (cdr obj))))
    (symbol-name (car obj)))))

(defun graphql--encode-argument-spec (spec)
  "Encode an argument spec SPEC.
SPEC is of the form..."
  (graphql--encode-argument (car spec) (cdr spec)))

(defun graphql--encode-argument (key value)
  "Encode an argument KEY with value VALUE."
  (format "%s:%s" key (graphql--encode-argument-value value)))

(defun graphql--encode-argument-value (value)
  "Encode an argument value VALUE.
VALUE is expected to be one of the following:

* a symbol
* a 'variable', i.e. \\='($ variableName)
* an object (as a list)
* a string
* a vector of values (e.g., symbols)
* a number
* something encode-able by `graphql-encode'."
  (cond
   ((symbolp value)
    (symbol-name value))
   ((eq '$ (car-safe value))
    (format "$%s" (cadr value)))
   ((listp value)
    (format "{%s}" (mapconcat #'graphql--encode-argument-spec value ",")))
   ((stringp value)
    (format "\"%s\"" value))
   ((vectorp value)
    (format "[%s]" (mapconcat #'graphql-encode value ",")))
   ((numberp value)
    (number-to-string value))
   (t
    (graphql-encode value))))

(defun graphql--encode-parameter-spec (spec)
  "Encode a parameter SPEC.
SPEC is expected to be of the following form:

   (NAME TYPE [REQUIRED] . [DEFAULT])

NAME is the name of the parameter.

TYPE is the parameter's type.

A non-nil value for REQUIRED will indicate the parameter is
required.  A value of `!' is recommended.

A non-nil value for DEFAULT will provide a default value for the
parameter."
  ;; Unfortunately can't use `pcase' here because the first DEFAULT
  ;; value (in the case of a complex value) might be misunderstood as
  ;; the value for REQUIRED.  We need to know if the third cons is the
  ;; very last one; not just that the list has at least three
  ;; elements.
  (if (eq (last spec) (nthcdr 2 spec))
      (graphql--encode-parameter (nth 0 spec)
                                 (nth 1 spec)
                                 (car (last spec))
                                 (cdr (last spec)))
    (graphql--encode-parameter (nth 0 spec)
                               (nth 1 spec)
                               nil
                               (nthcdr 2 spec))))

(defun graphql--encode-parameter (name type &optional required default)
  "Encode a GraphQL parameter with a NAME and TYPE.
If REQUIRED is non-nil, mark the parameter as required.
If DEFAULT is non-nil, is the default value of the parameter."
  (format "$%s:%s%s%s"
          (symbol-name name)
          (symbol-name type)
          (if required "!" "")
          (if default
              (concat "=" (graphql--encode-argument-value default))
            "")))

(defun graphql--get-keys (g)
  "Get the keyword arguments from a graph G.
Returns a list where the first element is a plist of arguments
and the second is a 'clean' copy of G."
  (or (and (not (consp g))
           (list nil g))
      (let (graph keys)
        (while g
          (if (keywordp (car g))
              (let* ((param (pop g))
                     (value (pop g)))
                (push (cons param value) keys))
            (push (pop g) graph)))
        (list keys (nreverse graph)))))

(defun graphql-encode (g)
  "Encode graph G as a GraphQL string."
  (pcase (graphql--get-keys g)
    (`(,keys ,graph)
     (let ((object (or (car-safe graph) graph))
           (name (alist-get :op-name keys))
           (params (alist-get :op-params keys))
           (arguments (alist-get :arguments keys))
           (fields (cdr-safe graph)))
       (concat
        (graphql--encode-object object)
        (when name
          (format " %S" name))
        (when arguments
          ;; Format arguments "key:value,key:value,..."
          (format "(%s)"
                  (mapconcat #'graphql--encode-argument-spec arguments ",")))
        (when params
          (format "(%s)"
                  (mapconcat #'graphql--encode-parameter-spec params ",")))
        (when fields
          (format "{%s}"
                  (mapconcat #'graphql-encode fields " "))))))))

(defun graphql-simplify-response-edges (data)
  "Simplify DATA to collapse edges into their nodes."
  (pcase data
    ;; When we encounter a collection of edges, simplify those edges
    ;; into their nodes
    (`(,object (edges . ,edges))
     (cons object (mapcar #'graphql-simplify-response-edges
                          (mapcar (lambda (edge) (alist-get 'node edge))
                                  edges))))
    ;; When we encounter a plain cons cell (not a list), let it pass
    (`(,(and key (guard (not (consp key)))) . ,(and value (guard (not (consp value)))))
     (cons key value))
    ;; symbols should pass unaltered
    (`,(and symbol (guard (symbolp symbol)))
     symbol)
    ;; everything else should be mapped
    (_ (mapcar #'graphql-simplify-response-edges data))))

(defun graphql--genform-operation (args kind)
  "Generate the Lisp form for an operation.
ARGS is is a list ([NAME [PARAMETERS]] GRAPH) where NAME is the
name of the operation, PARAMETERS are its parameters, and GRAPH
is the form of the actual operation.

KIND can be `query' or `mutation'."
  (pcase args
    (`(,name ,parameters ,graph)
     `(graphql-encode '(,kind :op-name ,name
                              :op-params ,parameters
                              ,@graph)))

    (`(,name ,graph)
     `(graphql-encode '(,kind :op-name ,name
                              ,@graph)))

    (`(,graph)
     `(graphql-encode '(,kind ,@graph)))

    (_ (error "Bad form"))))

(defmacro graphql-query (&rest args)
  "Construct a Query object.
ARGS is a listof the form described by `graphql--genform-operation'.

\(fn [NAME] [(PARAMETER-SPEC...)] GRAPH)"
  (graphql--genform-operation args 'query))

(defmacro graphql-mutation (&rest args)
  "Construct a Mutation object.
ARGS is a listof the form described by `graphql--genform-operation'.

\(fn [NAME] [(PARAMETER-SPEC...)] GRAPH)"
  (graphql--genform-operation args 'mutation))

(provide 'graphql)
;;; graphql.el ends here
