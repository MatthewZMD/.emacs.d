;;; bui.el --- Buffer interface library  -*- lexical-binding: t -*-

;; Copyright © 2014-2018 Alex Kost <alezost@gmail.com>

;; Author: Alex Kost <alezost@gmail.com>
;; Version: 1.2.1
;; URL: https://github.com/alezost/bui.el
;; Keywords: tools
;; Package-Requires: ((emacs "24.3") (dash "2.11.0"))

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

;; BUI (Buffer User Interface) is a library for making 'list' (similar
;; to "M-x list-packages") and 'info' (similar to customization buffers)
;; interfaces to display various data (packages, buffers, functions,
;; etc.).
;;
;; It is not an end-user package, it is a library that is intended to be
;; used by other packages.
;;
;; Basically, at first you define 'list'/'info' interface using
;; `bui-define-interface' macro, and then you can make user commands
;; that will display entries using `bui-get-display-entries' and similar
;; functions.
;;
;; See README at <https://github.com/alezost/bui.el> for more details.

;;; Code:

(require 'cl-lib)

;; Require all features, so a package maker can require only `bui'.
(require 'bui-button)
(require 'bui-core)
(require 'bui-entry)
(require 'bui-info)
(require 'bui-list)
(require 'bui-utils)

(defmacro bui-define-entry-type (entry-type &rest args)
  "Define variables for ENTRY-TYPE.
ARGS can be the same arguments as for `bui-define-interface'.
The difference is: arguments for `bui-define-interface' define
specific variables for different buffer types, while this macro
defines general variables used for any buffer type."
  (declare (indent 1))
  (bui-plist-let args
      ((reduced? :reduced?))
    `(progn
       ,@(bui-map-symbol-specifications
          (lambda (key suffix generate)
            (let ((val (plist-get %foreign-args key)))
              (when (or val (bui-symbol-generate? generate reduced?))
                (bui-inherit-defvar-clause
                 (bui-entry-symbol entry-type suffix)
                 (bui-make-symbol 'bui suffix)
                 :value val
                 :group entry-type))))
          bui-entry-symbol-specifications)

       ,@(bui-map-symbol-specifications
          (lambda (key suffix _generate)
            (let ((val (plist-get %foreign-args key)))
              (when val
                (bui-inherit-defvar-clause
                 (bui-entry-symbol entry-type suffix)
                 (bui-make-symbol 'bui suffix)
                 :value val
                 :group entry-type))))
          bui-symbol-specifications))))

(defmacro bui-define-interface (entry-type buffer-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE entries.
Remaining arguments ARGS should have a form [KEYWORD VALUE] ...
They are used to generate variables specific for the defined
interface.  For more details and the available keywords, see
`bui-symbol-specifications', `bui-entry-symbol-specifications'
and `bui-BUFFER-TYPE-symbol-specifications'.

`:get-entries-function' is the only required keyword (if the
interface is reduced, all keywords become optional).

To denote that the interface is reduced, a special `:reduced?'
keyword may be specified.  If it is non-nil, generate only
customization group, faces group and specified variables.  If it
is nil, along with the mentioned groups and variables,
`ENTRY-TYPE-BUFFER-TYPE-mode' will be generated."
  (declare (indent 2))
  (cl-flet ((name (&rest symbols)
              (apply #'bui-symbol entry-type buffer-type symbols))
            (bui-name (&rest symbols)
              (apply #'bui-make-symbol 'bui symbols)))
    (let ((group              (name))
          (faces-group        (name 'faces))
          (mode               (name 'mode))
          (mode-map           (name 'mode-map))
          (bui-buffer-type    (bui-name buffer-type))
          (symbol-fun         (bui-name buffer-type 'symbol))
          (symbol-specs       (bui-name buffer-type 'symbol-specifications))
          (parent-mode        (bui-name buffer-type 'mode)))
      (bui-plist-let args
          ((mode-name         :mode-name (capitalize (symbol-name group)))
           (reduced?          :reduced?))
        `(progn
           (defgroup ,group nil
             ,(format "Displaying '%S' entries in '%S' buffer."
                      entry-type buffer-type)
             :group ',entry-type
             :group ',bui-buffer-type)

           (defgroup ,faces-group nil
             ,(format "Faces for displaying '%S' entries in '%S' buffer."
                      entry-type buffer-type)
             :group ',group
             :group ',(bui-entry-symbol entry-type 'faces)
             :group ',(bui-name buffer-type 'faces))

           ,@(bui-map-symbol-specifications
              (lambda (key suffix generate)
                (let ((val (plist-get %foreign-args key)))
                  (when (or val (bui-symbol-generate? generate reduced?))
                    (bui-inherit-defvar-clause
                     (name suffix)
                     (bui-name suffix)
                     :value val
                     :group group))))
              bui-symbol-specifications)

           ,@(bui-map-symbol-specifications
              (lambda (key suffix _generate)
                (let ((val (plist-get %foreign-args key)))
                  (when val
                    (bui-inherit-defvar-clause
                     (name suffix)
                     (bui-name suffix)
                     :value val
                     :group group))))
              bui-entry-symbol-specifications)

           ,@(bui-map-symbol-specifications
              (lambda (key suffix generate)
                (let ((val (plist-get args key)))
                  (when (or val (bui-symbol-generate? generate reduced?))
                    (bui-inherit-defvar-clause
                     (funcall symbol-fun entry-type suffix)
                     (bui-name buffer-type suffix)
                     :value val
                     :group group))))
              (symbol-value symbol-specs))

           ,(unless reduced?
              `(define-derived-mode ,mode ,parent-mode
                 '(,mode-name (bui-active-filter-predicates
                               bui-filter-mode-line-string))
                 ,(format "\
Major mode for displaying '%S' entries in '%S' buffer.

\\{%S}"
                          entry-type buffer-type mode-map)
                 (bui-mode-initialize ',entry-type ',buffer-type)))

           (bui-register-interface ',entry-type ',buffer-type))))))

(provide 'bui)

;;; bui.el ends here
