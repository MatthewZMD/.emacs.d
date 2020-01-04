;;; bui-info.el --- 'Info' buffer interface for displaying data  -*- lexical-binding: t -*-

;; Copyright © 2014–2017 Alex Kost <alezost@gmail.com>
;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>

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

;; This file provides 'info' (help-like) buffer interface for displaying
;; an arbitrary data.

;;; Code:

(require 'dash)
(require 'bui-core)
(require 'bui-entry)
(require 'bui-button)
(require 'bui-utils)

(bui-define-groups bui-info)

(defface bui-info-heading
  '((((type tty pc) (class color)) :weight bold)
    (t :inherit variable-pitch :height 1.2 :weight bold))
  "Face for headings."
  :group 'bui-info-faces)

(defface bui-info-param-title
  '((t :inherit font-lock-type-face))
  "Face used for titles of parameters."
  :group 'bui-info-faces)


;;; General 'info' variables

(defvar bui-info-format nil
  "List of methods for inserting entries.
Each METHOD should be either nil, a function or a list.

If METHOD is nil, newline is inserted at point.

If METHOD is a function, it is called with an entry as argument.

If METHOD is a list, it should have the following form:

  (PARAM INSERT-TITLE INSERT-VALUE)

PARAM is a name of the entry parameter.

INSERT-TITLE may be either a symbol or a list.  If it is a
symbol, it should be a function or an alias from
`bui-info-title-aliases', in which case it is called with title
as argument.  If it is a list, it should have a
form (FUN-OR-ALIAS [ARGS ...]), in which case FUN-OR-ALIAS is
called with title and ARGS as arguments.

INSERT-VALUE may be either a symbol or a list.  If it is a
symbol, it should be a function or an alias from
`bui-info-value-aliases', in which case it is called with value
and entry as arguments.  If it is a list, it should have a
form (FUN-OR-ALIAS [ARGS ...]), in which case FUN-OR-ALIAS is
called with value and ARGS as arguments.

After inserting title/value with such a list METHOD, a new line
is inserted.

Parameters are inserted in the same order as defined by this list.")
(put 'bui-info-format 'permanent-local t)

(defcustom bui-info-ignore-empty-values nil
  "If non-nil, do not display non-boolean parameters with nil values."
  :type 'boolean
  :group 'bui-info)
(put 'bui-info-ignore-empty-values 'permanent-local t)

(defcustom bui-info-ignore-void-values t
  "If non-nil, do not display non-existing parameters."
  :type 'boolean
  :group 'bui-info)
(put 'bui-info-ignore-void-values 'permanent-local t)

(defcustom bui-info-fill t
  "If non-nil, fill string parameters to fit the window.
If nil, insert text parameters in a raw form."
  :type 'boolean
  :group 'bui-info)
(put 'bui-info-fill 'permanent-local t)

(defcustom bui-info-param-title-format "%-18s: "
  "String used to format a title of a parameter.
It should be a '%s'-sequence.  After inserting a title formatted
with this string, a value of the parameter is inserted.
This string is used by `bui-info-insert-title-format'."
  :type 'string
  :group 'bui-info)
(put 'bui-info-param-title-format 'permanent-local t)

(defcustom bui-info-multiline-prefix
  (make-string (length (format bui-info-param-title-format " "))
               ?\s)
  "String used to format multi-line parameter values.
If a value occupies more than one line, this string is inserted
in the beginning of each line after the first one.
This string is used by `bui-info-insert-value-format'."
  :type 'string
  :group 'bui-info)
(put 'bui-info-multiline-prefix 'permanent-local t)

(defcustom bui-info-delimiter "\n\f\n"
  "String used to separate entries."
  :type 'string
  :group 'bui-info)
(put 'bui-info-delimiter 'permanent-local t)

(defconst bui-info-symbol-specifications
  '((:delimiter delimiter t)
    (:fill fill t)
    (:format format always)
    (:ignore-empty-values ignore-empty-values t)
    (:ignore-void-values ignore-void-values t)
    (:multiline-prefix multiline-prefix t)
    (:title-format param-title-format t))
  "Specifications for generating 'info' variables.
See `bui-symbol-specifications' for details.")


;;; Wrappers for 'info' variables

(defun bui-info-symbol (entry-type symbol)
  "Return symbol for ENTRY-TYPE and 'info' buffer type."
  (bui-symbol entry-type 'info symbol))

(defun bui-info-symbol-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE and 'info' buffer type."
  (bui-symbol-value entry-type 'info symbol))

(defun bui-info-param-title (entry-type param)
  "Return a title of an ENTRY-TYPE parameter PARAM."
  (bui-param-title entry-type 'info param))

(defun bui-info-format (entry-type)
  "Return 'info' format for ENTRY-TYPE."
  (bui-info-symbol-value entry-type 'format))

(defun bui-info-displayed-params (entry-type)
  "Return a list of ENTRY-TYPE parameters that should be displayed."
  (-non-nil
   (--map (pcase it
            (`(,param . ,_) param))
          (bui-info-format entry-type))))


;;; Inserting entries

(defvar bui-info-title-aliases
  '((format . bui-info-insert-title-format)
    (simple . bui-info-insert-title-simple))
  "Alist of aliases and functions to insert titles.")

(defvar bui-info-value-aliases
  '((format . bui-info-insert-value-format)
    (indent . bui-info-insert-value-indent)
    (simple . bui-info-insert-value-simple)
    (time   . bui-info-insert-time))
  "Alist of aliases and functions to insert values.")

(defun bui-info-title-function (fun-or-alias)
  "Convert FUN-OR-ALIAS into a function to insert a title."
  (or (bui-assq-value bui-info-title-aliases fun-or-alias)
      fun-or-alias))

(defun bui-info-value-function (fun-or-alias)
  "Convert FUN-OR-ALIAS into a function to insert a value."
  (or (bui-assq-value bui-info-value-aliases fun-or-alias)
      fun-or-alias))

(defun bui-info-title-method->function (method)
  "Convert title METHOD into a function to insert a title."
  (pcase method
    ((pred null) #'ignore)
    ((pred symbolp) (bui-info-title-function method))
    (`(,fun-or-alias . ,rest-args)
     (lambda (title)
       (apply (bui-info-title-function fun-or-alias)
              title rest-args)))
    (_ (error "Unknown title method '%S'" method))))

(defun bui-info-value-method->function (method)
  "Convert value METHOD into a function to insert a value."
  (pcase method
    ((pred null) #'ignore)
    ((pred functionp) method)
    (`(,fun-or-alias . ,rest-args)
     (lambda (value _)
       (apply (bui-info-value-function fun-or-alias)
              value rest-args)))
    (_ (error "Unknown value method '%S'" method))))

(defun bui-info-insert-entries (entries entry-type)
  "Display ENTRY-TYPE ENTRIES in the current info buffer."
  (bui-mapinsert (lambda (entry)
                   (bui-info-insert-entry entry entry-type))
                 entries
                 bui-info-delimiter)
  (bui-history-insert-buttons))

(defun bui-info-insert-entry (entry entry-type &optional indent-level)
  "Insert ENTRY-TYPE ENTRY into the current info buffer.
If INDENT-LEVEL is non-nil, indent displayed data by this number
of `bui-indent' spaces."
  (bui-with-indent (* (or indent-level 0)
                      bui-indent)
    (dolist (spec (bui-info-format entry-type))
      (bui-info-insert-entry-unit spec entry entry-type))))

(defun bui-info-insert-entry-unit (format-spec entry entry-type)
  "Insert title and value of a PARAM at point.
ENTRY is alist with parameters and their values.
ENTRY-TYPE is a type of ENTRY."
  (pcase format-spec
    ((pred null)
     (bui-newline))
    ((pred functionp)
     (funcall format-spec entry))
    (`(,param ,title-method ,value-method)
     (let* ((value    (bui-entry-value entry param))
            (void?    (bui-void-value? value))
            (empty?   (null value))
            (boolean? (bui-boolean-param? entry-type 'info param)))
       (unless (or (and bui-info-ignore-void-values void?)
                   (and bui-info-ignore-empty-values
                        empty? (not boolean?)))
         (let ((title        (bui-info-param-title entry-type param))
               (insert-title (bui-info-title-method->function title-method))
               (insert-value (bui-info-value-method->function value-method)))
           (funcall insert-title title)
           (cond
            (void? (insert bui-empty-string))
            ((and empty? boolean?) (insert bui-false-string))
            (t (funcall insert-value value entry)))
           (bui-newline)))))
    (_ (error "Unknown format specification '%S'" format-spec))))

(defun bui-info-insert-title-simple (title &optional face)
  "Insert \"TITLE: \" string at point.
If FACE is nil, use `bui-info-param-title'."
  (bui-format-insert title
                     (or face 'bui-info-param-title)
                     "%s: "))

(defun bui-info-insert-title-format (title &optional face)
  "Insert TITLE using `bui-info-param-title-format' at point.
If FACE is nil, use `bui-info-param-title'."
  (bui-format-insert title
                     (or face 'bui-info-param-title)
                     bui-info-param-title-format))

(defun bui-info-insert-value-simple (value &optional button-or-face indent)
  "Format and insert parameter VALUE at point.

VALUE may be split into several short lines to fit the current
window, depending on `bui-info-fill', and each line is indented
with INDENT number of spaces.

If BUTTON-OR-FACE is a button type symbol, transform VALUE into
this (these) button(s) and insert each one on a new line.  If it
is a face symbol, propertize inserted line(s) with this face."
  (or indent (setq indent 0))
  (bui-with-indent indent
    (let* ((button?  (bui-button-type? button-or-face))
           (face     (unless button? button-or-face))
           (fill-col (unless (or button?
                                 (and (stringp value)
                                      (not bui-info-fill)))
                       (- (bui-fill-column) indent)))
           (value    (if (and value button?)
                         (bui-buttonize value button-or-face "\n")
                       value)))
      (bui-split-insert value face fill-col "\n"))))

(defun bui-info-insert-value-indent (value &optional button-or-face)
  "Format and insert parameter VALUE at point.

This function is intended to be called after inserting a title
with `bui-info-insert-title-simple'.

VALUE may be split into several short lines to fit the current
window, depending on `bui-info-fill', and each line is indented
with `bui-indent'.

For the meaning of BUTTON-OR-FACE, see `bui-info-insert-value-simple'."
  (when value (bui-newline))
  (bui-info-insert-value-simple value button-or-face bui-indent))

(defun bui-info-insert-value-format (value &optional button-or-face
                                           &rest button-properties)
  "Format and insert parameter VALUE at point.

This function is intended to be called after inserting a title
with `bui-info-insert-title-format'.

VALUE may be split into several short lines to fit the current
window, depending on `bui-info-fill' and
`bui-info-multiline-prefix'.  If VALUE is a list, its elements
will be separated with `bui-list-separator'.

If BUTTON-OR-FACE is a button type symbol, transform VALUE into
this (these) button(s).  If it is a face symbol, propertize
inserted line(s) with this face.

BUTTON-PROPERTIES are passed to `bui-buttonize' (only if
BUTTON-OR-FACE is a button type)."
  (let* ((button?  (bui-button-type? button-or-face))
         (face     (unless button? button-or-face))
         (fill-col (when (or button?
                             bui-info-fill
                             (not (stringp value)))
                     (- (bui-fill-column)
                        (length bui-info-multiline-prefix))))
         (value    (if (and value button?)
                       (apply #'bui-buttonize
                              value button-or-face bui-list-separator
                              button-properties)
                     value)))
    (bui-split-insert value face fill-col
                      (concat "\n" bui-info-multiline-prefix))))

(defun bui-info-insert-time (time &optional face)
  "Insert formatted time string using TIME at point.
See `bui-get-time-string' for the meaning of TIME."
  (bui-format-insert (bui-get-time-string time)
                     (or face 'bui-time)))


;;; Major mode

(defvar bui-info-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap (list bui-map button-buffer-map)
                               special-mode-map))
    map)
  "Keymap for `bui-info-mode' buffers.")

(define-derived-mode bui-info-mode special-mode "BUI-Info"
  "Parent mode for displaying data in 'info' form."
  (bui-info-initialize))

(defun bui-info-initialize ()
  "Set up the current 'info' buffer."
  ;; Without this, syntactic fontification is performed, and it may
  ;; break highlighting.  For example, if there is a single "
  ;; (double-quote) character, the default syntactic fontification
  ;; highlights the rest text after it as a string.
  ;; See (info "(elisp) Font Lock Basics") for details.
  (setq font-lock-defaults '(nil t)))

(provide 'bui-info)

;;; bui-info.el ends here
