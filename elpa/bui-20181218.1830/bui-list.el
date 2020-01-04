;;; bui-list.el --- 'List' buffer interface for displaying data  -*- lexical-binding: t -*-

;; Copyright © 2014–2018 Alex Kost <alezost@gmail.com>

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

;; This file provides 'list' buffer interface for displaying an arbitrary
;; data.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'tabulated-list)
(require 'bui-core)
(require 'bui-button)
(require 'bui-entry)
(require 'bui-utils)

(bui-define-groups bui-list)


;;; General 'list' variables

(defvar bui-list-format nil
  "List of methods to get values of the displayed columns.
Each element of the list has a form:

  (PARAM VALUE-FUN WIDTH SORT . PROPS)

PARAM is a name of an entry parameter.

VALUE-FUN may be either nil or a function returning a value that
will be inserted.  The function is called with 2 arguments: the
first one is the value of the parameter; the second one is an
entry (alist of parameter names and values).

For the meaning of WIDTH, SORT and PROPS, see
`tabulated-list-format'.")
(put 'bui-list-format 'permanent-local t)

(defcustom bui-list-sort-key nil
  "Default sort key for 'list' buffer.
Should be nil (no sort) or have a form:

  (PARAM . FLIP)

PARAM is the name of an entry parameter.  For the meaning of
FLIP, see `tabulated-list-sort-key'."
  :type '(choice (const :tag "No sort" nil)
                 (cons symbol boolean))
  :group 'bui-list)
(put 'bui-list-sort-key 'permanent-local t)

(defvar bui-list-additional-marks nil
  "Alist of additional marks for 'list' buffer.
Marks from this list are used along with `bui-list-default-marks'.")
(put 'bui-list-additional-marks 'permanent-local t)

(defcustom bui-list-show-single nil
  "If non-nil, list an entry even if it is the only matching result.
If nil, show a single entry in the 'info' buffer instead."
  :type 'boolean
  :group 'bui-list)
(put 'bui-list-show-single 'permanent-local t)

(defcustom bui-list-describe-warning-count 10
  "The maximum number of entries to describe without a warning.
If you want to describe more than this number of marked entries,
you will be prompted for confirmation.  See also
`bui-list-describe'."
  :type 'integer
  :group 'bui-list)
(put 'bui-list-describe-warning-count 'permanent-local t)

(defvar bui-list-describe-function nil
  "Function used by `bui-list-describe'.
It is applied to the entries IDs as the rest arguments.
If nil, 'describing' is not performed (it usually means that
'info' interface is not defined).")
(put 'bui-list-describe-function 'permanent-local t)

(defconst bui-list-symbol-specifications
  '((:describe-function describe-function t)
    (:describe-count    describe-warning-count t)
    (:format            format t)
    (:list-single?      show-single t)
    (:marks             additional-marks)
    (:sort-key          sort-key t))
  "Specifications for generating 'list' variables.
See `bui-symbol-specifications' for details.")


;;; Displaying 'info' buffer

(defun bui-list-describe (&rest mark-names)
  "Describe entries marked with MARK-NAMES.
'Describe' means display entries in 'info' buffer.
If no entries are marked, describe the current entry.

Available MARK-NAMES are symbols from `bui-list-marks'.

Interactively, describe entries marked with a general mark.  With
prefix argument, describe entries marked with any mark."
  (interactive (unless current-prefix-arg '(general)))
  (or bui-list-describe-function
      (error "Can't display 'info' buffer: '%S' is unset"
             (bui-list-symbol (bui-current-entry-type)
                              'describe-function)))
  (let* ((ids   (or (apply #'bui-list-get-marked-id-list mark-names)
                    (list (bui-list-current-id))))
         (count (length ids)))
    (when (or (<= count bui-list-describe-warning-count)
              (y-or-n-p (format "Do you really want to describe %d entries? "
                                count)))
      (apply bui-list-describe-function ids))))


;;; Wrappers for 'list' variables

(defun bui-list-symbol (entry-type symbol)
  "Return symbol for ENTRY-TYPE and 'list' buffer type."
  (bui-symbol entry-type 'list symbol))

(defun bui-list-symbol-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE and 'list' buffer type."
  (bui-symbol-value entry-type 'list symbol))

(defun bui-list-param-title (entry-type param)
  "Return column title of an ENTRY-TYPE parameter PARAM."
  (bui-param-title entry-type 'list param))

(defun bui-list-format (entry-type)
  "Return column format for ENTRY-TYPE."
  (bui-list-symbol-value entry-type 'format))

(defun bui-list-displayed-params (entry-type)
  "Return a list of ENTRY-TYPE parameters that should be displayed."
  (mapcar #'car (bui-list-format entry-type)))

(defun bui-list-show-single-entry? (entry-type)
  "Return non-nil, if a single entry of ENTRY-TYPE should be listed."
  (or (bui-list-symbol-value entry-type 'show-single)
      bui-list-show-single))


;;; Tabulated list internals

(defun bui-list-sort-numerically (column a b)
  "Compare COLUMN of tabulated entries A and B numerically.
This function is used for sort predicates for `tabulated-list-format'.
Return non-nil, if B is bigger than A."
  (cl-flet ((num (entry)
              (string-to-number (aref (cadr entry) column))))
    (> (num b) (num a))))

(defmacro bui-list-define-numerical-sorter (column)
  "Define numerical sort predicate for COLUMN.
See `bui-list-sort-numerically' for details."
  (let ((name (intern (format "bui-list-sort-numerically-%d" column)))
        (doc  (format "\
Predicate to sort tabulated list by column %d numerically.
See `bui-list-sort-numerically' for details."
                      column)))
    `(defun ,name (a b)
       ,doc
       (bui-list-sort-numerically ,column a b))))

(defmacro bui-list-define-numerical-sorters (n)
  "Define numerical sort predicates for columns from 0 to N.
See `bui-list-define-numerical-sorter' for details."
  `(progn
     ,@(mapcar (lambda (i)
                 `(bui-list-define-numerical-sorter ,i))
               (number-sequence 0 n))))

(bui-list-define-numerical-sorters 9)

(defun bui-list-tabulated-sort-key ()
  "Return sort key for `tabulated-list-sort-key'."
  (and bui-list-sort-key
       (cons (bui-current-param-title (car bui-list-sort-key))
             (cdr bui-list-sort-key))))

(defun bui-list-tabulated-vector (fun)
  "Call FUN on each column specification.

FUN is applied to column specification as arguments (see
`bui-list-format').

Return a vector made of values of FUN calls."
  (apply #'vector
         (mapcar (lambda (col-spec)
                   (apply fun col-spec))
                 bui-list-format)))

(defun bui-list-tabulated-format ()
  "Return list specification for `tabulated-list-format'."
  (bui-list-tabulated-vector
   (lambda (param _ &rest rest-spec)
     (cons (bui-current-param-title param)
           rest-spec))))

(defun bui-list-tabulated-entries (entries entry-type)
  "Return a list of ENTRY-TYPE values for `tabulated-list-entries'."
  (mapcar (lambda (entry)
            (list (bui-entry-id entry)
                  (bui-list-tabulated-entry entry entry-type)))
          entries))

(defun bui-list-tabulated-entry (entry entry-type)
  "Return array of values for `tabulated-list-entries'.
Parameters are taken from ENTRY-TYPE ENTRY."
  (bui-list-tabulated-vector
   (lambda (param fun &rest _)
     (let ((value (bui-entry-value entry param)))
       (cond
        ;; If function is specified, then it should probably return
        ;; something, even if VALUE is void, so give it the precedence.
        (fun (funcall fun (bui-entry-non-void-value entry param) entry))
        ((bui-void-value? value) bui-empty-string)
        ((and (null value)
              (bui-boolean-param? entry-type 'list param))
         bui-false-string)
        (t (bui-get-string value)))))))


;;; Displaying entries

(defun bui-list-get-display-entries (entry-type &rest args)
  "Search for entries and show them in a 'list' buffer preferably."
  (let ((entries (bui-get-entries entry-type 'list args)))
    (if (or (null entries)      ; = 0
            (cdr entries)       ; > 1
            (bui-list-show-single-entry? entry-type)
            (not (bui-interface-defined? entry-type 'info)))
        (bui-display-entries entries entry-type 'list args)
      (if (equal (bui-symbol-value entry-type 'info 'get-entries-function)
                 (bui-symbol-value entry-type 'list 'get-entries-function))
          (bui-display-entries entries entry-type 'info args)
        (bui-get-display-entries entry-type 'info args)))))

(defun bui-list-insert-entries (entries entry-type)
  "Print ENTRY-TYPE ENTRIES in the current buffer."
  (setq tabulated-list-entries
        (bui-list-tabulated-entries entries entry-type))
  (tabulated-list-print))

(defun bui-list-get-one-line (value &optional _)
  "Return one-line string from a multi-line string VALUE.
VALUE may be nil."
  (bui-get-non-nil value
    (bui-get-one-line value)))

(defun bui-list-get-time (time &optional _)
  "Return formatted time string from TIME.
TIME may be nil or another value supported by `bui-get-time-string'."
  (bui-get-non-nil time
    (bui-get-string (bui-get-time-string time)
                    'bui-time)))

(defun bui-list-get-file-name (file-name &optional _)
  "Return FILE-NAME button specification for `tabulated-list-entries'.
FILE-NAME may be nil."
  (bui-get-non-nil file-name
    (list file-name
          :type 'bui-file
          'file file-name)))

(defun bui-list-get-url (url &optional _)
  "Return URL button specification for `tabulated-list-entries'.
URL may be nil."
  (bui-get-non-nil url
    (list url
          :type 'bui-url
          'url url)))


;;; 'List' lines

(defun bui-list-current-id ()
  "Return ID of the entry at point."
  (or (tabulated-list-get-id)
      (user-error "No entry here")))

(defun bui-list-current-entry ()
  "Return entry at point."
  (bui-entry-by-id (bui-current-entries)
                   (bui-list-current-id)))

(defun bui-list-for-each-line (fun &rest args)
  "Call FUN with ARGS for each entry line."
  (or (derived-mode-p 'bui-list-mode)
      (error "The current buffer is not in `bui-list-mode'"))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (apply fun args)
      (forward-line))))

(defun bui-list-fold-lines (fun init)
  "Fold over entry lines in the current list buffer.
Call FUN with RESULT as argument for each line, using INIT as
the initial value of RESULT.  Return the final result."
  (let ((res init))
    (bui-list-for-each-line
     (lambda () (setq res (funcall fun res))))
    res))


;;; Marking and sorting

(defvar-local bui-list-marked nil
  "List of the marked entries.
Each element of the list has a form:

  (ID MARK-NAME . ARGS)

ID is an entry ID.
MARK-NAME is a symbol from `bui-list-marks'.
ARGS is a list of additional values.")

(defvar-local bui-list-marks nil
  "Alist of available mark names and mark characters.")

(defvar bui-list-default-marks
  '((empty   . ?\s)
    (general . ?*))
  "Alist of default mark names and mark characters.")

(defun bui-list-get-mark (name)
  "Return mark character by its NAME."
  (or (bui-assq-value bui-list-marks name)
      (error "Mark '%S' not found" name)))

(defun bui-list-get-mark-string (name)
  "Return mark string by its NAME."
  (string (bui-list-get-mark name)))

(defun bui-list-current-mark ()
  "Return mark character of the current line."
  (char-after (line-beginning-position)))

(defun bui-list-current-mark-name ()
  "Return name of the mark on the current line."
  (or (car (bui-assq-value bui-list-marked (bui-list-current-id)))
      'empty))

(defun bui-list-get-marked (&rest mark-names)
  "Return list of specs of entries marked with any mark from MARK-NAMES.
Entry specs are elements from `bui-list-marked' list.
If MARK-NAMES are not specified, use all marks from
`bui-list-marks' except the `empty' one."
  (or mark-names
      (setq mark-names
            (delq 'empty (mapcar #'car bui-list-marks))))
  (-filter (-lambda ((_id name . _))
             (memq name mark-names))
           bui-list-marked))

(defun bui-list-get-marked-args (mark-name)
  "Return list of (ID . ARGS) elements from lines marked with MARK-NAME.
See `bui-list-marked' for the meaning of ARGS."
  (mapcar (-lambda ((id _name . args))
            (cons id args))
          (bui-list-get-marked mark-name)))

(defun bui-list-get-marked-id-list (&rest mark-names)
  "Return list of IDs of entries marked with any mark from MARK-NAMES.
See `bui-list-get-marked' for details."
  (mapcar #'car (apply #'bui-list-get-marked mark-names)))

(defun bui-list-marked-or-current (&rest mark-names)
  "Return a list of IDs of the marked entries.
If nothing is marked, return a list with ID of the current entry.
See `bui-list-get-marked' for the meaning of MARK-NAMES."
  (or (apply #'bui-list-get-marked-id-list mark-names)
      (list (bui-list-current-id))))

(defun bui-list-map-marked (function &rest mark-names)
  "Apply FUNCTION to each element of the marked entries.
If nothing is marked, call FUNCTION on the current entry.
See `bui-list-get-marked' for the meaning of MARK-NAMES."
  (mapcar function
          (apply #'bui-list-marked-or-current mark-names)))

(defun bui-list--mark (mark-name &optional advance &rest args)
  "Put a mark on the current line.
Also add the current entry to `bui-list-marked' using its ID and ARGS.
MARK-NAME is a symbol from `bui-list-marks'.
If ADVANCE is non-nil, move forward by one line after marking."
  (let ((id (bui-list-current-id)))
    (if (eq mark-name 'empty)
        (setq bui-list-marked (assq-delete-all id bui-list-marked))
      (let ((assoc (assq id bui-list-marked))
            (val (cons mark-name args)))
        (if assoc
            (setcdr assoc val)
          (push (cons id val) bui-list-marked)))))
  (tabulated-list-put-tag (bui-list-get-mark-string mark-name)
                          advance))

(defun bui-list-mark (&optional arg)
  "Mark the current line and move to the next line.
With ARG, mark all lines."
  (interactive "P")
  (if arg
      (bui-list-mark-all)
    (bui-list--mark 'general t)))

(defun bui-list-mark-all (&optional mark-name)
  "Mark all lines with MARK-NAME mark.
MARK-NAME is a symbol from `bui-list-marks'.
Interactively, put a general mark on all lines."
  (interactive)
  (or mark-name (setq mark-name 'general))
  (setq bui-list-marked
        (if (eq mark-name 'empty)
            nil
          (mapcar (lambda (entry)
                    (list (bui-entry-id entry) mark-name))
                  (bui-current-entries))))
  (bui-list-for-each-line #'tabulated-list-put-tag
                          (bui-list-get-mark-string mark-name)))

(defun bui-list-unmark (&optional arg)
  "Unmark the current line and move to the next line.
With ARG, unmark all lines."
  (interactive "P")
  (if arg
      (bui-list-unmark-all)
    (bui-list--mark 'empty t)))

(defun bui-list-unmark-backward ()
  "Move up one line and unmark it."
  (interactive)
  (forward-line -1)
  (bui-list--mark 'empty))

(defun bui-list-unmark-all ()
  "Unmark all lines."
  (interactive)
  (bui-list-mark-all 'empty))

(defun bui-list-restore-marks ()
  "Put marks according to `bui-list-marked'."
  (bui-list-for-each-line
   (lambda ()
     (let ((mark-name (bui-list-current-mark-name)))
       (unless (eq mark-name 'empty)
         (tabulated-list-put-tag
          (bui-list-get-mark-string mark-name)))))))

(defun bui-list-sort (&optional n)
  "Sort list entries by the column at point.
With a numeric prefix argument N, sort the Nth column.
Same as `tabulated-list-sort', but also restore marks after sorting."
  (interactive "P")
  (tabulated-list-sort n)
  (bui-list-restore-marks))


;;; Major mode

(defvar bui-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap bui-map
                               tabulated-list-mode-map))
    (define-key map (kbd "i")   'bui-list-describe)
    (define-key map (kbd "RET") 'bui-list-describe)
    (define-key map (kbd "*")   'bui-list-mark)
    (define-key map (kbd "m")   'bui-list-mark)
    (define-key map (kbd "M")   'bui-list-mark-all)
    (define-key map (kbd "u")   'bui-list-unmark)
    (define-key map (kbd "DEL") 'bui-list-unmark-backward)
    (define-key map (kbd "U")   'bui-list-unmark-all)
    (define-key map (kbd "s")   'bui-list-sort)
    (define-key map [remap tabulated-list-sort] 'bui-list-sort)
    map)
  "Keymap for `bui-list-mode' buffers.")

(defvar bui-list-mark-hint
  '(("\\[bui-list-mark]") " mark; "
    ("\\[bui-list-unmark]") " unmark; "
    ("\\[bui-list-unmark-backward]") " unmark backward;\n")
  "Hint with 'mark' keys for 'list' buffer.
See `bui-hint' for details.")

(defvar bui-list-sort-hint
  '(("\\[bui-list-sort]") " sort by column;\n")
  "Hint with 'sort' keys for 'list' buffer.
See `bui-hint' for details.")

(defvar bui-list-info-hint
  '(("\\[bui-list-describe]") " show 'info' buffer;\n")
  "Hint for 'list' buffer used only when 'info' interface is defined.
See `bui-hint' for details.")

(defun bui-list-hint ()
  "Return hint structure for the current 'list' buffer."
  (bui-format-hints
   bui-list-mark-hint
   (and (bui-interface-defined? (bui-current-entry-type) 'info)
        bui-list-info-hint)
   bui-list-sort-hint))

(define-derived-mode bui-list-mode tabulated-list-mode "BUI-List"
  "Parent mode for displaying data in 'list' form."
  (bui-list-initialize))

(defun bui-list-initialize ()
  "Set up the current 'list' buffer."
  (setq tabulated-list-padding  2
        tabulated-list-format   (bui-list-tabulated-format)
        tabulated-list-sort-key (bui-list-tabulated-sort-key))
  (setq-local bui-list-marks (append bui-list-default-marks
                                     bui-list-additional-marks))
  (tabulated-list-init-header))

(provide 'bui-list)

;;; bui-list.el ends here
