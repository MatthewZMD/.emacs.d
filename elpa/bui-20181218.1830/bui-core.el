;;; bui-core.el --- Core functionality for BUI  -*- lexical-binding: t -*-

;; Copyright © 2014–2017 Alex Kost <alezost@gmail.com>

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

;; This file provides the code that is used by both `list' and `info'
;; interfaces, and the code to display defined interfaces.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'bui-history)
(require 'bui-utils)

(bui-define-groups bui
  :parent-group tools
  :parent-faces-group faces
  :group-doc "Settings for Buffer User Interface.")

(defvar bui-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") 'bui-history-back)
    (define-key map (kbd "C-c C-f") 'bui-history-forward)
    (define-key map (kbd "l") 'bui-history-back)
    (define-key map (kbd "r") 'bui-history-forward)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "R") 'bui-redisplay)
    (define-key map (kbd "f") 'bui-filter-map)
    (define-key map (kbd "h") 'bui-show-hint)
    (define-key map [remap self-insert-command] 'bui-show-hint)
    map)
  "Parent keymap for all BUI modes.")

(defvar bui-history-hint
  '("History: "
    ("\\[bui-history-back]") " go back, "
    ("\\[bui-history-forward]") " go forward;\n")
  "Hint with history keys.
See `bui-hint' for details.")

(defvar bui-common-hint
  '(("\\[revert-buffer]") " revert (update) buffer;\n"
    ("\\[bui-show-hint]") " show this hint; "
    ("\\[describe-mode]") " show full help.")
  "Hint with keys common for any buffer type.
See `bui-hint' for details.")


;;; Buffer item

(cl-defstruct (bui-item
               (:constructor nil)
               (:constructor bui-make-item
                             (entries entry-type buffer-type args))
               (:copier      nil))
  entries entry-type buffer-type args)

(defvar-local bui-item nil
  "Data (structure) for the current BUI buffer.
The structure consists of the following elements:

- `entries': list of the currently displayed entries.

  Each element of the list is an alist with an entry data of the
  following form:

    ((PARAM . VAL) ...)

  PARAM is a name of the entry parameter.
  VAL is a value of this parameter.

- `entry-type': type of the currently displayed entries.

- `buffer-type': type of the current buffer.

- `args': arguments used to get the current entries.")
(put 'bui-item 'permanent-local t)

(defmacro bui-with-item (item &rest body)
  "Evaluate BODY using buffer ITEM.
The following local variables are available inside BODY:
`%entries', `%buffer-type', `%entry-type', `%args'.
See `bui-item' for details."
  (declare (indent 1) (debug t))
  (let ((item-var (make-symbol "item")))
    `(let ((,item-var ,item))
       (let ((%entries     (bui-item-entries     ,item-var))
             (%entry-type  (bui-item-entry-type  ,item-var))
             (%buffer-type (bui-item-buffer-type ,item-var))
             (%args        (bui-item-args        ,item-var)))
         ,@body))))

(defmacro bui-with-current-item (&rest body)
  "Evaluate BODY using `bui-item'.
See `bui-with-item' for details."
  (declare (indent 0) (debug t))
  `(bui-with-item bui-item
     ,@body))

(defmacro bui-define-current-item-accessor (name)
  "Define `bui-current-NAME' function to access NAME
element of `bui-item' structure.
NAME should be a symbol."
  (let* ((name-str (symbol-name name))
         (accessor (intern (concat "bui-item-" name-str)))
         (fun-name (intern (concat "bui-current-" name-str)))
         (doc      (format "\
Return '%s' of the current BUI buffer.
See `bui-item' for details."
                           name-str)))
    `(defun ,fun-name ()
       ,doc
       (and bui-item
            (,accessor bui-item)))))

(defmacro bui-define-current-item-accessors (&rest names)
  "Define `bui-current-NAME' functions for NAMES.
See `bui-define-current-item-accessor' for details."
  `(progn
     ,@(mapcar (lambda (name)
                 `(bui-define-current-item-accessor ,name))
               names)))

(bui-define-current-item-accessors
 entries entry-type buffer-type args)

(defmacro bui-define-current-args-accessor (n prefix name)
  "Define `PREFIX-NAME' function to access Nth element of 'args'
field of `bui-item' structure.
PREFIX and NAME should be symbols."
  (let* ((prefix-str (symbol-name prefix))
         (name-str   (symbol-name name))
         (fun-name   (intern (concat prefix-str "-" name-str)))
         (doc        (format "\
Return '%s' of the current buffer.
'%s' is the element number %d in 'args' field of `bui-item'."
                             name-str name-str n)))
    `(defun ,fun-name ()
       ,doc
       (nth ,n (bui-current-args)))))

(defmacro bui-define-current-args-accessors (prefix &rest names)
  "Define `PREFIX-NAME' functions for NAMES.
See `bui-define-current-args-accessor' for details."
  (declare (indent 1))
  `(progn
     ,@(cl-loop for name in names
                for i from 0
                collect `(bui-define-current-args-accessor
                          ,i ,prefix ,name))))


;;; Filtering

(defvar bui-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'bui-enable-filter)
    (define-key map (kbd "d") 'bui-disable-filters)
    map)
  "Keymap with filter commands for BUI modes.")
(fset 'bui-filter-map bui-filter-map)

(defvar bui-filter-hint
  '(("\\[bui-enable-filter]") " enable filter; "
    ("\\[bui-disable-filters]") " disable filters;\n")
  "Hint with the default keys for filtering.
See `bui-hint' for details.")

(defcustom bui-filter-predicates nil
  "List of available filter predicates.
These predicates are used as completions for
'\\[bui-enable-filter]' command to hide entries. See
`bui-active-filter-predicates' for details."
  :type '(repeat function)
  :group 'bui)
(put 'bui-filter-predicates 'permanent-local t)

(defcustom bui-filter-mode-line-string "(f)"
  "String displayed in the mode line when filters are enabled.
Set it to nil, if you don't want to display such a string."
  :type '(choice string (const nil))
  :group 'bui)

(defvar-local bui-active-filter-predicates nil
  "List of the active filter predicates.
These predicates are used to hide unneeded entries from the
current buffer.  Each buffer entry is passed (as a single
argument) through these predicates in turn.  If a predicate
returns nil, the entry will be hidden (the rest predicates are
not called), otherwise the entry \"survives\" this predicate and
it is passed to the next one, and so on.")
(put 'bui-active-filter-predicates 'permanent-local t)

(defun bui-filter-current-entries (&rest predicates)
  "Filter the current entries using PREDICATES, and redisplay them.
If PREDICATES are not specified, display all entries."
  (setq bui-active-filter-predicates predicates)
  (bui-show-entries (bui-current-entries)
                    (bui-current-entry-type)
                    (bui-current-buffer-type)))

(defun bui-enable-filter (predicate &optional single?)
  "Apply filter PREDICATE to the current entries.
Interactively, prompt for PREDICATE, choosing candidates from the
available predicates.

If SINGLE? is non-nil (with prefix argument), make PREDICATE the
only active one (remove the other active predicates)."
  (interactive
   (let ((predicates bui-filter-predicates))
     (if (null predicates)
         (error "Filter predicates are not specified, see '%S' variable"
                (bui-entry-symbol (bui-current-entry-type)
                                  'filter-predicates))
       (list (intern (completing-read
                      (if current-prefix-arg
                          "Enable single filter: "
                        "Add filter: ")
                      predicates))
             current-prefix-arg))))
  (or (functionp predicate)
      (error "Wrong filter predicate: %S" predicate))
  (if (if single?
          (equal (list predicate) bui-active-filter-predicates)
        (memq predicate bui-active-filter-predicates))
      (message "Filter predicate '%S' already enabled" predicate)
    (apply #'bui-filter-current-entries
           (if single?
               (list predicate)
             (cons predicate bui-active-filter-predicates)))))

(defun bui-disable-filters ()
  "Disable all active filters."
  (interactive)
  (if (null bui-active-filter-predicates)
      (message "There are no active filters.")
    (bui-filter-current-entries)))


;;; Hints

(defface bui-hint-key
  '((t :inherit font-lock-warning-face))
  "Face used by `bui-show-hint' to display keys."
  :group 'bui-faces)

(defcustom bui-hint-format "[%s]"
  "String used to format each key in `bui-hint'.
This string should contain a single '%s' structure that will be
replaced by a key string."
  :type 'string
  :group 'bui)

(defvar bui-hint-key-separator ", "
  "String used to separate keys in `bui-hint'.")

(defvar bui-hint #'bui-default-hint
  "Hint displayed in the echo area by \\[bui-show-hint].

It can be either a string, a list, or a function returning one of
those.

If it is a list, its elements should have one of the following
forms:

  STRING
  (KEY-STRING ...)

STRING elements are displayed as is.

KEY-STRING elements are highlighted with `bui-hint-key' face and
are separated with `bui-hint-key-separator'.  Also these strings
are passed through `substitute-command-keys', so you can use any
supported structure.

Example of a possible value:

  (\"Press:\\n\" (\"a\" \"b\") \" to do something;\\n\")")
(put 'bui-hint 'permanent-local t)

(defun bui-format-hint-keys (key-strings)
  "Concatenate and highlight KEY-STRINGS.
See `bui-hint' for details."
  (mapconcat (lambda (key)
               (format bui-hint-format
                       (propertize (substitute-command-keys key)
                                   'face 'bui-hint-key)))
             key-strings
             bui-hint-key-separator))

(defun bui-format-hint (hint)
  "Return string from HINT that has `bui-hint' form."
  (pcase hint
    ((pred null) "")
    ((pred stringp) hint)
    ((pred functionp) (funcall hint))
    ((pred listp)
     (mapconcat (lambda (list-or-string)
                  (if (listp list-or-string)
                      (bui-format-hint-keys list-or-string)
                    list-or-string))
                hint ""))
    (_ (error "Unknown hint type: %S" hint))))

(defun bui-format-hints (&rest hints)
  "Call `bui-format-hint' on all HINTS and concatenate results."
  (mapconcat #'bui-format-hint hints ""))

(defun bui-default-hint ()
  "Return default hint structure for the current buffer."
  (let* ((buffer-type-hint-fun (bui-make-symbol
                                'bui (bui-current-buffer-type) 'hint))
         (buffer-type-hint (and (fboundp buffer-type-hint-fun)
                                (funcall buffer-type-hint-fun))))
    (apply #'bui-format-hints
           (delq nil
                 (list buffer-type-hint
                       (and bui-filter-predicates
                            bui-filter-hint)
                       bui-history-hint
                       bui-common-hint)))))

(defun bui-show-hint ()
  "Show `bui-hint' in the echo area."
  (interactive)
  (message (bui-format-hint bui-hint)))


;;; General variables

(defcustom bui-titles nil
  "Alist of titles of parameters."
  :type '(alist :key-type symbol :value-type string)
  :group 'bui)
(put 'bui-titles 'permanent-local t)

(defvar bui-boolean-params nil
  "List of boolean parameters.
These parameters are displayed using `bui-false-string' for
nil values (unlike usual parameters which are displayed using
`bui-empty-string').")
(put 'bui-boolean-params 'permanent-local t)

(defvar bui-get-entries-function nil
  "Function used to receive entries.")
(put 'bui-get-entries-function 'permanent-local t)

(defvar bui-show-entries-function nil
  "Function used to show entries.
This function is called with a list of entries as a single
argument.  If nil, `bui-show-entries-default' is called with
appropriate ENTRY-TYPE and BUFFER-TYPE.")
(put 'bui-show-entries-function 'permanent-local t)

(defvar bui-mode-initialize-function nil
  "Function used to set up the current BUI buffer.
This function is called without arguments after enabling the
mode (right before running mode hooks).
It can also be nil.")
(put 'bui-mode-initialize-function 'permanent-local t)

(defvar bui-message-function nil
  "Function used to display a message after showing entries.
If nil, do not display messages.")
(put 'bui-message-function 'permanent-local t)

(defcustom bui-buffer-name nil
  "Default name of a buffer for displaying entries.
May be nil, a string or a function returning a string.  The
function is called with the same arguments as the function used
to get entries.  If nil, the name is defined automatically."
  :type '(choice string function (const nil))
  :group 'bui)
(put 'bui-buffer-name 'permanent-local t)

(defcustom bui-revert-confirm t
  "If non-nil, ask to confirm for reverting the buffer."
  :type 'boolean
  :group 'bui)
(put 'bui-revert-confirm 'permanent-local t)


;;; Overriding variables

(defconst bui-entry-symbol-specifications
  '((:true-string           true-string t)
    (:false-string          false-string t)
    (:empty-string          empty-string t)
    (:list-separator        list-separator t)
    (:time-format           time-format t)
    (:filter-predicates     filter-predicates t)
    (:boolean-params        boolean-params))
  "Specifications for generating entry variables.
See `bui-symbol-specifications' for details.")

(defconst bui-symbol-specifications
  '((:get-entries-function  get-entries-function)
    (:show-entries-function show-entries-function)
    (:mode-init-function    mode-initialize-function)
    (:message-function      message-function)
    (:buffer-name           buffer-name t)
    (:titles                titles always)
    (:hint                  hint)
    (:history-size          history-size t)
    (:revert-confirm?       revert-confirm t))
  "Specifications for generating interface variables.
Each specification has the following form:

  (KEYWORD SYMBOL-SUFFIX [GENERATE])

KEYWORD is what can be specified in `bui-define-interface' macro.

SYMBOL-SUFFIX defines the name of a generated variable (it is
prefixed with ENTRY-TYPE-BUFFER-TYPE).

If GENERATE is nil, generate the variable only if a keyword/value
pair is specified in the macro.  If it is t, generate the
variable, unless the defined interface is reduced.  If it is a
symbol `always', generate the variable even for the reduced
interface.")

(defalias 'bui-symbol-specification-keyword #'cl-first
  "Return keyword from symbol specification.")

(defalias 'bui-symbol-specification-suffix #'cl-second
  "Return symbol suffix from symbol specification.")

(defalias 'bui-symbol-specification-generate #'cl-third
  "Return 'generate' value from symbol specification.")

(defun bui-symbol-generate? (generate &optional reduced?)
  "Return non-nil if a symbol should be generated.
See `bui-symbol-specifications' for the meaning of GENERATE.
If REDUCED? is non-nil, it means a reduced interface should be defined."
  (or (eq generate 'always)
      (and generate (not reduced?))))

(defun bui-map-symbol-specifications (function specifications)
  "Map through SPECIFICATIONS using FUNCTION.
SPECIFICATIONS should have a form of `bui-symbol-specifications'."
  (mapcar (lambda (spec)
            (funcall function
                     (bui-symbol-specification-keyword spec)
                     (bui-symbol-specification-suffix spec)
                     (bui-symbol-specification-generate spec)))
          specifications))

(defun bui-set-local-variable-maybe (symbol value)
  "Set SYMBOL's value to VALUE if SYMBOL is bound and VALUE is non-nil."
  (when (and value (boundp symbol))
    (set (make-local-variable symbol) value)))

(defun bui-set-local-variables (entry-type buffer-type)
  "Set BUI variables according to ENTRY-TYPE/BUFFER-TYPE variables."
  ;; General variables.
  (dolist (suffix (mapcar #'bui-symbol-specification-suffix
                          (append bui-entry-symbol-specifications
                                  bui-symbol-specifications)))
    (bui-set-local-variable-maybe
     (bui-make-symbol 'bui suffix)
     (bui-symbol-value entry-type buffer-type suffix)))
  ;; Variables specific to BUFFER-TYPE.
  (dolist (suffix (mapcar #'bui-symbol-specification-suffix
                          (symbol-value
                           (bui-symbol-if-bound
                            (bui-make-symbol
                             'bui buffer-type 'symbol-specifications)))))
    (bui-set-local-variable-maybe
     (bui-make-symbol 'bui buffer-type suffix)
     (bui-symbol-value entry-type buffer-type suffix))))


;;; Wrappers for defined variables

(defalias 'bui-entry-symbol #'bui-make-symbol)
(defalias 'bui-symbol #'bui-make-symbol)

(defun bui-entry-symbol-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE."
  (symbol-value
   (bui-symbol-if-bound (bui-entry-symbol entry-type symbol))))

(defun bui-symbol-value (entry-type buffer-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE/BUFFER-TYPE."
  (or (symbol-value (bui-symbol-if-bound
                     (bui-symbol entry-type buffer-type symbol)))
      (bui-entry-symbol-value entry-type symbol)))

(defun bui-get-entries (entry-type buffer-type &optional args)
  "Return ENTRY-TYPE entries.
Call an appropriate 'get-entries' function using ARGS as its arguments."
  (apply (bui-symbol-value entry-type buffer-type 'get-entries-function)
         args))

(defun bui-mode-enable (entry-type buffer-type)
  "Turn on major mode to display ENTRY-TYPE ENTRIES in BUFFER-TYPE buffer."
  (funcall (bui-symbol entry-type buffer-type 'mode)))

(define-obsolete-function-alias 'bui-mode-initialize-default
  'ignore "1.1.0")

(defun bui-mode-initialize (_entry-type _buffer-type)
  "Set up the current BUI buffer."
  (setq-local revert-buffer-function 'bui-revert)
  (when bui-mode-initialize-function
    (funcall bui-mode-initialize-function)))

(defun bui-insert-entries (entries entry-type buffer-type)
  "Show ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (funcall (bui-make-symbol 'bui buffer-type 'insert-entries)
           entries entry-type))

(defun bui-show-entries-default (entries entry-type buffer-type)
  "Default function to show ENTRY-TYPE ENTRIES in the BUFFER-TYPE buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (bui-mode-enable entry-type buffer-type)
    (let ((filtered-entries (apply #'bui-filter
                                   entries bui-active-filter-predicates)))
      (if filtered-entries
          (bui-insert-entries filtered-entries entry-type buffer-type)
        (when entries
          (message (substitute-command-keys
                    "Everything is filtered out :-)
Use '\\[bui-disable-filters]' to remove filters")))))
    (goto-char (point-min))))

(defun bui-show-entries (entries entry-type buffer-type)
  "Show ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (--if-let (bui-symbol-value entry-type buffer-type
                              'show-entries-function)
      (funcall it entries)
    (bui-show-entries-default entries entry-type buffer-type)))

(defun bui-message (entries entry-type buffer-type &optional args)
  "Display a message for BUFFER-ITEM after showing entries."
  (--when-let (bui-symbol-value entry-type buffer-type
                                'message-function)
    (apply it entries args)))

(defun bui-buffer-name (entry-type buffer-type &optional args)
  "Return name of BUFFER-TYPE buffer for displaying ENTRY-TYPE entries."
  (let ((val (bui-symbol-value entry-type buffer-type 'buffer-name)))
    (cond
     ((stringp val)
      val)
     ((functionp val)
      (apply val args))
     (t
      (concat "*"
              (capitalize (symbol-name entry-type))
              " "
              (capitalize (symbol-name buffer-type))
              "*")))))

(defun bui-param-title (entry-type buffer-type param)
  "Return PARAM title for ENTRY-TYPE/BUFFER-TYPE."
  (or (bui-assq-value (bui-symbol-value entry-type buffer-type 'titles)
                      param)
      (bui-assq-value (bui-entry-symbol-value entry-type 'titles)
                      param)
      (bui-symbol-title param)))

(defun bui-current-param-title (param)
  "Return PARAM title for the current ENTRY-TYPE/BUFFER-TYPE."
  (bui-param-title (bui-current-entry-type)
                   (bui-current-buffer-type)
                   param))

(defun bui-boolean-param? (entry-type buffer-type param)
  "Return non-nil if PARAM for ENTRY-TYPE/BUFFER-TYPE is boolean."
  (memq param (bui-symbol-value entry-type buffer-type 'boolean-params)))


;;; Displaying entries

(defun bui-display (buffer)
  "Switch to a BUI BUFFER."
  (pop-to-buffer buffer
                 '((display-buffer-reuse-window
                    display-buffer-same-window))))

(defun bui-history-item (buffer-item)
  "Make and return a history item for displaying BUFFER-ITEM."
  (list #'bui-set buffer-item 'no))

(defun bui-set (buffer-item &optional history)
  "Set up the current buffer for displaying BUFFER-ITEM.
HISTORY should be one of the following:

  `nil' or `add' - add it to history,

  `no' - do not save BUFFER-ITEM in history,

  `replace' - replace the current history item."
  (bui-with-item buffer-item
    (when %entries
      ;; At first, set buffer item so that its value can be used by the
      ;; code for displaying entries.
      (setq bui-item buffer-item)
      (bui-set-local-variables %entry-type %buffer-type)
      ;; History should be set after setting local variables (after
      ;; setting `bui-history-size'), but before showing entries (before
      ;; inserting history buttons).
      (unless (eq history 'no)
        (funcall (cl-ecase history
                   ((nil add) #'bui-history-add)
                   (replace   #'bui-history-replace))
                 (bui-history-item buffer-item)))
      (bui-show-entries %entries %entry-type %buffer-type))
    (bui-message %entries %entry-type %buffer-type %args)))

(defun bui-display-entries-current (entries entry-type buffer-type
                                    &optional args history)
  "Show ENTRIES in the current BUI buffer.
See `bui-item' for the meaning of BUFFER-TYPE, ENTRY-TYPE
and ARGS, and `bui-set' for the meaning of HISTORY."
  (bui-set (bui-make-item entries entry-type buffer-type args)
           history))

(defun bui-get-display-entries-current (entry-type buffer-type
                                        &optional args history)
  "Search for entries and show them in the current BUI buffer.
See `bui-display-entries-current' for details."
  (bui-display-entries-current
   (bui-get-entries entry-type buffer-type args)
   entry-type buffer-type args history))

(defun bui-display-entries (entries entry-type buffer-type
                            &optional args history)
  "Show ENTRIES in a BUFFER-TYPE buffer.
See `bui-display-entries-current' for details."
  (if entries
      (let ((buffer (get-buffer-create
                     (bui-buffer-name entry-type buffer-type args))))
        (with-current-buffer buffer
          (bui-display-entries-current
           entries entry-type buffer-type args history))
        (bui-display buffer))
    (bui-message entries entry-type buffer-type args)))

(defun bui-get-display-entries (entry-type buffer-type
                                &optional args history)
  "Search for entries and show them in a BUFFER-TYPE buffer.
See `bui-display-entries-current' for details."
  (bui-display-entries
   (bui-get-entries entry-type buffer-type args)
   entry-type buffer-type args history))

(defun bui-revert (_ignore-auto noconfirm)
  "Update the data in the current BUI buffer.
This function is suitable for `revert-buffer-function'.
See `revert-buffer' for the meaning of NOCONFIRM."
  (bui-with-current-item
    (ignore %entries)           ; to avoid compilation warning
    (when (or noconfirm
              (not bui-revert-confirm)
              (y-or-n-p "Update the current buffer? "))
      (bui-get-display-entries-current
       %entry-type %buffer-type %args 'replace))))

(defvar bui-after-redisplay-hook nil
  "Hook run by `bui-redisplay'.
This hook is called before seting up a window position.")

(defun bui-redisplay ()
  "Redisplay the current BUI buffer.
Restore the point and window positions after redisplaying.

This function does not update the buffer data, use
'\\[revert-buffer]' if you want the full update."
  (interactive)
  (let* ((old-point (point))
         ;; For simplicity, ignore an unlikely case when multiple
         ;; windows display the same buffer.
         (window (car (get-buffer-window-list (current-buffer) nil t)))
         (window-start (and window (window-start window))))
    (bui-set bui-item 'no)
    (goto-char old-point)
    (run-hooks 'bui-after-redisplay-hook)
    (when window
      (set-window-point window (point))
      (set-window-start window window-start))))

(defun bui-redisplay-goto-button ()
  "Redisplay the current buffer and go to the next button, if needed."
  (let ((bui-after-redisplay-hook
         (cons (lambda ()
                 (unless (button-at (point))
                   (forward-button 1)))
               bui-after-redisplay-hook)))
    (bui-redisplay)))


;; Interfaces

(defvar bui-interfaces nil
  "List of defined interfaces.")

(defalias 'bui-interface-id #'bui-make-symbol
  "Return some kind of identifier for ENTRY-TYPE/BUFFER-TYPE interface.")

(defun bui-interface-defined? (entry-type buffer-type)
  "Return non-nil if ENTRY-TYPE/BUFFER-TYPE interface is defined."
  (member (bui-interface-id entry-type buffer-type)
          bui-interfaces))

(defun bui-register-interface (entry-type buffer-type)
  "Add new ENTRY-TYPE/BUFFER-TYPE interface to `bui-interfaces'."
  (cl-pushnew (bui-interface-id entry-type buffer-type)
              bui-interfaces))

(provide 'bui-core)

;;; bui-core.el ends here
