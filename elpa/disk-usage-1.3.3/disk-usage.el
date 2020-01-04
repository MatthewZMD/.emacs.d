;;; disk-usage.el --- Sort and browse disk usage listings -*- lexical-binding: t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://gitlab.com/Ambrevar/emacs-disk-usage
;; Version: 1.3.3
;; Package-Requires: ((emacs "26.1"))
;; Keywords: files, convenience, tools

;; This file is not part of GNU Emacs.

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
;;
;; Warning: BSD and macOS users need `gdu`, the "GNU du" from the "GNU
;; coreutils".
;;
;; Disk Usage is a file system analyzer: it offers a tabulated view of file
;; listings sorted by size.  Directory sizes are computed recursively.  The results
;; are cached for speed.
;;
;; Run `disk-usage' or `disk-usage-here' to display a listing.
;; See `describe-mode' for additional bindings, such as
;; `disk-usage-dired-at-point' to open a `dired' buffer for the current
;; directory.
;;
;; Instead of displaying only the current folder, `disk-usage' can also display
;; files in all subfolders recursively with `disk-usage-toggle-recursive'.
;;
;; Marked files can be trashed with `disk-usage-delete-marked-files'.  When
;; called with a prefix argument, files are deleted permanently.
;;
;; Run `disk-usage-by-types' to display statistics of disk usage by file
;; extensions.
;;
;; With a prefix argument, cache is updated when reverting the buffer.
;;
;; With `disk-usage-add-filters' you can filter out files with arbitrary
;; predicates, e.g. files bigger than some size or older than a certain number
;; of days.
;;
;; You can customize options in the 'disk-usage group.


;;; Code:
(require 'tabulated-list)
(require 'seq)
(eval-when-compile (require 'cl-lib))

;; TODO: Can we factor `disk-usage-files' into filters?

;; TODO: Use file-notify library to watch file system changes and auto-update.
;; Also see https://github.com/Alexander-Miller/treemacs#filewatch-mode.

;; TODO: Apparent size?  Not obvious, because Emacs file-attributes does not support it.
;; TODO: Helm-FF does not work when file-name-nondirectory is on.
;; TODO: Add support for charts?

(defgroup disk-usage nil
  "Predefined configurations for `disk-usage'."
  :group 'files)

(defcustom disk-usage-discard-previous-buffer t
  "Whether to kill the current `disk-usage' buffer before moving directory."
  :type 'boolean)

(defvaralias 'disk-usage--du-command 'disk-usage-du-command)
(defcustom disk-usage-du-command (if (member system-type '(gnu gnu/linux gnu/kfreebsd))
                                     "du"
                                   "gdu")
  "Non-GNU users need GNU's `du' for the `-b' flag.  See `disk-usage-du-args'."
  :type 'string)

(defvaralias 'disk-usage--du-args 'disk-usage-du-args)
(defcustom disk-usage-du-args "-sb"
  "Non-GNU users need GNU's `du' for the `-b' flag.  See `disk-usage-du-command'."
  :type 'string)

(defvaralias 'disk-usage--find-command 'disk-usage-find-command)
(defcustom disk-usage-find-command "find"
  "The `find' executable.  This is required for recursive listings."
  :type 'string)

(defvaralias 'disk-usage--directory-size-function 'disk-usage-directory-size-function)
(defcustom disk-usage-directory-size-function
  (if (executable-find disk-usage-du-command)
      #'disk-usage-directory-size-with-du
    #'disk-usage-directory-size-with-emacs)
  "Function that returns the total disk usage of the directory passed as argument."
  :type '(choice (function :tag "Native (slow)" disk-usage-directory-size-with-emacs)
                 (function :tag "System \"du\"" disk-usage-directory-size-with-du)))

(defface disk-usage-inaccessible
  '((t :inherit error
       :underline t))
  "Face for inaccessible folders.")

(defface disk-usage-symlink
  '((t :inherit warning))
  "Face for symlinks.")

(defface disk-usage-symlink-directory
  '((t :inherit disk-usage-symlink
       :underline t))
  "Face for symlinked directories.")

(defface disk-usage-size
  '((t :inherit default))
  "Face for sizes.")

(defface disk-usage-percent
  '((t :inherit default))
  "Face for the percent column.")

(defface disk-usage-children
  '((t :inherit default))
  "Face for the children column.")

(defvar disk-usage-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "S-<return>") #'disk-usage-find-file-at-point)
    (define-key map (kbd "<backspace>") #'disk-usage-up)
    (define-key map "^" #'disk-usage-up)
    (define-key map "a" #'disk-usage-add-filters)
    (define-key map "A" #'disk-usage-remove-filters)
    (define-key map "d" #'disk-usage-dired-at-point)
    (define-key map "e" #'disk-usage-eshell-at-point)
    (define-key map "h" #'disk-usage-toggle-human-readable)
    (define-key map "f" #'disk-usage-toggle-full-path)
    (define-key map "R" #'disk-usage-toggle-recursive)
    (define-key map "m" #'disk-usage-mark)
    (define-key map "u" #'disk-usage-unmark)
    (define-key map "x" #'disk-usage-delete-marked-files)
    map)
  "Local keymap for `disk-usage-mode' buffers.")

(defvar disk-usage--cache nil)

(cl-defstruct (disk-usage--file-info
               (:constructor nil)
               (:constructor disk-usage--file-info-make))
  size
  name
  (children 0)
  (marked nil))

(defun disk-usage-reset-cache ()
  (interactive)
  (clrhash disk-usage--cache))

(defun disk-usage-filter-1-hour> (_path attributes &optional seconds)
  "Discard regular files older than 1 hour."
  (if (null (file-attribute-type attributes))
      ;; Regular files
      (time-less-p
       (time-since
        (file-attribute-modification-time attributes))
       (seconds-to-time (or seconds (* 60 60))))
    ;; Always keep directories and symlinks.
    t))

(defun disk-usage-filter-1-day> (_path attributes &optional days)
  "Discard regular files older than 1 day."
  (if (null (file-attribute-type attributes))
      ;; Regular files
      (time-less-p
       (time-since
        (file-attribute-modification-time attributes))
       (days-to-time (or days 1)))
    ;; Always keep directories and symlinks.
    t))

(defun disk-usage-filter-1-week> (path attributes)
  (disk-usage-filter-1-day> path attributes 7))

(defun disk-usage-filter-4-weeks> (path attributes)
  (disk-usage-filter-1-day> path attributes 28))

(defun disk-usage-filter-1-MiB> (_path attributes &optional size)
  "Discard regular files bigger than 1 MiB."
  (if (null (file-attribute-type attributes))
      ;; Regular files
      (< (file-attribute-size attributes) (or size (* 1024 1024)))
    ;; Always keep directories and symlinks.
    t))

(defun disk-usage-filter-10-MiB> (path attributes)
  (disk-usage-filter-1-MiB> path attributes (* 10 1024 1024)))

(defun disk-usage-filter-100-MiB> (path attributes)
  (disk-usage-filter-1-MiB> path attributes (* 100 1024 1024)))

(defun disk-usage-filter-1-GiB> (path attributes)
  (disk-usage-filter-1-MiB> path attributes (* 1024 1024 1024)))

(defcustom disk-usage-available-filters '(disk-usage-filter-1-hour>
                                          disk-usage-filter-1-day>
                                          disk-usage-filter-1-week>
                                          disk-usage-filter-4-weeks>
                                          disk-usage-filter-1-MiB>
                                          disk-usage-filter-10-MiB>
                                          disk-usage-filter-100-MiB>
                                          disk-usage-filter-1-GiB>)
  "Filters can be used to leave out files from the listing.

A filter is a function that takes a path and file attributes and
return nil to exclude a file or non-nil to include it.
A file must pass all the filters to be included.
See `disk-usage-add-filters' and `disk-usage-remove-filters'.

You can add custom filters to this list."
  :type '(repeat 'symbol))

(defcustom disk-usage-default-filters '()
  "Filters to enable in new `disk-usage' buffers."
  :type '(repeat 'symbol))

(defvar-local disk-usage-filters nil
  "List of `disk-usage' filters in current buffer.
See `disk-usage-add-filters' and `disk-usage-remove-filters'.")

(defun disk-usage-add-filters ()
  (interactive)
  (let ((filters (completing-read-multiple "Filters: "
                                           disk-usage-available-filters
                                           nil
                                           t)))
    (dolist (filter filters)
      (add-to-list 'disk-usage-filters (intern filter)))
    (tabulated-list-revert)))

(defun disk-usage-remove-filters ()
  (interactive)
  (if (null disk-usage-filters)
      (message "No filters in this buffer.")
    (let ((filters (completing-read-multiple "Filters: "
                                             disk-usage-filters
                                             nil
                                             t)))
      (setq disk-usage-filters
            (seq-difference disk-usage-filters
                            (mapcar #'intern filters)))
      (tabulated-list-revert))))

(defun disk-usage--list (directory &optional listing)
  (setq directory (or directory default-directory))
  (let ((listing (or listing
                     (and (file-accessible-directory-p directory)
                          (directory-files-and-attributes
                           directory
                           'full directory-files-no-dot-files-regexp 'nosort)))))
    (or (cl-loop for l in listing
                 for attributes = (cl-rest l)
                 for path = (cl-first l)
                 if (cl-loop for filter in disk-usage-filters
                             always (funcall filter path attributes))
                 ;; Files
                 if (null (file-attribute-type attributes))
                 collect (disk-usage--file-info-make
                          :name path
                          :size (file-attribute-size attributes))
                 ;; Symlinks
                 else if (stringp (file-attribute-type attributes))
                 collect (disk-usage--file-info-make
                          :name path
                          :size (file-attribute-size attributes))
                 ;; Folders
                 else if (eq t (file-attribute-type attributes))
                 collect
                 (disk-usage--file-info-make
                  :name path
                  :size (disk-usage--directory-size path)
                  :children (if (file-accessible-directory-p path)
                                (- (length (directory-files path)) 2)
                              0)))
        (list (disk-usage--file-info-make :size 0 :name directory)))))

(defun disk-usage--list-recursively (directory)
  "This is the equivalent of running the shell command
$ find . -type f -exec du -sb {} +"
  (setq directory (or directory default-directory))
  (let* ((default-directory directory)
         ;; Note: Cannot use `process-lines' if we want to work on remote hosts.
         (subdirs (split-string
                   (with-temp-buffer
                     (process-file disk-usage-find-command nil '(t nil) nil
                                   (file-local-name directory)
                                   "-type" "d")
                     (buffer-string))
                   "\n" 'omit-nulls))
         (remote-subdirs (mapcar (lambda (item)
                                   (concat (file-remote-p directory) item))
                                 subdirs)))
    (cl-loop for dir in remote-subdirs
             append (cl-loop for file in (directory-files-and-attributes dir
                                                                         'full
                                                                         directory-files-no-dot-files-regexp
                                                                         'nosort)
                             for name = (car file)
                             for attributes = (cdr file)
                             when (and attributes
                                       (not (file-attribute-type attributes))
                                       (cl-loop for filter in disk-usage-filters
                                                always (funcall filter name attributes)))
                             collect (disk-usage--file-info-make
                                      :name name
                                      :size (file-attribute-size attributes))))))

(defcustom disk-usage-list-function #'disk-usage--list
  "Function that returns a list of `disk-usage--file-info'.
It takes the directory to scan as argument."
  :type '(choice (function :tag "Hierarchical" disk-usage--list)
                 (function :tag "Flat (recursive)" disk-usage--list-recursively)))
(make-variable-buffer-local 'disk-usage-list-function)

(defun disk-usage-toggle-recursive ()
  "Toggle between hierarchical and flat view."
  (interactive)
  (setq disk-usage-list-function
        (if (eq disk-usage-list-function #'disk-usage--list)
            #'disk-usage--list-recursively
          #'disk-usage--list))
  (tabulated-list-revert))

(defun disk-usage--total (listing)
  (cl-loop for file in listing
           sum (disk-usage--file-info-size file)))

(defun disk-usage--directory-size (path)
  (let ((size (unless current-prefix-arg
                (gethash path disk-usage--cache))))
    (unless size
      ;; TODO: Add progress "bar"?  But can we get progress for "du"?
      ;; Maybe just print progress when using `disk-usage-directory-size-with-emacs'.
      (message "Computing disk usage for %S..." path)
      (setq size (funcall disk-usage-directory-size-function path))
      (puthash path size disk-usage--cache))
    size))

(defun disk-usage-directory-size-with-emacs (path)
  "Return the total disk usage of directory PATH as a number.
This is slow but does not require any external process."
  (disk-usage--total (disk-usage--list path)))
(defalias 'disk-usage--directory-size-with-emacs 'disk-usage-directory-size-with-emacs)

(defun disk-usage-directory-size-with-du (path)
  "See `disk-usage-directory-size-function'."
  (or (ignore-errors (string-to-number
                      (cl-first
                       (split-string
                        (with-temp-buffer
                          (with-output-to-string
                            (process-file disk-usage-du-command
                                          nil '(t nil) nil
                                          disk-usage-du-args (file-local-name path)))
                          (buffer-string))))))
      0))
(defalias 'disk-usage--directory-size-with-du 'disk-usage-directory-size-with-du)

(defun disk-usage--sort-by-size (a b)
  (< (disk-usage--file-info-size (car a))
     (disk-usage--file-info-size (car b))))

(defun disk-usage--sort-by-children (a b)
  (< (disk-usage--file-info-children (car a))
     (disk-usage--file-info-children (car b))))

(defcustom disk-usage-size-format-function #'file-size-human-readable
  "How to print size.
Takes a number and returns a string."
  :type '(choice (function :tag "Human readable" file-size-human-readable)
                 (function :tag "In bytes" number-to-string)))

(defun disk-usage--set-tabulated-list-format (&optional total-size)
  (setq tabulated-list-format
        `[("Size"
           ,(if (eq disk-usage-size-format-function #'file-size-human-readable)
                8
              12)
           disk-usage--sort-by-size . (:right-align t))
          ("%%" 6 disk-usage--sort-by-size . (:right-align t))
          ("Children" 8 disk-usage--sort-by-children . (:right-align t))
          (,(format "Files %sin '%s'"
                    (if total-size
                        (format "totalling %sB (%s) "
                                total-size
                                (file-size-human-readable total-size))
                      "")
                    default-directory)
           0 t)]))

(defun disk-usage--refresh (&optional directory)
  (setq directory (or directory default-directory))
  (let* ((listing (funcall disk-usage-list-function directory))
         (total-size (disk-usage--total listing)))
    (disk-usage--set-tabulated-list-format total-size)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar
           (lambda (file-info)
             (list file-info
                   (vector
                    (number-to-string (disk-usage--file-info-size file-info))
                    (format "%.1f%%"
                            (* 100 (/ (float (disk-usage--file-info-size file-info))
                                      total-size)))
                    (number-to-string (disk-usage--file-info-children file-info))
                    (let ((name (disk-usage--file-info-name file-info)))
                      (if (file-directory-p name)
                          ;; Make button.
                          (cons name
                                (list 'action
                                      (lambda (_)
                                        (disk-usage name))))
                        name)))))
           listing))))

(defvar disk-usage--format-files #'identity
  "How to print files.
Takes a string and returns a string.
`identity' and `file-name-nondirectory' are good candidates.")

(defun disk-usage-toggle-human-readable ()
  "Toggle between printing size in bytes or in more readable units."
  (interactive)
  (setq disk-usage-size-format-function
        (if (eq disk-usage-size-format-function #'file-size-human-readable)
            #'number-to-string
          #'file-size-human-readable))
  (tabulated-list-revert))

(defun disk-usage-toggle-full-path ()
  "Toggle between displaying the full paths or the base names."
  (interactive)
  (setq disk-usage--format-files
        (if (eq disk-usage--format-files #'identity)
            #'file-name-nondirectory
          #'identity))
  (tabulated-list-revert))

(defun disk-usage--print-file-col (file-entry)
  "Call `disk-usage--format-file' on FILE-ENTRY.
FILE-ENTRY may be a string or a button."
  (let* ((filename (if (listp file-entry)
                       (cl-first file-entry)
                     file-entry))
         (formatted-filename
          (cond
           ;; Symlinks
           ((stringp (file-attribute-type (file-attributes filename)))
            (concat (propertize (funcall disk-usage--format-files filename)
                                'face (if (file-directory-p filename)
                                          'disk-usage-symlink-directory
                                        'disk-usage-symlink))
                    " -> " (file-attribute-type (file-attributes filename))))
           ;; Directories
           ((and (not (null (file-attribute-type (file-attributes filename))))
                 (not (file-accessible-directory-p filename)))
            (propertize (funcall disk-usage--format-files filename)
                        'face 'disk-usage-inaccessible))
           ;; Regular files
           (t (funcall disk-usage--format-files filename)))))
    (if (listp file-entry)
        (let ((copy (cl-copy-list file-entry)))
          (setcar copy formatted-filename)
          copy)
      formatted-filename)))

;; TODO: We could avoid defining our own `disk-usage--print-entry' by settings
;; `tabulated-list-entries' to a closure over the listing calling
;; `disk-usage-size-format-function' to generate the columns.
(defun disk-usage--print-entry (id cols)
  "Like `tabulated-list-print-entry' but formats size for human beings."
  (let ((beg   (point))
        (x     (max tabulated-list-padding 0))
        (ncols (length tabulated-list-format))
        (inhibit-read-only t))
    (if (> tabulated-list-padding 0)
        (insert (make-string x ?\s)))
    ;; FIXME: External packages shouldn't (have to) refer to this internal var.
    (defvar tabulated-list--near-rows)
    (let ((tabulated-list--near-rows    ; Bind it if not bound yet (Bug#25506).
           (or (bound-and-true-p tabulated-list--near-rows)
               (list (or (tabulated-list-get-entry (point-at-bol 0))
                         cols)
                     cols))))
      (setq x (tabulated-list-print-col 0
                                        (propertize
                                         (funcall disk-usage-size-format-function
                                                  (string-to-number (aref cols 0)))
                                         'face 'disk-usage-size)
                                        x))
      (setq x (tabulated-list-print-col 1 (propertize (aref cols 1) 'face 'disk-usage-percent) x))
      (setq x (tabulated-list-print-col 2 (propertize (aref cols 2) 'face 'disk-usage-children) x))
      (setq x (tabulated-list-print-col 3 (disk-usage--print-file-col (aref cols 3)) x))
      (cl-loop for i from 4 below ncols
               do (setq x (tabulated-list-print-col i (aref cols i) x))))
    (insert ?\n)
    ;; Ever so slightly faster than calling `put-text-property' twice.
    (add-text-properties
     beg (point)
     `(tabulated-list-id ,id tabulated-list-entry ,cols
                         ;; WARNING: The following is a workaround for Helm so that
                         ;; (helm-find-files-input
                         ;;  (helm-ffap-guesser)
                         ;;  (thing-at-point 'filename))
                         ;; guesses the right file.
                         help-echo ,(aref cols 1)))))

(define-derived-mode disk-usage-mode tabulated-list-mode "Disk Usage"
  "Mode to display disk usage.
With a prefix argument, cache is updated when reverting the buffer.

Also see `disk-usage-by-types-mode'."
  ;; TODO: Option to display extra attributes and default column to sort.
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Size" 'flip))
  (setq tabulated-list-printer #'disk-usage--print-entry)
  (add-hook 'tabulated-list-revert-hook 'disk-usage--refresh nil t))

(defvar disk-usage-buffer-name "disk-usage")

;;;###autoload
(defun disk-usage (&optional directory)
  "Display listing of files in DIRECTORY with their size.
If DIRECTORY is nil, use current directory."
  (interactive "DDirectory name: ")
  (unless (and (stringp directory) (file-accessible-directory-p directory))
    (error "Directory cannot be opened: %S" directory))
  (unless disk-usage--cache
    (setq disk-usage--cache (make-hash-table :test #'equal)))
  (setq directory (file-truename (or (and (file-directory-p directory)
                                          directory)
                                     default-directory)))
  (switch-to-buffer
   (get-buffer-create (format "*%s<%s>*" disk-usage-buffer-name
                              (directory-file-name directory))))
  (disk-usage-mode)
  (setq disk-usage-filters disk-usage-default-filters)
  (setq default-directory directory)
  (tabulated-list-revert))

;;;###autoload
(defun disk-usage-here ()
  "Run `disk-usage' in current directory."
  (interactive)
  (disk-usage default-directory))

(defun disk-usage-up (&optional toggle-discard-previous-buffer)
  "Run `disk-usage' in the parent directory.
If `disk-usage-discard-previous-buffer' is non-nil,
the current buffer is discarded before switched.
With TOGGLE-DISCARD-PREVIOUS-BUFFER or prefix argument, this behaviour is
reversed."
  (interactive "p")
  (let ((directory default-directory))
    (when (and (or (and (not toggle-discard-previous-buffer)
                        disk-usage-discard-previous-buffer)
                   (and toggle-discard-previous-buffer
                        (not disk-usage-discard-previous-buffer)))
               (eq major-mode 'disk-usage-mode))
      (kill-this-buffer))
    (disk-usage (expand-file-name ".." directory))))

(defun disk-usage--file-name-at-point ()
  (let ((file-info (tabulated-list-get-id (point))))
    (disk-usage--file-info-name file-info)))

(defun disk-usage--directory-at-point ()
  (let ((path (disk-usage--file-name-at-point)))
    (if (file-directory-p path)
        path
      (setq path (file-name-directory path)))))

(defun disk-usage-mark-at-point (&optional count mark)
  "Mark entry at point.
See `disk-usage-mark' and `disk-usage-unmark'."
  (interactive "p")
  (let ((step (if (> count 0) 1 -1)))
    (setq count (abs count)
          mark (or mark "*"))
    (dotimes (_ count)
      (let ((file-info (tabulated-list-get-id (point))))
        (setf (disk-usage--file-info-marked file-info) (not (string= mark ""))))
      (tabulated-list-put-tag mark)
      (forward-line step))))

(defun disk-usage-mark (&optional count mark)
  "Mark files for deletion with `disk-usage-delete-marked-files'.
With numeric argument, mark that many times.
With negative numeric argument, move upward.
When region is active, mark all entries in region."
  (interactive "p")
  (if (region-active-p)
      (let ((count (count-lines
                    (region-beginning)
                    (region-end))))
        (save-mark-and-excursion
          (goto-char (region-beginning))
          (disk-usage-mark-at-point count mark)))
    (disk-usage-mark-at-point count mark)))

(defun disk-usage-unmark (&optional count)
  "Unmark files marked with `disk-usage-mark'.
With numeric argument, unmark that many times.
With negative numeric argument, move upward.
When region is active, unmark all entries in region."
  (interactive "p")
  (disk-usage-mark count ""))

(defun disk-usage-delete-marked-files (&optional permanently)
  "Delete marked files.
By default, files are moved to trash unless PERMANENTLY is
non-nil or with prefix argument."
  (interactive "P")
  (when (yes-or-no-p (format "%s marked files?" (if permanently
                                                    "Delete" "Trash")))
    (cl-loop for entry in tabulated-list-entries
             if (disk-usage--file-info-marked (car entry))
             do (let ((delete-by-moving-to-trash (not permanently))
                      (file (disk-usage--file-info-name (car entry))))
                  (if (file-directory-p file)
                      (delete-directory file
                                        'recursive
                                        delete-by-moving-to-trash)
                    (delete-file file delete-by-moving-to-trash))))
    (tabulated-list-revert)))

(defun disk-usage-find-file-at-point ()
  "Find file at point in Emacs."
  (interactive)
  (find-file (disk-usage--file-name-at-point)))

(defun disk-usage-dired-at-point ()
  (interactive)
  (dired (disk-usage--directory-at-point)))

(defun disk-usage-eshell-at-point ()
  "Run a new `eshell' from the folder at point."
  (interactive)
  (let ((default-directory (disk-usage--directory-at-point)))
    (eshell 'new-session)))

(defun disk-usage-shell-at-point ()
  "Run a new `shell' from the folder at point."
  (interactive)
  (let ((default-directory (disk-usage--directory-at-point)))
    (shell (get-buffer-create (generate-new-buffer-name "*shell*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defstruct (disk-usage--type-info
               (:constructor nil)
               (:constructor disk-usage--type-info-make))
  names
  size
  (count 1))

(defun disk-usage-by-types--list (directory)
  "Return a hash table of (TYPE DISK-USAGE-TYPE-INFO).
TYPE is the file extension (lower case)."
  (setq directory (or directory default-directory))
  (let ((listing (disk-usage--list-recursively directory))
        (table (make-hash-table :test #'equal)))
    (dolist (file-info listing)
      (let* ((ext (downcase (or (file-name-extension
                                 (disk-usage--file-info-name file-info))
                                "")))
             (size (disk-usage--file-info-size file-info))
             (type (gethash ext table)))
        (puthash ext
                 (if (not type)
                     (disk-usage--type-info-make
                      :names (list (disk-usage--file-info-name file-info))
                      :size size)
                   (setf
                    (disk-usage--type-info-names type)
                    (cons (disk-usage--file-info-name file-info)
                          (disk-usage--type-info-names type)))
                   (setf (disk-usage--type-info-count type)
                         (1+ (disk-usage--type-info-count type)))
                   (setf (disk-usage--type-info-size type)
                         (+ size (disk-usage--type-info-size type)))
                   type)
                 table)))
    table))

(defun disk-usage--type-average-size (type)
  (/ (float (disk-usage--type-info-size type))
     (disk-usage--type-info-count type)))

(defun disk-usage-by-types--sort-by-count (a b)
  (< (disk-usage--type-info-count (car a))
     (disk-usage--type-info-count (car b))))

(defun disk-usage-by-types--sort-by-size (a b)
  (< (disk-usage--type-info-size (car a))
     (disk-usage--type-info-size (car b))))

(defun disk-usage-by-types--sort-by-average (a b)
  (< (disk-usage--type-average-size (car a))
     (disk-usage--type-average-size (car b))))

(defun disk-usage-by-types--set-tabulated-list-format ()
  (setq tabulated-list-format
        `[("Extension" 12 t)
          ("Count" 12 disk-usage-by-types--sort-by-count)
          ("%%" 6 disk-usage-by-types--sort-by-size . (:right-align t))
          ("Total size" 12 disk-usage-by-types--sort-by-size . (:right-align t))
          ("Average size" 15
           disk-usage-by-types--sort-by-average . (:right-align t))]))

(defun disk-usage-by-types--refresh (&optional directory)
  (setq directory (or directory default-directory))
  (let ((listing (disk-usage-by-types--list directory)))
    (disk-usage-by-types--set-tabulated-list-format)
    (tabulated-list-init-header)
    (let ((total-size (cl-loop for e being the hash-values of listing
                               sum (disk-usage--type-info-size e))))
      (setq tabulated-list-entries
            (cl-loop for e being the hash-values of listing using (hash-keys ext)
                     collect
                     (list
                      e
                      (vector
                       ext
                       (number-to-string (disk-usage--type-info-count e))
                       (format "%.1f%%"
                               (* 100 (/ (float (disk-usage--type-info-size e))
                                         total-size)))
                       (funcall disk-usage-size-format-function
                                (disk-usage--type-info-size e))
                       (funcall disk-usage-size-format-function
                                (string-to-number
                                 (format "%.2f"
                                         (disk-usage--type-average-size e)))))))))))

(defvar disk-usage-by-types-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "h" #'disk-usage-toggle-human-readable)
    (define-key map "a" #'disk-usage-add-filters)
    (define-key map "A" #'disk-usage-remove-filters)
    ;; Don't use "<return>" since that doesn't work in a tty.
    (define-key map (kbd "RET") #'disk-usage-files)
    map)
  "Local keymap for `disk-usage-by-types-mode' buffers.")

(define-derived-mode disk-usage-by-types-mode tabulated-list-mode "Disk Usage By Types"
  "Mode to display disk usage by file types.
Also see `disk-usage-mode'."
  (setq tabulated-list-sort-key (cons "Total size" 'flip))
  (add-hook 'tabulated-list-revert-hook 'disk-usage-by-types--refresh nil t))

(defvar disk-usage-by-types-buffer-name "disk-usage-by-types")

;;;###autoload
(defun disk-usage-by-types (&optional directory)
  (interactive "DDirectory name: ")
  (setq directory (file-truename (or (and (file-directory-p directory)
                                          directory)
                                     default-directory)))
  (switch-to-buffer
   (get-buffer-create (format "*%s<%s>*" disk-usage-by-types-buffer-name
                              (directory-file-name directory))))
  (disk-usage-by-types-mode)
  (setq default-directory directory)
  (tabulated-list-revert))

;;;###autoload
(defun disk-usage-by-types-here ()
  "Run `disk-usage-by-types' in current directory."
  (interactive)
  (disk-usage-by-types default-directory))

(defvar disk-usage-files-buffer-name "disk-usage-files")

(defun disk-usage-files (&optional listing)
  "Run `disk-usage' over LISTING.
If nil, LISTING is taken from the entry in the
`disk-usage-by-types' buffer."
  (interactive)
  (unless (eq major-mode 'disk-usage-by-types-mode)
    (error "Must be in a disk-usage-by-types buffer"))
  (setq listing (or listing
                    (disk-usage--type-info-names (tabulated-list-get-id))))
  (let ((listing-with-attributes (cl-loop for l in listing
                                          collect (cons l (file-attributes l)))))
    (switch-to-buffer
     (get-buffer-create (format "*%s*" disk-usage-files-buffer-name)))
    (disk-usage-mode)
    (setq disk-usage-filters disk-usage-default-filters)
    (set (make-local-variable 'disk-usage-list-function)
         (lambda (_) (disk-usage--list nil listing-with-attributes)))
    (tabulated-list-revert)))

;;;; ChangeLog:



(provide 'disk-usage)
;;; disk-usage.el ends here
