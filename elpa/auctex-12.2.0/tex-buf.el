;;; tex-buf.el --- External commands for AUCTeX.

;; Copyright (C) 1991-1999, 2001-2019 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex, wp

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file provides support for external commands.

;;; Code:

(require 'tex)
(require 'latex)
(require 'comint)

;;; Customization:

(defcustom TeX-process-asynchronous (not (eq system-type 'ms-dos))
  "*Use asynchronous processes."
  :group 'TeX-command
  :type 'boolean)

(defcustom TeX-shell
  (if (memq system-type '(ms-dos emx windows-nt))
      shell-file-name
    "/bin/sh")
  "Name of shell used to parse TeX commands."
  :group 'TeX-command
  :type 'file)

(defcustom TeX-shell-command-option
  (cond ((memq system-type '(ms-dos emx windows-nt) )
	 (cond ((boundp 'shell-command-option)
		shell-command-option)
	       ((boundp 'shell-command-switch)
		shell-command-switch)
	       (t
		"/c")))
	(t				;Unix & EMX (Emacs 19 port to OS/2)
	 "-c"))
  "Shell argument indicating that next argument is the command."
  :group 'TeX-command
  :type 'string)

;;; Interactive Commands
;;
;; The general idea is, that there is one process and process buffer
;; associated with each master file, and one process and process buffer
;; for running TeX on a region.   Thus, if you have N master files, you
;; can run N + 1 processes simultaneously.
;;
;; Some user commands operates on ``the'' process.  The following
;; algorithm determine what ``the'' process is.
;;
;; IF   last process started was on a region
;; THEN ``the'' process is the region process
;; ELSE ``the'' process is the master file (of the current buffer) process

(defun TeX-save-document (name)
  "Save all files belonging to the current document.
Return non-nil if document needs to be re-TeX'ed."
  (interactive (list (TeX-master-file)))
  (if (string-equal name "")
      (setq name (TeX-master-file)))

  (TeX-check-files (concat name "." (TeX-output-extension))
		   (cons name (TeX-style-list))
		   TeX-file-extensions))

(defun TeX-command-master (&optional override-confirm)
  "Run command on the current document.

If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
depend on it being positive instead of the entry in `TeX-command-list'."
  (interactive "P")
  (TeX-command (TeX-command-query (TeX-master-file nil nil t))
	       'TeX-master-file override-confirm))

(defvar TeX-command-region-begin nil)
(defvar TeX-command-region-end nil)
;; Used for marking the last region.

(make-variable-buffer-local 'TeX-command-region-begin)
(make-variable-buffer-local 'TeX-command-region-end)

(defun TeX-current-offset (&optional pos)
  "Calculate line offset of POS, or of point if POS is nil."
  (save-restriction
    (widen)
    (save-excursion
      (let ((inhibit-point-motion-hooks t)
	    (inhibit-field-text-motion t))
	(if pos (goto-char pos))
	(+ (count-lines (point-min) (point))
	   (if (bolp) 0 -1))))))

(defun TeX-pin-region (begin end)
  "Pin the TeX region specified by BEGIN and END.
If BEGIN is nil, the region is unpinned.

In interactive use, a positive prefix arg will pin the region,
a non-positive one will unpin it.  Without a prefix arg, if
a region is actively marked, it will get pinned.  If not, a
pinned region will get unpinned and vice versa."
  (interactive
   (if
       (if current-prefix-arg
	   (> (prefix-numeric-value current-prefix-arg) 0)
	 (or (TeX-active-mark)
	     (null TeX-command-region-begin)))
       (list (region-beginning) (region-end))
     '(nil nil)))
  (if begin
      (progn
	(unless (markerp TeX-command-region-begin)
	  (setq TeX-command-region-begin (make-marker))
	  (setq TeX-command-region-end (make-marker)))
	(set-marker TeX-command-region-begin begin)
	(set-marker TeX-command-region-end end)
	(message "TeX region pinned."))
    (when (markerp TeX-command-region-begin)
      (set-marker TeX-command-region-begin nil)
      (set-marker TeX-command-region-end nil))
    (setq TeX-command-region-begin nil)
    (setq TeX-command-region-end nil)
    (message "TeX region unpinned.")))

(defun TeX-region-update ()
  "Update the TeX-region file."
  ;; Note that TeX-command-region-begin is not a marker when called
  ;; from TeX-command-buffer.
  (and (or (null TeX-command-region-begin)
	   (markerp TeX-command-region-begin))
       (TeX-active-mark)
       (TeX-pin-region (region-beginning) (region-end)))
  (let ((begin (or TeX-command-region-begin (region-beginning)))
	(end (or TeX-command-region-end (region-end))))
    (TeX-region-create (TeX-region-file TeX-default-extension)
		       (buffer-substring begin end)
		       (file-name-nondirectory (buffer-file-name))
		       (TeX-current-offset begin))))

(defun TeX-command-region (&optional override-confirm)
  "Run TeX on the current region.

Query the user for a command to run on the temporary file specified by
the variable `TeX-region'.  If there is an explicitly active region,
it is stored for later commands.  If not, a previously stored region
\(can be also be set with `TeX-pin-region') overrides the current region,
if present.

If a prefix argument OVERRIDE-CONFIRM is given, prompting will
ignore the prompting flag from `TeX-command-list' and instead
will prompt iff the prefix is positive.

If the master file for the document has a header, it is written to the
temporary file before the region itself.  The document's header is all
text before `TeX-header-end'.

If the master file for the document has a trailer, it is written to
the temporary file after the region itself.  The document's trailer is
all text after `TeX-trailer-start'."
  (interactive "P")
  (TeX-region-update)
  ;; In the next line, `TeX-region-file' should be called with nil
  ;; `nondirectory' argument, otherwise `TeX-comand-default' called
  ;; within `TeX-command-query' won't work in included files not
  ;; placed in `TeX-master-directory'.
  (TeX-command (TeX-command-query (TeX-region-file)) 'TeX-region-file
	       override-confirm))

(defun TeX-command-buffer (&optional override-confirm)
  "Run TeX on the current buffer.

Query the user for a command to run on the temporary file specified by
the variable `TeX-region'.  The region file will be recreated from the
visible part of the buffer.

If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
depend on it being positive instead of the entry in `TeX-command-list'."
  (interactive "P")
  (let ((TeX-command-region-begin (point-min))
	(TeX-command-region-end (point-max)))
    (TeX-command-region override-confirm)))

(defcustom TeX-record-buffer nil
  "Whether to record buffer names of generated TeX buffers.
When non-nil, these buffers are put at the front of the list of
recently selected ones."
  :group 'TeX-command
  :type 'boolean)

(defun TeX-pop-to-buffer (buffer &optional other-window norecord)
  "Compatibility wrapper for `pop-to-buffer'.

Select buffer BUFFER in some window, preferably a different one.
BUFFER may be a buffer, a string (a buffer name), or nil.
If BUFFER is a string which is not the name of an existing buffer,
then this function creates a buffer with that name.
If BUFFER is nil, then it chooses some other buffer.
If `pop-up-windows' is non-nil, windows can be split to do this.
If optional second arg OTHER-WINDOW is non-nil, insist on finding another
window even if BUFFER is already visible in the selected window,
and ignore `same-window-regexps' and `same-window-buffer-names'.
This function returns the buffer it switched to.
This uses the function `display-buffer' as a subroutine; see the documentation
of `display-buffer' for additional customization information.

Optional third arg NORECORD non-nil means do not put this buffer
at the front of the list of recently selected ones."
  (pop-to-buffer buffer other-window (and norecord (not TeX-record-buffer))))

(defun TeX-recenter-output-buffer (line)
  "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on line LINE of the window, or
at bottom if LINE is nil."
  (interactive "P")
  (let ((buffer (TeX-active-buffer)))
    (if buffer
	(let ((old-buffer (current-buffer)))
	  (TeX-pop-to-buffer buffer t t)
	  (bury-buffer buffer)
	  (goto-char (point-max))
	  (recenter (if line
			(prefix-numeric-value line)
		      (/ (window-height) 2)))
	  (TeX-pop-to-buffer old-buffer nil t))
      (message "No process for this document."))))

(defun TeX-kill-job ()
  "Kill the currently running TeX job."
  (interactive)
  (let ((process (TeX-active-process)))
    (if process
	(kill-process process)
      ;; Should test for TeX background process here.
      (error "No TeX process to kill"))))

;; FIXME: The vars below are defined in this file, but they're defined too
;; far down (i.e. further down than their first use), so we have to pre-declare
;; them here to explain it to the compiler.
;; We should move those vars's definitions earlier instead!
(defvar TeX-current-process-region-p)
(defvar TeX-save-query)
(defvar TeX-parse-function)
(defvar TeX-sentinel-function)
(defvar TeX-sentinel-default-function)
(defvar compilation-in-progress)
(defvar TeX-current-page)
(defvar TeX-error-overview-open-after-TeX-run)
(defvar TeX-error-list)
(defvar TeX-parse-all-errors)
(defvar TeX-command-buffer)
(defvar TeX-region)

(defun TeX-home-buffer ()
  "Go to the buffer where you last issued a TeX command.
If there is no such buffer, or you already are in that buffer, find
the master file."
  (interactive)
  (if (or (null TeX-command-buffer)
	  (null (buffer-name TeX-command-buffer))
	  (eq TeX-command-buffer (current-buffer)))
      (find-file (TeX-master-file TeX-default-extension))
    (switch-to-buffer TeX-command-buffer)))

(defvar TeX-error-last-visited -1
  "Index of the last visited error listed in `TeX-error-list'.

This variable is intended to be set only in output buffer so it
will be shared among all files of the same document.")
(make-variable-buffer-local 'TeX-error-last-visited)

(defun TeX-get-parse-function ()
  "Get the parse function for the current buffer."
  (with-current-buffer TeX-command-buffer
    (TeX-process-get-variable (TeX-active-master) 'TeX-parse-function)))

(defun TeX-next-error (&optional arg reparse)
  "Find the next error in the TeX output buffer.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages, if possible.

If REPARSE is non-nil, reparse the error message buffer.

\\[universal-argument] as a prefix means reparse the error
message buffer and start at the first error."
  (interactive "P")
  (if (or (null (TeX-active-buffer))
	  (eq 'compilation-mode (with-current-buffer TeX-command-buffer
				  major-mode)))
      (next-error arg reparse)

    ;; Force reparsing when the function is called with a universal-argument.
    (if (consp arg) (setq reparse t arg nil))

    (funcall (TeX-get-parse-function) arg reparse)))

(defun TeX-previous-error (arg)
  "Find the previous error in the TeX output buffer.

Prefix arg N says how many error messages to move backward (or
forward, if negative).

This works only with TeX commands and if the
`TeX-parse-all-errors' variable is non-nil."
  (interactive "p")
  (if (or (null (TeX-active-buffer))
	  (eq 'compilation-mode (with-current-buffer TeX-command-buffer
				  major-mode)))
      (previous-error arg)

    (let ((parse-function (TeX-get-parse-function)))
      (if (and TeX-parse-all-errors (equal parse-function #'TeX-parse-TeX))
	  ;; When `TeX-parse-all-errors' is non-nil and the parsing function is
	  ;; `TeX-parse-TeX' we can move backward in the errors.
	  (TeX-parse-TeX (- arg) nil)
	;; XXX: moving backward in the errors hasn't yet been implemented for
	;; other parsing functions.
	(error "Jumping to previous error not supported")))))

;;; Command Query

(defvar TeX-error-overview-frame nil
  "The frame of the error overview.")

(defconst TeX-error-overview-buffer-name "*TeX errors*"
  "Name of the buffer in which to show error list.")

(defvar LaTeX-idx-md5-alist nil
  "Alist of MD5 hashes of idx file.

Car is the idx file, cdr is its md5 hash.")

(defvar LaTeX-idx-changed-alist nil
  "Whether the idx files changed.

Car is the idx file, cdr is whether idx changed after LaTeX
run.")

(defcustom TeX-check-engine t
  "Whether AUCTeX should check the correct engine has been set before running LaTeX commands."
  :group 'TeX-command
  :type 'boolean)

(defvar TeX-check-engine-list '(default luatex omega xetex)
  "List of engines required by the loaded TeX packages.

Do not set this variable directly, use
`TeX-check-engine-add-engines' to specify required engines.")
(make-variable-buffer-local 'TeX-check-engine-list)

(defun TeX-check-engine-add-engines (&rest engines)
  "Add ENGINES to list of required engines.

Set `TeX-check-engine-list' to the intersection between the list
itself and the list of provided engines.

See for example style/fontspec.el"
  (let ((list TeX-check-engine-list)
	(res nil))
    (setq TeX-check-engine-list
	  ;; The following is based on the definition of `cl-intersection' of
	  ;; GNU Emacs.
	  (and list engines
	       (if (equal list engines) list
		 (or (>= (length list) (length engines))
		     (setq list (prog1 engines (setq engines list))))
		 (while engines
		   (if (memq (car engines) list)
		       (push (car engines) res))
		   (pop engines))
		 res)))))

(defun TeX-check-engine (name)
  "Check the correct engine has been set.

Look into `TeX-check-engine-list' for the required engines.

NAME is the command to be run.  Actually do the check only if the
variable `TeX-check-engine' is non-nil and LaTeX is the command
to be run."
  (and
   (string= name "LaTeX")
   TeX-check-engine
   TeX-check-engine-list
   (null (memq TeX-engine TeX-check-engine-list))
   (memq TeX-engine '(default luatex omega xetex))
   ;; The set engine is not listed in `TeX-check-engine-list'.  We check only
   ;; builtin engines because we can't take care of custom ones.  Do nothing if
   ;; there is no allowed engine, we don't know what to do in that case.
   (let ((length (length TeX-check-engine-list))
	 (name-alist '((default . "TeX")
		       (luatex  . "LuaTeX")
		       (omega   . "Omega")
		       (xetex   . "XeTeX")))
	 (completion-ignore-case t)
	 (engine nil))
     (when
	 (cond
	  ;; There is exactly one allowed engine.
	  ((= length 1)
	   (setq engine (car TeX-check-engine-list))
	   (y-or-n-p (format "%s is required to build this document.
Do you want to use this engine?" (cdr (assoc engine name-alist)))))
	  ;; More than one engine is allowed.
	  ((> length 1)
	   (if (y-or-n-p (format "It appears %s are required to build this document.
Do you want to select one of these engines?"
				 (mapconcat
				  (lambda (elt) (cdr (assoc elt name-alist)))
				  TeX-check-engine-list ", ")))
	       (setq engine
		     (car (rassoc
			   (completing-read
			    (format
			     "Choose between %s: "
			     (mapconcat
			      (lambda (elt) (cdr (assoc elt name-alist)))
			      TeX-check-engine-list ", "))
			    (mapcar
			     (lambda (elt) (cdr (assoc elt name-alist)))
			     TeX-check-engine-list))
			   name-alist)))
	     ;; Don't keep asking.  If user doesn't want to change engine,
	     ;; probably has a good reason.  In order to do so, without adding
	     ;; yet another variable we just hack `TeX-check-engine-list' and
	     ;; make it nil.
	     (setq TeX-check-engine-list nil))))
       (TeX-engine-set engine)
       (when (and (fboundp 'add-file-local-variable)
		  (y-or-n-p "Do you want to remember the choice?"))
	 (add-file-local-variable 'TeX-engine engine)
	 (save-buffer))))))

(defcustom TeX-check-TeX t
  "Whether AUCTeX should check if a working TeX distribution is present."
  :group 'TeX-command
  :type 'boolean)

(defcustom TeX-check-TeX-command-not-found 127
  "Numerical code returned by shell for a command not found error."
  :group 'TeX-command
  :type 'integer)

(defun TeX-command (name file &optional override-confirm)
  "Run command NAME on the file returned by calling FILE.

FILE is the symbol of a function returning a file name.  The
function has one optional argument, the extension to use on the
file.

Use the information in `TeX-command-list' to determine how to run
the command.

If OVERRIDE-CONFIRM is a prefix argument, confirmation will be
asked if it is positive, and suppressed if it is not.

Run function `TeX-check-engine' to check the correct engine has
been set."
  (TeX-check-engine name)

  ;; Make sure that `TeX-command-buffer' is set always.
  ;; It isn't safe to remove similar lines in `TeX-run-command' etc.
  ;; because preview-latex calls `TeX-run-command' directly.
  (setq-default TeX-command-buffer (current-buffer))

  (cond ((eq file #'TeX-region-file)
	 (setq TeX-current-process-region-p t))
	((eq file #'TeX-master-file)
	 (setq TeX-current-process-region-p nil)))

  ;; When we're operating on a region, we need to update the position
  ;; of point in the region file so that forward search works.
  (if (string= name "View") (TeX-region-update-point))

  (let ((command (TeX-command-expand (nth 1 (assoc name TeX-command-list))
				     file))
	(hook (nth 2 (assoc name TeX-command-list)))
	(confirm (if override-confirm
		     (> (prefix-numeric-value override-confirm) 0)
		   (nth 3 (assoc name TeX-command-list)))))

    ;; Verify the expanded command
    (if confirm
	(setq command
	      (read-from-minibuffer (concat name " command: ") command
				    nil nil)))

    ;; Kill the frame and buffer associated to the error overview before running
    ;; the command, but keep them if the command to be run is View.
    (unless (string= name "View")
      (if (frame-live-p TeX-error-overview-frame)
	  (delete-frame TeX-error-overview-frame))
      (if (get-buffer TeX-error-overview-buffer-name)
	  (kill-buffer TeX-error-overview-buffer-name)))

    ;; Before running some commands, check that AUCTeX is able to find "tex"
    ;; program.
    (and TeX-check-TeX
         (member name '("TeX" "LaTeX" "AmSTeX" "ConTeXt" "ConTeXt Full"))
	 (= TeX-check-TeX-command-not-found
            (call-process TeX-shell nil nil nil
                          TeX-shell-command-option TeX-command))
	 (error (format "ERROR: AUCTeX cannot find a working TeX distribution.
Make sure you have one and that TeX binaries are in PATH environment variable%s"
			(if (eq system-type 'darwin)
			    ".
If you are using OS X El Capitan or later
remember to add /Library/TeX/texbin/ to your PATH"
			  ""))))

    ;; Now start the process
    (setq file (funcall file))
    (TeX-process-set-variable file 'TeX-command-next TeX-command-Show)
    (funcall hook name command file)))

(defvar TeX-command-text)               ;Dynamically scoped.
(defvar TeX-command-pos)                ;Dynamically scoped.

(defun TeX-command-expand (command file &optional list)
  "Expand COMMAND for FILE as described in LIST.
LIST default to `TeX-expand-list'.  As a special exception,
`%%' can be used to produce a single `%' sign in the output
without further expansion."
  (defvar TeX-command-pos)
  (let (pat
	pos ;;FIXME: Should this be dynamically scoped?
	entry TeX-command-text TeX-command-pos
        expansion-res case-fold-search string expansion arguments)
    (setq list (cons
		(list "%%" (lambda nil
			     (setq pos (1+ pos))
			     "%"))
		(or list (TeX-expand-list)))
	  pat (regexp-opt (mapcar #'car list)))
    ;; `TeX-command-expand' is called with `file' argument being one
    ;; of `TeX-master-file', `TeX-region-file' and
    ;; `TeX-active-master'.  The return value of these functions
    ;; sometimes needs suitable "decorations" for an argument for
    ;; underlying shell or latex executable, or both, when the
    ;; relavant file name involves some special characters such as
    ;; space and multibyte characters.  Hence embed that function in a
    ;; template prepared for that purpose.
    (setq file (apply-partially
		#'TeX--master-or-region-file-with-extra-quotes
		file))
    (while (setq pos (string-match pat command pos))
      (setq string (match-string 0 command)
	    entry (assoc string list)
	    expansion (car (cdr entry)) ;Second element
	    arguments (cdr (cdr entry)) ;Remaining elements
	    string (save-match-data
		     ;; Note regarding the special casing of `file':
		     ;; `file' is prevented from being evaluated as a
		     ;; function because inside of AUCTeX it only has
		     ;; a meaning as a variable.  This makes sure that
		     ;; a function definition made by an external
		     ;; package (e.g. icicles) is not picked up.
		     (cond ((and (not (eq expansion 'file))
				 (functionp expansion))
			    (apply expansion arguments))
			   ((boundp expansion)
                            (setq expansion-res
                                  (apply (symbol-value expansion) arguments))
                            (when (eq expansion 'file)
                              ;; Advance past the file name in order to
                              ;; prevent expanding any substring of it.
                              (setq pos (+ pos (length expansion-res))))
			    expansion-res)
			   (t
			    (error "Nonexpansion %s" expansion)))))
      (if (stringp string)
	  (setq command
		(replace-match string t t command)))))
  command)

(defun TeX--master-or-region-file-with-extra-quotes
    (file-fn &optional extension nondirectory ask extra)
  "Return file name with quote for shell.
Helper function of `TeX-command-expand'.

This is a kind of template.  How to use:
Fix, by `apply-partially', the first argument FILE-FN as one of
the three functions `TeX-master-file', `TeX-region-file' or
`TeX-active-master'.  Then the result is just a wrapper for that
function suitable in `TeX-command-expand'.

As a wrapper described above, it passes EXTENSION, NONDIRECTORY
and ASK to the \"bare\" function as-is, and arranges the returned
file name for use with command shell.  I.e. it encloses the file
name with space within quotes `\"' first when \" \\input\" is
supplemented (indicated by dynamically binded variable
`TeX-command-text' having string value.)  It also encloses the
file name within \\detokenize{} when the following three
conditions are met:
1. compiling with standard (pdf)LaTeX or upLaTeX
2. \" \\input\" is supplemented
3. EXTRA is non-nil (default when expanding \"%T\")"
  (shell-quote-argument
   (let* ((raw (funcall file-fn extension nondirectory ask))
	  ;; String `TeX-command-text' means that the file name is
	  ;; given through \input command.
	  (quote-for-space (if (and (stringp TeX-command-text)
				    (string-match " " raw))
			       "\"" "")))
     (format
      (if (and extra
	       (stringp TeX-command-text)
	       (memq major-mode '(latex-mode doctex-mode))
	       (memq TeX-engine '(default uptex)))
	  ;; Since TeXLive 2018, the default encoding for LaTeX
	  ;; files has been changed to UTF-8 if used with
	  ;; classic TeX or pdfTeX.  I.e.,
	  ;; \usepackage[utf8]{inputenc} is enabled by default
	  ;; in (pdf)latex.
	  ;; c.f. LaTeX News issue 28
	  ;; Due to this change, \detokenize is required to
	  ;; recognize non-ascii characters in the file name
	  ;; when \input precedes.
	  "\\detokenize{ %s }" "%s")
      (concat quote-for-space raw quote-for-space)))))

(defun TeX-check-files (derived originals extensions)
  "Check if DERIVED is newer than any of the ORIGINALS.
Try each original with each member of EXTENSIONS, in all directories
in `TeX-check-path'.  Returns true if any of the ORIGINALS with any of the
EXTENSIONS are newer than DERIVED.  Will prompt to save the buffer of any
ORIGINALS which are modified but not saved yet."
  (let (existingoriginals
        found
	(extensions (TeX-delete-duplicate-strings extensions))
        (buffers (buffer-list)))
    (dolist (path (TeX-delete-duplicate-strings
		   (mapcar (lambda (dir)
			     (expand-file-name (file-name-as-directory dir)))
			   (append
			    TeX-check-path
			    ;; In `TeX-command-default', this function is used to
			    ;; check whether bibliography databases are newer
			    ;; than generated *.bbl files, but bibliography
			    ;; database are relative to `TeX-master-directory'
			    ;; and the test can be run also from included files
			    ;; that are in directories different from
			    ;; `TeX-master-directory'.
			    (list (TeX-master-directory))))))
      (dolist (orig originals)
	(dolist (ext extensions)
	  (let ((filepath (concat path orig "." ext)))
	    (if (or (file-exists-p filepath)
		    (get-file-buffer filepath))
                (setq existingoriginals (cons filepath existingoriginals)))))))
    (while buffers
      (let* ((buffer (car buffers))
             (name (buffer-file-name buffer)))
        (setq buffers (cdr buffers))
        (if (and name (member name existingoriginals))
            (progn
              (and (buffer-modified-p buffer)
                   (or (not TeX-save-query)
                       (y-or-n-p (concat "Save file "
                                         (buffer-file-name buffer)
                                         "? ")))
                   (with-current-buffer buffer (save-buffer)))))))
    (dolist (eo existingoriginals)
      (if (file-newer-than-file-p eo derived)
          (setq found t)))
    found))

(defcustom TeX-command-sequence-max-runs-same-command 4
  "Maximum number of runs of the same command."
  :type 'integer
  :group 'TeX-command)

(defcustom TeX-command-sequence-max-runs 12
  "Maximum number of total runs."
  :type 'integer
  :group 'TeX-command)

(defvar TeX-command-sequence-count-same-command 1
  "Counter for the runs of the same command in `TeX-command-sequence'.")

(defvar TeX-command-sequence-count 1
  "Counter for the total runs of `TeX-command-sequence'.")

(defvar TeX-command-sequence-last-command nil
  "Last command run in `TeX-command-sequence'.")

(defvar TeX-command-sequence-sentinel nil
  "Sentinel for `TeX-command-sequence'.")

(defvar TeX-command-sequence-file-function nil
  "File function for `TeX-command-sequence'.")

(defvar TeX-command-sequence-command nil
  "Command argument for `TeX-command-sequence'.

It is set in `TeX-command-sequence' and used in
`TeX-command-sequence-sentinel' to call again
`TeX-command-sequence' with the appropriate command argument.")

(defun TeX-command-sequence (command &optional reset file-fn)
  "Run a sequence of TeX commands defined by COMMAND.

The COMMAND argument may be

  * nil: no command will be run in this case

  * a string with a command from `TeX-command-list'

  * a non-nil list of strings, which are commands from
    `TeX-command-list'; the car of the list is used as command to
    be executed in the first run of `TeX-command-sequence', the
    cdr of the list will be passed to the function in the next
    run, etc.

  * a function name, returning a string which is command from
    `TeX-command-list'; it will be funcall'd (without arguments!)
    and used again in the next run of `TeX-command-sequence'.

  * with any other value the function `TeX-command-default' is
    used to determine the command to run, until a stopping
    condition is met.

This function runs at most
`TeX-command-sequence-max-runs-same-command' times the same
command in a row, and `TeX-command-sequence-max-runs' times in
total in any case.  It ends when `TeX-command-Show' is the
command to be run.

A non-nil value for the optional argument RESET means this is the
first run of the function and some variables need to be reset.

FILE-FN is a function of zero arguments returning the current
filename.  Valid choices are `TeX-master-file' (default if
omitted) and `TeX-region-file'."
  (setq TeX-command-sequence-file-function (or file-fn #'TeX-master-file))
  (if (null command)
      (message "No command to run.")
    (let (cmd process)
      (cond
       ((stringp command)
	(setq cmd command
	      TeX-command-sequence-command nil))
       ((listp command)
	(setq cmd (pop command)
	      TeX-command-sequence-command command))
       ((functionp command)
	(setq cmd (funcall command)
	      TeX-command-sequence-command command))
       (t
	(setq cmd (TeX-command-default
		   ;; File function should be called with nil `nondirectory'
		   ;; argument, otherwise `TeX-command-sequence' won't work in
		   ;; included files not placed in `TeX-master-directory'.  In
		   ;; addition, `TeX-master-file' is called with the third
		   ;; argument (`ask') set to t, so that the master file is
		   ;; properly set.  This is also what `TeX-command-master'
		   ;; does.
		   (funcall TeX-command-sequence-file-function nil nil t))
	      TeX-command-sequence-command t)))
      (TeX-command cmd TeX-command-sequence-file-function 0)
      (when reset
	(setq TeX-command-sequence-count-same-command 1
	      TeX-command-sequence-count 1
	      TeX-command-sequence-last-command nil))
      (cond
       ;; Stop when the same command has been run
       ;; `TeX-command-sequence-max-runs-same-command' times in a row.
       ((>= TeX-command-sequence-count-same-command
	    TeX-command-sequence-max-runs-same-command)
	(message "Stopping after running %S %d times in a row."
		 TeX-command-sequence-last-command
		 TeX-command-sequence-count-same-command))
       ;; Stop when there have been `TeX-command-sequence-max-runs' total
       ;; compilations.
       ((>= TeX-command-sequence-count TeX-command-sequence-max-runs)
	(message "Stopping after %d compilations." TeX-command-sequence-count))
       ;; The command just run is `TeX-command-Show'.
       ((equal command TeX-command-Show))
       ;; In any other case continue: increase counters (when needed), update
       ;; `TeX-command-sequence-last-command' and run the sentinel.
       (t
	(if (equal cmd TeX-command-sequence-last-command)
	    (setq TeX-command-sequence-count-same-command
		  (1+ TeX-command-sequence-count-same-command))
	  (setq TeX-command-sequence-count-same-command 1))
	(setq TeX-command-sequence-count (1+ TeX-command-sequence-count)
	      TeX-command-sequence-last-command cmd)
	(and (setq process (get-buffer-process (current-buffer)))
	     (setq TeX-command-sequence-sentinel (process-sentinel process))
	     (set-process-sentinel process
                                   #'TeX-command-sequence-sentinel)))))))

(defcustom TeX-save-query t
  "*If non-nil, ask user for permission to save files before starting TeX."
  :group 'TeX-command
  :type 'boolean)

(defvar TeX-command-history nil)

(defun TeX-command-default (name)
  "Guess the next command to be run on NAME."
  (let ((command-next nil))
    (cond (;; name might be absolute or relative, so expand it for
	   ;; comparison.
	   (if (string-equal (expand-file-name name)
			     (expand-file-name (TeX-region-file)))
	       (TeX-check-files (concat name "." (TeX-output-extension))
				;; Each original will be checked for all dirs
				;; in `TeX-check-path' so this needs to be just
				;; a filename without directory.
				(list (file-name-nondirectory name))
				TeX-file-extensions)
	     (TeX-save-document (TeX-master-file)))
	   TeX-command-default)
	  ((and (memq major-mode '(doctex-mode latex-mode))
		;; Want to know if bib file is newer than .bbl
		;; We don't care whether the bib files are open in emacs
		(TeX-check-files (concat name ".bbl")
				 (mapcar #'car
					 (LaTeX-bibliography-list))
				 (append BibTeX-file-extensions
					 TeX-Biber-file-extensions)))
	   ;; We should check for bst files here as well.
	   (if LaTeX-using-Biber TeX-command-Biber TeX-command-BibTeX))
	  ((and
	    ;; Rationale: makeindex should be run when final document is almost
	    ;; complete (see
	    ;; https://tex-talk.net/2012/09/dont-forget-to-run-makeindex/),
	    ;; otherwise, after following latex runs, index pages may change due
	    ;; to changes in final document, resulting in extra makeindex and
	    ;; latex runs.
	    (member
	     (setq command-next
		   (TeX-process-get-variable
		    name
		    'TeX-command-next
		    (or (and TeX-PDF-mode (TeX-PDF-from-DVI))
			TeX-command-Show)))
	     (list "Dvips" "Dvipdfmx" TeX-command-Show))
	    (cdr (assoc (expand-file-name (concat name ".idx"))
			LaTeX-idx-changed-alist)))
	   "Index")
	  (command-next)
	  (TeX-command-Show))))

(defun TeX-command-query (name)
  "Query the user for what TeX command to use."
  (let* ((default (TeX-command-default name))
         (completion-ignore-case t)
         (answer (or TeX-command-force
                     (completing-read
                      (concat "Command (default " default "): ")
                      (TeX-mode-specific-command-list major-mode) nil t
                      nil 'TeX-command-history default))))
    ;; If the answer is "latex" it will not be expanded to "LaTeX"
    (setq answer (car-safe (TeX-assoc answer TeX-command-list)))
    (if (and answer
             (not (string-equal answer "")))
        answer
      default)))

(defvar TeX-command-next nil
  "The default command next time `TeX-command' is invoked.")

 (make-variable-buffer-local 'TeX-command-next)

(defun TeX-printer-query (&optional queue)
  "Query the user for a printer name.
QUEUE is non-nil when we are checking for the printer queue."
  (let (command element printer)
    (if queue
	(unless (setq element 2 command TeX-queue-command)
	  (error "Need to customize `TeX-queue-command'"))
      (unless (setq element 1 command TeX-print-command)
	  (error "Need to customize `TeX-print-command'")))
    (while (progn
	     (setq printer (if TeX-printer-list
			       (let ((completion-ignore-case t))
				 (completing-read
				  (format "Printer%s: "
					  (if TeX-printer-default
					      (format " (default %s)" TeX-printer-default) ""))
				  TeX-printer-list))
			     ""))
	     (setq printer (or (car-safe (TeX-assoc printer TeX-printer-list))
			       printer))
	     (not (if (or (null printer) (string-equal "" printer))
		      (setq printer TeX-printer-default)
		    (setq TeX-printer-default printer)))))

    (let ((expansion (let ((entry (assoc printer TeX-printer-list)))
		       (or (nth element entry)
			   command))))
      (if (string-match "%p" printer)
	  (error "Don't use %s in printer names" "%p"))
      (while (string-match "%p" expansion)
	(setq expansion (replace-match printer t t expansion 0)))
      expansion)))

(defun TeX-style-check (styles)
  "Check STYLES compared to the current style options."
  (let ((files (TeX-style-list)))
    (while (and styles
		(not (TeX-member (car (car styles)) files 'string-match)))
      (setq styles (cdr styles))))
  (if styles
      (nth 1 (car styles))
    ""))

(defun TeX-output-extension ()
  "Get the extension of the current TeX output file."
  (if (listp TeX-output-extension)
      (car TeX-output-extension)
    (or (TeX-process-get-variable (TeX-active-master)
				  'TeX-output-extension
				  TeX-output-extension)
	TeX-output-extension)))

(defun TeX-view-mouse (event)
  "Start `TeX-view' at mouse position."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (goto-char (posn-point (event-start event)))
    (TeX-view)))

(defun TeX-region-update-point ()
  "Syncs the location of point in the region file with the current file.

Thereafter, point in the region file is on the same text as in
the current buffer.

Do nothing in case the last command hasn't operated on the region
or `TeX-source-correlate-mode' is disabled."
  (when (and TeX-current-process-region-p TeX-source-correlate-mode)
    (let ((region-buf (get-file-buffer (TeX-region-file t)))
	  (orig-line (TeX-current-offset))
	  (pos-in-line (- (point) (max (line-beginning-position)
				       (or TeX-command-region-begin
					   (region-beginning))))))
      (when region-buf
	(with-current-buffer region-buf
	  (goto-char (point-min))
	  (when (re-search-forward "!offset(\\(-?[0-9]+\\)" nil t)
	    (let ((offset (string-to-number (match-string 1))))
	      (goto-char (point-min))
	      (forward-line (- orig-line offset))
	      (forward-char pos-in-line))))))))

(defun TeX-view ()
  "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
  (interactive)
  (let ((output-file (TeX-active-master (TeX-output-extension))))
    (if (file-exists-p output-file)
	(TeX-command "View" 'TeX-active-master 0)
      (message "Output file %S does not exist." output-file))))

(defun TeX-output-style-check (styles)
  "Check STYLES compared to the current view output file extension and
the current style options."
  (let ((ext  (TeX-output-extension))
	(files (TeX-style-list)))
    (while (and
	    styles
	    (or
	     (not (string-match (car (car styles)) ext))
	     (let ((style (nth 1 (car styles))))
	       (cond
		((listp style)
		 (while
		     (and style
			  (TeX-member (car style) files 'string-match))
		   (setq style (cdr style)))
		 style)
		((not (TeX-member style files 'string-match)))))))
      (setq styles (cdr styles)))
    (if styles
	(nth 2 (car styles))
      "%v")))

;;; Command Hooks

(defvar TeX-after-compilation-finished-functions nil
  "Hook being run after TeX/LaTeX/ConTeXt finished successfully.
The functions in this hook are run with the DVI/PDF output file
given as argument.  Using this hook can be useful for updating
the viewer automatically after re-compilation of the document.

If you use an emacs-internal viewer such as `doc-view-mode' or
`pdf-view-mode', add `TeX-revert-document-buffer' to this hook.")

(make-obsolete-variable 'TeX-after-TeX-LaTeX-command-finished-hook
			'TeX-after-compilation-finished-functions
			"11.89")

(defun TeX-revert-document-buffer (file)
  "Revert the buffer visiting FILE.
This function is intended to be used in
`TeX-after-compilation-finished-functions' for users that view
their compiled document with an emacs viewer such as
`doc-view-mode' or `pdf-view-mode'.  (Note that this function
just calls `revert-buffer' in the respective buffer and thus
requires that the corresponding mode defines a sensible
`revert-buffer-function'.)"
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (with-current-buffer buf
	(revert-buffer nil t t)))))

(defvar TeX-after-start-process-function
  #'TeX-adjust-process-coding-system
  "Function to adjust coding system of an asynchronous process.
Called with one argument PROCESS.")

(defun TeX-adjust-process-coding-system (process)
  "Adjust coding system of PROCESS to suitable value.
Usually coding system is the same as the TeX file with eol format
adjusted to OS default value.  Take care of Japanese TeX, which
requires special treatment."
  (if (and (boundp 'japanese-TeX-mode)
	   (fboundp 'japanese-TeX-set-process-coding-system)
	   (with-current-buffer TeX-command-buffer
	     japanese-TeX-mode))
      (japanese-TeX-set-process-coding-system process)
    (let ((cs (coding-system-base (with-current-buffer TeX-command-buffer
				    buffer-file-coding-system))))
      ;; The value of `buffer-file-coding-system' is sometimes
      ;; undecided-{unix,dos,mac}.  That happens when the file
      ;; contains no multibyte chars and only end of line format is
      ;; determined.  Emacs lisp reference recommends not to use
      ;; undecided-* for process coding system, so it might seem
      ;; reasonable to change `undecided' into some fixed coding
      ;; system like this:
      ;; (if (eq 'undecided cs)
      ;;     (setq cs 'utf-8))
      ;; However, that can lose when the following conditions are met:
      ;; (1) The document is divided into multiple files.
      ;; (2) The command buffer contains no multibyte chars.
      ;; (3) The other files contain mutlibyte chars and saved in a
      ;;     coding system other than the one chosen above.
      ;; So we leave `undecided' unchanged here.  Although `undecided'
      ;; is not quite safe for the coding system for encoding, i.e.,
      ;; keyboard input to the TeX process, we expect that this does
      ;; not raise serious problems because it is pretty rare that TeX
      ;; process needs keyboard input of multibyte chars.

      ;; `utf-8-with-signature' (UTF-8 with BOM) doesn't suit at all
      ;; for the coding system for encoding because it always injects
      ;; 3-byte BOM in front of its return value (even when the string
      ;; to be sent has only ascii characters!)  Thus we change it
      ;; into `utf-8'.  On decoding, `utf-8' can decode UTF-8 with
      ;; BOM.  So it is safe for both decoding and encoding.
      (if (eq cs 'utf-8-with-signature)
	  (setq cs 'utf-8))

      ;; Eol format of TeX files can differ from OS default. TeX
      ;; binaries accept all type of eol format in the given files
      ;; and output messages according to OS default.  So we set eol
      ;; format to OS default value.
      (setq cs (coding-system-change-eol-conversion
		cs
		;; The eol of macosX is LF, not CR.  So we choose
		;; other than `unix' only for w32 system.
		;; FIXME: what should we do for cygwin?
		(if (eq system-type 'windows-nt) 'dos 'unix)))
      (set-process-coding-system process cs cs))))

(defcustom TeX-show-compilation nil
  "*If non-nil, show output of TeX compilation in other window."
  :group 'TeX-command
  :type 'boolean)

(defun TeX-run-command (name command file)
  "Create a process for NAME using COMMAND to process FILE.
Return the new process."
  (let ((default TeX-command-default)
	(buffer (TeX-process-buffer-name file))
	(dir (TeX-master-directory))
	(command-buff (current-buffer)))
    (TeX-process-check file)		; Check that no process is running
    (setq-default TeX-command-buffer command-buff)
    (get-buffer-create buffer)
    (set-buffer buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (set (make-local-variable 'line-number-display-limit) 0)
    (setq TeX-output-extension nil)
    (set (make-local-variable 'TeX-command-buffer) command-buff)
    (if dir (cd dir))
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    (TeX-output-mode)
    (if TeX-show-compilation
	(display-buffer buffer)
      (message "Type `%s' to display results of compilation."
	       (substitute-command-keys
		"\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
    (setq TeX-parse-function #'TeX-parse-command)
    (setq TeX-command-default default)
    (setq TeX-sentinel-function
	  (lambda (_process name)
	    (message (concat name ": done."))))
    (if TeX-process-asynchronous
	(let ((process (start-process name buffer TeX-shell
				      TeX-shell-command-option command)))
	  (if TeX-after-start-process-function
	      (funcall TeX-after-start-process-function process))
	  (TeX-command-mode-line process)
	  (set-process-filter process #'TeX-command-filter)
	  (set-process-sentinel process #'TeX-command-sentinel)
	  (set-marker (process-mark process) (point-max))
	  (setq compilation-in-progress (cons process compilation-in-progress))
	  process)
      (setq mode-line-process ": run")
      (set-buffer-modified-p (buffer-modified-p))
      (sit-for 0)				; redisplay
      (call-process TeX-shell nil buffer nil
		    TeX-shell-command-option command))))

(defun TeX-run-set-command (name command)
  "Remember TeX command to use to NAME and set corresponding output extension."
  (setq TeX-command-default name
	TeX-output-extension
	(if (and (null (TeX-PDF-from-DVI)) TeX-PDF-mode) "pdf" "dvi"))
  (let ((case-fold-search t)
	(lst TeX-command-output-list))
    (while lst
      (if (string-match (car (car lst)) command)
	  (setq TeX-output-extension (car (cdr (car lst)))
		lst nil)
	(setq lst (cdr lst))))))

(defun TeX-run-format (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (TeX-run-set-command name command)
  (let ((buffer (TeX-process-buffer-name file))
	(process (TeX-run-command name command file)))
    ;; Hook to TeX debugger.
    (with-current-buffer buffer
      (TeX-parse-reset)
      (setq TeX-parse-function #'TeX-parse-TeX)
      (setq TeX-sentinel-function #'TeX-TeX-sentinel)
      (if TeX-process-asynchronous
	  (progn
	    ;; Updating the mode line.
	    (setq TeX-current-page "[0]")
	    (TeX-format-mode-line process)
	    (set-process-filter process #'TeX-format-filter)))
      process)))

(defvar TeX-error-report-switches nil
  "Reports presence of errors after `TeX-run-TeX'.
To test whether the current buffer has a compile error from last
run of `TeX-run-TeX', use
  (TeX-error-report-has-errors-p)")

(defun TeX-error-report-has-errors-p ()
  "Return non-nil if current buffer has compile errors from last TeX run."
  (plist-get TeX-error-report-switches (intern (TeX-master-file))))

(defun TeX-run-TeX (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."

  ;; Save information in TeX-error-report-switches
  ;; Initialize error to nil (no error) for current master.
  ;; Presence of error is reported inside `TeX-TeX-sentinel-check'
  (let ((current-master (TeX-master-file))
	(idx-file nil) (element nil))
    ;; the current master file is saved because error routines are
    ;; parsed in other buffers;
    (setq TeX-error-report-switches
	  (plist-put TeX-error-report-switches
		     'TeX-current-master current-master))
    ;; reset error to nil (no error)
    (setq TeX-error-report-switches
	  (plist-put TeX-error-report-switches
		     (intern current-master) nil))

    ;; Store md5 hash of the index file before running LaTeX.
    (and (memq major-mode '(doctex-mode latex-mode))
	 (prog1 (file-exists-p
		 (setq idx-file (expand-file-name (concat file ".idx"))))
	   ;; In order to avoid confusion and pollution of
	   ;; `LaTeX-idx-md5-alist', remove from this alist all md5 hashes of
	   ;; the current index file.  Note `assq-delete-all' doesn't work with
	   ;; string keys and has problems with non-list elements in Emacs 21
	   ;; (see file tex-site.el).
	   (while (setq element (assoc idx-file LaTeX-idx-md5-alist))
	     (setq LaTeX-idx-md5-alist (delq element LaTeX-idx-md5-alist))))
	 (with-temp-buffer
	   (insert-file-contents idx-file)
	   (push (cons idx-file (md5 (current-buffer))) LaTeX-idx-md5-alist))))

  ;; can we assume that TeX-sentinel-function will not be changed
  ;; during (TeX-run-format ..)? --pg
  ;; rather use let* ? --pg

  (if TeX-interactive-mode
      (TeX-run-interactive name command file)
    (let* ((sentinel-function TeX-sentinel-default-function)
           (process (TeX-run-format name command file)))
      (setq TeX-sentinel-function sentinel-function)
      (if TeX-process-asynchronous
          process
        (TeX-synchronous-sentinel name file process)))))

;; backward compatibilty

(defalias 'TeX-run-LaTeX 'TeX-run-TeX)


(defun TeX-run-BibTeX (name command file)
  "Create a process for NAME using COMMAND to format FILE with BibTeX."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-BibTeX-sentinel)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-Biber-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-dvips (name command file)
  "Create a process for NAME using COMMAND to convert FILE with dvips."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-dvips-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-dvipdfmx (name command file)
  "Create a process for NAME using COMMAND to convert FILE with dvipdfmx."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-dvipdfmx-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-ps2pdf (name command file)
  "Create a process for NAME using COMMAND to convert FILE with ps2pdf."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-ps2pdf-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-index (name command file)
  "Create a process for NAME using COMMAND to compile the index file."
  (let ((process (TeX-run-command name command file))
	(element nil))
    (setq TeX-sentinel-function #'TeX-index-sentinel)
    ;; Same cleaning as that for `LaTeX-idx-md5-alist' in `TeX-run-TeX'.
    (while (setq element
		 ;; `file' has been determined in `TeX-command-buffer', while
		 ;; this function has `TeX-master-directory' as
		 ;; `default-directory', then we have to expand `file' file-name
		 ;; in the same directory of `TeX-command-buffer'.
		 (assoc (with-current-buffer TeX-command-buffer
			    (expand-file-name (concat file ".idx")))
			LaTeX-idx-changed-alist))
      (setq LaTeX-idx-changed-alist (delq element LaTeX-idx-changed-alist)))
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-compile (_name command _file)
  "Ignore first and third argument, start compile with second argument."
  (let ((default-directory (TeX-master-directory)))
    (setq TeX-command-buffer (compile command)))
  ;; Make `compilation-mode' recognize file names with spaces.
  ;; (bug#36483)
  ;; FIXME: This is just an ad-hoc workaround and it's better to fix
  ;; the regular expression in compile.el properly, if possible.  But
  ;; there was no response to such request in emacs-devel@gnu.org.
  (with-current-buffer TeX-command-buffer
    (make-local-variable 'compilation-error-regexp-alist)
    ;; Add slightly modified entry of the one associated with `comma'
    ;; in `compilation-error-regexp-alist-alist' to pick up file names
    ;; with spaces.
    (add-to-list 'compilation-error-regexp-alist
		 '("^\"\\([^,\"\n\t]+\\)\", line \\([0-9]+\\)\
\\(?:[(. pos]+\\([0-9]+\\))?\\)?[:.,; (-]\\( warning:\\|[-0-9 ]*(W)\\)?" 1 2 3 (4))
		 t)))

(defun TeX-run-shell (_name command _file)
  "Ignore first and third argument, start shell-command with second argument."
  (let ((default-directory (TeX-master-directory)))
    (shell-command command)
    (if (eq system-type 'ms-dos)
	(redraw-display))))

(defun TeX-run-discard (_name command _file)
  "Start COMMAND as process, discarding its output.
NAME and FILE are ignored."
  (let ((default-directory (TeX-master-directory)))
    (call-process TeX-shell
		  nil 0 nil
		  TeX-shell-command-option
		  command)))

(defun TeX-run-discard-foreground (_name command _file)
  "Call process with second argument in the foreground, discarding its output.
With support for MS-DOS, especially when dviout is used with PC-9801 series."
  (if (and (boundp 'dos-machine-type) (eq dos-machine-type 'pc98)) ;if PC-9801
      (send-string-to-terminal "\e[2J")) ; clear screen
  (call-process TeX-shell (if (eq system-type 'ms-dos) "con") nil nil
		TeX-shell-command-option command)
  (if (eq system-type 'ms-dos)
      (redraw-display)))
(defalias 'TeX-run-dviout 'TeX-run-discard-foreground)

(defun TeX-run-background (name command _file)
  "Start process with second argument, show output when and if it arrives."
  (let ((dir (TeX-master-directory)))
    (set-buffer (get-buffer-create "*TeX background*"))
    (if dir (cd dir))
    (erase-buffer)
    (let ((process (start-process (concat name " background")
				  nil TeX-shell
				  TeX-shell-command-option command)))
      (if TeX-after-start-process-function
	  (funcall TeX-after-start-process-function process))
      (set-process-filter process #'TeX-background-filter)
      (set-process-query-on-exit-flag process nil))))

(defun TeX-run-silent (name command _file)
  "Start process with second argument."
  (let ((dir (TeX-master-directory)))
    (set-buffer (get-buffer-create "*TeX silent*"))
    (if dir (cd dir))
    (erase-buffer)
    (let ((process (start-process (concat name " silent")
				  (current-buffer) TeX-shell
				  TeX-shell-command-option command)))
      (if TeX-after-start-process-function
	  (funcall TeX-after-start-process-function process))
      (set-process-query-on-exit-flag process nil))))

(defun TeX-run-interactive (name command file)
  "Run TeX interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts user
interaction. If you return to the file buffer after the TeX run,
Error parsing on \\[next-error] should work with a bit of luck."
  (TeX-run-set-command name command)
  (require 'comint)
  (let ((default TeX-command-default)
	(buffer (TeX-process-buffer-name file))
	(process nil)
	(dir (TeX-master-directory))
	(command-buff (current-buffer))
	(sentinel-function TeX-sentinel-default-function)) ; inherit from major mode
    (TeX-process-check file)		; Check that no process is running
    (setq-default TeX-command-buffer command-buff)
    (with-output-to-temp-buffer buffer)
    (set-buffer buffer)
    (set (make-local-variable 'TeX-command-buffer) command-buff)
    (setq buffer-read-only nil)
    (if dir (cd dir))
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    (comint-exec buffer name TeX-shell nil
		 (list TeX-shell-command-option command))
    (comint-mode)
    (add-hook 'comint-output-filter-functions #'TeX-interactive-goto-prompt)
    (setq mode-name name)
    (setq TeX-command-default default)
    (setq process (get-buffer-process buffer))
    (if TeX-after-start-process-function
	(funcall TeX-after-start-process-function process))
    (TeX-command-mode-line process)
    (set-process-sentinel process #'TeX-command-sentinel)
    (set-marker (process-mark process) (point-max))
    (setq compilation-in-progress (cons process compilation-in-progress))
    (TeX-parse-reset)
    (setq TeX-parse-function #'TeX-parse-TeX)
    ;; use the sentinel-function that the major mode sets, not the LaTeX one
    (setq TeX-sentinel-function sentinel-function)))

(defun TeX-run-function (_name command _file)
  "Execute Lisp function or function call given as the string COMMAND.
Parameters NAME and FILE are ignored."
  (let ((fun (car (read-from-string command))))
    (if (functionp fun) (funcall fun) (eval fun))))

(defun TeX-run-discard-or-function (name command file)
  "Start COMMAND as process or execute it as a Lisp function.
If run as a process, the output is discarded.  COMMAND is
expected to be a string.  NAME and FILE are ignored."
  (if (functionp (car (read-from-string command)))
      (TeX-run-function name command file)
    (TeX-run-discard name command file)))

(defun TeX-run-ispell-on-document (_command _ignored _name)
  "Run ispell on all open files belonging to the current document.
This function is *obsolete* and only here for compatibility
reasons.  Use `TeX-run-function' instead."
  (interactive)
  (TeX-ispell-document ""))


;;; Command Sentinels

(defun TeX-synchronous-sentinel (name file result)
  "Process TeX command output buffer after the process dies."
  (let ((buffer (TeX-process-buffer (file-name-nondirectory file))))
    (with-current-buffer buffer

      ;; Append post-mortem information to the buffer
      (goto-char (point-max))
      (insert "\n" mode-name (if (and result (zerop result))
				 " finished" " exited") " at "
	      (substring (current-time-string) 0 -5))
      (setq mode-line-process ": exit")

      ;; Do command specific actions.
      (setq TeX-command-next TeX-command-Show)
      (goto-char (point-min))
      (apply TeX-sentinel-function nil name nil)

      ;; Force mode line redisplay soon
      (set-buffer-modified-p (buffer-modified-p)))))

(defun TeX-command-sentinel (process msg)
  "Process TeX command output buffer after the process dies."
  ;; Set `TeX-transient-master' here because `preview-parse-messages'
  ;; may open files and thereby trigger master file questions which we
  ;; don't want and need because we already know the master.  Use
  ;; `TeX-master-file' instead of `TeX-active-master' to determine the
  ;; master because the region file should never be the master.
  (let* ((TeX-transient-master (TeX-master-file))
	 (buffer (process-buffer process))
	 (name (process-name process)))
    (cond ((null (buffer-name buffer))	; buffer killed
	   (set-process-buffer process nil)
	   (set-process-sentinel process nil))
	  ((memq (process-status process) '(signal exit))
	   (with-current-buffer buffer

	     ;; Append post-mortem information to the buffer
	     (goto-char (point-max))
	     (insert-before-markers "\n" mode-name " " msg)
	     (forward-char -1)
	     (insert " at "
		     (substring (current-time-string) 0 -5))
	     (forward-char 1)

	     ;; Do command specific actions.
	     (TeX-command-mode-line process)
	     (setq TeX-command-next TeX-command-Show)
	     (goto-char (point-min))
	     (apply TeX-sentinel-function process name nil)


	     ;; If buffer and mode line will show that the process
	     ;; is dead, we can delete it now.  Otherwise it
	     ;; will stay around until M-x list-processes.
	     (delete-process process)

	     ;; Force mode line redisplay soon
	     (set-buffer-modified-p (buffer-modified-p))))))
  (setq compilation-in-progress (delq process compilation-in-progress)))


(defvar TeX-sentinel-function (lambda (_process _name) nil)
  "Hook to cleanup TeX command buffer after temination of PROCESS.
NAME is the name of the process.")

(make-variable-buffer-local 'TeX-sentinel-function)


(defvar TeX-sentinel-default-function (lambda (_process _name) nil)
  "Default for `TeX-sentinel-function'.  To be set in major mode.
Hook to cleanup TeX command buffer after temination of PROCESS.
NAME is the name of the process.")

(make-variable-buffer-local 'TeX-sentinel-default-function)

(defun TeX-TeX-sentinel (process name)
  "Cleanup TeX output buffer after running TeX.

Parse the output buffer to collect errors and warnings if the
variable `TeX-parse-all-errors' is non-nil.

Open the error overview if
`TeX-error-overview-open-after-TeX-run' is non-nil and there are
errors or warnings to show."
  (if (TeX-TeX-sentinel-check process name)
      (progn
	(if TeX-parse-all-errors
	    (TeX-parse-all-errors))
	(if (and TeX-error-overview-open-after-TeX-run
		 (TeX-error-overview-make-entries
		  (TeX-master-directory) (TeX-active-buffer)))
	    (TeX-error-overview)))
    (message (concat name ": formatted " (TeX-current-pages)))
    (let (dvi2pdf)
	(if (with-current-buffer TeX-command-buffer
	   (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
	 (setq TeX-command-next dvi2pdf)
       (setq TeX-command-next TeX-command-Show)))))

(defun TeX-current-pages ()
  "Return string indicating the number of pages formatted."
  (cond ((null TeX-current-page)
	 "some pages")
	((string-match "[^0-9]1[^0-9]" TeX-current-page)
	 (concat TeX-current-page " page"))
	(t
	 (concat TeX-current-page " pages"))))

(defun TeX-TeX-sentinel-check (process name)
  "Cleanup TeX output buffer after running TeX.
Return nil ifs no errors were found."
  (save-excursion
    (goto-char (point-max))
    (cond
     ((and (string-match "ConTeXt" name) (boundp 'ConTeXt-Mark-version)
	   (with-current-buffer TeX-command-buffer
	     (string= ConTeXt-Mark-version "IV")))
      (when (re-search-backward " > result saved in file: \\(.*?\\), " nil t)
	(let ((output-file (TeX-match-buffer 1)))
	  ;; Shave off quotation marks if present.
	  (when (string-match "\\`\"\\(.*\\)\"\\'" output-file)
	    (setq output-file (match-string 1 output-file)))
	  (setq TeX-output-extension
		(if (string-match "\\.\\([^.]*\\)$" output-file)
		    (match-string 1 output-file)
		  "dvi")))
	(if (re-search-forward ", \\([0-9]+\\) shipped pages, " nil t)
	    (setq TeX-current-page (concat "{" (TeX-match-buffer 1) "}")))))
     (t
      (if (re-search-backward "^Output written on \\(.*?\\) (\\([0-9]+\\) page"
			      nil t)
	  (let ((output-file (TeX-match-buffer 1)))
	    (setq TeX-current-page (concat "{" (TeX-match-buffer 2) "}"))
	    ;; Shave off quotation marks if present.
	    (when (string-match "\\`\"\\(.*\\)\"\\'" output-file)
	      (setq output-file (match-string 1 output-file)))
	    (setq TeX-output-extension
		  (if (string-match "\\.\\([^.]*\\)$" output-file)
		      (match-string 1 output-file)
		    "dvi")))))))
  (if process (TeX-format-mode-line process))
  (if (re-search-forward "^\\(!\\|.*:[0-9]+:\\) " nil t)
      (progn
	(message "%s errors in `%s'. Use %s to display." name (buffer-name)
		 (substitute-command-keys
		  "\\<TeX-mode-map>\\[TeX-next-error]"))
	(setq TeX-command-next TeX-command-default)
	;; error reported to TeX-error-report-switches
	(setq TeX-error-report-switches
	      (plist-put TeX-error-report-switches
			 (intern (plist-get TeX-error-report-switches
					    'TeX-current-master))
			 t))
	t)
    (let (dvi2pdf)
	(if (with-current-buffer TeX-command-buffer
	   (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
	 (setq TeX-command-next dvi2pdf)
       (setq TeX-command-next TeX-command-Show)))
    nil))

;; This regexp should catch warnings of the type
;;   LaTeX Warning: ...
;;   LaTeX Font Warning: ...
;;   Package xyz123 Warning: ...
;;   Class xyz123 Warning: ...
(defvar LaTeX-warnings-regexp
  "\\(?:LaTeX\\|Class\\|Package\\|\*\\) [-A-Za-z0-9]* ?[Ww]arning:"
  "Regexp matching LaTeX warnings.")

(defun TeX-LaTeX-sentinel-has-warnings ()
  "Return non-nil, if the output buffer contains warnings.
Warnings can be indicated by LaTeX or packages."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^" LaTeX-warnings-regexp) nil t)))

(defun TeX-LaTeX-sentinel-has-bad-boxes ()
  "Return non-nil, if LaTeX output indicates overfull or underfull boxes."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\(Ov\\|Und\\)erfull \\\\" nil t)))

;; should go into latex.el? --pg
(defun TeX-LaTeX-sentinel (process name)
  "Cleanup TeX output buffer after running LaTeX.

Parse the output buffer to collect errors and warnings if the
variable `TeX-parse-all-errors' is non-nil.

Open the error overview if
`TeX-error-overview-open-after-TeX-run' is non-nil and there are
errors or warnings to show."
  (if TeX-parse-all-errors
      (TeX-parse-all-errors))
  (if (and TeX-error-overview-open-after-TeX-run
	   (TeX-error-overview-make-entries
	    (TeX-master-directory) (TeX-active-buffer)))
      (TeX-error-overview))
  (cond ((TeX-TeX-sentinel-check process name))
	((and (save-excursion
		(re-search-forward
		 "^Package biblatex Warning: Please (re)run Biber on the file"
		 nil t))
	      (with-current-buffer TeX-command-buffer
		(and (LaTeX-bibliography-list)
		     (TeX-check-files (TeX-master-file "bbl")
				      (TeX-style-list)
				      (append TeX-file-extensions
					      BibTeX-file-extensions
					      TeX-Biber-file-extensions)))))
	 (message "%s%s" "You should run Biber to get citations right, "
		  (TeX-current-pages))
	 (setq TeX-command-next (with-current-buffer TeX-command-buffer
				  TeX-command-Biber)))
	((and (save-excursion
		(re-search-forward
		 "^\\(?:LaTeX\\|Package natbib\\) Warning: Citation" nil t))
	      (with-current-buffer TeX-command-buffer
		(and (LaTeX-bibliography-list)
		     (TeX-check-files (TeX-master-file "bbl")
				      (TeX-style-list)
				      (append TeX-file-extensions
					      BibTeX-file-extensions
					      TeX-Biber-file-extensions)))))
	 (message "%s%s" "You should run BibTeX to get citations right, "
		  (TeX-current-pages))
	 (setq TeX-command-next (with-current-buffer TeX-command-buffer
				  TeX-command-BibTeX)))
	((re-search-forward "Package biblatex Warning: Please rerun LaTeX" nil t)
	 (message "%s%s" "You should run LaTeX again, " (TeX-current-pages))
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward "^(biblatex)\\W+Page breaks have changed" nil t)
	 (message "%s%s" "You should run LaTeX again - page breaks have changed, "
		  (TeX-current-pages))
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward "^\\(?:LaTeX Warning: Label(s)\\|\
Package natbib Warning: Citation(s)\\)" nil t)
	 (message "%s%s" "You should run LaTeX again to get references right, "
		  (TeX-current-pages))
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward
	  "^\\(?:(rerunfilecheck)\\|Package hyperref Warning:\\)\\W+\
Rerun to get outlines right" nil t)
	 (message "%s%s" "You should run LaTeX again to get outlines right, "
		  (TeX-current-pages))
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward "^LaTeX Warning: Reference" nil t)
	 (message "%s%s%s" name ": there were unresolved references, "
		  (TeX-current-pages))
	 (let (dvi2pdf)
	   (if (with-current-buffer TeX-command-buffer
		 (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
	       (setq TeX-command-next dvi2pdf)
	     (setq TeX-command-next TeX-command-Show))))
	((re-search-forward "^\\(?:LaTeX Warning: Citation\\|\
Package natbib Warning:.*undefined citations\\)" nil t)
	 (message "%s%s%s" name ": there were unresolved citations, "
		  (TeX-current-pages))
	 (let (dvi2pdf)
	   (if (with-current-buffer TeX-command-buffer
		 (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
	       (setq TeX-command-next dvi2pdf)
	     (setq TeX-command-next TeX-command-Show))))
	((re-search-forward "Package longtable Warning: Table widths have \
changed\\. Rerun LaTeX\\." nil t)
	 (message
	  "%s" "You should run LaTeX again to get table formatting right")
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward "^hf-TikZ Warning: Mark '.*' changed\\. \
Rerun to get mark in right position\\." nil t)
	 (message
	  "%s" "You should run LaTeX again to get TikZ marks in right position")
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward "^\* xsim warning: \"rerun\"" nil t)
	 (message
	  "%s" "You should run LaTeX again to synchronize exercise properties")
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward
	  "^\\(\\*\\* \\)?J?I?p?\\(La\\|Sli\\)TeX\\(2e\\)? \
\\(Version\\|ver\\.\\|<[0-9/-]*\\(?:u[^>]*\\)?>\\)" nil t)
	 (let* ((warnings (and TeX-debug-warnings
			       (TeX-LaTeX-sentinel-has-warnings)))
		(bad-boxes (and TeX-debug-bad-boxes
				(TeX-LaTeX-sentinel-has-bad-boxes)))
		(add-info (when (or warnings bad-boxes)
			    (concat " (with "
				    (when warnings "warnings")
				    (when (and warnings bad-boxes) " and ")
				    (when bad-boxes "bad boxes")
				    ")"))))
	   (message "%s" (concat name ": successfully formatted "
				 (TeX-current-pages) add-info)))
	 (let (dvi2pdf)
	   (if (with-current-buffer TeX-command-buffer
		 (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
	       (setq TeX-command-next dvi2pdf)
	     (setq TeX-command-next TeX-command-Show))))
	(t
	 (message "%s%s%s" name ": problems after " (TeX-current-pages))
	 (setq TeX-command-next TeX-command-default)))

  ;; Check whether the idx file changed.
  (let ((idx-file nil) (master nil))
    (and (file-exists-p
	  (setq idx-file
		(concat
		 (setq master
		       (with-current-buffer TeX-command-buffer
			 (expand-file-name (TeX-active-master)))) ".idx")))
	 ;; imakeidx package automatically runs makeindex, thus, we need to be
	 ;; sure .ind file isn't newer than .idx.
	 (TeX-check-files (concat master ".ind")
			  (list (file-name-nondirectory master)) '("idx"))
	 (with-temp-buffer
	   (insert-file-contents idx-file)
	   (not (equal
		 ;; Compare old md5 hash of the idx file with the new one.
		 (cdr (assoc idx-file LaTeX-idx-md5-alist))
		 (md5 (current-buffer)))))
	 (push (cons idx-file t) LaTeX-idx-changed-alist)))

  (unless (TeX-error-report-has-errors-p)
    (run-hook-with-args 'TeX-after-compilation-finished-functions
			(with-current-buffer TeX-command-buffer
			  (expand-file-name
			   (TeX-active-master (TeX-output-extension)))))))

;; should go into latex.el? --pg
(defun TeX-BibTeX-sentinel (_process _name)
  "Cleanup TeX output buffer after running BibTeX."
  (goto-char (point-max))
  (cond
   ;; Check whether BibTeX reports any warnings or errors.
   ((re-search-backward (concat
			 "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
			 "\\(warnings?\\|error messages?\\))")
                        nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "BibTeX finished with %s %s. "
		     "Type `%s' to display output.")
	     (match-string 1) (match-string 2)
	     (substitute-command-keys
	      "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "BibTeX finished successfully. "
		     "Run LaTeX again to get citations right."))))
  ;; In any case, run the default next command.
  (setq TeX-command-next TeX-command-default))

(defun TeX-Biber-sentinel (_process _name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (cond
   ((re-search-backward "^INFO - \\(WARNINGS\\|ERRORS\\): \\([0-9]+\\)" nil t)
    (message (concat "Biber finished with %s %s. "
                     "Type `%s' to display output.")
             (match-string 2) (downcase (match-string 1))
             (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]"))
    (setq TeX-command-next TeX-command-default))
   ((re-search-backward "^FATAL" nil t)
    (message (concat "Biber had a fatal error and did not finish! "
                     "Type `%s' to display output.")
             (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]"))
    (setq TeX-command-next TeX-command-Biber))
   (t
    (message (concat "Biber finished successfully. "
                     "Run LaTeX again to get citations right."))
    (setq TeX-command-next TeX-command-default))))

(defun TeX-dvips-sentinel (_process _name)
  "Cleanup TeX output buffer after running dvips."
  (goto-char (point-max))
  (cond
   ((search-backward "TeX Output exited abnormally" nil t)
    (message "Dvips failed.  Type `%s' to display output."
	     (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (if (with-current-buffer TeX-command-buffer
	  (and (equal (TeX-PDF-from-DVI) "Dvips") TeX-PDF-mode))
	(setq TeX-output-extension "ps"
	      TeX-command-next "Ps2pdf"))
    (message "Dvips finished successfully. "))))

(defun TeX-dvipdfmx-sentinel (_process _name)
  "Cleanup TeX output buffer after running dvipdfmx."
  (goto-char (point-max))
  (cond
   ((search-backward "TeX Output exited abnormally" nil t)
    (message "Dvipdfmx failed.  Type `%s' to display output."
	     (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (if (with-current-buffer TeX-command-buffer
	  (and (equal (TeX-PDF-from-DVI) "Dvipdfmx") TeX-PDF-mode))
	(setq TeX-output-extension "pdf"
	      TeX-command-next TeX-command-Show))
    (message "Dvipdfmx finished successfully. "))))

(defun TeX-ps2pdf-sentinel (_process _name)
  "Cleanup TeX output buffer after running ps2pdf."
  (goto-char (point-max))
  (cond
   ((search-backward "TeX Output exited abnormally" nil t)
    (message "ps2pdf failed.  Type `%s' to display output."
	     (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (if (with-current-buffer TeX-command-buffer
	  (and (equal (TeX-PDF-from-DVI) "Dvips") TeX-PDF-mode))
	(setq TeX-command-next TeX-command-Show
	      TeX-output-extension "pdf"))
    (message "ps2pdf finished successfully. "))))

(defun TeX-index-sentinel (_process _name)
  "Cleanup TeX output buffer after compiling index."
  (goto-char (point-max))
  (cond
   ((search-backward "TeX Output exited abnormally" nil t)
    (message "Index failed.  Type `%s' to display output."
	     (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (setq TeX-command-next TeX-command-default)
    (message (concat "Index finished successfully. "
		     "Run LaTeX again to get index right.")))))

(defun TeX-command-sequence-sentinel (process string)
  "Call the appropriate sentinel for the current process.

If there are no errors, call back `TeX-command-sequence' using
`TeX-command-sequence-command' as command argument, unless this
variable is nil."
  (with-current-buffer (process-buffer process)
    (funcall TeX-command-sequence-sentinel process string)
    (if (string-match "\\(finished\\|exited\\)" string)
	(with-current-buffer TeX-command-buffer
	  (unless
	      (or
	       (TeX-error-report-has-errors-p)
	       (null TeX-command-sequence-command))
	    (TeX-command-sequence TeX-command-sequence-command nil
				  TeX-command-sequence-file-function))))))

;;; Process Control


;; This variable is shared with `compile.el'.
;; FIXME: Then it should not be defvar'd here!
(defvar compilation-in-progress nil
  "List of compilation processes now running.")

(or (assq 'compilation-in-progress minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-in-progress " Compiling")
				 minor-mode-alist)))

(defun TeX-process-get-variable (name symbol &optional default)
  "Return the value in the process buffer for NAME of SYMBOL.

Return DEFAULT if the process buffer does not exist or SYMBOL is not
defined."
  (let ((buffer (TeX-process-buffer name)))
    (if (and buffer
	     (local-variable-p symbol buffer))
	(with-current-buffer buffer
	  (symbol-value symbol))
      default)))

(defun TeX-process-set-variable (name symbol value)
  "Set the variable SYMBOL in the process buffer to VALUE.
Return nil iff no process buffer exist."
  (let ((buffer (TeX-process-buffer name)))
    (if buffer
	(with-current-buffer buffer
	  (set symbol value)
	  t)
      nil)))

(defun TeX-process-check (name)
  "Check if a process for the TeX document NAME already exist.
If so, give the user the choice of aborting the process or the current
command."
  (let (process)
    (while (and (setq process (TeX-process name))
		(eq (process-status process) 'run))
      (cond
       ((yes-or-no-p (concat "Process `"
			     (process-name process)
			     "' for document `"
			     name
			     "' running, kill it? "))
	(delete-process process))
       ((eq (process-status process) 'run)
	   (error "Cannot have two processes for the same document"))))))

(defun TeX-process-buffer-name (name)
  "Return name of AUCTeX buffer associated with the document NAME."
  (concat "*" (abbreviate-file-name (expand-file-name name)) " output*"))

(defun TeX-process-buffer (name)
  "Return the AUCTeX buffer associated with the document NAME."
  (get-buffer (TeX-process-buffer-name name)))

(defun TeX-process (name)
  "Return AUCTeX process associated with the document NAME."
  (and TeX-process-asynchronous
       (get-buffer-process (TeX-process-buffer name))))

;;; Process Filters

(defun TeX-command-mode-line (process)
  "Format the mode line for a buffer containing output from PROCESS."
    (setq mode-line-process (concat ": "
				    (symbol-name (process-status process))))
    (set-buffer-modified-p (buffer-modified-p)))

(defun TeX-command-filter (process string)
  "Filter to process normal output."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (process-mark process))
      (insert-before-markers string)
      (set-marker (process-mark process) (point)))))

(defvar TeX-current-page nil
  "The page number currently being formatted, enclosed in brackets.")

 (make-variable-buffer-local 'TeX-current-page)

(defun TeX-format-mode-line (process)
  "Format the mode line for a buffer containing TeX output from PROCESS."
    (setq mode-line-process (concat " " TeX-current-page ": "
				    (symbol-name (process-status process))))
    (set-buffer-modified-p (buffer-modified-p)))

(defun TeX-format-filter (process string)
  "Filter to process TeX output."
  (with-current-buffer (process-buffer process)
    (let (str pos end (pt (marker-position (process-mark process))))
      (save-excursion
	(goto-char pt)
	(insert-before-markers string)
	(set-marker (process-mark process) (point))
	;; Remove line breaks at columns 79 and 80
	(while (> (point) pt)
	  (end-of-line 0)
	  (when (and (memq (- (point) (line-beginning-position)) '(79 80))
		     ;; Heuristic: Don't delete the linebreak if the next line
		     ;; is empty or starts with an opening parenthesis, or if
		     ;; point is located after a period and in the next line no
		     ;; word char follows.
		     (not (memq (char-after (1+ (point))) '(?\n ?\()))
		     (not (and (eq (char-before) ?.)
			       (char-after (1+ (point)))
			       (not (eq ?w (char-syntax (char-after (1+ (point)))))))))
	    (delete-char 1)))
	(goto-char (marker-position (process-mark process)))
	;; Determine current page
	(while (and pt
		    (skip-chars-backward "^]" pt)
		    (> (point) pt))
	  (setq end (point))
	  (backward-char)
	  (skip-chars-backward "-0-9\n." (max (point-min) (- pt 128)))
	  (when (and (eq ?\[ (char-before))
		     (not (eq ?\] (char-after)))
		     (progn
		       (setq str (buffer-substring (1- (point)) end)
			     pos nil)
		       (while (setq pos (string-match "\n" str pos))
			 (setq str (replace-match "" t t str)))
		       (string-match
			"\\`\\[-?[0-9]+\\(\\.-?[0-9]+\\)\\{0,9\\}\\]\\'"
			str)))
	    (setq TeX-current-page str
		  pt nil)
	    (TeX-format-mode-line process)))))))

(defvar TeX-parse-function nil
  "Function to call to parse content of TeX output buffer.")
(make-variable-buffer-local 'TeX-parse-function)

(defun TeX-background-filter (_process string)
  "Filter to process background output."
  (let ((old-window (selected-window))
	(pop-up-windows t))
    (TeX-pop-to-buffer "*TeX background*" nil t)
    (goto-char (point-max))
    (insert string)
    (select-window old-window)))

;; Copy and adaption of `comint-postoutput-scroll-to-bottom' from CVS
;; Emacs of 2005-04-24.
(defun TeX-interactive-goto-prompt (string)
  "Move point to prompt of an interactive TeX run."
  (let* ((selected (selected-window))
	 (current (current-buffer))
	 (process (get-buffer-process current)))
    (unwind-protect
	(when process
	  (walk-windows
	   (lambda (window)
	     (when (eq (window-buffer window) current)
	       (select-window window)
	       (when (and (< (point) (process-mark process))
			  (string-match "^\\? $" string))
		 (goto-char (process-mark process)))
	       (select-window selected)))
	   nil t))
      (set-buffer current))))


;;; Active Process

(defvar TeX-current-process-region-p nil
  "This variable is set to t iff the last TeX command is on a region.")

(defun TeX-active-process ()
  "Return the active process for the current buffer."
  (TeX-process (TeX-active-master)))

(defun TeX-active-buffer ()
  "Return the buffer of the active process for this buffer."
  (and TeX-command-buffer
       (with-current-buffer TeX-command-buffer
	 (TeX-process-buffer (TeX-active-master)))))

(defun TeX-active-master (&optional extension nondirectory _ignore)
  "The master file currently being compiled.

If optional argument EXTENSION is non-nil, add that file extension to
the name.  Special value t means use `TeX-default-extension'.

If optional second argument NONDIRECTORY is non-nil, do not include
the directory.

The compatibility argument IGNORE is ignored."
  ;; The third argument `_ignore' is kept for symmetry with
  ;; `TeX-master-file's third argument `ask'.  For example, it's used
  ;; in `TeX--master-or-region-file-with-extra-quotes', where we don't
  ;; know which function has to be called.  Keep this in mind should
  ;; you want to use another argument here.
  ;; See also the similar comment in `TeX-region-file'.
  (if TeX-current-process-region-p
      (TeX-region-file extension nondirectory)
    (TeX-master-file extension nondirectory)))

(defvar TeX-command-buffer nil
  "The buffer from where the last TeX command was issued.")

;;; Region File

(defcustom TeX-region-extra ""
  "*String to insert in the region file between the header and the text."
  :group 'TeX-command
  :type 'string)

;; This was "{\\makeatletter\\gdef\\AucTeX@cite#1[#2]#3{[#3#1#2]}\
;;           \\gdef\\cite{\\@ifnextchar[{\\AucTeX@cite{, }}\
;;           {\\AucTeX@cite{}[]}}}\n"
;; However, that string is inappropriate for plain TeX and ConTeXt.
;; This needs reconsideration.


(defvar TeX-region-hook nil
  "List of hooks to run before the region file is saved.
The hooks are run in the region buffer, you may use the variable
`master-buffer' to access the buffer of the master file and
`orig-buffer' to access the buffer where \\[TeX-command-region] or
\\[TeX-command-buffer] is invoked from.")

(defun TeX-quote-filename (file)
  "Convert file name into a form acceptable to TeX."
  (let (pos)
    (while (setq pos (string-match "\\\\" file pos))
      (setq file (replace-match "/" t t file 0)
	    pos (1+ pos)))
    (while (setq pos (string-match "[~#]" file pos))
      (setq file (replace-match "\\\\string\\&" t nil file 0)
	    pos (+ pos 8))))
  ;; Use \unexpanded so that \message outputs the raw file name.
  ;; When \usepackage[utf8]{inputenc} is used in standard (pdf)latex,
  ;; \message does not output non-ascii file name in raw form without
  ;; \unexpanded, which makes AUCTeX to fail to recognize the file
  ;; names right when analysing the process output buffer.
  ;; Note that \usepackage[utf8]{inputenc} is enabled by default in
  ;; standard (pdf)latex since TeXLive 2018.
  (if (and (memq major-mode '(latex-mode doctex-mode))
	   ;; Japanese upLaTeX requires the same treatment with
	   ;; respect to non-ascii characters other than Japanese, in
	   ;; file names within \message{}.
	   ;; However, pLaTeX (non u- version) does not support
	   ;; non-ascii file name encoded in UTF-8.  So considering
	   ;; `ptex' doesn't make sense here.  We cater for only
	   ;; `default' and `uptex' engines.
	   (memq TeX-engine '(default uptex)))
      ;; It would fail to put entire `file' inside \unexpanded{} when
      ;; the above loop injects \string before "#" and "~".  So put
      ;; only multibyte characters inside \unexpanded{}.
      ;; It is safe in upLaTeX to use \unexpanded{} on Japanese
      ;; characters though they are handled by upLaTeX in a totally
      ;; different way from inputenc.
      ;; Thus put all multibyte characters, without considering
      ;; whether they are Japanese or not, inside \unexpanded{}.
      (replace-regexp-in-string "[[:multibyte:]]+"
				"\\\\unexpanded{\\&}" file t)
    file))

(defvar font-lock-mode-enable-list)
(defvar font-lock-auto-fontify)
(defvar font-lock-defaults-alist)

(defvar TeX-region-orig-buffer nil
  "The original buffer in which the TeX-region was created.")
(make-variable-buffer-local 'TeX-region-orig-buffer)

(defun TeX-region-create (file region original offset)
  "Create a new file named FILE with the string REGION.
The region is taken from ORIGINAL starting at line OFFSET.

The current buffer and master file is searched, in order to ensure
that the TeX header and trailer information is also included.

The OFFSET is used to provide the debugger with information about the
original file."
  (let* (;; We shift buffer a lot, so we must keep track of the buffer
	 ;; local variables.
	 (header-end TeX-header-end)
	 (trailer-start TeX-trailer-start)

	 ;; We seach for header and trailer in the master file.
	 (orig-buffer (current-buffer))
	 (master-name (TeX-master-file TeX-default-extension))
	 (master-buffer (find-file-noselect master-name))

	 ;; Attempt to disable font lock.
	 (font-lock-defaults-alist nil)
	 (font-lock-defaults nil)
	 (font-lock-maximum-size 0)
	 (font-lock-mode-hook nil)
	 (font-lock-auto-fontify nil)
	 (font-lock-mode-enable-list nil)
	 ;; And insert them into the FILE buffer.
	 (file-buffer (let (;; Don't query for master file
			    (TeX-transient-master t)
			    ;; Don't choose a special mode (and call its hooks)
			    (auto-mode-alist nil)
			    (magic-mode-alist nil)
			    (enable-local-variables nil)
			    ;; Don't run any f-f hooks
			    (find-file-hook nil))
			(find-file-noselect file)))
	 ;; But remember original content.
	 original-content

	 ;; We search for the header from the master file, if it is
	 ;; not present in the region.
	 (header (if (string-match header-end region)
		     ""
		   (save-excursion
		     (save-restriction
		       (set-buffer master-buffer)
		       (save-excursion
			 (save-restriction
			   (widen)
			   (goto-char (point-min))
			   ;; NOTE: We use the local value of
			   ;; TeX-header-end from the master file.
			   (if (not (re-search-forward TeX-header-end nil t))
			       ""
			     (re-search-forward "[\r\n]" nil t)
			     (buffer-substring-no-properties
			      (point-min) (point)))))))))
	 (header-offset 0)
	 first-line
	 ;; We search for the trailer from the master file, if it is
	 ;; not present in the region.
	 (trailer-offset 0)
	 (trailer (if (string-match trailer-start region)
		      ""
		    (save-excursion
		      (save-restriction
			(set-buffer master-buffer)
			(save-excursion
			  (save-restriction
			    (widen)
			    (goto-char (point-max))
			    ;; NOTE: We use the local value of
			    ;; TeX-trailer-start from the master file.
			    (if (not (re-search-backward TeX-trailer-start nil t))
				""
			      ;;(beginning-of-line 1)
			      (re-search-backward "[\r\n]" nil t)
			      (setq trailer-offset (TeX-current-offset))
			      (buffer-substring-no-properties
			       (point) (point-max))))))))))
    ;; file name should be relative to master
    (setq original (TeX-quote-filename (file-relative-name
					original (TeX-master-directory)))
	  master-name (TeX-quote-filename master-name))

    ;; If the first line begins with "%&", put that line separately on
    ;; the very first line of the region file so that the first line
    ;; parsing will work.
    (setq first-line (if (and (> (length header) 1)
			      (string= (substring header 0 2) "%&"))
			 ;; This would work even if header has no newline.
			 (substring header 0 (string-match "\n" header))
		       ""))
    (unless (string= first-line "")
      ;; Remove first-line from header.
      (setq header (substring header (length first-line)))
      (setq first-line (concat first-line "\n")))

    (with-current-buffer file-buffer
      (setq buffer-read-only t
	    buffer-undo-list t)
      (setq original-content (buffer-string))
      (let ((inhibit-read-only t))
	(erase-buffer)
	(setq buffer-file-coding-system
	      (with-current-buffer master-buffer buffer-file-coding-system))
	(insert first-line
		"\\message{ !name(" master-name ")}"
		header
		TeX-region-extra
		"\n\\message{ !name(" original ") !offset(")
	(setq header-offset (- offset
			       (1+ (TeX-current-offset))))
	(insert (int-to-string header-offset)
		") }\n"
		region
		"\n\\message{ !name("  master-name ") !offset(")
	(insert (int-to-string (- trailer-offset
				  (1+ (TeX-current-offset))))
		") }\n"
		trailer)
	(setq TeX-region-orig-buffer orig-buffer)
	(run-hooks 'TeX-region-hook)
	(if (string-equal (buffer-string) original-content)
	    (set-buffer-modified-p nil)
	  (save-buffer 0))))))

(defun TeX-region-file (&optional extension nondirectory _ignore)
  "Return TeX-region file name with EXTENSION.
If optional second argument NONDIRECTORY is non-nil, do not include
the directory.

The compatibility argument IGNORE is ignored."
  ;; The third argument `_ignore' is kept for symmetry with `TeX-master-file's
  ;; third argument `ask'.  For example, it's used in `TeX-command-sequence',
  ;; where we don't know which function has to be called.  Keep this in mind
  ;; should you want to use another argument here.
  (concat (if nondirectory "" (TeX-master-directory))
	  (cond ((eq extension t)
		 (concat TeX-region "." TeX-default-extension))
		(extension
		 (concat TeX-region "." extension))
		(t
		 TeX-region))))

(defcustom TeX-region "_region_"
  "*Base name of temporary file for `TeX-command-region' and `TeX-command-buffer'."
  :group 'TeX-command
  :type 'string)

(defvar LaTeX-command-section-level nil
  "The section level used for `LaTeX-command-section'.
Will be initialized to `LaTeX-largest-level' buffer-locally.")
(make-variable-buffer-local 'LaTeX-command-section-level)

(defun LaTeX-command-section-level ()
  "Return the value of `LaTeX-command-section-level'.
Initialize it to `LaTeX-largest-level' if needed."
  (unless LaTeX-command-section-level
    (setq LaTeX-command-section-level LaTeX-largest-level))
  LaTeX-command-section-level)


(defun LaTeX-command-section-change-level (arg)
  "Change `LaTeX-command-section-level' by ARG.
`LaTeX-command-section-level' is the sectioning level used to
determine the current section by `LaTeX-command-section'.
The levels are defined by `LaTeX-section-list'."
  (interactive "p")
  (let ((old-level (car (rassoc (list (LaTeX-command-section-level))
				LaTeX-section-list))))
    (setq LaTeX-command-section-level (+ LaTeX-command-section-level arg))
    (cond
     ((> LaTeX-command-section-level 6)
      (setq LaTeX-command-section-level 6)
      (message "Cannot shrink LaTeX-command-section-level below subparagraph."))
     ((< LaTeX-command-section-level 0)
      (setq LaTeX-command-section-level 0)
      (message "Cannot enlarge LaTeX-command-section-level above part."))
     (t (message "Changed level from %s to %s."
		 old-level (car (rassoc (list LaTeX-command-section-level)
					LaTeX-section-list)))))))

(defun LaTeX-command-section-boundaries ()
  "Return the boundaries of the current section as (start . end).
The section is determined by `LaTeX-command-section-level'."
  (let* ((case-fold-search nil)
	 (rx (concat "\\\\" (regexp-opt
			     (mapcar
			      (lambda (level)
				(car (rassoc (list level) LaTeX-section-list)))
			      (let (r)
				(dotimes (i (1+ (LaTeX-command-section-level)))
				  (push i r))
				r)))
		     "{")))
    (cons (save-excursion
	    (re-search-backward rx nil t)
	    (point))
	  (save-excursion
	    (re-search-forward (concat rx "\\|\\\\end{document}") nil t)
	    (forward-line 0)
	    (point)))))

(defun LaTeX-command-section (&optional override-confirm)
  "Run a command on the current section.

What makes the current section is defined by
`LaTeX-command-section-level' which can be enlarged or shrunken
with `LaTeX-command-section-change-level'.

Query the user for a command to run on the temporary file
specified by the variable `TeX-region'.  The region file will be
recreated from current section.

If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
depend on it being positive instead of the entry in
`TeX-command-list'."
  (interactive "P")
  (if (eq major-mode 'latex-mode)
      (let* ((bounds (LaTeX-command-section-boundaries))
	     (TeX-command-region-begin (car bounds))
	     (TeX-command-region-end (cdr bounds)))
	(TeX-command-region override-confirm))
    (error "LaTeX-command-section can only be run on LaTeX documents")))

(defun TeX-command-run-all-region ()
  "Compile the current region until an error occurs or it is finished."
  (interactive)
  (TeX-region-update)
  (TeX-command-sequence t t #'TeX-region-file))

(defun LaTeX-command-run-all-section ()
  "Compile the current section until an error occurs or it is finished."
  (interactive)
  (if (eq major-mode 'latex-mode)
      (let* ((bounds (LaTeX-command-section-boundaries))
	     (TeX-command-region-begin (car bounds))
	     (TeX-command-region-end (cdr bounds)))
	(TeX-region-update)
	(TeX-command-sequence t t #'TeX-region-file))
    (error "LaTeX-command-run-all-section can only be run on LaTeX documents")))

(defun TeX-command-run-all (arg)
  "Compile the current document until an error occurs or it is finished.
With a prefix ARG (`\\[universal-argument] \\[TeX-command-run-all]'),
compile the current region instead, e.g, call
`TeX-command-run-all-region'.  With multiple prefix
arguments (`\\[universal-argument] \\[universal-argument] \\[TeX-command-run-all]'),
compile the current section instead, e.g. call
`LaTeX-command-run-all-section'."
  (interactive "P")
  (cond
   ((null arg)       (TeX-command-sequence t t))
   ((= 4 (car arg))  (TeX-command-run-all-region))
   (t                (LaTeX-command-run-all-section))))

;;; Parsing

;;; - Global Parser Variables

(defvar TeX-error-point nil
  "How far we have parsed until now.")

(make-variable-buffer-local 'TeX-error-point)

(defvar TeX-error-file nil
  "Stack of files in which errors have occurred.")

(make-variable-buffer-local 'TeX-error-file)

(defvar TeX-error-offset nil
  "Add this to any line numbers from TeX.  Stack like `TeX-error-file'.")

(make-variable-buffer-local 'TeX-error-offset)

(defun TeX-parse-reset (&optional reparse)
  "Reset all variables used for parsing TeX output.
If optional argument REPARSE is non-nil, reparse the output log."
  (setq TeX-error-point (point-min)
	TeX-error-offset nil
	TeX-error-file nil
	TeX-error-list nil
	TeX-error-last-visited -1)
  (if reparse
      (TeX-parse-all-errors)))

;;; - Parsers Hooks

;; All this parsers hooks should have the same arguments even though they will
;; be ignored, because `TeX-next-error' can call any of these functions.
(defun TeX-parse-command (_arg _reparse)
  "We can't parse anything but TeX."
  (error "I cannot parse %s output, sorry"
	 (if (TeX-active-process)
	     (process-name (TeX-active-process))
	   "this")))

(defun TeX-error-list-skip-warning-p (type ignore)
  "Decide if a warning of `TeX-error-list' should be skipped.

TYPE is one of the types listed in `TeX-error-list', IGNORE
is the flag to choose if the warning should be skipped."
  ;; The warning should be skipped if it...
  (or
   ;; ...is a warning and we want to ignore all warnings, or...
   (and (null TeX-debug-warnings)
	(equal type 'warning))
   ;; ...is a bad-box and we want to ignore all bad-boxes, or...
   (and (null TeX-debug-bad-boxes)
	(equal type 'bad-box))
   ;; ...is a warning to be ignored.
   (and TeX-suppress-ignored-warnings
	ignore)))

(defun TeX-parse-TeX (arg reparse)
  "Find the next error produced by running TeX.

ARG specifies how many error messages to move, when possible;
negative means move back to previous error messages.

If REPARSE is non-nil, reparse the output log.

If the file occurs in an included file, the file is loaded (if not
already in an Emacs buffer) and the cursor is placed at the error."
  (let ((old-buffer (current-buffer))
	(default-major-mode major-mode)
	max-index item)

    ;; Switch to the output buffer.
    (with-current-buffer (TeX-active-buffer)
      (if reparse
	  (TeX-parse-reset reparse))
      (if TeX-parse-all-errors
	  (progn
	    (setq arg (or arg 1)
		  max-index (length TeX-error-list))
	    ;; This loop is needed to skip ignored warnings, when
	    ;; `TeX-suppress-ignored-warnings' is non-nil and there are ignore
	    ;; warnings.
	    (while (null (zerop arg))
	      (setq TeX-error-last-visited
		    ;; Increase or decrese `TeX-error-last-visited' depending on
		    ;; the sign of `arg'.  Note: `signum' is a function from
		    ;; `cl' library, do not be tempted to use it.
		    (if (> arg 0)
			(1+ TeX-error-last-visited)
		      (1- TeX-error-last-visited))
		    item (nth TeX-error-last-visited TeX-error-list))
	      ;; Increase or decrease `arg' only if the warning isn't to be
	      ;; skipped.
	      (unless (TeX-error-list-skip-warning-p (nth 0 item) (nth 10 item))
		;; Note: `signum' is a function from `cl' library, do not be
		;; tempted to use it.
		(setq arg (if (> arg 0)
			      (1- arg)
			    (1+ arg)))))
	    (if (< TeX-error-last-visited -1)
		(setq TeX-error-last-visited -1))
	    (cond ((or (null item)
		       (< TeX-error-last-visited 0))
		   (if (> TeX-error-last-visited max-index)
		       (setq TeX-error-last-visited max-index))
		   (message "No more errors.")
		   (beep)
		   (TeX-pop-to-buffer old-buffer))
		  (t
		   (apply #'TeX-find-display-help item))))

	(goto-char TeX-error-point)
	(TeX-parse-error old-buffer)))))

;;; - Parsing (La)TeX

(defvar TeX-translate-location-hook nil
  "List of functions to be called before showing an error or warning.

You might want to examine and modify the free variables `file',
`offset', `line', `string', `error', and `context' from this hook.")

;; `ignore' flag should be the always the last one in the list of information
;; for each error/warning, because it can be set within `TeX-warning' by a
;; custom function taking as argument all information present in
;; `TeX-error-list' but `ignore', see `TeX-ignore-warnings'.
(defvar TeX-error-list nil
  "List of warnings and errors.

Each element of the list is a list of information for a specific
error or warning.  This is the structure of each element:
 *  0: type (error, warning, bad-box)
 *  1: file
 *  2: line
 *  3: message of the error or warning
 *  4: offset
 *  5: context, to be displayed in the help window
 *  6: string to search in the buffer, in order to find location
       of the error or warning
 *  7: for warnings referring to multiple lines (e.g. bad boxes),
       the last line mentioned in the warning message
 *  8: t if it is a bad-box, nil otherwise
 *  9: value of `TeX-error-point'
 * 10: whether the warning should be ignored

This variable is intended to be set only in output buffer so it
will be shared among all files of the same document.")
(make-variable-buffer-local 'TeX-error-list)

(defcustom TeX-parse-all-errors t
  "Whether to automatically collect all warning and errors after running TeX.

If t, it makes it possible to use `TeX-previous-error' with TeX
commands."
  :group 'TeX-command
  :type 'boolean)

(defun TeX-parse-all-errors ()
  "Parse TeX output buffer to collect all warnings and errors."
  ;; Reset error list.
  (setq TeX-error-list nil)
  (save-excursion
    (goto-char (point-min))
    (while (TeX-parse-error nil t)))
  ;; Reset last visited error.
  (setq TeX-error-last-visited -1))

(defun TeX-parse-error (old &optional store)
  "Goto next error.  Pop to OLD buffer if no more errors are found.

If the optional argument STORE is non-nil, the function will
store the found warning or error in `TeX-error-list' instead of
displaying the issue.

Return non-nil if an error or warning is found."
  (let ((regexp
	 (concat
	  ;; TeX error
	  "^\\(!\\|\\(.*?\\):[0-9]+:\\) \\|"
	  ;; New file
	  "(\n?\\([^\n())]+\\)\\|"
	  ;; End of file.
	  "\\()\\)\\|"
	  ;; Hook to change line numbers
	  " !\\(?:offset(\\([---0-9]+\\))\\|"
	  ;; Hook to change file name
	  "name(\\([^)]+\\))\\)\\|"
	  ;; Start of LaTeX bad box
	  "^\\(\\(?:Overfull\\|Underfull\\|Tight\\|Loose\\) "
	  ;;   Horizontal bad box
	  "\\(?:\\\\hbox.* at lines? [0-9]+\\(?:--[0-9]+\\)?$\\|"
	  ;;   Vertical bad box.  See also `TeX-warning'.
	  "\\\\vbox ([ a-z0-9]+) has occurred while \\\\output is active \\[[^]]+\\]\\)\\)\\|"
	  ;; LaTeX warning
	  "^\\(" LaTeX-warnings-regexp ".*\\)"))
	(error-found nil))
    (while
	(cond
	 ((null
	   (re-search-forward regexp nil t))
	  ;; No more errors.
	  (unless store
	    (message "No more errors.")
	    (beep)
	    (TeX-pop-to-buffer old))
	  nil)
	 ;; TeX error
	 ((match-beginning 1)
	  (when (match-beginning 2)
	    (unless TeX-error-file
	      (push nil TeX-error-file)
	      (push nil TeX-error-offset))
	    (unless (car TeX-error-offset)
	      (rplaca TeX-error-file (TeX-match-buffer 2))))
	  (setq error-found t)
	  (if (looking-at "Preview ")
	      t
	    (TeX-error store)
	    nil))
	 ;; LaTeX bad box
	 ((match-beginning 7)
	  ;; In `TeX-error-list' we collect all warnings, also if they're going
	  ;; to be actually skipped.
	  (if (or store TeX-debug-bad-boxes)
	      (progn
		(setq error-found t)
		(TeX-warning (TeX-match-buffer 7) (match-beginning 7) t store)
		nil)
	    (re-search-forward "\r?\n\
\\(?:.\\{79\\}\r?\n\
\\)*.*\r?$")
	    t))
	 ;; LaTeX warning
	 ((match-beginning 8)
	  ;; In `TeX-error-list' we collect all warnings, also if they're going
	  ;; to be actually skipped.
	  (if (or store TeX-debug-warnings)
	      (progn
		(setq error-found t)
		(TeX-warning (TeX-match-buffer 8) (match-beginning 8) nil store)
		nil)
	    t))

	 ;; New file -- Push on stack
	 ((match-beginning 3)
	  (let ((file (TeX-match-buffer 3))
		(end (match-end 3)))
	    ;; Strip quotation marks and remove newlines if necessary
	    (when (or (eq (string-to-char file) ?\")
		      (string-match "[ \t\n]" file))
	      (setq file (mapconcat #'identity (split-string file "[\"\n]+") "")))
	    ;; Polish `file' string
	    (setq file
		  (let ((string file))
		    (setq string
			  (if (string-match "\\`[ \t\n\r]+" string)
			      (replace-match "" t t string)
			    string))
		    ;; Sometimes `file' is something like
		    ;;     "./path/to/file.tex [9] [10 <./path/to/file>] "
		    ;; where "[9]" and "[10 <./path/to/file>]" are pages of the
		    ;; output file, with path to an included file.  Remove these
		    ;; numbers together with whitespaces at the end of the
		    ;; string.
		    (if (string-match "\\( *\\(\\[[^]]+\\]\\)? *\\)*\\'" string)
			(replace-match "" t t string)
		      string)))
	    (push file TeX-error-file)
	    (push nil TeX-error-offset)
	    (goto-char end))
	  t)

	 ;; End of file -- Pop from stack
	 ((match-beginning 4)
	  (when (> (length TeX-error-file) 0)
	    (pop TeX-error-file)
	    (pop TeX-error-offset))
	  (goto-char (match-end 4))
	  t)

	 ;; Hook to change line numbers
	 ((match-beginning 5)
	  (setq TeX-error-offset
		(list (string-to-number (TeX-match-buffer 5))))
	  t)

	 ;; Hook to change file name
	 ((match-beginning 6)
	  (setq TeX-error-file
		(list (TeX-match-buffer 6)))
	  t)))
    error-found))

(defun TeX-find-display-help (type file line error offset context string
				   line-end _bad-box error-point _ignore)
  "Find the error and display the help.

For a description of arguments, see `TeX-error-list'.  IGNORE
value is not used here."
  ;; Go back to TeX-buffer
  (let ((runbuf (TeX-active-buffer))
	(master (with-current-buffer TeX-command-buffer
		  (expand-file-name (TeX-master-file))))
	(command-buffer TeX-command-buffer)
	error-file-buffer start)
    (run-hooks 'TeX-translate-location-hook)

    (if file
	(progn
	  (setq error-file-buffer
		(find-file
		 (expand-file-name file (file-name-directory master))))
	  ;; Set the value of `TeX-command-buffer' in the next file with an
	  ;; error to be displayed to the value it has in the current buffer.
	  (with-current-buffer error-file-buffer
	    (set (make-local-variable 'TeX-command-buffer) command-buffer))

	  ;; Find the location of the error or warning.
	  (when line
	    (goto-char (point-min))
	    (forward-line (+ offset line -1))
	    (cond
	     ;; Error.
	     ((equal type 'error)
	      (if (not (string= string " "))
		  (search-forward string nil t)))
	     ;; Warning or bad box.
	     (t
	      (beginning-of-line 0)
	      (setq start (point))
	      (goto-char (point-min))
	      (forward-line (+ offset line-end -1))
	      (end-of-line)
	      (when string
		(search-backward string start t)
		(search-forward string nil t))))))
      ;; When the file cannot be determined stay here but issue a warning.
      (message (concat "Could not determine file for "
		       (cond ((equal type 'error) "error")
			     (t "warning"))))
      (beep))

    ;; Display the help.
    (cond ((eq TeX-display-help 'expert)
	   (TeX-pop-to-buffer runbuf nil t)
	   (goto-char error-point)
	   (TeX-pop-to-buffer error-file-buffer nil t))
	  (TeX-display-help
	   (TeX-help-error
	    error
	    (if (equal type 'warning) (concat "\n" context) context)
	    runbuf type))
	  (t
	   (message (concat "! " error))))))

(defun TeX-error (&optional store)
  "Display an error.

If optional argument STORE is non-nil, store the error
information in `TeX-error-list' instead of displaying the error."

  (let* ( ;; We need the error message to show the user.
	 (error (progn
		  (re-search-forward "\\(.*\\)")
		  (TeX-match-buffer 1)))

	 ;; And the context for the help window.
	 (context-start (point))
	 context-available

	 ;; And the line number to position the cursor.
	 (line (cond
		;; regular style
		((re-search-forward "l\\.\\([0-9]+\\)" nil t)
		 (setq context-available t)
		 (string-to-number (TeX-match-buffer 1)))
		;; file:line:error style
		((save-excursion
		   (re-search-backward ":\\([0-9]+\\): "
				       (line-beginning-position) t))
		 (string-to-number (TeX-match-buffer 1)))
		;; nothing found
		(t 1)))

	 ;; And a string of the context to search for.
	 (string (progn
		   (beginning-of-line)
		   (re-search-forward " \\(\\([^ \t]*$\\)\\|\\($\\)\\)")
		   (TeX-match-buffer 1)))

	 ;; And we have now found to the end of the context.
	 (context (if context-available
		      (buffer-substring context-start (progn (forward-line 1)
							     (end-of-line)
							     (point)))
		    ;; There is no real context available, so we
		    ;; simply show the line with the error message.
		    (buffer-substring (1- (line-beginning-position))
				      context-start)))
	 ;; We may use these in another buffer.
	 (offset (or (car TeX-error-offset) 0))
	 (file (car TeX-error-file))
	 info-list)

    ;; Remember where we was.
    (setq TeX-error-point (point)
	  info-list (list 'error file line error offset context string nil nil
			  TeX-error-point nil))
    (if store
	;; Store the error information.
	(add-to-list 'TeX-error-list info-list t)
      ;; Find the error point and display the help.
      (apply #'TeX-find-display-help info-list))))

(defun TeX-warning (warning warning-start bad-box &optional store)
  "Display a warning for WARNING.

WARNING-START is the position where WARNING starts.  If BAD-BOX
is non-nil, the warning refers to a bad-box, otherwise it is a
generic warning.

If optional argument STORE is non-nil, store the warning
information in `TeX-error-list' instead of displaying the
warning."

  (let* ( ;; line-string: match 1 is beginning line, match 2 is end line
	 (line-string (if bad-box
			  "at lines? \\([0-9]*\\)\\(?:--\\([0-9]*\\)\\)?"
			"on input line \\([0-9]*\\)\\."))
	 ;; word-string: match 1 is the word
	 (word-string (if bad-box "[][\\W() ---]\\(\\w+\\)[][\\W() ---]*$"
			;; Match "ref" in both "Reference `ref' on page NN
			;; undefined" and "Citation 'ref' on page NN undefined".
			"\\(?:`\\|'\\)\\([-a-zA-Z0-9:]+\\)'"))

	 ;; Get error-line (warning).  Don't search before `warning-start' to
	 ;; avoid catching completely unrelated line numbers.
	 (line (when (save-excursion (re-search-backward line-string
							 warning-start t))
		 (string-to-number (TeX-match-buffer 1))))
	 ;; If this is a bad box and the warning ends with "...at lines MM--NN"
	 ;; we can use "NN" as `line-end', in any other case (including bad
	 ;; boxes ending with "...at line NN") just use `line'.
	 (line-end (if (and bad-box (match-beginning 2))
		       (string-to-number (TeX-match-buffer 2))
		     line))

	 ;; Find the context
	 (context-start (progn (cond
				((and bad-box (string-match "\\\\hbox" warning))
				 ;; Horizontal bad box
				 (end-of-line))
				(bad-box
				 ;; Vertical bad box (by exclusion), don't move
				 ;; point.  In the output buffer, unlike in the
				 ;; actual *.log file, these warnings do not end
				 ;; with "...is active []", but in the same line
				 ;; there may be something else, including a new
				 ;; file opened.  Thus, point shouldn't move
				 ;; from the end of the actual bad box warning.
				 ;; This is why the corresponding regexp in
				 ;; `TeX-parse-error' doesn't match everything
				 ;; until the end of the line.
				 nil)
				(t
				 ;; Generic warning.
				 (beginning-of-line)))
			       (point)))

	 (context (cond ((string-match LaTeX-warnings-regexp warning)
			 ;; The warnings matching `LaTeX-warnings-regexp' are
			 ;; emitted by \GenericWarning macro, or macros based on
			 ;; it (\ClassWarning, \PackageWarning, etc).  After
			 ;; such warnings there is an empty line, just look for
			 ;; it to find the end.
			 (beginning-of-line)
			 (while (null (eolp))
			   (forward-line 1))
			 (buffer-substring context-start (progn (end-of-line)
								(point))))

			((and bad-box (string-match "\\\\vbox" warning))
			 ;; Vertical bad boxes don't provide any additional
			 ;; information.  In this case, reuse the `warning' as
			 ;; `context' and don't move point, so that we avoid
			 ;; eating the next line that may contain another
			 ;; warning.  See also comment for `context-start'.
			 (concat "\n" warning))

			(t
			 ;; Horizontal bad boxes.
			 (forward-line 1)
			 (end-of-line)
			 (while (equal (current-column) 79)
			   (forward-line 1)
			   (end-of-line))
			 (buffer-substring context-start (point)))))

	 ;; This is where we want to be.
	 (error-point (point))

	 ;; Now find the error word.
	 (string (when (save-excursion
			 (re-search-backward word-string context-start t))
		   (TeX-match-buffer 1)))

	 ;; We might use these in another file.
	 (offset (or (car TeX-error-offset) 0))
	 (file (car TeX-error-file))
	 info-list ignore)

    ;; Second chance to get line number right.  If `line' is nil, check whether
    ;; the reference to the line number is in `context'.  For example, this is
    ;; the case for warnings emitted with \ClassWarning and \PackageWarning.
    ;; XXX: maybe it suffices to evaluate `line' after `context' above, but I
    ;; don't know if there are cases in which it's important to get `line'
    ;; before `context'.
    (and (null line)
	 (string-match line-string context)
	 (setq line-end
	       (setq line (and (match-beginning 1)
			       (string-to-number (match-string 1 context))))))

    ;; This is where we start next time.
    (goto-char error-point)
    (setq TeX-error-point (point))

    ;; Explanation of what follows: we add the warning to `TeX-error-list' even
    ;; if it has to be ignored, with a flag specifying whether it is ignored.
    ;; We do so in order to be able to change between "ignore" and "dont-ignore"
    ;; behavior by just looking to the flag, without the need to reparse the
    ;; output log.

    ;; Store the list of information about the warning.
    (setq info-list (list (if bad-box 'bad-box 'warning) file line warning
			  offset context string line-end bad-box
			  TeX-error-point)
	  ;; Decide whether it should be ignored.
	  ignore (and TeX-ignore-warnings
		      (cond
		       ((stringp TeX-ignore-warnings)
			(string-match TeX-ignore-warnings warning))
		       ((fboundp TeX-ignore-warnings)
			(apply TeX-ignore-warnings info-list))))
	  ;; Update `info-list'.
	  info-list (append info-list (list ignore)))

    (if store
	;; Store the warning information.
	(add-to-list 'TeX-error-list info-list t)
      ;; Find the warning point and display the help.
      (apply #'TeX-find-display-help info-list))))

;;; Error Messages

(defcustom TeX-error-description-list
  '(("\\(?:Package Preview Error\\|Preview\\):.*" .
     "The `auctex' option to `preview' should not be applied manually.
If you see this error message outside of a preview run, either
you did something too clever, or AUCTeX something too stupid.")

    ("Bad \\\\line or \\\\vector argument.*" .
     "The first argument of a \\line or \\vector command, which specifies the
slope, is illegal\.")

    ("Bad math environment delimiter.*" .
     "TeX has found either a math-mode-starting command such as \\[ or \\(
when it is already in math mode, or else a math-mode-ending command
such as \\) or \\] while in LR or paragraph mode.  The problem is caused
by either unmatched math mode delimiters or unbalanced braces\.")

    ("Bad use of \\\\\\\\.*" .
     "A \\\\ command appears between paragraphs, where it makes no sense. This
error message occurs when the \\\\ is used in a centering or flushing
environment or else in the scope of a centering or flushing
declaration.")

    ("\\\\begin{[^ ]*} ended by \\\\end{[^ ]*}." .
     "LaTeX has found an \\end command that doesn't match the corresponding
\\begin command. You probably misspelled the environment name in the
\\end command, have an extra \\begin, or else forgot an \\end.")

    ("Can be used only in preamble." .
     "LaTeX has encountered, after the \\begin{document}, one of the
following commands that should appear only in the preamble:
\\documentclass, \\nofiles, \\includeonly, \\makeindex, or
\\makeglossary.  The error is also caused by an extra \\begin{document}
command.")

    ("Command name [^ ]* already used.*" .
     "You are using \\newcommand, \\newenvironment, \\newlength, \\newsavebox,
or \\newtheorem to define a command or environment name that is
already defined, or \\newcounter to define a counter that already
exists. (Defining an environment named gnu automatically defines the
command \\gnu.) You'll have to choose a new name or, in the case of
\\newcommand or \\newenvironment, switch to the \\renew ...  command.")

    ("Counter too large." .
     "1. Some object that is numbered with letters, probably an item in a
enumerated list, has received a number greater than 26. Either you're
making a very long list or you've been resetting counter values.

2. Footnotes are being ``numbered'' with letters or footnote symbols
and LaTeX has run out of letters or symbols. This is probably caused
by too many \\thanks commands.")

    ("Environment [^ ]* undefined." .
     "LaTeX has encountered a \\begin command for a nonexistent environment.
You probably misspelled the environment name. ")

    ("Float(s) lost." .
     "You put a figure or table environment or a \\marginpar command inside a
parbox---either one made with a minipage environment or \\parbox
command, or one constructed by LaTeX in making a footnote, figure,
etc. This is an outputting error, and the offending environment or
command may be quite a way back from the point where LaTeX discovered
the problem. One or more figures, tables, and/or marginal notes have
been lost, but not necessarily the one that caused the error.")

    ("Illegal character in array arg." .
     "There is an illegal character in the argument of an array or tabular
environment, or in the second argument of a \\multicolumn command.")

    ("Missing \\\\begin{document}." .
     "LaTeX produced printed output before encountering a \\begin{document}
command. Either you forgot the \\begin{document} command or there is
something wrong in the preamble. The problem may be a stray character
or an error in a declaration---for example, omitting the braces around
an argument or forgetting the \\ in a command name.")

    ("Missing p-arg in array arg.*" .
     "There is a p that is not followed by an expression in braces in the
argument of an array or tabular environment, or in the second argument
of a \\multicolumn command.")

    ("Missing @-exp in array arg." .
     "There is an @ character not followed by an @-expression in the
argument of an array or tabular environment, or in the second argument
of a \\multicolumn command.")

    ("No such counter." .
     "You have specified a nonexistent counter in a \\setcounter or
\\addtocounter command. This is probably caused by a simple typing
error.  However, if the error occurred while a file with the extension
aux is being read, then you probably used a \\newcounter command
outside the preamble.")

    ("Not in outer par mode." .
     "You had a figure or table environment or a \\marginpar command in math
mode or inside a parbox.")

    ("\\\\pushtabs and \\\\poptabs don't match." .
     "LaTeX found a \\poptabs with no matching \\pushtabs, or has come to the
\\end{tabbing} command with one or more unmatched \\pushtabs commands.")

    ("Something's wrong--perhaps a missing \\\\item." .
     "The most probable cause is an omitted \\item command in a list-making
environment. It is also caused by forgetting the argument of a
thebibliography environment.")

    ("Tab overflow." .
     "A \\= command has exceeded the maximum number of tab stops that LaTeX
permits.")

    ("There's no line here to end." .
     "A \\newline or \\\\ command appears between paragraphs, where it makes no
sense. If you're trying to ``leave a blank line'', use a \\vspace
command.")

    ("This may be a LaTeX bug." .
     "LaTeX has become thoroughly confused. This is probably due to a
previously detected error, but it is possible that you have found an
error in LaTeX itself. If this is the first error message produced by
the input file and you can't find anything wrong, save the file and
contact the person listed in your Local Guide.")

    ("Too deeply nested." .
     "There are too many list-making environments nested within one another.
How many levels of nesting are permitted may depend upon what computer
you are using, but at least four levels are provided, which should be
enough.")

    ("Too many unprocessed floats." .
     "While this error can result from having too many \\marginpar commands
on a page, a more likely cause is forcing LaTeX to save more figures
and tables than it has room for.  When typesetting its continuous
scroll, LaTeX saves figures and tables separately and inserts them as
it cuts off pages. This error occurs when LaTeX finds too many figure
and/or table environments before it is time to cut off a page, a
problem that is solved by moving some of the environments farther
towards the end of the input file. The error can also be caused by a
``logjam''---a figure or table that cannot be printed causing others
to pile up behind it, since LaTeX will not print figures or tables out
of order. The jam can be started by a figure or table that either is
too large to fit on a page or won't fit where its optional placement
argument says it must go. This is likely to happen if the argument
does not contain a p option.")

    ("Undefined tab position." .
     "A \\>, \\+, \\-, or \\< command is trying to go to a nonexistent tab
position---one not defined by a \\= command.")

    ("\\\\< in mid line." .
     "A \\< command appears in the middle of a line in a tabbing environment.
This command should come only at the beginning of a line.")

    ("Double subscript." .
     "There are two subscripts in a row in a mathematical
formula---something like x_{2}_{3}, which makes no sense.")

    ("Double superscript." .
     "There are two superscripts in a row in a mathematical
formula---something like x^{2}^{3}, which makes no sense.")

    ("Extra alignment tab has been changed to \\\\cr." .
     "There are too many separate items (column entries) in a single row of
an array or tabular environment. In other words, there were too many &
's before the end of the row. You probably forgot the \\\\ at the end of
the preceding row.")

    ("Extra \\}, or forgotten \\$." .
     "The braces or math mode delimiters don't match properly. You probably
forgot a {, \\[, \\(, or $.")

    ("Font [^ ]* not loaded: Not enough room left." .
     "The document uses more fonts than TeX has room for. If different parts
of the document use different fonts, then you can get around the
problem by processing it in parts.")

    ("I can't find file `.*'." .
     "TeX can't find a file that it needs. If the name of the missing file
has the extension tex, then it is looking for an input file that you
specified---either your main file or another file inserted with an
\\input or \\include command. If the missing file has the extension sty
, then you have specified a nonexistent document style or style
option.")

    ("Illegal parameter number in definition of .*" .
     "This is probably caused by a \\newcommand, \\renewcommand,
\\newenvironment, or \\renewenvironment command in which a # is used
incorrectly.  A # character, except as part of the command name \\#,
can be used only to indicate an argument parameter, as in #2, which
denotes the second argument. This error is also caused by nesting one
of the above four commands inside another, or by putting a parameter
like #2 in the last argument of a \\newenvironment or \\renewenvironment
command.")

    ("Illegal unit of measure ([^ ]* inserted)." .
     "If you just got a

      ! Missing number, treated as zero.

error, then this is part of the same problem.  If not, it means that
LaTeX was expecting a length as an argument and found a number
instead.  The most common cause of this error is writing 0 instead of
something like 0in for a length of zero, in which case typing return
should result in correct output. However, the error can also be caused
by omitting a command argument.")

    ("Misplaced alignment tab character \\&." .
     "The special character &, which should be used only to separate items
in an array or tabular environment, appeared in ordinary text. You
probably meant to type \\&.")

    ("Missing control sequence inserted." .
     "This is probably caused by a \\newcommand, \\renewcommand, \\newlength,
or \\newsavebox command whose first argument is not a command name.")

    ("Missing number, treated as zero." .
     "This is usually caused by a LaTeX command expecting but not finding
either a number or a length as an argument. You may have omitted an
argument, or a square bracket in the text may have been mistaken for
the beginning of an optional argument. This error is also caused by
putting \\protect in front of either a length command or a command such
as \\value that produces a number.")

    ("Missing [{}] inserted." .
     "TeX has become confused. The position indicated by the error locator
is probably beyond the point where the incorrect input is.")

    ("Missing \\$ inserted." .
     "TeX probably found a command that can be used only in math mode when
it wasn't in math mode.  Remember that unless stated otherwise, all
all the commands of Section 3.3 in LaTeX Book (Lamport) can be used
only in math mode. TeX is not in math mode when it begins processing
the argument of a box-making command, even if that command is inside a
math environment. This error also occurs if TeX encounters a blank
line when it is in math mode.")

    ("Not a letter." .
     "Something appears in the argument of a \\hyphenation command that
doesn't belong there.")

    ("Paragraph ended before [^ ]* was complete." .
     "A blank line occurred in a command argument that shouldn't contain
one. You probably forgot the right brace at the end of an argument.")

    ("\\\\[^ ]*font [^ ]* is undefined .*" .
     "These errors occur when an uncommon font is used in math mode---for
example, if you use a \\sc command in a formula inside a footnote,
calling for a footnote-sized small caps font.  This problem is solved
by using a \\load command.")

    ("Font .* not found." .
     "You requested a family/series/shape/size combination that is totally
unknown.  There are two cases in which this error can occur:
  1) You used the \\size macro to select a size that is not available.
  2) If you did not do that, go to your local `wizard' and
     complain fiercely that the font selection tables are corrupted!")

    ("TeX capacity exceeded, sorry .*" .
     "TeX has just run out of space and aborted its execution. Before you
panic, remember that the least likely cause of this error is TeX not
having the capacity to process your document.  It was probably an
error in your input file that caused TeX to run out of room. The
following discussion explains how to decide whether you've really
exceeded TeX's capacity and, if so, what to do. If the problem is an
error in the input, you may have to use the divide and conquer method
described previously to locate it. LaTeX seldom runs out of space on a
short input file, so if running it on the last few pages before the
error indicator's position still produces the error, then there's
almost certainly something wrong in the input file.

The end of the error indicator tells what kind of space TeX ran out
of. The more common ones are listed below, with an explanation of
their probable causes.

buffer size
===========
Can be caused by too long a piece of text as the argument
of a sectioning, \\caption, \\addcontentsline, or \\addtocontents
command. This error will probably occur when the \\end{document} is
being processed, but it could happen when a \\tableofcontents,
\\listoffigures, or \\listoftables command is executed. To solve this
problem, use a shorter optional argument. Even if you're producing a
table of contents or a list of figures or tables, such a long entry
won't help the reader.

exception dictionary
====================
You have used \\hyphenation commands to give TeX
more hyphenation information than it has room for. Remove some of the
less frequently used words from the \\hyphenation commands and insert
\\- commands instead.

hash size
=========
Your input file defines too many command names and/or uses
too many cross-ref- erencing labels.

input stack size
================
This is probably caused by an error in a command
definition. For example, the following command makes a circular
definition, defining \\gnu in terms of itself:

	  \\newcommand{\\gnu}{a \\gnu} % This is wrong!

When TeX encounters this \\gnu command, it will keep chasing its tail
trying to figure out what \\gnu should produce, and eventually run out
of ``input stack''.

main memory size
================
This is one kind of space that TeX can run out of when processing a
short file. There are three ways you can run TeX out of main memory
space: (1) defining a lot of very long, complicated commands, (2)
making an index or glossary and having too many \\index or \\glossary
commands on a single page, and (3) creating so complicated a page of
output that TeX can't hold all the information needed to generate it.
The solution to the first two problems is obvious: define fewer
commands or use fewer \\index and \\glossary commands. The third problem
is nastier. It can be caused by large tabbing, tabular, array, and
picture environments. TeX's space may also be filled up with figures
and tables waiting for a place to go.  To find out if you've really
exceeded TeX's capacity in this way, put a \\clearpage command in your
input file right before the place where TeX ran out of room and try
running it again. If it doesn't run out of room with the \\clearpage
command there, then you did exceed TeX's capacity.  If it still runs
out of room, then there's probably an error in your file.  If TeX is
really out of room, you must give it some help. Remember that TeX
processes a complete paragraph before deciding whether to cut the
page. Inserting a \\newpage command in the middle of the paragraph,
where TeX should break the page, may save the day by letting TeX write
the current page before processing the rest of the paragraph. (A
\\pagebreak command won't help.) If the problem is caused by
accumulated figures and tables, you can try to prevent them from
accumulating---either by moving them further towards the end of the
document or by trying to get them to come out sooner.  If you are
still writing the document, simply add a \\clearpage command and forget
about the problem until you're ready to produce the final version.
Changes to the input file are likely to make the problem go away.

pool size
=========
You probably used too many cross-ref-erencing \\labels and/or defined
too many new command names. More precisely, the labels and command
names that you define have too many characters, so this problem can be
solved by using shorter names. However, the error can also be caused
by omitting the right brace that ends the argument of either a counter
command such as \\setcounter, or a \\newenvironment or \\newtheorem
command.

save size
=========
This occurs when commands, environments, and the scopes of
declarations are nested too deeply---for example, by having the
argument of a \\multiput command contain a picture environment that in
turn has a \\footnotesize declaration whose scope contains a \\multiput
command containing a ....")

    ("Text line contains an invalid character." .
     "The input contains some strange character that it shouldn't. A mistake
when creating the file probably caused your text editor to insert this
character. Exactly what could have happened depends upon what text
editor you used. If examining the input file doesn't reveal the
offending character, consult the Local Guide for suggestions.")

    ("Undefined control sequence."   .
     "TeX encountered an unknown command name. You probably misspelled the
name. If this message occurs when a LaTeX command is being processed,
the command is probably in the wrong place---for example, the error
can be produced by an \\item command that's not inside a list-making
environment. The error can also be caused by a missing \\documentclass
command.")

    ("Use of [^ ]* doesn't match its definition." .
     "It's probably one of the picture-drawing commands, and you have used
the wrong syntax for specifying an argument. If it's \\@array that
doesn't match its definition, then there is something wrong in an
@-expression in the argument of an array or tabular
environment---perhaps a fragile command that is not \\protect'ed.")

    ("You can't use `macro parameter character \\#' in [^ ]* mode." .
     "The special character # has appeared in ordinary text. You probably
meant to type \\#.")

    ("Overfull \\\\hbox .*" .
     "Because it couldn't find a good place for a line break, TeX put more
on this line than it should.")

    ("Overfull \\\\vbox .*" .
     "Because it couldn't find a good place for a page break, TeX put more
on the page than it should. ")

    ("Underfull \\\\hbox .*" .
     "Check your output for extra vertical space.  If you find some, it was
probably caused by a problem with a \\\\ or \\newline command---for
example, two \\\\ commands in succession. This warning can also be
caused by using the sloppypar environment or \\sloppy declaration, or
by inserting a \\linebreak command.")

    ("Underfull \\\\vbox .*" .
     "TeX could not find a good place to break the page, so it produced a
page without enough text on it. ")

    ;; New list items should be placed here
    ;;
    ;; ("err-regexp" . "context")
    ;;
    ;; the err-regexp item should match anything

    (".*" . "No help available"))	; end definition
  "A list of the form (\"err-regexp\" . \"context\") used by function
`TeX-help-error' to display help-text on an error message or warning.
err-regexp should be a regular expression matching the error message
given from TeX/LaTeX, and context should be some lines describing that
error."
  :group 'TeX-output
  :type '(repeat (cons :tag "Entry"
		       (regexp :tag "Match")
		       (string :format "Description:\n%v"))))

;;; - Help

(defgroup TeX-error-description-faces nil
  "Faces used in error descriptions."
  :prefix "TeX-error-description-"
  :group 'TeX-output)

(defface TeX-error-description-error
  ;; This is the same as `error' face in latest GNU Emacs versions.
  '((((class color) (min-colors 88) (background light))
     :foreground "Red1" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :foreground "Pink" :weight bold)
    (((class color) (min-colors 16) (background light))
     :foreground "Red1" :weight bold)
    (((class color) (min-colors 16) (background dark))
     :foreground "Pink" :weight bold)
    (((class color) (min-colors 8))
     :foreground "red" :weight bold)
    (t (:inverse-video t :weight bold)))
  "Face for \"Error\" string in error descriptions.")

(defface TeX-error-description-warning
  ;; This is the same as `warning' face in latest GNU Emacs versions.
  '((((class color) (min-colors 16)) :foreground "DarkOrange" :weight bold)
    (((class color)) :foreground "yellow" :weight bold))
  "Face for \"Warning\" string in error descriptions.")

(defface TeX-error-description-tex-said
  ;; This is the same as `font-lock-function-name-face' face in latest GNU
  ;; Emacs versions.
  '((((class color) (min-colors 88) (background light))
     :foreground "Blue1")
    (((class color) (min-colors 88) (background dark))
     :foreground "LightSkyBlue")
    (((class color) (min-colors 16) (background light))
     :foreground "Blue")
    (((class color) (min-colors 16) (background dark))
     :foreground "LightSkyBlue")
    (((class color) (min-colors 8))
     :foreground "blue" :weight bold)
    (t (:inverse-video t :weight bold)))
  "Face for \"TeX said\" string in error descriptions.")

(defface TeX-error-description-help
  '((t (:inherit TeX-error-description-tex-said)))
  "Face for \"Help\" string in error descriptions.")

(defun TeX-help-error (error output runbuffer type)
  "Print ERROR in context OUTPUT from RUNBUFFER in another window.
TYPE is a symbol specifing if ERROR is a real error, a warning or
a bad box."

  (let ((old-buffer (current-buffer))
	(log-file (with-current-buffer runbuffer
		    (with-current-buffer TeX-command-buffer
		      (expand-file-name (TeX-active-master "log")))))
	(TeX-error-pointer 0))

    ;; Find help text entry.
    (while (not (string-match (car (nth TeX-error-pointer
					TeX-error-description-list))
			      error))
      (setq TeX-error-pointer (+ TeX-error-pointer 1)))

    (TeX-pop-to-buffer (get-buffer-create "*TeX Help*") nil t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (cond
	((equal type 'error)
	 (propertize "ERROR" 'font-lock-face 'TeX-error-description-error))
	((equal type 'warning)
	 (propertize "WARNING" 'font-lock-face 'TeX-error-description-warning))
	((equal type 'bad-box)
	 (propertize "BAD BOX" 'font-lock-face 'TeX-error-description-warning)))
       ": " error
       (propertize "\n\n--- TeX said ---" 'font-lock-face
		   'TeX-error-description-tex-said)
       output
       (propertize "\n--- HELP ---\n" 'font-lock-face
		   'TeX-error-description-help)
       (let ((help (cdr (nth TeX-error-pointer
			     TeX-error-description-list))))
	 (save-excursion
	   (if (and (= (1+ TeX-error-pointer)
		       (length TeX-error-description-list))
		    (let* ((log-buffer (find-buffer-visiting log-file)))
		      (if log-buffer
			  (progn
			    (set-buffer log-buffer)
			    (revert-buffer t t))
			(setq log-buffer
			      (find-file-noselect log-file))
			(set-buffer log-buffer))
		      (auto-save-mode nil)
		      (setq buffer-read-only t)
		      (goto-char (point-min))
		      (search-forward error nil t 1))
		    (re-search-forward "^l\\." nil t)
		    (re-search-forward "^ [^\n]+$" nil t))
	       (let ((start (1+ (point))))
		 (forward-char 1)
		 (re-search-forward "^$")
		 (concat "From the .log file...\n\n"
			 (buffer-substring start (point))))
	     help)))))
    (goto-char (point-min))
    (TeX-special-mode)
    (TeX-pop-to-buffer old-buffer nil t)))

;;; Error Overview

(defvar TeX-error-overview-active-buffer nil
  "The active buffer for the current error overview.")

(defvar TeX-error-overview-orig-frame nil
  "Frame from which the error overview has been launched.")

(defvar TeX-error-overview-orig-window nil
  "Window from which the error overview has been launched.")

(defcustom TeX-error-overview-setup nil
  "The frame setup of the error overview.

The possible value is: `separate-frame' (error oveview in a
separate frame); with a nil value the current frame is used.

If the display does not support multi frame, the current frame
will be used regardless of the value of this variable."
  :group 'TeX-output
  :type '(choice
          (const :tag "Error overview in separate frame" separate-frame)
          (const :tag "Use current frame" nil)))

(defun TeX-error-overview-setup ()
  "Return the frame setup of the error overview for the current display."
  (and (display-multi-frame-p) TeX-error-overview-setup))

(defun TeX-error-overview-goto-source (&optional button)
  "Go to the error point in the source.
If optional argument BUTTON is non-nil, go to source associated
to the selected error."
  (interactive)
  (let ((index (if button (button-get button 'id) (tabulated-list-get-id)))
	item window)
    (if index
	(progn
	  ;; Select the source frame/window, if still live.
	  (if (TeX-error-overview-setup)
	      (if (frame-live-p TeX-error-overview-orig-frame)
		  (select-frame TeX-error-overview-orig-frame)
		(error "You have deleted a vital frame---\
please restart TeX error overview"))
	    (if (window-live-p TeX-error-overview-orig-window)
		(select-window TeX-error-overview-orig-window)
	      (error "You have deleted a vital window---\
please restart TeX error overview")))
	  ;; Get the error details.
	  (with-current-buffer TeX-error-overview-active-buffer
	    (setq item (nth index TeX-error-list)
		  TeX-error-last-visited index))
	  ;; Find the error and display the help.
	  (with-current-buffer TeX-command-buffer
	    ;; For consistency with `TeX-parse-TeX', use the major mode of
	    ;; `TeX-command-buffer' when visiting the error point.
	    (let ((default-major-mode major-mode))
	      ;; Find the error and display the help.
	      (apply #'TeX-find-display-help item)))
	  ;; Return to the error overview.
	  (if (TeX-error-overview-setup)
	      (select-frame TeX-error-overview-frame)
	    (if (setq window
		      (get-buffer-window TeX-error-overview-buffer-name))
		;; If error overview window is visible just select it.
		(select-window window)
	      ;; Otherwise, split the help window and display the error overview
	      ;; near to it.  This should be the only reason for the error
	      ;; overview window not being still visible after the beginning of
	      ;; the function.
	      (select-window
	       (get-buffer-window (cond
				   ((eq TeX-display-help 'expert)
				    TeX-error-overview-active-buffer)
				   (TeX-display-help  "*TeX Help*"))))
	      (if (window-splittable-p (selected-window) t)
		  (split-window-horizontally)
		(split-window-vertically))
	      (switch-to-buffer TeX-error-overview-buffer-name))))
      (message "No more errors.")
      (beep))))

(defun TeX-error-overview-make-entries (&optional master-dir active-buffer)
  "Generate the list of errors to be printed using `tabulated-list-entries'.
Write file names relative to MASTER-DIR when they are not absolute.

ACTIVE-BUFFER is used as buffer from which to extract the list of
errors.  If nil, defaults to `TeX-error-overview-active-buffer'."
  (with-current-buffer (or active-buffer TeX-error-overview-active-buffer)
    (let ((id 0)
	  type file line msg entries)
      (mapc
       (lambda (entry)
	 (setq type (nth 0 entry)
	       file (nth 1 entry)
	       line (nth 2 entry)
	       msg  (nth 3 entry))
	 ;; Add the entry only if it isn't to be skipped.
	 (unless (TeX-error-list-skip-warning-p type (nth 10 entry))
	   (push
	    (list
	     ;; ID.
	     id
	     (vector
	      ;; File.
	      (if (stringp file)
		  (if (file-name-absolute-p file)
		      file
		    (file-relative-name file master-dir))
		"")
	      ;; Line.
	      (if (numberp line)
		  (number-to-string line)
		"")
	      ;; Type.
	      (cond
	       ((equal type 'error)
		(propertize "Error" 'font-lock-face 'TeX-error-description-error))
	       ((equal type 'warning)
		(propertize "Warning" 'font-lock-face
			    'TeX-error-description-warning))
	       ((equal type 'bad-box)
		(propertize "Bad box" 'font-lock-face
			    'TeX-error-description-warning))
	       (t
		""))
	      ;; Message.
	      (list (if (stringp msg)
			;; Sometimes, the message can be longer than one line,
			;; but print here only the first one.
			(progn
			  (string-match "^.*" msg)
			  (match-string 0 msg))
		      "")
		    'face 'link
		    'follow-link t
		    'id id
		    'action 'TeX-error-overview-goto-source)))
	    entries))
	 ;; Increase the `id' counter in any case.
	 (setq id (1+ id)))
       TeX-error-list)
      (reverse entries))))

(defun TeX-error-overview-next-error (&optional arg)
  "Move to the next line and find the associated error.

A prefix ARG specifies how many error messages to move; negative
means move back to previous error messages."
  (interactive "p")
  (if (= (forward-line arg) 0)
      (TeX-error-overview-goto-source)
    ;; If there are lines left to move we are at the beginning or at the end of
    ;; the buffer and there are no more errors.
    (message "No more errors.")
    (beep)))

(defun TeX-error-overview-previous-error (&optional arg)
  "Move to the previous line and find the associated error.

Prefix arg N says how many error messages to move backward (or
forward, if negative)."
  (interactive "p")
  (TeX-error-overview-next-error (- arg)))

(defun TeX-error-overview-jump-to-source ()
  "Display the help and move point to the error source."
  (interactive)
  (TeX-error-overview-goto-source)
  (pop-to-buffer
   (save-window-excursion
     (select-window TeX-error-overview-orig-window)
     (current-buffer))))

(defun TeX-error-overview-goto-log ()
  "Display the current error in log buffer."
  (interactive)
  (let ((TeX-display-help 'expert))
    (TeX-error-overview-goto-source)))

(defun TeX-error-overview-toggle-debug-bad-boxes ()
  "Run `TeX-toggle-debug-bad-boxes' and update entries list."
  (interactive)
  (TeX-toggle-debug-bad-boxes)
  (setq tabulated-list-entries
	(TeX-error-overview-make-entries
	 (with-current-buffer TeX-command-buffer (TeX-master-directory))))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun TeX-error-overview-toggle-debug-warnings ()
  "Run `TeX-toggle-debug-warnings' and update entries list."
  (interactive)
  (TeX-toggle-debug-warnings)
  (setq tabulated-list-entries
	(TeX-error-overview-make-entries
	 (with-current-buffer TeX-command-buffer (TeX-master-directory))))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun TeX-error-overview-toggle-suppress-ignored-warnings ()
  "Toggle visibility of ignored warnings and update entries list."
  (interactive)
  (TeX-toggle-suppress-ignored-warnings)
  (setq tabulated-list-entries
	(TeX-error-overview-make-entries
	 (with-current-buffer TeX-command-buffer (TeX-master-directory))))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun TeX-error-overview-quit ()
  "Delete the window or the frame of the error overview."
  (interactive)
  (if (TeX-error-overview-setup)
      (delete-frame TeX-error-overview-frame)
    (delete-window))
  (setq TeX-error-overview-orig-frame nil))

(defvar TeX-error-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b"    'TeX-error-overview-toggle-debug-bad-boxes)
    (define-key map "j"    'TeX-error-overview-jump-to-source)
    (define-key map "l"    'TeX-error-overview-goto-log)
    (define-key map "n"    'TeX-error-overview-next-error)
    (define-key map "p"    'TeX-error-overview-previous-error)
    (define-key map "q"    'TeX-error-overview-quit)
    (define-key map "w"    'TeX-error-overview-toggle-debug-warnings)
    (define-key map "x"    'TeX-error-overview-toggle-suppress-ignored-warnings)
    (define-key map "\C-m" 'TeX-error-overview-goto-source)
    map)
  "Local keymap for `TeX-error-overview-mode' buffers.")

(easy-menu-define TeX-error-overview-menu
  TeX-error-overview-mode-map
  "Menu used in TeX error overview mode."
  '("TeX errors"
    ["Next error" TeX-error-overview-next-error
     :help "Jump to the next error"]
    ["Previous error" TeX-error-overview-previous-error
     :help "Jump to the previous error"]
    ["Go to source" TeX-error-overview-goto-source
     :help "Show the error in the source"]
    ["Jump to source" TeX-error-overview-jump-to-source
     :help "Move point to the error in the source"]
    ["Go to log" TeX-error-overview-goto-log
     :help "Show the error in the log buffer"]
    "-"
    ["Debug Bad Boxes" TeX-error-overview-toggle-debug-bad-boxes
     :style toggle :selected TeX-debug-bad-boxes
     :help "Show overfull and underfull boxes"]
    ["Debug Warnings" TeX-error-overview-toggle-debug-warnings
     :style toggle :selected TeX-debug-warnings
     :help "Show warnings"]
    ["Ignore Unimportant Warnings"
     TeX-error-overview-toggle-suppress-ignored-warnings
     :style toggle :selected TeX-suppress-ignored-warnings
     :help "Hide specified warnings"]
    "-"
    ["Quit" TeX-error-overview-quit
     :help "Quit"]))

(defvar TeX-error-overview-list-entries nil
  "List of errors to be used in the error overview.")

(define-derived-mode TeX-error-overview-mode tabulated-list-mode
		     "TeX errors"
  "Major mode for listing TeX errors."
  (setq tabulated-list-format [("File" 25 nil)
                               ("Line" 4 nil :right-align t)
                               ("Type" 7 nil)
                               ("Message" 0 nil)]
        tabulated-list-padding 1
        tabulated-list-entries TeX-error-overview-list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (easy-menu-add TeX-error-overview-menu TeX-error-overview-mode-map))

(defcustom TeX-error-overview-frame-parameters
  '((name . "TeX errors")
    (title . "TeX errors")
    (height . 10)
    (width . 80)
    (top . (- 0))
    (left . (- 0))
    (unsplittable . t)
    (minibuffer . nil)
    (vertical-scroll-bars . t)
    (tool-bar-lines . 0))
  "Parameters of the error overview frame."
  :group 'TeX-output
  :type 'alist
  :options '((name string) (title string) (height integer) (width integer)
	     (top integer) (left integer) (unsplittable boolean)
	     (minibuffer boolean) (vertical-scroll-bars boolean)
	     (tool-bar-lines integer)))

(defcustom TeX-error-overview-open-after-TeX-run nil
  "Whether to open automatically the error overview after running TeX."
  :group 'TeX-output
  :type 'boolean)

(defun TeX-error-overview ()
  "Show an overview of the errors occurred in the last TeX run."
  (interactive)
  ;; Check requirements before start.
  (if (setq TeX-error-overview-active-buffer (TeX-active-buffer))
      ;; `TeX-error-overview-list-entries' is going to be used only as value
      ;; of `tabulated-list-entries' in `TeX-error-overview-mode'.  In
      ;; principle, we don't need `TeX-error-overview-list-entries', but
      ;; `tabulated-list-entries' is buffer-local and we need the list of
      ;; entries before creating the error overview buffer in order to
      ;; decide whether we need to show anything.
      (if (setq TeX-error-overview-list-entries
		(TeX-error-overview-make-entries
		 (TeX-master-directory)))
	  (progn
	    (setq TeX-error-overview-orig-window (selected-window)
		  TeX-error-overview-orig-frame
		  (window-frame TeX-error-overview-orig-window))
	    ;; Create the error overview buffer.  This is
	    ;; automatically killed before running TeX commands, so if
	    ;; exists it is up-to-date and doesn't need to be
	    ;; re-created.
	    (unless (get-buffer TeX-error-overview-buffer-name)
	      (with-current-buffer
		  (get-buffer-create TeX-error-overview-buffer-name)
		(TeX-error-overview-mode)))
	    ;; Move point to the line associated to the last visited
	    ;; error.
	    (with-current-buffer TeX-error-overview-buffer-name
	      (goto-char (point-min))
	      (forward-line (with-current-buffer
				TeX-error-overview-active-buffer
			      TeX-error-last-visited))
	      ;; Create a new frame for the error overview or display the
	      ;; buffer in the same frame, depending on the setup.
	      (if (TeX-error-overview-setup)
		  (if (frame-live-p TeX-error-overview-frame)
		      ;; Do not create a duplicate frame if there is
		      ;; already one, just select it.
		      (select-frame-set-input-focus
		       TeX-error-overview-frame)
		    ;; Create a new frame and store its name.
		    (select-frame
		     (setq TeX-error-overview-frame
			   (make-frame
			    TeX-error-overview-frame-parameters)))
		    (set-window-buffer (selected-window)
				       TeX-error-overview-buffer-name)
		    (set-window-dedicated-p (selected-window) t))
		(TeX-pop-to-buffer TeX-error-overview-buffer-name))))
	(error (concat "No error or warning to show"
		       ;; Suggest to display warnings and bad boxes with the
		       ;; appropriate key-bindings if there are such
		       ;; messages in the output buffer.  Rationale of the
		       ;; test: `TeX-error-overview-list-entries' is nil,
		       ;; but if `TeX-error-list' is not nil it means that
		       ;; there are hidden warnings/bad boxes.
		       (when (TeX-process-get-variable (TeX-active-master)
						       'TeX-error-list)
			 (format ".  Type `%s' and `%s' to display \
warnings and bad boxes"
				 (substitute-command-keys
				  "\\<TeX-mode-map>\\[TeX-toggle-debug-warnings]")
				 (substitute-command-keys
				  "\\<TeX-mode-map>\\[TeX-toggle-debug-bad-boxes]"))))))
    (error "No process for this document")))

;;; Output mode

(define-derived-mode TeX-special-mode special-mode "TeX")

(defvar TeX-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map TeX-special-mode-map)
    (define-key map "n" 'TeX-next-error)
    (define-key map "p" 'TeX-previous-error)
    (define-key map "b" 'TeX-toggle-debug-bad-boxes)
    (define-key map "w" 'TeX-toggle-debug-warnings)
    (define-key map "i" (lambda ()
                          (interactive)
                          (with-current-buffer TeX-command-buffer
                            (TeX-interactive-mode (if TeX-interactive-mode -1 1)))))
    (define-key map "s" (lambda ()
                          (interactive)
                          (with-current-buffer TeX-command-buffer
                            (TeX-source-correlate-mode (if TeX-source-correlate-mode -1 1)))))
    map)
  "Keymap for `TeX-output-mode'.")

(define-derived-mode TeX-output-mode TeX-special-mode "TeX Output"
  "Major mode for viewing TeX output.
\\{TeX-output-mode-map} "
  :syntax-table nil
  (set (make-local-variable 'revert-buffer-function)
       #'TeX-output-revert-buffer)
  ;; special-mode makes it read-only which prevents input from TeX.
  (setq buffer-read-only nil))

(defun TeX-output-revert-buffer (_ignore-auto _noconfirm)
  "Rerun the TeX command which of which this buffer was the output."
  (goto-char (point-min))
  (if (looking-at "Running `\\(.*\\)' on `\\(.*\\)' with ``\\(.*\\)''$")
      (let ((name (match-string 1))
            (file (match-string 2)))
        (with-current-buffer TeX-command-buffer
          (TeX-command name (if (string-match TeX-region file)
                                #'TeX-region-file
                              #'TeX-master-file))))
    (error "Unable to find what command to run")))

(provide 'tex-buf)

;;; tex-buf.el ends here
