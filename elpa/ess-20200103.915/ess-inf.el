;;; ess-inf.el --- Support for running S as an inferior Emacs process  -*- lexical-binding: t; -*-

;; Copyright (C) 1989-1994 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997-1999 A.J. Rossini <rossini@u.washington.edu>,
;;      Martin Maechler <maechler@stat.math.ethz.ch>.
;; Copyright (C) 2000--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2012 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: David Smith <dsmith@stats.adelaide.edu.au>
;; Created: 7 Jan 1994
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/

;;; Commentary:

;; Code for handling running ESS processes.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'tramp)
  (require 'subr-x))
(require 'ess-utils)
(require 'ess)
(require 'ess-tracebug)

(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'format-spec)
(require 'overlay)
(require 'project)

;; Don't require tramp at run time. It's an expensive library to load.
;; Instead, guard calls with (require 'tramp) and silence the byte
;; compiler.
(declare-function tramp-sh-handle-expand-file-name "tramp-sh" (name &optional dir))
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-tramp-file-p "tramp" (name))
(declare-function inferior-ess-r-mode "ess-r-mode" ())
(declare-function inferior-ess-julia-mode "ess-julia" ())
(declare-function inferior-ess-stata-mode "ess-stata-mode" ())
(declare-function extract-rectangle-bounds "rect" (start end))

(declare-function ess-mode "ess-mode" ())
(declare-function ess-complete-object-name "ess-r-completion" ())
;; FIXME:This one should not be necessary
(declare-function ess-display-help-on-object "ess-help" (object &optional command))
(declare-function ess-dump-object-into-edit-buffer "ess-mode" (object))

(defvar add-log-current-defun-header-regexp)

;; The following declares can be removed once we drop Emacs 25
(declare-function tramp-file-name-method "tramp")
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-file-name-hop "tramp")

(defcustom inferior-ess-mode-hook nil
  "Hook for customizing inferior ESS mode.
Called after `inferior-ess-mode' is entered and variables have
been initialized."
  :group 'ess-hooks
  :type 'hook)

(defvar inferior-ess-mode-syntax-table
  (let ((tab (copy-syntax-table comint-mode-syntax-table)))
    tab)
  "Syntax table for `inferior-ess-mode'.")

(defun inferior-ess--set-major-mode (dialect)
  "Set major mode according to DIALECT."
  (cond ((string= "R" dialect)
         (progn (require 'ess-r-mode)
                (inferior-ess-r-mode)))
        ((string= "julia" dialect)
         (progn (require 'ess-julia)
                (inferior-ess-julia-mode)))
        ((string= "stata" dialect)
         (progn (require 'ess-stata-mode)
                (inferior-ess-stata-mode)))
        ;; FIXME: we need this horrible hack so that
        ;; inferior-ess-mode-syntax-table gets set for
        ;; languages that still rely on the old way of doing
        ;; things (before we used define-derived-mode for
        ;; inferior modes).
        (t
         (progn
           (setq-local inferior-ess-mode-syntax-table
                       (eval (or (alist-get 'inferior-ess-mode-syntax-table ess-local-customize-alist)
                                 (alist-get 'ess-mode-syntax-table ess-local-customize-alist))))
           (inferior-ess-mode)))))

 ;;*;; Process handling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In this section:
;;;
;;; * User commands for starting an ESS process
;;; * Functions called at startup
;;; * Process handling code
;;; * Multiple process implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*;; Starting a process
(defun ess-proc-name (n name)
  "Return name of process N, as a string, with NAME prepended.
If `ess-plain-first-buffername', then initial process is number-free."
  (concat name
          (if (not (and ess-plain-first-buffername
                        (= n 1))) ; if not both first and plain-first add number
              (concat ":" (number-to-string n)))))

(defvar-local inferior-ess--local-data nil
  "Program name and arguments used to start the inferior process.")

(defun inferior-ess (start-args customize-alist &optional no-wait)
  "Start inferior ESS process.
Without a prefix argument, starts a new ESS process, or switches
to the ESS process associated with the current buffer. With
START-ARGS (perhaps specified via \\[universal-argument]), starts
the process with those args. The current buffer is used if it is
an `inferior-ess-mode' or `ess-transcript-mode' buffer.

If `ess-ask-about-transfile' is non-nil, you will be asked for a
transcript file to use.  If there is no transcript file, the
buffer name will be like *R* or *R2*, determined by
`ess-gen-proc-buffer-name-function'.

Takes the program name from the variable `inferior-ess-program'.

See Info node `(ess)Customizing startup' and
`display-buffer-alist' to control where and how the buffer is
displayed.

\(Type \\[describe-mode] in the process buffer for a list of
commands.)

CUSTOMIZE-ALIST is the list of dialect-specific variables.  When
non-nil, NO-WAIT tells ESS not to wait for the process to finish.
This may be useful for debugging."
  ;; Use the current buffer if it is in inferior-ess-mode or ess-trans-mode
  ;; If not, maybe ask about starting directory and/or transcript file.
  ;; If no transfile, use buffer *S*
  ;; This function is primarily used to figure out the Process and
  ;; buffer names to use for inferior-ess.
  (run-hooks 'ess-pre-run-hook)
  (let* ((dialect (eval (cdr (assoc 'ess-dialect customize-alist))))
         (process-environment process-environment)
         ;; Use dialect if not R, R program name otherwise
         (temp-dialect (if ess-use-inferior-program-in-buffer-name ;VS[23-02-2013]: FIXME: this should not be here
                           (if (string-equal dialect "R")
                               (file-name-nondirectory inferior-ess-r-program)
                             dialect)
                         dialect))
         (inf-buf (inferior-ess--get-proc-buffer-create temp-dialect))
         (proc-name (buffer-local-value 'ess-local-process-name inf-buf))
         (cur-dir (inferior-ess--maybe-prompt-startup-directory proc-name temp-dialect))
         (default-directory cur-dir))
    (with-current-buffer inf-buf
      ;; TODO: Get rid of this, we should rely on modes to set the
      ;; variables they need.
      (ess-setq-vars-local customize-alist)
      (inferior-ess--set-major-mode ess-dialect)
        ;; Set local variables after changing mode because they might
        ;; not be permanent
        (setq default-directory cur-dir)
        (setq inferior-ess--local-data (cons inferior-ess-program start-args))
      ;; Read the history file
      (when ess-history-file
        (setq comint-input-ring-file-name
              (expand-file-name (if (eql t ess-history-file)
                                    (concat "." ess-dialect "history")
                                  ess-history-file)
                                ess-history-directory))
        (comint-read-input-ring))
      ;; Show the buffer
      ;; TODO: Remove inferior-ess-own-frame after ESS 19.04, then just have:
      ;; (pop-to-buffer inf-buf)
      (pop-to-buffer inf-buf (with-no-warnings
                               (when inferior-ess-own-frame
                                 '(display-buffer-pop-up-frame))))
      (let ((proc (inferior-ess--start-process inf-buf proc-name start-args)))
        (ess-make-buffer-current)
        (goto-char (point-max))
        (unless no-wait
          (ess-write-to-dribble-buffer "(inferior-ess: waiting for process to start (before hook)\n")
          (ess-wait-for-process proc nil 0.01 t))
        (unless (and proc (eq (process-status proc) 'run))
          (error "Process %s failed to start" proc-name))
        (when ess-setwd-command
          (ess-set-working-directory cur-dir))
        (setq-local font-lock-fontify-region-function #'inferior-ess-fontify-region)
        (setq-local ess-sl-modtime-alist nil)
        (run-hooks 'ess-post-run-hook)
        ;; User initialization can take some time ...
        (unless no-wait
          (ess-write-to-dribble-buffer "(inferior-ess 3): waiting for process after hook")
          (ess-wait-for-process proc)))
      inf-buf)))

(defun inferior-ess--get-proc-buffer-create (name)
  "Get a process buffer, creating a new one if needed.
This always returns a process-less buffer. The variable
`ess-local-process-name' is set in the buffer with the name of
the next process to spawn. This name may be different from the
buffer name, depending on how `ess-gen-proc-buffer-name-function'
generated the latter from NAME."
  (let* ((proc-name (let ((ntry 1))
                      ;; Find the next non-existent process N (*R:N*)
                      (while (get-process (ess-proc-name ntry name))
                        (setq ntry (1+ ntry)))
                      (ess-proc-name ntry name)))
         (inf-name (funcall ess-gen-proc-buffer-name-function proc-name)))
    (let ((buf (cond
                ;; Try to use current buffer, if inferior-ess-mode but
                ;; no process
                ((and (not (comint-check-proc (current-buffer)))
                      (derived-mode-p 'inferior-ess-mode))
                 ;; Don't change existing buffer name in this case. It
                 ;; is very common to restart the process in the same
                 ;; buffer.
                 (setq proc-name ess-local-process-name)
                 (current-buffer))
                ;; Pick up a transcript file
                (ess-ask-about-transfile
                 (let ((transfilename (read-file-name
                                       "Use transcript file (default none):" nil "")))
                   (if (string= transfilename "")
                       (get-buffer-create inf-name)
                     (find-file-noselect (expand-file-name transfilename)))))
                ;; Create a new buffer or take the *R:N* buffer if
                ;; already exists (it should contain a dead process)
                (t
                 (get-buffer-create inf-name)))))
      ;; We generated a new process name but there might still be a
      ;; live process in the buffer in corner cases because of
      ;; `ess-gen-proc-buffer-name-function` or if the user renames
      ;; inferior buffers
      (when (comint-check-proc buf)
        (error "Can't start a new session in buffer `%s` because one already exists"
               inf-name))
      (with-current-buffer buf
        (setq-local ess-local-process-name proc-name))
      buf)))

(defun ess--accumulation-buffer (proc)
  (let ((abuf (process-get proc :accum-buffer)))
    (if (buffer-live-p abuf)
        abuf
      (let ((abuf (get-buffer-create (format " *%s:accum*" (process-name proc)))))
        (process-put proc :accum-buffer abuf)
        (with-current-buffer abuf
          (buffer-disable-undo)
          (setq-local inhibit-modification-hooks t))
        abuf))))

(defvar-local inferior-ess-objects-command nil
  "The language/dialect specific command for listing objects.
It is initialized from the corresponding inferior-<lang>-objects-command
and then made buffer local."); and the *-<lang>-* ones are customized!

(defvar-local ess-save-lastvalue-command nil
  "The command to save the last value.  See S section for more details.
Default depends on the ESS language/dialect and hence made buffer local")

(defvar-local ess-retr-lastvalue-command nil
  "The command to retrieve the last value.  See S section for more details.
Default depends on the ESS language/dialect and hence made buffer local")

(defun inferior-ess-fontify-region (beg end &optional verbose)
  "Fontify output by output to avoid fontification spilling over prompts."
  (let* ((buffer-undo-list t)
         (inhibit-point-motion-hooks t)
         (font-lock-dont-widen t)
         (font-lock-extend-region-functions nil)
         (pos1 beg)
         (pos2))
    (when (< beg end)
      (with-silent-modifications
        ;; fontify chunks from prompt to prompt
        (while (< pos1 end)
          (goto-char pos1)
          (comint-next-prompt 1)
          (setq pos2 (min (point) end))
          (save-restriction
            (narrow-to-region pos1 pos2)
            (font-lock-default-fontify-region pos1 pos2 verbose))
          (setq pos1 pos2))
        ;; highlight errors
        (setq compilation--parsed beg)
        `(jit-lock-bounds ,beg . ,end)))))

(defun ess-gen-proc-buffer-name:simple (proc-name)
  "Function to generate buffer name by wrapping PROC-NAME in *proc-name*.
See `ess-gen-proc-buffer-name-function'."
  (format "*%s*" proc-name))

(defun ess-gen-proc-buffer-name:directory (proc-name)
  "Function to generate buffer name by wrapping PROC-NAME in *PROC-NAME:DIR-NAME*.
DIR-NAME is a short directory name. See
`ess-gen-proc-buffer-name-function'."
  (format "*%s:%s*" proc-name (file-name-nondirectory
                               (directory-file-name default-directory))))

(defun ess-gen-proc-buffer-name:abbr-long-directory (proc-name)
  "Function to generate buffer name in the form *PROC-NAME:ABBREVIATED-LONG-DIR-NAME*.
PROC-NAME is a string representing an internal process
name. ABBREVIATED-LONG-DIR-NAME is an abbreviated full directory
name. Abbreviation is performed by `abbreviate-file-name'. See
`ess-gen-proc-buffer-name-function'."
  (format "*%s:%s*" proc-name (abbreviate-file-name default-directory)))

(defun ess-gen-proc-buffer-name:project-or-simple (proc-name)
  "Function to generate buffer name in the form *PROC-NAME:PROJECT-ROOT*.
PROC-NAME is a string representing an internal process name.
PROJECT-ROOT is directory name returned by `project-roots'. If no
project directory has been found use
`ess-gen-proc-buffer-name:simple'. See
`ess-gen-proc-buffer-name-function'."
  (if-let ((p (project-current))
           (proj (car (project-roots p))))
      (format "*%s:%s*" proc-name (file-name-nondirectory
                                   (directory-file-name proj)))
    (ess-gen-proc-buffer-name:simple proc-name)))

(defun ess-gen-proc-buffer-name:project-or-directory (proc-name)
  "Function to generate buffer name in the form *PROC-NAME:PROJECT-ROOT*.
PROC-NAME is a string representing an internal process name.
PROJECT-ROOT is directory name returned by `project-roots' if
defined. If no project directory has been found, use
`ess-gen-proc-buffer-name:directory'. See
`ess-gen-proc-buffer-name-function'."
  (if-let ((p (project-current))
           (proj (car (project-roots p))))
      (format "*%s:%s*" proc-name (file-name-nondirectory
                                   (directory-file-name proj)))
    (ess-gen-proc-buffer-name:directory proc-name)))

;; This ensures that people who have this set in their init file don't
;; get errors about undefined functions after upgrading ESS:
(define-obsolete-function-alias 'ess-gen-proc-buffer-name:projectile-or-simple
  'ess-gen-proc-buffer-name:project-or-simple "ESS 19.04")
(define-obsolete-function-alias 'ess-gen-proc-buffer-name:projectile-or-directory
  'ess-gen-proc-buffer-name:project-or-directory "ESS 19.04")

(defun inferior-ess-available-p (&optional proc)
  "Return non-nil if PROC is not busy."
  (when-let ((proc (or proc (and ess-local-process-name
                                 (get-process ess-local-process-name)))))
    (unless (process-get proc 'busy)
      (or (ess-debug-active-p proc) ; don't send empty lines in debugger
          (when-let ((last-check (process-get proc 'last-availability-check)))
            (time-less-p (process-get proc 'last-eval) last-check))
          (progn
            ;; Send an empty string and waiting a bit to make sure we are not busy.
            (process-send-string proc "\n")
            (inferior-ess-mark-as-busy proc)
            (process-put proc 'availability-check t)
            ;; Start with a very conservative waiting time and quickly average
            ;; down to the actual response.
            (let ((avresp (or (process-get proc 'average-response-time) 0.1))
                  (ts (current-time)))
              (when (accept-process-output proc (max 0.005 (* 2.0 avresp)))
                (let ((avresp (/ (+ (* 2.0 avresp)
                                    (float-time (time-subtract (current-time) ts)))
                                 3.0)))
                  (process-put proc 'average-response-time avresp)))
              (process-put proc 'last-availability-check ts))
            (not (process-get proc 'busy)))))))

(defun inferior-ess--set-status (proc string)
  "Internal function to set the status of process PROC.
Return non-nil if the process is in a ready (not busy) state."
  ;; TODO: do it in one search, use starting position, use prog1
  (let ((ready (string-match-p (concat "\\(" inferior-ess-primary-prompt "\\)\\'") string)))
    (process-put proc 'busy-end? (and ready (process-get proc 'busy)))
    ;; When "\n" inserted from inferior-ess-available-p, delete the prompt.
    (when (and ready
               (process-get proc 'availability-check)
               (string-match-p (concat "^" inferior-ess-primary-prompt "\\'") string))
      (process-put proc 'suppress-next-output? t))
    (process-put proc 'availability-check nil)
    (when ready
      (process-put proc 'running-async? nil))
    (process-put proc 'busy (not ready))
    (process-put proc 'sec-prompt
                 (when inferior-ess-secondary-prompt
                   (string-match (concat "\\(" inferior-ess-secondary-prompt "\\)\\'") string)))
    ready))

(defun inferior-ess-mark-as-busy (proc)
  (process-put proc 'busy t)
  (process-put proc 'sec-prompt nil))

(defun inferior-ess-run-callback (proc string)
  ;; callback is stored in 'callbacks proc property. Callbacks is a list that
  ;; can contain either functions to be called with two arguments PROC and
  ;; STRING, or cons cells of the form (func . suppress). If SUPPRESS is non-nil
  ;; next process output will be suppressed.
  (unless (process-get proc 'busy)
    ;; only one callback is implemented for now
    (let* ((cb (car (process-get proc 'callbacks)))
           (listp (not (functionp cb)))
           (suppress (and listp (consp cb) (cdr cb)))
           (cb (if (and listp (consp cb))
                   (car cb)
                 cb)))
      (when cb
        (when ess-verbose
          (ess-write-to-dribble-buffer "executing callback ...\n"))
        (when suppress
          (process-put proc 'suppress-next-output? t))
        (process-put proc 'callbacks nil)
        (condition-case-unless-debug err
            (funcall cb proc string)
          (error (message "%s" (error-message-string err))))))))

(defun ess--if-verbose-write-process-state (proc string &optional filter)
  (ess-if-verbose-write
   (format "\n%s:
    --> busy:%s busy-end:%s sec-prompt:%s interruptable:%s <--
    --> running-async:%s callback:%s suppress-next-output:%s <--
    --> dbg-active:%s is-recover:%s <--
    --> string:%s\n"
           (or filter "NORMAL-FILTER")
           (process-get proc 'busy)
           (process-get proc 'busy-end?)
           (process-get proc 'sec-prompt)
           (process-get proc 'interruptable?)
           (process-get proc 'running-async?)
           (if (process-get proc 'callbacks) "yes")
           (process-get proc 'suppress-next-output?)
           (process-get proc 'dbg-active)
           (process-get proc 'is-recover)
           (if (> (length string) 150)
               (format "%s .... %s" (substring string 0 50) (substring string -50))
             string))))

(defun inferior-ess-output-filter (proc string)
  "Standard output filter for the inferior ESS process PROC.
Ring Emacs bell if process output starts with an ASCII bell, and pass
the rest to `comint-output-filter'.
Taken from octave-mod.el."
  (inferior-ess--set-status proc string)
  (ess--if-verbose-write-process-state proc string)
  (inferior-ess-run-callback proc string)
  (if (process-get proc 'suppress-next-output?)
      ;; works only for suppressing short output, for time being is enough (for callbacks)
      (process-put proc 'suppress-next-output? nil)
    (comint-output-filter proc (inferior-ess-strip-ctrl-g string))))

(defun inferior-ess-strip-ctrl-g (string)
  "Strip leading `^G' character.
If STRING starts with a `^G', ring the Emacs bell and strip it.
Depending on the value of `visible-bell', either the frame will
flash or you'll hear a beep.  Taken from octave-mod.el."
  (if (string-match "^\a" string)
      (progn
        (ding)
        (setq string (substring string 1))))
  string)

(defun ess-process-sentinel (proc message)
  "Sentinel for use with ESS processes.
This marks the process with a message, at a particular time point."
  (let ((abuf (process-get proc :accum-buffer)))
    (when (buffer-live-p abuf)
      (kill-buffer abuf)))
  (let ((pbuf (process-buffer proc)))
    (when (buffer-live-p pbuf)
      (with-current-buffer pbuf
        (save-excursion
          (setq message (substring message 0 -1)) ; strip newline
          (set-buffer (process-buffer proc))
          (comint-write-input-ring)
          (goto-char (point-max))
          (insert-before-markers
           (format "\nProcess %s %s at %s\n"
                   (process-name proc) message (current-time-string))))))))

;; FIXME: This list is structured as '(("R:2") ("R")). It doesn't
;; appear the CDR are used. Can probably just be '("R:2" "R").
(defvar ess-process-name-list nil
  "Alist of active ESS processes.")

(defun inferior-ess--start-process (buf proc-name switches)
  "Make a comint process in buffer BUF with process PROC-NAME.
SWITCHES is passed to `comint-exec'. BUF is guaranteed to be a
process-less buffer because it was created with
`inferior-ess--get-proc-buffer-create'."
  (with-current-buffer buf
    (if (eq (buffer-size) 0) nil
      (goto-char (point-max))
      (insert "\^L\n")))
  (let ((process-environment
         (nconc
          (list "STATATERM=emacs"
                (format "PAGER=%s" inferior-ess-pager))
          process-environment))
        (tramp-remote-process-environment
         (nconc ;; it contains a pager already, so append
          (when (boundp 'tramp-remote-process-environment)
            (copy-sequence tramp-remote-process-environment))
          (list "STATATERM=emacs"
                (format "PAGER=%s" inferior-ess-pager)))))
    (comint-exec buf
                 proc-name
                 inferior-ess-program
                 nil
                 (split-string switches)))
  (let ((proc (get-buffer-process buf)))
    ;; Set the process hooks
    (set-process-sentinel proc 'ess-process-sentinel)
    (set-process-filter proc 'inferior-ess-output-filter)
    (inferior-ess-mark-as-busy proc)
    ;; Add this process to ess-process-name-list, if needed
    (let ((conselt (assoc proc-name ess-process-name-list)))
      (unless conselt
        (setq ess-process-name-list
              (cons (cons proc-name nil) ess-process-name-list))))
    proc))


;;*;; Requester functions called at startup

;; FIXME EMACS 25.1:
;; Deprecate `ess-directory-function' in favor of `project-find-functions'?
(defun inferior-ess--get-startup-directory ()
  (let ((dir (or (and ess--enable-experimental-projects
                      (fboundp 'project-current)
                      (cdr (project-current)))
                 (and ess-directory-function
                      (funcall ess-directory-function))
                 ess-startup-directory
                 default-directory)))
    (directory-file-name dir)))

(defun inferior-ess--maybe-prompt-startup-directory (procname dialect)
  "Possibly prompt for a startup directory.
When `ess-ask-for-ess-directory' is non-nil, prompt.  PROCNAME is
the name of the inferior process (e.g. \"R:1\"), and DIALECT is
the language dialect (e.g. \"R\")."
  (let ((default-dir (if (fboundp 'inferior-ess-r--adjust-startup-directory)
                         (inferior-ess-r--adjust-startup-directory
                          (inferior-ess--get-startup-directory) dialect)
                       (inferior-ess--get-startup-directory))))
    (if ess-ask-for-ess-directory
        (let ((prompt (format "%s starting project directory? " procname)))
          (ess-prompt-for-directory default-dir prompt))
      default-dir)))

(defun ess-prompt-for-directory (default prompt)
  "PROMPT for a directory, using DEFAULT as the usual."
  (let* ((def-dir (file-name-as-directory default))
         (the-dir (expand-file-name
                   (file-name-as-directory
                    (read-directory-name prompt def-dir def-dir t nil)))))
    (if (file-directory-p the-dir) nil
      (error "%s is not a valid directory" the-dir))
    the-dir))


;;*;; General process handling code
(defmacro with-ess-process-buffer (no-error &rest body)
  "Execute BODY in the process buffer of `ess-current-process-name'.
If NO-ERROR is t don't trigger error when there is not current
process. Symbol *proc* is bound to the current process during the
evaluation of BODY."
  (declare (indent 1) (debug t))
  `(let ((*proc* (and ess-local-process-name (get-process ess-local-process-name))))
     (if *proc*
         (with-current-buffer (process-buffer *proc*)
           ,@body)
       (unless ,no-error
         (error "No current ESS process")))))

(defmacro ess-with-current-buffer (buffer &rest body)
  "Like `with-current-buffer' but with transfer of some essential
local ESS vars like `ess-local-process-name'."
  (declare (indent 1) (debug t))
  (let ((lpn (make-symbol "lpn"))
        (dialect (make-symbol "dialect"))
        (alist (make-symbol "alist")))
    `(let ((,lpn ess-local-process-name)
           (,dialect ess-dialect)
           (,alist ess-local-customize-alist))
       (with-current-buffer ,buffer
         (ess-setq-vars-local (eval ,alist))
         (setq ess-local-process-name ,lpn)
         (setq ess-dialect ,dialect)
         ,@body))))

(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\(ess-with-current-buffer\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))))

(defun ess-get-process (&optional name use-another)
  "Return the ESS process named by NAME.
If USE-ANOTHER is non-nil, and the process NAME is not
running (anymore), try to connect to another if there is one. By
default (USE-ANOTHER is nil), the connection to another process
happens interactively (when possible)."
  (setq name (or name ess-local-process-name))
  (cl-assert name nil "No ESS process is associated with this buffer now")
  (update-ess-process-name-list)
  (cond ((assoc name ess-process-name-list)
         (get-process name))
        ((= 0 (length ess-process-name-list))
         (save-current-buffer
           (message "trying to (re)start process %s for language %s ..."
                    name ess-language)
           (ess-start-process-specific ess-language ess-dialect)
           ;; and return the process: "call me again"
           (ess-get-process name)))
        ;; else: there are other running processes
        (use-another ; connect to another running process : the first one
         (let ((other-name (car (elt ess-process-name-list 0))))
           ;; "FIXME": try to find the process name that matches *closest*
           (message "associating with *other* process '%s'" other-name)
           (ess-get-process other-name)))
        ((and (not noninteractive)
              (y-or-n-p
               (format "Process %s is not running, but others are. Switch? " name)))
         (ess-force-buffer-current (concat ess-dialect " process to use: ") 'force)
         (ess-get-process ess-current-process-name))
        (t (error "Process %s is not running" name))))

(defun inferior-ess-default-directory ()
  (ess-get-process-variable 'default-directory))

;;--- Unfinished idea (ESS-help / R-help ) -- probably not worth it...
;;- (defun ess-set-inferior-program (filename)
;;-   "Allows to set or change `inferior-ess-program', the program (file)name."
;;-   (interactive "fR executable (script) file: ")
;;-   ;; "f" : existing file {file name completion} !
;;-   (setq inferior-ess-program filename))
;; the inferior-ess-program is initialized in the customize..alist,
;; e.g. from  inferior-ess-r-program ... --> should change rather these.
;; However these really depend on the current ess-language!
;; Plan: 1) must know and use ess-language
;;       2) change the appropriate  inferior-<ESSlang>-program
;; (how?) in R/S : assign(paste("inferior-",ESSlang,"-p...."),  filename))


;;*;; Multiple process handling code

;; FIXME: It seems the only effect of this function is to remove dead
;; processes from `ess-process-name-list'. Am I missing something?
(defun ess-make-buffer-current nil
  "Make the process associated with the current buffer the current ESS process.
Returns the name of the process, or nil if the current buffer has none."
  (update-ess-process-name-list)
  ;; (if ess-local-process-name
  ;;     (setq ess-current-process-name ess-local-process-name))
  ess-local-process-name)

(defun ess-get-process-variable (var)
  "Return the variable VAR (symbol) local to ESS process called NAME (string)."
  (buffer-local-value var (process-buffer (ess-get-process ess-local-process-name))))

(defun ess-set-process-variable (var val)
  "Set variable VAR (symbol) local to ESS process called NAME (string) to VAL."
  (with-current-buffer (process-buffer (ess-get-process ess-local-process-name))
    (set var val)))

(defun ess-process-live-p (&optional proc)
  "Check if the local ess process is alive.
Return nil if current buffer has no associated process, or
process was killed. PROC defaults to `ess-local-process-name'"
  (and (or proc ess-local-process-name)
       (let ((proc (or proc (get-process ess-local-process-name))))
         (and (processp proc)
              (process-live-p proc)))))

(defun ess-process-get (propname &optional proc)
  "Return the variable PROPNAME (symbol) from the plist of the current ESS process.
PROC defaults to process with name `ess-local-process-name'."
  (process-get (or proc (get-process ess-local-process-name)) propname))

(defun ess-process-put (propname value &optional proc)
  "Set the variable PROPNAME (symbol) to VALUE in the plist of the current ESS process.
PROC defaults to the process given by `ess-local-process-name'"
  (process-put (or proc (get-process ess-local-process-name)) propname value))

(defun ess-start-process-specific (language dialect)
  "Start an ESS process.
Typically from a language-specific buffer, using LANGUAGE (and DIALECT)."
  (save-current-buffer
    (let ((dsymb (intern dialect)))
      (ess-write-to-dribble-buffer
       (format " ..start-process-specific: lang:dialect= %s:%s, current-buf=%s\n"
               language dialect (current-buffer)))
      (cond ;; ((string= dialect "R") (R))
       ;; ((string= language "S") ;
       ;;  (message "ESS process not running, trying to start R, since language = 'S")
       ;;  (R))
       ;; ((string= dialect STA-dialect-name) (stata))
       ;;general case
       ((fboundp dsymb)
        (funcall dsymb))
       (t ;; else: ess-dialect is not a function

        ;; Typically triggered from
        ;; ess-force-buffer-current("Process to load into: ")
        ;;  \-->  ess-request-a-process("Process to load into: " no-switch)
        (error "No ESS processes running; not yet implemented to start (%s,%s)"
               language dialect))))))

(defun ess-request-a-process (message &optional noswitch ask-if-1)
  "Ask for a process, and make it the current ESS process.
If there is exactly one process, only ask if ASK-IF-1 is non-nil.
Also switches to the process buffer unless NOSWITCH is non-nil.  Interactively,
NOSWITCH can be set by giving a prefix argument.
Returns the name of the selected process."
  (interactive
   (list "Switch to which ESS process? " current-prefix-arg))
                                        ; prefix sets 'noswitch
  (ess-write-to-dribble-buffer "ess-request-a-process: {beginning}\n")
  (update-ess-process-name-list)

  (setq ess-dialect (or ess-dialect
                        (ess-completing-read
                         "Set `ess-dialect'"
                         (delete-dups (list "R" "S+" (or (bound-and-true-p S+-dialect-name) "S+")
                                            "stata" (or (bound-and-true-p STA-dialect-name) "stata")
                                            "julia" "SAS" "XLS"  "ViSta")))))

  (let* ((pname-list (delq nil ;; keep only those matching dialect
                           (append
                            (mapcar (lambda (lproc)
                                      (and (equal ess-dialect
                                                  (buffer-local-value
                                                   'ess-dialect
                                                   (process-buffer (get-process (car lproc)))))
                                           (not (equal ess-local-process-name (car lproc)))
                                           (car lproc)))
                                    ess-process-name-list)
                            ;; append local only if running
                            (when (assoc ess-local-process-name ess-process-name-list)
                              (list ess-local-process-name)))))
         (num-processes (length pname-list))
         (auto-started?))
    (if (or (= 0 num-processes)
            (and (= 1 num-processes)
                 (not (equal ess-dialect ;; don't auto connect if from different dialect
                             (buffer-local-value
                              'ess-dialect
                              (process-buffer (get-process
                                               (car pname-list))))))))
        ;; try to start "the appropriate" process
        (progn
          (ess-write-to-dribble-buffer
           (concat " ... request-a-process:\n  "
                   (format
                    "major mode %s; current buff: %s; ess-language: %s, ess-dialect: %s\n"
                    major-mode (current-buffer) ess-language ess-dialect)))
          (ess-start-process-specific ess-language ess-dialect)
          (ess-write-to-dribble-buffer
           (format "  ... request-a-process: buf=%s\n" (current-buffer)))
          (setq num-processes 1
                pname-list (car ess-process-name-list)
                auto-started? t)))
    ;; now num-processes >= 1 :
    (let* ((proc-buffers (mapcar (lambda (lproc)
                                   (buffer-name (process-buffer (get-process lproc))))
                                 pname-list))
           (proc
            (if (or auto-started?
                    (and (not ask-if-1) (= 1 num-processes)))
                (progn
                  (message "using process '%s'" (car proc-buffers))
                  (car pname-list))
              ;; else
              (unless (and ess-current-process-name
                           (get-process ess-current-process-name))
                (setq ess-current-process-name nil))
              (when message
                (setq message (replace-regexp-in-string ": +\\'" "" message))) ;; <- why is this here??
              ;; ask for buffer name not the *real* process name:
              (let ((buf (ess-completing-read message (append proc-buffers (list "*new*")) nil t nil nil)))
                (if (equal buf "*new*")
                    (progn
                      (ess-start-process-specific ess-language ess-dialect) ;; switches to proc-buff
                      (caar ess-process-name-list))
                  (process-name (get-buffer-process buf))
                  ))
              )))
      (if noswitch
          (pop-to-buffer (current-buffer)) ;; VS: this is weird, but is necessary
        (pop-to-buffer (buffer-name (process-buffer (get-process proc)))))
      proc)))

(defun ess-force-buffer-current (&optional prompt force no-autostart ask-if-1)
  "Make sure the current buffer is attached to an ESS process.
If not, or FORCE (prefix argument) is non-nil, prompt for a
process name with PROMPT. If NO-AUTOSTART is nil starts the new
process if process associated with current buffer has
died. `ess-local-process-name' is set to the name of the process
selected.  `ess-dialect' is set to the dialect associated with
the process selected. ASK-IF-1 asks user for the process, even if
there is only one process running.  Returns the inferior buffer if
it was successfully forced, throws an error otherwise."
  (interactive
   (list (concat ess-dialect " process to use: ") current-prefix-arg nil))
  (let ((proc-name (ess-make-buffer-current)))
    (cond ((and (not force) proc-name (get-process proc-name)))
          ;; Make sure the source buffer is attached to a process
          ((and ess-local-process-name (not force) no-autostart)
           (error "Process %s has died" ess-local-process-name))
          ;; Request a process if `ess-local-process-name' is nil
          (t
           (let* ((prompt (or prompt "Process to use: "))
                  (proc (ess-request-a-process prompt 'no-switch ask-if-1)))
             (setq ess-local-process-name proc)))))
  (process-buffer (get-process ess-local-process-name)))

(defalias 'inferior-ess-force #'ess-force-buffer-current)

(defun ess-switch-process ()
  "Force a switch to a new underlying process."
  (interactive)
  (ess-force-buffer-current "Process to use: " 'force nil 'ask-if-1))

(defun ess-get-next-available-process (&optional dialect ignore-busy)
  "Return first available (aka not busy) process of dialect DIALECT.
DIALECT defaults to the local value of ess-dialect. Return nil if
no such process has been found."
  (setq dialect (or dialect ess-dialect))
  (when dialect
    (let (proc)
      (catch 'found
        (dolist (p (cons ess-local-process-name
                         (mapcar 'car ess-process-name-list)))
          (when p
            (setq proc (get-process p))
            (when (and proc
                       (process-live-p proc)
                       (equal dialect
                              (buffer-local-value 'ess-dialect (process-buffer proc)))
                       (or ignore-busy
                           (inferior-ess-available-p proc)))
              (throw 'found proc))))))))


;;*;;; Commands for switching to the process buffer

(defun ess-switch-to-ESS (eob-p)
  "Switch to the current inferior ESS process buffer.
With (prefix) EOB-P non-nil, positions cursor at end of buffer."
  (interactive "P")
  (ess-force-buffer-current)
  (pop-to-buffer (buffer-name (process-buffer (get-process ess-current-process-name)))
                 '(nil . ((inhibit-same-window . t))))
  (when eob-p (goto-char (point-max))))

(defun ess-switch-to-end-of-ESS ()
  "Switch to the end of the inferior ESS process buffer."
  (interactive)
  (ess-switch-to-ESS t))

(defun ess-switch-to-inferior-or-script-buffer (toggle-eob)
  "Switch between script and process buffer.
This is a single-key command. Assuming that it is bound to C-c
C-z, you can navigate back and forth between iESS and script
buffer with C-c C-z C-z C-z ... If variable
`ess-switch-to-end-of-proc-buffer' is t (the default) this
function switches to the end of process buffer. If TOGGLE-EOB is
given, the value of `ess-switch-to-end-of-proc-buffer' is
toggled."
  (interactive "P")
  (let ((eob (if toggle-eob
                 (not ess-switch-to-end-of-proc-buffer)
               ess-switch-to-end-of-proc-buffer)))
    (if (derived-mode-p 'inferior-ess-mode)
        (let ((dialect ess-dialect)
              (proc-name ess-local-process-name)
              (blist (buffer-list)))
          (while (and (pop blist)
                      (with-current-buffer (car blist)
                        (not (or (and (ess-derived-mode-p)
                                      (equal dialect ess-dialect)
                                      (null ess-local-process-name))
                                 (and (ess-derived-mode-p)
                                      (equal proc-name ess-local-process-name)))))))
          (if blist
              (pop-to-buffer (car blist))
            (message "Found no buffers for `ess-dialect' %s associated with process %s"
                     dialect proc-name)))
      (ess-switch-to-ESS eob))
    (set-transient-map (let ((map (make-sparse-keymap))
                             (key (vector last-command-event)))
                         (define-key map key #'ess-switch-to-inferior-or-script-buffer) map))))


(defun ess-get-process-buffer (&optional name)
  "Return the buffer associated with the ESS process named by NAME."
  (process-buffer (ess-get-process (or name ess-local-process-name))))

(defun update-ess-process-name-list ()
  "Remove names with no process."
  (let (defunct)
    (dolist (conselt ess-process-name-list)
      (let ((proc (get-process (car conselt))))
        (unless (and proc (eq (process-status proc) 'run))
          (push conselt defunct))))
    (dolist (pointer defunct)
      (setq ess-process-name-list (delq pointer ess-process-name-list))))
  (if (eq (length ess-process-name-list) 0)
      (setq ess-current-process-name nil)))


;;; Functions for evaluating code

;;*;; Utils for evaluation

(defun ess-build-eval-command (string &optional visibly output file &rest args)
  "Format an evaluation command.
Wrap STRING with `ess-quote-special-chars' and dispatch on
`ess-build-eval-command--override'."
  (setq string (ess-quote-special-chars string))
  (ess-build-eval-command--override string visibly output file args))

(cl-defgeneric ess-build-eval-command--override
    (string &optional _visibly _output file &rest _args)
  "Default method to build eval command."
  (and ess-eval-command
       (format-spec ess-eval-command
                    `((?s . ,string)
                      (?f . ,file)))))

(cl-defgeneric ess-build-load-command (file &optional _visibly _output &rest _args)
  "Format a loading command.
Dispatches on the dialect-specific `ess-build-load-command'
and `ess-load-command', in that order."
  (and ess-load-command
       (format ess-load-command file)))

(defun ess-wait-for-process (&optional proc sec-prompt wait force-redisplay timeout)
  "Wait for 'busy property of the process to become nil.
If SEC-PROMPT is non-nil return if secondary prompt is detected
regardless of whether primary prompt was detected or not. If WAIT
is non-nil wait for WAIT seconds for process output before the
prompt check, default 0.002s. When FORCE-REDISPLAY is non-nil
force redisplay. You better use WAIT >= 0.1 if you need
FORCE-REDISPLAY to avoid excessive redisplay. If TIMEOUT is
non-nil stop waiting for output after TIMEOUT seconds."
  (setq proc (or proc (get-process ess-local-process-name)))
  (setq wait (or wait 0.005))
  (setq timeout (or timeout most-positive-fixnum))
  (let ((start-time (float-time))
        (elapsed 0))
    (save-excursion
      (while (and
              (or (eq (process-status proc) 'run)
                  (progn
                    (when (process-buffer proc)
                      (display-buffer (process-buffer proc)))
                    (error "ESS process has died unexpectedly")))
              (< elapsed timeout)
              (or (accept-process-output proc wait)
                  (unless (and sec-prompt (process-get proc 'sec-prompt))
                    (process-get proc 'busy))))
        (when force-redisplay
          (redisplay 'force))
        (setq elapsed (- (float-time) start-time))
        (when (>  elapsed .3)
          (setq wait .3))))))

(defun inferior-ess-ordinary-filter (proc string)
  (inferior-ess--set-status proc string)
  (ess--if-verbose-write-process-state proc string "ordinary-filter")
  (inferior-ess-run-callback proc string)
  (with-current-buffer (process-buffer proc)
    (insert string)))

(defvar ess-presend-filter-functions nil
  "List of functions to call before sending the input string to the process.
Each function gets one argument, a string containing the text to
be send to the subprocess.  It should return the string sent,
perhaps the same string that was received, or perhaps a modified
or transformed string.

The functions on the list are called sequentially, and each one
is given the string returned by the previous one. The string
returned by the last function is the text that is actually sent
to the process. You can use `add-hook' to add functions to this
list either globally or locally. The hook is executed in current
buffer. Before execution, the local value of this hook in the
process buffer is appended to the hook from the current buffer.")

(defvar ess--inhibit-presend-hooks nil
  "If non-nil don't run presend hooks.")

(defun ess--run-presend-hooks (process string)
  ;; run ess-presend-filter-functions and comint-input-filter-functions
  (if ess--inhibit-presend-hooks
      string
    ;;return modified string
    (let* ((pbuf (process-buffer process))
           ;; also run proc buffer local hooks
           (functions (unless (eq pbuf (current-buffer))
                        (buffer-local-value 'ess-presend-filter-functions pbuf))))
      (setq functions (append  (delq t (copy-sequence functions)) ;; even in let, delq distructs
                               ess-presend-filter-functions))
      (while (and functions string)
        ;; cannot use run-hook-with-args here because string must be passed from one
        ;; function to another
        (if (eq (car functions) t)
            (let ((functions
                   (default-value 'ess-presend-filter-functions)))
              (while (and functions string)
                (setq string (funcall (car functions) string))
                (setq functions (cdr functions))))
          (setq string (funcall (car functions) string)))
        (setq functions (cdr functions)))
      (with-current-buffer pbuf
        (run-hook-with-args 'comint-input-filter-functions string))
      string)))

(defun ess--concat-new-line-maybe (string)
  "Append \\n at the end of STRING if missing."
  (if (string-match "\n\\'" string (max (- (length string) 2) 0))
      string
    (concat string "\n")))

(defvar ess--dbg-del-empty-p t
  "Internal variable to control removal of empty lines during the debugging.
Let-bind it to nil before calling `ess-send-string' or
`ess-send-region' if no removal is necessary.")

(defun inferior-ess--interrupt-subjob-maybe (proc)
  "Internal. Interrupt the process if interruptable? process variable is non-nil.
Hide all the junk output in temporary buffer."
  (when (process-get proc 'interruptable?)
    (let ((cb (cadr (process-get proc 'callbacks)))
          (buf (get-buffer-create " *ess-temp-buff*"))
          (old-filter (process-filter proc))
          (old-buff (process-buffer proc)))
      (unwind-protect
          (progn
            (ess-if-verbose-write "interrupting subjob ... start")
            (process-put proc 'interruptable? nil)
            (process-put proc 'callbacks nil)
            (process-put proc 'running-async? nil)
            ;; this is to avoid putting junk in user's buffer on process
            ;; interruption
            (set-process-buffer proc buf)
            (set-process-filter proc 'inferior-ess-ordinary-filter)
            (interrupt-process proc)
            (when cb
              (ess-if-verbose-write "executing interruption callback ... ")
              (funcall cb proc))
            ;; should be very fast as it inputs only the prompt
            (ess-wait-for-process proc)
            (ess-if-verbose-write "interrupting subjob ... finished")
            )
        (set-process-buffer proc old-buff)
        (set-process-filter proc old-filter)))))


;;*;; Evaluation primitives

(defun ess-send-string (process string &optional visibly message _type)
  "ESS wrapper for `process-send-string'.
Run `comint-input-filter-functions' and current buffer's and
PROCESS' `ess-presend-filter-functions' hooks on the input
STRING. VISIBLY can be nil, t, 'nowait or a string.  If string
the behavior is as with 'nowait with the differences that
inserted string is VISIBLY instead of STRING (evaluated command
is still STRING).  In all other cases the behavior is as
described in `ess-eval-visibly'. STRING need not end with
\\n. TYPE is a symbol indicating type of the string.
MESSAGE is a message to display."
  ;; No support of `visibly' when there's no secondary prompt
  (let ((visibly (if (and (eq visibly t)
                          (null inferior-ess-secondary-prompt))
                     'nowait
                   visibly))
        (string (ess--run-presend-hooks process string)))
    (inferior-ess--interrupt-subjob-maybe process)
    (inferior-ess-mark-as-busy process)
    (process-put process 'last-eval (current-time))
    (cond
     ;; Wait after each line
     ((eq visibly t)
      (let ((ess--inhibit-presend-hooks t))
        (ess-eval-linewise string)))
     ;; Insert command and eval invisibly
     ((or (stringp visibly)
          (eq visibly 'nowait))
      (with-current-buffer (process-buffer process)
        (save-excursion
          (goto-char (process-mark process))
          (insert-before-markers
           (propertize (format "%s\n"
                               (replace-regexp-in-string
                                "\n" "\n+ "
                                (if (stringp visibly) visibly string)))
                       'font-lock-face 'comint-highlight-input)))
        (process-send-string process (ess--concat-new-line-maybe string))))
     (t
      (process-send-string process (ess--concat-new-line-maybe string))))
    (when message
      (message "%s" message))))

(defun ess-send-region (process start end &optional visibly message type)
  "Low level ESS version of `process-send-region'.
If VISIBLY call `ess-eval-linewise', else call
`ess-send-string'. If MESSAGE is supplied, display it at the
end. Run current buffer's and PROCESS'
`ess-presend-filter-functions' hooks. TYPE is a symbol indicating
type of the region."
  (cond
   ((ess-tracebug-p)
    (ess-tracebug-send-region process start end visibly message type))
   (t (ess-send-region--override process start end visibly message type))))

(cl-defgeneric ess-send-region--override (process start end visibly message type)
  (ess-send-string process (buffer-substring start end) visibly message type))


;;*;; Evaluation commands

(defun ess-load-file--normalise-file (file)
  "Handle Tramp and system peculiarities."
  (require 'tramp)
  (let* ((file (if (tramp-tramp-file-p file)
                   (tramp-file-name-localname (tramp-dissect-file-name file))
                 file))
         (file (if ess-microsoft-p
                   (ess-replace-in-string file "[\\]" "/")
                 file)))
    (abbreviate-file-name file)))

(defun ess-load-file--normalise-buffer (file)
  (when (ess-save-file file)
    (error "Buffer %s has not been saved" (buffer-name file)))
  (let ((source-buffer (get-file-buffer file)))
    (if source-buffer
        (with-current-buffer source-buffer
          (when (buffer-modified-p) (save-buffer))
          (ess-force-buffer-current "Process to load into: ")
          (ess-check-modifications))
      (ess-force-buffer-current "Process to load into: "))))

;;;###autoload
(defun ess-load-file (&optional filename)
  "Load FILENAME into an inferior ESS process.
This handles Tramp when working on a remote."
  (interactive (list (or (and (ess-derived-mode-p)
                              (buffer-file-name))
                         (expand-file-name
                          (read-file-name "Load source file: " nil nil t)))))
  (ess-load-file--normalise-buffer filename)
  (setq filename (ess-load-file--normalise-file filename))
  (ess-load-file--override filename)
  (message "Loaded %s" filename))

(cl-defgeneric ess-load-file--override (filename)
  (let ((command (ess-build-load-command filename nil t)))
    (ess-send-string (ess-get-process) command t)))

;; ;;; VS[03-09-2012]: Test Cases:
;; (ess-command "a<-0\n" nil nil nil nil (get-process "R"))
;; (ess-async-command-delayed "Sys.sleep(5);a<-a+1;cat(1:10)\n" nil
;;                            (get-process "R") (lambda (proc) (message "done")))
;; (ess-async-command-delayed "Sys.sleep(5)\n" nil (get-process "R")
;;                            (lambda (proc) (message "done")))
;; (process-get (get-process "R") 'running-async?)

(defun ess-command--get-proc (proc no-prompt-check)
  (if proc
      (unless ess-local-process-name
        (setq ess-local-process-name (process-name proc)))
    (setq proc (ess-get-process ess-local-process-name)))
  (unless no-prompt-check
    (when (process-get proc 'busy)
      (user-error "ESS process not ready. Finish your command before trying again")))
  proc)

(defun ess-command (cmd &optional out-buffer _sleep no-prompt-check wait proc force-redisplay)
  "Send the ESS process CMD and delete the output from the ESS process buffer.
If an optional second argument OUT-BUFFER exists save the output
in that buffer. OUT-BUFFER is erased before use. CMD should have
a terminating newline. Guarantees that the value of `.Last.value'
will be preserved.

SLEEP is deprecated and no longer has any effect. WAIT and
FORCE-REDISPLAY are as in `ess-wait-for-process' and are passed
to `ess-wait-for-process'.

PROC should be a process, if nil the process name is taken from
`ess-local-process-name'.  This command doesn't set 'last-eval
process variable.

Note: for critical, or error prone code you should consider
wrapping the code into:

 local({
    olderr <- options(error=NULL)
    on.exit(options(olderr))
    ...
 })"
  (let ((out-buffer (or out-buffer (get-buffer-create " *ess-command-output*")))
        (proc (ess-command--get-proc proc no-prompt-check))
        ;; Set `inhibit-quit' to t to avoid dumping R output to the
        ;; process buffer if `ess-command' gets interrupted for some
        ;; reason. See bugs #794 and #842
        (inhibit-quit t))
    (with-current-buffer (process-buffer proc)
      (let ((primary-prompt inferior-ess-primary-prompt)
            (oldpb (process-buffer proc))
            (oldpf (process-filter proc))
            (oldpm (marker-position (process-mark proc))))
        (ess-if-verbose-write (format "(ess-command %s ..)" cmd))
        ;; Swap the process buffer with the output buffer before
        ;; sending the command
        (unwind-protect
            (progn
              (set-process-buffer proc out-buffer)
              (set-process-filter proc 'inferior-ess-ordinary-filter)
              (with-current-buffer out-buffer
                (setq inferior-ess-primary-prompt primary-prompt)
                (setq buffer-read-only nil)
                (erase-buffer)
                (set-marker (process-mark proc) (point-min))
                (inferior-ess-mark-as-busy proc)
                (process-send-string proc cmd)
                ;; Need time for ess-create-object-name-db on PC
                (if no-prompt-check
                    (sleep-for 0.02)   ; 0.1 is noticeable!
                  (ess-wait-for-process proc nil wait force-redisplay)
                  ;; Should (almost) never be incomplete unless the message
                  ;; contains "> " and was accidentally split by the process
                  ;; right there.
                  (while (eq :incomplete (ess-mpi-handle-messages (current-buffer)))
                    (ess-wait-for-process proc nil wait force-redisplay))
                  ;; Remove prompt
                  ;; If output is cat(..)ed this deletes the output
                  (goto-char (point-max))
                  (delete-region (point-at-bol) (point-max)))
                (ess-if-verbose-write " .. ok{ess-command}")))
          (ess-if-verbose-write " .. exiting{ess-command}\n")
          ;; Restore the process buffer in its previous state
          (set-process-buffer proc oldpb)
          (set-process-filter proc oldpf)
          (set-marker (process-mark proc) oldpm))))
    out-buffer))

(defun ess-boolean-command (com &optional buf wait)
  "Like `ess-command' but expects COM to print TRUE or FALSE.
If TRUE (or true) is found return non-nil otherwise nil.
Example (ess-boolean-command \"2>1\n\")"
  (with-current-buffer (ess-command com buf nil nil wait)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (re-search-forward "true" nil t))))

(defun ess-string-command (com &optional buf wait)
  "Returns the output of COM as a string."
  (let ((prompt inferior-ess-prompt))
    (with-current-buffer (ess-command com buf nil nil wait)
      (goto-char (point-min))
      ;; remove leading prompt
      (when (and prompt (re-search-forward (concat "^" prompt) (point-at-eol) t))
        (delete-region (point-min) (match-end 0)))
      (ess-kill-last-line)
      (buffer-substring (point-min) (point-max)))))

(defun ess-async-command (com &optional buf proc callback interrupt-callback)
  "Asynchronous version of `ess-command'.
COM, BUF, WAIT and PROC are as in `ess-command'.

CALLBACK is a function of two arguments (PROC STRING) to run
after the successful execution. When INTERRUPT-CALLBACK is
non-nil, user evaluation can interrupt the
job. INTERRUPT-CALLBACK should be either t or a function of one
argument (PROC) to be called on interruption.

NOTE: Currently this function should be used only for background
jobs like caching. ESS tries to suppress any output from the
asynchronous command, but long output of COM will most likely end
up in user's main buffer."
  (setq proc (or proc (get-process ess-local-process-name)))
  (cond ((not (and proc (eq (process-status proc) 'run)))
         (error "Process %s is dead" proc))
        ((process-get proc 'busy)
         (error "Process %s is busy" proc))
        ((process-get proc 'running-async?)
         (error "Process %s is already running an async command" proc)))
  (when (eq interrupt-callback t)
    (setq interrupt-callback (lambda (_proc))))
  (process-put proc 'callbacks (list (cons callback 'suppress-output)
                                     interrupt-callback))
  (process-put proc 'interruptable? (and interrupt-callback t))
  (process-put proc 'running-async? t)
  (ess-command com buf nil 'no-prompt-check .01 proc))

(defun ess-async-command-delayed (com buf proc &optional callback delay)
  "Delayed asynchronous ess-command.
COM and BUF are as in `ess-command'. DELAY is a number of idle
seconds to wait before starting the execution of the COM. On
interruption (by user's evaluation) ESS tries to rerun the job
after next DELAY seconds, and the whole process repeats itself
until the command manages to run completely. DELAY defaults to
`ess-idle-timer-interval' + 3 seconds. You should always provide
PROC for delayed evaluation, as the current process might change,
leading to unpredictable consequences. This function is a wrapper
of `ess-async-command' with an explicit interrupt-callback."
  (let* ((delay (or delay
                    (+ ess-idle-timer-interval 3)))
         (int-cb  `(lambda (proc)
                     (ess-async-command-delayed ,com ,buf proc ,callback ,delay)))
         (com-fun `(lambda ()
                     (when (eq (process-status ,proc)  'run) ; do nothing if not running
                       (if (or (process-get ,proc 'busy) ; if busy, try later
                               (process-get ,proc 'running-async?))
                           ;; idle timer doesn't work here
                           (run-with-timer ,delay nil 'ess-async-command-delayed
                                           ,com ,buf ,proc ,callback ,delay))
                       (ess-async-command ,com ,buf ,proc ,callback ',int-cb)))))
    (run-with-idle-timer delay nil com-fun)))

(defun ess-load-library ()
  "Prompt and load dialect specific library/package/module.
Note that in R these are called 'packages' and the name of this
function has nothing to do with R package mechanism, but it
rather serves a generic, dialect independent purpose. It is also
similar to `load-library' Emacs function."
  (interactive)
  (let ((ess-eval-visibly-p t)
        (packs (ess-installed-packages))
        pack)
    (setq pack (ess-completing-read "Load" packs))
    (ess-load-library--override pack)
    (ess--mark-search-list-as-changed)))

(cl-defgeneric ess-installed-packages ()
  "Return a list of installed packages.")

(cl-defgeneric ess-load-library--override (pack)
  "Load library/package PACK.")


;;*;;  Evaluating lines, paragraphs, regions, and buffers.

(defun ess-eval-linewise
    (text &optional invisibly eob even-empty wait-last-prompt sleep-sec wait-sec)
  "Evaluate TEXT in the ESS process buffer as if typed in w/o tabs.
Waits for prompt after each line of input, so won't break on large texts.

If optional second arg INVISIBLY is non-nil, don't echo commands.
If it is a string, just include that string. If optional third
arg EOB is non-nil go to end of ESS process buffer after
evaluation.  If optional 4th arg EVEN-EMPTY is non-nil, also send
empty text (e.g. an empty line).  If 5th arg WAIT-LAST-PROMPT is
non-nil, also wait for the prompt after the last line; if 6th arg
SLEEP-SEC is a number, ESS will call '(\\[sleep-for] SLEEP-SEC)
at the end of this function.  If the 7th arg WAIT-SEC is set, it
will be used instead of the default .001s and be passed to
\\[ess-wait-for-process].

Run `comint-input-filter-functions' and
`ess-presend-filter-functions' of the associated PROCESS on the
TEXT."
  (unless (numberp wait-sec)
    (setq wait-sec 0.001))
  (ess-force-buffer-current "Process to use: ")
  ;; Use this to evaluate some code, but don't wait for output.
  (let* ((deactivate-mark)           ; keep local {do *not* deactivate wrongly}
         (sprocess (ess-get-process ess-current-process-name))
         (sbuffer (process-buffer sprocess))
         (win (get-buffer-window sbuffer t)))
    (setq text (ess--concat-new-line-maybe
                (ess--run-presend-hooks sprocess text)))
    (with-current-buffer sbuffer
      (setq text (propertize text 'field 'input 'front-sticky t))
      (goto-char (marker-position (process-mark sprocess)))
      (if (stringp invisibly)
          (insert-before-markers (concat "*** " invisibly " ***\n")))
      ;; dbg:
      ;; dbg (ess-write-to-dribble-buffer
      ;; dbg  (format "(eval-visibly 2): text[%d]= '%s'\n" (length text) text))
      (while (or (> (length text) 0) even-empty)
        (setq even-empty nil)
        (let* ((pos (string-match "\n\\|$" text))
               (input (if (= (length text) 0)
                          "\n"
                        (concat (substring text 0 pos) "\n"))))
          (setq text (substring text (min (length text) (1+ pos))))
          (goto-char (marker-position (process-mark sprocess)))
          (if win (set-window-point win (process-mark sprocess)))
          (unless invisibly
            ;; for consistency with comint :(
            (insert (propertize input 'font-lock-face 'comint-highlight-input))
            (set-marker (process-mark sprocess) (point)))
          (inferior-ess-mark-as-busy sprocess)
          (process-send-string sprocess input))
        (when (or (> (length text) 0)
                  wait-last-prompt)
          (ess-wait-for-process sprocess t wait-sec)))
      (if eob (with-temp-buffer (buffer-name sbuffer)))
      (goto-char (marker-position (process-mark sprocess)))
      (when win
        (with-selected-window win
          (goto-char (point))
          ;; this is crucial to avoid resetting window-point
          (recenter (- -1 scroll-margin))))))
  (if (numberp sleep-sec)
      (sleep-for sleep-sec)))


;;;*;;; Evaluate only

(defun ess-eval-region--normalise-region (start end)
  "Clean the region from START to END for evaluation.
This trims newlines at beginning and end of the region because
they might throw off the debugger."
  (save-excursion
    (goto-char start)
    (skip-chars-forward "\n\t ")
    (setq start (point))
    (unless mark-active
      (ess-blink-region start end))
    (goto-char end)
    (skip-chars-backward "\n\t ")
    (setq end (point))))

(defun ess-eval-region (start end vis &optional message type)
  "Send the region from START to END to the inferior ESS process.
VIS switches the meaning of `ess-eval-visibly'. If given,
MESSAGE is `message'ed. TYPE is a symbol indicating what type of
region this is. If command `rectangle-mark-mode' is active, send
the lines of the rectangle separately to the inferior process."
  (interactive "r\nP")
  (ess-force-buffer-current "Process to use: ")
  (message "Starting evaluation...")
  (unless ess-local-customize-alist
    ;; External applications might call ess-eval-* functions; make it
    ;; easier for them
    (ess-setq-vars-local (symbol-value (ess-get-process-variable 'ess-local-customize-alist))))
  (if (bound-and-true-p rectangle-mark-mode)
      ;; If we're in rectangle-mark-mode, loop over each line of the
      ;; rectangle. Send them separately.
      (let ((reclines (extract-rectangle-bounds (min (mark) (point)) (max (mark) (point)))))
        (mapc (lambda (l)
                (ess--eval-region (car l) (cdr l) vis message type))
              reclines))
    (ess--eval-region start end vis message type)))

(defun ess--eval-region (start end vis &optional message type)
  "Helper function for `ess-eval-region', which see.
START, END, VIS, MESSAGE, and TYPE described there."
  (ess-eval-region--normalise-region start end)
  (let ((visibly (if vis (not ess-eval-visibly) ess-eval-visibly))
        (message (or message "Eval region"))
        (proc (ess-get-process)))
    (save-excursion
      (ess-send-region proc start end visibly message type)))
  (when ess-eval-deactivate-mark
    (ess-deactivate-mark))
  (list start end))

(defun ess-eval-buffer (&optional vis)
  "Send the current buffer to the inferior ESS process.
VIS has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-region (point-min) (point-max) vis "Eval buffer" 'buffer))

(defun ess-eval-buffer-from-beg-to-here (&optional vis)
  "Send region from beginning to point to the inferior ESS process.
VIS has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-region (point-min) (point) vis "Eval buffer till point"))

(defun ess-eval-buffer-from-here-to-end (&optional vis)
  "Send region from point to end of buffer to the inferior ESS process.
VIS has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-region (point) (point-max) vis "Eval buffer till end"))

(defun ess-eval-function (&optional vis)
  "Send the current function to the inferior ESS process.
Prefix arg VIS toggles visibility of ess-code as for
`ess-eval-region'. Returns nil if not inside a function."
  (interactive "P")
  (ess-force-buffer-current)
  (save-excursion
    (ignore-errors
      ;; Evaluation is forward oriented
      (forward-line -1)
      (ess-next-code-line 1))
    (let ((pos (point))
          beg end msg)
      (end-of-defun)
      (beginning-of-defun)
      ;; While we are the beginning of the function, get the function
      ;; name. FIXME: should use our ess-function-pattern.
      (setq msg (format "Eval function: %s"
                        (if (looking-at add-log-current-defun-header-regexp)
                            (match-string 1)
                          (buffer-substring (point) (point-at-eol)))))
      (setq beg (point))
      (end-of-defun)
      (setq end (point))
      (when (or (< pos beg)
                (< end pos))
        (error "Not in a function"))
      (if (ess-tracebug-p)
          (ess-tracebug-send-function (get-process ess-local-process-name) beg end vis msg)
        (ess-eval-region beg end vis msg)))))

(defun ess-eval-paragraph (&optional vis)
  "Send the current paragraph to the inferior ESS process.
Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
  (interactive "P")
  (let ((start-pos (point)))
    (if (= (point-at-bol) (point-min))
        (ess-next-code-line 0)
      ;; Evaluation is forward oriented
      (forward-line -1)
      (ess-next-code-line 1))
    (when (< (point) start-pos)
      (goto-char start-pos))
    (save-excursion
      (let ((beg (progn (backward-paragraph) (point)))
            (end (progn (forward-paragraph) (point))))
        (ess-eval-region beg end vis)))))

(defun ess-eval-function-or-paragraph (&optional vis)
  "Send the current function if \\[point] is inside one.
Otherwise send the current paragraph to the inferior ESS process.
Prefix arg VIS toggles visibility of ess-code as for
`ess-eval-region'. Returns 'function if a function was evaluated
or 'paragraph if a paragraph."
  (interactive "P")
  (condition-case nil
      (progn (ess-eval-function vis)
             'function)
    ;; TODO: Maybe be smarter than just catching all errors?
    (error (ess-eval-paragraph vis)
           'paragraph)))

(defun ess-eval-function-or-paragraph-and-step (&optional vis)
  "Send the current function if \\[point] is inside one.
Otherwise send the current paragraph to the inferior ESS process.
Prefix arg VIS toggles visibility of ess-code as for
`ess-eval-region'."
  (interactive "P")
  (ess-skip-thing (ess-eval-function-or-paragraph vis))
  (ess-next-code-line))

(defun ess-eval-region-or-function-or-paragraph (&optional vis)
  "Send the region, function, or paragraph depending on context.
Send the region if it is active. If not, send function if `point'
is inside one, otherwise the current paragraph. Treats
rectangular regions as `ess-eval-region' does. Prefix arg VIS
toggles visibility of ess-code as for `ess-eval-region'."
  (interactive "P")
  (if (use-region-p)
      (ess-eval-region (region-beginning) (region-end) vis)
    (ess-eval-function-or-paragraph vis)))

(defun ess-eval-region-or-function-or-paragraph-and-step (&optional vis)
  "Send the region, function, or paragraph depending on context.
Send the region if it is active. If not, send function if `point'
is inside one, otherwise the current paragraph. Treats
rectangular regions as `ess-eval-region' does. After evaluation
step to the next code line or to the end of region if region was
active. Prefix arg VIS toggles visibility of ess-code as for
`ess-eval-region'."
  (interactive "P")
  (ess-skip-thing (ess-eval-region-or-function-or-paragraph vis))
  (ess-next-code-line))

(defun ess-eval-region-or-line-and-step (&optional vis)
  "Evaluate region if active, otherwise `ess-eval-line-and-step'.
See `ess-eval-region' for the meaning of VIS. Treats rectangular
regions as `ess-eval-region' does."
  (interactive "P")
  (if (use-region-p)
      (ess-eval-region (region-beginning) (region-end) vis)
    (ess-eval-line-and-step)))

(defun ess-eval-region-or-line-visibly-and-step ()
  "Evaluate region if active, otherwise the current line and step.
Evaluation is done visibly.

Note that when inside a package and namespaced evaluation is in
place (see `ess-r-set-evaluation-env') evaluation of multiline
input will fail."
  (interactive)
  (ess-force-buffer-current)
  (display-buffer (ess-get-process-buffer)
                  ;; Use a different window for the process buffer:
                  '(nil (inhibit-same-window . t))
                  ;; Pass t to reusable-frames if users have help in
                  ;; own frames, otherwise help frames get split to
                  ;; display the inferior.
                  (or (equal ess-help-own-frame 'one)
                      ess-help-own-frame))
  (let ((ess-eval-visibly t))
    (ess-eval-region-or-line-and-step)))

(defun ess-eval-line (&optional vis)
  "Send the current line to the inferior ESS process.
VIS has same meaning as for `ess-eval-region'."
  (interactive "P")
  (let* ((beg (point-at-bol))
         (end (point-at-eol))
         (msg (format "Loading line: %s" (buffer-substring beg end))))
    (ess-eval-region beg end vis msg)))

(defun ess-eval-line-and-step (&optional vis)
  "Evaluate the current line and step to the \"next\" line.
See `ess-eval-region' for VIS."
  (interactive "P")
  (ess-eval-line vis)
  (ess-skip-thing 'line)
  (ess-next-code-line))

(defun ess-eval-line-visibly-and-step (&optional simple-next)
  "Evaluate the current line visibly and step to the \"next\" line.
If SIMPLE-NEXT is non-nil, possibly via prefix arg, first skip
empty and commented lines. When the variable `ess-eval-empty'
is non-nil both SIMPLE-NEXT and EVEN-EMPTY are interpreted as
true.

Note that when inside a package and namespaced evaluation is in
place (see `ess-r-set-evaluation-env'), the evaluation of
multiline input will fail."
  (interactive "P")
  (ess-force-buffer-current)
  (display-buffer (ess-get-process-buffer)
                  ;; Use a different window for the process buffer:
                  '(nil (inhibit-same-window . t))
                  ;; Pass t to reusable-frames if users have help in
                  ;; own frames, otherwise help frames get split to
                  ;; display the inferior.
                  (or (equal ess-help-own-frame 'one)
                      ess-help-own-frame))
  (let ((ess-eval-visibly t)
        (ess-eval-empty (or ess-eval-empty simple-next)))
    (ess-eval-line)
    (ess-skip-thing 'line)
    (ess-next-code-line)))

(defun ess-eval-line-invisibly-and-step ()
  "Evaluate the current line invisibly and step to the next line.
Evaluate all comments and empty lines."
  (interactive)
  (let ((ess-eval-visibly nil))
    (ess-eval-line-and-step)))
(define-obsolete-function-alias 'ess-eval-line-and-step-invisibly 'ess-eval-line-invisibly-and-step "18.10")


;;;*;;; Evaluate and switch to S

(defun ess-eval-region-and-go (start end &optional vis)
  "Send region from START to END to the inferior process buffer.
START and END default to the current region, and rectangular
regions are treated as `ess-eval-region'. VIS has same meaning as
for `ess-eval-region'."
  (interactive "r\nP")
  (ess-eval-region start end vis)
  (ess-switch-to-ESS t))

(defun ess-eval-buffer-and-go (&optional vis)
  "Send the current buffer to the inferior S and switch to the process buffer.
VIS has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-buffer vis)
  (ess-switch-to-ESS t))

(defun ess-eval-function-and-go (&optional vis)
  "Send the current function, then switch to the inferior process buffer.
VIS has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-function vis)
  (ess-switch-to-ESS t))

(defun ess-eval-line-and-go (&optional vis)
  "Send the current line, then switch to the inferior process buffer.
VIS has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-line vis)
  (ess-switch-to-ESS t))

(defun ess-eval-paragraph-and-go (&optional vis)
  "Send the current paragraph, then switch to the inferior process buffer.
VIS has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-paragraph vis)
  (ess-switch-to-ESS t))

(defun ess-eval-paragraph-and-step (&optional vis)
  "Evaluate the current paragraph and move point to the next line.
If not inside a paragraph, evaluate the next one. VIS has same
meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-paragraph vis)
  (ess-skip-thing 'paragraph)
  (ess-next-code-line))

 ; Inferior ESS mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The major mode inferior-ess-mode
;;;; * Process handling code
;;;; * Completion code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*;; Major mode definition

(defvar inferior-ess-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-y"              #'ess-yank)
    (define-key map "\r"       #'inferior-ess-send-input)
    (define-key map "\C-a"     #'comint-bol)
    ;; 2010-06-03 SJE
    ;; disabled this in favor of ess-dirs.  Martin was not sure why this
    ;; key was defined anyway in this mode.
    ;;(define-key map "\M-\r"    #'ess-transcript-send-command-and-move)
    (define-key map "\C-c\M-l" #'ess-load-file)
    (define-key map "\C-c`"    #'ess-show-traceback)
    (define-key map [(control ?c) ?~] #'ess-show-call-stack)
    (define-key map "\C-c\C-d" #'ess-dump-object-into-edit-buffer)
    (define-key map "\C-c\C-v" #'ess-display-help-on-object)
    (define-key map "\C-c\C-q" #'ess-quit)
    (define-key map "\C-c\C-s" #'ess-execute-search)
    (define-key map "\C-c\C-x" #'ess-execute-objects)
    (define-key map "\C-c\034" #'ess-abort) ; \C-c\C-backslash
    (define-key map "\C-c\C-z" #'ess-switch-to-inferior-or-script-buffer) ; mask comint map
    (define-key map "\C-d"     #'delete-char)   ; EOF no good in S
    (define-key map "\t"       #'completion-at-point)
    (define-key map "\M-?"     #'ess-complete-object-name)
    (define-key map "\C-c\C-k" #'ess-request-a-process)
    (define-key map ","        #'ess-smart-comma)
    (define-key map "\C-c\C-d"  'ess-doc-map)
    (define-key map "\C-c\C-e"  'ess-extra-map)
    (define-key map "\C-c\C-t"  'ess-dev-map)
    map)
  "Keymap for `inferior-ess' mode.")

(easy-menu-define
  inferior-ess-mode-menu inferior-ess-mode-map
  "Menu for use in Inferior S mode"
  '("iESS"
    ["Quit"			ess-quit			t]
    ["Reload process"		inferior-ess-reload             t]
    ;; ["Send and move"  ess-transcript-send-command-and-move  t]
    ["Copy command"		comint-copy-old-input		t]
    ["Send command"		inferior-ess-send-input		t]
    ["Switch to script buffer"	ess-switch-to-inferior-or-script-buffer t]
    ["Get help on S object"	ess-display-help-on-object	t]
    "------"
    ("Process"
     ["Process Echoes" (lambda () (interactive)
                         (setq comint-process-echoes (not comint-process-echoes)))
      :active t
      :style toggle
      :selected comint-process-echoes]
     ("Eval visibly "
      :filter ess--generate-eval-visibly-submenu ))
    "------"
    ("Utils"
     ["Attach directory"	ess-execute-attach	t]
     ["Display object list"	ess-execute-objects	t]
     ["Display search list"	ess-execute-search	t]
     ["Edit S object"  ess-dump-object-into-edit-buffer t]
     ["Enter S command"		ess-execute		t]
     ["Jump to error"		ess-parse-errors	t]
     ["Load source file"	ess-load-file		t]
     ["Resynch S completions"	ess-resynch		t]
     ["Recreate R versions known to ESS"
      (lambda () (interactive) (ess-r-redefine-runners 'verbose)) t]
     )
    "------"
    ("start-dev" :visible nil); <-- ??
    ("end-dev" :visible nil)
    "------"
    ("Font Lock"
     :active ess-font-lock-keywords
     :filter ess--generate-font-lock-submenu)
    "------"
    ["Describe"         describe-mode                       t]
    ["Send bug report"  ess-submit-bug-report               t]
    ["About"            (ess-goto-info "Entering Commands") t]
    ))


(defvar ess-mode-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t"       #'ess-complete-object-name)
    (define-key map "\C-\M-i"  #'ess-complete-object-name) ;; doesn't work:(
    (define-key map "\C-c\C-s" #'ess-execute-search)
    (define-key map "\C-c\C-x" #'ess-execute-objects)
    map)
  "Keymap used in `ess-execute'.")

(define-derived-mode inferior-ess-mode comint-mode "iESS"
  "Major mode for interacting with an inferior ESS process.
To learn more about how to use inferior ess modes, see Info
node `(ess)Top'. If you accidentally suspend your process, use
\\[comint-continue-subjob] to continue it."
  :group 'ess-proc
  (setq-local comint-input-sender 'inferior-ess-input-sender)
  (setq-local font-lock-fontify-region-function
              #'inferior-ess-fontify-region)
  ;; If comint-process-echoes is t  inferior-ess-input-sender
  ;; recopies the input, otherwise not
  (setq-local comint-process-echoes (not (member ess-language '("SAS" "XLS" "OMG" "julia"))))

  (when comint-use-prompt-regexp ;; why comint is not setting this? bug?
    (setq-local inhibit-field-text-motion t))

  (unless inferior-ess-prompt ;; build when unset
    (setq inferior-ess-prompt
          (concat "\\("
                  inferior-ess-primary-prompt
                  (when inferior-ess-secondary-prompt "\\|")
                  inferior-ess-secondary-prompt
                  "\\)")))
  (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))

  (setq mode-line-process
        '(" ["
          ess--mode-line-process-indicator
          ess--local-mode-line-process-indicator
          "]: %s"))

  ;;; Completion support ----------------
  (remove-hook 'completion-at-point-functions 'comint-completion-at-point t) ;; reset the hook
  (add-hook 'completion-at-point-functions 'comint-c-a-p-replace-by-expanded-history nil 'local)
  (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)

  ;; hyperlinks support
  (goto-address-mode t)

  ;; Avoid spaces after filenames
  (setq-local comint-completion-addsuffix (cons "/" ""))

  (setq comint-input-autoexpand t) ; Only for completion, not on input.

  (add-hook 'window-configuration-change-hook #'ess-set-width nil t)
  (setq-local indent-tabs-mode nil)

  (setq-local paragraph-start (concat inferior-ess-primary-prompt "\\|\^L"))
  (setq-local paragraph-separate "\^L")
  (setq-local jit-lock-chunk-size inferior-ess-jit-lock-chunk-size))



;;*;; Commands used exclusively in inferior-ess-mode

;;;*;;; Main user commands

(defun inferior-ess-input-sender (proc string)
  (inferior-ess--interrupt-subjob-maybe proc)
  (let ((comint-input-filter-functions nil)) ; comint runs them, don't run twice.
    (if comint-process-echoes
        (ess-eval-linewise string nil nil ess-eval-empty)
      (ess-send-string proc string))))

(defvar ess-help-arg-regexp "\\(['\"]?\\)\\([^,=)'\"]*\\)\\1"
  "Reg(ular) Ex(pression) of help(.) arguments.  MUST: 2nd \\(.\\) = arg.")

(defun inferior-ess-send-input ()
  "Sends the command on the current line to the ESS process."
  (interactive)
  (run-hooks 'ess-send-input-hook)
  (unless (ess-process-get 'busy)
    ;; avoid new line insertion
    (ess-process-put 'prev-prompt nil))
  (comint-send-input)
  (setq ess-object-list nil))

(defun inferior-ess--goto-input-start:field ()
  "Move point to the beginning of input skipping all continuation lines.
If in the output field, goes to the beginning of previous input
field.
Note: `inferior-ess-secondary-prompt' should match exactly."
  (goto-char (field-beginning))
  ;; move to the beginning of non-output field
  (while (and (not (bobp))
              (eq (field-at-pos (point)) 'output))
    (goto-char (field-beginning nil t)))
  ;; skip all secondary prompts
  (let ((pos (field-beginning (point) t))
        (secondary-prompt (concat "^" inferior-ess-secondary-prompt)))
    (while (and pos
                (if (eq (get-text-property pos 'field) 'output)
                    (string-match secondary-prompt (field-string-no-properties pos))
                  t))
      (goto-char pos)
      (setq pos (previous-single-property-change pos 'field)))))

(defun inferior-ess--goto-input-end:field ()
  "Move point to the end of input skipping all continuation lines.
If in the output field, goes to the beginning of previous input
field. NOTE: to be used only with fields, see
`comint-use-prompt-regexp'."
  ;; this func is not used but might be useful some day
  (goto-char (field-end))
  (let ((pos (point))
        (secondary-prompt (concat "^" inferior-ess-secondary-prompt)))
    (while (and pos
                (if (eq (get-text-property pos 'field) 'output)
                    (string-match secondary-prompt (field-string-no-properties pos))
                  t))
      (goto-char pos)
      (setq pos (next-single-property-change pos 'field)))))

(defun inferior-ess--get-old-input:field ()
  "Return the ESS command surrounding point (use with fields)."
  (save-excursion
    (if (eq (field-at-pos (point)) 'output)
        (if (called-interactively-p 'any)
            (error "No command on this line")
          ;; else, just return ""
          "")
      (inferior-ess--goto-input-start:field)
      (let ((command (field-string-no-properties (point)))
            (pos (next-single-property-change (point) 'field ))
            (secondary-prompt (concat "^" inferior-ess-secondary-prompt)))
        (while (and pos
                    (cond
                     ((eq (get-text-property pos 'field) 'input)
                      (setq command (concat command "\n" (field-string-no-properties pos))))
                     ((eq (get-text-property pos 'field) 'output)
                      (string-match secondary-prompt (field-string-no-properties pos)))
                     (t)));; just skip if unknown
          (setq pos (next-single-property-change pos 'field)))
        command))))

;; TODO: error when entering a multiline function
;; check.integer <- function(N){
;;      is.integer(N) | !length(grep("[^[:digit:]]", as.character(N)))
;; }
(defun inferior-ess--goto-input-start:regexp ()
  "Move point to the beginning of input skipping all continuation lines.
If in the output field, goes to the beginning of previous input."
  (beginning-of-line)
  (unless (looking-at inferior-ess-prompt)
    (re-search-backward (concat "^" inferior-ess-prompt) nil t))
  ;; at bol
  (when (and inferior-ess-secondary-prompt
             (looking-at inferior-ess-secondary-prompt))
    (while (and (> (forward-line -1) -1)
                (looking-at inferior-ess-secondary-prompt))))
  (unless (looking-at inferior-ess-prompt)
    (error "Beginning of input not found"))
  (comint-skip-prompt))

(defun inferior-ess--get-old-input:regexp ()
  "Return the ESS command surrounding point (use regexp)."
  ;;VS[03-09-2012]: This should not rise errors!! Troubles comint-interrupt-subjob
  (save-excursion
    (let* ((inhibit-field-text-motion t)
           command)
      (beginning-of-line)
      (when  (and inferior-ess-secondary-prompt
                  (looking-at inferior-ess-secondary-prompt))
        (inferior-ess--goto-input-start:regexp))
      (beginning-of-line)
      (if (looking-at inferior-ess-prompt) ; cust.var, might not include sec-prompt
          (progn
            (comint-skip-prompt)
            (setq command (buffer-substring-no-properties (point) (point-at-eol)))
            (when inferior-ess-secondary-prompt
              (while (progn (forward-line 1)
                            (looking-at inferior-ess-secondary-prompt))
                (re-search-forward inferior-ess-secondary-prompt (point-at-eol) t)
                (setq command (concat command "\n"
                                      (buffer-substring-no-properties (point) (point-at-eol))))))
            (forward-line -1)
            command)
        (message "No command at this point")
        ""))))

(defun inferior-ess-get-old-input ()
  "Return the ESS command surrounding point."
  (if comint-use-prompt-regexp
      (inferior-ess--get-old-input:regexp)
    (inferior-ess--get-old-input:field)))


;;;*;;; Hot key commands

(defun ess-execute-objects (posn)
  "Send the objects() command to the ESS process.
By default, gives the objects at position 1.
A prefix argument toggles the meaning of `ess-execute-in-process-buffer'.
A prefix argument of 2 or more means get objects for that position.
A negative prefix argument gets the objects for that position
  and toggles `ess-execute-in-process-buffer' as well."
  (interactive "P")
  (ess-make-buffer-current)
  (let* ((num-arg (if (listp posn)
                      (if posn -1 1)
                    (prefix-numeric-value posn)))
         (the-posn (if (< num-arg 0) (- num-arg) num-arg))
         (invert (< num-arg 0))
         (the-command (format inferior-ess-objects-command the-posn ".*"))
         (the-message (concat ">>> Position "
                              (number-to-string the-posn)
                              " ("
                              (nth (1- the-posn) (ess-search-list))
                              ")\n")))
    (ess-execute the-command invert "S objects" the-message)))

(defun ess-execute-search (invert)
  "Send the `inferior-ess-search-list-command' command to the `ess-language' process.
[search(..) in S]"
  (interactive "P")
  (ess-execute inferior-ess-search-list-command  invert "S search list"))

;; FIXME --- this *only* works in S / S-plus; not in R
;; -----     ("at least" is not assigned to any key by default)
(defun ess-execute-attach (dir &optional posn)
  "Attach a directory in the `ess-language' process with the attach() command.
When used interactively, user is prompted for DIR to attach and
prefix argument is used for POSN (or 2, if absent.)
Doesn't work for data frames."
  (interactive "Attach directory: \nP")
  (ess-execute (concat "attach(\""
                       (directory-file-name (expand-file-name dir))
                       "\""
                       (if posn (concat "," (number-to-string
                                             (prefix-numeric-value posn))))
                       ")") 'buffer)
  (ess-process-put 'sp-for-help-changed? t))

(defun ess-execute-screen-options (&optional invisibly)
  "Cause S to set the \"width\" option to 1 less than the window width.
Also sets the \"length\" option to 99999. When INVISIBLY is
non-nil, don't echo to R subprocess. This is a good thing to put
in `ess-r-post-run-hook' or `ess-S+-post-run-hook'."
  (interactive)
  (if (null ess-execute-screen-options-command)
      (message "Not implemented for '%s'" ess-dialect)
    (let ((command (ess-calculate-width 'window)))
      (if invisibly
          (ess-command command)
        (ess-eval-linewise command nil nil nil 'wait-prompt)))))

(defun ess-calculate-width (opt)
  "Calculate width command given OPT.
OPT can be 'window, 'frame, or an integer. Return a command
suitable to send to the inferior process (e.g. \"options(width=80, length=999999)\")."
  (when (null ess-execute-screen-options-command)
    (error "Not implemented for %s" ess-dialect))
  (let (command)
    (cond ((integerp opt)
           (setq command (format ess-execute-screen-options-command opt)))
          ((eql 'window opt)
           ;; We cannot use (window-width) here because it returns sizes
           ;; in default (frame) characters which leads to incorrect
           ;; sizes with scaled fonts.To solve this we approximate font
           ;; width in pixels and use window-pixel-width to compute the
           ;; approximate number of characters that fit into line.
           (let* ((wedges (window-inside-pixel-edges))
                  (wwidth (- (nth 2 wedges) (nth 0 wedges)))
                  (nchars (floor (/ wwidth (default-font-width)))))
             (setq command (format ess-execute-screen-options-command
                                   nchars))))
          ((eql 'frame opt)
           (setq command
                 (format ess-execute-screen-options-command (frame-width))))
          (t (error "OPT (%s) not 'window, 'frame or an integer" opt)))
    command))

(defun ess-set-width ()
  "Set the width option.
A part of `window-configuration-change-hook' in inferior ESS
buffers."
  (when (and ess-auto-width
             ess-execute-screen-options-command)
    ;; `window-configuration-change-hook' runs with the window selected.
    (let ((proc (get-buffer-process (window-buffer)))
          command)
      ;; TODO: Set the width once the process is no longer busy.
      (when (and (process-live-p proc)
                 (not (process-get proc 'busy)))
        (setq command (ess-calculate-width ess-auto-width))
        (if ess-auto-width-visible
            (ess-eval-linewise command nil nil nil 'wait-prompt)
          (ess-command command))))))

(defun ess-execute (command &optional invert buff message)
  "Send a command to the ESS process.
A newline is automatically added to COMMAND.  Prefix arg (or second arg
INVERT) means invert the meaning of
`ess-execute-in-process-buffer'.  If INVERT is 'buffer, output is
forced to go to the process buffer.  If the output is going to a
buffer, name it *BUFF*.  This buffer is erased before use.  Optional
fourth arg MESSAGE is text to print at the top of the buffer (defaults
to the command if BUFF is not given.)"
  (interactive (list
                ;; simpler way to set proc name in mb?
                (let ((enable-recursive-minibuffers t)
                      (proc-name (progn (ess-force-buffer-current)
                                        ess-local-process-name)))
                  (with-current-buffer (get-buffer " *Minibuf-1*") ;; FIXME: hardcoded name
                    (setq ess-local-process-name proc-name))
                  (read-from-minibuffer "Execute> " nil
                                        ess-mode-minibuffer-map))
                current-prefix-arg))
  (ess-make-buffer-current)
  (let ((the-command (concat command "\n"))
        (buff-name (concat "*" (or buff "ess-output") "*"))
        (in-pbuff (if invert (or (eq invert 'buffer)
                                 (not ess-execute-in-process-buffer))
                    ess-execute-in-process-buffer)))
    (if in-pbuff
        (ess-eval-linewise the-command)
      (ess-with-current-buffer (get-buffer-create buff-name)
        (ess-command the-command (current-buffer) nil nil nil
                     (get-process ess-local-process-name))
        (ansi-color-apply-on-region (point-min) (point-max))
        (goto-char (point-min))
        (if message (insert message)
          (insert "> " the-command))
        (display-buffer (current-buffer))))))


;;;*;;; Quitting

(cl-defgeneric ess-quit--override (_arg)
  "Stops the inferior process"
  (let ((proc (ess-get-process)))
    (ess-cleanup)
    (goto-char (marker-position (process-mark proc)))
    (insert inferior-ess-exit-command)
    (process-send-string proc inferior-ess-exit-command)))

(defun ess-quit (&optional arg)
  "Issue an exiting command to the inferior process.
Runs `ess-cleanup'. ARG gets passed to a language specific
method, see `ess-quit--override'."
  (interactive "P")
  (unless (ess-process-live-p)
    (user-error "No live ESS process associated with this buffer"))
  (ess-force-buffer-current "Process to quit: ")
  (ess-interrupt)
  (ess-make-buffer-current)
  (ess-quit--override arg))

(defun ess-interrupt ()
  "Interrupt the inferior process.
This sends an interrupt and quits a debugging session."
  (interactive)
  (inferior-ess-force)
  (let ((proc (ess-get-process)))
    ;; Interrupt current task before reloading. Useful if the process is
    ;; prompting for input, for instance in R in case of a crash
    (interrupt-process proc comint-ptyp)
    ;; Workaround for Windows terminals
    (unless (memq system-type '(gnu/linux darwin))
      (process-send-string nil "\n"))
    (ess-wait-for-process proc)
    ;; Quit debugging session before reloading
    (when (ess-debug-active-p)
      (ess-debug-command-quit)
      (ess-wait-for-process proc))))

(defun ess-abort ()
  "Kill the ESS process, without executing .Last or terminating devices.
If you want to finish your session, use \\[ess-quit] instead."
;;; Provided as a safety measure over the default binding of C-c C-z in
;;; comint-mode-map.
  (interactive)
  (ding)
  (message "WARNING: \\[inferior-ess-exit-command] will not be executed and graphics devices won't finish properly!")
  (sit-for 2)
  (if (y-or-n-p "Still abort? ")
      (comint-quit-subjob)
    (message "Good move.")))

(defun ess-cleanup ()
  "Cleanup buffers associated with the process.
Possibly kill or offer to kill, depending on the value of
`ess-S-quit-kill-buffers-p', all buffers associated with this ESS
process. Uses `display-buffer' to display the process buffer. It
is run automatically by \\[ess-quit]."
  (interactive)
  (let* ((the-procname (or (ess-make-buffer-current) ess-local-process-name))
         (buf (buffer-name (process-buffer (get-process the-procname)))))
    (unless the-procname
      (error "I don't know which ESS process to clean up after!"))
    (when
        (or (eq ess-S-quit-kill-buffers-p t)
            (and
             (eq ess-S-quit-kill-buffers-p 'ask)
             (y-or-n-p
              (format
               "Delete all buffers associated with process %s? " the-procname))))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          ;; Consider buffers for which ess-local-process-name is
          ;; the same as the-procname
          (when (and (not (get-buffer-process buf))
                     ess-local-process-name
                     (equal ess-local-process-name the-procname))
            (kill-buffer buf)))))
    (display-buffer buf)
    buf))

(defun inferior-ess-reload (&optional start-args)
  "Reload the inferior process.
START-ARGS gets passed to the dialect-specific
`inferior-ess-reload-override'."
  (interactive)
  (let* ((inf-buf (inferior-ess-force))
         (inf-proc (get-buffer-process inf-buf))
         (inf-start-data (buffer-local-value 'inferior-ess--local-data inf-buf))
         (start-name (car inf-start-data))
         (start-args (or start-args (cdr inf-start-data))))
    ;; Interrupt early so we can get working directory
    (ess-interrupt)
    (save-window-excursion
      ;; Make sure we don't ask for directory again
      ;; Use current working directory as default
      (let ((project-find-functions nil)
            (ess-directory-function nil)
            (ess-startup-directory (ess-get-working-directory))
            (ess-ask-for-ess-directory nil))
        (ess-quit 'no-save)
        (inferior-ess--wait-for-exit inf-proc)
        (with-current-buffer inf-buf
          (inferior-ess-reload--override start-name start-args))))))

(cl-defgeneric inferior-ess-reload--override (_start-name _start-args)
  (user-error "Reloading not implemented for %s" ess-dialect))

(defun inferior-ess--wait-for-exit (proc)
  "Wait for process exit.
This should be used instead of `ess-wait-for-process' for waiting
after issuing a quit command as the latter assumes a live process."
  (let ((start-time (float-time)))
    (while (eq (process-status proc) 'run)
      (accept-process-output proc 0.002)
      (when (> (- (float-time) start-time) 1)
        (error "Timeout while quitting process")))))


;;;*;;; Support functions

(defun ess-extract-onames-from-alist (alist posn &optional force)
  "Return the object names in position POSN of ALIST.
ALIST is an alist like `ess-sl-modtime-alist'. POSN should be in 1 .. (length
ALIST).  If optional third arg FORCE is t, the corresponding element
of the search list is re-read. Otherwise it is only re-read if it's a
directory and has been modified since it was last read."
  (let* ((entry (nth (1- posn) alist))
         (dir (car entry))
         (timestamp (car (cdr entry)))
         (new-modtime (and timestamp
                           (ess-dir-modtime dir))))
    ;; Refresh the object listing if necessary
    (if (or force (not (equal new-modtime timestamp)))
        (setcdr (cdr entry) (ess-object-names dir posn)))
    (cdr (cdr entry))))

(defun ess-dir-modtime (dir)
  "Return the last modtime if DIR is a directory, and nil otherwise."
  (and ess-filenames-map
       (file-directory-p dir)
       (nth 5 (file-attributes dir))))

(defun ess-object-modtime (object)
  "Return the modtime of the S object OBJECT (a string).
Searches along the search list for a file named OBJECT and returns its modtime
Returns nil if that file cannot be found, i.e., for R or any non-S language!"
  (let ((path (ess-search-list))
        result)
    (while (and (not result) path)
      (setq result (file-attributes
                    (concat (file-name-as-directory (car path))
                            object)))
      (setq path (cdr path)))
    (nth 5 result)))

(defun ess-modtime-gt (mod1 mod2)
  "Return t if MOD1 is later than MOD2."
  (and mod1
       (or (> (car mod1) (car mod2))
           (and (= (car mod1) (car mod2))
                (> (car (cdr mod1)) (car (cdr mod2)))))))

(defun ess-get-object-list (name &optional exclude-first)
  "Return a list of current S object names associated with process NAME,
using `ess-object-list' if that is non-nil.
If exclude-first is non-nil, don't return objects in first positon (.GlobalEnv)."
  (or ess-object-list ;; <<-  MM: this is now always(?) nil; we cache the *-modtime-alist
      (with-current-buffer (process-buffer (ess-get-process name))
        (ess-make-buffer-current)
        (ess-write-to-dribble-buffer (format "(get-object-list %s) .." name))
        (if (or (not ess-sl-modtime-alist)
                (ess-process-get 'sp-for-help-changed?))
            (progn (ess-write-to-dribble-buffer "--> (ess-get-modtime-list)\n")
                   (ess-get-modtime-list))
          ;;else
          (ess-write-to-dribble-buffer " using existing ess-sl-modtime-alist\n"))
        (let* ((alist ess-sl-modtime-alist)
               (i 2)
               (n (length alist))
               result)
          (ess-write-to-dribble-buffer (format " (length alist) : %d\n" n))
          (unless exclude-first
            ;; re-read of position 1 :
            (setq result (ess-extract-onames-from-alist alist 1 'force)))
          (ess-write-to-dribble-buffer
           (format " have re-read pos=1: -> length %d\n" (length result)))
          ;; Re-read remaining directories if necessary.
          (while (<= i n)
            (setq result
                  (append result
                          (ess-extract-onames-from-alist alist i)))
            (setq i (1+ i)))
          (setq ess-object-list (delete-dups result))))))

(defun ess-get-words-from-vector (command &optional no-prompt-check wait proc)
  "Evaluate the S command COMMAND, which returns a character vector.
Return the elements of the result of COMMAND as an alist of
strings. COMMAND should have a terminating newline.
NO-PROMPT-CHECK, WAIT, and PROC are passed to `ess-command'.
FILTER may be the keyword 'non-... or nil. To avoid truncation of
long vectors, wrap your command (%s) like this, or a version with
explicit options(max.print=1e6): \"local({ out <- try({%s});
print(out, max=1e6) })\n\"."
  (unless proc
    (inferior-ess-force))
  (let* ((tbuffer (get-buffer-create
                   " *ess-get-words*")); initial space: disable-undo
         (word-RE
          (concat "\\("
                  "\\\\\"" "\\|" "[^\"]" ;  \" or non-"-char
                  "\\)*"))
         (full-word-regexp
          (concat "\"" "\\(" word-RE "\\)"
                  "\""
                  "\\( \\|$\\)"; space or end
                  ))
         words)
    (ess-command command tbuffer 'sleep no-prompt-check wait proc)
    (with-current-buffer tbuffer
      (goto-char (point-min))
      (while (re-search-forward full-word-regexp nil t)
        (setq words (cons (buffer-substring (match-beginning 1) (match-end 1))
                          words))))
    (ess-if-verbose-write
     (if (> (length words) 5)
         (format " |-> (length words)= %d\n" (length words))
       (format " |-> words= '%s'\n" words)))
    (reverse words)))

(defun ess-compiled-dir (dir)
  "Return non-nil if DIR is an S object directory with special files.
I.e. if the filenames in DIR are not representative of the objects in DIR."
  (or (file-exists-p (concat (file-name-as-directory dir) "___nonfile"))
      (file-exists-p (concat (file-name-as-directory dir) "__BIGIN"))
      (file-exists-p (concat (file-name-as-directory dir) "___NONFI"))))

(defun ess-object-names (obj &optional pos)
  "Return alist of S object names in directory (or object) OBJ.
If OBJ is a directory name (begins with `/') returns a listing of
that dir. This may use the search list position POS if necessary.
If OBJ is an object name, returns result of the command
`inferior-ess-safe-names-command'. If POS is supplied return the
result of the command in `inferior-ess-objects-command'. If OBJ
is nil or not a directory, POS must be supplied. In all cases,
the value is an list of object names."
  (cond ((and (stringp obj)
              (string-match-p "ESSR" obj))
         nil)
        ;; FIXME: in both cases below, the same fallback "objects(POS)" is used -- merge!
        ((and obj (file-accessible-directory-p obj))
         ;; Check the pre-compiled object list in ess-object-name-db first

         ;; FIXME: If used at all, ess-object-name-db should not only
         ;; -----  be used in the directory case !!
         (or (cdr-safe (assoc obj ess-object-name-db))
             ;; Take a directory listing
             (and ess-filenames-map
                  ;; first try .Data subdirectory:
                  ;;FIXME: move ".Data" or ``this function'' to ess-sp6-d.el etc:
                  (let ((dir (concat (file-name-as-directory obj) ".Data")))
                    (if (not (file-accessible-directory-p dir))
                        (setq dir obj))
                    (and (not (ess-compiled-dir dir))
                         (directory-files dir))))
             ;; Get objects(pos) instead
             (and (or (ess-write-to-dribble-buffer
                       (format "(ess-object-names ..): directory %s not used\n" obj))
                      t)
                  pos
                  (ess-get-words-from-vector
                   (format inferior-ess-objects-command pos)))))
        ((and obj ;; want names(obj)
              (ess-get-words-from-vector
               (format inferior-ess-safe-names-command obj))))
        (pos
         (ess-get-words-from-vector
          (format inferior-ess-objects-command pos)))))

(defun ess-slot-names (obj)
  "Return alist of S4 slot names of S4 object OBJ."
  (ess-get-words-from-vector (format "slotNames(%s)\n" obj)))

(defun ess-function-arguments (funname &optional proc)
  "Get FUNARGS from cache or ask the process for it.
Return FUNARGS - a list with the first element being a
cons (PACKAGE_NAME . TIME_STAMP), second element is a string
giving arguments of the function as they appear in documentation,
third element is a list of arguments of all methods. If PROC is
given, it should be an ESS process. If PACKAGE_NAME is nil, and
TIME_STAMP is less recent than the time of the last user
interaction to the process, then update the entry. PACKAGE_NAME
is also nil when FUNNAME was not found, or FUNNAME is a special
name that contains :,$ or @."
  (when (and funname ;; usually returned by ess--fn-name-start (might be nil)
             (or proc (ess-process-live-p)))
    (let* ((proc (or proc (get-process ess-local-process-name)))
           (cache (or (process-get proc 'funargs-cache)
                      (let ((cache (make-hash-table :test 'equal)))
                        (process-put proc 'funargs-cache cache)
                        cache)))
           (args (gethash funname cache))
           (pack (caar args))
           (ts   (cdar args)))
      (when (and args
                 (and (time-less-p ts (process-get proc 'last-eval))
                      (or (null pack)
                          (equal pack ""))))
        ;; reset cache
        (setq args nil))
      (or args
          (cadr (assoc funname (process-get proc 'funargs-pre-cache)))
	      (and
	       (not (process-get proc 'busy))
	       (with-current-buffer (ess-command (format ess-funargs-command
						                             (ess-quote-special-chars funname))
					                         nil nil nil nil proc)
	         (goto-char (point-min))
	         (when (re-search-forward "(list" nil t)
	           (goto-char (match-beginning 0))
	           (setq args (ignore-errors (eval (read (current-buffer)))))
	           (when args
		         (setcar args (cons (car args) (current-time)))))
	         ;; push even if nil
	         (puthash (substring-no-properties funname) args cache)))))))

;;; SJE: Wed 29 Dec 2004 --- remove this function.
;;; rmh: Wed 5 Jan 2005 --- bring it back for use on Windows
(defun ess-create-object-name-db ()
  "Create a database of object names in standard S directories.
This database is saved in the file specified by
`ess-object-name-db-file', and is loaded when `ess-mode' is
loaded. It defines the variable `ess-object-name-db', which is
used for completions. Before you call this function, modify the S
search list so that it contains all the non-changing (i.e.
system) S directories. All positions of the search list except
for position 1 are searched and stored in the database. After
running this command, you should move ess-namedb.el to a
directory in the `load-path'."
  (interactive)
  (setq ess-object-name-db nil)
  (let ((search-list (cdr (ess-search-list)))
        (pos 2)
        name
        (buffer (get-buffer-create " *ess-db*"))
        (temp-object-name-db nil))

    (ess-write-to-dribble-buffer
     (format "(object db): search-list=%s \n " search-list))
    (while search-list
      (message "Searching %s" (car search-list))
      (setq temp-object-name-db (cons (cons (car search-list)
                                            (ess-object-names nil pos))
                                      temp-object-name-db))
      (setq search-list (cdr search-list))
      (ess-write-to-dribble-buffer
       (format "(object db): temp-obj-name-db=%s \n pos=%s"
               temp-object-name-db pos))
      (setq pos (1+ pos)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "(setq ess-object-name-db '")
      (prin1 temp-object-name-db (current-buffer))
      (insert ")\n")
      (setq name (expand-file-name ess-object-name-db-file))
      (write-region (point-min) (point-max) name)
      (message "Wrote %s" name))
    (kill-buffer buffer)
    (setq ess-object-name-db temp-object-name-db)))

(defun ess-resynch nil
  "Reread all directories/objects in variable `ess-search-list' to form completions."
  (interactive)
  (if (ess-make-buffer-current) nil
    (error "Not an ESS process buffer"))
  (setq
   ess-sl-modtime-alist nil
   ess-object-list nil
   ess-object-name-db nil ; perhaps it would be better to reload?
   )
  (ess-process-put 'sp-for-help-changed? t)
  ;; Action! :
  (ess-get-modtime-list))

(defun ess-filename-completion ()
  "Return completion only within string or comment."
  (save-restriction ;; explicitly handle inferior-ess
    (ignore-errors
      (when (and (derived-mode-p 'inferior-ess-mode)
                 (> (point) (process-mark (get-buffer-process (current-buffer)))))
        (narrow-to-region (process-mark (get-buffer-process (current-buffer)))
                          (point-max))))
    (when (and (not (equal ?` (nth 3 (syntax-ppss (point)))))
               (ess-inside-string-or-comment-p (point)))
      (append (comint-filename-completion) '(:exclusive no)))))

(defun ess-complete-filename ()
  "Do file completion only within strings."
  (save-restriction ;; explicitly handle inferior-ess
    (ignore-errors
      (when (and (derived-mode-p 'inferior-ess-mode)
                 (> (point) (process-mark (get-buffer-process (current-buffer)))))
        (narrow-to-region (process-mark (get-buffer-process (current-buffer)))
                          (point-max))))
    (when (or (ess-inside-string-or-comment-p (point))) ;; usable within ess-mode as well
      (comint-dynamic-complete-filename))))

(defun ess-after-pathname-p nil
  ;; Heuristic: after partial pathname if it looks like we're in a
  ;; string, and that string looks like a pathname. Not the best for
  ;; use with unix() (or it's alias, !). Oh well.
  (save-excursion
    (save-match-data
      (let ((opoint (point)))
        (and (re-search-backward "\\(\"\\|'\\)[~/#$.a-zA-Z0-9][^ \t\n\"']*"
                                 nil t)
             (eq opoint (match-end 0)))))))


;;*;; Functions handling the search list

(defun ess-search-list (&optional force-update)
  "Return the current search list as a list of strings.
Elements which are apparently directories are expanded to full
dirnames. Don't try to use cache if FORCE-UPDATE is non-nil. Is
*NOT* used by \\[ess-execute-search], but by \\[ess-resynch],
\\[ess-get-object-list], \\[ess-get-modtime-list],
\\[ess-execute-objects], \\[ess-object-modtime],
\\[ess-create-object-name-db], and (indirectly) by
\\[ess-get-help-files-list]."
  (with-current-buffer
      (ess-get-process-buffer ess-current-process-name);to get *its* local vars
    (let ((result nil)
          (slist (ess-process-get 'search-list))
          (tramp-mode nil)) ;; hack for bogus file-directory-p below
      (if (and slist
               (not force-update)
               (not (ess-process-get 'sp-for-help-changed?)))
          slist
        ;; else, re-compute:
        (ess-write-to-dribble-buffer " (ess-search-list ... ) ")
        (let ((tbuffer (get-buffer-create " *search-list*"))
              (homedir default-directory)
              (my-search-cmd inferior-ess-search-list-command); from ess-buffer
              elt)
          (ess-command my-search-cmd tbuffer 0.05); <- sleep for dde only; does (erase-buffer)
          (with-current-buffer tbuffer
            ;; guaranteed by the initial space in its name: (buffer-disable-undo)
            (goto-char (point-min))
            (ess-write-to-dribble-buffer
             (format "after '%s', point-max=%d\n" my-search-cmd (point-max)))
            (while (re-search-forward "\"\\([^\"]*\\)\"" nil t)
              (setq elt (buffer-substring (match-beginning 1) (match-end 1)))
              ;;Dbg: (ess-write-to-dribble-buffer (format "  .. elt= %s \t" elt))
              (if (and (string-match "^[^/]" elt)
                       (file-directory-p (concat homedir elt)))
                  (progn
                    ;;Dbg: (ess-write-to-dribble-buffer "*IS* directory\n")
                    (setq elt (concat homedir elt)))
                ;;else
                ;;dbg
                ;;-             (ess-write-to-dribble-buffer "not dir.\n")
                )
              (setq result (append result (list elt))))
            (kill-buffer tbuffer)))
        result))))

;;; ess-sl-modtime-alist is a list with elements as follows:
;;;  * key             (directory or object name)
;;;  * modtime         (list of 2 integers)
;;;  * name, name ...  (accessible objects in search list posn labeled by key)
;;; It is a buffer-local variable (belonging to e.g. *R*, *S+6*, .. etc)
;;; and has the same number of elements and is in the same order as the
;;; S search list

(defun ess-get-modtime-list (&optional cache-var-name exclude-first)
  "Record directories in the search list, and the objects in those directories.
The result is stored in CACHE-VAR-NAME. If nil, CACHE-VAR-NAME
defaults to `ess-sl-modtime-alist'. If EXCLUDE-FIRST is non-nil
don't recompile first object in the search list."
  ;; Operation applies to process of current buffer
  (let* ((searchlist (if exclude-first
                         (cdr (ess-search-list))
                       (ess-search-list)))
         (index (if exclude-first 2 1))
         (cache-name (or cache-var-name 'ess-sl-modtime-alist))
         pack newalist)
    (while searchlist
      (setq
       pack  (car searchlist)
       newalist (append newalist
                        (list (or (assoc pack (symbol-value cache-name))
                                  (append
                                   (list pack (ess-dir-modtime pack))
                                   (prog2
                                       (message "Forming completions for %s..." pack)
                                       (ess-object-names pack index)
                                     (message "Forming completions for %s...done" pack))))))
       index  (1+ index)
       searchlist  (cdr searchlist)))
    ;;DBG:
    (ess-write-to-dribble-buffer
     (format "(%s): created new alist of length %d\n"
             cache-var-name (length newalist)))
    (set cache-name newalist)))


(defun ess-search-path-tracker (str)
  "Check if input STR changed the search path.
This function monitors user input to the inferior ESS process so
that Emacs can keep the process variable 'search-list' up to
date. `ess-completing-read' in \\[ess-read-object-name] uses this
list indirectly when it prompts for help or for an object to
dump. From ESS 12.09 this is not necessary anymore, as the search
path is checked on idle time. It is kept for robustness and
backward compatibility only."
  (when ess-change-sp-regexp
    (if (string-match ess-change-sp-regexp str)
        (ess-process-put 'sp-for-help-changed? t))))


;;; Miscellaneous routines

;;;*;;; Routines for reading object names
(defun ess-read-object-name (p-string)
  "Read an object name from the minibuffer with completion, and return it.
P-STRING is the prompt string."
  (let* ((default (ess-read-object-name-dump))
         (object-list (ess-get-object-list ess-local-process-name))
         (spec (ess-completing-read p-string object-list nil nil nil nil default)))
    (list (cond
           ((string= spec "") default)
           (t spec)))))

(defun ess-read-object-name-default ()
  "Return the object name at point, or nil if none."
  (ignore-errors
    (save-excursion
      ;; The following line circumvents an 18.57 bug in following-char
      (if (eobp) (backward-char 1))   ; Hopefully buffer is not empty!
      ;; Get onto a symbol
      (catch 'nosym          ; bail out if there's no symbol at all before point
        (while (let ((sc (char-syntax (following-char))))
                 (not (or (= sc ?w) (= sc ?_))))
          (if (bobp) (throw 'nosym nil) (backward-char 1))))
      (let*
          ((end (progn (forward-sexp 1) (point)))
           (beg (progn (backward-sexp 1) (point))))
        (buffer-substring-no-properties beg end)))))

(defun ess-read-object-name-dump ()
  "Return the object name at point, or \"Temporary\" if none."
  (ignore-errors
    (save-excursion
      ;; Get onto a symbol
      (catch 'nosym ; bail out if there's no symbol at all before point
        (while (/= (char-syntax (following-char)) ?w)
          (if (bobp) (throw 'nosym nil) (backward-char 1)))
        (let*
            ((end (progn (forward-sexp 1) (point)))
             (beg (progn (backward-sexp 1) (point)))
             (object-name (buffer-substring beg end)))
          (or object-name "Temporary"))))))

;;;; start of ess-smart-operators
;;;; inspired by slime repl shortcuts

(defvar ess--handy-history nil)

(defun ess-handy-commands ()
  "Request and execute a command from `ess-handy-commands' list."
  (interactive)
  (let* ((commands (or ess--local-handy-commands
                       ess-handy-commands))
         (hist (and (assoc (car ess--handy-history)
                           commands)
                    (car ess--handy-history))))
    (call-interactively
     (cdr (assoc (ess-completing-read "Execute"
                                      (sort (mapcar 'car commands)
                                            'string-lessp)
                                      nil t nil 'ess--handy-history hist)
                 commands)))))

(defun ess-smart-comma ()
  "If comma is invoked at the process marker of an ESS inferior
buffer, request and execute a command from `ess-handy-commands'
list."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc
             (eq (point) (marker-position (process-mark proc))))
        (ess-handy-commands)
      (if ess-smart-operators
          (progn
            (delete-horizontal-space)
            (insert ", ")
            (unless (derived-mode-p 'inferior-ess-mode)
              (indent-according-to-mode)))
        (insert ",")))))

 ; directories
(defun ess-set-working-directory (path &optional no-error)
  "Set the current working to PATH for the ESS buffer and iESS process.
NO-ERROR prevents errors when this has not been implemented for
`ess-dialect'."
  (interactive "DChange working directory to: ")
  (if ess-setwd-command
      (let* ((remote (file-remote-p path))
             (path (if remote
                       (progn
                         (require 'tramp-sh)
                         (tramp-sh-handle-expand-file-name path))
                     path))
             (lpath (if remote
                        (with-parsed-tramp-file-name path v v-localname)
                      path)))
        (ess-eval-linewise (format ess-setwd-command lpath))
        ;; use set instead of setq to take effect even when let bound
        (set 'default-directory (file-name-as-directory path)))
    (unless no-error
      (error "Not implemented for dialect %s" ess-dialect))))

(defalias 'ess-change-directory 'ess-set-working-directory)
(define-obsolete-function-alias
  'ess-use-dir 'ess-set-working-directory "ESS 18.10")

(defun ess-use-this-dir (&rest _ignore)
  "Set the current process directory to the directory of this file.
`default-directory' is used as a fallback."
  (interactive)
  (let ((dir (if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory)))
    (ess-set-working-directory (abbreviate-file-name dir))))

(defun ess-get-working-directory (&optional no-error)
  "Retrieve the current working directory from the current ess process."
  (if ess-getwd-command
      (abbreviate-file-name (car (ess-get-words-from-vector ess-getwd-command)))
    (unless no-error
      (error "Not implemented for dialect %s" ess-dialect))))

(defun ess-synchronize-dirs ()
  "Set Emacs' current directory to be the same as the subprocess directory.
To be used in `ess-idle-timer-functions'."
  (when (and ess-can-eval-in-background
             ess-getwd-command
             (inferior-ess-available-p))
    (ess-when-new-input last-sync-dirs
      (ess-if-verbose-write "\n(ess-synchronize-dirs)\n")
      (setq default-directory
            (car (ess-get-words-from-vector ess-getwd-command)))
      default-directory)))

(defun ess-dirs ()
  "Set Emacs' current directory to be the same as the *R* process."
  ;; Note: This function is not necessary anymore. The Emacs
  ;; default-directory and subprocess working directory are
  ;; synchronized automatically.
  (interactive)
  (let ((dir (car (ess-get-words-from-vector "getwd()\n"))))
    (message "(ESS / default) directory: %s" dir)
    (setq default-directory (file-name-as-directory dir))))

;; search path
(defun ess--mark-search-list-as-changed ()
  "Internal. Mark all the search-list related variables as changed."
  ;; other guys might track their own
  (ess-process-put 'sp-for-help-changed? t)
  (ess-process-put 'sp-for-ac-changed? t))

(defun ess-cache-search-list ()
  "To be used in `ess-idle-timer-functions', to set search path related variables."
  (when (and ess-can-eval-in-background
             inferior-ess-search-list-command)
    (ess-when-new-input last-cache-search-list
      (let ((path (ess-search-list 'force))
            (old-path (process-get *proc* 'search-list)))
        (when (not (equal path old-path))
          (process-put *proc* 'search-list path)
          (ess--mark-search-list-as-changed)
          path)))))


;;*;; Temporary buffer handling
(defun ess-display-temp-buffer (buff)
  "Display the buffer BUFF.
Uses `temp-buffer-show-function' and respects
`ess-display-buffer-reuse-frames'."
  (if (fboundp temp-buffer-show-function)
      (funcall temp-buffer-show-function buff))
  (display-buffer buff '(display-buffer-reuse-window) ess-display-buffer-reuse-frames))

(defun ess--inject-code-from-file (file &optional chunked)
  "Load code from FILE into process.
If CHUNKED is non-nil, split the file by  separator (must be at
bol) and load each chunk separately."
  ;; This is different from ess-load-file as it works by directly loading the
  ;; string into the process and thus works on remotes.
  (let ((proc-name ess-local-process-name)
        (dialect ess-dialect)
        (send-1 (lambda (str)
                  (if (string= ess-dialect "R")
                      ;; avoid detection of intermediate prompts
                      (ess-command (concat "{" str "}\n"))
                    (ess-command str)))))
    (with-temp-buffer
      (setq ess-local-process-name proc-name
            ess-dialect dialect)
      (insert-file-contents-literally file)
      (if chunked
          (let ((beg (point-min)))
            (goto-char beg)
            (while (re-search-forward "^" nil t)
              (funcall send-1 (buffer-substring beg (point)))
              (setq beg (point)))
            (funcall send-1 (buffer-substring (point) (point-max))))
        (funcall send-1 (buffer-string))))))

(defun ess-check-modifications nil
  "Check whether loading this file would overwrite some ESS objects
which have been modified more recently than this file, and confirm
if this is the case."
  ;; FIXME: this should really cycle through all top-level assignments in
  ;; the buffer
  ;;VS[02-04-2012|ESS 12.03]: this is sooo ugly
  (when (> (length ess-change-sp-regexp) 0)
    (and (buffer-file-name) ess-filenames-map
         (let ((sourcemod (nth 5 (file-attributes (buffer-file-name))))
               (objname))
           (save-excursion
             (goto-char (point-min))
             ;; Get name of assigned object, if we can find it
             (setq objname
                   (and
                    (re-search-forward
                     "^\\s *\"?\\(\\(\\sw\\|\\s_\\)+\\)\"?\\s *[<_]"
                     nil
                     t)
                    (buffer-substring (match-beginning 1)
                                      (match-end 1)))))
           (and
            sourcemod			; the file may have been deleted
            objname			; may not have been able to
                                        ; find name
            (ess-modtime-gt (ess-object-modtime objname) sourcemod)
            (not (y-or-n-p
                  (format
                   "The ESS object %s is newer than this file. Continue? "
                   objname)))
            (error "Aborted"))))))

(define-obsolete-function-alias 'ess-check-source #'ess-save-file "ESS 19.04")
(defun ess-save-file (file)
  "If FILE (a string) has an unsaved buffer, offer to save it.
Return t if the buffer existed and was modified, but was not
saved. If `ess-save-silently' is non-nil, the buffer is
saved without offering."
  (when-let ((buff (find-buffer-visiting file)))
    (when (and (buffer-modified-p buff)
               (or (eql ess-save-silently t)
                   (and (eql ess-save-silently 'auto)
                        (or (not compilation-ask-about-save)
                            (bound-and-true-p
                             ;; Only added in Emacs 26.1
                             auto-save-visited-mode)))
                   (y-or-n-p
                    (format "Buffer %s is modified. Save? "
                            (buffer-name buff)))))
      (with-current-buffer buff
        (save-buffer)))
    (buffer-modified-p buff)))


;;*;; Error messages

(defun ess-parse-errors (&optional showerr _reset)
  "Jump to error in last loaded ESS source file.
With prefix argument SHOWERR, only show the errors ESS reported. RESET
is for compatibility with `next-error' and is ignored."
  (interactive "P")
  (ess-make-buffer-current)
  (let ((errbuff (get-buffer ess-error-buffer-name)))
    (when (not errbuff)
      (error "You need to do a load first!"))
    (set-buffer errbuff)
    (goto-char (point-max))
    ;; FIXME: R does not give "useful" error messages by default. We
    ;; could try to use a more useful one, via
    ;; options(error=essErrorHandler)
    (cond ((re-search-backward ess-error-regexp nil t)
           (let* ((filename (buffer-substring (match-beginning 3) (match-end 3)))
                  (fbuffer (get-file-buffer filename))
                  (linenum
                   (string-to-number
                    (buffer-substring (match-beginning 2) (match-end 2))))
                  (errmess (buffer-substring (match-beginning 1) (match-end 1))))
             (if showerr
                 (ess-display-temp-buffer errbuff)
               (if fbuffer nil
                 (setq fbuffer (find-file-noselect filename))
                 (with-current-buffer fbuffer
                   ;; TODO: ess-mode is surely wrong here, but I don't
                   ;; think we need this whole function anymore?
                   (when (fboundp 'ess-mode)
                     (ess-mode))))
               (pop-to-buffer fbuffer)
               (ess-goto-line linenum))
             (princ errmess t)))
          (t
           (message "Not a syntax error.")
           (ess-display-temp-buffer errbuff)))))

(defun ess-error (msg)
  "Something bad has happened.
Display the S buffer, and cause an error displaying MSG."
  (declare (obsolete error "ESS 18.10"))
  (display-buffer (process-buffer (get-process ess-local-process-name)))
  (error msg))

(provide 'ess-inf)
;;; ess-inf.el ends here
