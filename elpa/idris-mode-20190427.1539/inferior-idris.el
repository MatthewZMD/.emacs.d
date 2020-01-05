;;; inferior-idris.el --- Run an Idris interpreter using S-Expression communication protocol -*- lexical-binding: t -*-

;; Copyright (C) 2013 Hannes Mehnert

;; Author: Hannes Mehnert <hannes@mehnert.org>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'idris-core)
(require 'idris-settings)
(require 'idris-common-utils)
(require 'pp)
(require 'cl-lib)
(require 'idris-events)
(require 'idris-log)
(require 'idris-warnings)

;;; Words of encouragement - strongly inspired by Slime
(defun idris-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))


(defvar idris-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    ,(format "%s, this could be the start of a beautiful program."
             (idris-user-first-name))
    ,(format "%s, this could be the start of a beautiful proof."
             (idris-user-first-name))
    "The terms have seized control of the means of computation - a glorious future awaits!"
    "It typechecks! Ship it!"
    "Do you know 'Land of My Fathers'?"
    "Constructors are red / Types are blue / Your code always works / Because Idris loves you"))

(defun idris-random-words-of-encouragement ()
  "Return a random string of encouragement"
  (nth (random (length idris-words-of-encouragement))
       idris-words-of-encouragement))

;;; Process stuff
(defvar idris-process nil
  "The Idris process.")

(defvar idris-connection nil
  "The Idris connection.")

(defvar idris-protocol-version 0 "The protocol version")

(defun idris-version-hook-function (event)
  (pcase event
    (`(:protocol-version ,version ,_target)
     (setf idris-protocol-version version)
     (remove-hook 'idris-event-hooks 'idris-version-hook-function)
     t)))

(defvar-local idris-load-packages nil
  "The list of packages to be loaded by Idris. Set using file or directory variables.")

(defun idris-compute-flags ()
  "Calculate the command line options to use when running Idris."
  (append (cl-loop for p in idris-load-packages
                   collecting "-p"
                   collecting p)
          idris-interpreter-flags
          (cl-mapcan #'funcall
                     idris-command-line-option-functions)))

(defvar idris-current-flags nil
  "The list of command-line-args actually passed to Idris. This
  is maintained to restart Idris when the arguments change.")

(autoload 'idris-prover-event-hook-function "idris-prover.el")
(autoload 'idris-quit "idris-commands.el")
(defun idris-run ()
  "Run an inferior Idris process."
  (interactive)
  (let ((command-line-flags (idris-compute-flags)))
    ;; Kill the running Idris if the command-line flags need updating
    (when (and (get-buffer-process (get-buffer (idris-buffer-name :connection)))
               (not (equal command-line-flags idris-current-flags)))
      (message "Idris command line arguments changed, restarting Idris")
      (idris-quit)
      (sit-for 0.01)) ; allows the sentinel to run and reset idris-process
    ;; Start Idris if necessary
    (when (not idris-process)
      (setq idris-process
            (get-buffer-process
             (apply #'make-comint-in-buffer
                    "idris"
                    (idris-buffer-name :process)
                    idris-interpreter-path
                    nil
                    "--ide-mode-socket"
                    command-line-flags)))
      (with-current-buffer (idris-buffer-name :process)
        (add-hook 'comint-preoutput-filter-functions
                  'idris-process-filter
                  nil
                  t)
        (add-hook 'comint-output-filter-functions
                  'idris-show-process-buffer
                  nil
                  t))
      (set-process-sentinel idris-process 'idris-sentinel)
      (setq idris-current-flags command-line-flags)
      (accept-process-output idris-process 3))))

(defun idris-connect (port)
  "Establish a connection with a Idris REPL."
  (when (not idris-connection)
    (setq idris-connection
          (open-network-stream "Idris IDE support" (idris-buffer-name :connection) "127.0.0.1" port))
    (add-hook 'idris-event-hooks 'idris-version-hook-function)
    (add-hook 'idris-event-hooks 'idris-log-hook-function)
    (add-hook 'idris-event-hooks 'idris-warning-event-hook-function)
    (add-hook 'idris-event-hooks 'idris-prover-event-hook-function)
    (set-process-filter idris-connection 'idris-output-filter)
    (set-process-sentinel idris-connection 'idris-sentinel)
    (set-process-query-on-exit-flag idris-connection t)
    (setq idris-process-current-working-directory "")
    (run-hooks 'idris-run-hook)
    (message "Connected. %s" (idris-random-words-of-encouragement))))

(defun idris-sentinel (_process msg)
  (message "Idris disconnected: %s" (substring msg 0 -1))
  (when idris-connection
    (delete-process idris-connection)
    (setq idris-connection nil))
  (when idris-process
    (delete-process idris-process)
    (setq idris-process nil)))

(defvar idris-process-port-output-regexp (rx (? (group (+ any (not num)))) (group (+ (any num))))
  "Regexp used to match the port of an Idris process.")

(defun idris-process-filter (string)
  "Accept output from the process"
  (if idris-connection
      string
    ;; Idris sometimes prints a warning prior to the port number, which causes
    ;; `string-match' to return 0
    (cl-flet ((idris-warn (msg)
                          (unless (or (null msg) (string-blank-p msg))
                            (message "Warning from Idris: %s" msg))))
      (if (not (string-match idris-process-port-output-regexp string))
          (idris-warn string)
        (idris-warn (match-string 1 string))
        (idris-connect (string-to-number (match-string 2 string))))
      "")))

(defun idris-show-process-buffer (string)
  "Show the Idris process buffer if STRING is non-empty."
  (when (> (length string) 0)
    (pop-to-buffer (get-buffer (idris-buffer-name :process)))))

(defun idris-output-filter (process string)
  "Accept output from the socket and process all complete messages"
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (idris-connection-available-input process))

(defun idris-connection-available-input (process)
  "Process all complete messages which arrived from Idris."
  (with-current-buffer (process-buffer process)
    (while (idris-have-input-p)
      (let ((event (idris-receive)))
        (idris-event-log event nil)
        (unwind-protect
            (save-current-buffer
              (idris-dispatch-event event process)))))))

(defun idris-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (idris-decode-length))))

(defun idris-receive ()
  "Read a message from the idris process"
  (goto-char (point-min))
  (let* ((length (idris-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (current-buffer)))
      (delete-region (point-min) end))))

(defun idris-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun idris-send (sexp proc)
  "Send a SEXP to Idris over the PROC. This is the lowest level of communication."
  (let* ((msg (concat (idris-prin1-to-string sexp) "\n"))
         (string (concat (idris-encode-length (length msg)) msg)))
    (idris-event-log sexp t)
    (process-send-string proc string)))

(defun idris-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun idris-prin1-to-string (sexp)
  "Like `prin1-to-string', but don't octal-escape non-ascii characters."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(defvar idris-rex-continuations '()
  "List of (ID FUNCTION [FUNCTION]) continuations waiting for RPC
  results. The first function will be called with a final result,
  and the second (if present) will be called with intermediate
  output results.")

(defvar idris-continuation-counter 1
  "Continuation serial number counter.")

(defvar idris-event-hooks)

(defun idris-dispatch-event (event process)
  (or (run-hook-with-args-until-success 'idris-event-hooks event)
      (destructure-case event
        ((:emacs-rex form continuation &optional output-continuation)
         (let ((id (cl-incf idris-continuation-counter)))
           (idris-send `(,form ,id) process)
           (push (if output-continuation
                     (list id continuation output-continuation)
                   (list id continuation))
                 idris-rex-continuations)))
        ((:output value id)
         (let ((rec (assq id idris-rex-continuations)))
           ;; Commands that don't ask for :output don't get it
           (when (and rec (nth 2 rec))
             (funcall (nth 2 rec) value))))
        ((:return value id)
         (let ((rec (assq id idris-rex-continuations)))
           (cond (rec (setf idris-rex-continuations
                            (remove rec idris-rex-continuations))
                      (funcall (cadr rec) value))
                 (t (error "Unexpected reply: %S %S" id value))))))))

(cl-defmacro idris-rex ((&rest saved-vars) sexp intermediate &rest continuations)
  "(idris-rex (VAR ...) (SEXP) INTERMEDIATE CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Idris.

If INTERMEDIATE is non-nil, also register for intermediate results.

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:error CONDITION).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (declare (indent 3))
  (let ((result (cl-gensym)))
    `(let ,(cl-loop for var in saved-vars
                    collect (cl-etypecase var
                              (symbol (list var var))
                              (cons var)))
       (idris-dispatch-event
        (list :emacs-rex ,sexp
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations))
              ,@(when intermediate
                  `((lambda (,result)
                      (destructure-case ,result
                        ,@continuations)))))
        idris-connection))))

(defun idris-eval-async (sexp cont &optional failure-cont)
  "Evaluate EXPR on the superior Idris and call CONT with the result, or FAILURE-CONT in failure case."
  (idris-rex (cont (buffer (current-buffer)) failure-cont)
      sexp t
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:error condition &optional _spans)
     (when failure-cont
       (set-buffer buffer)
       (funcall failure-cont condition))
     (message "Evaluation returned an error: %s." condition))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar idris-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(autoload 'idris-list-compiler-notes "idris-warnings-tree.el")
(defun idris-eval (sexp &optional no-errors)
  "Evaluate EXPR on the inferior Idris and return the result,
ignoring intermediate output. If `NO-ERRORS' is non-nil, don't
trigger warning buffers and don't call `ERROR' if there was an
Idris error."
  (let* ((tag (cl-gensym (format "idris-result-%d-"
                                 (1+ idris-continuation-counter))))
	 (idris-stack-eval-tags (cons tag idris-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (idris-rex (tag sexp)
           sexp nil
         ((:ok value &optional spans)
          (if (member tag idris-stack-eval-tags)
              (throw tag (list #'identity (cons value spans)))
            (if no-errors
                nil
              (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                     tag sexp))))
         ((:error condition &optional _spans)
          (if no-errors
              (throw tag (list #'identity nil))
            (when (member 'warnings-tree idris-warnings-printing)
              (when (idris-list-compiler-notes)
                (pop-to-buffer (idris-buffer-name :notes))))
            (throw tag (list #'error "%s (synchronous Idris evaluation failed)" condition)))))
       (let ((debug-on-quit t)
             (inhibit-quit nil))
         (while t
           (when (eq (process-status idris-process) 'exit)
             (error "Idris process exited unexpectedly"))
           (accept-process-output idris-connection 0.1)))))))

(defvar idris-options-cache '()
  "An alist caching the Idris interpreter options, to
  allow consulting them when the Idris interpreter is busy.")

(defun idris-update-options-cache ()
  (idris-eval-async '(:get-options)
                    #'(lambda (opts) (setq idris-options-cache opts))))

(defun idris-get-options ()
  (idris-eval '(:get-options)))

(defun idris-get-option (opt)
  ;; First check the local cache
  (let ((local-val (assoc opt idris-options-cache)))
    (if local-val
        (equal (cadr local-val) :True)
      (let ((remote-val (assoc opt (car (idris-get-options)))))
        (if remote-val
            (equal (cadr remote-val) :True)
          (error "Unknown Idris option %s" opt))))))

(defun idris-set-option (opt b)
  (let ((bi (if b :True :False)))
    (idris-rex ((buffer (current-buffer)) opt b bi)
        `(:set-option ,opt ,bi) nil
      ((:ok _res)
       (set-buffer buffer)
       (let ((cache-elt (assoc opt idris-options-cache)))
         (if cache-elt
             (setf (cadr cache-elt) bi)
           (add-to-list 'idris-options-cache (list opt bi)))))
      ((:error condition &optional _spans)
       (message "Setting option %s to %s returned an error: %s." opt b condition)))))

(defun idris-get-idris-version ()
  "Ask the Idris compiler for its version information.
Returns a cons cell whose car is a list of version number
components and whose cdr is a list of prerelease identifiers, if
applicable. Returns nil if the version of Idris used doesn't
support asking for versions."
  (pcase (idris-eval :version t)
    (`((,version ,prerelease)) (cons version prerelease))
    (_ nil)))

(defun idris-get-idris-version-string ()
  "Ask the Idris compiler for its version information, and return the result as a user-friendly string.
Returns nil if the version of Idris used doesn't support asking
for versions."
  (let ((version (idris-get-idris-version)))
    (if (consp version) ; returns nil on older versions of Idris
        (let* ((version-number (car version))
               (version-prerelease (cdr version)))
          (concat (mapconcat #'number-to-string version-number ".")
                  (if version-prerelease
                      (concat "-" (mapconcat #'identity version-prerelease "-"))
                    "")))
      nil)))


(provide 'inferior-idris)
