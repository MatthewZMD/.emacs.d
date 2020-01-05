;;; lcr.el --- lightweight coroutines  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Jean-Philippe Bernardy.

;; Author: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; Maintainer: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; URL: https://github.com/jyp/lcr
;; Package-Version: 20180902.1919
;; Created: January 2018
;; Version: 1.1
;; Keywords: tools
;; Package-Requires: ((dash "2.12.0") (emacs "25.1"))

;; With parts copied from the work of Daniel Colascione
;; <dancol@dancol.org> on generators.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; We call a lightweight coroutine (or `lcr' for short) a function
;; which does not return its result directly, but instead passes it to
;; an extra *continuation* argument (often called 'continue' or
;; 'cont').  Codinig with explicit contination arguments is a
;; well-known technique, called continuation-passing style (CPS).
;;
;; CPS allows inversion of control to take place.  Indeed the
;; continuation may be installed as a callback, rather than being
;; called directly.  In general, any CPS function may yield control,
;; and thus implement a lightweight form of concurrency.
;;
;; This module
;; provides:
;;
;; - marcros which can translate direct-style code into cps
;; - a library of lcr's to read from processes, wait some time, etc.
;;
;; Why use this module, instead of Emacs' built-in concurrency support?
;;
;; - for better control over context switch and/or scheduling
;; - for versions of Emacs which do not provide concurrency


;;; Code:

(require 'dash)

(defun lcr-call (fun &rest args)
  "Call the coroutine FUN with arguments ARGS."
  (error "`lcr-call' used outside a co-routine (%S %S)" fun args))

(defconst lcr-inhibit-atomic-optimization nil)
(defvar lcr-yield-seen nil)
(defun lcr--atomic-p (form)
  "Return whether the given FORM never jumps to another coroutine."
  (and (not lcr-inhibit-atomic-optimization)
       (let* ((lcr-yield-seen nil))
         (ignore (macroexpand-all
                  `(cl-macrolet ((lcr-call (fun &rest args) (setf lcr-yield-seen t)))
                     ,form)
                  macroexpand-all-environment))
         (not lcr-yield-seen))))

(defmacro lcr-def (name arglist &rest body)
  "Define a lightweight coroutine (lcr) with NAME, ARGLIST and BODY.
The defined lcr is added an extra continuation argument and the
body is translated to continuation-passing style automatically.
Within an lcr, call another lcr using `lcr-call' (this will
forward the continuation as expected).  From the outside, use
`lcr-async-let' or call the lcr with an explicit continuation."
  (declare (doc-string 3) (indent 2))
  (let ((exp-body (macroexpand-all `(progn ,@body) macroexpand-all-environment)))
    ;; expand all macros so lcr--transform-1 only has to deal with basic constructs.
    `(progn
       (put (quote ,name) 'lcr? t)
       (defun ,name ,(-snoc  arglist 'lcr--continuation)
         ,(lcr--transform-1 exp-body (lambda (x) `(funcall lcr--continuation ,x)))))))

;; (defmacro lcr-async-bind (var expr &rest body)
;;   "Bind VAR in a continuation passed to EXPR with contents BODY.
;; EXPR is turned to a co-routine but BODY is executed in direct
;; style (but only after EXPR returns)."
;;   (declare (indent 2))
;;   (lcr--transform-1 expr `(lambda (,var) ,@body)))

;; (defmacro lcr-async-let (bindings &rest body)
;; "Expand multiple BINDINGS and call BODY as a continuation.
;; Example:
;;  (lcr-async-let ((x (lcr1 a b c))
;;                  (y (lcr2 a b c x)))
;;     (f x y))"
;;   (declare (indent 2))
;;   (pcase bindings
;;     (`((,vars ,expr)) `(progn (lcr-async-bind ,vars ,expr ,@body) (lcr-scheduler)))
;;     (`((,vars ,expr) . ,rest) `(lcr-async-bind ,vars ,expr (lcr-async-let ,rest ,@body)))))

(defmacro lcr-cps-bind (vars expr &rest body)
  "Bind VARS in a continuation passed to EXPR with contents BODY.
So (lcr-cps-bind x (fun arg) body) expands to (fun arg (λ (x) body))"
  (declare (indent 2))
  (if (listp vars)
      `(,@expr (lambda ,vars ,@body))
    `(,@expr (lambda (,vars) ,@body))))

(defmacro lcr-cps-let (bindings &rest body)
"Expand multiple BINDINGS and call BODY as a continuation.
Example: (lcr-cps-let ((x (fun1 arg1)) (y z (fun2 arg2))) body)
expands to: (fun1 arg1 (λ (x) (fun2 arg2 (λ (y z) body))))."
  (declare (indent 1))
  (pcase bindings
    (`((,vars ,expr)) `(progn (lcr-cps-bind ,vars ,expr ,@body) (lcr-scheduler)))
    (`((,vars ,expr) . ,rest) `(lcr-cps-bind ,vars ,expr (lcr-cps-let ,rest ,@body)))))

(defun lcr--transform-body (forms k)
  "Transform FORMS and pass the result of the last form to K."
  (lcr--transform-1 `(progn ,@forms) k))

(defun lcr--transform-n (forms k)
  "Transform FORMS and pass all the results, as a list, to K."
  (pcase forms
    (`() (funcall k ()))
    (`(,form . ,rest)
     (lcr--transform-1
      form
      (lambda (x) (lcr--transform-n rest (lambda (xs) (funcall k (cons x xs)))))))))

;; see cps--transform-1 in generators for all possible forms.
(defun lcr--transform-1 (form k)
  "Transform FORM and pass the result to K."
  ;; (message "Transforming: %S" form)
  (pcase form
    ;; must leave atoms unchanged (e.g. so that variables can be assigned)
    ((guard (atom form)) (funcall k form))
    ;; ((guard (lcr--atomic-p form))
    ;;  (funcall k form)) ;; won't work because k is not guaranteed to eval its argument.

    ((guard (lcr--atomic-p form))
     (let ((var (cl-gensym "atom")))
       `(let ((,var ,form)) ,(funcall k var))))

    ;; Process `and'.
    (`(and) (funcall k 't))
    (`(and ,condition)
      (lcr--transform-1 condition k))
    (`(and ,condition . ,rest)
      (lcr--transform-1
       condition
       (lambda (x)
         `(if ,x ,(lcr--transform-1 `(and ,@rest) k)
            ,(funcall k 'nil)))))
    ;; Process `cond'.
    (`(cond)
      (lcr--transform-1 nil k))
    (`(cond (,condition) . ,rest)
     (lcr--transform-1 `(or ,condition (cond ,@rest)) k))
    (`(cond (,condition . ,body) . ,rest)
     (lcr--transform-1 `(if ,condition
                            (progn ,@body)
                          (cond ,@rest))
                       k))
    (`(cond (,condition) . ,rest)
      (lcr--transform-1 `(or ,condition (cond ,@rest))
                        next-state))
    (`(cond (,condition . ,body) . ,rest)
      (lcr--transform-1 `(if ,condition
                             (progn ,@body)
                           (cond ,@rest))
                        next-state))

    ;; Process `if'.

    (`(if ,cond ,then . ,else)
      (lcr--transform-1 cond
                        (lambda (c)
                          `(if ,c
                               ,(lcr--transform-1 then k)
                             ,(lcr--transform-1 `(progn ,@else) k)))))

    ;; Process `progn' and `inline': they are identical except for the
    ;; name, which has some significance to the byte compiler.

    (`(inline) (lcr--transform-1 nil k))
    (`(inline ,form) (lcr--transform-1 form k))
    (`(inline ,form . ,rest)
     (lcr--transform-1
      form
      (lambda (_) (lcr--transform-1 `(inline ,@rest)
                                    k))))

    (`(progn) (lcr--transform-1 nil k))
    (`(progn ,form) (lcr--transform-1 form k))
    (`(progn ,form . ,rest)
     (lcr--transform-1
      form
      (lambda (_) (lcr--transform-1 `(progn ,@rest)
                                    k))))

    ;; Process `let'.
    (`(let () . ,body)
      (lcr--transform-1 `(progn ,@body) k))
    (`(let* () . ,body)
      (lcr--transform-1 `(progn ,@body) k))
    ((and `(let ,vars . ,body) (guard (-all? 'atom vars)))
     `(let ,@vars ,(lcr--transform-1 `(progn ,@body) k)))
    ((and `(let* ,vars . ,body) (guard (-all? 'atom vars)))
     `(let ,@vars ,(lcr--transform-1 `(progn ,@body) k)))
    (`(let ,bindings . ,body)
     (lcr--transform-n
      (-map #'cadr bindings)
      (lambda (xs) `(let ,(-zip-with 'list (-map #'car bindings) xs)
                      ,(lcr--transform-1 `(progn ,@body) k)))))
    (`(let* ((,var ,value-form) . ,more-bindings) . ,body)
        (lcr--transform-1
         value-form
         (lambda (x) `(let* ((,var ,x)) ,(lcr--transform-1 `(let* ,more-bindings ,@body) k)))))
    ;; Process `or'.
    (`(or) (lcr--transform-1 'nil k))
    (`(or ,condition) (lcr--transform-1 condition k))
    (`(or ,condition . ,rest)
      (lcr--transform-1 condition
                        (lambda (x)
                          `(if ,x
                              ,(funcall k x)
                            ,(lcr--transform-1
                              `(or ,@rest) k)))))
    ;; Process `prog1'.
    (`(prog1 ,first) (lcr--transform-1 first k))
    (`(prog1 ,first . ,body)
      (lcr--transform-1
       first (lambda (x) (lcr--transform-1 `(progn ,@body) (lambda (_) (funcall k x))))))
    ;; Process `prog2'.
    (`(prog2 ,form1 ,form2 . ,body)
      (lcr--transform-1 `(progn ,form1 (prog1 ,form2 ,@body)) k))
    ;; Process `while'.
    (`(while ,test . ,body)
     (let ((while-fun (cl-gensym "while")))
       ;; [while c body]k --> (flet (loop () [c]λx.(if x ([body]λy. (loop) (k nil))) )
       `(let (,while-fun)
          (setq ,while-fun (lambda ()
                             ,(lcr--transform-1
                               test
                               (lambda (x) `(if ,x
                                                ,(lcr--transform-1 `(progn ,@body) (lambda (_) `(lcr-yield ,while-fun)))
                                              ,(funcall k 'nil))))))
          (funcall ,while-fun))))
    ;; Process various kinds of `quote'.
    (`(quote ,arg) (funcall k `(quote ,arg)))
    (`(function ,arg) (funcall k `(function ,arg)))

    ;; Some not-really-special-but-special forms
    (`(save-current-buffer . ,body)
     ;; we should do this because the buffer should be restored in the
     ;; continuation, not right now.
     (lcr--transform-1 `(let ((buf (current-buffer))) (prog1 (progn ,@body) (set-buffer buf))) k))

    ;; Process function calls
    (`(lcr-call . (,fun . ,args))
     (let ((var (cl-gensym "v")))
       (lcr--transform-n args (lambda (xs)
                                `(,fun ,@xs (lambda (,var) ,(funcall k var)))))))
    (`(,fun . ,args )
     (let ((var (cl-gensym "v")))
       (lcr--transform-n args (lambda (xs)
                                `(let ((,var (,fun ,@xs))) ,(funcall k var))))))
    (`(form)
     (error "Special form %S incorrect or not supported" form))))

;; Tests:
;; (lcr--transform-1 '(lcr-call f x y) (lambda (x) x))
;; (lcr--transform-1 '(lcr-call f (g x y) z) (lambda (x) x))
;; (lcr--transform-1 '(lcr-call f (lcr-call g x y) z) (lambda (x) x))
;; (lcr--transform-1 '(while c a) (lambda (x) x))
;; (lcr--transform-1 '(quote quote) (lambda (x) x))
;; (lcr--transform-1 '(f x y) (lambda (x) x))
;; (lcr--transform-1 ''example (lambda (x) x))
;; (lcr--transform-1 '(and a b) (lambda (x) x))
;; (lcr--transform-1 '(progn (if a b c) d) (lambda (x) x))
;; (lcr--transform-1 '(progn a b c d) (lambda (x) x))
;; (lcr--transform-1 '(if a b c d) (lambda (x) x))
;; (lcr--transform-1 '(if a (and e f) c d) (lambda (x) x))
;; (lcr--transform-1 '(let () aowfutn) (lambda (x) x))
;; (lcr--transform-1 '(let ((x yop)) (and a b)) (lambda (x) x))
;; (lcr--transform-1 '(let* ((x yop)) (and a b)) (lambda (x) x))
;; (lcr--transform-1 '(or) (lambda (x) x))
;; (lcr--transform-1 '(or a) (lambda (x) x))
;; (lcr--transform-1 '(or a b) (lambda (x) x))
;; (lcr--transform-1 '(prog1 a b c) (lambda (x) x))
;; (lcr--transform-1 '(prog2 a b c) (lambda (x) x))
;; (lcr--transform-n '(x y z) (lambda (xs) xs))
;; (lcr--transform-n '() (lambda (xs) xs))

(defun lcr--context ()
  "Make a copy of the resonably restorable context.
This is useful for coming back to such a context after control
comes back."
  (point-marker))

(defmacro lcr--with-context (ctx &rest body)
  "Temporarily switch to CTX (if possible) and run BODY."
  (declare (indent 2))
  `(save-current-buffer
     (when (marker-buffer ,ctx) (set-buffer (marker-buffer ,ctx)))
     (save-excursion
       (goto-char ,ctx)
       ,@body)))

(defvar lcr-context-switch-hook nil
"Hook to run when a context switch (lightweight yield) occurs.")

(defun lcr-refresh-modelines ()
  "Update all modelines."
  (force-mode-line-update t))

(defvar lcr-ready-in (list) "List of ready processes, inbound portion of queue.")
(defvar lcr-ready-out (list) "List of ready processes, outbound portion of queue.")

(defun lcr-yield (cont)
  "Enqueue the ready process CONT."
  (push cont lcr-ready-in))

(defun lcr-scheduler ()
  "This is the main loop of the lcr 'OS'.
This is a simple FIFO scheduler.  The ready queue is polled for
processes (continuations) to run until it gets empty.  If no
process is ready to run the control is yielded back to the Emacs
main loop."
  (while (or lcr-ready-out lcr-ready-in)
    (when (not lcr-ready-out)
      (setq lcr-ready-out (nreverse lcr-ready-in))
      (setq lcr-ready-in nil))
    (let ((next-process (pop lcr-ready-out)))
      ;; (message "lcr-scheduler running... %s ready" (+ (length lcr-ready-in) (length lcr-ready-out)))
      (funcall next-process))))

(defmacro lcr-context-switch (&rest body)
  "Save the current context, to restore it in a continuation.
The current continuation is passed as CONT and can be called
within a BODY by using the macro `lcr-resume'.  The operations
performed here correspond to a context-switch in operating-system
parlance.  After BODY is run `lcr-scheduler' is called."
  (declare (indent 2))
  `(let ((ctx (lcr--context)))
     (cl-macrolet ((lcr-resume (cont &rest args)
                                 `(lcr--with-context
                                   ctx
                                   (funcall ,cont ,@args))))
       (progn ,@body
              (run-hooks 'lcr-context-switch-hook)
              (lcr-scheduler)))))

(defvar-local lcr-process-callback nil "Callback used by `lcr-process-read'.")

(defun lcr-process-initialize (buffer)
  "Initialize a proccess BUFFER for communication.
After initialization, you can use `lcr-process-read' to read the
process' output.  This function overwrites the `process-filter'."
  (set-process-filter
   (get-buffer-process buffer)
   (lambda (_process string)
     (let ((cb (buffer-local-value 'lcr-process-callback buffer)))
       (if cb (funcall cb string)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (goto-char (point-max))
             (insert string))))))))


(defun lcr-process-read (buffer continue)
  "Asynchronously read from the process associated with BUFFER and CONTINUE.
The amount of data read is unknown, so this function should most
certainly be called within a loop.  Note that if the process
outputs text and `lcr-process-read' is not waiting for output,
the data is simply appended to the process' buffer.  This
function is a lightweight coroutine, see `lcr'."
  (if (buffer-local-value 'lcr-process-callback buffer)
      (funcall continue "lcr-process-read: try to read from (%s), but another coroutine is reading from it already.")
    (lcr-context-switch
        (lcr-set-local 'lcr-process-callback
                       (lambda (input)
                         (lcr-set-local 'lcr-process-callback nil buffer)
                         (lcr-resume continue input))
                       buffer))))

(defun lcr-set-local (var val buffer)
  "Set variable VAR to value VAL in BUFFER."
  (with-current-buffer buffer (set (make-local-variable var) val)))

(defun lcr-wait (secs continue)
  "Wait SECS then CONTINUE.
This function is a lightweight coroutine, see `lcr'."
  (lcr-context-switch
      (run-with-timer secs 'nil
                      (lambda ()
                        (lcr-resume continue ())))))

;; (defun lcr-sema-new ()
;;   "Create a new semaphore structure."
;;   (list t nil))

;; (defalias 'lcr-sema-free? 'car "Is the semaphore free?")
;; (defalias 'lcr-sema-queue 'cadr "The semaphore's queue.")

;; (defun lcr-sema-get (sema continue)
;;   "Get (exclusive) access to the semaphore SEMA, then CONTINUE.
;; This function is a lightweight coroutine."
;;   (if (lcr-sema-free? sema)
;;       (setf (lcr-sema-free? sema) nil)
;;     (lcr-context-switch
;;         (push (lambda () (lcr-resume continue ()))
;;               (lcr-sema-queue)))))

;; (defun lcr-sema-put (sema)
;; "Release the semaphore SEMA.
;; This will run *synchronously* the next process waiting for it."
;;   (let ((next-process (pop (lcr-sema-queue sema))))
;;     (if next-process (funcall next-process ())
;;       (setf (lcr-sema-free? sema) t))))

(defun lcr-blocking-call (cont)
  "Call CONT as (CONT K) and block until (K res) is called, then return res."
  (let ((result nil))
    (funcall cont (lambda (reply) (setq result (list reply))))
    ;; use a list so that even 'nil' will be detected as a result.
    (while (not result) (sleep-for 0.01))
    (car result)))

(provide 'lcr)
;;; lcr.el ends here
