;;; ess-mouse.el --- Support for mouse- or cursor-sensitive actions

;; Copyright (C) 2001 Richard M. Heiberger <rmh@temple.edu>
;; Copyright (C) 2002--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Richard M. Heiberger <rmh@temple.edu>
;; Created: 25 Mar 2001
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

;; Support for mouse- or cursor-sensitive actions.  This is based on
;; and uses mouseme.el.  mouseme.el only does mouse sensititivity.
;; The new functions ess-mouse-me and ess-mouse-me-helper do similar
;; things based on the cursor, not the mouse, and can be bound to a
;; keystroke.

;;; Code:

 ; Requires and autoloads

;;*;; Requires
(require 'mouseme)
(require 'ess-trns)
;;(if (or (equal window-system 'w32)
;;      (equal window-system 'win32)
;;      (equal window-system 'mswindows))
;;    (require 'essiw32b))

(defun ess-mouse-me ()
  "Popup a menu of functions to run on selected string or region."
  (interactive)
  (ess-mouse-me-helper
   (lambda (name)
     (or (x-popup-menu (list '(0 0)
                             (get-buffer-window (get-buffer (buffer-name))))
                       (funcall mouse-me-build-menu-function name))
         (error "No command to run")))))



(defun ess-mouse-me-helper (func)
  "Determine the string to use to process EVENT and call FUNC to get cmd."
  (let (name sp sm mouse beg end cmd mmtype)
    ;; temporarily goto where the event occurred, get the name clicked
    ;; on and enough info to figure out what to do with it
    (save-match-data
      (save-excursion
        (setq sp (point))               ; saved point
        (setq sm (mark t))              ; saved mark
;;;     (set-buffer (window-buffer (posn-window (event-start event))))
;;;     (setq mouse (goto-char (posn-point (event-start event))))
        (setq mouse (point))  ;; ess-mouse-me-helper
        ;; if there is a region and point is inside it
        ;; check for sm first incase (null (mark t))
        ;; set name to either the thing they clicked on or region
        (if (and sm
                 (or (and transient-mark-mode mark-active)
                     (eq last-command 'mouse-drag-region))
                 (>= mouse (setq beg (min sp sm)))
                 (<= mouse (setq end (max sp sm))))
            (setq name (buffer-substring beg end))
          (setq name (funcall mouse-me-get-string-function))
          (if (listp name)
              (setq beg (nth 1 name)
                    end (nth 2 name)
                    name (car name))
            (goto-char mouse)
            (while (not (looking-at (regexp-quote name)))
              (backward-char 1))
            (setq beg (point))
            (setq end (search-forward name))))))
    ;; check if name is null, meaning they clicked on no word
    (if (or (null name)
            (and (stringp name) (string= name "" )))
        (error "No string to pass to function"))
    ;; popup a menu to get a command to run
    (setq cmd (funcall func name))
    ;; run the command, eval'ing if it was a list
    (if (listp cmd)
        (setq cmd (eval cmd)))
    (setq mmtype (get cmd 'mouse-me-type))
    (cond ((eq mmtype 'region)
           (funcall cmd beg end))
          ((eq mmtype 'string)
           (funcall cmd name))
          (t
           (funcall cmd name)))))

(defcustom ess-S-mouse-me-menu-commands-alist
  '("S-Plus 4 and 6 GUI under Windows"
    ("Edit.data"   . ess-mouse-me-Edit.data)
    "----"
    ("print"       . ess-mouse-me-print)
    ("summary"     . ess-mouse-me-summary)
    ("plot"        . ess-mouse-me-plot)
    ("show"        . ess-mouse-me-show)
    ("help"        . ess-display-help-on-object)
    ("args"        . ess-mouse-me-args)
    "----"
    ("Browser on"  . ess-mouse-me-browser-on)
    ("Browser off" . ess-mouse-me-browser-off))
  "Command menu used by `mouse-me-build-menu'.
A alist of elements where each element is either a cons cell or a string.
If a cons cell the car is a string to be displayed in the menu and the
cdr is either a function to call passing a string to, or a list which evals
to a function to call passing a string to.  If the element is a string
it makes a non-selectable element in the menu.  To make a separator line
use a string consisting solely of hyphens.

The function returned from this menu will be called with one string
argument.  Or if the function has the symbol property `mouse-me-type'
and if its value is the symbol `region' it will be called with the
beginning and ending points of the selected string.  If the value is
the symbol `string' it will be called with one string argument."
  :type '(repeat sexp)
  :group 'mouseme)


(defun ess-mouse-me-Edit.data (string)
  (ess-mouse-me-eval-expanded string "Edit.data(" ")" nil nil nil))

(defun ess-mouse-me-print (string)
  (ess-mouse-me-eval-expanded string "" "" nil nil t))
(defun ess-mouse-me-summary (string)
  (ess-mouse-me-eval-expanded string "summary(" ")" nil nil t))
(defun ess-mouse-me-plot (string)
  (ess-mouse-me-eval-expanded string "plot(" ")") nil nil nil)
(defun ess-mouse-me-show (string)
  (ess-mouse-me-eval-expanded string "show(" ")") nil nil nil)
(defun ess-mouse-me-args (string)
  (ess-mouse-me-eval-expanded string "args(" ")" nil nil t))

(defun ess-mouse-me-browser-on (string)
  (if (equal (substring ess-dialect 0 1) "R")
      (ess-eval-linewise (concat "debug(" string ")"))
    (ess-mouse-me-eval-expanded string "trace(" ", exit=browser)") nil nil nil))

(defun ess-mouse-me-browser-off  (string)
  (if (equal (substring ess-dialect 0 1) "R")
      (ess-eval-linewise (concat "undebug(" string ")"))
    (ess-mouse-me-eval-expanded string "untrace(" ")") nil nil nil))



(defun ess-mouse-me-eval-expanded (string &optional head tail commands-buffer
                                          page value-returned)
  "Send the expanded STRING to the inferior-ess process using `ess-command'
after first concating the HEAD and TAIL.  Put answer in COMMANDS-BUFFER if
specified, otherwise in \"tmp-buffer\".  In either
case the buffer containing the answer is renamed to the value of the
constructed command.  If PAGE is non-nil, expand
the string one more time by embedding it in a \"page()\" command."
  (interactive)
  (let* (scommand
         page-scommand
         (lproc-name ess-local-process-name)
         (ess-mouse-customize-alist ess-local-customize-alist))
    (if (not head) (setq head "summary("))
    (if (not tail) (setq tail ")"))
    (if (not commands-buffer) (setq commands-buffer
                                    (get-buffer-create "tmp-buffer")))
    (setq scommand (concat head string tail))

    (ess-make-buffer-current)
    (pop-to-buffer-same-window commands-buffer)
    (ess-setq-vars-local (eval ess-mouse-customize-alist) (current-buffer))
    (setq ess-local-process-name lproc-name)
    (ess-command (concat scommand "\n") commands-buffer)
    (if (not value-returned) (pop-to-buffer-same-window (nth 1 (buffer-list))))
    (if (not value-returned)
        nil
      (if ess-microsoft-p                      ;; there ought to be a filter
          (while (search-forward "\r" nil t)   ;; function to keep the ^M
            (replace-match "" nil t)))         ;; from showing up at all
      (ess-transcript-mode)
      (setq ess-local-process-name lproc-name)
      (rename-buffer scommand))))


 ; Provide package

(provide 'ess-mouse)



;;;;;;;; STARTUP STUFF ;;;;;;;;;;;;

(make-variable-buffer-local 'mouse-me-menu-commands)

(defun ess-S-mouse-me-menu-commands ()
  (if (equal ess-language "S")
      (setq mouse-me-menu-commands ess-S-mouse-me-menu-commands-alist)))

;;       (define-key ess-mode-map              [S-mouse-3] 'ess-mouse-me)
;;       (define-key inferior-ess-mode-map     [S-mouse-3] 'ess-mouse-me)
;;       (defun ess-S-mouse-me-ess-transcript-mode ()
;;         (define-key ess-transcript-mode-map [S-mouse-3] 'ess-mouse-me))
;;
(add-hook 'ess-mode-hook            'ess-S-mouse-me-menu-commands)
(add-hook 'inferior-ess-mode-hook   'ess-S-mouse-me-menu-commands)
(add-hook 'ess-transcript-mode-hook 'ess-S-mouse-me-menu-commands)
;; (add-hook 'ess-transcript-mode-hook 'ess-S-mouse-me-ess-transcript-mode)


;;; ess-mouse.el ends here
