;;; ess-mode.el -- Emacs Speaks Statistics root mode.  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 1994-2018 ESS Core Team
;; Maintainer: ESS-core <ESS-core@r-project.org>
;;
;; Keywords: languages
;;
;; This file is part of ESS
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/
;;
;;; Commentary:
;;
;;; Code:

(require 'ess)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'ess-inf)

;; Silence the byte compiler
(declare-function run-ess-r "ess-r-mode" (&optional start-args))
(declare-function S+ "ess-sp6-d" (&optional proc-name))
(declare-function SAS "ess-sas-d" ())

;; FIXME:This one should not be necessary
(declare-function ess-display-help-on-object "ess-help" (object &optional command))

;; ESS mode
;; Major mode definition

;;*;; Hooks

(defcustom ess-mode-hook nil
  "Hook for customizing ESS each time it is entered."
  :group 'ess-hooks
  :type 'hook)

(defvar ess-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap yank] #'ess-yank)
    (define-key map "\C-c\C-r"   #'ess-eval-region)
    (define-key map "\C-c\M-r"   #'ess-eval-region-and-go)
    (define-key map "\C-c\C-b"   #'ess-eval-buffer)
    (define-key map "\C-c\M-b"   #'ess-eval-buffer-and-go)
    (define-key map (kbd "C-c C-<up>")   #'ess-eval-buffer-from-beg-to-here)
    (define-key map (kbd "C-c C-<down>") #'ess-eval-buffer-from-here-to-end)
    (define-key map "\C-c\C-f"   #'ess-eval-function)
    (define-key map "\C-c\M-f"   #'ess-eval-function-and-go)
    (define-key map "\C-c\C-c"   #'ess-eval-region-or-function-or-paragraph-and-step)
    (define-key map "\C-c\C-p"   #'ess-eval-paragraph-and-step)
    (define-key map "\C-c\M-p"   #'ess-eval-paragraph-and-go)
    (define-key map "\C-\M-x"    #'ess-eval-region-or-function-or-paragraph)
    (define-key map "\C-c\C-n"   #'ess-eval-line-visibly-and-step)
    (define-key map "\C-c\C-j"   #'ess-eval-line)
    (define-key map [(control return)] #'ess-eval-region-or-line-visibly-and-step)
    (define-key map "\C-c\M-j"   #'ess-eval-line-and-go)
    ;; FIXME: The next three can only work in S/R - mode
    (define-key map "\C-\M-a"    #'ess-goto-beginning-of-function-or-para)
    (define-key map "\C-\M-e"    #'ess-goto-end-of-function-or-para)
    (define-key map "\C-xnd"     #'ess-narrow-to-defun-or-para)
    (define-key map "\C-xnf"     #'ess-narrow-to-defun-or-para)
    (define-key map "\C-c\C-z"   #'ess-switch-to-inferior-or-script-buffer)
    (define-key map "\C-c\C-l"   #'ess-load-file)
    ;;; Make an alias because C-c C-l is taken up by comint in inferiors
    (define-key map "\C-c\M-l"   #'ess-load-file)
    (define-key map "\C-c\C-v"   #'ess-display-help-on-object)
    (define-key map "\C-c\C-s"   #'ess-switch-process)
    (define-key map "\C-c\C-k"   #'ess-force-buffer-current)
    (define-key map "\C-c`"      #'ess-show-traceback)
    (define-key map [(control ?c) ?~] #'ess-show-call-stack)
    (define-key map "\C-\M-q"    #'ess-indent-exp)
    (define-key map "{"          #'skeleton-pair-insert-maybe)
    (define-key map "}"          #'skeleton-pair-insert-maybe)
    (define-key map "\C-\M-h"    #'ess-mark-function-or-para)
    (define-key map "\t"         #'ess-indent-or-complete)
    (define-key map "\C-c\C-q"   #'ess-quit)
    (define-key map "\M-\r"      #'ess-use-this-dir)
    (define-key map ","          #'ess-smart-comma)
    (define-key map "\C-c\C-d"   'ess-doc-map)
    (define-key map "\C-c\C-e"   'ess-extra-map)
    (define-key map "\C-c\C-t"   'ess-dev-map)
    map)
  "Keymap for `ess-mode'.")

;; Redefine `indent-new-comment-line' commands for Emacs < 26. Emacs
;; 27 binds M-j to `default-indent-new-line' which calls
;; `comment-line-break-function' if point is in a comment. We set this
;; function in the mode init.
(substitute-key-definition 'indent-new-comment-line
                           'ess-indent-new-comment-line
                           ess-mode-map global-map)

(defvar ess-extra-map
  (let (ess-extra-map)
    (define-prefix-command 'ess-extra-map)
    (define-key ess-extra-map "\C-d" #'ess-dump-object-into-edit-buffer)
    (define-key ess-extra-map "d"    #'ess-dump-object-into-edit-buffer)
    (define-key ess-extra-map "\C-e" #'ess-execute)
    (define-key ess-extra-map "e"    #'ess-execute)
    (define-key ess-extra-map "\C-i" #'ess-install-library)
    (define-key ess-extra-map "i"    #'ess-install-library)
    (define-key ess-extra-map "\C-l" #'ess-load-library)
    (define-key ess-extra-map "l"    #'ess-load-library)
    (define-key ess-extra-map "\C-r" #'inferior-ess-reload)
    (define-key ess-extra-map "r"    #'inferior-ess-reload)
    (define-key ess-extra-map "\C-s" #'ess-set-style)
    (define-key ess-extra-map "s"    #'ess-set-style)
    (define-key ess-extra-map "\C-t" #'ess-build-tags-for-directory)
    (define-key ess-extra-map "t"    #'ess-build-tags-for-directory)
    (define-key ess-extra-map "\C-w" #'ess-execute-screen-options)
    (define-key ess-extra-map "w"    #'ess-execute-screen-options)
    (define-key ess-extra-map "/"    #'ess-set-working-directory)
    ess-extra-map)
  "ESS extra map.")

(easy-menu-define
  ess-mode-menu ess-mode-map
  "Menu for use in `ess-mode'."
  '("ESS" ; ESS-mode
    ["Load file"                ess-load-file t]
    ["Eval region | func | para" ess-eval-region-or-function-or-paragraph t]
    ["Eval region | func | para & step" ess-eval-region-or-function-or-paragraph-and-step t]
    ["Eval region | line" ess-eval-region-or-line-visibly-and-step t]
    ["Enter expression" ess-execute                 t]
    ;; sub menus
    "------"
    ("Process"
     ["Goto end of process buffer"  ess-switch-to-end-of-ESS        t]
     ["Switch to process buffer"    ess-switch-to-inferior-or-script-buffer t]
     ["Switch process"   ess-switch-process              t]
     ;; ["Recreate R and S versions known to ESS" (ess-r-s-versions-creation+menu) t]
     ("Start Process"
      ["R"     R   :help "Start a new R process" :active t]
      ["S"     S   :help "Start a new S process" :active t]
      ["Sqpe" Sqpe ess-microsoft-p] ;; :help "Start a new Sqpe process" :active t
      ["S+6-exisiting" S+6-existing ess-microsoft-p] ;; :help "Access an existing S process" :active t
      ["SAS"   SAS-menu t] ;;  :help "Start a new SAS process" :active t
      ;; The following menu item "Other" is a place-holder that will
      ;; be replaced with the other versions of R and Sqpe that can be run.
      ;; See `ess-r-define-runners' and ess-site.el
      ("Other"
       ["No other R or Sqpe versions" nil nil])
      ["About"
       (ess-goto-info "Starting up") t]
      ;; :help "Read about starting a new ESS process" :active t]
      )
     ("Eval visibly "
      :filter ess--generate-eval-visibly-submenu)
     ["Quit process" ess-quit t]
     ["Reload process" inferior-ess-reload t])
    "------"
    ("ESS Eval"
     ["Eval region | func | para" ess-eval-region-or-function-or-paragraph t]
     ["Eval region | func | para & step" ess-eval-region-or-function-or-paragraph-and-step t]
     ["Eval region | line & step" ess-eval-region-or-line-visibly-and-step t]
     "-----"
     ["Eval buffer"     ess-eval-buffer                    t]
     ["Eval buffer till here" ess-eval-buffer-from-beg-to-here t]
     ["Eval buffer from here" ess-eval-buffer-from-here-to-end t]
     ["Eval region"     ess-eval-region                    t]
     ["Eval function"   ess-eval-function                  t]
     ["Eval line"       ess-eval-line                      t]
     ["Eval line & step" ess-eval-line-and-step            t]
     ["Eval paragraph"   ess-eval-paragraph                t]
     ["Eval paragraph & step" ess-eval-paragraph-and-step  t]
     ["About"           (ess-goto-info "Evaluating code")  t]
     )
    ("Eval and Go"
     ["Eval buffer"     ess-eval-buffer-and-go            t]
     ["Eval region"     ess-eval-region-and-go            t]
     ["Eval function"   ess-eval-function-and-go          t]
     ["Eval line"       ess-eval-line-and-go              t]
     ["Eval paragraph"  ess-eval-paragraph-and-go         t]
     ["About"           (ess-goto-info "Evaluating code") t]
     )
    ("Motion"
     ["Beginning of function or para"   ess-goto-beginning-of-function-or-para       t]
     ["End of function or para"         ess-goto-end-of-function-or-para             t]
     "-----"
     ["Backward list"           backward-list                   t]
     ["Forward list"            forward-list                    t]
     ["Next parenthesis"        down-list                       t]
     ["Enclosing parenthesis"   backward-up-list                t]
     ["Backward sexp"           backward-sexp                   t]
     ["Forward sexp"            forward-sexp                    t]
     ["About"                   (Info-goto-node "(Emacs)Lists") t]
     )
    ("ESS Edit"
     ["Edit new object"   ess-dump-object-into-edit-buffer      t]
     ["Complete Filename" comint-replace-by-expanded-filename   t]
     ["Complete File or Object"   ess-indent-or-complete        t]
     ["Kill sexp"         kill-sexp                             t]
     ["Mark function"     ess-mark-function-or-para             t]
     ["Indent expression" ess-indent-exp                        t]
     ["Indent line"       ess-indent-command                    t]
     ["Toggle Auto-Fill Mode" auto-fill-mode                    t]
     ["Undo"              undo                                  t]
     ["About"             (ess-goto-info "Edit buffer")         t]
     )
    "------"
    ("start-dev" :visible nil)
    ("end-dev" :visible nil)
    "------"
    ("Font Lock"
     :active ess-font-lock-keywords
     :filter ess--generate-font-lock-submenu)
    "------"
    ["Describe"         describe-mode                   t]
    ["About editing" (ess-goto-info "Editing")  t]
    ["Read ESS info" (ess-goto-info "") t]
    ["Send bug report"  ess-submit-bug-report           t]))

;;;###autoload
(define-derived-mode ess-mode prog-mode "ESS"
  "Major mode for editing ESS source.
Optional arg ALIST describes how to customize the editing mode.
Optional arg PROC-NAME is name of associated inferior process.

\\{ess-mode-map}

You can send text to the inferior ESS process from other buffers containing
ESS source.
    `ess-eval-region' sends the current region to the ESS process.
    `ess-eval-buffer' sends the current buffer to the ESS process.
    `ess-eval-function' sends the current function to the ESS process.
    `ess-eval-line' sends the current line to the ESS process.
    `ess-switch-to-ESS' switches the current buffer to the ESS process buffer.
    `ess-switch-to-end-of-ESS' switches the current buffer to the ESS process
        buffer and puts point at the end of it.

    `ess-eval-region-and-go', `ess-eval-buffer-and-go',
        `ess-eval-function-and-go', and `ess-eval-line-and-go' switch to the S
        process buffer after sending their text.

    `ess-load-file' sources a file of commands to the ESS process.

\\[ess-indent-command] indents for ESS code.
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
Comments are indented in a similar way to Emacs-lisp mode:
       `###'     beginning of line
       `##'      the same level of indentation as the code
       `#'       the same column on the right, or to the right of such a
                 column if that is not possible.(default value 40).
                 \\[indent-for-comment] command automatically inserts such a
                 `#' in the right place, or aligns such a comment if it is
                 already inserted.
\\[ess-indent-exp] command indents each line of the syntactic unit following point.

Variables controlling indentation style:
 `ess-indent-offset'
    Indentation of ESS statements within surrounding block.
    The surrounding block's indentation is the indentation of the line on
    which the open-brace appears.
 `ess-offset-block'
    Indentation of blocks opened with curly braces or anonymous parentheses.
 `ess-offset-arguments'
    Indentation of function arguments or bracket indices.
 `ess-offset-arguments-newline'
    Indentation of function arguments or bracket indices when the opening
    delimiter is immediately followed by a newline.
 `ess-offset-continued'
    Indentation style for continued statements.
 `ess-align-nested-calls'
    Functions whose nested calls should be aligned.
 `ess-align-arguments-in-calls'
    Calls in which arguments should be aligned.
 `ess-align-continuations-in-calls'
    Whether ignore indentation after an operator in calls
 `ess-align-blocks'
    Blocks that should always be aligned vertically.
 `ess-indent-from-lhs'
    Whether function calls given as argument should be indented from the
    parameter name.
 `ess-indent-from-chain-start'
    Whether to indent arguments from the first of several consecutive calls.
 `ess-indent-with-fancy-comments'
    Non-nil means distinguish between #, ##, and ### for indentation.

Furthermore, \\[ess-set-style] command enables you to set up predefined ess-mode
indentation style. See `ess-style-alist' for predefined styles."
  :group 'ess
  ;; TODO: get rid of these and rely on modes to set variables properly
  (when-let ((alist (buffer-local-value 'ess-local-customize-alist (current-buffer))))
    (ess-setq-vars-local alist))
  (when-let ((alist ess-mode-editing-alist))
    (ess-setq-vars-local alist))
  ;; Keep <tabs> out of the code.
  (setq-local indent-tabs-mode nil)
  (setq-local comment-line-break-function #'ess-newline-and-indent)
  (setq mode-line-process
        '(" ["
          (:eval (ess--get-mode-line-indicator))
          ess--local-mode-line-process-indicator
          "]"))
  (add-hook 'ess-idle-timer-functions 'ess-synchronize-dirs nil 'local))

(defun ess--get-mode-line-indicator ()
  "Get `ess--mode-line-process-indicator' from process buffer.
Internal function to be used for dynamic mode-line display in
`ess-mode'."
  (if ess-local-process-name
      (let* ((proc (get-process ess-local-process-name))
             (buff (when proc (process-buffer proc))))
        (if (and proc (buffer-live-p buff))
            (with-current-buffer buff (mapcar 'eval ess--mode-line-process-indicator))
          "none"))
    "none"))


;;; User commands in ess-mode

(defun ess-install-library (&optional update package)
  "Install PACKAGE for current dialect.
With UPDATE, update cached package list."
  (interactive "P")
  (ess-install-library--override update package))

(cl-defgeneric ess-install-library--override (update package)
  "See `ess-install-library' for UPDATE, PACKAGE."
  (when update (message "Don't know how to update for %s" ess-dialect))
  (error "Cannot install %s, not available for %s" package ess-dialect))


;; Motion / manipulation commands

(defun ess-goto-beginning-of-function-or-para ()
  "If inside a function go to the beginning of it.
Otherwise go to the beginning of paragraph."
  (interactive)
  (let ((start-pos (point))
        beg end)
    (beginning-of-defun)
    (setq beg (point))
    (end-of-defun)
    (setq end (point))
    (goto-char beg)
    (unless (and (< beg start-pos)
                 (> end start-pos))
      (let ((par-pos (save-excursion
                       (goto-char start-pos)
                       (forward-comment most-negative-fixnum)
                       (backward-paragraph)
                       (forward-comment most-positive-fixnum)
                       (point))))
        (if (< end par-pos)
            (goto-char par-pos)
          (goto-char beg))))))

(defun ess-goto-end-of-function-or-para ()
  "If inside a function go to end of it.
Otherwise go to the end of paragraph."
  (interactive)
  (let ((pos (point))
        beg end)
    (end-of-defun)
    (setq end (point))
    (beginning-of-defun)
    (setq beg (point))
    (goto-char end)
    (when (or (< beg pos)
              (> end pos))
      (let ((par-pos (save-excursion
                       (goto-char pos)
                       (forward-comment most-positive-fixnum)
                       (forward-paragraph)
                       (point))))
        (when (<= par-pos beg)
          (goto-char par-pos))))))

(defun ess-mark-function-or-para ()
  "Put mark at end of ESS function, point at beginning."
  (interactive)
  (ess-goto-beginning-of-function-or-para)
  (push-mark (point))
  (ess-goto-end-of-function-or-para)
  (exchange-point-and-mark))

(define-obsolete-function-alias 'ess-mark-function 'ess-mark-function-or-para "15.09")

(defun ess-narrow-to-defun-or-para ()
  "Make text outside current function invisible.
If text is already narrowed, this is removed before narrowing to the
current function."
  (interactive)
  (save-excursion
    (widen)
    (let* ((beg (progn (ess-goto-beginning-of-function-or-para)
                       (point)))
           (end (progn (ess-goto-end-of-function-or-para)
                       (point))))
      (narrow-to-region beg end))))

(define-obsolete-function-alias 'ess-narrow-to-defun 'ess-narrow-to-defun-or-para "15.09")

;; FIXME: Support soft breaks with `insert-and-inherit'. See
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hard-and-Soft-Newlines.html
(defun ess-newline-and-indent (&optional _soft)
  (ess-indent-new-comment-line))

(defun ess-indent-new-comment-line ()
  "Like `indent-new-comment-line' but accounts for roxygen comments."
  (interactive)
  (cond ((and (fboundp 'ess-roxy-indent-new-comment-line)
              (string= ess-dialect "R"))
         (ess-roxy-indent-new-comment-line))
        (t
         (indent-new-comment-line))))


;;; Formatting / indentation
(defvar-local ess--installed-style-vars nil
  "A cons of the form (STYLE . VARS).
VARS is a list of all style vars which were not set explicitly to
buffer local values by the user in mode hooks.")

(defun ess-set-style (&optional style _quiet)
  "Set up the `ess-mode' style variables from the `ess-style' variable.
If STYLE argument is given, use that instead. It makes the ESS
indentation style variables buffer local. QUIET is for backward
compatibility and is ignored.

In programs, when STYLE is nil, the `ess-style' is installed. In
this case, if `ess-style' is buffer local, all settings are
overwritten, otherwise only those settings which are not already
buffer local. For example, `ess-style' is buffer local when it is
set in .dir-locals and thus must have priority over the user
settings in the mode hook."
  (interactive
   (list (let ((styles (mapcar (lambda (x) (symbol-name (car x)))
                               ess-style-alist)))
           (intern (ess-completing-read
                    "Set ESS mode indentation style"
                    styles nil t nil nil ess-style)))))
  (let* ((keep-local (and (null style)
                          (not (local-variable-p 'ess-style))))
         (style (or style ess-style))
         (style-alist (or (cdr (assq style ess-style-alist))
                          (error "Bad ESS style: %s" style)))
         (vars (if keep-local
                   ;; Install, but Keep user's buffer-local settings.
                   (cl-loop for (var . _) in (cdr (assq 'DEFAULT ess-style-alist))
                            unless (local-variable-p var)
                            collect var)
                 (mapcar #'car style-alist))))
    (when (called-interactively-p 'any)
      (message "Set indentation style to %s" style))
    (mapc (lambda (var)
            (make-local-variable var)
            (set var (cdr (assq var style-alist))))
          vars)
    style))

(defun ess-indent-command (&optional whole-exp)
  "Indent current line as ESS code, or in some cases insert a tab character.
If `tab-always-indent' is non-nil, always indent current line.
Otherwise, indent the current line only if point is at the left
margin or in the line's indentation; otherwise insert a tab. If
given, WHOLE-EXP means indent rigidly all the lines of the
expression starting after point so that this line becomes
properly indented. The relative indentation among the lines of
the expression are preserved.

If in a roxygen block at the beginning of the line with
`ess-roxy-hide-show-p' non-nil, call `ess-roxy-toggle-hiding'
instead of indenting."
  (interactive "P")
  (cond ((and (fboundp 'ess-roxy-entry-p)
              (fboundp 'ess-roxy-toggle-hiding)
              (bolp)
              (ess-roxy-entry-p)
              ess-roxy-hide-show-p)
         (ess-roxy-toggle-hiding))
        (whole-exp
         ;; If arg, always indent this line as S
         ;; and shift remaining lines of expression the same amount.
         (let ((shift-amt (funcall indent-line-function))
               beg end)
           (save-excursion
             (if tab-always-indent
                 (beginning-of-line))
             (setq beg (point))
             (backward-up-list 1)
             (forward-list 1)
             (setq end (point))
             (goto-char beg)
             (forward-line 1)
             (setq beg (point)))
           (if (> end beg)
               (indent-code-rigidly beg end shift-amt))))
        ((and (not tab-always-indent)
              (save-excursion
                (skip-chars-backward " \t")
                (not (bolp))))
         (insert-tab))
        (t (funcall indent-line-function))))

(defun ess-indent-or-complete ()
  "When region is selected indent the region.
Otherwise, if `tab-always-indent' is 'complete, try to indent, if
code is already indented, complete instead. Also see
`ess-first-tab-never-complete'."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (let ((shift (let ((indent (current-indentation)))
                   (ess-indent-command)
                   (- (current-indentation) indent))))
      (when (and (or (equal tab-always-indent 'complete)
                     ess-tab-complete-in-script)
                 (numberp shift) ;; can be nil if tab-always-indent is nil
                 (equal shift 0)
                 (or (eq last-command 'ess-indent-or-complete)
                     (null ess-first-tab-never-complete)
                     (and (eq ess-first-tab-never-complete 'unless-eol)
                          (looking-at "\\s-*$"))
                     (and (eq ess-first-tab-never-complete 'symbol)
                          (not (looking-at "\\w\\|\\s_")))
                     (and (eq ess-first-tab-never-complete 'symbol-or-paren)
                          (not (looking-at "\\w\\|\\s_\\|\\s)")))
                     (and (eq ess-first-tab-never-complete 'symbol-or-paren-or-punct)
                          (not (looking-at "\\w\\|\\s_\\|\\s)\\|\\s.")))))
        (completion-at-point)))))

(defun ess-indent-exp ()
  "Indent each line of the ESS grouping following point."
  (interactive)
  (cond ((and (fboundp 'ess-r-indent-exp)
              (string= ess-dialect "R"))
         (ess-r-indent-exp))
        (t
         (save-excursion
           (let ((start (point))
                 (end (ignore-errors (forward-sexp 1) (point))))
             (when end
               (indent-region start end)))))))

(defun ess-indent-line ()
  "Indent current line as ESS code.
Return the amount the indentation changed by."
  (declare (obsolete 'indent-line-function "ESS 19.04"))
  (funcall indent-line-function))


;;; Dump Objects

(defun ess-dump-object-into-edit-buffer (object)
  "Edit an ESS OBJECT in its own buffer.
Without a prefix argument, this simply finds the file pointed to by
`ess-source-directory'.  If this file does not exist, or if a
prefix argument is given, a dump() command is sent to the ESS process to
generate the source buffer."
  (interactive
   (progn
     (ess-force-buffer-current "Process to dump from: ")
     (ess-read-object-name "Object to edit")))
  (let* ((dirname (file-name-as-directory
                   (if (stringp ess-source-directory)
                       ess-source-directory
                     (with-current-buffer (process-buffer (ess-get-process
                                                           ess-local-process-name))
                       (ess-setq-vars-local ess-local-customize-alist)
                       (apply ess-source-directory nil)))))
         (filename (concat dirname (convert-standard-filename (format ess-dump-filename-template object))))
         (old-buff (get-file-buffer filename)))
    ;; If the directory doesn't exist, offer to create it
    (unless (file-exists-p (directory-file-name dirname))
      (if (y-or-n-p (format "Directory %s does not exist. Create it? " dirname))
          (make-directory (directory-file-name dirname))
        (error "Directory %s does not exist" dirname)))
    ;; Three options:
    ;;  (1) Pop to an existing buffer containing the file in question
    ;;  (2) Find an existing file
    ;;  (3) Create a new file by issuing a dump() command to S
    ;; Force option (3) if there is a prefix arg
    (cond (current-prefix-arg
           (ess-dump-object object filename))
          (old-buff
           (pop-to-buffer old-buff))
          ((file-exists-p filename)
           (ess-find-dump-file-other-window filename)
           (message "Read %s" filename))
          (t (ess-dump-object object filename)))))

(defun ess-dump-object (object filename)
  "Dump the ESS object OBJECT into file FILENAME."
  (unless (file-writable-p filename)
    (error "Can't dump %s as %f is not writeable" object filename))
  (let ((dump-cmd (format inferior-ess-dump-command object filename)))
    ;; Make sure we start fresh
    (when (get-file-buffer filename)
      (kill-buffer (get-file-buffer filename)))
    (ess-command dump-cmd)
    (message "Dumped in %s" filename)
    (ess-find-dump-file-other-window filename)
    ;; PD, 1Apr97
    ;;This ensures that the object gets indented according to ess-mode,
    ;;not as the R/S deparser does it. At the same time, it gets rid
    ;;of the mess generated by sending TAB characters to the readline
    ;;functions in R when you eval-buffer-*.
    (indent-region (point-min-marker) (point-max-marker) nil)
    (set-buffer-modified-p nil) ; no need to safe just because of indenting
    ;; Don't make backups for temporary files; it only causes clutter.
    ;; The ESS object itself is a kind of backup, anyway.
    (unless ess-keep-dump-files
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil))
    ;; Don't get confirmation to delete dumped files when loading
    (when (eq ess-keep-dump-files 'check)
      (setq ess-keep-dump-files nil))
    ;; Delete the file if necessary
    (when ess-delete-dump-files
      (delete-file (buffer-file-name)))))

(defun ess-find-dump-file-other-window (filename)
  "Find ESS source file FILENAME in another window."
  (unless (file-readable-p filename)
    (ess-write-to-dribble-buffer
     (format "%s does not exist. Bad dump, starting fresh." filename)))
  ;; Generate a buffer with the dumped data
  (find-file-other-window filename)
  (auto-save-mode 1)            ; Auto save in this buffer
  (when (and ess-function-template
             (goto-char (point-max))
             (re-search-backward ess-dumped-missing-re nil t))
    (replace-match ess-function-template t t)
    (set-buffer-modified-p nil) ; Don't offer to save if killed now
    (goto-char (point-min))
    (ignore-errors
      ;; This may fail if there are no opens
      (down-list 1))))


;;; Runners

(defun ess-define-runner (name dialect &optional path)
  "Create a function NAME.
This function starts the inferior process with the specified
version. DIALECT can be \"R,\" \"S,\", \"SAS.\" If given, PATH
should be the absolute path to the program. It defaults to NAME."
  (let ((name name)
        (dialect dialect)
        (path path))
    (fset (intern name)
          (lambda (&optional start-args)
            "Start this process version in an inferior ESS buffer.
Function defined using `ess-define-runner'."
            (interactive "P")
            (cond ((string= dialect "R")
                   (let ((inferior-ess-r-program (or path name)))
                     (require 'ess-r-mode)
                     (run-ess-r start-args)))
                  ((string= dialect "S")
                   (let ((inferior-S+-program (or path name)))
                     (require 'ess-sp6-d)
                     (S+)))
                  ((string= dialect "SAS")
                   (let ((inferior-SAS-program (or path name)))
                     (require 'ess-sas-d)
                     (SAS))))))))



(provide 'ess-mode)

;;; ess-mode.el ends here
