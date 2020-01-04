;;; ess-help.el --- Support for viewing ESS help files  -*- lexical-binding: t; -*-

;; Copyright (C) 1989-1994, 2017 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997, A.J. Rossini <rossini@stat.sc.edu>
;; Copyright (C) 1998--2001 A.J. Rossini, Martin Maechler, Kurt Hornik and
;;      Richard M. Heiberger <rmh@temple.edu>.
;; Copyright (C) 2001--2010 A.J. Rossini, Richard M. Heiberger, Martin
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

;; Code for dealing with ESS help files.  See README.<LANGUAGE> where
;; <LANGUAGE> is one of `S', `SAS', `Stata' or `XLispStat'.

;;; Code:

 ; Requires and autoloads
(require 'cl-lib)
(eval-when-compile
  (require 'tramp))
(require 'info)
(require 'ess-mode)
(require 'ess-inf)
(require 'ess-utils)
(require 'ansi-color)

(declare-function ess-r-help-mode "ess-r-mode")
(declare-function ess-stata-help-mode "ess-stata-lang")


(defcustom ess-help-mode-hook nil
  "Functions to call when entering `ess-help-mode'."
  :group 'ess-hooks
  :type 'hook)

(defvar ess--help-frame nil
  "Stores the frame used for displaying R help buffers.")

 ; ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; In this section:
;;;;
;;;; * The function ess-display-help-on-object
;;;; * The major mode ess-help-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defgeneric ess--help-major-mode ()
  "Determine which help major mode to call, and call it.
Uses `ess-dialect' to determine the appropriate help mode."
  (ess-help-mode))

(defun ess--help-get-bogus-buffer-substring (buffer &optional nr-first)
  "Return non-nil if BUFFER looks like a bogus ESS help buffer.
Return the pair (match-beg. match-end) which can be used in error
message. NR-FIRST is the number of characters at the start of the
buffer to examine when deciding if the buffer if bogus. If nil,
the first 150 characters of the buffer are searched."
  (if (not nr-first) (setq nr-first 150))
  (with-current-buffer buffer
    (let ((PM (point-min))
          (case-fold-search t)
          searching res)
      (setq res
            (or  ;; evaluate up to first non-nil (or end):
             (< (- (point-max) PM) 80); buffer less than 80 chars
             (not (setq searching t))
             ;; todo: move to customize-alist
             (progn (goto-char PM) ;; R:
                    (re-search-forward "Error in help"    nr-first t))
             (progn (goto-char PM) ;; S-plus 5.1 :
                    (re-search-forward "^cat: .*--"       nr-first t))
             (progn (goto-char PM) ;; S version 3 ; R :
                    (re-search-forward "no documentation .+" nr-first t))
             (progn (goto-char PM) ;; stata
                    (re-search-forward "^help .*not found" nr-first t))))
      (ess-write-to-dribble-buffer
       (format " |--> %s [searching %s]\n" res searching))
      (when res
        (if searching
            (buffer-substring (match-beginning 0) (match-end 0))
          (buffer-string))))))

(defun ess-help-get-local-help-buffers ()
  (ess-force-buffer-current)
  (cl-remove-if-not
   (lambda (buffer)
     (let* ((pattern (concat "*help[" ess-current-process-name "]("))
            (name (buffer-name buffer))
            (candidate (when (> (length name) (length pattern))
                         (substring name 0 (length pattern))) ))
       (when (string= pattern candidate)
         buffer)))
   (buffer-list)))

(defvar-local ess-help-type nil
  "Type of help file, help, index, vignettes etc.
Local in `ess-help' buffers.")

(defvar-local ess-help-object nil
  "Name of the object the help is displayed for.
Is name of the package for package index.
Local in `ess-help' buffers.")
(put 'ess-help-object 'permanent-local t)

(defun ess-display-help-on-object (object &optional command)
  "Display documentation for OBJECT.
If prefix ARG is given, force an update of the cached help topics
and query the ESS process for the help file instead of reusing an
existing buffer if it exists. Uses the variable
`inferior-ess-help-command' for the actual help command. Prompts
for the object name based on the cursor location for all cases
except the S-Plus GUI. With S-Plus on Windows (both GUI and in an
inferior Emacs buffer) the GUI help window is used. If COMMAND is
supplied, it is used instead of `inferior-ess-help-command'."
  (interactive
   (progn
     (ess-force-buffer-current)
     (when current-prefix-arg ;update cache if prefix
       (ess-process-put 'sp-for-help-changed? t))
     (list (ess-find-help-file "Help on"))))
  (let* ((hb-name (concat "*help[" ess-current-process-name "]("
                          (replace-regexp-in-string "^\\?\\|`" "" object) ")*"))
         (old-hb-p (get-buffer hb-name))
         (tbuffer (get-buffer-create hb-name)))
    (when (or (not old-hb-p)
              (ess-process-get 'sp-for-help-changed?)
              (ess--help-get-bogus-buffer-substring old-hb-p))
      (ess-with-current-buffer tbuffer
        (ess--flush-help-into-current-buffer object command)
        (setq ess-help-object object)
        (ess--help-major-mode)
        (setq truncate-lines nil
              ess-help-type 'help)))
    (unless (ess--help-kill-bogus-buffer-maybe tbuffer)
      (ess-display-help tbuffer))))

(defun ess-help-revert-buffer (_ignore-auto _noconfirm)
  "Revert the current help buffer.
This reloads the documentation. IGNORE-AUTO and NOCONFIRM are
ignored."
  (ess-process-put 'sp-for-help-changed? t)
  (ess-display-help-on-object ess-help-object))

(defalias 'ess-help 'ess-display-help-on-object)

(cl-defgeneric ess-build-help-command (object)
  "Build a string command for retrieving help on OBJECT."
  (format inferior-ess-help-command object))

(defun ess--flush-help-into-current-buffer (object &optional command)
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (let ((command (if (and command (string-match-p "%s" command))
                       (format command object)
                     command)))
      (ess-command (or command (ess-build-help-command object)) (current-buffer)))
    (ess-help-underline)
    (unless (string= ess-language "STA")
      (ess-nuke-help-bs))
    (goto-char (point-min))
    (set-buffer-modified-p 'nil)))

(defun ess--help-kill-bogus-buffer-maybe (buffer)
  "Internal, try to kill bogus BUFFER with message. Return t if killed."
  (when ess-help-kill-bogus-buffers
    (let ((bog-mes  (ess--help-get-bogus-buffer-substring buffer)))
      (when bog-mes
        ;; The following is giving erroneous messages when help is displayed in the browser
        ;; (when (< (length bog-mes) 10) ;;no message at all, how to treat this properly?
        ;;   (setq bog-mes (format "No documentation found; %s" bog-mes)))
        (ess-write-to-dribble-buffer
         (format "(ess-help: kill bogus buffer %s ..\n" (buffer-name buffer)))
        (message "%s" (replace-regexp-in-string  "\n" "" bog-mes))
        ;; (ding) ;; don't ding, in julia a lot of doc strings are very short
        (kill-buffer buffer)))))

(defun ess-display-help-in-browser ()
  "Displaying HTML help where available, using \\[browse-url]."
  (interactive)
  ;; Three ways to find HTML help, 1) ask sub-process 2) get URL/file from subprocess
  ;; 3) call elisp function to get the file path
  ;; For 2 and 3 call browse-url on the output
  (let (com-html-help                   ;1) command for sub-process to trigger
                                        ;help, must contain %s for help obj
        com-get-file-path               ;2) command for sub-process to return a
                                        ; location for the help page, must
                                        ; contain %s for help obj
        fun-get-file-path               ;3) elisp function to return the
                                        ;location, gets one argument, help topic
        not-implemented
        )
    (cond
     ((string-match "^R" ess-dialect)
      (setq com-html-help "help('%s', help_type='html')\n"))
     (t (setq not-implemented t))
     )
    (if not-implemented
        (message "Sorry, not implemented for %s " ess-dialect)
      (if (or (not ess-help-object)
              (not (eq ess-help-type 'help)))
          (message "No help topic found")
        (if com-html-help
            (ess-command (format com-html-help  ess-help-object))
          (require 'browse-url)
          (if com-get-file-path
              (browse-url (car (ess-get-words-from-vector
                                (format com-get-file-path ess-help-object))))
            (when (functionp fun-get-file-path)
              (browse-url (funcall fun-get-file-path ess-help-object)))))))))

(defun ess--button-action (&optional button)
  "Provide help on object at the beginning of line.
It's intended to be used in R-index help pages. Load the package
if necessary.  It is bound to RET and C-m in R-index pages."
  (let* ((string (button-label button))
         (command (ess-build-help-command string)))
    (ess-display-help-on-object string command)))

(cl-defgeneric ess-help-commands ()
  "Return an alist of dialect specific retriever commands.
Currently understood commands:
 - package-for-object - command to get the package of current help object
 - packages - command to get a list of available packages (REQUIRED)
 - package-index - command to get the package index (REQUIRED)
 - index-keyword-reg - regexp used to find keywords for linking in index listing
                 only (1st subexpression is used)
 - index-start-reg - regexp from where to start searching for keywords in index listing"
  (user-error "Not implemented for %s " ess-dialect))

(cl-defmethod ess-help-commands (&context (ess-dialect "R"))
  '((package-for-object . "sub('package:', '', .ess.findFUN('%s'))\n")
    (packages           . ".packages(all.available=TRUE)\n")
    (package-index      . ".ess.help(package='%s', help.type='text')\n")
    (index-keyword-reg  . "^\\([^ \t\n:]+\\)")
    (index-start-reg    . "^Index:")))

(defun ess-display-package-index (&optional package)
  "Prompt for package name and display its index."
  (interactive
   (list (let* ((coms (ess-help-commands))
                (all-packs (ess-get-words-from-vector (cdr (assoc 'packages coms))))
                (pack (or (when (and ess-help-object
                                     (cdr (assoc 'package-for-object coms))
                                     (eq ess-help-type 'help))
                            (car (ess-get-words-from-vector
                                  (format (cdr (assoc 'package-for-object coms))
                                          ess-help-object))))
                          (car (member (ess-read-object-name-default) all-packs)))))
           (ess-completing-read "Index of" all-packs nil nil nil nil pack))))
  (let ((coms (ess-help-commands)))
    (ess--display-indexed-help-page
     (format (cdr (assoc 'package-index coms)) package)
     (cdr (assoc 'index-keyword-reg coms))
     (format "*help[%s](index:%s)*"  ess-dialect package)
     'index nil nil (cdr (assoc 'index-start-reg coms)) package)))

(defun ess--display-indexed-help-page (command item-regexp title help-type
                                               &optional action help-echo reg-start help-object)
  "Internal function to display help pages with linked actions.
COMMAND to produce the indexed help page,
ITEM-REGEXP -- first subexpression is highlighted,
TITLE of the help page,
HELP-TYPE to be stored in `ess-help-type' local variable,
ACTION is a function with no argument (default is `ess--button-action'),
HELP-ECHO if given becomes the help-echo property of the button,
REG-START gives the start location from where to search linkifying, and HELP-OBJECT becomes `ess-help-object'."
  (let ((inhibit-modification-hooks t)
        (alist          ess-local-customize-alist)
        (pname ess-local-process-name)
        (buff (get-buffer-create title)))
    (ess-with-current-buffer buff
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (setq ess-local-process-name pname)
      (ess--help-major-mode)
      (ess-setq-vars-local (eval alist))
      (setq ess-help-object help-object
            ess-help-sec-regex "\\(^\\s-.*\n\\)\\|\\(^\n\\)")
      (ess-command command buff)
      (ess-help-underline)
      (set-buffer-modified-p 'nil)
      (goto-char (point-min))
      (when reg-start ;; go to the beginning of listing
        (re-search-forward  reg-start  nil t))
      (when item-regexp
        ;;linkify the buffer
        (save-excursion
          (while (re-search-forward item-regexp nil t)
            (make-text-button (match-beginning 1) (match-end 1)
                              'mouse-face 'highlight
                              'action (or action #'ess--button-action)
                              'help-object (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                              'follow-link t
                              'help-echo (or help-echo "help on object")))))
      (save-excursion ;; why R help adds all these spaces?
        (goto-char (point-min))
        (when (re-search-forward "Index:\n\n" nil t)
          (let ((beg (point)))
            (forward-paragraph 1)
            (align-regexp beg (point) "\\(\\s-+\\)"))))
      (setq buffer-read-only t)
      (setq ess-help-type help-type)
      (setq truncate-lines nil))
    (unless (ess--help-kill-bogus-buffer-maybe buff)
      (ess-display-help buff))))

(defun ess-display-help-apropos (&optional pattern)
  "Create an ess-apropos buffer with a *linked* list of apropos topics."
  (interactive "sPattern: ")
  (let (com regexp)
    (cond ((equal ess-dialect "R")
           (setq com "help.search('%s')\n"
                 regexp "^\\([^ \t\n:]+::[^ \t\n:]+\\)[ \t\n]+"))
          ((equal ess-dialect "julia")
           (setq com "apropos(\"%s\")\n"
                 regexp "^\\(\\(\\w\\|\\s_\\)+\\)("))
          ((equal ess-dialect "stata")
           (setq com "hsearch %s\n"
                 regexp "^[\t ]*[0-9]+\\.[\t ]+\\(.+\\)$"))
          (t (error "Not implemented for dialect %s" ess-dialect)))
    (ess--display-indexed-help-page
     (format com pattern) regexp
     (format "*ess-apropos[%s](%s)*" ess-current-process-name pattern)
     'appropos)))

(defun ess-display-demos ()
  "Create an ess-demos buffer with a *linked* list of available demos."
  (interactive)
  (let (com regexp)
    (cond ((equal ess-dialect "R")
           (setq com "demo()\n"
                 regexp "^\\([^ \n:]+\\)  +"))
          (t (error "Not implemented for dialect %s" ess-dialect)))
    (ess--display-indexed-help-page
     com regexp
     (format "*ess-demos[%s]*" ess-current-process-name)
     'demos #'ess--action-demo)))

(defun ess--action-demo (&optional button)
  "Provide help on object at the beginning of line.
It's intended to be used in R-index help pages. Load the package
if necessary.  It is bound to RET and C-m in R-index pages."
  (let* ((string (button-label button))
         (command
          (cond ((equal ess-dialect "R")
                 (format "demo('%s')\n" string))
                (t (error "Not implemented for dialect %s" ess-dialect)))))
    (ess-eval-linewise command)
    (ess-switch-to-end-of-ESS)))

(defun ess-display-vignettes (&optional all)
  "Display vignettes if available for the current dialect.
With (prefix) ALL non-nil, use `vignette(*, all=TRUE)`, i.e., from all installed
 packages, which can be *very* slow."
  (interactive "P")
  (ess--display-vignettes-override all))

(cl-defgeneric ess--display-vignettes-override (_all)
  "Display vignettes for the current dialect.
See `ess-display-vignettes' for ALL."
  (user-error "Sorry, not implemented for %s" ess-dialect))

(defun ess--action-open-in-emacs (pos)
  (display-buffer (find-file-noselect (get-text-property pos 'help-echo))))
(defun ess--action-R-open-vignette (pos)
  (ess-eval-linewise (format "vignette('%s', package='%s')\n"
                             (get-text-property pos 'vignette)
                             (get-text-property pos 'package))))

(defalias 'ess-help-quit 'quit-window)
(make-obsolete 'ess-help-quit 'quit-window "16.04")

(defun ess-display-help (buff)
  "Display buffer BUFF.
If `ess-help-pop-to-buffer' is non-nil, call `pop-to-buffer',
otherwise call `display-buffer' to display the buffer.

You may control how help buffers are displayed by EITHER setting
an entry in `display-buffer-alist' (see examples in info
node `(ess) Controlling buffer display') OR setting the
ESS-specific variables `ess-help-own-frame',
`ess-help-reuse-window', `ess-help-frame-alist', and
`ess-display-buffer-reuse-frames'."
  (let* ((action (cond (ess-help-own-frame
                        '(display-buffer-reuse-window
                          display-buffer-use-some-frame
                          display-buffer-pop-up-frame))
                       (ess-help-reuse-window
                        '(display-buffer-reuse-window
                          ess-display-buffer-reuse-mode-window
                          display-buffer-pop-up-window
                          display-buffer-use-some-window))
                       (t '(display-buffer-pop-up-window
                            display-buffer-use-some-window))))
         (alist `((mode . (ess-help-mode ess-r-help-mode ess-stata-help-mode ess-julia-help-mode))
                  (reusable-frames . ,ess-display-buffer-reuse-frames)
                  ;; `display-buffer-use-some-frame' uses this to
                  ;; determine whether to use the frame or not.
                  (frame-predicate . (lambda (f)
                                       (when (equal ess-help-own-frame 'one)
                                         ;; Note we're always returning
                                         ;; nil for `ess-help-own-frame' t.
                                         (frame-parameter f 'ess-help-frame))))
                  ;; If `display-buffer' makes a new frame, these are
                  ;; given as frame parameters.
                  (pop-up-frame-parameters . ,(append ess-help-frame-alist
                                                      `((auto-hide-function . delete-frame)
                                                        (ess-help-frame . ,(equal ess-help-own-frame 'one)))))))
         (display-alist `(,action . ,alist)))
    (if ess-help-pop-to-buffer
        (pop-to-buffer buff display-alist)
      (display-buffer buff display-alist))))

(defun ess-help-web-search (cmd)
  "Search the web for documentation on CMD."
  (interactive "sSearch for: ")
  (ess--help-web-search-override cmd))

(cl-defgeneric ess--help-web-search-override (_cmd)
  "Dialect-specific override for `ess-help-web-search', which see for CMD."
  (error "Not implemented for %s" ess-dialect))

(defun ess-manual-lookup ()
  "Search manual for documentation."
  (interactive)
  (ess--manual-lookup-override))

(cl-defgeneric ess--manual-lookup-override ()
  "Dialect-specific override for `ess-manual-lookup'."
  (error "Not implemented for %s" ess-dialect))

(defvar ess-doc-map
  (let (ess-doc-map)
    (define-prefix-command 'ess-doc-map)
    (define-key ess-doc-map "\C-e" #'ess-describe-object-at-point)
    (define-key ess-doc-map "e" #'ess-describe-object-at-point)
    (define-key ess-doc-map "\C-d" #'ess-display-help-on-object)
    (define-key ess-doc-map "d" #'ess-display-help-on-object)
    (define-key ess-doc-map "\C-i" #'ess-display-package-index)
    (define-key ess-doc-map "i" #'ess-display-package-index)
    (define-key ess-doc-map "\C-a" #'ess-display-help-apropos)
    (define-key ess-doc-map "a" #'ess-display-help-apropos)
    (define-key ess-doc-map "\C-v" #'ess-display-vignettes)
    (define-key ess-doc-map "v" #'ess-display-vignettes)
    (define-key ess-doc-map "\C-o" #'ess-display-demos)
    (define-key ess-doc-map "o" #'ess-display-demos)
    (define-key ess-doc-map "\C-w" #'ess-help-web-search)
    (define-key ess-doc-map "w" #'ess-help-web-search)
    (define-key ess-doc-map "\C-m" #'ess-manual-lookup)
    (define-key ess-doc-map "m" #'ess-manual-lookup)
    ess-doc-map)
  "ESS documentation map.")


(defvar ess-help-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-m" #'next-line)
    (define-key map "h" #'ess-display-help-on-object)
    (define-key map "w" #'ess-display-help-in-browser)
    (define-key map "i" #'ess-display-package-index)
    (define-key map "a" #'ess-display-help-apropos)
    (define-key map "v" #'ess-display-vignettes)
    (define-key map "l" #'ess-eval-line-visibly-and-step)
    (define-key map "r" #'ess-eval-region-and-go)
    (define-key map "f" #'ess-eval-function-or-paragraph-and-step)
    (define-key map "n" #'ess-skip-to-next-section)
    (define-key map "p" #'ess-skip-to-previous-section)
    (define-key map "/" #'isearch-forward)
    (define-key map "x" #'ess-kill-buffer-and-go)
    (define-key map "k" #'kill-this-buffer)
    (define-key map "\C-c\C-s" #'ess-switch-process)
    (define-key map "\C-c\C-r" #'ess-eval-region)
    (define-key map "\C-c\M-r" #'ess-eval-region-and-go)
    (define-key map "\C-c\C-f" #'ess-eval-function)
    (define-key map "\M-\C-x"  #'ess-eval-function)
    (define-key map "\C-c\M-f" #'ess-eval-function-and-go)
    (define-key map "\C-c\C-j" #'ess-eval-line)
    (define-key map "\C-c\C-n" #'ess-eval-line-visibly-and-step)
    (define-key map "\C-c\C-c"   #'ess-eval-region-or-function-or-paragraph-and-step)
    (define-key map [(control return)] #'ess-eval-region-or-line-visibly-and-step)
    (define-key map "\C-c\M-j" #'ess-eval-line-and-go)
    (define-key map "\M-\C-a"  #'ess-goto-beginning-of-function-or-para)
    (define-key map "\M-\C-e"  #'ess-goto-end-of-function-or-para)
    (define-key map "\C-c\C-y" #'ess-switch-to-ESS)
    (define-key map "\C-c\C-z" #'ess-switch-to-end-of-ESS)
    (define-key map "\C-c\C-l" #'ess-load-file)
    (define-key map "\C-c\M-l" #'ess-load-file); alias, as in #'iESS#' where C-c C-l is comint-list-*
    (define-key map "\C-c\C-v" #'ess-display-help-on-object)
    (define-key map "\C-c\C-k" #'ess-request-a-process)
    (define-key map "\C-c\C-d"   'ess-doc-map)
    (define-key map "\C-c\C-e"   'ess-extra-map)
    (define-key map "\C-c\C-t"   'ess-dev-map)
    map)
  "Keymap for ESS help mode.")

;; One reason for the following menu is to <TEACH> the user about key strokes
(defvar ess-help-mode-menu
  '("ESS-help"
    ["Search forward"		isearch-forward t]
    ["Next section"		ess-skip-to-next-section t]
    ["Previous section"		ess-skip-to-previous-section t]
    ["Help on section skipping"	ess-describe-sec-map t]
    ["Beginning of buffer"	beginning-of-buffer t]
    ["End of buffer"		end-of-buffer t]
    "-"
    ["Help on ..."		ess-display-help-on-object t]
    ["Apropos of ..."		ess-display-help-apropos t]
    ["Index of ..."		ess-display-package-index t]
    ["Vignettes"		ess-display-vignettes t]
    ["Open in browser"		ess-display-help-in-browser t]
    "-"
    ["Eval line"		ess-eval-line-and-step t]
    ["Eval paragraph & step"	ess-eval-paragraph-and-step t]
    ["Eval region & go"		ess-eval-region-and-go t]
    ["Switch to ESS process"	ess-switch-to-ESS t]
    ["Switch to end of ESS proc." ess-switch-to-end-of-ESS t]
    ["Switch _the_ process"	ess-switch-process t]
    "-"
    ["Kill buffer"		kill-this-buffer t]
    ["Kill buffer & go"		ess-kill-buffer-and-go t]
    "-"
    ["Handy commands"		ess-handy-commands t])
  "Menu used in ess-help mode.")

(easy-menu-define ess-help-mode-menu-map ess-help-mode-map
  "Menu keymap for ess-help mode." ess-help-mode-menu)

(define-derived-mode ess-help-mode special-mode "ESS Help"
  "Mode for viewing ESS help files."
  :group 'ess-help
  ;; FIXME
  ;; (if ess-mode-syntax-table ;;set in advance by ess-setq-local
  ;;     (set-syntax-table ess-mode-syntax-table))
  (setq-local revert-buffer-function #'ess-help-revert-buffer)
  (setq show-trailing-whitespace nil))


;;*;; User commands defined in ESS help mode

(defun ess-skip-to-help-section ()
  "Jump to a section heading of a help buffer.
The section selected is determined by the command letter used to
invoke the command, as indicated by `ess-help-sec-keys-alist'.
Use \\[ess-describe-sec-map] to see which keystrokes find which
sections."
  (interactive)
  (let ((old-point (point))
        (case-fold-search nil)
        (the-sec (cdr (assoc last-command-event ess-help-sec-keys-alist))))
    (cl-assert the-sec nil (format "Invalid section key: %c" last-command-event))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" the-sec) nil t)
        (progn (recenter)
               (beginning-of-line))
      (message "No %s section in this help. Sorry." the-sec)
      (goto-char old-point))))

(defun ess-skip-to-next-section ()
  "Jump to next section in ESS help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (when (looking-at-p ess-help-sec-regex)
      (forward-line))
    (if (re-search-forward ess-help-sec-regex nil 'no-error)
        (beginning-of-line)
      (message "No more sections."))))

(defun ess-skip-to-previous-section ()
  "Jump to previous section in ESS help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (if (re-search-backward ess-help-sec-regex nil 'no-error)
        (beginning-of-line)
      (message "No previous section."))))

(defun ess-kill-buffer-and-go nil
  "Kill the current buffer and switch back to the ESS process."
  (interactive)
  (kill-buffer (current-buffer))
  (when (and ess-current-process-name (get-process ess-current-process-name))
    (ess-switch-to-ESS nil)))

(defun ess-describe-sec-map nil
  "Display help for the `s' key."
  (interactive)
  (let ((keys-alist ess-help-sec-keys-alist))
    (describe-function 'ess-skip-to-help-section)

    (with-current-buffer "*Help*"
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (insert "\n\nCurrently defined keys are:

Keystroke    Section
---------    -------\n")
      (dolist (cs keys-alist)
        (insert "    "
                (car cs)
                "      "
                (cdr cs) "\n")))))

(defun ess-helpobjs-at-point--read-obj ()
  (let* ((obj (ess-read-object-name-default)))
    ;; Exclude numbers
    (unless (and obj (not (string-match "[[:alpha:]]" obj)))
      obj)))

(defun ess-unqualify-symbol (object)
  (if (string-match "^[[:alnum:].]+::?" object)
      (substring object (match-end 0))
    object))

(defun ess-helpobjs-at-point (slist)
  "Return a list (def obj fun).
Obj is a name at point, fun is the name of the function call
point is in, and def is either obj or fun (in that order) which
has a a help file, i.e. it is a member of SLIST (string-list).
nil otherwise."
  (let* ((obj (ess-helpobjs-at-point--read-obj))
         (unqualified-obj (and obj (ess-unqualify-symbol obj)))
         ;; FIXME: probably should use syntactic logic here
         (fun (ignore-errors
                (save-excursion
                  (save-restriction
                    (narrow-to-region (max (point-min) (- (point) 1000))
                                      (point-max))
                    (backward-up-list 1)
                    (backward-char 1)
                    (ess-read-object-name-default))))))
    (list (or (car (member obj slist))
              (when (member unqualified-obj slist)
                obj)
              (car (member fun slist)))
          obj fun)))

(cl-defgeneric ess-help-get-topics (proc)
  "Return a list of help topics from PROC."
  (user-error "Not supported for %s (process: %s)" ess-dialect proc))

(defun ess-find-help-file (p-string)
  "Find help, prompting for P-STRING."
  (ess-make-buffer-current)
  (let* ((help-files-list (ess-help-get-topics ess-current-process-name))
         (hlpobjs (delq nil (ess-helpobjs-at-point help-files-list))))
    (ess-completing-read p-string (append hlpobjs help-files-list)
                         nil nil nil nil (car hlpobjs))))


;;*;; Utility functions

(defun ess-get-help-files-list ()
  "Return a list of files which have help available."
  (apply 'nconc
         (mapcar (lambda (str)
                   (let ((dirname (concat str "/.Help")))
                     (and (file-directory-p dirname)
                          (directory-files dirname))))
                 (ess-search-list))))

(defun ess-get-help-aliases-list ()
  "Return a list of aliases which have help available."
  (message "Retrieving RDS aliases...")
  ;; ess-command locks display, make sure the above message is visible
  (redisplay t)
  (ess-write-to-dribble-buffer "Processing RDS files ...\n")
  (prog1 (ess-get-words-from-vector ".ess.getHelpAliases()\n")
    (message "Retrieving RDS aliases...done")))

(defun ess-nuke-help-bs ()
  "Remove ASCII underlining and overstriking performed by ^H codes."
  ;; This function is a modification of nuke-nroff-bs in man.el from the
  ;; standard Emacs 18 lisp library.
  ;; Nuke underlining and overstriking (only by the same letter)
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let* ((preceding (char-after (- (point) 2)))
           (following (following-char)))
      (cond ((= preceding following)
             ;; x\bx
             (delete-char -2))
            ((= preceding ?\_)
             ;; _\b
             (delete-char -2))
            ((= following ?\_)
             ;; \b_
             (delete-region (1- (point)) (1+ (point)))))))
  (goto-char (point-min))
  (let ((case-fold-search nil)); 'URL' != 'url' ('libcurl: ' on ?capabilities)
    (while (re-search-forward "\\bURL: " nil t); test with ?rtags
      ;; quick fix for C-x f confusion (getting into tramp)
      (delete-region (match-beginning 0) (match-end 0))))
  ;; Crunch blank lines
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n\n*" nil t)
    (replace-match "\n\n"))
  ;; Nuke blanks lines at start.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point)))

(defun ess-help-underline ()
  "Replace _^H codes with underline face."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "_" nil t)
      (backward-delete-char 2)
      (put-text-property (point) (1+ (point)) 'face 'underline))))

;;*;; Link to Info
(defun ess-goto-info (node)
  "Display node NODE from `ess-mode' info."
  (require 'info)
  (split-window)
  (Info-goto-node (concat "(ess)" node)))


;; describe object at point

(defvar-local ess-describe-object-at-point-commands nil
  "Commands cycled by `ess-describe-object-at-point'.
Dialect specific.")

(defvar ess--descr-o-a-p-commands nil)

(defun ess-describe-object-at-point ()
  "Get info for object at point, & display it in an electric buffer or tooltip.
If region is active use it instead of the object at point.

This is an electric command (`ess--execute-electric-command'),
which means that you can use the last key to cycle through the
action set (in this case `C-e'). After invocation of this command
all standard Emacs commands, except those containing 'window' in
their names, remove the electric *ess-describe* buffer. Use
`other-window' to switch to *ess-describe* window.

Customize `ess-describe-at-point-method' if you wan to display
the description in a tooltip. See also
`ess-r-describe-object-at-point-commands' (and similar option for
other dialects)."
  (interactive)
  (if (not ess-describe-object-at-point-commands)
      (message "Not implemented for dialect %s" ess-dialect)
    (ess-force-buffer-current)
    (let ((map (make-sparse-keymap))
          (objname (or (and (use-region-p)
                            (buffer-substring-no-properties (point) (mark)))
                       (ess-symbol-at-point)))
          ess--descr-o-a-p-commands) ;; used in ess--describe-object-at-point
      (unless objname (error "No object at point "))
      (define-key map (vector last-command-event) 'ess--describe-object-at-point)
      ;; todo: put digits into the map
      (let* ((inhibit-quit t) ;; C-g removes the buffer
             (buf (ess--execute-electric-command
                   map (format "Press %s to cycle"
                               (single-key-description last-command-event))
                   nil nil objname))
             ;; read full command
             (keys (read-key-sequence-vector ""))
             (command (and keys (key-binding keys))))
        (when (and (commandp command)
                   (bufferp buf)
                   (or (not (symbolp command)) ;; kill on lambdas
                       (not (string-match "window" (symbol-name command)))))
          (kill-buffer buf)) ;; bury does not work here :( (Emacs bug?)
        (setq unread-command-events
              (append keys unread-command-events))))))

(defun ess--describe-object-at-point (_ev objname)
  (setq ess--descr-o-a-p-commands (or ess--descr-o-a-p-commands
                                      (symbol-value ess-describe-object-at-point-commands)))
  (let* ((com (format (car (pop ess--descr-o-a-p-commands)) objname))
         (buf (get-buffer-create "*ess-describe*"))
         pos)
    (unless (eq ess-describe-at-point-method 'tooltip)
      ;; can take some time for the command to execute
      (display-buffer buf))
    (sit-for .01)
    (ess-command (concat com "\n") buf) ;; erases buf
    (with-current-buffer buf
      (goto-char (point-min))
      (insert (propertize (format "%s:\n\n" com) 'face 'font-lock-string-face))
      (forward-line -1)
      (setq pos (point))
      ;; set the keys that we are used to in help mode
      (special-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))
    (if (eq ess-describe-at-point-method 'tooltip)
        (ess-tooltip-show-at-point
         (with-current-buffer buf (buffer-string))  0 30)
      (display-buffer buf)
      (set-window-point (get-buffer-window buf) pos) ;; don't move window point
      buf)))

(with-no-warnings
  ;; We're just backporting here, don't care about compiler warnings
  (defalias 'ess-display-buffer-reuse-mode-window
    ;; TODO: Remove once we drop support for Emacs 25
    (if (fboundp 'display-buffer-reuse-mode-window)
        'display-buffer-reuse-mode-window
      (lambda (buffer alist)
        (let* ((alist-entry (assq 'reusable-frames alist))
               (alist-mode-entry (assq 'mode alist))
	       (frames (cond (alist-entry (cdr alist-entry))
		             ((if (eq pop-up-frames 'graphic-only)
			          (display-graphic-p)
			        pop-up-frames)
			      0)
		             (display-buffer-reuse-frames 0)
		             (t (last-nonminibuffer-frame))))
               (inhibit-same-window-p (cdr (assq 'inhibit-same-window alist)))
	       (windows (window-list-1 nil 'nomini frames))
               (buffer-mode (with-current-buffer buffer major-mode))
               (allowed-modes (if alist-mode-entry
                                  (cdr alist-mode-entry)
                                buffer-mode))
               (curwin (selected-window))
               (curframe (selected-frame)))
          (unless (listp allowed-modes)
            (setq allowed-modes (list allowed-modes)))
          (let (same-mode-same-frame
                same-mode-other-frame
                derived-mode-same-frame
                derived-mode-other-frame)
            (dolist (window windows)
              (let ((mode?
                     (with-current-buffer (window-buffer window)
                       (cond ((memq major-mode allowed-modes)
                              'same)
                             ((derived-mode-p allowed-modes)
                              'derived)))))
                (when (and mode?
                           (not (and inhibit-same-window-p
                                     (eq window curwin))))
                  (push window (if (eq curframe (window-frame window))
                                   (if (eq mode? 'same)
                                       same-mode-same-frame
                                     derived-mode-same-frame)
                                 (if (eq mode? 'same)
                                     same-mode-other-frame
                                   derived-mode-other-frame))))))
            (let ((window (car (nconc same-mode-same-frame
                                      same-mode-other-frame
                                      derived-mode-same-frame
                                      derived-mode-other-frame))))
              (when (window-live-p window)
                (prog1 (window--display-buffer buffer window 'reuse alist)
                  (unless (cdr (assq 'inhibit-switch-frame alist))
                    (window--maybe-raise-frame (window-frame window))))))))))))

(provide 'ess-help)
;;; ess-help.el ends here
