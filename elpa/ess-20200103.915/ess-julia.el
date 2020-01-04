;; ess-julia.el --- ESS julia mode and inferior interaction  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2012-2015 Vitalie Spinu and the ESS Core team.
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Created: 02-04-2012 (ESS 12.03)
;; Keywords: ESS, julia
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;; This file is part of ESS
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Customize inferior-julia-program to point to your julia binary
;;  and start the inferior with M-x julia.
;;
;;  As of Sept 2015, this file depends heavily on julia-mode.el from the Julia
;;  sources.  If you install ESS using `make', this will work fine, otherwise
;;  ensure that julia-mode.el is on your path before loading this file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ess-help)
(require 'ess-inf)
(require 'ess-r-mode)
(require 'ess-utils)
(require 'julia-mode)

(defvar ac-prefix)
(declare-function company-in-string-or-comment "company")
(declare-function company-doc-buffer "company")

(defcustom inferior-julia-args ""
  "String of arguments (see `julia --help') used when starting julia."
  :group 'ess-julia
  :type 'string)

(eval-when-compile
  (require 'cl-lib))

(defun ess-julia-send-string-function (process string _visibly)
  "Send the Julia STRING to the PROCESS.
VISIBLY is not currently used."
  (let ((file (concat temporary-file-directory "julia_eval_region.jl")))
    (with-temp-file file
      (insert string))
    (process-send-string process (format ess-load-command file))))


;;; HELP
(cl-defmethod ess-help-get-topics (proc &context (ess-dialect "julia"))
  (append (with-current-buffer (ess-command "ESS.all_help_topics()\n")
            (split-string (buffer-string) "\n"))
          (ess-julia--get-objects proc)))

(defun ess-julia--retrive-topics (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (require 'url)
    (goto-char (point-min))
    (let (out)
      (while (re-search-forward
              "toctext[ \"]+href=\"\\([^>]+\\)\">\\([^<]+\\)</a" nil t)
        (push (propertize (match-string 2)
                          :manual (concat url (match-string 1)))
              out))
      (kill-buffer)
      (nreverse out))))

(defvar ess-julia--manual-topics nil)
(cl-defmethod ess--manual-lookup-override (&context (ess-dialect "julia"))
  "Look up topics at https://docs.julialang.org/en/latest/manual/."
  (let* ((pages (or ess-julia--manual-topics
                    (setq ess-julia--manual-topics
                          (ess-julia--retrive-topics
                           "https://docs.julialang.org/en/latest/"))))
         (page (ess-completing-read "Lookup:" pages nil t)))
    (browse-url (get-text-property 1 :manual page))))

(defun ess-julia-input-sender (proc string)
  "Function to send PROC STRING submitted by user.
See `comint-input-sender'."
  (save-current-buffer
    (let* ((help-?-regexp "^ *\\(?:\\(?1: *?\\? *\\)\\(?2:.+\\)\\)")
           (help-?-match (string-match help-?-regexp string)))
      (cond (help-?-match
             (ess-display-help-on-object (match-string 2 string))
             (process-send-string proc "\n"))
            (t ;; normal command
             (inferior-ess-input-sender proc string))))))

(cl-defmethod ess-installed-packages (&context (ess-dialect "julia"))
  "Return list of installed julia packages."
  ;; FIXME: This doesn't work if the user hasn't done "using Pkg" yet
  (ess-get-words-from-vector "keys(Pkg.installed())\n"))

(cl-defmethod ess-load-library--override (pack &context (ess-dialect "julia"))
  (ess-eval-linewise (format "using %s\n" pack)))


;;; COMPLETION
(defun ess-julia-latexsub-completion ()
  "Complete latex input, and return format required by `completion-at-point-functions'."
  (if (julia-latexsub) ; julia-latexsub returns nil if it performed a completion, the point otherwise
      nil
    (lambda () t) ;; bypass other completion methods
    ))

(defun ess-julia-object-completion ()
  "Return completions at point in a format required by `completion-at-point-functions'."
  (let ((proc (ess-get-next-available-process ess-dialect t))
        (beg (ess-symbol-start)))
    (if proc
        (when beg
          (let* ((prefix (buffer-substring-no-properties beg (point)))
                 (obj (and (string-match "\\(.*\\)\\..*$" prefix)
                           (match-string 1 prefix)))
                 (beg (if obj
                          (+ beg 1 (length obj))
                        beg)))
            (list beg (point)
                  (nreverse (mapcar 'car (ess-julia--get-objects proc obj)))
                  :exclusive 'no)))
      (when (string-match "complet" (symbol-name last-command))
        (message "No ESS process of dialect %s started" ess-dialect)
        nil))))

(defun ess-julia-objects (prefix &optional proc)
  "Given PREFIX get all cached objects from PROC."
  (when prefix
    (let ((proc (or proc (ess-get-next-available-process nil t))))
      (if (string-match "\\(.*\\)\\..*$" prefix)
          (let ((module (match-string 1 prefix)))
            (mapcar (lambda (el) (concat module "." (car el)))
                    (ess-julia--get-objects proc module)))
        (ess-julia--get-objects proc)))))

(defun ess-julia--get-objects (&optional proc obj)
  "Return all available objects.
Local caching might be used. If MODULE is givven, return only
objects from that MODULE."
  (setq proc (or proc (ess-get-process)))
  (when (stringp proc)
    (setq proc (get-process proc)))
  (when (process-live-p proc)
    (let ((objects (process-get proc 'objects)))
      (if (process-get proc 'busy)
          (if obj
              (assoc obj objects)
            (process-get proc 'objects))
        (if obj
            (or (cdr (assoc obj objects))
                ;; don't cache composite objects and datatypes
                (ess-julia--get-components proc obj))
          ;; this segment is entered when user completon at top level is
          ;; requested, either Tab or AC. Hence Main is always updated.
          (let ((modules (ess-get-words-from-vector
                          "ESS.main_modules()\n" nil nil proc))
                (loc (process-get proc 'last-objects-cache))
                (lev (process-get proc 'last-eval)))
            (prog1
                (apply #'nconc
                       (mapcar
                        (lambda (mod)
                          ;; we are caching all modules, and reinit Main every
                          ;; time user enters commands
                          (copy-sequence
                           (or (and (or (not (equal mod "Main"))
                                        (ignore-errors (time-less-p lev loc)))
                                    (cdr (assoc mod objects)))
                               (ess-julia--get-components proc mod t))))
                        modules))
              (process-put proc 'last-objects-cache (current-time)))))))))

(defun ess-julia--get-components (proc obj &optional cache?)
  (with-current-buffer (ess-command (format "ESS.components(%s)\n" obj)
                                    nil nil nil nil proc)
    (goto-char (point-min))
    (let (list)
      (while (re-search-forward
              "^\\([^ \t\n]+\\) +\\([^ \t\n]+\\)$" nil t)
        (push (cons (match-string 1) (match-string 2)) list))
      (when cache?
        (let ((objects (process-get proc 'objects)))
          (push (cons obj list) objects)
          (process-put proc 'objects objects)))
      list)))

(defun ess-julia-get-object-help-string (sym)
  "Help string for ac."
  (let ((proc (ess-get-next-available-process nil t)))
    (if (null proc)
        "No free ESS process found"
      (let ((buf (get-buffer-create " *ess-command-output*")))
        (with-current-buffer (process-buffer proc)
          (ess-with-current-buffer buf
            (ess--flush-help-into-current-buffer sym nil)))
        (with-current-buffer buf
          (ess-help-underline)
          (goto-char (point-min))
          (buffer-string))))))

(defvar ac-source-ess-julia-objects
  '((prefix     . ess-symbol-start)
    (requires   . 2)
    (candidates . ess-ac-julia-objects)
    (document   . ess-julia-get-object-help-string))
  "Auto-completion source for julia objects.")

(defun ess-ac-julia-objects ()
  (require 'auto-complete)
  (ess-julia-objects ac-prefix))

(declare-function company-begin-backend "company.el")

(defun company-ess-julia-objects (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ess-julia-objects))
    (prefix (unless (company-in-string-or-comment)
              (let ((start (ess-symbol-start)))
                (when start (buffer-substring-no-properties start (point))))))
    (candidates (let ((proc (ess-get-next-available-process)))
                  (when proc
                    (all-completions arg (mapcar (lambda (x) (or (car-safe x) x))
                                                 (ess-julia-objects arg proc))))))
    (doc-buffer (company-doc-buffer (ess-julia-get-object-help-string arg)))))


;;; ERRORS
(defvar ess-julia-error-regexp-alist '(ess-julia-in ess-julia-at ess-julia-while-load)
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(add-to-list 'compilation-error-regexp-alist-alist
             '(ess-julia-in  "^\\s-*in [^ \t\n]* \\(at \\(.*\\):\\([0-9]+\\)\\)" 2 3 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(ess-julia-at "^\\S-+\\s-+\\(at \\(.*\\):\\([0-9]+\\)\\)"  2 3 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(ess-julia-while-load "^\\s-*\\(while loading\\s-\\(.*\\), in .* on line +\\([0-9]+\\)\\)"  2 3 nil 2 1))


;;; ELDOC
(defun ess-julia-eldoc-function ()
  "Return the doc string, or nil.
If an ESS process is not associated with the buffer, do not try
to look up any doc strings."
  (when (and ess-can-eval-in-background
             (ess-process-live-p)
             (not (ess-process-get 'busy)))
    (let ((funname (or (and ess-eldoc-show-on-symbol ;; aggressive completion
                            (symbol-name (ess-symbol-at-point)))
                       (car (ess--fn-name-start)))))
      (when funname
        (let* ((args (copy-sequence (nth 2 (ess-function-arguments funname))))
               (W (- (window-width (minibuffer-window)) (+ 4 (length funname))))
               (doc (concat (propertize funname 'face font-lock-function-name-face) ": ")))
          (when args
            (setq args (sort args (lambda (s1 s2)
                                    (< (length s1) (length s2)))))
            (setq doc (concat doc (pop args)))
            (while (and args (< (+ (length doc) (length (car args))) W))
              (setq doc (concat doc "  "
                                (pop args))))
            (when (and args (< (length doc) W))
              (setq doc (concat doc " {--}")))
            doc))))))


;;; CORE
(defvar ess-julia-customize-alist
  '((inferior-ess-primary-prompt   . "\\w*> ")
    (inferior-ess-secondary-prompt . nil)
    (inferior-ess-prompt           . "\\w*> ")
    (ess-local-customize-alist     . 'ess-julia-customize-alist)
    (inferior-ess-program          . inferior-julia-program)
    (ess-load-command              . "include(expanduser(\"%s\"))\n")
    (ess-funargs-command           . "ESS.fun_args(\"%s\")\n")
    (ess-dump-error-re             . "in \\w* at \\(.*\\):[0-9]+")
    (ess-error-regexp              . "\\(^\\s-*at\\s-*\\(?3:.*\\):\\(?2:[0-9]+\\)\\)")
    (ess-error-regexp-alist        . ess-julia-error-regexp-alist)
    (ess-mode-completion-syntax-table . ess-julia-completion-syntax-table)
    ;; (inferior-ess-objects-command    . inferior-ess-r-objects-command)
    ;; (inferior-ess-search-list-command        . "search()\n")
    (inferior-ess-help-command     . "ESS.help(\"%s\")\n")
    ;; (inferior-ess-help-command       . "help(\"%s\")\n")
    (ess-language                  . "julia")
    (ess-dialect                   . "julia")
    (ess-suffix                    . "jl")
    (ess-dump-filename-template    . (replace-regexp-in-string
                                      "S$" ess-suffix ; in the one from custom:
                                      ess-dump-filename-template-proto))
    (ess-mode-editing-alist        . nil)
    (ess-change-sp-regexp          . nil );ess-r-change-sp-regexp)
    (ess-help-sec-regex            . ess-help-r-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-r-sec-keys-alist)
    (ess-function-pattern          . ess-r-function-pattern)
    (ess-object-name-db-file       . "ess-jl-namedb.el" )
    (ess-smart-operators           . ess-r-smart-operators)
    (inferior-ess-exit-command     . "exit()\n")
    ;;harmful for shell-mode's C-a: -- but "necessary" for ESS-help?
    (inferior-ess-language-start   . nil)
    (ess-STERM                     . "iESS")
    (ess-editor                    . ess-r-editor)
    (ess-pager                     . ess-r-pager)
    (ess-getwd-command             . "pwd()\n")
    (ess-setwd-command             . "cd(expanduser(\"%s\"))\n"))
  "Variables to customize for Julia.")

(cl-defmethod ess--help-web-search-override (cmd &context (ess-dialect "julia"))
  "Offer to search the web for a Julia command."
  (browse-url (format "https://docs.julialang.org/en/latest/search/?q=%s" cmd)))

(defvar ess-julia-completion-syntax-table
  (let ((table (copy-syntax-table ess-r-mode-syntax-table)))
    (modify-syntax-entry ?. "_" table)
    ;; (modify-syntax-entry ?: "_" table)
    ;; (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Syntax table used for completion and help symbol lookup.
It makes underscores and dots word constituent chars.")

(cl-defmethod ess-help-commands (&context (ess-dialect "julia"))
  '((packages      . "_ess_list_categories()\n")
    (package-index . "_ess_print_index(\"%s\")\n")
    (index-keyword-reg . "^\\(.*+\\):$*")
    (index-start-reg   . ":")))

(defvar ess-julia-mode-syntax-table (copy-syntax-table julia-mode-syntax-table))

(defvar ess-julia-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ess-mode-map)
    map)
  "Keymap for `ess-julia-mode'.")

;;;###autoload
(define-derived-mode ess-julia-mode julia-mode "ESS[julia]"
  "Major mode for julia files."
  :group 'ess-Julia
  (setq-local ess-local-customize-alist ess-julia-customize-alist)
  (setq ess-dialect "julia")
  (ess-setq-vars-local ess-julia-customize-alist)
  ;; eldoc
  (add-function :before-until (local 'eldoc-documentation-function)
                #'ess-julia-eldoc-function)
  (when ess-use-eldoc (eldoc-mode))
  ;; auto-complete
  (ess--setup-auto-complete '(ac-source-ess-julia-objects))
  ;; company
  (ess--setup-company '(company-ess-julia-objects))
  ;; for emacs >= 24
  (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  (add-hook 'completion-at-point-functions 'ess-julia-object-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-julia-latexsub-completion nil 'local)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar)))

;; Inferior mode
(defvar inferior-ess-julia-mode-syntax-table
  (copy-syntax-table ess-julia-mode-syntax-table)
  "Syntax table for `inferior-ess-julia-mode'.")

(define-derived-mode inferior-ess-julia-mode inferior-ess-mode "iESS[julia]"
  "Major mode for inferior julia processes."
  :group 'ess-Julia
  (ess-setq-vars-local ess-julia-customize-alist)
  (setq-local comint-use-prompt-regexp t)
  (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))
  (setq ess-dialect "julia")
  ;; eldoc
  (add-function :before-until (local 'eldoc-documentation-function)
                #'ess-julia-eldoc-function)
  (when ess-use-eldoc (eldoc-mode))
  ;; auto-complete
  (ess--setup-auto-complete '(ac-source-ess-julia-objects) t)
  ;; company
  (ess--setup-company '(company-ess-julia-objects) t)
  (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  (add-hook 'completion-at-point-functions 'ess-julia-object-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-julia-latexsub-completion nil 'local)
  (setq comint-input-sender #'ess-julia-input-sender))

(defvar ess-julia-mode-hook nil)
(defvar ess-julia-post-run-hook nil
  "Functions run in process buffer after starting julia process.")

;;;###autoload
(defun run-ess-julia (&optional start-args)
  "Start an inferior julia process.
Optional prefix START-ARGS (\\[universal-argument]) allows to set
command line arguments, such as --load=<file>. This should be OS
agnostic. If you have certain command line arguments that should
always be passed to julia, put them in the variable
`inferior-julia-args'."
  (interactive "P")
  ;; get settings, notably inferior-julia-program :
  (if (null inferior-julia-program)
      (error "'inferior-julia-program' does not point to 'julia' or 'julia-basic' executable")
    (ess-write-to-dribble-buffer   ;; for debugging only
     (format
      "\n(julia): ess-dialect=%s, buf=%s, start-arg=%s\n current-prefix-arg=%s\n"
      ess-dialect (current-buffer) start-args current-prefix-arg))
    (let* ((jl-start-args
	        (concat inferior-julia-args " " ; add space just in case
		            (if start-args
			            (read-string
                         (concat "Starting Args"
                                 (if inferior-julia-args
                                     (concat " [other than '" inferior-julia-args "']"))
                                 " ? "))
		              nil))))
      (let ((inf-buf (inferior-ess jl-start-args ess-julia-customize-alist)))
        (with-current-buffer inf-buf
          (ess--tb-start)
          ;; Remove ` from julia's logo
          (goto-char (point-min))
          (while (re-search-forward "`" nil t)
            (replace-match "'"))
          ;; Remove an offending unmatched parenthesis
          (goto-char (point-min))
          (forward-line 4)
          (when (re-search-forward "(" nil t)
            (replace-match "|"))
          (goto-char (point-max))
          ;; --> julia helpers from ../etc/ess-julia.jl :
          (ess--inject-code-from-file (format "%sess-julia.jl" ess-etc-directory))
          (run-mode-hooks 'ess-julia-post-run-hook))
        inf-buf))))

;;;###autoload
(defalias 'julia #'run-ess-julia)

(cl-defmethod ess--help-major-mode (&context (ess-dialect "julia"))
  (ess-julia-help-mode))

(define-derived-mode ess-julia-help-mode ess-help-mode "ESS[Julia] Help"
  "Major mode for Julia documentation."
  :group 'ess-help
  (let ((inhibit-read-only t))
    ;; Julia help buffers can contain color if julia starts with
    ;; --color=yes
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode))

(provide 'ess-julia)
;;; ess-julia.el ends here
