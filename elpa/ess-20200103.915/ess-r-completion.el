;;; ess-r-completion.el --- R completion  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015 A.J. Rossini, Richard M. Heiberger, Martin Maechler, Kurt
;;      Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.
;;
;; Author: Vitalie Spinu
;; Maintainer: ESS-core <ESS-core@r-project.org>
;;
;; Keywords: languages, statistics
;;
;; This file is part of ESS.
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


;;; ElDoc

(require 'cl-lib)
(require 'ess-inf)
(require 'ess-help)

(eval-when-compile
  (require 'subr-x))

(defvar ac-auto-start)
(defvar ac-prefix)
(defvar ac-point)
(defvar ac-use-comphist)
(declare-function company-begin-backend "company")
(declare-function company-doc-buffer "company")

(defcustom ess-R-argument-suffix " = "
  "Suffix appended by `ac-source-R' and `ac-source-R-args' to candidates."
  :group 'ess-R
  :type 'string)

(defun ess-r-eldoc-function ()
  "Return the doc string, or nil.
If an ESS process is not associated with the buffer, do not try
to look up any doc strings."
  (when (and eldoc-mode ess-can-eval-in-background)
    (let* ((proc (ess-get-next-available-process))
           (funname (and proc (or (and ess-eldoc-show-on-symbol ;; Aggressive completion
                                       (thing-at-point 'symbol))
                                  (car (ess--fn-name-start))))))
      (when funname
        (let* ((args (ess-function-arguments funname proc))
               (bargs (cadr args))
               (doc (mapconcat (lambda (el)
                                 (if (equal (car el) "...")
                                     "..."
                                   (concat (car el) "=" (cdr el))))
                               bargs ", "))
               (margs (nth 2 args))
               (W (- (window-width (minibuffer-window)) (+ 4 (length funname))))
               doc1)
          (when doc
            (setq doc (ess-eldoc-docstring-format funname doc))
            (when (and margs (< (length doc1) W))
              (setq doc1 (concat doc (propertize "  || " 'face font-lock-function-name-face)))
              (while (and margs (< (length doc1) W))
                (let ((head (pop margs)))
                  (unless (assoc head bargs)
                    (setq doc doc1
                          doc1 (concat doc1 head  "=, ")))))
              (when (equal (substring doc -2) ", ")
                (setq doc (substring doc 0 -2)))
              (when (and margs (< (length doc) W))
                (setq doc (concat doc " {--}"))))
            doc))))))

(defun ess-eldoc-docstring-format (funname doc)
  (save-match-data
    (let* (;; (name (symbol-name sym))
           (truncate (or  (not (eq t eldoc-echo-area-use-multiline-p))
                          (eq ess-eldoc-abbreviation-style 'aggressive)))
           ;; Subtract 1 from window width since will cause a wraparound and
           ;; resize of the echo area.
           (W (1- (- (window-width (minibuffer-window))
                     (+ 2 (length funname))))))
      (setq doc
            (if (or (<= (length doc) W)
                    (null ess-eldoc-abbreviation-style)
                    (eq 'none ess-eldoc-abbreviation-style))
                doc
              ;;MILD filter
              (setq doc (replace-regexp-in-string "TRUE" "T" doc))
              (setq doc (replace-regexp-in-string "FALSE" "F" doc))
              (if (or (<= (length doc) W)
                      (eq 'mild ess-eldoc-abbreviation-style))
                  doc
                ;;NORMAL filter (deal with long defaults)
                (setq doc (replace-regexp-in-string
                           ;; function calls inside default docs foo(xxxx{..})
                           "([^)]\\{8\\}\\([^)]\\{4,\\}\\))"
                           "{.}" doc nil nil 1))
                (if (<= (length doc) W)
                    doc
                  (setq doc (replace-regexp-in-string
                             " +[^ \t=,\"\]+=[^ \t]\\{10\\}\\([^ \t]\\{4,\\}\\)\\(,\\|\\'\\)"
                             "{.}," doc nil nil 1))
                  (if (<= (length doc) W)
                      doc
                    (setq doc (replace-regexp-in-string
                               " +[^ \t=,\"]+=\\([^ \t]\\{10,\\}\\)\\(,\\|\\'\\)"
                               "{.}," doc nil nil 1))
                    (if (or (<= (length doc) W)
                            (eq 'normal ess-eldoc-abbreviation-style))
                        doc
                      ;;STRONG filter (replace defaults)
                      (setq doc (replace-regexp-in-string
                                 " *[^ \t=,\"\\]* = \\([^ \t]\\{4,\\}\\)\\(,\\|\\'\\)"
                                 "{.}," doc nil nil 1))
                      (if (<= (length doc) W)
                          doc
                        (setq doc (replace-regexp-in-string
                                   "\\(=[^FT0-9].+?\\)\\(, [^ =,\"\\]+=\\|\\'\\)"
                                   "" doc nil nil 1))
                        (setq doc (replace-regexp-in-string
                                   "\\(=[^FT0-9].+?\\)\\(, [^ =,\"\\]+,\\|\\'\\)"
                                   "" doc nil nil 1))
                        (if (or (<= (length doc) W)
                                (eq 'strong ess-eldoc-abbreviation-style))
                            doc
                          ;;AGGRESSIVE filter (truncate what is left)
                          (concat (substring doc 0 (- W 4)) "{--}")))))))))
      (when (and truncate
                 (> (length doc) W))
        (setq doc (concat (substring doc 0 (- W 4)) "{--}")))
      (format "%s: %s" (propertize funname 'face 'font-lock-function-name-face) doc))))


;;; OBJECTS

(defun ess-r-object-completion ()
  "Return completions at point in a format required by `completion-at-point-functions'."
  (if (ess-make-buffer-current)
      (let* ((funstart (cdr (ess--fn-name-start)))
             (completions (ess-r-get-rcompletions funstart))
             (token (pop completions)))
        (when completions
          (list (- (point) (length token)) (point)
                completions)))
    (when (string-match "complete" (symbol-name last-command))
      (message "No ESS process associated with current buffer")
      nil)))

(defun ess-complete-object-name ()
  "Perform completion on object preceding point.
Uses `ess-r-complete-object-name' when `ess-use-R-completion' is non-nil,
and `ess-internal-complete-object-name' otherwise."
  (interactive)
  (if (ess-make-buffer-current)
      (if ess-use-R-completion
          (ess-r-complete-object-name)
        (ess-internal-complete-object-name))
    ;; else give a message on second invocation
    (when (string-match "complete" (symbol-name last-command))
      (message "No ESS process associated with current buffer")
      nil)))

(define-obsolete-function-alias 'ess-list-object-completions #'ess-complete-object-name "ESS 19.04")

;; This one is needed for R <= 2.6.x -- hence *not* obsoleting it
(defun ess-internal-complete-object-name ()
  "Perform completion on `ess-language' object preceding point.
The object is compared against those objects known by
`ess-get-object-list' and any additional characters up to ambiguity are
inserted.  Completion only works on globally-known objects (including
elements of attached data frames), and thus is most suitable for
interactive `command-line' entry, and not so much for function editing
since local objects (e.g. argument names) aren't known.

Use \\[ess-resynch] to re-read the names of the attached directories.
This is done automatically (and transparently) if a directory is
modified (S only!), so the most up-to-date list of object names is always
available.  However attached dataframes are *not* updated, so this
command may be necessary if you modify an attached dataframe."
  (ess-make-buffer-current)
  (if (memq (char-syntax (preceding-char)) '(?w ?_))
      (let* ((comint-completion-addsuffix nil)
             (bounds (ess-bounds-of-symbol))
             (beg (car bounds))
             (end (cdr bounds))
             (full-prefix (buffer-substring beg end))
             (pattern full-prefix)
             ;; See if we're indexing a list with `$'
             (listname (if (string-match "\\(.+\\)\\$\\(\\(\\sw\\|\\s_\\)*\\)$"
                                         full-prefix)
                           (progn
                             (setq pattern
                                   (if (not (match-beginning 2)) ""
                                     (substring full-prefix
                                                (match-beginning 2)
                                                (match-end 2))))
                             (substring full-prefix (match-beginning 1)
                                        (match-end 1)))))
             ;; are we trying to get a slot via `@' ?
             (classname (if (string-match "\\(.+\\)@\\(\\(\\sw\\|\\s_\\)*\\)$"
                                          full-prefix)
                            (progn
                              (setq pattern
                                    (if (not (match-beginning 2)) ""
                                      (substring full-prefix
                                                 (match-beginning 2)
                                                 (match-end 2))))
                              (ess-write-to-dribble-buffer
                               (format "(ess-C-O-Name : slots..) : patt=%s"
                                       pattern))
                              (substring full-prefix (match-beginning 1)
                                         (match-end 1)))))
             (components (if listname
                             (ess-object-names listname)
                           (if classname
                               (ess-slot-names classname)
                             ;; Default case: It hangs here when
                             ;;    options(error=recover) :
                             (ess-get-object-list ess-current-process-name)))))
        ;; always return a non-nil value to prevent history expansions
        (or (completion-in-region beg end components) 'none))))

(defun ess-r-get-rcompletions (&optional start end prefix allow-3-dots)
  "Call R internal completion utilities (rcomp) for possible completions.
Optional START and END delimit the entity to complete, default to
bol and point.  If PREFIX is given, perform completion on
PREFIX.  First element of the returned list is the completion
token.  Needs version of R >= 2.7.0."
  (let* ((start (or start
                    (if prefix
                        0
                      (save-excursion (comint-bol nil) (point)))))
         (end (or end (if prefix (length prefix) (point))))
         (prefix (or prefix (buffer-substring start end)))
         ;; (opts1 (if no-args "op<-rc.options(args=FALSE)" ""))
         ;; (opts2 (if no-args "rc.options(op)" ""))
         (call1 (format ".ess_get_completions(\"%s\", %d, \"%s\")"
                        (ess-quote-special-chars prefix)
                        (- end start)
                        ess-R-argument-suffix))
         (cmd (if allow-3-dots
                  (concat call1 "\n")
                (concat "local({ r <- " call1 "; r[r != '...='] })\n"))))
    (ess-get-words-from-vector cmd)))

(defun ess-r-complete-object-name ()
  "Completion in R via R's completion utilities (formerly 'rcompgen').
To be used instead of ESS' completion engine for R versions >= 2.7.0."
  (let ((possible-completions (ess-r-get-rcompletions))
        token-string)
    (when possible-completions
      (setq token-string (pop possible-completions))
      (or (completion-in-region (- (point) (length token-string))
                                (point)
                                possible-completions)
          'none))))

(defvar ess--cached-sp-objects nil)

(defun ess--get-cached-completions (prefix &optional _point)
  (if (string-match-p "[]:$@[]" prefix)
      ;; call proc for objects
      (cdr (ess-r-get-rcompletions nil nil prefix))
    ;; else, get cached list of objects
    (with-ess-process-buffer 'no-error ;; use proc buf alist
      (ess-when-new-input last-cached-completions
        (if (and ess--cached-sp-objects
                 (not  (process-get *proc* 'sp-for-ac-changed?)))
            ;; if global cache is already there, only re-read local .GlobalEnv
            (progn
              (unless ess-sl-modtime-alist
                ;; initialize if empty
                (setq ess-sl-modtime-alist '((".GlobalEnv" nil))))
              ;; fixme: Make adaptive. Not on all remotes are slow; For lots of
              ;; objects in .GlobalEnv,locals could also be slow.
              (unless (file-remote-p default-directory)
                (ess-extract-onames-from-alist ess-sl-modtime-alist 1 'force)))
          (if ess--cached-sp-objects
              (ess-get-modtime-list 'ess--cached-sp-objects 'exclude-first)
            (ess-get-modtime-list)
            (setq ess--cached-sp-objects (cdr ess-sl-modtime-alist)))
          ;; reread new package, but not rda, much faster and not needed anyways
          (process-put *proc* 'sp-for-ac-changed? nil)))
      (apply 'append
             (cl-cddar ess-sl-modtime-alist) ; .GlobalEnv
             (mapcar 'cddr ess--cached-sp-objects)))))


;;; ARGUMENTS

(defvar ess-r--funargs-pre-cache
  '(("plot"
     (("graphics")
      (("x" . "")    ("y" . "NULL")    ("type" . "p")    ("xlim" . "NULL")    ("ylim" . "NULL")    ("log" . "")    ("main" . "NULL")    ("sub" . "NULL")    ("xlab" . "NULL")    ("ylab" . "NULL")
       ("ann" . "par(\"ann\")")     ("axes" . "TRUE")    ("frame.plot" . "axes")    ("panel.first" . "NULL")    ("panel.last" . "NULL")    ("asp" . "NA")    ("..." . ""))
      ("x" "y" "..." "ci" "type" "xlab" "ylab" "ylim" "main" "ci.col" "ci.type" "max.mfrow" "ask" "mar" "oma" "mgp" "xpd" "cex.main" "verbose" "scale" "xlim" "log" "sub" "ann" "axes" "frame.plot"
       "panel.first" "panel.last" "asp" "center" "edge.root" "nodePar" "edgePar" "leaflab" "dLeaf" "xaxt" "yaxt" "horiz"
       "zero.line" "verticals" "col.01line" "pch" "legend.text" "formula" "data" "subset" "to" "from" "newpage" "vp" "labels"
       "hang" "freq" "density" "angle" "col" "border" "lty" "add" "predicted.values" "intervals" "separator" "col.predicted"
       "col.intervals" "col.separator" "lty.predicted" "lty.intervals" "lty.separator" "plot.type" "main2" "par.fit" "grid"
       "panel" "cex" "dimen" "abbrev" "which" "caption" "sub.caption" "id.n" "labels.id" "cex.id" "qqline" "cook.levels"
       "add.smooth" "label.pos" "cex.caption" "rows" "levels" "conf" "absVal" "ci.lty" "xval" "do.points" "col.points" "cex.points"
       "col.hor" "col.vert" "lwd" "set.pars" "range.bars" "col.range" "xy.labels" "xy.lines" "nc" "yax.flip" "mar.multi" "oma.multi")))
    ("print"
     (("base")
      (("x" . "")    ("digits" . "NULL")    ("quote" . "TRUE")    ("na.print" . "NULL")    ("print.gap" . "NULL")    ("right" . "FALSE")    ("max" . "NULL")    ("useSource" . "TRUE")    ("..." . ""))
      ("x" "..." "digits" "signif.stars" "intercept" "tol" "se" "sort" "verbose" "indent" "style" ".bibstyle" "prefix" "vsep" "minlevel" "quote" "right" "row.names" "max" "na.print" "print.gap"
       "useSource" "diag" "upper" "justify" "title" "max.levels" "width" "steps" "showEnv" "newpage" "vp" "cutoff" "max.level" "give.attr" "units" "abbrCollate" "print.x" "deparse" "locale" "symbolic.cor"
       "loadings" "zero.print" "calendar"))))
  "Alist of cached arguments for time consuming functions.")


;;; HELP

(defun ess-r-get-object-help-string (sym)
  "Help string for ac."
  (let ((proc (ess-get-next-available-process)))
    (if (null proc)
        "No free ESS process found"
      (let ((buf (get-buffer-create " *ess-command-output*")))
        (when (string-match ":+\\(.*\\)" sym)
          (setq sym (match-string 1 sym)))
        (with-current-buffer (process-buffer proc)
          (ess-with-current-buffer buf
            (ess--flush-help-into-current-buffer sym nil)))
        (with-current-buffer buf
          (ess-help-underline)
          (goto-char (point-min))
          (buffer-string))))))

(defun ess-r-get-arg-help-string (sym &optional proc)
  "Help string for ac."
  (setq sym (replace-regexp-in-string " *= *\\'" "" sym))
  (let ((proc (or proc (ess-get-next-available-process))))
    (if (null proc)
        "No free ESS process found"
      (let ((fun (car ess--fn-name-start-cache)))
        (with-current-buffer (ess-command (format ".ess_arg_help('%s','%s')\n" sym fun)
                                          nil nil nil nil proc)
          (goto-char (point-min))
          (forward-line)
          (buffer-substring-no-properties (point) (point-max)))))))


;;; COMPANY
;;; https://company-mode.github.io/

(defun company-R-objects (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-R-objects))
    (prefix (unless (ess-inside-string-or-comment-p)
              (let ((start (ess-symbol-start)))
                (when start
                  (buffer-substring-no-properties start (point))))))
    (candidates (let ((proc (ess-get-next-available-process)))
                  (when proc
                    (with-current-buffer (process-buffer proc)
                      (all-completions arg (ess--get-cached-completions arg))))))
    (doc-buffer (company-doc-buffer (ess-r-get-object-help-string arg)))))

(defun company-R-args (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-R-args))
    (prefix (unless (ess-inside-string-or-comment-p)
              (let ((start (ess-arg-start)))
                (when start
                  (let ((prefix (buffer-substring-no-properties start (point))))
                    (if ess-company-arg-prefix-length
                        (cons prefix (>= (length prefix)
                                         ess-company-arg-prefix-length))
                      prefix))))))
    (candidates (let* ((proc (ess-get-next-available-process))
                       (args (delete "..." (nth 2 (ess-function-arguments
                                                   (car ess--fn-name-start-cache) proc))))
                       (args (mapcar (lambda (a) (concat a ess-R-argument-suffix))
                                     args)))
                  (all-completions arg args)))
    (meta (let ((proc (ess-get-next-available-process)))
            (when (and proc
                       (with-current-buffer (process-buffer proc)
                         (not (file-remote-p default-directory))))
              ;; fixme: ideally meta should be fetched with args
              (let ((doc (ess-r-get-arg-help-string arg proc)))
                (replace-regexp-in-string "^ +\\| +$" ""
                                          (replace-regexp-in-string "[ \t\n]+" " " doc))))))
    (sorted t)
    (require-match 'never)
    (doc-buffer (company-doc-buffer (ess-r-get-arg-help-string arg)))))

(defun company-R-library (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-R-library))
    (prefix (and (member (car (ess--fn-name-start))
                         '("library" "require"))
                 (let ((start (ess-symbol-start)))
                   (and start (buffer-substring start (point))))))
    (candidates (all-completions arg (ess-installed-packages)))
    (annotation "<pkg>")
    (duplicates nil)
    (sorted t)))

;; FIXME: There's a lot of overlap between `ess-r-package-completion'
;; and `company-R-library'. Can we merge them somehow?
(defun ess-r-package-completion ()
  "Return installed packages if in a call to library or require.
Return format suitable for `completion-at-point-functions'."
  (when (member (car (ess--fn-name-start))
                '("library" "require"))
    (list (ess-symbol-start)
          (point)
          (ess-installed-packages)
          :annotation-function
          (lambda (_) " <pkg>"))))


;;; AC SOURCES
;;; http://cx4a.org/software/auto-complete/index.html
;; auto-complete is de-facto unmaintained, users should switch to `company-mode'.

(defvar ac-source-R
  '((prefix     . ess-ac-start)
    ;; (requires   . 0) ::)
    (candidates . ess-ac-candidates)
    ;; (action  . ess-ac-action-args) ;; interfere with ac-fallback mechanism on RET (which is extremely annoying in inferior buffers)
    (document   . ess-ac-help))
  "Combined ad-completion source for R function arguments and R objects.")
(make-obsolete-variable 'ac-source-R "Use company-mode instead" "ESS 19.04")

(defun ess-ac-start ()
  (when (ess-process-live-p)
    (or (ess-arg-start)
        (ess-symbol-start))))
(make-obsolete-variable 'ess-ac-start "Use company-mode instead" "ESS 19.04")

(defun ess-ac-candidates ()
  "OBJECTS + ARGS."
  (let ((args (with-no-warnings
                ;; suppress obsolete warnings
                (ess-ac-args))))
    ;; sort of intrusive but right
    (if (and ac-auto-start
             (< (length ac-prefix) ac-auto-start))
        args
      (if args
          (append args (with-no-warnings
                         ;; suppress obsolete warnings
                         (ess-ac-objects t)))
        (with-no-warnings
          ;; suppress obsolete warnings
          (ess-ac-objects))))))
(make-obsolete-variable 'ess-ac-candidates "Use company-mode instead" "ESS 19.04")

(defun ess-ac-help (sym)
  (if (string-match-p "= *\\'" sym)
      (ess-r-get-arg-help-string sym)
    (ess-r-get-object-help-string sym)))
(make-obsolete-variable 'ess-ac-help "Use company-mode instead" "ESS 19.04")

;; OBJECTS
(defvar  ac-source-R-objects
  '((prefix     . ess-symbol-start)
    ;; (requires   . 2)
    (candidates . ess-ac-objects)
    (document   . ess-r-get-object-help-string))
  "Auto-completion source for R objects.")
(make-obsolete-variable 'ac-source-R-objects "Use company-mode instead" "ESS 19.04")

(defun ess-ac-objects (&optional no-kill)
  "Get all cached objects."
  (declare (obsolete "Use company-mode instead" "ESS 19.04"))
  (let ((aprf ac-prefix))
    (when (and aprf (ess-process-live-p))
      (unless no-kill ;; workaround
        (kill-local-variable 'ac-use-comphist))
      (ess--get-cached-completions aprf ac-point))))

;; ARGS
(defvar  ac-source-R-args
  '((prefix     . ess-arg-start)
    ;; (requires   . 0)
    (candidates . ess-ac-args)
    ;; (action     . ess-ac-action-args)
    (document   . ess-r-get-arg-help-string))
  "Auto-completion source for R function arguments.")
(make-obsolete-variable 'ac-source-R-args "Use company-mode instead" "ESS 19.04")

(defun ess-ac-args ()
  "Get the args of the function when inside parentheses."
  (declare (obsolete "Use company-mode-instead" "ESS 19.04"))
  (when  (and ess--fn-name-start-cache ;; set in a call to ess-arg-start
              (ess-process-live-p))
    (let ((args (nth 2 (ess-function-arguments (car ess--fn-name-start-cache)))))
      (if args
          (setq-local ac-use-comphist nil)
        (kill-local-variable 'ac-use-comphist))
      (delete "..." args)
      (mapcar (lambda (a) (concat a ess-R-argument-suffix))
              args))))

(provide 'ess-r-completion)

;;; ess-r-completion.el ends here
