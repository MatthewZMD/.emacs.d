;;; ess-r-package.el --- Package development mode for R.  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2015 Lionel Henry, Vitalie Spinu, A.J. Rossini, Richard
;;      M. Heiberger, Martin Maechler, Kurt Hornik, Rodney Sparapani, and
;;      Stephen Eglen.

;; Author: Lionel Henry, Vitalie Spinu
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages, tools

;; This file is part of ESS.

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

;; see appropriate documentation section of ESS user manual

;;; Code:
(require 'cl-lib)
(require 'ess-inf)

(eval-when-compile
  (require 'subr-x)
  (require 'tramp))
;; Silence the byte compiler, OK because this file is only loaded by
;; ess-r-mode and has no autoloads.
(defvar ess-r-customize-alist)
(declare-function inferior-ess-r-force "ess-r-mode")
(declare-function ess-r-get-evaluation-env "ess-r-mode")
(declare-function ess-r-set-evaluation-env "ess-r-mode")
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
;; This can be drop after dropping support for Emacs 25:
(declare-function tramp-file-name-localname "tramp" (cl-x))

(defvar ess-r-prompt-for-attached-pkgs-only nil
  "If nil provide completion for all installed R packages.
If non-nil, only look for attached packages.")

(define-obsolete-variable-alias 'ess-r-package-auto-set-evaluation-env 'ess-r-package-auto-enable-namespaced-evaluation "18.04")
(define-obsolete-variable-alias 'ess-r-package-auto-set-evaluation-env 'ess-r-package-auto-enable-namespaced-evaluation "18.04")
(defcustom ess-r-package-auto-enable-namespaced-evaluation t
  "If non-nil, evaluation env is set to package env automatically.
See also `ess-r-set-evaluation-env' and `ess-r-evaluation-env'."
  :group 'ess-r-package
  :type 'boolean)

(defvar-local ess-r-package--info-cache nil
  "Current package info cache.
See `ess-r-package-info' for its structure.")

(define-obsolete-variable-alias 'ess-r-package-library-path 'ess-r-package-library-paths "v18.04")
(defcustom ess-r-package-library-paths nil
  "Default path to find user packages.
Can be either a string specifying a directory or a list of directories."
  :group 'ess-r-package-library-paths
  :type `(choice string (repeat string)))

(defvar ess-r-package-root-file "DESCRIPTION"
  "Presence of this file indicates the project's root.")

(defvar ess-r-package-dirs
  '(("R"        . 1)
    ("r"        . 1)
    ("tests"    . 1)
    ("testthat" . 2)
    ("inst"     . 1)
    ("include"  . 2)
    ("src"      . 1))
  "Alist of directories names and their depth in R package hierarchy.
This list is used to figure out whether the current file belongs
to an R package. If the file specified in `ess-r-package-root-file'
\(DESCRIPTION by default) is found at the presumed root directory
of the package, the current directory is considered to be part of
a R package.")

(defvar ess-r-package-source-roots
  '("R" "src" "tests" "inst/include")
  "List of sub-directories within R package where source files are located.
All children of these directories are also considered source
containing directories.  Use `ess-r-package-source-dirs' to get
all source dirs recursively within the current package.")


;;;*;;; Package Detection

(defun ess-r-package-project (&optional dir)
  "Return the current package as an Emacs project instance.
A project instance is a cons cell of the project type as symbol
and the project path as string. If DIR is provided, the package
is searched from that directory instead of `default-directory'."
  (let ((pkg-info (ess-r-package-info dir)))
    (when (car pkg-info)
      (cons 'ess-r-package (plist-get pkg-info :root)))))

(cl-defmethod project-roots ((project (head ess-r-package)))
  "Return the project root for ESS R packages"
  (list (cdr project)))

(defun ess-r-package-name (&optional dir)
  "Return the name of the current package as a string."
  (plist-get (ess-r-package-info dir) :name))

(defun ess-r-package-info (&optional dir)
  "Get the description of the R project in directory DIR.
Return an plist with the keys :name and :root. When not in a
package return '(nil). This value is cached buffer-locally for
efficiency reasons."
  (if (and (null dir) (car ess-r-package--info-cache))
      ess-r-package--info-cache
    (let* ((path (ess-r-package--find-package-path (or dir default-directory)))
           (name (when path
                   (ess-r-package--find-package-name path)))
           (info (if name
                     (list :name name
                           :root path)
                   '(nil))))
      ;; If DIR was supplied we cannot cache in the current buffer.
      (if dir
          info
        (setq-local ess-r-package--info-cache info)))))

(defun ess-r-package--all-source-dirs (dir)
  (when (file-directory-p dir)
    (cl-loop for f in (directory-files-and-attributes dir t "^[^.]")
             if (cadr f)
             append (cons (car f) (ess-r-package--all-source-dirs (car f))))))

(defun ess-r-package-source-dirs ()
  "Get paths within current R package with source files.
Return nil if not in a package. Search sub-directories listed in
`ess-r-package-source-roots' are searched recursively and
return all physically present directories."
  (let ((pkg-root (plist-get (ess-r-package-info) :root)))
    (when pkg-root
      (let ((files (directory-files-and-attributes pkg-root t "^[^.]")))
        (cl-loop for f in files
                 if (and (cadr f)
                         (cl-some (lambda (el) (string-match-p (concat "/" el "$") (car f)))
                                  ess-r-package-source-roots))
                 append (cons (car f)
                              (ess-r-package--all-source-dirs (car f))))))))

(defun ess-r--select-package-name ()
  (inferior-ess-r-force)
  (let ((pkgs (ess-get-words-from-vector
               (format "print(.packages(%s), max = 1e6)\n"
                       (if ess-r-prompt-for-attached-pkgs-only "FALSE" "TRUE"))))
        (current-pkg (ess-r-package-name)))
    (let ((env (ess-r-get-evaluation-env)))
      (when env
        (setq pkgs (append '("*none*") pkgs))
        (when (equal env current-pkg)
          (setq current-pkg "*none*"))))
    (ess-completing-read "Package" pkgs nil nil nil nil current-pkg)))

(defun ess-r-package--find-package-path (&optional dir)
  "Get the root of R package in directory DIR.
DIR defaults to the current buffer's file name (if non-nil) or
`default-directory'. Root is determined by locating
`ess-r-package-root-file'. If the path looks like a tramp file,
remove the remote information."
  (when-let ((path (cond
                    (dir)
                    ((buffer-file-name)
                     (file-name-directory (buffer-file-name)))
                    (t
                     default-directory)))
             (pkg-path
              (when path
                (or
                 ;; First check current directory
                 (and (file-exists-p (expand-file-name ess-r-package-root-file path))
                      path)
                 ;; Check for known directories in current path
                 (let ((current-dir (file-name-nondirectory (directory-file-name path)))
                       known-pkg-dir known-path presumptive-path)
                   (while (and path (not presumptive-path))
                     (setq current-dir (file-name-nondirectory (directory-file-name path)))
                     (if (and (setq known-pkg-dir (assoc current-dir ess-r-package-dirs))
                              (setq known-path (ess--parent-dir path (cdr known-pkg-dir)))
                              (file-exists-p (expand-file-name ess-r-package-root-file known-path)))
                         (setq presumptive-path known-path)
                       (setq path (ess--parent-dir path 1))))
                   presumptive-path)))))
    (if (file-remote-p pkg-path)
        (tramp-file-name-localname (tramp-dissect-file-name pkg-path))
      (directory-file-name pkg-path))))

(defun ess-r-package--find-package-name (path)
  (let ((file (expand-file-name ess-r-package-root-file path))
        (case-fold-search t))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents-literally file)
        (goto-char (point-min))
        (when (re-search-forward "package: \\(.*\\)" nil t)
          (match-string 1))))))


;;;*;;; UI

(defun ess-r-package-use-dir ()
  "Set process directory to current package directory."
  (interactive)
  (let ((pkg-root (plist-get (ess-r-package-info) :root)))
    (if pkg-root
        (ess-set-working-directory (abbreviate-file-name pkg-root))
      (user-error "Not in a project"))))


;;;*;;; Evaluation

(defun ess-r-package-enable-namespaced-evaluation ()
  "Enable namespaced evaluation in current buffer.
Namespaced evaluation is enabled if
`ess-r-package-auto-enable-namespaced-evaluation' is non-nil."
  (when ess-r-package-auto-enable-namespaced-evaluation
    (let ((root (plist-get (ess-r-package-info) :root)))
      ;; Check that we are in a file within R/
      (when (and root
                 default-directory
                 (> (length default-directory) (1+ (length root)))
                 (let ((subpath (substring default-directory
                                           (1+ (length root))
                                           (length default-directory))))
                   (string= (directory-file-name subpath) "R")))
        (ess-r-set-evaluation-env (ess-r-package-name))))))

(add-hook 'ess-r-mode-hook 'ess-r-package-enable-namespaced-evaluation)

(defun ess-r-package-eval-linewise (command &optional msg p actions)
  "Send COMMAND to R process.
COMMAND is a command string with %s placeholder for the
arguments. MSG is the message displayed in minibuffer with %s
placeholder for the package name. P is the value of universal
argument usually received from the upstream command and indicates
which action in ACTIONS list to perform; if 0 or nil, first
action, if 1 or (4) second if 2 or (16) third etc. ACTIONS is a
list of strings (R arguments), or functions which return R
arguments, or expressions which return R arguments."
  (inferior-ess-r-force)
  (let ((pkg-info (ess-r-package-info))
        (args (ess-r-command--build-args p actions)))
    (unless (car pkg-info)
      (user-error "Not in a package"))
    (ess-project-save-buffers)
    (message msg (plist-get pkg-info :name))
    (display-buffer (ess-get-process-buffer))
    (let ((pkg-path (concat "'" (abbreviate-file-name (plist-get pkg-info :root)) "'")))
      (ess-eval-linewise (format command (concat pkg-path args))))))

(defun ess-r-command--build-args (ix &optional actions)
  (let* ((n (cond ((null ix) 0)
                  ((listp ix) (round (log (car ix) 4)))
                  ((integerp ix) ix)
                  (t (error "Invalid index"))))
         (action (nth n actions))
         (args (cond ((null action) "")
                     ((stringp action) action)
                     ((functionp action) (funcall action))
                     ((listp action) (eval action))
                     (t (error "Invalid action")))))
    (if (string= "" args)
        args
      (concat ", " args))))


;;;*;;; Devtools Integration

(defun ess-r-devtools-load-package (&optional arg)
  "Interface for `devtools::load_all()'.
With prefix ARG ask for extra args."
  (interactive "P")
  (ess-r-package-eval-linewise
   "devtools::load_all(%s)\n" "Loading %s" arg
   '("" (read-string "Arguments: " "recompile = TRUE"))))

(defun ess-r-devtools-unload-package ()
  "Interface to `devtools::unload()'."
  (interactive)
  (ess-r-package-eval-linewise
   "devtools::unload(%s)\n" "Unloading %s"))

(defun ess-r-devtools-check-package (&optional arg)
  "Interface for `devtools::check()'.
With prefix ARG ask for extra args."
  (interactive "P")
  (ess-r-package-eval-linewise
   "devtools::check(%s)\n" "Checking %s" arg
   '("" (read-string "Arguments: " "vignettes = FALSE"))))

(defun ess-r-devtools-check-with-winbuilder (&optional arg)
  "Interface for `devtools::buildwin()'.
With prefix ARG build with R-devel instead of R-patched."
  (interactive "P")
  (ess-r-package-eval-linewise
   "devtools::build_win(%s)\n" "Checking %s on CRAN's Windows server" arg
   '("" "version = 'R-devel'")))

(defvar ess-r-rhub--history nil)
(declare-function ess-r-check-install-package "ess-r-mode.el")
(defun ess-r-rhub-check-package (&optional arg)
  "Interface for `rhub::check()'.
With prefix ARG run with `valgrind = TRUE'."
  (interactive "P")
  (inferior-ess-r-force)
  (ess-r-check-install-package "rhub")
  (let* ((platforms (ess-get-words-from-vector "rhub::platforms()$name\n"))
         (platform (completing-read "Platform: " platforms nil t  nil
                                    ess-r-rhub--history (car ess-r-rhub--history)))
         (cmd (format "rhub::check_for_cran(%%s, platform = '%s')\n" platform))
         (msg (format "Checking %%s on RHUB (%s)" platform)))
    (ess-r-package-eval-linewise cmd msg arg '("" "valgrind = TRUE"))))

(defun ess-r-devtools-build (&optional arg)
  "Interface for `devtools::build()'.
With prefix ARG, build with 'vignettes = FALSE'."
  (interactive "P")
  (ess-r-package-eval-linewise
   "devtools::build(%s)\n" "Building %s" arg
   '("" "vignettes = FALSE")))

(defun ess-r-devtools-test-package (&optional arg)
  "Interface for `devtools::test()'.
With prefix argument ARG, run tests on current file only."
  (interactive "P")
  (ess-r-package-eval-linewise
   "devtools::test(%s)\n" "Testing %s" arg
   '("" ess-r-devtools--cur-file-filter)))

(defun ess-r-devtools--cur-file-filter ()
  (let ((file (or (and buffer-file-name
                       (file-name-nondirectory buffer-file-name))
                  (error "Buffer not visiting a file"))))
    (format "filter = \"%s\""
            (if (string-match "test-\\([[:alnum:]_-]+\\)\\.[rR]" file)
                (match-string-no-properties 1 file)
              (file-name-base buffer-file-name)))))

(defvar ess-r-devtools-revdep-check-cmd
  "local({
  pkg_path <- %s
  res <- devtools::revdep_check(pkg_path)

  if (file.exists(file.path(pkg_path, 'revdep'))) {
    save_path <- file.path(pkg_path, 'revdep')
  } else {
    save_path <- file.path(pkg_path, '.metadata', 'revdep')
  }
  devtools::revdep_check_save_summary(res, save_path)

  logs_path <- file.path(save_path, 'logs')
  if (!dir.exists(logs_path)) {
    dir.create(logs_path)
  }
  devtools::revdep_check_save_logs(res, logs_path)
})
")

(defun ess-project-save-buffers ()
  "Offer to save modified files in the current project.
Respects `ess-save-silently', which see."
  (let ((cur-proj ess-r-package--info-cache))
    (dolist (buf (buffer-list))
      (when-let ((file (buffer-file-name buf))
                 (buf-proj (buffer-local-value 'ess-r-package--info-cache buf)))
        (when (equal cur-proj buf-proj)
          (ess-save-file file))))))

(defun ess-r-devtools-document-package (&optional arg)
  "Interface for `devtools::document()'.
With prefix ARG ask for extra arguments."
  (interactive "P")
  (ess-r-package-eval-linewise
   "devtools::document(%s)\n" "Documenting %s" arg
   '("" (read-string "Arguments: "))))

(defun ess-r-devtools-install-package (&optional arg)
  "Interface to `devtools::install()'.
By default the installation is \"quick\" with arguments quick =
TRUE, upgrade = FALSE, build = FALSE. On prefix ARG
\\[universal-argument] install with the default
`devtools::install()' arguments."
  (interactive "P")
  (ess-r-package-eval-linewise
   "devtools::install(%s)\n" "Installing %s" arg
   '("quick = TRUE, build = FALSE, upgrade = FALSE, keep_source = TRUE"
     (read-string "Arguments: " "keep_source = TRUE, force = TRUE"))))

(defvar ess-r-devtools--install-github-history nil)
(defun ess-r-devtools-install-github (&optional arg)
  "Interface to `devtools::install_github()'.
Asks for GitHub repository in the form of user/repo. Force
re-installation when called with a prefix ARG."
  (interactive "P")
  (let ((command "devtools::install_github(%s%s)")
        (repo (format "'%s'"
                      (read-string "User/Repo: " nil
                                   'ess-r-devtools--install-github-history
                                   (car ess-r-devtools--install-github-history))))
        (args (if arg
                  (ess-r-command--build-args 0 '((read-string "Arguments: " "force = TRUE")))
                "")))
    (inferior-ess-r-force)
    (unless (derived-mode-p 'inferior-ess-mode)
      (display-buffer (ess-get-process-buffer)
                      '(nil . ((inhibit-same-window . t)))))
    (message "Installing %s from github" repo)
    (ess-eval-linewise (format command repo args))))

(defun ess-r-devtools-create-package ()
  "Interface to `devtools::create()'.
Default location is determined by the first element of
`ess-r-package-library-paths'."
  (interactive)
  (let* ((command "devtools::create(\"%s\")")
         (default-path (if (stringp ess-r-package-library-paths)
                           ess-r-package-library-paths
                         (car ess-r-package-library-paths)))
         (path (read-directory-name "Path: " default-path)))
    (ess-eval-linewise (format command path))))

(defun ess-r-devtools-execute-command (&optional arg)
  "Asks with completion for a devtools command.
When called with prefix ARG asks for additional arguments."
  (interactive "P")
  (inferior-ess-r-force)
  (let* ((devtools-funs (ess-get-words-from-vector ".ess_devtools_functions()\n"))
         (fun (completing-read "Function: " devtools-funs))
         (command (format "devtools::%s(%%s)\n" fun)))
    (ess-r-package-eval-linewise
     command (format "Running %s" fun) arg
     '("" (read-string "Arguments: ")))))


;;;*;;; Minor Mode

(defcustom ess-r-package-auto-activate t
  "If non-nil, `ess-r-package-mode' is turned on within R packages.
If 't' the minor mode auto-activates in R packages. See
`ess-r-package-exclude-modes' if you wish to inhibit
`ess-r-package-mode' in specific buffers."
  :group 'ess-r-package
  :type 'boolean)

(defcustom ess-r-package-exclude-modes '(fundamental-mode)
  "A list of modes where `ess-r-package' must not be activated.
The check is done with `derived-mode-p'."
  :group 'ess-r-package
  :type '(repeat symbol)
  :package-version '(ess "18.10"))

(defcustom ess-r-package-enter-hook nil
  "Normal hook run on entering `ess-r-package-mode'."
  :group 'ess-r-package
  :type 'hook)

(defcustom ess-r-package-exit-hook nil
  "Normal hook run on exiting `ess-r-package-mode'."
  :group 'ess-r-package
  :type 'hook)

(defcustom ess-r-package-mode-line
  ;; FIXME Emacs 25.1: Use `when-let'
  '(:eval (let ((pkg-name (ess-r-package-name)))
            (when pkg-name
              (format " [pkg:%s]" pkg-name))))
  "Mode line for ESS developer.
Set this variable to nil to disable the mode line entirely."
  :group 'ess-r-package
  :type 'sexp
  :risky t)

(defvar ess-r-package-mode-map
  (let ((ess-r-package-mode-map (make-sparse-keymap)))
    (define-key ess-r-package-mode-map "\C-c\C-w" 'ess-r-package-dev-map)
    ess-r-package-mode-map))

(define-minor-mode ess-r-package-mode
  "Minor mode for enabling R package development features.

\\{ess-r-package-mode-map}"
  :init-value nil
  :keymap ess-r-package-mode-map
  :lighter ess-r-package-mode-line
  (if ess-r-package-mode
      (progn
        ;; Forward relevant R settings for interacting with inferior
        ;; processes from any mode
        (let ((vars '(ess-dialect
                      ess-setwd-command
                      ess-getwd-command
                      ess-quit-function
                      inferior-ess-reload-function)))
          (mapc (lambda (var)
                  (set (make-local-variable var)
                       (eval (cdr (assq var ess-r-customize-alist)))))
                vars))
        (add-hook 'project-find-functions #'ess-r-package-project)
        (run-hooks 'ess-r-package-enter-hook))
    (remove-hook 'project-find-functions #'ess-r-package-project)
    (run-hooks 'ess-r-package-exit-hook)))

(add-hook 'after-change-major-mode-hook 'ess-r-package-auto-activate)


;;;*;;; Activation

(defun ess-r-package-auto-activate ()
  "Activate developer if current file is part of a package."
  (when (and ess-r-package-auto-activate
             (or (buffer-name) default-directory)
             (not (eq major-mode 'minibuffer-inactive-mode))
             (or
              ;; users probably have these in fundamental mode
              (member (buffer-name) '("DESCRIPTION" "NAMESPACE"))
              (if ess-r-package-exclude-modes
                  (not (apply #'derived-mode-p ess-r-package-exclude-modes))
                t)))
    (when (car (ess-r-package-info))
      (ess-r-package-mode 1))))

(defun ess-r-package-re-activate ()
  "Restart `ess-r-package-mode'.
First, deactivate package mode if active, and activate if in
package mode. Use this function if state of the buffer such as
`default-directory' has changed."
  (when ess-r-package-mode
    (ess-r-package-mode -1))
  (setq ess-r-package--info-cache nil)
  (ess-r-package-auto-activate))

(defvar-local ess-r--old-default-dir nil)
(defun ess-r-package-default-directory-tracker (&rest _)
  (unless (equal ess-r--old-default-dir default-directory)
    (setq ess-r--old-default-dir default-directory)
    (ess-r-package-re-activate)))

(defun ess-r-package-activate-directory-tracker ()
  (add-hook 'after-change-functions 'ess-r-package-default-directory-tracker t t))

(add-hook 'shell-mode-hook 'ess-r-package-activate-directory-tracker t)
(add-hook 'eshell-mode-hook 'ess-r-package-activate-directory-tracker t)
(when (fboundp 'advice-add)
  (require 'shell)
  (advice-add 'shell-resync-dirs :after 'ess-r-package-re-activate))


;;;*;;; Deprecated variables and functions
(defun ess-developer (&optional _val)
  (error "As of ESS 16.04, `ess-developer' is deprecated. Use `ess-r-set-evaluation-env' instead"))

(defalias 'ess-toggle-developer 'ess-developer)
(define-obsolete-function-alias 'ess-r-devtools-check-package-buildwin 'ess-r-devtools-check-with-winbuilder)
(define-obsolete-function-alias 'ess-r-devtools-ask 'ess-r-devtools-execute-command "18.04")

(make-obsolete-variable 'ess-developer "Please use `ess-developer-select-package' and `ess-r-set-evaluation-env' instead." "16.04")
(make-obsolete-variable 'ess-developer-root-file "Please use `ess-r-package-root-file' instead." "16.04")
(make-obsolete-variable 'ess-developer-packages "Please use `ess-r-package-set-package' and `ess-r-set-evaluation-env' instead." "16.04")
(make-obsolete-variable 'ess-developer-load-on-add-commands "Please use `ess-r-package-set-package' and `ess-r-set-evaluation-env' instead." "16.04")
(make-obsolete-variable 'ess-developer-activate-in-package "Please use `ess-r-package-auto-activate' instead." "16.04")
(make-obsolete-variable 'ess-developer-enter-hook "Please use `ess-r-package-enter-hook' instead." "16.04")
(make-obsolete-variable 'ess-developer-exit-hook "Please use `ess-r-package-exit-hook' instead." "16.04")


(provide 'ess-r-package)

;;; ess-r-package.el ends here
