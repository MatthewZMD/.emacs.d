;;; ghc.el --- ghc-mod front-end for haskell-mode

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009
;; Revised:
;; License: BSD-3-clause

;; Put the following code to your "~/.emacs".
;;
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;;
;; Or if you wish to display error each goto next/prev error,
;; set ghc-display-error valiable.
;;
;; (setq ghc-display-error 'minibuffer) ; to minibuffer
;; ; (setq ghc-display-error 'other-buffer) ; to other-buffer

;;

;;; Code:

;; defvar-local was introduced in 24.3
(let* ((major 24)
       (minor 3))
  (if (or (< emacs-major-version major)
	  (and (= emacs-major-version major)
	       (< emacs-minor-version minor)))
      (error "ghc-mod requires at least Emacs %d.%d" major minor)))

(defconst ghc-version "5.8.0.0")

(defgroup ghc-mod '() "ghc-mod customization")

;; (eval-when-compile
;;  (require 'haskell-mode))

(require 'ghc-comp)
(require 'ghc-doc)
(require 'ghc-info)
(require 'ghc-check)
(require 'ghc-command)
(require 'ghc-ins-mod)
(require 'ghc-indent)
(require 'ghc-rewrite)
(require 'dabbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Variables
;;;

(defun ghc-find-C-h ()
  (or
   (when keyboard-translate-table
     (aref keyboard-translate-table ?\C-h))
   ?\C-h))

(defvar ghc-completion-key  "\e\t")
(defvar ghc-document-key    "\e\C-d")
(defvar ghc-import-key      "\e\C-m")
(defvar ghc-previous-key    "\ep")
(defvar ghc-next-key        "\en")
(defvar ghc-help-key        "\e?")
(defvar ghc-insert-key      "\et")
(defvar ghc-sort-key        "\es")
(defvar ghc-type-key        "\C-c\C-t")
(defvar ghc-info-key        "\C-c\C-i")
(defvar ghc-toggle-key      "\C-c\C-c")
(defvar ghc-jump-key        "\C-c\C-j")
(defvar ghc-module-key      "\C-c\C-m")
(defvar ghc-expand-key      "\C-c\C-e")
(defvar ghc-kill-key        "\C-c\C-k")
(defvar ghc-hoogle-key      (format "\C-c%c" (ghc-find-C-h)))
(defvar ghc-shallower-key   "\C-c<")
(defvar ghc-deeper-key      "\C-c>")
;(defvar ghc-case-split-key  "\C-c\C-s")
(defvar ghc-refine-key      "\C-c\C-f")
(defvar ghc-auto-key        "\C-c\C-a")
(defvar ghc-prev-hole-key   "\C-c\ep")
(defvar ghc-next-hole-key   "\C-c\en")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar ghc-initialized nil)

;;;###autoload
(defun ghc-init ()
  (ghc-abbrev-init)
  (ghc-type-init)
  (unless ghc-initialized
    (define-key haskell-mode-map ghc-completion-key  'ghc-complete)
    (define-key haskell-mode-map ghc-document-key    'ghc-browse-document)
    (define-key haskell-mode-map ghc-type-key        'ghc-show-type)
    (define-key haskell-mode-map ghc-info-key        'ghc-show-info)
    (define-key haskell-mode-map ghc-expand-key      'ghc-expand-th)
    (define-key haskell-mode-map ghc-import-key      'ghc-import-module)
    (define-key haskell-mode-map ghc-previous-key    'ghc-goto-prev-error)
    (define-key haskell-mode-map ghc-next-key        'ghc-goto-next-error)
    (define-key haskell-mode-map ghc-help-key        'ghc-display-errors)
    (define-key haskell-mode-map ghc-insert-key      'ghc-insert-template-or-signature)
    (define-key haskell-mode-map ghc-sort-key        'ghc-sort-lines)
    (define-key haskell-mode-map ghc-toggle-key      'ghc-toggle-check-command)
    (define-key haskell-mode-map ghc-jump-key        'ghc-jump-file)
    (define-key haskell-mode-map ghc-module-key      'ghc-insert-module)
    (define-key haskell-mode-map ghc-kill-key        'ghc-kill-process)
    (define-key haskell-mode-map ghc-hoogle-key      'haskell-hoogle)
    (define-key haskell-mode-map ghc-shallower-key   'ghc-make-indent-shallower)
    (define-key haskell-mode-map ghc-deeper-key      'ghc-make-indent-deeper)
    ;(define-key haskell-mode-map ghc-case-split-key  'ghc-case-split)
    (define-key haskell-mode-map ghc-refine-key      'ghc-refine)
    (define-key haskell-mode-map ghc-auto-key        'ghc-auto)
    (define-key haskell-mode-map ghc-prev-hole-key   'ghc-goto-prev-hole)
    (define-key haskell-mode-map ghc-next-hole-key   'ghc-goto-next-hole)
    (ghc-comp-init)
    (setq ghc-initialized t)
    (add-hook 'kill-buffer-hook 'ghc-kill-process)
    (defadvice save-buffer (after ghc-check-syntax-on-save activate)
      "Check syntax with GHC when a haskell-mode buffer is saved."
      (when (eq 'haskell-mode major-mode) (ghc-check-syntax))))
  (ghc-import-module)
  (ghc-check-syntax))

(defun ghc-abbrev-init ()
  (set (make-local-variable 'dabbrev-case-fold-search) nil))

;;;###autoload
(defun ghc-debug ()
  (interactive)
  (let ((el-path (locate-file "ghc.el" load-path))
	(ghc-path (executable-find "ghc")) ;; FIXME
	(ghc-mod-path (executable-find ghc-module-command))
	(el-ver ghc-version)
	(ghc-ver (ghc-run-ghc-mod '("--version") "ghc"))
	(ghc-mod-ver (ghc-run-ghc-mod '("version")))
	(path (getenv "PATH"))
	(debug (ghc-run-ghc-mod '("debug")))) ;; before switching buffers.
    (switch-to-buffer (get-buffer-create "**GHC Debug**"))
    (erase-buffer)
    (insert "Path: check if you are using intended programs.\n")
    (insert (format "\t  ghc.el path: %s\n" el-path))
    (insert (format "\t ghc-mod path: %s\n" ghc-mod-path))
    (insert (format "\t     ghc path: %s\n" ghc-path))
    (insert "\nVersion: all GHC versions must be the same.\n")
    (insert (format "\t  ghc.el version %s\n" el-ver))
    (insert (format "\t %s\n" ghc-mod-ver))
    (insert (format "\t%s\n" ghc-ver))
    (insert "\nEnvironment variables:\n")
    (insert (format "\tPATH=%s\n" path))
    (insert "\nThe result of \"ghc-mod debug\":\n")
    (insert debug)
    (goto-char (point-min))))

(defun ghc-insert-template-or-signature (&optional flag)
  (interactive "P")
  (if flag
      (ghc-initial-code-from-signature)
    (ghc-insert-template)))

(provide 'ghc)
