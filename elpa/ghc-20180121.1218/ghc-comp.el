;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-comp.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009
;; License: BSD-3-clause

;;; Code:

(require 'ghc-func)
(require 'ghc-rewrite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Variables
;;;

(defvar ghc-idle-timer-interval 30
 "*Period of idle timer in second. When timeout, the names of
unloaded modules are loaded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constants
;;;

;; must be sorted
(defconst ghc-reserved-keyword-for-bol '("class" "data" "default" "import" "infix" "infixl" "infixr" "instance" "main" "module" "newtype" "type"))

;; must be sorted
(defconst ghc-reserved-keyword '("case" "deriving" "do" "else" "if" "in" "let" "module" "of" "then" "where"))

(defconst ghc-extra-keywords '("ByteString" "Text"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local Variables
;;;

(defvar ghc-window-configuration nil)

(mapc 'make-variable-buffer-local
      '(ghc-window-configuration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar ghc-module-names nil)   ;; completion for "import"
(defvar ghc-merged-keyword nil) ;; completion for type/func/...
(defvar ghc-language-extensions nil)
(defvar ghc-option-flags nil)
(defvar ghc-pragma-names '("LANGUAGE" "OPTIONS_GHC" "INCLUDE" "WARNING" "DEPRECATED" "INLINE" "NOINLINE" "ANN" "LINE" "RULES" "SPECIALIZE" "UNPACK" "SOURCE"))

(defconst ghc-keyword-prefix "ghc-keyword-")
(defvar ghc-keyword-Prelude nil)
(defvar ghc-keyword-Control.Applicative nil)
(defvar ghc-keyword-Control.Exception nil)
(defvar ghc-keyword-Control.Monad nil)
(defvar ghc-keyword-Data.Char nil)
(defvar ghc-keyword-Data.List nil)
(defvar ghc-keyword-Data.Maybe nil)
(defvar ghc-keyword-System.IO nil)

(defvar ghc-loaded-module nil)

(defun ghc-comp-init ()
  (let* ((syms '(ghc-module-names
		 ghc-language-extensions
		 ghc-option-flags
		 ;; hard coded in GHCMod.hs
		 ghc-keyword-Prelude
		 ghc-keyword-Control.Applicative
		 ghc-keyword-Control.Exception
		 ghc-keyword-Control.Monad
		 ghc-keyword-Data.Char
		 ghc-keyword-Data.List
		 ghc-keyword-Data.Maybe
		 ghc-keyword-System.IO))
	 (vals (ghc-boot (length syms))))
    (ghc-set syms vals))
  (ghc-add ghc-module-names "qualified")
  (ghc-add ghc-module-names "hiding")
  ;; hard coded in GHCMod.hs
  (ghc-merge-keywords '("Prelude"
			"Control.Applicative"
			"Control.Exception"
			"Control.Monad"
			"Data.Char"
			"Data.List"
			"Data.Maybe"
			"System.IO")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing command
;;;

(defun ghc-boot (n)
  (prog2
      (message "Initializing...")
      (ghc-sync-process "boot\n" n)
    (message "Initializing...done")))

(defun ghc-load-modules (mods)
  (if mods
      (mapcar 'ghc-load-module mods)
    (message "No new modules")
    nil))

(defun ghc-load-module (mod)
  (prog2
      (message "Loading symbols for %s..." mod)
      (ghc-sync-process (format "browse %s\n" mod))
    (message "Loading symbols for %s...done" mod)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion
;;;

(defvar ghc-completion-buffer-name "*Completions*")

(defun ghc-complete ()
  (interactive)
  (if (ghc-should-scroll)
      (ghc-scroll-completion-buffer)
    (ghc-try-complete)))

(defun ghc-should-scroll ()
  (let ((window (ghc-completion-window)))
    (and (eq last-command this-command)
	 window (window-live-p window) (window-buffer window)
	 (buffer-name (window-buffer window)))))

(defun ghc-scroll-completion-buffer ()
  (let ((window (ghc-completion-window)))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
	  (set-window-start window (point-min))
	(save-selected-window
	  (select-window window)
	  (scroll-up))))))

(defun ghc-completion-window ()
  (get-buffer-window ghc-completion-buffer-name 0))

(defun ghc-try-complete ()
  (let* ((end (point))
	 (symbols (ghc-select-completion-symbol))
	 (beg (ghc-completion-start-point))
	 (pattern (buffer-substring-no-properties beg end))
	 (completion (try-completion pattern symbols)))
    (cond
     ((eq completion t) ;; completed
      ) ;; do nothing
     ((null completion) ;; no completions
      (ding))
     ((not (string= pattern completion)) ;; ???
      (delete-region beg end)
      (insert completion)
      (ghc-reset-window-configuration))
     (t ;; multiple completions
      (let* ((list0 (all-completions pattern symbols))
	     (list (sort list0 'string<)))
	(if (= (length list) 1)
	    (ghc-reset-window-configuration)
	  (ghc-save-window-configuration)
	  (with-output-to-temp-buffer ghc-completion-buffer-name
	    (display-completion-list list))))))))

(defun ghc-save-window-configuration ()
  (unless (get-buffer-window ghc-completion-buffer-name)
    (setq ghc-window-configuration (current-window-configuration))))

(defun ghc-reset-window-configuration ()
  (when ghc-window-configuration
    (set-window-configuration ghc-window-configuration)
    (setq ghc-window-configuration nil)))

(defun ghc-module-completion-p ()
  (or (minibufferp)
      (let ((end (point)))
	(save-excursion
	  (beginning-of-line)
	  (and (looking-at "import ")
	       (not (search-forward "(" end t)))))
      (save-excursion
	(beginning-of-line)
	(looking-at " +module "))))

(defun ghc-select-completion-symbol ()
  (cond
   ((ghc-module-completion-p)
    ghc-module-names)
   ((save-excursion
      (beginning-of-line)
      (looking-at "{-# LANGUAGE "))
    ghc-language-extensions)
   ((save-excursion
      (beginning-of-line)
      (looking-at "{-# OPTIONS_GHC "))
    ghc-option-flags)
   ((save-excursion
      (beginning-of-line)
      (looking-at "{-# "))
    ghc-pragma-names)
   ((or (bolp)
	(let ((end (point)))
	  (save-excursion
	    (beginning-of-line)
	    (not (search-forward " " end t)))))
    ghc-reserved-keyword-for-bol)
   (t ghc-merged-keyword)))

(defun ghc-completion-start-point ()
  (save-excursion
    (let ((beg (save-excursion (beginning-of-line) (point)))
	  (regex (if (ghc-module-completion-p) "[ (,`]" "[\[ (,`.]")))
      (if (re-search-backward regex beg t)
	  (1+ (point))
	beg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loading keywords
;;;

(defun ghc-import-module ()
  (interactive)
  (ghc-load-module-buffer))

(defun ghc-unloaded-modules (mods)
  (ghc-filter (lambda (mod)
		(and (member mod ghc-module-names)
		     (not (member mod ghc-loaded-module))))
	      mods))

(defun ghc-load-module-buffer ()
  (ghc-load-merge-modules (ghc-gather-import-modules-buffer)))

(defun ghc-load-merge-modules (mods)
  (let* ((umods (ghc-unloaded-modules mods))
	 (syms (mapcar 'ghc-module-symbol umods))
	 (names (ghc-load-modules umods)))
    (ghc-set syms names)
    (ghc-merge-keywords umods)))

(defun ghc-merge-keywords (mods)
  (setq ghc-loaded-module (append mods ghc-loaded-module))
  (let* ((modkeys (mapcar 'ghc-module-keyword ghc-loaded-module))
	 (keywords (cons ghc-extra-keywords (cons ghc-reserved-keyword modkeys)))
	 (uniq-sorted (sort (ghc-uniq-lol keywords) 'string<)))
    (setq ghc-merged-keyword uniq-sorted)))

(defun ghc-module-symbol (mod)
  (intern (concat ghc-keyword-prefix mod)))

(defun ghc-module-keyword (mod)
  (symbol-value (ghc-module-symbol mod)))

(defun ghc-gather-import-modules-buffer ()
  (let (ret)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^import +\\(qualified\\)? *\\([^\n ]+\\)" nil t)
	(ghc-add ret (match-string-no-properties 2))
	(forward-line)))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Background Idle Timer
;;;

(provide 'ghc-comp)
