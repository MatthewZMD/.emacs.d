;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-ins-mod.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 27, 2011
;; License: BSD-3-clause

(require 'ghc-process)

;;; Code:

(defun ghc-insert-module ()
  (interactive)
  (let* ((expr0 (ghc-things-at-point))
	 (expr (ghc-read-expression expr0)))
    (ghc-ins-mod expr)))

(defvar ghc-preferred-modules '("Control.Applicative"
				"Data.ByteString"
				"Data.Text"
				"Text.Parsec"
				"System.FilePath"
				"System.Directory"))

(defun ghc-reorder-modules (mods)
  (catch 'loop
    (dolist (pmod ghc-preferred-modules)
      (if (member pmod mods)
	  (throw 'loop (cons pmod (delete pmod mods)))))
    mods))

(defun ghc-ins-mod (expr)
  (let (prefix fun mods)
    (if (not (string-match "^\\([^.]+\\)\\\.\\([^.]+\\)$" expr))
	(setq fun expr)
      (setq prefix (match-string 1 expr))
      (setq fun (match-string 2 expr)))
    (setq mods (ghc-reorder-modules (ghc-function-to-modules fun)))
    (if (null mods)
	(message "No module guessed")
      (let* ((key (or prefix fun))
	     (fmt (concat "Module name for \"" key "\" (%s): "))
	     (mod (ghc-completing-read fmt mods)))
	(save-excursion
	  (ghc-goto-module-position)
	  (if prefix
	      (insert-before-markers "import qualified " mod " as " prefix "\n")
	    (insert-before-markers "import " mod " (" (ghc-enclose expr) ")\n")))))))

(defun ghc-completing-read (fmt lst)
  (let* ((def (car lst))
	 (prompt (format fmt def))
	 (inp (completing-read prompt lst)))
    (if (string= inp "") def inp)))

(defun ghc-goto-module-position ()
  (goto-char (point-max))
  (if (re-search-backward "^import +" nil t)
      (ghc-goto-empty-line)
    (if (not (re-search-backward "^module" nil t))
	(goto-char (point-min))
      (ghc-goto-empty-line)
      (forward-line)
      (unless (eolp)
	;; save-excursion is not proper due to insert-before-markers.
	(let ((beg (point)))
	  (insert-before-markers "\n")
	  (goto-char beg))))))

(defun ghc-goto-empty-line ()
  (unless (re-search-forward "^$" nil t)
    (forward-line)))

(defun ghc-function-to-modules (fun)
  (let ((cmd (format "find %s\n" fun)))
    (ghc-sync-process cmd)))

(provide 'ghc-ins-mod)
