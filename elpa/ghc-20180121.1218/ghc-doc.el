;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009
;; License: BSD-3-clause

(require 'ghc-func)
(require 'ghc-comp)
(require 'ghc-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Variables
;;;

(defcustom ghc-doc-browser-function #'browse-url
  "Function used to browse documentation."
  :type '(radio (function-item browse-url)
                (function-item ghc-browse-url-safari))
  :group 'ghc-mod)

;;; Code:

(defun ghc-browse-document (&optional haskell-org)
  (interactive "P")
  (let ((mod0 (ghc-extract-module))
	(expr0 (ghc-things-at-point))
	pkg-ver-path mod expr info)
    (if (or mod0 (not expr0))
	(setq mod (ghc-read-module-name mod0))
      (setq expr (ghc-read-expression expr0))
      (setq info (ghc-get-info expr0))
      (setq mod (ghc-extact-module-from-info info)))
    (setq pkg-ver-path (and mod (ghc-resolve-document-path mod)))
    (if pkg-ver-path
	(ghc-display-document pkg-ver-path mod haskell-org expr)
      (message "No documentation found"))))

(ghc-defstruct pkg-ver-path pkg ver path)

(defun ghc-resolve-document-path (mod)
  (let ((root ghc-process-root))
    (with-temp-buffer
      (let ((default-directory root))
	(ghc-call-process ghc-module-command nil t nil "doc" mod))
      (goto-char (point-min))
      (when (looking-at "^\\([^ ]+\\)-\\([0-9]*\\(\\.[0-9]+\\)*\\) \\(.*\\)$")
	(ghc-make-pkg-ver-path
	 :pkg (match-string-no-properties 1)
	 :ver (match-string-no-properties 2)
	 :path (match-string-no-properties 4))))))

(defconst ghc-doc-local-format "file://%s/%s.html")
(defconst ghc-doc-hackage-format
  "http://hackage.haskell.org/packages/archive/%s/%s/doc/html/%s.html")

(defun ghc-browse-url-safari (uri &rest _args)
"Open a URI in Safari using AppleScript. This preserves anchors."
  (let ((script (format "
tell application \"Safari\"
  open location \"%s\"
  activate
end tell" uri)))
    (do-applescript script)))

(defun ghc-display-document (pkg-ver-path mod haskell-org &optional symbol)
  (let* ((pkg  (ghc-pkg-ver-path-get-pkg pkg-ver-path))
         (mod- (ghc-replace-character mod ?. ?-))
	 (ver  (ghc-pkg-ver-path-get-ver pkg-ver-path))
	 (path (ghc-pkg-ver-path-get-path pkg-ver-path))
	 (local (format ghc-doc-local-format path mod-))
	 (remote (format ghc-doc-hackage-format pkg ver mod-))
	 (file (format "%s/%s.html" path mod-))
	 (url0 (if (or haskell-org (not (file-exists-p file))) remote local))
	 (url (if symbol (ghc-add-anchor url0 symbol) url0)))
    (funcall ghc-doc-browser-function url)))

(defun ghc-add-anchor (url symbol)
  (let ((case-fold-search nil))
    (if (string-match "^[A-Z]" symbol)
	(concat url "#t:" symbol)
      (if (string-match "^[a-z]" symbol)
	  (concat url "#v:" symbol)
	(concat url "#v:" (ghc-url-encode symbol))))))

(defun ghc-url-encode (symbol)
  (let ((len (length symbol))
	(i 0)
	acc)
    (while (< i len)
      (ghc-add acc (format "-%d-" (aref symbol i)))
      (setq i (1+ i)))
    (apply 'concat (nreverse acc))))

(defun ghc-extact-module-from-info (info)
  (when (string-match "[`\u2018]\\([^'\u2019]+\\)['\u2019]" info)
    (match-string 1 info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-input-map nil)

(unless ghc-input-map
  (setq ghc-input-map
	(if (boundp 'minibuffer-local-map)
	    (copy-keymap minibuffer-local-map)
	  (make-sparse-keymap)))
  (define-key ghc-input-map "\t" 'ghc-complete))

(defun ghc-read-module-name (def)
  (read-from-minibuffer "Module name: " def ghc-input-map))

(defun ghc-read-expression (def)
  (read-from-minibuffer "Identifier: " def ghc-input-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-extract-module ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\(import\\|module\\) +\\(qualified +\\)?\\([^ (\n]+\\)")
	(match-string-no-properties 3))))

(provide 'ghc-doc)
