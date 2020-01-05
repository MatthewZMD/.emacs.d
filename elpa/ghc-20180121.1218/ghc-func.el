;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-func.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009
;; License: BSD-3-clause

;;; Code:

(defvar ghc-module-command "ghc-mod"
"*The command name of \"ghc-mod\"")

(defvar ghc-ghc-options nil "*GHC options")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-replace-character (string from to)
  "Replace characters equal to FROM to TO in STRING."
  (let ((ret (copy-sequence string)))
    (dotimes (cnt (length ret))
      (if (char-equal (aref ret cnt) from)
	  (aset ret cnt to)))
    ret))

(defun ghc-replace-character-buffer (from-c to-c)
  (let ((from (char-to-string from-c))
	(to (char-to-string to-c)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward from nil t)
	(replace-match to)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-unescape-string (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward "\\n" nil t) (replace-match "\n" nil t))
    (goto-char (point-min))
    (while (search-forward "\\\\" nil t) (replace-match "\\" nil t))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ghc-add (sym val)
  `(setq ,sym (cons ,val ,sym)))

(defun ghc-set (vars vals)
  (dolist (var vars)
    (if var (set var (car vals))) ;; var can be nil to skip
    (setq vals (cdr vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-filter (pred lst)
  (let (ret)
    (dolist (x lst (reverse ret))
      (if (funcall pred x) (ghc-add ret x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-uniq-lol (lol)
  (let ((hash (make-hash-table :test 'equal))
	ret)
    (dolist (lst lol)
      (dolist (key lst)
	(puthash key key hash)))
    (maphash (lambda (key _val) (ghc-add ret key)) hash)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-read-lisp (func)
  (with-temp-buffer
    (funcall func)
    (ghc-read-lisp-this-buffer)))

;; OK/NG are ignored.
(defun ghc-read-lisp-this-buffer ()
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
	(read (current-buffer))
      (error ()))))

(defun ghc-read-lisp-list-this-buffer (n)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
	(let ((m (set-marker (make-marker) 1 (current-buffer)))
	      ret)
	  (dotimes (_i n)
	    (ghc-add ret (read m)))
	  (nreverse ret))
      (error ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-mapconcat (func list)
  (apply 'append (mapcar func list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-things-at-point ()
  (thing-at-point 'sexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-keyword-number-pair (spec)
  (let ((len (length spec)) key ret)
    (dotimes (i len)
      (setq key (intern (concat ":" (symbol-name (car spec)))))
      (setq ret (cons (cons key i) ret))
      (setq spec (cdr spec)))
    (nreverse ret)))

(defmacro ghc-defstruct (type &rest spec)
  `(progn
     (ghc-defstruct-constructor ,type ,@spec)
     (ghc-defstruct-s/getter ,type ,@spec)))

(defmacro ghc-defstruct-constructor (type &rest spec)
  `(defun ,(intern (concat "ghc-make-" (symbol-name type))) (&rest args)
     (let* ((alist (quote ,(ghc-keyword-number-pair spec)))
	    (struct (make-list (length alist) nil))
	    key val key-num)
       (while args ;; cannot use dolist
	 (setq key  (car args))
	 (setq args (cdr args))
	 (setq val  (car args))
	 (setq args (cdr args))
	 (unless (keywordp key)
	   (error "'%s' is not a keyword" key))
	 (setq key-num (assoc key alist))
	 (if key-num
	     (setcar (nthcdr (cdr key-num) struct) val)
	   (error "'%s' is unknown" key)))
       struct)))

(defmacro ghc-defstruct-s/getter (type &rest spec)
  `(let* ((type-name (symbol-name ',type))
	  (keys ',spec)
	  (len (length keys))
	  member-name setter getter)
     (dotimes (i len)
       (setq member-name (symbol-name (car keys)))
       (setq setter (intern (format "ghc-%s-set-%s" type-name member-name)))
       (fset setter (list 'lambda '(struct value) (list 'setcar (list 'nthcdr i 'struct) 'value) 'struct))
       (setq getter (intern (format "ghc-%s-get-%s" type-name member-name)))
       (fset getter (list 'lambda '(struct) (list 'nth i 'struct)))
       (setq keys (cdr keys)))))

(defun ghc-make-ghc-options ()
  (ghc-mapconcat (lambda (x) (list "-g" x)) ghc-ghc-options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ghc-error-buffer-name "*GHC Info*")

(defun ghc-display (fontify ins-func)
  (ghc-display-with-name fontify ins-func ghc-error-buffer-name))

;; (defun ghc-display (fontify ins-func)
;;   (let ((buf ghc-error-buffer-name))
;;     (with-output-to-temp-buffer buf
;;       (with-current-buffer buf
;;         (erase-buffer)
;;         (funcall ins-func)
;;         (goto-char (point-min))
;;         (if (not fontify)
;;             (turn-off-haskell-font-lock)
;;           (haskell-font-lock-defaults-create)
;;           (turn-on-haskell-font-lock)))
;;       (display-buffer buf
;;         '((display-buffer-reuse-window
;;            display-buffer-pop-up-window))))))

(defun ghc-display-with-name (fontify ins-func name)
  (let ((buf name))
    (with-output-to-temp-buffer buf
      (with-current-buffer buf
        (erase-buffer)
        (funcall ins-func)
        (goto-char (point-min))
        (if (not fontify)
            ;; turn-off-haskell-font-lock has been removed from haskell-mode
            ;; test if the function is defined in our version
            (if (fboundp 'turn-off-haskell-font-lock)
                (turn-off-haskell-font-lock)
              ;; it's not defined, fallback on font-lock-mode
              (font-lock-mode -1))
          (haskell-font-lock-defaults-create)
          ;; turn-on-haskell-font-lock has been removed from haskell-mode
          ;; test if the function is defined in our version
          (if (fboundp 'turn-on-haskell-font-lock)
              (turn-on-haskell-font-lock)
            ;; it's not defined, fallback on font-lock-mode
            (turn-on-font-lock))))
      (display-buffer buf
        '((display-buffer-reuse-window
           display-buffer-pop-up-window))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-run-ghc-mod (cmds &optional prog)
  (let ((target (or prog ghc-module-command)))
    (ghc-executable-find target
      (let ((cdir (or ghc-process-root  ;; ghc-mod version/debug
		      default-directory))) ;; ghc-mod root
	(with-temp-buffer
	  (let ((default-directory cdir))
	    (apply 'ghc-call-process target nil '(t nil) nil
		   (append (ghc-make-ghc-options) cmds))
	    (buffer-substring (point-min) (1- (point-max)))))))))

(defmacro ghc-executable-find (cmd &rest body)
  ;; (declare (indent 1))
  `(if (not (executable-find ,cmd))
       (message "\"%s\" not found" ,cmd)
     ,@body))

(put 'ghc-executable-find 'lisp-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-debug nil)

(defvar ghc-debug-buffer "*GHC Debug*")

(defmacro ghc-with-debug-buffer (&rest body)
  `(with-current-buffer (set-buffer (get-buffer-create ghc-debug-buffer))
     (goto-char (point-max))
     ,@body))

(defun ghc-call-process (cmd x y z &rest args)
  (apply 'call-process cmd x y z args)
  (when ghc-debug
    (let ((cbuf (current-buffer)))
      (ghc-with-debug-buffer
       (insert (format "%% %s %s\n" cmd (mapconcat 'identity args " ")))
       (insert-buffer-substring cbuf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-enclose (expr)
  (let ((case-fold-search nil))
    (if (string-match "^[a-zA-Z0-9_]" expr)
	expr
      (concat "(" expr ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ghc-with-current-buffer (buf &rest body)
  ;; (declare (indent 1))
  `(if (buffer-live-p ,buf)
       (with-current-buffer ,buf
	 ,@body)))

(provide 'ghc-func)
