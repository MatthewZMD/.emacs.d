;;; tex-info.el --- Support for editing Texinfo source.

;; Copyright (C) 1993, 1994, 1997, 2000, 2001, 2004, 2005, 2006,
;;               2011-2015, 2017-2019  Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'tex)

(require 'texinfo)

;;; Environments:
(defvar Texinfo-environment-list
  '(("cartouche") ("command") ("copying") ("defcv") ("deffn") ("defivar")
    ("defmac") ("defmethod") ("defop") ("defopt") ("defspec")
    ("deftp") ("deftypefn") ("deftypefun") ("deftypevar") ("deftypevr")
    ("defun") ("defvar") ("defvr") ("description") ("detailmenu")
    ("direntry") ("display") ("documentdescription") ("enumerate")
    ("example") ("float") ("flushleft") ("flushright") ("format") ("ftable")
    ("group") ("html") ("ifclear") ("ifdocbook") ("ifhtml") ("ifinfo")
    ("ifnotdocbook") ("ifnothtml") ("ifnotinfo") ("ifnotplaintext")
    ("ifnottex") ("ifnotxml") ("ifplaintext") ("ifset") ("iftex")
    ("ifxml") ("ignore") ("itemize") ("lisp") ("macro") ("menu")
    ("multitable") ("quotation") ("smalldisplay") ("smallexample")
    ("smallformat") ("smalllisp") ("table") ("tex") ("titlepage")
    ("verbatim") ("vtable"))
  "Alist of Texinfo environments.")

(defconst texinfo-environment-regexp
  ;; Overwrite version from `texinfo.el'.
  (concat "^@\\("
	  (mapconcat 'car Texinfo-environment-list "\\|")
	  "\\|end\\)\\>")
  "Regexp for environment-like Texinfo list commands.
Subexpression 1 is what goes into the corresponding `@end' statement.")

(defun Texinfo-environment (env &optional arg)
  "Make Texinfo environment ENV.
With optional ARG, modify current environment."
  ;; XXX: This could be enhanced to act like `LaTeX-environment',
  ;; i.e. suggest a default environment and have its own history.
  (interactive (list (completing-read "Environment: "
				      Texinfo-environment-list)
		     current-prefix-arg))
  (if arg
      (Texinfo-modify-environment env)
    (Texinfo-insert-environment env)))

(defun Texinfo-insert-environment (env)
  "Insert Texinfo environment ENV."
  (if (and (TeX-active-mark)
	   (not (eq (mark) (point))))
      (progn
	(when (< (mark) (point))
	  (exchange-point-and-mark))
	(unless (TeX-looking-at-backward "^[ \t]*")
	  (newline))
	(insert "@" env)
	(newline)
	(goto-char (mark))
	(unless (TeX-looking-at-backward "^[ \t]*")
	  (newline))
	(insert "@end " env)
	(save-excursion (newline))
	(end-of-line 0))
    (insert "@" env "\n\n@end " env "\n")
    (if (null (cdr-safe (assoc "defcv" Texinfo-environment-list)))
	(forward-line -2))))

(defun Texinfo-modify-environment (env)
  "Change current environment to environment ENV."
  (save-excursion
    (Texinfo-find-env-end)
    (re-search-backward (concat (regexp-quote TeX-esc) "end \\([a-zA-Z]*\\)")
			(line-beginning-position))
    (replace-match env t t nil 1)
    (beginning-of-line)
    (Texinfo-find-env-start)
    (re-search-forward (concat (regexp-quote TeX-esc) "\\([a-zA-Z]*\\)")
		       (line-end-position))
    (replace-match env t t nil 1)))

(defun Texinfo-find-env-end ()
  "Move point to the end of the current environment."
  (interactive)
  (let* ((envs (mapcar 'car Texinfo-environment-list))
	 (regexp (concat "^[ \t]*" (regexp-quote TeX-esc) "\\(end \\)*"
			 (regexp-opt envs t) "\\b"))
	 (orig-pos (point))
	 (level 1)
	 case-fold-search)
    (save-restriction
      (save-excursion
	(save-excursion
	  (beginning-of-line)
	  ;; Stop if point is inside of an @end <env> command, but not
	  ;; if it is behind it.
	  (when (and (looking-at regexp)
		     (match-string 1)
		     (> (match-end 0) orig-pos))
	    (setq level 0)))
	(while (and (> level 0) (re-search-forward regexp nil t))
	  (if (match-string 1)
	      (setq level (1- level))
	    (setq level (1+ level)))))
      (if (= level 0)
	  (goto-char (match-end 0))
	(error "Can't locate end of current environment")))))

(defun Texinfo-find-env-start ()
  "Move point to the start of the current environment."
  (interactive)
  (let* ((envs (mapcar 'car Texinfo-environment-list))
	 (regexp (concat "^[ \t]*\\(" (regexp-quote TeX-esc) "\\)\\(end \\)*"
			 (regexp-opt envs t) "\\b"))
	 (level 1)
	 (orig-pos (point))
	 case-fold-search)
    (save-restriction
      (save-excursion
	(save-excursion
	  (beginning-of-line)
	  ;; Stop if point is inside of an @<env> command, but not if
	  ;; it is before it.
	  (when (and (looking-at regexp)
		     (not (match-string 2))
		     (< (match-beginning 1) orig-pos))
	    (setq level 0)))
	(while (and (> level 0) (re-search-backward regexp nil t))
	  (if (match-string 2)
	      (setq level (1+ level))
	    (setq level (1- level)))))
      (if (= level 0)
	  (goto-char (match-beginning 0))
	(error "Can't locate start of current environment")))))

(defun Texinfo-mark-environment (&optional count)
  "Set mark to end of current environment and point to the matching begin.
If prefix argument COUNT is given, mark the respective number of
enclosing environments.  The command will not work properly if
there are unbalanced begin-end pairs in comments and verbatim
environments."
  ;; TODO:
  ;; This is identical to the LaTeX counterpart but for the find begin/end
  ;; functions. So some day the implemenation should be factorized.
  (interactive "p")
  (setq count (if count (abs count) 1))
  (let ((cur (point)) beg end)
    ;; Only change point and mark after beginning and end were found.
    ;; Point should not end up in the middle of nowhere if the search fails.
    (save-excursion
      (dotimes (c count)
	(Texinfo-find-env-end))
      (setq end (line-beginning-position 2))
      (goto-char cur)
      (dotimes (c count)
	(Texinfo-find-env-start)
	(unless (= (1+ c) count)
	  (beginning-of-line 0)))
      (setq beg (point)))
    (push-mark end)
    (goto-char beg)
    (TeX-activate-region)))

(defun Texinfo-mark-section (&optional no-subsection)
  "Mark current section, with inclusion of any containing node.

The current section is detected as starting by any of the
structuring commands matched by regexp in variable
`outline-regexp' which in turn is a regexp matching any element
of variable `texinfo-section-list'.

If optional argument NO-SUBSECTION is set to any integer or is a
non nil empty argument (i.e. `C-u \\[Texinfo-mark-section]'),
then mark the current section with exclusion of any subsections.

Otherwise, any included subsections are also marked along with
current section.

Note that when current section is starting immediatley after a
node commande, then the node command is also marked as part as
the section."
  (interactive "P")
  (let (beg end is-beg-section is-end-section
	    (section-re (concat "^\\s-*" outline-regexp)))
    (if (and (consp no-subsection) (eq (car no-subsection) 4))
	;; section with exclusion of any subsection
	(setq beg (save-excursion
		    (unless (looking-at section-re)
		      (end-of-line))
		    (re-search-backward section-re nil t))
	      is-beg-section t
	      end (save-excursion
		    (beginning-of-line)
		    (when
			(re-search-forward (concat section-re
						   "\\|^\\s-*@bye\\_>" ) nil t)
		      (save-match-data
			(beginning-of-line)
			(point))))
	      is-end-section (match-string 1))
      ;; full section without exclusion of any subsection
      (let (section-command-level)
	(setq beg
	      (save-excursion
		(end-of-line)
		(re-search-backward section-re nil t)))
	(when beg
	  (setq is-beg-section t
		section-command-level
		(cadr (assoc (match-string 1) texinfo-section-list))
		end
		(save-excursion
		  (beginning-of-line)
		  (while
		      (and (re-search-forward
			    (concat section-re "\\|^\\s-*@bye\\_>" ) nil t)
			   (or (null (setq is-end-section  (match-string 1)))
			       (> (cadr (assoc is-end-section
					       texinfo-section-list))
				  section-command-level))))
		  (when (match-string 0)
		    (beginning-of-line)
		    (point)))))));  (if ...)
    (when (and beg end)
      ;; now take also enclosing node of beg and end
      (dolist
	  (boundary '(beg end))
	(when (symbol-value (intern (concat "is-" (symbol-name boundary)
					    "-section")))
	  (save-excursion
	    (goto-char (symbol-value boundary))
	    (while
		(and
		 (null (bobp))
		 (progn
		   (beginning-of-line 0)
		   (looking-at "^\\s-*\\($\\|@\\(c\\|comment\\)\\_>\\)"))))
	    (when  (looking-at "^\\s-*@node\\_>")
	      (set boundary (point))))))

      (push-mark end)
      (goto-char beg)
      (TeX-activate-region) )))

(defun Texinfo-mark-node ()
  "Mark the current node.  \
This is the node in which the pointer is.  It is starting at
previous beginning of keyword `@node' and ending at next
beginning of keyword `@node' or `@bye'."
  (interactive)
  (let ((beg (save-excursion
	       (unless (looking-at "^\\s-*@\\(?:node\\)\\_>")
		 (end-of-line))
	       (re-search-backward "^\\s-*@\\(?:node\\)\\_>" nil t )))
	(end (save-excursion
	       (beginning-of-line)
	       (and (re-search-forward "^\\s-*@\\(?:node\\|bye\\)\\_>" nil t )
		    (progn (beginning-of-line) (point))))))

    (when (and beg end)
      (push-mark end)
      (goto-char beg)
      (TeX-activate-region) )))

(defun Texinfo-nodename-de-escape (node-name)
  "In NODE-NAME, convert `@comma{}' commands to the corresponding `,'
character. Return the resulting string."
  (let ((pos 0) (map '(("comma" . ","))))
    (while (and (< pos (length
			node-name)) (string-match "@\\(comma\\)[[:blank:]]*{}" node-name pos))
      (setq node-name (concat  (substring node-name 0 (match-beginning 0))
			       (cdr (assoc-string (match-string 1 node-name) map))
			       (substring node-name (match-end 0)))
	    pos (1+ (match-beginning 0)))))
  node-name)


(defun Texinfo-nodename-escape (node-name)
  "Convert in NODE-NAME the `,' characters to `@comma{}'
commands. Return the resulting string."
  (let* ((pos 0)
	 (map '(("," . "comma")))
	 (re (regexp-opt (mapcar 'car map))) )
    (while (and (< pos (length node-name)) (string-match re node-name pos))
      (setq node-name (concat  (substring node-name 0 (match-beginning 0))
			       "@" (cdr (assoc-string (match-string 0 node-name) map))
			       "{}"
			       (substring node-name (match-end 0)))
	    pos (1+ (match-beginning 0)))))
  node-name)


(defun Texinfo-make-node-list ()
  ;; Build list of nodes in current buffer.
  ;; (What about using `imenu--index-alist'?)
  ;; FIXME: Support multi-file documents.
  (save-excursion
    (goto-char (point-min))
    (let (nodes dups)
      (while (re-search-forward "^@node\\b" nil t)
	(skip-chars-forward "[:blank:]")
	(cl-pushnew (list (Texinfo-nodename-de-escape
			   (buffer-substring-no-properties
			    (point) (progn (skip-chars-forward "^\r\n,")
					   (skip-chars-backward "[:blank:]")
					   (point)))))
		    nodes
		    :test (lambda (a b)
			    (when (equal a b)
			      (push (cons a (line-number-at-pos (point))) dups)
			      t))))
      (when dups
	(display-warning
	 'AUCTeX
	 (format "There are duplicate nodes:\n%s"
		 (mapconcat (lambda (dup)
			      (format "    %s on line %d" (car dup) (cdr dup)))
			    (nreverse dups)
			    "\n"))))
      (nreverse nodes))))

(defun Texinfo-insert-node ()
  "Insert a Texinfo node in the current buffer.
That means, insert the string `@node' and prompt for current,
next, previous and upper node.  If there is an active region, use
this for the current node and inhibit the prompt for it.  Insert
a comment on the following line indicating the order of arguments
for @node."
  (interactive)
  (let ((active-mark (and (TeX-active-mark) (not (eq (mark) (point)))))
	(nodes (Texinfo-make-node-list))
	node-name next-node previous-node up-node)
    (unless active-mark
      (setq node-name (Texinfo-nodename-escape
		       (TeX-read-string "Node name: "))))
    ;; FIXME: What if key binding for `minibuffer-complete' was changed?
    ;; `substitute-command-keys' doesn't return the correct value.
    (setq next-node (Texinfo-nodename-escape
		     (completing-read "Next node (TAB completes): " nodes)))
    (setq previous-node
	  (Texinfo-nodename-escape
	   (completing-read "Previous node (TAB completes): " nodes)))
    (setq up-node (Texinfo-nodename-escape
		   (completing-read "Upper node (TAB completes): " nodes)))
    (when (and active-mark
	       (< (mark) (point)))
      (exchange-point-and-mark))
    (insert "@node ")
    (if active-mark
	(goto-char (mark))
      (insert node-name))
    (insert ", " next-node ", " previous-node ", " up-node
	    "\n@comment  node-name,  next,  previous,  up\n")
    ;; Position point at first empty field.
    (unless (and (or (> (length node-name) 0) active-mark)
		 (> (length next-node) 0)
		 (> (length previous-node) 0)
		 (> (length  up-node) 0))
      (forward-line -2)
      (forward-char 6)
      (catch 'break
	(if (or (> (length node-name) 0) active-mark)
	    (progn (skip-chars-forward "^,") (forward-char 2))
	  (throw 'break nil))
	(dolist (node (list next-node previous-node up-node))
	  (if (> (length node) 0)
	      (progn (skip-chars-forward "^,") (forward-char 2))
	    (throw 'break nil)))))))

(defun Texinfo-arg-nodename (optional &optional prompt definition)
  "Prompt for a node name completing with known node names.
OPTIONAL is ignored.
Use PROMPT as the prompt string.
If DEFINITION is non-nil, then chosen node name is a node name to be
added to the list of defined node names. Current implementation
ignored DEFINITION as the full document is scanned for node names at
each invocation."
  (let ((node-name (completing-read (TeX-argument-prompt optional prompt "Node")
				    (Texinfo-make-node-list))))
    (insert "{" (Texinfo-nodename-escape node-name) "}" )))

(defun Texinfo-arg-lrc (optional &rest args)
  (let ((l (read-from-minibuffer "Enter left part: "))
	(c (read-from-minibuffer "Enter center part: "))
	(r (read-from-minibuffer "Enter right part: ")))
    (insert " " l " @| " c " @| " r)))

(defun Texinfo-arg-next-line (optional &rest args)
  "Go to the beginning of next line if we are at the end of line. Otherwise insert an end-of-line."
  (if (eolp)  (forward-line) (insert "\n")))

(defun Texinfo-arg-on|off (optional &optional prompt style)
  "Prompt for a boolean input.
OPTIONAL is ignored.
Use PROMPT as the prompt string.
STYLE may be one of `:on|off' or `:true|false', if omitted `:on|off'
is assumed by default."
(let ((collection  (cdr (assq style
			 '((nil . #1=("on" "off"))
			   (:on|off . #1#)
			   (:true|false "true" "false"))))))
  (insert (if (y-or-n-p  (TeX-argument-prompt optional prompt (concat (car collection) ", not " (cadr collection))))
			 (car collection)
	    (cadr collection)))))

(defun Texinfo-arg-choice (optional &optional prompt collection)
  (insert (completing-read (TeX-argument-prompt optional prompt "Key")
			   collection)))

;; Silence the byte-compiler from warnings for variables and functions declared
;; in reftex.
(defvar reftex-section-levels-all)
(defvar reftex-level-indent)
(defvar reftex-label-menu-flags)
(defvar reftex-tables-dirty)

(declare-function reftex-match-string "reftex" (n))
(declare-function reftex-section-number "reftex-parse" (&optional level star))
(declare-function reftex-nicify-text "reftex" (text))
(declare-function reftex-ensure-compiled-variables "reftex" ())

(defun Texinfo-reftex-section-info (file)
  ;; Return a section entry for the current match.
  ;; Carefull: This function expects the match-data to be still in place!
  (let* ((marker (set-marker (make-marker) (1- (match-beginning 3))))
         (macro (reftex-match-string 3))
         (level-exp (cdr (assoc macro reftex-section-levels-all)))
         (level (if (symbolp level-exp)
                    (save-match-data (funcall level-exp))
                  level-exp))
         (unnumbered  (< level 0))
         (level (abs level))
         (section-number (reftex-section-number level unnumbered))
         (text1 (save-match-data
                  (save-excursion
		    (buffer-substring-no-properties (point) (progn (end-of-line) (point))))))
         (literal (buffer-substring-no-properties
                   (1- (match-beginning 3))
                   (min (point-max) (+ (match-end 0) (length text1) 1))))
         ;; Literal can be too short since text1 too short. No big problem.
         (text (reftex-nicify-text text1)))

    ;; Add section number and indentation
    (setq text
          (concat
           (make-string (* reftex-level-indent level) ?\ )
           (if (nth 1 reftex-label-menu-flags) ; section number flag
               (concat section-number " "))
           text))
    (list 'toc "toc" text file marker level section-number
          literal (marker-position marker))))

(defun Texinfo-reftex-hook ()
  "Hook function to plug Texinfo into RefTeX."
  ;; force recompilation of variables
  (when (string= TeX-base-mode-name "Texinfo")
    ;; dirty temporary hook to remove when reftex has a Texinfo builtin
    ;; TODO --- taken on <2014-01-06 mon> --- remove the dirty trick once reftex
    ;; has been corrected for long enough a time
    (unless (assq 'Texinfo reftex-label-alist-builtin)
      (setq reftex-label-alist-builtin (append reftex-label-alist-builtin
					       '((Texinfo "Texinfo default environments" nil)))))
    (dolist (v `((reftex-section-pre-regexp . "@")
		 ; section post-regexp must contain exactly one group
		 (reftex-section-post-regexp . "\\([ \t]+\\)")
		 (reftex-section-info-function . Texinfo-reftex-section-info)
		 (reftex-default-label-alist-entries . (Texinfo))
	       (reftex-section-levels
		. ,(mapcar
		    (lambda (x)
		      (if (string-match "\\(\\`unnumbered\\)\\|\\(heading\\'\\)\\|\\(\\`top\\'\\)"
					(car x))
			  (cons (car x) (- (cadr x)))
			(cons (car x) (cadr x))))
		    texinfo-section-list))))
      (set (make-local-variable (car v) ) (cdr v)))
    (reftex-ensure-compiled-variables)))

;;; Keymap:

(defvar Texinfo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map TeX-mode-map)

    ;; From texinfo.el
    ;; bindings for updating nodes and menus
    (define-key map "\C-c\C-um"      'texinfo-master-menu)
    (define-key map "\C-c\C-u\C-m"   'texinfo-make-menu)
    (define-key map "\C-c\C-u\C-n"   'texinfo-update-node)
    (define-key map "\C-c\C-u\C-e"   'texinfo-every-node-update)
    (define-key map "\C-c\C-u\C-a"   'texinfo-all-menus-update)

    ;; Simulating LaTeX-mode
    (define-key map "\C-c\C-e" 'Texinfo-environment)
    (define-key map "\C-c." 'Texinfo-mark-environment)
    (define-key map "\C-c*" 'Texinfo-mark-section)
    (define-key map "\M-\C-h" 'Texinfo-mark-node)
    (define-key map "\C-c\n"   'texinfo-insert-@item)
    (or (key-binding "\e\r")
	(define-key map "\e\r" 'texinfo-insert-@item)) ;*** Alias
    (define-key map "\C-c\C-s" 'Texinfo-insert-node)
    (define-key map "\C-c]" 'texinfo-insert-@end)

    ;; Override some bindings in `TeX-mode-map'
    ;; FIXME: Inside @math{}, you can use all plain TeX math commands
    ;; even in Texinfo documents.  Thus it might be nice to develop
    ;; context sensitive command so that the following four keys
    ;; inherit the command in `TeX-mode-map' inside @math{}.
    (define-key map "$"  #'self-insert-command)
    (define-key map "^"  #'self-insert-command)
    (define-key map "_"  #'self-insert-command)
    (define-key map "\\" #'self-insert-command)
    ;; Users benefit from `TeX-electric-macro' even in Texinfo mode
    (define-key map "@" #'TeX-insert-backslash)
    map)
  "Keymap for Texinfo mode.")

(easy-menu-define Texinfo-command-menu
  Texinfo-mode-map
  "Menu used in Texinfo mode for external commands."
  (TeX-mode-specific-command-menu 'texinfo-mode))

(easy-menu-define Texinfo-mode-menu
  Texinfo-mode-map
  "Menu used in Texinfo mode."
  `("Texinfo"
    ["Node ..." texinfo-insert-@node
     :help "Insert a node"]
    ["Macro ..." TeX-insert-macro
     :help "Insert a macro and possibly arguments"]
    ["Complete Macro" TeX-complete-symbol
     :help "Complete the current macro"]
    ["Environment ..." Texinfo-insert-environment
     :help "Insert an environment"]
    ["Item" texinfo-insert-@item
     :help "Insert an @item"]
    "-"
    ("Insert Font"
     ["Emphasize"  (TeX-font nil ?\C-e) :keys "C-c C-f C-e"]
     ["Bold"       (TeX-font nil ?\C-b) :keys "C-c C-f C-b"]
     ["Typewriter" (TeX-font nil ?\C-t) :keys "C-c C-f C-t"]
     ["Small Caps" (TeX-font nil ?\C-c) :keys "C-c C-f C-c"]
     ["Italic"     (TeX-font nil ?\C-i) :keys "C-c C-f C-i"]
     ["Sample"    (TeX-font nil ?\C-s) :keys "C-c C-f C-s"]
     ["Roman"      (TeX-font nil ?\C-r) :keys "C-c C-f C-r"])
    ("Replace Font"
     ["Emphasize"  (TeX-font t ?\C-e) :keys "C-u C-c C-f C-e"]
     ["Bold"       (TeX-font t ?\C-b) :keys "C-u C-c C-f C-b"]
     ["Typewriter" (TeX-font t ?\C-t) :keys "C-u C-c C-f C-t"]
     ["Small Caps" (TeX-font t ?\C-c) :keys "C-u C-c C-f C-c"]
     ["Italic"     (TeX-font t ?\C-i) :keys "C-u C-c C-f C-i"]
     ["Sample"    (TeX-font t ?\C-s) :keys "C-u C-c C-f C-s"]
     ["Roman"      (TeX-font t ?\C-r) :keys "C-u C-c C-f C-r"])
    ["Delete Font" (TeX-font t ?\C-d) :keys "C-c C-f C-d"]
    "-"
    ["Create Master Menu" texinfo-master-menu
     :help "Make a master menu for the whole Texinfo file"]
    ["Create Menu" texinfo-make-menu
     :help "Make or update the menu for the current section"]
    ["Update Node" texinfo-update-node
     :help "Update the current node"]
    ["Update Every Node" texinfo-every-node-update
     :help "Update every node in the current file"]
    ["Update All Menus" texinfo-all-menus-update
     :help "Update every menu in the current file"]
    "-"
    ("Commenting"
     ["Comment or Uncomment Region"
      comment-or-uncomment-region
      :help "Comment or uncomment the currently selected region"]
     ["Comment or Uncomment Paragraph"
      TeX-comment-or-uncomment-paragraph
      :help "Comment or uncomment the current paragraph"])
    ,TeX-fold-menu
    "-"
    . ,TeX-common-menu-entries))

(defvar Texinfo-font-list
  '((?\C-b "@b{" "}")
    (?\C-c "@sc{" "}")
    (?\C-e "@emph{" "}")
    (?\C-i "@i{" "}")
    (?\C-r "@r{" "}")
    (?\C-s "@samp{" "}")
    (?\C-t "@t{" "}")
    (?s    "@strong{" "}")
    (?\C-f "@file{" "}")
    (?d "@dfn{" "}")
    (?\C-v "@var{" "}")
    (?k    "@key{" "}")
    (?\C-k "@kbd{" "}")
    (?c    "@code{" "}")
    (?C    "@cite{" "}")
    (?\C-d "" "" t))
  "Font commands used in Texinfo mode.  See `TeX-font-list'.")

;;; Mode:

;;;###autoload
(defalias 'Texinfo-mode 'texinfo-mode)

;;;###autoload
(defun TeX-texinfo-mode ()
  "Major mode in AUCTeX for editing Texinfo files.

Special commands:
\\{Texinfo-mode-map}

Entering Texinfo mode calls the value of `text-mode-hook'  and then the
value of `Texinfo-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (setq TeX-mode-p t)
  (setq TeX-output-extension (if TeX-PDF-mode "pdf" "dvi"))
  (setq TeX-sentinel-default-function 'TeX-TeX-sentinel)
  ;; Mostly stolen from texinfo.el
  (setq TeX-base-mode-name "Texinfo")
  (setq major-mode 'texinfo-mode)
  (use-local-map Texinfo-mode-map)
  (set-syntax-table texinfo-mode-syntax-table)

  (set (make-local-variable 'page-delimiter)
       (concat
	"^@node [ \t]*[Tt]op\\|^@\\("
	texinfo-chapter-level-regexp
	"\\)"))
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'paragraph-separate)
       (concat "\b\\|@[a-zA-Z]*[ \n]\\|" paragraph-separate))
  (set (make-local-variable 'paragraph-start)
       (concat "\b\\|@[a-zA-Z]*[ \n]\\|" paragraph-start))
  (set (make-local-variable 'fill-column) 72)
  (set (make-local-variable 'comment-start) "@c ")
  (set (make-local-variable 'comment-start-skip) "@c +\\|@comment +")
  (set (make-local-variable 'comment-use-syntax) nil)
  (set (make-local-variable 'words-include-escapes) t)
  (set (make-local-variable 'imenu-generic-expression)
       texinfo-imenu-generic-expression)

  (set (make-local-variable 'font-lock-defaults)
       '(texinfo-font-lock-keywords nil nil nil backward-paragraph))
  (set (make-local-variable 'syntax-propertize-function)
       texinfo-syntax-propertize-function)

  ;; Outline settings.
  (set (make-local-variable 'outline-regexp)
       (concat "@\\("
	       (mapconcat 'car texinfo-section-list "\\>\\|")
	       "\\>\\)"))
  (set (make-local-variable 'outline-level) 'texinfo-outline-level)

  ;; Mostly AUCTeX stuff
  (easy-menu-add Texinfo-mode-menu Texinfo-mode-map)
  (easy-menu-add Texinfo-command-menu Texinfo-mode-map)
  (set (make-local-variable 'TeX-command-current) 'TeX-command-master)

  (setq TeX-default-extension "texi")
  (set (make-local-variable 'TeX-esc) "@")

  (set (make-local-variable 'TeX-auto-regexp-list) 'TeX-auto-empty-regexp-list)
  (set (make-local-variable 'TeX-auto-update) t)

  (setq TeX-command-default "TeX")
  (setq TeX-header-end "%*end")
  (setq TeX-trailer-start (regexp-quote (concat TeX-esc "bye")))

  (set (make-local-variable 'TeX-complete-list)
	(list (list "@\\([a-zA-Z]*\\)" 1 'TeX-symbol-list-filtered nil)
	      (list "" TeX-complete-word)))

  (set (make-local-variable 'TeX-font-list) Texinfo-font-list)
  (set (make-local-variable 'TeX-font-replace-function) 'TeX-font-replace-macro)
  (set (make-local-variable 'TeX-style-hook-dialect) :texinfo)

  (add-hook 'find-file-hook (lambda ()
                              (unless (file-exists-p (buffer-file-name))
                                (TeX-master-file nil nil t)))
            nil t)

  (when (and (boundp 'add-log-current-defun-function)
	     (fboundp 'texinfo-current-defun-name))
    (set (make-local-variable 'add-log-current-defun-function)
	 #'texinfo-current-defun-name))

  (TeX-add-symbols
   '("acronym" "Acronym")
   '("allowcodebreaks" (TeX-arg-literal " ") (Texinfo-arg-on|off nil :true|false) (Texinfo-arg-next-line))
   '("appendix" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("appendixsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("appendixsection" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("appendixsubsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("appendixsubsubsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("asis")
   '("atchar" nil)
   '("author" (TeX-arg-literal " ") (TeX-arg-free "Author"))
   '("b" "Text")
   '("bullet")
   '("bye")
   '("c" (TeX-arg-literal " ") (TeX-arg-free "Comment"))
   '("caption" "Caption"
     ;; TODO: caption is meaningful only inside float env. Maybe some checking
     ;; and warning would be good.
     )
   '("center" (TeX-arg-literal " ") (TeX-arg-free "Line of text"))
   '("chapheading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("chapter" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("cindex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("cite" "Reference")
   '("clear" (TeX-arg-literal " ") (TeX-arg-free "Flag"))
   '("code" "Sample code")
   '("codequotebacktick" (TeX-arg-literal " ") (Texinfo-arg-on|off) (Texinfo-arg-next-line))
   '("codequoteundirected"  (TeX-arg-literal " ") (Texinfo-arg-on|off) (Texinfo-arg-next-line))
   '("command" "Command")
   '("comment" (TeX-arg-literal " ") (TeX-arg-free "Comment"))
   '("contents")
   '("copyright" nil)
   '("defcodeindex" (TeX-arg-literal " ") (TeX-arg-free "Index name"))
   '("defindex" (TeX-arg-literal " ") (TeX-arg-free "Index name"))
   '("dfn" "Term")
   '("dmn" "Dimension")
   '("documentlanguage"  (TeX-arg-literal " ") (Texinfo-arg-choice "Language" ("ca" "cs" "de" "en" "es" "fr" "hu" "is" "it" "ja" "nb" "nl" "nn" "pl" "pt" "ru" "sr" "tr" "uk"))  (Texinfo-arg-next-line))
   '("documentencoding"  (TeX-arg-literal " ") (Texinfo-arg-choice "Encoding" ("US-ASCII" "UTF-8" "ISO-8859-1" "ISO-8859-15" "ISO-8859-2" "koi8-r" "koi8-u"))  (Texinfo-arg-next-line))
   '("dots" nil)
   '("emph" "Text")
   '("email" "Email address")
   '("equiv" nil)
   '("error")
   '("evenfooting" Texinfo-arg-lrc)
   '("evenheading" Texinfo-arg-lrc)
   '("everyfooting" Texinfo-arg-lrc)
   '("everyheading" Texinfo-arg-lrc)
   '("exdent" (TeX-arg-literal " ") (TeX-arg-free "Line of text"))
   '("expansion" nil)
   '("file" "Filename")
   '("finalout")
   '("findex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("footnote" "Text of footnote")
   '("footnotestyle" (TeX-arg-literal " ") (TeX-arg-free "Style"))
   '("guillemetleft")
   '("guillemetright")
   '("guilsinglleft")
   '("guilsinglright")
   '("heading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   ;; XXX: Would be nice with completion.
   '("headings" (TeX-arg-literal " ") (TeX-arg-free "On off single double"))
   '("i" "Text")
   '("ignore")
   '("include" (TeX-arg-literal " ") (TeX-arg-free "Filename"))
   '("inforef" "Node name" "Info file name")
   '("item")
   '("itemx")
   '("kbd" "Keyboard characters")
   '("key" "Key name")
   '("kindex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("LaTeX" nil)
   '("lowersections" 0)
   '("majorheading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("menu")
   '("minus")
   '("need" "N")
   '("node" (TeX-arg-literal " ") (TeX-arg-free "Name")
     (TeX-arg-literal ", ") (TeX-arg-free "Next")
     (TeX-arg-literal ", ") (TeX-arg-free "Previous")
     (TeX-arg-literal ", ") (TeX-arg-free "Up"))
   '("noindent")
   '("oddfooting" Texinfo-arg-lrc)
   '("oddheading" Texinfo-arg-lrc)
   '("page")
   '("paragraphindent" (TeX-arg-literal " ") (TeX-arg-free "Indent"))
   '("pindex" "Entry")
   '("point" nil)
   '("print")
   '("printindex" (TeX-arg-literal " ") (TeX-arg-free "Index name"))
   '("pxref" (Texinfo-arg-nodename "Node name"))
   '("quotedblbase")
   '("quotesinglbase")
   '("r" "Text")
   '("raisesections" 0)
   '("ref" (Texinfo-arg-nodename "Node name"))
   '("refill")
   '("result")
   '("samp" "Text")
   '("sc" "Text")
   '("section" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("set" (TeX-arg-literal " ") (TeX-arg-free "Flag"))
   '("setchapternewpage" (TeX-arg-literal " ") (Texinfo-arg-choice "On off odd" ("on" "off" "odd")) (Texinfo-arg-next-line))
   '("setfilename" (TeX-arg-literal " ") (TeX-arg-free "Info file name"))
   '("settitle" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("shortcontents")
   '("smallbook")
   '("sp" "N")
   '("strong" "Text")
   '("subheading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("subsection" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("subsubheading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("subsubsection" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("subtitle" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("summarycontents")
   '("syncodeindex" (TeX-arg-literal " ") (TeX-arg-free "From index")
     (TeX-arg-literal " ") (TeX-arg-free "Into index"))
   '("synindex" (TeX-arg-literal " ") (TeX-arg-free "From index")
     (TeX-arg-literal " ") (TeX-arg-free "Into index"))
   '("t" "Text")
   '("TeX" nil)
   '("thischapter")
   '("thischaptername")
   '("thisfile")
   '("thispage")
   '("tie")
   '("tindex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("title" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("titlefont" "Text")
   '("titlepage")
   '("today" nil)
   '("top" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("unnumbered" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("unnumberedsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("unnumberedsubsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("unnumberedsubsubsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("url" "Link")
   '("value" "Flag")
   '("var" "Metasyntactic variable")
   '("vindex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("vskip" (TeX-arg-literal " ") (TeX-arg-free "Amount"))
   '("w" "Text")
   '("xref" (Texinfo-arg-nodename "Node name")))

  ;; RefTeX plugging
  (add-hook 'reftex-mode-hook 'Texinfo-reftex-hook)
  (if (and (boundp 'reftex-mode) reftex-mode)
      (Texinfo-reftex-hook))

  (run-mode-hooks 'text-mode-hook 'Texinfo-mode-hook)
  (TeX-set-mode-name))

(defcustom Texinfo-clean-intermediate-suffixes nil
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(defcustom Texinfo-clean-output-suffixes
  ;; See `man texi2html' for the HTML stuff.
  '("\\.info\\(-[0-9]+\\)?" "\\.dvi" "\\.pdf" "\\.ps" "\\.html"
    "_toc\\.html" "_fot\\.html" "_abt\\.html" "_[0-9]+\\.html" "_l2h_img.+")
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(provide 'tex-info)

;;; tex-info.el ends here
