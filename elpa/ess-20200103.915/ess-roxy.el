;;; ess-roxy.el --- convenient editing of in-code roxygen documentation
;;
;; Copyright (C) 2009--2018 Henning Redestig, A.J. Rossini, Richard
;;      M. Heiberger, Martin Maechler, Kurt Hornik, Rodney Sparapani, Stephen
;;      Eglen, Vitalie Spinu, and J. Alexander Branham.
;;
;; Author: Henning Redestig <henning.red * go0glemail c-m>
;; Keywords: convenience, tools
;;
;; This file is part of ESS
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Lots of inspiration from doc-mode,
;; https://nschum.de/src/emacs/doc-mode/
;;
;; Features::
;;
;; - basic highlighting
;; - generating and updating templates from function definition and customized default template
;;   - C-c C-o C-o :: update template
;; - navigating and filling roxygen fields
;;   - C-c TAB, M-q, C-a, ENTER, M-h :: advised tag completion, fill-paragraph,
;;        ess-roxy-move-beginning-of-line, newline-and-indent
;;   - C-c C-o n,p :: next, previous roxygen entry
;;   - C-c C-o C-c :: Unroxygen region. Convenient for editing examples.
;; - folding visibility using hs-minor-mode
;;   - TAB :: advised ess-indent-command, hide entry if in roxygen doc.
;; - preview
;;   - C-c C-o C-r :: create a preview of the Rd file as generated
;;     using roxygen
;;   - C-c C-o C-t :: create a preview of the Rd HTML file as generated
;;     using roxygen and the tools package
;;   - C-c C-o t :: create a preview of the Rd text file
;;
;; Known issues:
;;
;; - hideshow mode does not work very well. In particular, if ordinary
;;   comments precede a roxygen entry, then both will be hidden in the
;;   same overlay from start and not unfoldable using TAB since the
;;   roxygen prefix is not present. The planned solution is implement
;;   a replacement for hideshow.
;; - only limited functionality for S4 documentation.

;;; Code:

(require 'ess-utils)
(require 'hideshow)
(require 'outline)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'ess-rd)
(require 'ess-r-syntax)

(defvar roxy-str)
(defvar ess-r-mode-syntax-table)
(declare-function ess-fill-args "ess-r-mode")
(declare-function ess-fill-continuations "ess-r-mode")
(declare-function inferior-ess-r-force "ess-r-mode")

(defvar-local ess-roxy-re nil
  "Regular expression to recognize roxygen blocks.")


;;*;; Roxy Minor Mode

(defvar ess-roxy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o h") #'ess-roxy-hide-all)
    (define-key map (kbd "C-c C-o n")   #'ess-roxy-next-entry)
    (define-key map (kbd "C-c C-o p")   #'ess-roxy-previous-entry)
    (define-key map (kbd "C-c C-o C-o") #'ess-roxy-update-entry)
    (define-key map (kbd "C-c C-o C-r") #'ess-roxy-preview-Rd)
    (define-key map (kbd "C-c C-o C-w") #'ess-roxy-preview-HTML)
    (define-key map (kbd "C-c C-o C-t")   #'ess-roxy-preview-text)
    (define-key map (kbd "C-c C-o C-c") #'ess-roxy-toggle-roxy-region)
    (define-key map [remap back-to-indentation] #'ess-roxy-goto-end-of-roxy-comment)
    (define-key map [remap newline] #'ess-roxy-newline-and-indent)
    (define-key map [remap newline-and-indent] #'ess-roxy-newline-and-indent)
    (define-key map [remap ess-indent-command] #'ess-roxy-ess-indent-command)
    (define-key map [remap move-beginning-of-line] #'ess-roxy-move-beginning-of-line)
    (define-key map [remap beginning-of-visual-line] #'ess-roxy-move-beginning-of-line)
    map))

(defvar ess-roxy-font-lock-keywords nil
  "Cache set by `ess-roxy-generate-keywords'.
Used to remove keywords added by function `ess-roxy-mode'.")

(defun ess-roxy-generate-keywords ()
  "Generate a list of keywords suitable for `font-lock-add-keywords'."
  (setq-local ess-roxy-font-lock-keywords
              `((,(concat ess-roxy-re " *\\([@\\]"
                          (regexp-opt ess-roxy-tags-param t)
                          "\\)\\>")
                 (1 'font-lock-keyword-face prepend))
                (,(concat ess-roxy-re " *\\(@"
                          (regexp-opt '("param" "importFrom" "importClassesFrom"
                                        "importMethodsFrom" "describeIn")
                                      'words)
                          "\\)\\(?:[ \t]+\\(\\(?:\\sw+,?\\)+\\)\\)")
                 (1 'font-lock-keyword-face prepend)
                 (3 'font-lock-variable-name-face prepend))
                (,(concat "[@\\]" (regexp-opt ess-roxy-tags-noparam t) "\\>")
                 (0 'font-lock-variable-name-face prepend))
                (,(concat ess-roxy-re)
                 (0 'bold prepend)))))

(defvar ess-roxy-fold-examples nil
  "Whether to fold `@examples' when opening a buffer.
Use you regular key for `outline-show-entry' to reveal it.")

;;;###autoload
(define-minor-mode ess-roxy-mode
  "Minor mode for editing ROxygen documentation."
  :keymap ess-roxy-mode-map
  (if ess-roxy-mode
      ;; Turn on `ess-roxy-mode':
      (progn
        (setq-local ess-roxy-re (concat "^" (string-trim comment-start) "+'"))
        (font-lock-add-keywords nil (ess-roxy-generate-keywords))
        (add-hook 'completion-at-point-functions #'ess-roxy-complete-tag nil t)
        ;; Hideshow Integration
        (when ess-roxy-hide-show-p
          (hs-minor-mode 1)
          (when ess-roxy-start-hidden-p
            (ess-roxy-hide-all)))
        ;;  Outline Integration
        (when ess-roxy-fold-examples
          (ess-roxy-hide-all-examples))
        ;; Autofill
        (setq-local paragraph-start (concat "\\(" ess-roxy-re "\\)*" paragraph-start))
        (setq-local paragraph-separate (concat "\\(" ess-roxy-re "\\)*" paragraph-separate))
        (setq-local adaptive-fill-function 'ess-roxy-adaptive-fill-function)
        ;; Hooks
        (add-hook 'ess-presend-filter-functions 'ess-roxy-remove-roxy-re nil t))
    ;; Turn off `ess-roxy-mode':
    ;; Hideshow
    (when (and ess-roxy-hide-show-p hs-minor-mode)
      (hs-show-all)
      (hs-minor-mode))
    ;; Hooks
    (remove-hook 'ess-presend-filter-functions 'ess-roxy-remove-roxy-re t)
    (font-lock-remove-keywords nil ess-roxy-font-lock-keywords)
    ;; (setq-local syntax-propertize-function nil)
    ;; (setq-local font-lock-fontify-region-function nil)
    ;; (setq-local font-lock-unfontify-region-function nil)
    )
  ;; Regardless of turning on or off we need to re-fontify the buffer:
  (when font-lock-mode
    (font-lock-flush)))



;;*;; Outline Integration

(defvar ess-roxy-outline-regexp "^#+' +@examples\\|^[^#]")

(defun ess-roxy-substitute-outline-regexp (command)
  (let ((outline-regexp (if (ess-roxy-entry-p "examples")
                            ess-roxy-outline-regexp
                          outline-regexp)))
    (funcall command)))

(declare-function outline-cycle "outline-magic")
(defun ess-roxy-cycle-example ()
  (interactive)
  (unless (featurep 'outline-magic)
    (error "Please install and load outline-magic"))
  ;; Don't show children when cycling @examples
  (let ((this-command 'outline-cycle-overwiew))
    (ess-roxy-substitute-outline-regexp #'outline-cycle)))

(defun ess-roxy-show-example ()
  (interactive)
  (ess-roxy-substitute-outline-regexp #'outline-show-entry))

(defun ess-roxy-hide-example ()
  (interactive)
  (ess-roxy-substitute-outline-regexp #'outline-hide-entry))

(defun ess-roxy-hide-all-examples ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#+' +@examples\\b" nil t)
      ;; Handle edge cases
      (when (ess-roxy-entry-p "examples")
        (ess-roxy-hide-example)))))

(when (featurep 'outline-magic)
  (substitute-key-definition 'outline-cyle
                             'ess-roxy-cyle-example
                             ess-roxy-mode-map outline-mode-menu-bar-map))

(substitute-key-definition 'outline-hide-entry
                           'ess-roxy-hide-example
                           ess-roxy-mode-map outline-minor-mode-map)

(substitute-key-definition 'outline-show-entry
                           'ess-roxy-show-example
                           ess-roxy-mode-map outline-minor-mode-map)


;;*;; Function definitions

(defun ess-back-to-roxy ()
  "Go to roxy prefix."
  (end-of-line)
  (re-search-backward (concat ess-roxy-re " ?") (point-at-bol))
  (goto-char (match-end 0)))

(defun ess-roxy-beg-of-entry ()
  "Get point number at start of current entry, 0 if not in entry."
  (save-excursion
    (let (beg)
      (beginning-of-line)
      (setq beg -1)
      (if (not (ess-roxy-entry-p))
          (setq beg 0)
        (setq beg (point)))
      (while (and (= (forward-line -1) 0) (ess-roxy-entry-p))
        (setq beg (point)))
      beg)))

(defun ess-roxy-in-header-p ()
  "True if point is the description / details field."
  (save-excursion
    (let ((res t)
          (cont (ess-roxy-entry-p)))
      (beginning-of-line)
      (while cont
        (if (looking-at (concat ess-roxy-re " *[@].+"))
            (progn (setq res nil)
                   (setq cont nil)))
        (setq cont (and (= (forward-line -1) 0) (ess-roxy-entry-p)))
        )res)))

(defun ess-roxy-beg-of-field ()
  "Get point number at beginning of current field, 0 if not in entry."
  (save-excursion
    (let (cont beg)
      (beginning-of-line)
      (setq beg 0)
      (setq cont t)
      (while (and (ess-roxy-entry-p) cont)
        (setq beg (point))
        (if (looking-at (concat ess-roxy-re " *[@].+"))
            (setq cont nil))
        (if (ess-roxy-in-header-p)
            (if (looking-at (concat ess-roxy-re " *$"))
                (progn
                  (forward-line 1)
                  (setq beg (point))
                  (setq cont nil))))
        (if cont (setq cont (= (forward-line -1) 0))))
      beg)))

(defun ess-roxy-end-of-entry ()
  "Get point number at end of current entry, 0 if not in entry."
  (save-excursion
    (let ((end))
      (end-of-line)
      (setq end -1)
      (if (not (ess-roxy-entry-p))
          (setq end 0)
        (setq end (point)))
      (while (and (= (forward-line 1) 0) (ess-roxy-entry-p))
        (end-of-line)
        (setq end (point)))
      end)))

(defun ess-roxy-end-of-field ()
  "Get point number at end of current field, 0 if not in entry."
  (save-excursion
    (let ((end nil)
          (cont nil))
      (setq end 0)
      (if (ess-roxy-entry-p) (progn (end-of-line) (setq end (point))))
      (beginning-of-line)
      (forward-line 1)
      (setq cont t)
      (while (and (ess-roxy-entry-p) cont)
        (save-excursion
          (end-of-line)
          (setq end (point)))
        (if (or (and (ess-roxy-in-header-p)
                     (looking-at (concat ess-roxy-re " *$")))
                (looking-at (concat ess-roxy-re " *[@].+")))
            (progn
              (forward-line -1)
              (end-of-line)
              (setq end (point))
              (setq cont nil)))
        (if cont (setq cont (= (forward-line 1) 0))))
      end)))

(defun ess-roxy-entry-p (&optional field)
  "Non-nil if point is in a roxy entry.
FIELD allows checking for a specific field with
`ess-roxy-current-field'."
  (and ess-roxy-mode
       (save-excursion
         (beginning-of-line)
         (looking-at-p ess-roxy-re))
       (or (null field)
           (string= (ess-roxy-current-field) field))))

(defun ess-roxy-narrow-to-field ()
  "Go to to the start of current field."
  (interactive)
  (let ((beg (ess-roxy-beg-of-field))
        (end (ess-roxy-end-of-field)))
    (narrow-to-region beg end)))

(defun ess-roxy-extract-field ()
  (let ((field (buffer-substring (ess-roxy-beg-of-entry)
                                 (ess-roxy-end-of-entry)))
        (prefix-re (ess-roxy-guess-str))
        (roxy-re ess-roxy-re))
    (with-temp-buffer
      (setq ess-roxy-re roxy-re)
      (insert field)
      (goto-char (point-min))
      (while (re-search-forward prefix-re (point-max) 'noerror)
        (replace-match ""))
      (buffer-substring (point-min) (point-max)))))

(defun ess-roxy-adaptive-fill-function ()
  "Return prefix for filling paragraph or nil if not determined."
  (when (ess-roxy-entry-p)
    (let ((roxy-str (car (split-string (ess-roxy-guess-str) "'"))))
      (if (ess-roxy-in-header-p)
          (save-excursion
            (ess-back-to-roxy)
            (re-search-forward "\\([ \t]*\\)" (line-end-position) t)
            (concat roxy-str "' " (match-string 1)))
        (concat roxy-str "' " (make-string ess-indent-offset ? ))))))

(defun ess-roxy-current-field ()
  "Return the name of the field at point."
  (and (not (ess-roxy-in-header-p))
       (save-excursion
         (goto-char (ess-roxy-beg-of-field))
         (if (re-search-forward (concat ess-roxy-re
                                        "[ \t]+@\\([[:alpha:]]+\\)")
                                (line-end-position) t)
             (match-string-no-properties 1)))))

(defun ess-roxy-maybe-indent-line ()
  "Indent line when point is in a field, but not in its first line."
  (when (and (not (ess-roxy-in-header-p))
             (not (equal (ess-roxy-current-field) "examples"))
             (save-excursion
               (beginning-of-line)
               (let ((line-n (count-lines 1 (point))))
                 (goto-char (ess-roxy-beg-of-field))
                 (not (equal line-n (count-lines 1 (point)))))))
    (ess-back-to-roxy)
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))
    (insert (make-string ess-indent-offset ? ))))

(defun ess-roxy-goto-func-def ()
  "Put point at start of function.
Go to the beginning of the current one or below the current
roxygen entry, error otherwise"
  (if (ess-roxy-entry-p)
      (progn
        (ess-roxy-goto-end-of-entry)
        (forward-line 1)
        (beginning-of-line))
    (unless (looking-at-p ess-function-pattern)
      (beginning-of-defun))))

(defun ess-roxy-get-args-list-from-def ()
  "Get args list for current function."
  (save-excursion
    (ess-roxy-goto-func-def)
    (let ((args (ess-roxy-get-function-args)))
      (mapcar (lambda (x) (cons x '(""))) args))))

(defun ess-roxy-insert-args (args &optional here)
  "Insert an ARGS list to the end of the current roxygen entry.
If HERE is supplied start inputting `here'. Finish at end of
line."
  (let* ((roxy-str (ess-roxy-guess-str))
         arg-des)
    (if (and here (< 1 here))
        (goto-char here)
      (ess-roxy-goto-end-of-entry)
      (beginning-of-line)
      (when (not (looking-at-p "="))
        (end-of-line)))
    (while (stringp (caar args))
      (setq arg-des (pop args))
      (unless (string= (car arg-des) "")
        (insert (concat "\n" roxy-str " @param " (car arg-des) " "))
        (insert
         (ess-replace-in-string (concat (car (cdr arg-des))) "\n"
                                (concat "\n" roxy-str)))
        (when ess-roxy-fill-param-p
          (fill-paragraph))))))

(defun ess-roxy-merge-args (fun ent)
  "Take two args lists (alists) and return their union.
The result holds all keys from both FUN and ENT but no duplicates and
association from ent are preferred over entries from fun. Also,
drop entries from ent that are not in fun and are associated with
the empty string."
  (let ((res-arg nil)
        (arg-des))
    (while (stringp (caar fun))
      (setq arg-des (pop fun))
      (if (assoc (car arg-des) ent)
          (setq res-arg
                (cons (cons (car arg-des) (cdr (assoc (car arg-des) ent))) res-arg))
        (setq res-arg (cons (cons (car arg-des) '("")) res-arg))))
    (while (stringp (caar ent))
      (setq arg-des (pop ent))
      (if (and (not (assoc (car arg-des) res-arg)) (not (string= (car (cdr arg-des)) "")))
          (setq res-arg (cons (cons (car arg-des) (cdr arg-des)) res-arg))))
    (nreverse res-arg)))

(defun ess-roxy-update-entry ()
  "Update the entry at point or the entry above the current function.
Add a template empty roxygen documentation if no roxygen entry is
available. The template can be customized via the variable
`ess-roxy-template-alist'. The parameter descriptions can are
filled if `ess-roxy-fill-param-p' is non-nil."
  (interactive)
  (unless (derived-mode-p 'ess-r-mode)
    (user-error "%s mode not yet supported" major-mode))
  (save-excursion
    (let* ((args-fun (ess-roxy-get-args-list-from-def))
           (args-ent (ess-roxy-get-args-list-from-entry))
           (args (ess-roxy-merge-args args-fun args-ent))
           (roxy-str (ess-roxy-guess-str))
           (line-break "")
           template tag-def)
      (ess-roxy-goto-func-def)
      (when (not (= (forward-line -1) 0))
        (insert "\n")
        (forward-line -1))
      (when (and (not (looking-at "^\n")) (not (ess-roxy-entry-p)))
        (end-of-line)
        (insert "\n"))
      (if (ess-roxy-entry-p)
          (ess-roxy-insert-args args (1- (ess-roxy-delete-args)))
        (setq template (copy-sequence ess-roxy-template-alist))
        (while (stringp (caar template))
          (setq tag-def (pop template))
          (if (string= (car tag-def) "param")
              (ess-roxy-insert-args args (point))
            (if (string= (car tag-def) "description")
                (insert (concat line-break roxy-str " "
                                (cdr tag-def) "\n" roxy-str))
              (if (string= (car tag-def) "details")
                  (insert (concat line-break roxy-str " " (cdr tag-def)))
                (insert (concat line-break roxy-str " @"
                                (car tag-def) " " (cdr tag-def))))))
          (setq line-break "\n"))))))

(defun ess-roxy-goto-end-of-entry ()
  "Put point at the bottom of the current entry or above the function at point.
Return t if the point is left in a roxygen entry, otherwise nil.
Error if point is not in function or roxygen entry."
  (when (not (ess-roxy-entry-p))
    (beginning-of-defun)
    (forward-line -1))
  (if (ess-roxy-entry-p)
      (progn (goto-char (ess-roxy-end-of-entry))
             t)
    (forward-line)
    nil)
  (ess-roxy-entry-p))

(defun ess-roxy-goto-beg-of-entry ()
  "Put point at the top of the entry at point or above the function at point.
Return t if the point is left in a roxygen
entry, otherwise nil. Error if point is not in function or
roxygen entry."
  (if (not (ess-roxy-entry-p))
      (progn
        (goto-char (nth 0 (end-of-defun)))
        (forward-line -1)))
  (if (ess-roxy-entry-p)
      (progn
        (goto-char (ess-roxy-beg-of-entry))
        t)
    (forward-line) nil))

(defun ess-roxy-delete-args ()
  "Remove all args from the entry at point or above the function at point.
Return 0 if no deletions were made other wise the point at where
the last deletion ended"
  (save-excursion
    (let* ((cont t)
           (field-beg 0)
           entry-beg entry-end field-end)
      (ess-roxy-goto-end-of-entry)
      (setq entry-beg (ess-roxy-beg-of-entry))
      (setq entry-end (ess-roxy-end-of-entry))
      (goto-char entry-end)
      (beginning-of-line)
      (while (and (<= entry-beg (point)) (> entry-beg 0) cont)
        (if (looking-at
             (concat ess-roxy-re " *@param"))
            (progn
              (setq field-beg (ess-roxy-beg-of-field))
              (setq field-end (ess-roxy-end-of-field))
              (delete-region field-beg (+ field-end 1))))
        (setq cont nil)
        (if (= (forward-line -1) 0)
            (setq cont t)))
      field-beg)))

(defun ess-roxy-get-args-list-from-entry ()
  "Fill an args list from the entry above the function where the point is."
  (save-excursion
    (let* (args entry-beg field-beg field-end args-text arg-name desc)
      (if (ess-roxy-goto-end-of-entry)
          (progn
            (setq roxy-str (ess-roxy-guess-str))
            (beginning-of-line)
            (setq entry-beg (ess-roxy-beg-of-entry))
            (while (and (< entry-beg (point)) (> entry-beg 0))
              (if (looking-at
                   (concat ess-roxy-re " *@param"))
                  (progn
                    (setq field-beg (ess-roxy-beg-of-field))
                    (setq field-end (ess-roxy-end-of-field))
                    (setq args-text (buffer-substring-no-properties
                                     field-beg field-end))
                    (setq args-text
                          (ess-replace-in-string args-text roxy-str ""))
                    (setq args-text
                          (ess-replace-in-string
                           args-text "[[:space:]]*@param *" ""))
                    ;; (setq args-text
                    ;;    (ess-replace-in-string args-text "\n" ""))
                    (string-match "[^[:space:]]*" args-text)
                    (setq arg-name (match-string 0 args-text))
                    (setq desc (replace-regexp-in-string
                                (concat "^" (regexp-quote arg-name) " *") "" args-text))
                    (setq args (cons (list (concat arg-name)
                                           (concat desc))
                                     args))))
              (forward-line -1))
            args)
        nil))))

(defun ess-roxy-toggle-roxy-region (beg end)
  "Toggle prefix roxygen string from BEG to END.
Add the prefix if missing, remove if found. BEG and END default
to the region, if active, and otherwise the entire line. This is
convenient for editing example fields."
  (interactive "r")
  (unless (and beg end)
    (setq beg (line-beginning-position)
          end (line-end-position)))
  (ess-roxy-roxy-region beg end (ess-roxy-entry-p)))

(defun ess-roxy-roxy-region (beg end &optional on)
  (save-excursion
    (let (RE to-string
             (roxy-str (ess-roxy-guess-str)))
      (narrow-to-region beg (- end 1))
      (if on
          (progn (setq RE (concat ess-roxy-re " +?"))
                 (setq to-string ""))
        (setq RE "^")
        (setq to-string (concat roxy-str " ")))
      (goto-char beg)
      (while (re-search-forward RE (point-max) 'noerror)
        (replace-match to-string))
      (widen))))

(defun ess-roxy-preview ()
  "Generate documentation for roxygen entry at point.
Use a connected R session (starting one if necessary) and
`ess-roxy-package' to generate the Rd code for the entry at
point. Place it in a buffer and return that buffer."
  (unless (derived-mode-p 'ess-r-mode)
    (user-error "Preview only supported in R buffers, try `ess-r-devtools-document-package' instead"))
  (let* ((beg (ess-roxy-beg-of-entry))
         (tmpf (make-temp-file "ess-roxy"))
         (roxy-buf (get-buffer-create " *RoxygenPreview*"))
         (R-old-roxy
          (concat
           "..results <- roxygen2:::roc_process(rd_roclet(), parse.files(P), \"\");"
           "cat(vapply(..results, function(x) roxygen2:::rd_out_cache$compute(x, format(x)), character(1)))" ))
         (R-new-roxy
          (concat
           "..results <- roc_proc_text(rd_roclet(), readChar(P, file.info(P)$size));"
           "cat(vapply(..results, format, character(1)), \"\n\")" ))
         (out-rd-roclet
          (cond ((string= "roxygen" ess-roxy-package)
                 "make.Rd2.roclet()$parse")
                ;; must not line break strings to avoid getting +s in the output
                ((string= "roxygen2" ess-roxy-package)
                 (concat "(function(P) { if(packageVersion('roxygen2') < '3.0.0') {"
                         R-old-roxy "} else {" R-new-roxy "} })"))
                (t (error "Need to hard code the roclet output call for roxygen package '%s'"
                          ess-roxy-package)))))
    (when (= beg 0)
      (error "Point is not in a Roxygen entry"))
    (save-excursion
      (goto-char (ess-roxy-end-of-entry))
      (forward-line 1)
      (if (end-of-defun)
          (append-to-file beg (point) tmpf)
        (while (and (forward-line 1)
                    (not (looking-at-p "^$"))
                    (not (eobp))
                    (not (looking-at-p ess-roxy-re))))
        (append-to-file beg (point) tmpf))
      (inferior-ess-r-force)
      (ess-force-buffer-current)
      (unless (ess-boolean-command (concat "print(suppressWarnings(require(" ess-roxy-package
                                           ", quietly=TRUE)))\n"))
        (error (concat "Failed to load the " ess-roxy-package " package; "
                       "in R, try  install.packages(\"" ess-roxy-package "\")")))
      (ess-command (concat out-rd-roclet "(\"" tmpf "\")\n") roxy-buf)
      (with-current-buffer roxy-buf
        ;; Kill characters up to % in case we missed stripping prompts
        ;; or +'s:
        (goto-char (point-min))
        (when (re-search-forward "%" (line-end-position) t)
          (backward-char)
          (delete-region (line-beginning-position) (point)))))
    (delete-file tmpf)
    roxy-buf))

(defun ess-roxy-preview-HTML (&optional visit-instead-of-browse)
  "Use a (possibly newly) connected R session and the roxygen package to
generate a HTML page for the roxygen entry at point and open that
buffer in a browser.  Visit the HTML file instead of showing it in
a browser if `visit-instead-of-browse' is non-nil."
  (interactive "P")
  (let* ((roxy-buf (ess-roxy-preview))
         (rd-tmp-file (make-temp-file "ess-roxy-" nil ".Rd"))
         (html-tmp-file (make-temp-file "ess-roxy-" nil ".html"))
         (rd-to-html (concat "Rd2HTML(\"" rd-tmp-file "\",\""
                             html-tmp-file "\", stages=c(\"render\"))"))
         )
    (with-current-buffer roxy-buf
      (set-visited-file-name rd-tmp-file)
      (save-buffer)
      (kill-buffer roxy-buf))
    (ess-force-buffer-current)
    (ess-command "print(suppressWarnings(require(tools, quietly=TRUE)))\n")
    (if visit-instead-of-browse
        (progn
          (ess-command (concat rd-to-html "\n"))
          (find-file html-tmp-file))
      (ess-command (concat "browseURL(" rd-to-html ")\n")))))

(defun ess-roxy-preview-text ()
  "Use the connected R session and the roxygen package to
generate the text help page of the roxygen entry at point."
  (interactive)
  (with-current-buffer (ess-roxy-preview)
    (Rd-preview-help)))

(defun ess-roxy-preview-Rd (&optional name-file)
  "Preview Rd for the roxygen entry at point.
Use the connected R session and the roxygen package to
generate the Rd code for the roxygen entry at point. If called
with a non-nil NAME-FILE (\\[universal-argument]),
also set the visited file name of the created buffer to
facilitate saving that file."
  (interactive "P")
  (let ((roxy-buf (ess-roxy-preview)))
    (pop-to-buffer roxy-buf)
    (if name-file
        (save-excursion
          (goto-char 1)
          (search-forward-regexp "name{\\(.+\\)}")
          (set-visited-file-name (concat (match-string 1) ".Rd"))))
    (Rd-mode)
    ;; why should the following be needed here? [[currently has no effect !!]]
    ;; usually in a *.Rd file fontification happens automatically
    (font-lock-ensure)))


(defun ess-roxy-guess-str (&optional not-here)
  "Guess the prefix used in the current roxygen block.
If NOT-HERE is non-nil, guess the prefix for nearest roxygen
block before the point."
  (save-excursion
    (if (ess-roxy-entry-p)
        (progn
          (goto-char (point-at-bol))
          (search-forward-regexp ess-roxy-re))
      (if not-here
          (search-backward-regexp ess-roxy-re)))
    (if (or not-here (ess-roxy-entry-p))
        (match-string 0)
      (if (derived-mode-p 'ess-r-mode)
          ess-roxy-str
        (concat (string-trim comment-start) "'")))))

(defun ess-roxy-hide-block ()
  "Hide current roxygen comment block."
  (interactive)
  (save-excursion
    (let ((end-of-entry (ess-roxy-end-of-entry))
          (beg-of-entry (ess-roxy-beg-of-entry)))
      (hs-hide-block-at-point nil (list beg-of-entry end-of-entry)))))

(defun ess-roxy-toggle-hiding ()
  "Toggle hiding/showing of a block.
See `hs-show-block' and `ess-roxy-hide-block'."
  (interactive)
  (hs-life-goes-on
   (if (hs-overlay-at (point-at-eol))
       (hs-show-block)
     (ess-roxy-hide-block))))

(defun ess-roxy-show-all ()
  "Hide all Roxygen entries in current buffer."
  (interactive)
  (ess-roxy-hide-all t))

(defun ess-roxy-hide-all (&optional show)
  "Hide all Roxygen entries in current buffer."
  (interactive)
  (when (not ess-roxy-hide-show-p)
    (user-error "First enable hide-show with `ess-roxy-hide-show-p'"))
  (hs-life-goes-on
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward (concat ess-roxy-re) (point-max) t 1)
       (let ((end-of-entry (ess-roxy-end-of-entry)))
         (if show
             (hs-show-block)
           (ess-roxy-hide-block))
         (goto-char end-of-entry)
         (forward-line 1))))))

(defun ess-roxy-previous-entry ()
  "Go to beginning of previous Roxygen entry."
  (interactive)
  (if (ess-roxy-entry-p)
      (progn
        (goto-char (ess-roxy-beg-of-entry))
        (forward-line -1)))
  (search-backward-regexp ess-roxy-re (point-min) t 1)
  (goto-char (ess-roxy-beg-of-entry)))

(defun ess-roxy-next-entry ()
  "Go to beginning of next Roxygen entry."
  (interactive)
  (if (ess-roxy-entry-p)
      (progn
        (goto-char (ess-roxy-end-of-entry))
        (forward-line 1)))
  (search-forward-regexp ess-roxy-re (point-max) t 1)
  (goto-char (ess-roxy-beg-of-entry)))

(defun ess-roxy-get-function-args ()
  "Return the arguments specified for the current function as a list of strings.
Assumes point is at the beginning of the function."
  (save-excursion
    (let ((args-txt
           (buffer-substring-no-properties
            (progn
              (search-forward-regexp "\\([=,-]+ *function *\\|^\s*function\\)" nil nil 1)
              (+ (point) 1))
            (progn
              (ess-roxy-match-paren)
              (point)))))
      (setq args-txt (replace-regexp-in-string "#+[^\"']*\n" "" args-txt))
      (setq args-txt (replace-regexp-in-string "([^)]+)" "" args-txt))
      (setq args-txt (replace-regexp-in-string "=[^,]+" "" args-txt))
      (setq args-txt (replace-regexp-in-string "[ \t\n]+" "" args-txt))
      (split-string args-txt ","))))

(defun ess-roxy-match-paren ()
  "Go to the matching parenthesis."
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun ess-roxy-complete-tag ()
  "Complete the tag at point."
  (let ((bounds (ess-bounds-of-symbol)))
    (when (and bounds
               (save-excursion
                 (goto-char (car bounds))
                 (eq (following-char) ?@)))
      (list (1+ (car bounds)) (cdr bounds)
            (append ess-roxy-tags-noparam ess-roxy-tags-param)))))

(defun ess-roxy-tag-completion ()
  "Completion data for Emacs >= 24."
  (when (save-excursion (re-search-backward "@\\<\\(\\w*\\)" (point-at-bol) t))
    (let ((beg (match-beginning 1))
          (end (match-end 1)))
      (when (and end (= end (point)))
        (list beg end (append ess-roxy-tags-noparam ess-roxy-tags-param) :exclusive 'no)))))

(defun ess-roxy-remove-roxy-re (string)
  "Remove `ess-roxy-str' from STRING before sending to R process.
Useful for sending code from example section. This function is
placed in `ess-presend-filter-functions'."
  ;; Only strip the prefix in the @examples field, and only when
  ;; STRING is entirely contained inside it. This allows better
  ;; behavior for evaluation of regions.
  (let ((roxy-re ess-roxy-re))
    (if (and (ess-roxy-entry-p "examples")
             ;; don't send just @examples if we're looking at a line
             ;; like: ##' @examples
             (not (string-match-p (concat roxy-re "[[:space:]]*@") string))
             ;; (with-temp-buffer
             ;;   ;; Need to carry the buffer-local value of
             ;;   ;; `ess-roxy-re' into the temp buffer:
             ;;   (setq ess-roxy-re roxy-re)
             ;;   (insert string)
             ;;   (ess-roxy-entry-p))
             )
        (replace-regexp-in-string (concat ess-roxy-re "\\s-*") "" string)
      string)))

(defun ess-roxy-find-par-end (stop-point &rest stoppers)
  (mapc #'(lambda (stopper)
            (when (and (> stop-point (point))
                       (save-excursion
                         (re-search-forward stopper stop-point t)))
              (setq stop-point (match-beginning 0))))
        stoppers)
  (save-excursion
    (goto-char stop-point)
    (line-end-position 0)))


;;*;; Advices
(defmacro ess-roxy-with-filling-context (examples &rest body)
  "Setup context (e.g. `comment-start') for filling roxygen BODY.
EXAMPLES should be non-nil if filling an example block."
  (declare (indent 2) (debug (&rest form)))
  `(let ((comment-start (concat ess-roxy-re "[ \t]+#"))
         (comment-start-skip (concat ess-roxy-re "[ \t]+# *"))
         (comment-use-syntax nil)
         (adaptive-fill-first-line-regexp (concat ess-roxy-re "[ \t]*"))
         (paragraph-start (concat "\\(" ess-roxy-re "\\(" paragraph-start
                                  "\\|[ \t]*@" "\\)" "\\)\\|\\(" paragraph-start "\\)"))
         (temp-table (if ,examples
                         (make-syntax-table ess-r-mode-syntax-table)
                       Rd-mode-syntax-table)))
     (when ,examples
       ;; Prevent the roxy prefix to be interpreted as comment or string
       ;; starter
       (modify-syntax-entry ?# "w" temp-table)
       (modify-syntax-entry ?' "w" temp-table))
     ;; Neutralize (comment-normalize-vars) because it modifies the
     ;; comment-start regexp in such a way that paragraph filling of
     ;; comments in @examples fields does not work
     (cl-letf (((symbol-function 'comment-normalize-vars) #'ignore))
       (with-syntax-table temp-table
         ,@body))))

(defun ess-roxy-ess-indent-command (&optional whole-exp)
  "Hide this block if we are at the beginning of the line.
Else call `ess-indent-command'."
  (interactive "P")
  (if (and (bolp) (ess-roxy-entry-p) ess-roxy-hide-show-p)
      (progn (ess-roxy-toggle-hiding))
    (ess-indent-command whole-exp)))

(defun ess--roxy-fill-block (fun &optional args)
  "Fill a roxygen block.
FUN should be a filling function and ARGS gets passed to it."
  (let* ((saved-pos (point))
         (par-start (save-excursion
                      (if (save-excursion
                            (and (backward-paragraph)
                                 (forward-paragraph)
                                 (<= (point) saved-pos)))
                          (line-beginning-position)
                        (progn (backward-paragraph) (point)))))
         (par-end (ess-roxy-find-par-end
                   (save-excursion
                     (forward-paragraph)
                     (point))
                   (concat ess-roxy-re "[ \t]*@examples\\b") "^[^#]")))
    ;; Refill the whole structural paragraph sequentially, field by
    ;; field, stopping at @examples
    (ess-roxy-with-filling-context nil
        (save-excursion
          (save-restriction
            (narrow-to-region par-start par-end)
            (goto-char (point-min))
            (while (< (point) (point-max))
              (ess-roxy-maybe-indent-line)
              (apply fun args)
              (forward-paragraph)))))))

(defun ess-r--fill-paragraph (orig-fun &rest args)
  "ESS fill paragraph for R mode.
Overrides `fill-paragraph' which is ORIG-FUN when necessary and
passes ARGS to it."
  (cond
   ;; Regular case
   ((not (derived-mode-p 'ess-r-mode))
    (apply orig-fun args))
   ;; Filling of code comments in @examples roxy field
   ((and (ess-roxy-entry-p)
         (save-excursion
           (ess-roxy-goto-end-of-roxy-comment)
           (looking-at "#")))
    (ess-roxy-with-filling-context t
        (apply orig-fun args)))
   ((and (not (ess-roxy-entry-p))
         (ess-inside-comment-p))
    (apply orig-fun args))
   ;; Filling of call arguments with point on call name
   ((and ess-fill-calls
         (ess-inside-call-name-p))
    (save-excursion
      (skip-chars-forward "^([")
      (forward-char)
      (ess-fill-args)))
   ;; Filling of continuations
   ((and ess-fill-continuations
         (ess-inside-continuation-p))
    (ess-fill-continuations))
   ;; Filling of call arguments
   ((and ess-fill-calls
         (ess-inside-call-p))
    (ess-fill-args))
   ;; Filling of roxy blocks
   ((ess-roxy-entry-p)
    (ess--roxy-fill-block orig-fun args))
   (t
    (apply orig-fun args))))
(advice-add 'fill-paragraph :around #'ess-r--fill-paragraph)

(defun ess-roxy-move-beginning-of-line (arg)
  "Move point to the beginning of the current line or roxygen comment.
If not in a roxygen comment, call `move-beginning-of-line', which
see for ARG. If in a roxygen field, leave point at the end of a
roxygen comment. If already there, move to the beginning of the
line."
  (interactive "^p")
  (if (ess-roxy-entry-p)
      (let ((pos (point)))
        (ess-roxy-goto-end-of-roxy-comment)
        (when (eql (point) pos)
          (move-beginning-of-line nil)))
    (move-beginning-of-line arg)))

(defun ess-roxy-goto-end-of-roxy-comment ()
  "Leave point at the end of a roxygen comment.
If not in a roxygen entry, call `back-to-indentation'."
  (interactive)
  (if (ess-roxy-entry-p)
      (progn
        (end-of-line)
        (re-search-backward (concat ess-roxy-re " *") (point-at-bol) t)
        (goto-char (match-end 0)))
    (back-to-indentation)))

(defun ess-roxy-indent-new-comment-line ()
  (if (not (ess-roxy-entry-p))
      (indent-new-comment-line)
    (ess-roxy-indent-on-newline)))

(defun ess-roxy-newline-and-indent ()
  "Start a newline and insert the roxygen prefix.
Only do this if in a roxygen block and
`ess-roxy-insert-prefix-on-newline' is non-nil."
  (interactive)
  (if (and (ess-roxy-entry-p)
           ess-roxy-insert-prefix-on-newline)
      (ess-roxy-indent-on-newline)
    (newline-and-indent)))

(defun ess-roxy-indent-on-newline ()
  "Insert a newline in a roxygen field."
  (cond
   ;; Point at beginning of first line of entry; do nothing
   ((= (point) (ess-roxy-beg-of-entry))
    (newline-and-indent))
   ;; Otherwise: skip over roxy comment string if necessary and then
   ;; newline and then inset new roxy comment string
   (t
    (let ((point-after-roxy-string
           (save-excursion (forward-line 0)
                           (ess-back-to-roxy)
                           (point))))
      (goto-char (max (point) point-after-roxy-string)))
    (newline-and-indent)
    (insert (concat (ess-roxy-guess-str t) " ")))))

(defun ess-roxy-cpp-fill-paragraph (&rest _args)
  "Advice for `c-fill-paragraph' that accounts for roxygen comments."
  (cond
   ;; Fill roxy @example's.
   ((ess-roxy-entry-p "examples")
    (ess--roxy-fill-block 'fill-paragraph) nil)
   ;; Fill roxy entries.
   ((ess-roxy-entry-p)
    (ess--roxy-fill-block 'fill-paragraph) nil)
   ;; Return t to signal to go on to `c-fill-paragraph'.
   (t t)))

(advice-add 'c-fill-paragraph :before-while 'ess-roxy-cpp-fill-paragraph)

(defun ess-roxy-enable-in-cpp ()
  "Enable `ess-roxy-mode' in C++ buffers in R packages."
  (when (and (fboundp 'ess-r-package-project)
             (ess-r-package-project))
    (ess-roxy-mode)))

(with-eval-after-load "cc-mode"
  (add-hook 'c++-mode-hook #'ess-roxy-enable-in-cpp))

(provide 'ess-roxy)

;;; ess-roxy.el ends here
