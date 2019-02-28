;;; awesome-pair.el --- Auto parenthesis pairing with syntax table

;; Filename: awesome-pair.el
;; Description: Auto parenthesis pairing with syntax table
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-11-11 09:27:58
;; Version: 1.3
;; Last-Updated: 2019-02-27 08:56:34
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/awesome-pair.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `subr-x'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Auto parenthesis pairing with syntax table.
;;
;; I'm a big fans of paredit.el, I have used the paredit.el more than ten years.
;; But paredit.el not very good for web programming, so I think it's time to write my own plugin.
;;
;; Thanks Taylor R. Campbell, you surprise me how wonderful Emacs is.
;;

;;; Installation:
;;
;; Put awesome-pair.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'awesome-pair)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET awesome-pair RET
;;

;;; Change log:
;;
;; 2019/02/27
;;      * Don't insert \" in string that wrap by `...` when current mode is golang.
;;
;; 2019/02/09
;;      * Insert ) directly in sh-mode for case ... in syntax.
;;
;; 2019/02/07
;;      * Don't insert \ before " if cursor at comment area.
;;      * Fix `integer-or-marker-p' error when run `awesome-pair-*-delete-in-string' functions in Python language.
;;
;; 2019/01/30
;;      * Fix 'wrong type character-p' error when call `awesome-pair-forward-delete' in beginning of buffer.
;;      * Add docs of `awesome-pair-forward-delete'.
;;
;; 2019/01/29
;;      * Fixed bug where `awesome-pair-jump-out-pair-and-newline' function did not clean unnecessary whitespaces sometimes.
;;
;; 2019/01/09
;;      * Just indent parent expression after unwrap pair when in lisp like language.
;;
;; 2018/12/27
;;      * Just clean unnecessary whitespace before close parenthesis when in lisp like language.
;;
;; 2018/12/09
;;      * Fix bug of `awesome-pair-in-string-p' when cursor at left side of string.
;;
;; 2018/12/02
;;      * Use `get-text-property' improve algorithm of `awesome-pair-in-string-p' and `awesome-pair-in-commit-p'
;;
;; 2018/11/30
;;      * Fix `awesome-pair-kill-line-in-string' won't work with golang string.
;;
;; 2018/11/23
;;      * Make `awesome-pair-kill-line-in-string' support single quote string.
;;
;; 2018/11/11
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'subr-x)

;;; Code:

(defvar awesome-pair-mode-map (make-sparse-keymap)
  "Keymap for the awesome-pair minor mode.")

(define-minor-mode awesome-pair-mode
  "Minor mode for auto parenthesis pairing with syntax table.
\\<awesome-pair-mode-map>"
  )

;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;

(defun awesome-pair-open-round ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "(")
      (goto-char (+ end 1))
      (insert ")")
      (goto-char (+ start 1))))
   ((or (awesome-pair-in-string-p)
        (awesome-pair-in-comment-p))
    (insert "("))
   (t
    (insert "()")
    (backward-char))
   ))

(defun awesome-pair-open-curly ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "{")
      (goto-char (+ end 1))
      (insert "}")
      (goto-char (+ start 1))))
   ((or (awesome-pair-in-string-p)
        (awesome-pair-in-comment-p))
    (insert "{"))
   (t
    (cond ((derived-mode-p 'ruby-mode)
           (insert "{  }")
           (backward-char 2))
          (t
           (insert "{}")
           (backward-char)))
    )
   ))

(defun awesome-pair-open-bracket ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "[")
      (goto-char (+ end 1))
      (insert "]")
      (goto-char (+ start 1))))
   ((or (awesome-pair-in-string-p)
        (awesome-pair-in-comment-p))
    (insert "["))
   (t
    (insert "[]")
    (backward-char))
   ))

(defun awesome-pair-close-round ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert ")"))
        ;; Insert ) directly in sh-mode for case ... in syntax.
        ((derived-mode-p 'sh-mode)
         (insert ")"))
        (t
         (let ((close (awesome-pair-missing-close)))
           (if close
               (if (eq ?\) (matching-paren close))
                   (insert ")"))
             (up-list))
           ))))

(defun awesome-pair-close-curly ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert "}"))
        (t
         (let ((close (awesome-pair-missing-close)))
           (if close
               (if (eq ?\} (matching-paren close))
                   (insert "}"))
             (up-list))
           ))))

(defun awesome-pair-close-bracket ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert "]"))
        (t
         (let ((close (awesome-pair-missing-close)))
           (if close
               (if (eq ?\] (matching-paren close))
                   (insert "]"))
             (up-list))
           ))))

(defun awesome-pair-double-quote ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (if (and (derived-mode-p 'go-mode)
                  (equal (save-excursion (nth 3 (awesome-pair-current-parse-state))) 96))
             ;; When current mode is golang.
             ;; Don't insert \" in string that wrap by `...`
             (insert "\"")
           (insert "\\\"")))
        ((awesome-pair-in-comment-p)
         (insert "\""))
        (t
         (insert "\"\"")
         (backward-char))
        ))

(defun awesome-pair-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((or (awesome-pair-in-comment-p)
             (awesome-pair-in-string-p))
         (self-insert-command (or arg 1)))
        ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        (t
         (cond
          ;; Enhancement the automatic jump of web-mode.
          ((derived-mode-p 'web-mode)
           (awesome-pair-web-mode-match-paren))
          (t
           (self-insert-command (or arg 1))))
         )))

(defun awesome-pair-web-mode-match-paren ()
  (cond ((looking-at "<")
         (sgml-skip-tag-forward 1))
        ((looking-back ">")
         (sgml-skip-tag-backward 1))
        (t (self-insert-command (or arg 1)))))

(defun awesome-pair-backward-delete ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (awesome-pair-backward-delete-in-string))
        ((awesome-pair-in-comment-p)
         (backward-delete-char 1))
        ((awesome-pair-after-close-pair-p)
         (awesome-pair-backward-movein-or-delete-close-pair))
        ((awesome-pair-in-empty-pair-p)
         (awesome-pair-backward-delete-in-pair))
        ((not (awesome-pair-after-open-pair-p))
         (backward-delete-char 1))))

(defun awesome-pair-forward-delete ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (awesome-pair-forward-delete-in-string))
        ((awesome-pair-in-comment-p)
         (delete-char 1))
        ((awesome-pair-before-open-pair-p)
         (awesome-pair-forward-movein-or-delete-open-pair))
        ((awesome-pair-in-empty-pair-p)
         (awesome-pair-backward-delete-in-pair))
        ((not (awesome-pair-before-close-pair-p))
         (delete-char 1)
         )))

(defun awesome-pair-kill ()
  "It's annoying that we need re-indent line after we delete blank line with `awesome-pair-kill'.
`paredt-kill+' fixed this problem.

If current mode is `web-mode', use `awesome-pair-web-mode-kill' instead `awesome-pair-kill' for smarter kill operation."
  (interactive)
  (cond ((derived-mode-p 'web-mode)
         (awesome-pair-web-mode-kill))
        ((derived-mode-p 'ruby-mode)
         (awesome-pair-ruby-mode-kill))
        (t
         (awesome-pair-common-mode-kill))))

(defun awesome-pair-wrap-round ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "(")
      (goto-char (+ end 1))
      (insert ")")
      (goto-char start)))
   ((awesome-pair-in-string-p)
    (let ((string-bound (awesome-pair-string-start+end-points)))
      (save-excursion
        (goto-char (car string-bound))
        (insert "(")
        (goto-char (+ (cdr string-bound) 2))
        (insert ")"))))
   ((awesome-pair-in-comment-p)
    (save-excursion
      (let ((start (beginning-of-thing 'symbol))
            (end (end-of-thing 'symbol)))
        (goto-char start)
        (insert "(")
        (goto-char (+ end 1))
        (insert ")"))))
   (t
    (save-excursion
      (let ((start (beginning-of-thing 'sexp))
            (end (end-of-thing 'sexp)))
        (goto-char start)
        (insert "(")
        (goto-char (+ end 1))
        (insert ")"))
      )))
  ;; Forward to jump in parenthesis.
  (forward-char)
  ;; Indent wrap area.
  (awesome-pair-indent-parenthesis-area))

(defun awesome-pair-wrap-bracket ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "[")
      (goto-char (+ end 1))
      (insert "]")
      (goto-char start)))
   ((awesome-pair-in-string-p)
    (let ((string-bound (awesome-pair-string-start+end-points)))
      (save-excursion
        (goto-char (car string-bound))
        (insert "[")
        (goto-char (+ (cdr string-bound) 2))
        (insert "]"))))
   ((awesome-pair-in-comment-p)
    (save-excursion
      (let ((start (beginning-of-thing 'symbol))
            (end (end-of-thing 'symbol)))
        (goto-char start)
        (insert "[")
        (goto-char (+ end 1))
        (insert "]"))))
   (t
    (save-excursion
      (let ((start (beginning-of-thing 'sexp))
            (end (end-of-thing 'sexp)))
        (goto-char start)
        (insert "[")
        (goto-char (+ end 1))
        (insert "]"))
      )))
  ;; Forward to jump in parenthesis.
  (forward-char)
  ;; Indent wrap area.
  (awesome-pair-indent-parenthesis-area))

(defun awesome-pair-wrap-curly ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "{")
      (goto-char (+ end 1))
      (insert "}")
      (goto-char start)))
   ((awesome-pair-in-string-p)
    (let ((string-bound (awesome-pair-string-start+end-points)))
      (save-excursion
        (goto-char (car string-bound))
        (insert "{")
        (goto-char (+ (cdr string-bound) 2))
        (insert "}"))))
   ((awesome-pair-in-comment-p)
    (save-excursion
      (let ((start (beginning-of-thing 'symbol))
            (end (end-of-thing 'symbol)))
        (goto-char start)
        (insert "{")
        (goto-char (+ end 1))
        (insert "}"))))
   (t
    (save-excursion
      (let ((start (beginning-of-thing 'sexp))
            (end (end-of-thing 'sexp)))
        (goto-char start)
        (insert "{")
        (goto-char (+ end 1))
        (insert "}"))
      )))
  ;; Forward to jump in parenthesis.
  (forward-char)
  ;; Indent wrap area.
  (awesome-pair-indent-parenthesis-area))

(defun awesome-pair-wrap-double-quote ()
  (interactive)
  (cond
   ((region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (setq mark-active nil)
      (goto-char start)
      (insert "\"")
      (goto-char (+ end 1))
      (insert "\"")
      (goto-char start)))
   ((awesome-pair-in-string-p)
    (goto-char (1+ (cdr (awesome-pair-string-start+end-points)))))
   ((awesome-pair-in-comment-p)
    (save-excursion
      (let ((start (beginning-of-thing 'symbol))
            (end (end-of-thing 'symbol)))
        (goto-char start)
        (insert "\"")
        (goto-char (+ end 1))
        (insert "\""))))
   (t
    (save-excursion
      (let ((start (beginning-of-thing 'sexp))
            (end (end-of-thing 'sexp)))
        (goto-char start)
        (insert "\"")
        (goto-char (+ end 1))
        (insert "\"")))))
  ;; Forward to jump in parenthesis.
  (forward-char)
  ;; Indent wrap area.
  (awesome-pair-indent-parenthesis-area))

(defun awesome-pair-unwrap (&optional argument)
  (interactive "P")
  (if (awesome-pair-in-string-p)
      (awesome-pair-splice-string argument)
    (save-excursion
      (awesome-pair-kill-surrounding-sexps-for-splice argument)
      (backward-up-list)
      (save-excursion
        (forward-sexp)
        (backward-delete-char 1))
      (delete-char 1)
      ;; Try to indent parent expression after unwrap pair.
      ;; This feature just enable in lisp-like language.
      (when (or
             (derived-mode-p 'lisp-mode)
             (derived-mode-p 'emacs-lisp-mode))
        (ignore-errors
          (backward-up-list)
          (indent-sexp))))))

(defun awesome-pair-jump-out-pair-and-newline ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (goto-char (1+ (cdr (awesome-pair-string-start+end-points))))
         (newline-and-indent))
        (t
         ;; Just do when have `up-list' in next step.
         (if (awesome-pair-ignore-errors (save-excursion (up-list)))
             (let (up-list-point)
               (if (awesome-pair-is-blank-line-p)
                   ;; Clean current line first if current line is blank line.
                   (awesome-pair-kill-current-line)
                 ;; Move out of current parentheses and newline.
                 (up-list)
                 (setq up-list-point (point))
                 (newline-and-indent)
                 ;; Try to clean unnecessary whitespace before close parenthesis.
                 ;; This feature just enable in lisp-like language.
                 (when (or
                        (derived-mode-p 'lisp-mode)
                        (derived-mode-p 'emacs-lisp-mode))
                   (save-excursion
                     (goto-char up-list-point)
                     (backward-char)
                     (when (awesome-pair-only-whitespaces-before-cursor-p)
                       (awesome-pair-delete-whitespace-around-cursor))))))
           ;; Try to clean blank line if no pair can jump out.
           (if (awesome-pair-is-blank-line-p)
               (awesome-pair-kill-current-line))))))

(defun awesome-pair-jump-left ()
  "To left of previous match parentheses."
  (interactive)
  (cond
   ;; Jump out of string if cursor in string area.
   ((awesome-pair-in-string-p)
    (goto-char (car (awesome-pair-string-start+end-points))))
   ;; Jump to previous pair.
   (t
    (backward-char 1)
    (while (not (looking-at "\\(['\"<({]\\|[[]\\)")) (backward-char 1)))))

(defun awesome-pair-jump-right ()
  "To right of next match parentheses."
  (interactive)
  (cond
   ;; Jump out of string if cursor in string area.
   ((awesome-pair-in-string-p)
    (goto-char (+ (cdr (awesome-pair-string-start+end-points)) 1)))
   ;; Jump to next pair.
   (t
    (while (not (looking-at "\\(['\">)}]\\|]\\)")) (forward-char 1))
    (forward-char 1))))

(defun awesome-pair-delete-whitespace-before-cursor ()
  (kill-region (save-excursion
                 (search-backward-regexp "[^ \t\n]" nil t)
                 (forward-char)
                 (point))
               (point)))

(defun awesome-pair-delete-whitespace-around-cursor ()
  (kill-region (save-excursion
                 (search-backward-regexp "[^ \t\n]" nil t)
                 (forward-char)
                 (point))
               (save-excursion
                 (search-forward-regexp "[^ \t\n]" nil t)
                 (backward-char)
                 (point))))

(defun awesome-pair-kill-current-line ()
  (kill-region (beginning-of-thing 'line) (end-of-thing 'line))
  (back-to-indentation))

(defun awesome-pair-missing-close ()
  (let (open)
    (ignore-errors
      (save-excursion
        (backward-up-list)
        (setq open (char-after))
        (if (awesome-pair-ignore-errors (forward-sexp))
            nil
          open)))))

(defun awesome-pair-backward-delete-in-pair ()
  (backward-delete-char 1)
  (delete-char 1))

(defun awesome-pair-backward-movein-or-delete-close-pair ()
  (if (awesome-pair-ignore-errors (save-excursion (backward-sexp)))
      (backward-char)
    (backward-delete-char 1)))

(defun awesome-pair-forward-movein-or-delete-open-pair ()
  (if (awesome-pair-ignore-errors (save-excursion (forward-sexp)))
      (forward-char)
    (delete-char 1)))

(defun awesome-pair-backward-delete-in-string ()
  (let ((start+end (awesome-pair-string-start+end-points)))
    (cond
     ;; Some language, such as Python, `awesome-pair-string-start+end-points' will return nil cause by `beginning-of-defun' retun nil.
     ;; This logical branch is handle this.
     ((not start+end)
      ;; First determine if it is in the string area?
      (when (awesome-pair-in-string-p)
        (let ((syn-before (char-syntax (char-before)))
              (syn-after  (char-syntax (char-after))))
          (cond
           ;; Remove double quotes when the string is empty
           ((and (eq syn-before ?\" )
                 (eq syn-after  ?\" ))
            (backward-delete-char 1)
            (delete-char 1))
           ;; If there is still content in the string and the double quotation marks are in front of the cursor,
           ;; no delete operation is performed.
           ((eq syn-before ?\" ))
           ;; If the cursor is not double quotes before and after, delete the previous character.
           (t
            (backward-delete-char 1))))))
     ((not (eq (1- (point)) (car start+end)))
      (if (awesome-pair-in-string-escape-p)
          (delete-char 1))
      (backward-delete-char 1)
      (if (awesome-pair-in-string-escape-p)
          (backward-delete-char 1)))
     ((eq (point) (cdr start+end))
      (backward-delete-char 1)
      (delete-char 1)))))

(defun awesome-pair-forward-delete-in-string ()
  (let ((start+end (awesome-pair-string-start+end-points)))
    (cond
     ;; Some language, such as Python, `awesome-pair-string-start+end-points' will return nil cause by `beginning-of-defun' retun nil.
     ;; This logical branch is handle this.
     ((not start+end)
      ;; First determine if it is in the string area?
      (when (awesome-pair-in-string-p)
        (let ((syn-before (char-syntax (char-before)))
              (syn-after  (char-syntax (char-after))))
          (cond
           ;; Remove double quotes when the string is empty
           ((and (eq syn-before ?\" )
                 (eq syn-after  ?\" ))
            (backward-delete-char 1)
            (delete-char 1))
           ;; If there is still content in the string and the double quotation marks are after of the cursor,
           ;; no delete operation is performed.
           ((eq syn-after ?\" ))
           ;; If the cursor is not double quotes before and after, delete the previous character.
           (t
            (delete-char 1))))))
     ((not (eq (point) (cdr start+end)))
      (cond ((awesome-pair-in-string-escape-p)
             (delete-char -1))
            ((eq (char-after) ?\\ )
             (delete-char +1)))
      (delete-char +1))
     ((eq (1- (point)) (car start+end))
      (delete-char -1)
      (delete-char +1)))))

(defun awesome-pair-splice-string (argument)
  (let ((original-point (point))
        (start+end (awesome-pair-string-start+end-points)))
    (let ((start (car start+end))
          (end (cdr start+end)))
      (let* ((escaped-string
              (cond ((not (consp argument))
                     (buffer-substring (1+ start) end))
                    ((= 4 (car argument))
                     (buffer-substring original-point end))
                    (t
                     (buffer-substring (1+ start) original-point))))
             (unescaped-string
              (awesome-pair-unescape-string escaped-string)))
        (if (not unescaped-string)
            (error "Unspliceable string.")
          (save-excursion
            (goto-char start)
            (delete-region start (1+ end))
            (insert unescaped-string))
          (if (not (and (consp argument)
                        (= 4 (car argument))))
              (goto-char (- original-point 1))))))))

(defun awesome-pair-point-at-sexp-start ()
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (point)))

(defun awesome-pair-point-at-sexp-end ()
  (save-excursion
    (backward-sexp)
    (forward-sexp)
    (point)))

(defun awesome-pair-point-at-sexp-boundary (n)
  (cond ((< n 0) (awesome-pair-point-at-sexp-start))
        ((= n 0) (point))
        ((> n 0) (awesome-pair-point-at-sexp-end))))

(defun awesome-pair-kill-surrounding-sexps-for-splice (argument)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (error "Invalid context for splicing S-expressions."))
        ((or (not argument) (eq argument 0)) nil)
        ((or (numberp argument) (eq argument '-))
         (let* ((argument (if (eq argument '-) -1 argument))
                (saved (awesome-pair-point-at-sexp-boundary (- argument))))
           (goto-char saved)
           (ignore-errors (backward-sexp argument))
           (awesome-pair-hack-kill-region saved (point))))
        ((consp argument)
         (let ((v (car argument)))
           (if (= v 4)
               (let ((end (point)))
                 (ignore-errors
                   (while (not (bobp))
                     (backward-sexp)))
                 (awesome-pair-hack-kill-region (point) end))
             (let ((beginning (point)))
               (ignore-errors
                 (while (not (eobp))
                   (forward-sexp)))
               (awesome-pair-hack-kill-region beginning (point))))))
        (t (error "Bizarre prefix argument `%s'." argument))))

(defun awesome-pair-unescape-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (not (eobp))
                (search-forward "\\" nil t))
      (delete-char -1)
      (forward-char))
    (condition-case condition
        (progn (check-parens) (buffer-string))
      (error nil))))

(defun awesome-pair-hack-kill-region (start end)
  (let ((this-command nil)
        (last-command nil))
    (kill-region start end)))

(defun awesome-pair-kill-internal ()
  (cond (current-prefix-arg
         (kill-line (if (integerp current-prefix-arg)
                        current-prefix-arg
                      1)))
        ((awesome-pair-in-string-p)
         (awesome-pair-kill-line-in-string))
        ((awesome-pair-in-single-quote-string-p)
         (awesome-pair-kill-line-in-single-quote-string))
        ((or (awesome-pair-in-comment-p)
             (save-excursion
               (awesome-pair-skip-whitespace t (point-at-eol))
               (or (eq (char-after) ?\; )
                   (eolp))))
         (kill-line))
        (t (awesome-pair-kill-sexps-on-line))))

(defun awesome-pair-kill-line-in-single-quote-string ()
  (let ((sexp-end (save-excursion
                    (forward-sexp)
                    (backward-char)
                    (point))))
    (kill-region (point) sexp-end)))

(defun awesome-pair-kill-line-in-string ()
  (cond ((save-excursion
           (awesome-pair-skip-whitespace t (point-at-eol))
           (eolp))
         (kill-line))
        (t
         (save-excursion
           (if (awesome-pair-in-string-escape-p)
               (backward-char))
           (let ((beginning (point)))
             (while (save-excursion
                      (forward-char)
                      (awesome-pair-in-string-p))
               (forward-char))
             (kill-region beginning (point)))
           ))))

(defun awesome-pair-skip-whitespace (trailing-p &optional limit)
  (funcall (if trailing-p 'skip-chars-forward 'skip-chars-backward)
           " \t\n"
           limit))

(defun awesome-pair-kill-sexps-on-line ()
  (if (awesome-pair-in-char-p)
      (backward-char 2))
  (let ((beginning (point))
        (eol (point-at-eol)))
    (let ((end-of-list-p (awesome-pair-forward-sexps-to-kill beginning eol)))
      (if end-of-list-p (progn (up-list) (backward-char)))
      (if kill-whole-line
          (awesome-pair-kill-sexps-on-whole-line beginning)
        (kill-region beginning
                     (if (and (not end-of-list-p)
                              (eq (point-at-eol) eol))
                         eol
                       (point)))))))

(defun awesome-pair-forward-sexps-to-kill (beginning eol)
  (let ((end-of-list-p nil)
        (firstp t))
    (catch 'return
      (while t
        (if (and kill-whole-line (eobp)) (throw 'return nil))
        (save-excursion
          (unless (awesome-pair-ignore-errors (forward-sexp))
            (if (awesome-pair-ignore-errors (up-list))
                (progn
                  (setq end-of-list-p (eq (point-at-eol) eol))
                  (throw 'return nil))))
          (if (or (and (not firstp)
                       (not kill-whole-line)
                       (eobp))
                  (not (awesome-pair-ignore-errors (backward-sexp)))
                  (not (eq (point-at-eol) eol)))
              (throw 'return nil)))
        (forward-sexp)
        (if (and firstp
                 (not kill-whole-line)
                 (eobp))
            (throw 'return nil))
        (setq firstp nil)))
    end-of-list-p))

(defun awesome-pair-kill-sexps-on-whole-line (beginning)
  (kill-region beginning
               (or (save-excursion
                     (awesome-pair-skip-whitespace t)
                     (and (not (eq (char-after) ?\; ))
                          (point)))
                   (point-at-eol)))
  (cond ((save-excursion (awesome-pair-skip-whitespace nil (point-at-bol))
                         (bolp))
         (lisp-indent-line))
        ((eobp) nil)
        ((let ((syn-before (char-syntax (char-before)))
               (syn-after  (char-syntax (char-after))))
           (or (and (eq syn-before ?\) )
                    (eq syn-after  ?\( ))
               (and (eq syn-before ?\" )
                    (eq syn-after  ?\" ))
               (and (memq syn-before '(?_ ?w))
                    (memq syn-after  '(?_ ?w)))))
         (insert " "))))

(defun awesome-pair-common-mode-kill ()
  (if (awesome-pair-is-blank-line-p)
      (awesome-pair-kill-blank-line-and-reindent)
    (awesome-pair-kill-internal)))

(defun awesome-pair-web-mode-kill ()
  "It's a smarter kill function for `web-mode'."
  (if (awesome-pair-is-blank-line-p)
      (awesome-pair-kill-blank-line-and-reindent)
    (cond
     ;; Kill all content wrap by <% ... %> when right is <%
     ((and (looking-at "<%")
           (save-excursion (search-forward-regexp "%>" nil t)))
      (kill-region (point) (search-forward-regexp "%>" nil t)))
     ;; Kill content in <% ... %> if left is <% or <%=
     ((and (looking-back "<%=?\\s-?")
           (save-excursion (search-forward-regexp "%>" nil t)))
      (let ((start (point))
            (end (progn
                   (search-forward-regexp "%>" nil t)
                   (backward-char 2)
                   (point)
                   )))
        (kill-region start end)))
     ;; Kill string if current pointer in string area.
     ((awesome-pair-in-string-p)
      (awesome-pair-kill-internal))
     ;; Kill string in single quote.
     ((awesome-pair-in-single-quote-string-p)
      (awesome-pair-kill-line-in-single-quote-string))
     ;; Kill element if no attributes in tag.
     ((and
       (looking-at "\\s-?+</")
       (looking-back "<[a-z]+\\s-?>\\s-?+"))
      (web-mode-element-kill 1))
     ;; Kill whitespace in tag.
     ((looking-at "\\s-+>")
      (search-forward-regexp ">" nil t)
      (backward-char)
      (awesome-pair-delete-whitespace-before-cursor))
     ;; Jump in content if point in start tag.
     ((and (looking-at ">")
           (looking-back "<[a-z]+"))
      (forward-char 1))
     ;; Kill tag if in end tag.
     ((and (looking-at ">")
           (looking-back "</[a-z]+"))
      (beginning-of-thing 'sexp)
      (web-mode-element-kill 1))
     ;; Kill attributes if point in attributes area.
     ((and
       (web-mode-attribute-beginning-position)
       (web-mode-attribute-end-position)
       (>= (point) (web-mode-attribute-beginning-position))
       (<= (point) (web-mode-attribute-end-position)))
      (web-mode-attribute-kill))
     ;; Kill attributes if only space between point and attributes start.
     ((and
       (looking-at "\\s-+")
       (save-excursion
         (search-forward-regexp "\\s-+" nil t)
         (equal (point) (web-mode-attribute-beginning-position))))
      (search-forward-regexp "\\s-+")
      (web-mode-attribute-kill))
     ;; Kill line if rest chars is whitespace.
     ((looking-at "\\s-?+\n")
      (kill-line))
     (t
      (unless (awesome-pair-ignore-errors
               ;; Kill all sexps in current line.
               (awesome-pair-kill-sexps-on-line))
        ;; Kill block if sexp parse failed.
        (web-mode-block-kill))))))

(defun awesome-pair-ruby-mode-kill ()
  "It's a smarter kill function for `ruby-mode'.

If current line is blank line, re-indent line after kill whole line.

If current line is not blank, do `awesome-pair-kill' first, re-indent line if rest line start with ruby keywords.
"
  (if (awesome-pair-is-blank-line-p)
      (awesome-pair-kill-blank-line-and-reindent)
    ;; Do `awesome-pair-kill' first.
    (awesome-pair-kill-internal)

    ;; Re-indent current line if line start with ruby keywords.
    (when (let (in-beginning-block-p
                in-end-block-p
                current-symbol)
            (save-excursion
              (back-to-indentation)
              (ignore-errors (setq current-symbol (buffer-substring-no-properties (beginning-of-thing 'symbol) (end-of-thing 'symbol))))
              (setq in-beginning-block-p (member current-symbol '("class" "module" "else" "def" "if" "unless" "case" "while" "until" "for" "begin" "do")))
              (setq in-end-block-p (member current-symbol '("end")))

              (or in-beginning-block-p in-end-block-p)))
      (indent-for-tab-command))))

(defun awesome-pair-kill-blank-line-and-reindent ()
  (kill-region (beginning-of-thing 'line) (end-of-thing 'line))
  (back-to-indentation))

(defun awesome-pair-indent-parenthesis-area ()
  (let ((bound-start (save-excursion
                       (backward-up-list)
                       (point)))
        (bound-end (save-excursion
                     (up-list)
                     (point)
                     )))
    (save-excursion
      (indent-region bound-start bound-end))))

;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;
(defun awesome-pair-current-parse-state ()
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun awesome-pair-string-start+end-points (&optional state)
  (ignore-errors
    (save-excursion
      (let ((start (nth 8 (or state (awesome-pair-current-parse-state)))))
        (goto-char start)
        (forward-sexp 1)
        (cons start (1- (point)))))))

(defun awesome-pair-after-open-pair-p ()
  (save-excursion
    (let ((syn (char-syntax (char-before))))
      (or (eq syn ?\()
          (and (eq syn ?_)
               (eq (char-before) ?\{)))
      )))

(defun awesome-pair-after-close-pair-p ()
  (save-excursion
    (let ((syn (char-syntax (char-before))))
      (or (eq syn ?\) )
          (eq syn ?\" )
          (and (eq syn ?_ )
               (eq (char-before) ?\}))
          ))))

(defun awesome-pair-before-open-pair-p ()
  (save-excursion
    (let ((syn (char-syntax (char-after))))
      (or (eq syn ?\( )
          (eq syn ?\" )
          (and (eq syn ?_)
               (eq (char-after) ?\{))))))

(defun awesome-pair-before-close-pair-p ()
  (save-excursion
    (let ((syn (char-syntax (char-after))))
      (or (eq syn ?\) )
          (and (eq syn ?_)
               (eq (char-after) ?\}))))))

(defun awesome-pair-in-empty-pair-p ()
  (ignore-errors
    (save-excursion
      (or (and (eq (char-syntax (char-before)) ?\()
               (eq (char-after) (matching-paren (char-before))))
          (and (eq (char-syntax (char-before)) ?_)
               (eq (char-before) ?\{)
               (eq (char-syntax (char-after)) ?_)
               (eq (char-after) ?\})
               )))))

(defun awesome-pair-in-single-quote-string-p ()
  (save-excursion
    (when (awesome-pair-ignore-errors
           (progn
             (save-excursion (backward-sexp))
             (save-excursion (forward-sexp))))
      (let* ((current-sexp (buffer-substring-no-properties
                            (save-excursion
                              (backward-sexp)
                              (point))
                            (save-excursion
                              (forward-sexp)
                              (point))
                            ))
             (first-char (substring current-sexp 0 1))
             (last-char (substring current-sexp -1 nil)))
        (and (string-equal first-char "'")
             (string-equal last-char "'"))))))

(defun awesome-pair-in-string-p (&optional state)
  (save-excursion
    (or (nth 3 (or state (awesome-pair-current-parse-state)))
        (and
         (eq (get-text-property (point) 'face) 'font-lock-string-face)
         (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
        (and
         (eq (get-text-property (point) 'face) 'font-lock-doc-face)
         (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
        )))

(defun awesome-pair-in-comment-p (&optional state)
  (save-excursion
    (or (nth 4 (or state (awesome-pair-current-parse-state)))
        (eq (get-text-property (point) 'face) 'font-lock-comment-face))))

(defun awesome-pair-in-string-escape-p ()
  (let ((oddp nil))
    (save-excursion
      (while (eq (char-before) ?\\ )
        (setq oddp (not oddp))
        (backward-char)))
    oddp))

(defun awesome-pair-in-char-p (&optional argument)
  (let ((argument (or argument (point))))
    (and (eq (char-before argument) ?\\ )
         (not (eq (char-before (1- argument)) ?\\ )))))

(defun awesome-pair-is-blank-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun awesome-pair-only-whitespaces-before-cursor-p ()
  (let ((string-before-cursor
         (buffer-substring
          (save-excursion
            (beginning-of-line)
            (point))
          (point))))
    (equal (length (string-trim string-before-cursor)) 0)))

(defmacro awesome-pair-ignore-errors (body)
  `(ignore-errors
     ,body
     t))

(provide 'awesome-pair)

;;; awesome-pair.el ends here
