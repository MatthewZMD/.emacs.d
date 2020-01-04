;;; evil-nerd-commenter-operator.el --- Provides an evil operator for evil-nerd-commenter

;; Author: Chen Bin <chenbin.sh@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides operators for evil-mode.

;;; Code:

(require 'evil)
(require 'evil-nerd-commenter-sdk)

(defvar evilnc-c-style-comment-modes
  '(awk-mode
    c++-mode
    c-mode
    css-mode
    dart-mode
    ess-mode
    go-mode
    java-mode
    javascript-mode
    js-mode
    js2-mode
    perl-mode
    php-mode
    swift-mode
    web-mode)
  "Major modes using C comment syntax.")

(defvar evilnc-temporary-goal-column 0
  "Value of`temporary-goal-column' specifying right edge of rectangle yank.")

(defadvice evil-visual-highlight-block (around evil-visual-highlight-block-hack activate)
  "Show overlay over innert comment text object."
  ad-do-it
  (when (eq this-command 'evilnc-inner-comment)
    (dolist (overlay evil-visual-block-overlays)
      (let* ((b (overlay-start overlay))
             (e (save-excursion
                  (goto-char (overlay-end overlay))
                  (line-end-position))))
        (move-overlay overlay b e)))))

(defadvice evil-apply-on-block (around evil-apply-on-block-around-hack activate)
  "Yank correct region of inner comment text object."
  (let* ((tmp-command last-command))
    ;; force `evil-apply-on-block' use our temporary-goal-column
    (when (> evilnc-temporary-goal-column 0)
      (setq temporary-goal-column (max temporary-goal-column
                                       evilnc-temporary-goal-column))
      ;; Read `evil-apply-on-block'. Note `temporary-goal-column' is used
      ;; if and only if `last-command' is `next-line' or `previous-line'
      (setq last-command 'next-line))
    ad-do-it
    ;; restore last command
    (setq last-command tmp-command)
    (setq evilnc-temporary-goal-column 0)))

(defun evilnc--in-comment-p (pos)
  "Check whether the code at POS is comment by comparing font face."
  (interactive)
  (let* ((fontfaces (get-text-property pos 'face)))
    (if (not (listp fontfaces))
        (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar #'(lambda (f)
                      ;; learn this trick from flyspell
                      (or (eq f 'font-lock-comment-face)
                          (eq f 'font-lock-comment-delimiter-face)))
                  fontfaces))))

(defun evilnc--extend-to-whole-comment (beg end)
  "Extend the comment region defined by BEG and END so ALL comment is included."
  (interactive)
  (if (evilnc--in-comment-p beg)
      (save-excursion
        (let* ((newbeg beg)
               (newend end))

          ;; extend the beginning
          (goto-char newbeg)
          (while (and (>= (1- newbeg) (line-beginning-position)) (evilnc--in-comment-p (1- newbeg)))
            (setq newbeg (1- newbeg)))

          ;; extend the end
          (goto-char newend)
          (while (and (<= newend (line-end-position)) (evilnc--in-comment-p newend))
            (setq newend (1+ newend)))

          (list newbeg newend)))
    (list beg end)))

(evil-define-operator evilnc-comment-operator (beg end type)
  "Comments text from BEG to END with TYPE."
  (interactive "<R>")
  (cond
   ((eq type 'block)
    (let* ((newpos (evilnc--extend-to-whole-comment beg end) ))
      (evil-apply-on-block #'evilnc--comment-or-uncomment-region
                           (nth 0 newpos)
                           (nth 1 newpos)
                           nil)))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (evilnc--comment-or-uncomment-region (1- beg) end))

   ((eq type 'line)
    ;; comment whole line, for now
    (evilnc--comment-or-uncomment-region beg
                                         (save-excursion
                                           (goto-char (1- end))
                                           (line-end-position))))

   (t
    (when (and beg end)
      (let* ((newpos (evilnc--extend-to-whole-comment beg end)))
        (evilnc--comment-or-uncomment-region (nth 0 newpos) (nth 1 newpos))))))

  ;; place cursor on beginning of line
  (if (and (called-interactively-p 'any) (eq type 'line))
      (evil-first-non-blank)))

(evil-define-operator evilnc-copy-and-comment-operator (beg end)
  "Inserts an out commented copy of the text from BEG to END."
  :move-point (not evilnc-original-above-comment-when-copy-and-comment)
  (interactive "<r>")
  (evil-with-single-undo
    (evil-yank-lines beg end nil 'lines)
    (cond
     (evilnc-original-above-comment-when-copy-and-comment
      (let* ((p (point)))
        (comment-region beg end)
        (goto-char beg)
        (evil-paste-before 1)
        (goto-char p)))
     (t
      (goto-char end)
      (evil-paste-before 1)
      ;; actual comment operatio should happen at last
      ;; or else beg end will be screwed up
      (comment-region beg end)))))

(defun evilnc-is-one-line-comment (b e)
  "Check whether text between B and E is one line comment."
  (save-excursion
    (goto-char b)
    (and (<= (line-beginning-position) b)
         ;; e is the upper limit great than (line-end-position)
         (<= e (1+ (line-end-position))))))

(defun evilnc-get-comment-bounds ()
  "Return bounds like (cons beg end)."
  (let* ((b (point))
         (e (point))
         (col 0)
         rlt)
    ;; extend begin
    (while (evilnc-is-comment (- b 1))
      (setq b (- b 1)))

    ;; extend end
    (while (evilnc-is-comment (+ e 1))
      (setq e (+ e 1)))

    ;; we could select extra spaces at the end of comment
    ;; so we need go back
    (let* ((str (save-excursion
                  (goto-char e)
                  (buffer-substring-no-properties (line-beginning-position) e)))
           (empty-line-p (string-match "^[ \t]*$" str)))
      (if empty-line-p
          ;; empty line plus line feed
          (setq e (- e (length str) 1))))
    (cond
     ((>= b e)
      (setq rlt nil))
     ((evilnc-is-one-line-comment b e)
      ;; contract begin
      (while (not (evilnc-is-pure-comment b))
        (setq b (+ b 1)))

      ;; contract end
      (while (not (evilnc-is-pure-comment e))
        (setq e (- e 1)))

      (if (< b e) (setq rlt (cons b (+ e 1)))))
     (t
      ;; multi-line comment
      (setq rlt (cons b e))))
    rlt))

(defun evilnc-adjusted-comment-end (b e)
  "Adjust comment end of region between B and E."
  (let* ((next-end-char (evilnc-get-char (- e 2)))
         (end-char (evilnc-get-char (- e 1))))
    ;; avoid selecting CR/LF at the end of comment
    (while (and (< b e)
                (memq (evilnc-get-char (- e 1)) '(10 13)))
      (setq e (- e 1)))

    ;; avoid selecting comment limiter
    (cond
     ((and (memq major-mode evilnc-c-style-comment-modes)
           (= end-char ?/)
           (= next-end-char ?*))
      ;; avoid selecting the ending comment limiter "*/"
      (setq e (- e 2))
      (while (and (> e b)
                  (= (evilnc-get-char (- e 1)) ?*))
        (setq e (- e 1))))
     (t
      ;; other languages we can safely use font face
      (while (and (> e b)
                  (evilnc-is-comment-delimiter (- e 1)))
        (setq e (- e 1)))))
    e))

(defun evilnc-is-c-style-comment (pos)
  "Is C style comment at POS?"
  (and (memq major-mode evilnc-c-style-comment-modes)
       (= (evilnc-get-char pos) ?/)
       (= (memq (evilnc-get-char (1+ pos)) '(?/ ?*)))))

(defun evilnc-comment-column-bounds (beg end &optional c-style)
  "From BEG to END find column bounds of rectangle selection.
Return (cons col-min col-max) or nil.  If C-STYLE is t,
we are processing C like language."
  (let* ((col-min most-positive-fixnum)
         (col-max 0))
    (while (< beg end)
      (when (and (not (evilnc-is-whitespace beg))
                 (evilnc-is-pure-comment beg)
                 (not (or (evilnc-is-comment-delimiter beg)
                          (and c-style
                               (memq (evilnc-get-char beg) '(?/ ?*))))))
        (let* ((col (evil-column beg)))
          (if (< col col-min)
              (setq col-min col))
          (if (> col col-max)
              (setq col-max col))))
      (setq beg (1+ beg)))
    (if (< col-min col-max)
        (cons col-min col-max))))

(evil-define-text-object evilnc-inner-comment (&optional count begin end type)
  "An inner comment text object."
  (let* ((bounds (evilnc-get-comment-bounds))
         b
         e
         c-style)
    (cond
     (bounds
      (setq b (car bounds))
      (setq e (cdr bounds))
      (cond
       ((setq c-style (evilnc-is-c-style-comment b))
        (while (and (< b e)
                    (or (evilnc-is-whitespace b)
                        (evilnc-is-line-end b)
                        (memq (evilnc-get-char b) '(?/ ?*))))
          (setq b (1+ b)))
        (while (and (< b e)
                    (or (evilnc-is-whitespace e)
                        (evilnc-is-line-end e)
                        (memq (evilnc-get-char e) '(?/ ?*))))
          (setq e (1- e)))
        (setq e (1+ e))
        (setq b (save-excursion
                  (goto-char b)
                  (forward-word 1)
                  (forward-word -1)
                  (point))))
       (t
        (setq b (save-excursion
                  (goto-char (car bounds))
                  (forward-word 1)
                  (forward-word -1)
                  (point)))
        (setq e (save-excursion
                  (goto-char (cdr bounds))
                  (goto-char (evilnc-adjusted-comment-end b (line-end-position)))
                  (point)))))
      (cond
       ((evilnc-is-one-line-comment b e)
        ;; keep move e to the end of comment
        (evil-range b ;; (if c-style (1+ e) e)
                    e))
       (t
        ;; multi-line comment
        (let* ((col-b (evil-column b))
               (col-bounds (evilnc-comment-column-bounds b e c-style)))
          (cond
           (col-bounds
            (if (> col-b (car col-bounds))
                (setq b (- b (- col-b (car col-bounds)))))
            (setq evilnc-temporary-goal-column (cdr col-bounds)))
           (t
            (setq evilnc-temporary-goal-column (evil-column e)))))
        (evil-range b e 'block :expanded t))))
     (t
      (error "Not inside a comment")))))

(evil-define-text-object evilnc-outer-commenter (&optional count begin end type)
  "An outer comment text object."
  (let* ((bounds (evilnc-get-comment-bounds)))
    (cond
     (bounds
      (let* ((b (car bounds))
             (e (cdr bounds)))
        (evil-range b e 'exclusive :expanded t)))
     (t
      (error "Not inside a comment")))))

(provide 'evil-nerd-commenter-operator)
;;; evil-nerd-commenter-operator.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
