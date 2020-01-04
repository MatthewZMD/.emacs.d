;;; ess-noweb-font-lock-mode.el --- edit noweb files with GNU Emacs

;; Copyright (C) 1999 by  Adnan Yaqub (AYaqub@orga.com)
;;                    and Mark Lunt (mark.lunt@mrc-bsu.cam.ac.uk
;; Copyright (C) 2002 by A.J. Rossini <rossini@u.washington.edu>
;; Copyright (C) 2003--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/
;;

;;; Commentary:

;; Code-dependent highlighting
;;  *****
;;
;;  Adding highlighting to ess-noweb-mode.el
;;
;;  Here is a description of how one can add highlighting via the
;;  font-lock package to noweb buffers.  It uses the hooks provided by
;;  ess-noweb-mode.el.  The solution provides the following features:
;;  1) The documentation chunks are highlighted in the ess-noweb-doc-mode
;;  (e.g., LaTeX).
;;  2) The code chunks without mode comments (-*- mode -*-) are
;;  highlighted in the ess-noweb-code-mode.
;;  3) The code chunks with mode comments (-*- mode -*-) on the first
;;  line of the first chunk with this name are highlighted in the mode
;;  in the comment.
;;
;;  For example, given the file:
;;
;;    % -*- mode: Noweb; ess-noweb-code-mode: c-mode -*-
;;
;;    \begin{itemize}
;;    \item a main routine written in C,
;;    \item a log configuration file parser written in YACC, and
;;    \item a lexical analyzer written in Lex.
;;    \end{itemize}
;;
;;    <<warning c comment>>=
;;    /* DO NOT EDIT ME! */
;;    /* This file was automatically generated from %W% (%G%). */
;;    @
;;
;;    <<warning nroff comment>>=
;;    .\" -*- nroff -*-
;;    .\" DO NOT EDIT ME!
;;    .\" This file was automatically generated from %W% (%G%).
;;    @
;;
;;  The LaTeX list is highlighted in latex-mode (the default noweb doc
;;  mode), the chunk <<warning c comment>> is highlighted in c-mode (the
;;  default noweb code mode), and the chunk <<warning nroff comment>> is
;;  highlighted in nroff-mode due to the "-*- nroff -*-" comment.
;;
;;  Chunks are highlighted each time point moves into them from a
;;  different mode. They are also fontified 'on the fly', but this is
;;  less reliable, since the syntax can depend on the context. It's as
;;  good as you would get outside ess-noweb-mode, though.
;;
;;  To use it, you must add
;;  (require 'ess-noweb-font-lock-mode) to your .emacs file.
;;  Then, if you use either global-font-lock or turn-on-font-lock
;;  statements, any ess-noweb-mode buffers will be fontified
;;  appropriately. (We have to redefine turn-on-font-lock, but it
;;  saves breaking other packages (in particular ESS, which I use a
;;  lot), that assume that turn-on-font-lock is the way to turn on
;;  font locking.

;;  Alternatively, you can turn ess-noweb-font-lock-mode on and off by
;;  using M-x ess-noweb-font-lock-mode. However, turning
;;  ess-noweb-font-lock-mode off when global-font-lock-mode is t makes it
;;  impossible to use font-locking in that buffer subsequently, other
;;  than by turning ess-noweb-font-lock-mode back on.

;;  2) The highlighting sometimes get confused, but this is no longer
;;  a noweb problem. Highlighting should work as well within a chunk
;;  as it does without ess-noweb-mode.
;;  There are some problems with, for example latex-mode: a `$' in a
;;  verbatim environment with throw the font-locking out.
;;  One slight blemish is that code-quotes are highlighted as comments
;;  as they are being entered. They are only highlighted correctly
;;  after `ess-noweb-font-lock-fontify-chunk' has been run, either as a
;;  command or through changing to a different chunk and back again
;;  (unless they lie on a single line, in which case they are
;;  fontified correctly once they are completed).

;;; Code:

(require 'ess-noweb-mode)
(require 'font-lock)

(defvar ess-noweb-font-lock-mode nil
  "Buffer local variable, t iff this buffer is using `ess-noweb-font-lock-mode'.")

(defvar ess-noweb-use-font-lock-mode t
  "DO NOT CHANGE THIS VARIABLE
If you use `nw-turn-on-font-lock' to turn on font-locking, then turn it
off again, it would come back on again of its own accord when you
changed major-mode. This variable is used internally to stop it.")

(defvar ess-noweb-font-lock-mode-hook nil
  "Hook that is run after entering ess-noweb-font-lock mode.")

(defvar ess-noweb-font-lock-max-initial-chunks 30
  "Maximum number of chunks to fontify initially.
If nil, will fontify the entire buffer when
ess-noweb-font-lock-initial-fontify-buffer is called" )

;; (defvar old-beginning-of-syntax nil
;;   "Stores the function used to find the beginning of syntax in the
;; current major mode. ess-noweb-font-lock-mode needs a different one." )

(defvar ess-noweb-font-lock-doc-start-face font-lock-reference-face
  "Face to use to highlight the `@' at the start of each doc chunk")

(defvar ess-noweb-font-lock-brackets-face font-lock-reference-face
  "Face to use to highlight `<<', `>>' `[[' and `]]' ")

(defvar ess-noweb-font-lock-chunk-name-face font-lock-keyword-face
  "Face to use to highlight the between `<<' and `>>'")

(defvar ess-noweb-font-lock-code-quote-face font-lock-keyword-face
  "Face to use to highlight the between `[[' and `]]'")

;; Now we add [[ess-noweb-font-lock-mode]] to the list of existing minor
;; modes. The string ``NWFL'' will be added to the mode-line: ugly, but
;; brief.

(if (not (assq 'ess-noweb-font-lock-mode minor-mode-alist))
    (setq minor-mode-alist (append minor-mode-alist
                                   (list '(ess-noweb-font-lock-mode " NWFL")))))

;; An ugly kludge to get around problems with global-font-lock, which
;; fontifies the entire buffer in the new major mode every time you
;; change mode, which is time-consuming and makes a pigs trotters of
;; it. Trying to stop it looks tricky, but using this function as your
;; `font-lock-fontify-buffer' function stops it wasting your time

(defalias 'nwfl-donowt #'ignore)

;; The following function is just a wrapper for ess-noweb-font-lock-mode,
;; enabling it to be called as ess-noweb-font-lock-minor-mode instead.

(define-obsolete-function-alias 'ess-noweb-font-lock-minor-mode
  #'ess-noweb-font-lock-mode "ESS-16.11")

;; Here we get to the meat of the problem
;;;###autoload
(defun ess-noweb-font-lock-mode ( &optional arg)
  ;; FIXME: Don't define a new minor mode.  Instead, arrange for normal
  ;; font-lock to call our functions such as
  ;; ess-noweb-font-lock-fontify-chunk-by-number.
  "Minor mode for syntax highlighting when using `ess-noweb-mode' to edit noweb files.
Each chunk is fontified in accordance with its own mode."
  (interactive "P")
  (if (or ess-noweb-mode ess-noweb-font-lock-mode)
      (progn
        ;; This bit is tricky: copied almost verbatim from bib-cite-mode.el
        ;; It seems to ensure that the variable ess-noweb-font-lock-mode is made
        ;; local to this buffer. It then sets ess-noweb-font-lock-mode to `t' if
        ;;     1) It was called with a prefix argument greater than 0
        ;; or  2) It was called with no argument, and ess-noweb-font-lock-mode is
        ;;        currently nil
        ;; ess-noweb-font-lock-mode is nil if the prefix argument was <= 0 or there
        ;; was no prefix argument and ess-noweb-font-lock-mode is currently `t'
        (set (make-local-variable 'ess-noweb-font-lock-mode)
             (if arg
                 (> (prefix-numeric-value arg) 0)
               (not ess-noweb-font-lock-mode)))
        ;; Now, if ess-noweb-font-lock-mode is true, we want to turn
        ;; ess-noweb-font-lock-mode on
        (cond
          (ess-noweb-font-lock-mode                 ;Setup the minor-mode
           (when (and (boundp 'global-font-lock-mode) global-font-lock-mode)
             (mapc #'ess-noweb-make-variable-permanent-local
                   '(font-lock-fontify-buffer-function
                     font-lock-unfontify-buffer-function))
             (setq font-lock-fontify-buffer-function #'nwfl-donowt)
             (setq font-lock-unfontify-buffer-function #'nwfl-donowt))
           (mapc #'ess-noweb-make-variable-permanent-local
                 '(ess-noweb-font-lock-mode
                   font-lock-dont-widen
                   ;; font-lock-beginning-of-syntax-function
                   syntax-begin-function
                   ess-noweb-use-font-lock-mode
                   after-change-functions))
           (setq ess-noweb-font-lock-mode t
                 font-lock-dont-widen t)
           (add-hook 'after-change-functions
                     #'font-lock-after-change-function nil t)
           ;; FIXME: Why add ess-noweb-font-lock-mode-fn to our own hook,
           ;; instead of just calling ess-noweb-font-lock-mode-fn directly?
           (add-hook 'ess-noweb-font-lock-mode-hook #'ess-noweb-font-lock-mode-fn)
           (add-hook 'ess-noweb-changed-chunk-hook
             #'ess-noweb-font-lock-fontify-this-chunk)
           (run-hooks 'ess-noweb-font-lock-mode-hook)
           (message "ess-noweb-font-lock mode: use `M-x ess-noweb-font-lock-describe-mode' for more info"))
         ;; If we didn't do the above, then we want to turn ess-noweb-font-lock-mode
         ;; off, no matter what (hence the condition `t')
          (t
           (when (and (boundp 'global-font-lock-mode) global-font-lock-mode)
             ;; (setq font-lock-fontify-buffer-function
             ;;       'font-lock-default-fontify-buffer)
             ;; Get back our unfontify buffer function
             (setq font-lock-unfontify-buffer-function
                   #'font-lock-default-unfontify-buffer))
           (remove-hook 'ess-noweb-font-lock-mode-hook #'ess-noweb-font-lock-mode-fn)
           (remove-hook 'ess-noweb-changed-chunk-hook
                        #'ess-noweb-font-lock-fontify-this-chunk)
           (remove-hook 'after-change-functions
                        #'font-lock-after-change-function )
           (font-lock-default-unfontify-buffer)
           (setq ess-noweb-use-font-lock-mode nil)
           (message "ess-noweb-font-lock-mode removed"))))
    (message "ess-noweb-font-lock-mode can only be used with ess-noweb-mode")))

(defun ess-noweb-start-of-syntax ()
  "Go to the place to start fontifying from"
  (interactive)
  (goto-char (car (ess-noweb-chunk-region))))

(defvar font-latex-extend-region-functions)
(defvar syntax-begin-function)

(defun ess-noweb-font-lock-fontify-chunk-by-number ( chunk-num )
  "Fontify chunk CHUNK-NUM based on the current major mode."
  (save-excursion
    (font-lock-set-defaults)
    ;; (setq old-beginning-of-syntax font-lock-beginning-of-syntax-function)
    (setq font-lock-keywords
          ;;         (append font-lock-keywords
          ;;                 '(("\\(\\[\\[\\)\\([^]]*\\]*\\)\\(\\]\\]\\|\\$\\)"
          ;;                    (1 ess-noweb-font-lock-brackets-face prepend )
          ;;                    (2 ess-noweb-font-lock-code-quote-face prepend)
          ;;                    (3 ess-noweb-font-lock-brackets-face prepend))
          ;;                   ("^[ \t\n]*\\(<<\\)\\([^>]*\\)\\(>>=?\\)"
          ;;                    (1 ess-noweb-font-lock-brackets-face  prepend )
          ;;                    (2 ess-noweb-font-lock-chunk-name-face prepend)
          ;;                    (3 ess-noweb-font-lock-brackets-face prepend))
          ;;                   ("^@[ \t\n]+"
          ;;                    (0 ess-noweb-font-lock-doc-start-face prepend )))))
          (append font-lock-keywords
                  '(("^[ \t\n]*\\(<<\\)\\([^>]*\\)\\(>>=?\\)"
                     ;; FIXME: Why not use ess-noweb-font-lock-brackets-face?
                     (1 font-lock-reference-face  prepend )
                     ;; FIXME: Why not use ess-noweb-font-lock-chunk-name-face?
                     (2 font-lock-keyword-face prepend)
                     (3 font-lock-reference-face prepend))
                    ("^@[ \t\n]+"
                     (0 font-lock-reference-face prepend )))))


    (let ((r (cons (marker-position (cdr (aref ess-noweb-chunk-vector
                                               chunk-num)))
                   (marker-position (cdr (aref ess-noweb-chunk-vector
                                               (1+ chunk-num))))))
          (font-latex-extend-region-functions nil);; don't extend anything
          (font-lock-extend-region-functions nil)) ;; this infloops :(
      (save-restriction
        (narrow-to-region (car r) (cdr r))
        ;; (sit-for 3)
        (font-lock-fontify-region (car r) (cdr r)))
      t)))


(defun ess-noweb-font-lock-fontify-this-chunk ()
  "Fontify this chunk according to its own major mode.
Since we are in the chunk, the major mode will already have been set
by ess-noweb-mode.el"
  (interactive)
  (ess-noweb-font-lock-fontify-chunk-by-number (ess-noweb-find-chunk-index-buffer)))

(defun ess-noweb-font-lock-initial-fontify-buffer ()
  "Applies syntax highlighting to some or all chunks in a noweb buffer.
The number of chunks is set by `ess-noweb-font-lock-max-initial-chunks': if
this is nil, the entire buffer is fontified.
It is intended to be called when first entering `ess-noweb-font-lock-mode'.
For other purposes, use `ess-noweb-font-lock-fontify-chunks'."
  (interactive)
  ;; This will be tricky. It will be very slow to go throught the chunks
  ;; in order, switching major modes all the time.
  ;; So, we will do the documentation in one pass, the code in a second
  ;; pass. This could still be a little slow if we have to swap between
  ;; different code modes regularly, but it should be bearable. It should
  ;; only happen when the file is first read in, anyway
  (save-excursion
    (let (start-chunk end-chunk this-chunk)
      (setq this-chunk (ess-noweb-find-chunk-index-buffer))
      (if ess-noweb-font-lock-max-initial-chunks
          (progn
            (setq start-chunk
                  (max 0
                       (- this-chunk
                          (/ ess-noweb-font-lock-max-initial-chunks 2))))
            ;; Don't you just love hairy lisp syntax ? The above means set the
            ;; starting chunk to the current chunk minus half of
            ;; ess-noweb-font-lock-max-initial-chunks, unless that is negative in
            ;; which case set it to 0
            (setq end-chunk (+ start-chunk ess-noweb-font-lock-max-initial-chunks))
            (if (> end-chunk (- (length ess-noweb-chunk-vector) 2))
                (setq end-chunk (- (length ess-noweb-chunk-vector) 2))))
        ;; If ess-noweb-font-lock-max-initial-chunks is nil, do the whole buffer
        (progn
          (setq start-chunk 0)
          (setq end-chunk (- (length ess-noweb-chunk-vector) 2))))
      (ess-noweb-font-lock-fontify-chunks start-chunk end-chunk))))

(defun ess-noweb-font-lock-fontify-buffer ()
  "This function will fontify each chunk in the buffer appropriately."
  (interactive)
  (let ((start-chunk 0)
        (end-chunk (- (length ess-noweb-chunk-vector) 2)))
    (ess-noweb-font-lock-fontify-chunks start-chunk end-chunk)))

(defun ess-noweb-font-lock-fontify-chunks (start-chunk end-chunk)
  "Fontify a noweb file from START-CHUNK to END-CHUNK."
  (interactive)
  (let ((chunk-counter start-chunk))
    (save-excursion
      (message "Fontifying from %d to %d" start-chunk end-chunk)
      ;; Want to set DOC mode for the first Doc chunk, not for the others
      (while  (stringp (car (aref ess-noweb-chunk-vector chunk-counter)))
        (setq chunk-counter (+ chunk-counter 1)))
      (goto-char (cdr (aref ess-noweb-chunk-vector chunk-counter)))
      (ess-noweb-select-mode)
      ;; Now go through the chunks, fontifying the documentation ones.
      (while (<= chunk-counter end-chunk)
        (if  (not (stringp (car (aref ess-noweb-chunk-vector chunk-counter))))
            (ess-noweb-font-lock-fontify-chunk-by-number chunk-counter))
        (message "Fontifying documentation chunks: chunk %d" chunk-counter)
        (setq chunk-counter (+ 1 chunk-counter)))
      ;; Go back to the start and go through the chunks, fontifying the code ones.
      (setq chunk-counter start-chunk)
      (message "About to do code chunks")
      (while (<= chunk-counter end-chunk)
        (when (stringp (car (aref ess-noweb-chunk-vector chunk-counter)))
          ;; It's a code chunk: goto it to set the correct code mode, then
          ;; fontify it.
          (message "Fontifying code chunks: chunk %d" chunk-counter)
          (goto-char (cdr (aref ess-noweb-chunk-vector chunk-counter)))
          (ess-noweb-select-mode)
          (ess-noweb-font-lock-fontify-this-chunk))
        (setq chunk-counter (1+ chunk-counter))))
    (ess-noweb-select-mode)))

(defun ess-noweb-font-lock-mode-fn()
  "Function that is intended to be attached to ess-noweb-font-lock-mode-hook."
  (ess-noweb-font-lock-initial-fontify-buffer))

;; This is a wee bit of a hack. If people attach `turn-on-font-lock'
;; to their major mode hook, it will play hell with
;; ess-noweb-font-lock-mode. I had hoped that providing a replacement
;; `nw-turn-on-font-lock' would solve the problem, but it didn't
;; (sometimes turn-on-font-lock appears in places other than
;; `.emacs', such as in ESS). So rather than have it fall over if
;; turn-on-lock was around, I redefined turn-on-font-lock to do the
;; right thing.

(define-obsolete-function-alias 'nw-turn-on-font-lock
  #'turn-on-font-lock "ESS-16.11")

(defadvice turn-on-font-lock (around ess-noweb-font-lock activate)
  ;; FIXME: An advice on turn-on-font-lock is definitely not sufficient,
  ;; since font-lock can be enabled without going through turn-on-font-lock!
  (if (not ess-noweb-mode)
      ad-do-it
    (if (and (not ess-noweb-font-lock-mode) ess-noweb-use-font-lock-mode)
        (ess-noweb-font-lock-mode))))

(provide 'ess-noweb-font-lock-mode)
;;  *****
;;
;;  Adnan Yaqub (AYaqub@orga.com)
;;  ORGA Kartensysteme GmbH // An der Kapelle 2 // D-33104 Paderborn // Germany
;;  Tel. +49 5254 991-823 //Fax. +49 5254 991-749


;;; ess-noweb-font-lock-mode.el ends here
