;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani
;; Copyright (C) 2018 Fangrui Song

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'ccls-common)

(defgroup ccls-code-lens nil
  "ccls code lens."
  :group 'tools
  :group 'ccls)

(defcustom ccls-code-lens-position 'end
  "The position to put code lens overlays."
  :type '(choice (const end) (const inplace))
  :group 'ccls-code-lens)

(defface ccls-code-lens-face
  '((t :inherit shadow))
  "The face used for code lens overlays."
  :group 'ccls-code-lens)

(defface ccls-code-lens-mouse-face
  '((t :box t))
  "The face used for code lens overlays."
  :group 'ccls-code-lens)

;; ---------------------------------------------------------------------
;;   Codelens
;;
;;   Enable by calling `ccls-request-code-lens'
;;   Clear them away using `ccls-clear-code-lens'
;;
;;   TODO:
;;   - Find a better way to display them.
;;
;;   - Instead of adding multiple lenses to one symbol, append the text
;;     of the new one to the old. This will fix flickering when moving
;;     over lenses.
;;
;;   - Add a global option to request code lenses on automatically
;; ---------------------------------------------------------------------

(defun ccls--make-code-lens-string (lpad command &optional rpad)
  "."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      (lambda () (interactive)
        (when-let ((xrefs (lsp--locations-to-xref-items
                           (lsp--send-execute-command (gethash "command" command) (gethash "arguments" command)))))
          (xref--show-xrefs xrefs nil))))
    (propertize (concat lpad (gethash "title" command) rpad)
                'face 'ccls-code-lens-face
                'mouse-face 'ccls-code-lens-mouse-face
                'local-map map)))

(defun ccls--code-lens-callback (result)
  "."
  (overlay-recenter (point-max))
  (ccls-clear-code-lens)
  (setq
   result
   (seq-sort
    (lambda (x y)
      (let ((xl (aref x 2)) (yl (aref y 2)))
        (if (/= xl yl) (< xl yl) (< (aref x 3) (aref y 3)))))
    (seq-map (lambda (lens)
               (-let* (((&hash "command" command "range" range) lens)
                       ((&hash "start" start "end" end) range))
                 (vector (gethash "line" start) (gethash "character" start)
                         (gethash "line" end) (gethash "character" end) command)
                 )) result)))
  (save-excursion
    (widen)
    (goto-char 1)
    (let ((line 0) (col 0) ov)
      (seq-doseq (lens result)
        (-let (([l0 c0 l1 c1 command] lens) (pad " "))
          (pcase ccls-code-lens-position
            ('end
             (forward-line (- l0 line))
             (if (and ov (= l0 line))
                 (overlay-put ov 'display
                              (concat (overlay-get ov 'display)
                                      (ccls--make-code-lens-string (if (/= c0 col) "|" " ") command)))
               (when ov
                 (overlay-put ov 'display (concat (overlay-get ov 'display) "\n")))
               (let ((p (point-at-eol)))
                 (setq ov (make-overlay p (1+ p) nil 'front-advance))
                 (overlay-put ov 'ccls-code-lens t)
                 (overlay-put ov 'display (ccls--make-code-lens-string " " command))))
             (setq line l0 col c0))
            ('inplace
             (forward-line (- l1 line))
             (forward-char c1)
             (setq line l1)
             (setq ov (make-overlay (point) (point)))
             (overlay-put ov 'ccls-code-lens t)
             (overlay-put ov 'after-string (ccls--make-code-lens-string " " command)))))
        )
      (when (and (eq ccls-code-lens-position 'end) ov)
        (overlay-put ov 'display (concat (overlay-get ov 'display) "\n"))))))

(defun ccls-request-code-lens (&optional buffer)
  "Request code lens from ccls."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (lsp--cur-workspace-check)
    (lsp--send-request-async
     (lsp--make-request "textDocument/codeLens"
                        `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))))
     #'ccls--code-lens-callback)))

(defun ccls-clear-code-lens ()
  "Clear all code lenses from this buffer."
  (interactive)
  (remove-overlays nil nil 'ccls-code-lens t))

(defun ccls-code-lens--request-when-idle ()
  (run-with-idle-timer 0.5 nil #'ccls-request-code-lens (current-buffer)))

(define-minor-mode ccls-code-lens-mode
  "toggle code-lens overlays"
  :group 'ccls
  :global nil
  :init-value nil
  :lighter "Lens"
  (cond
   (ccls-code-lens-mode
    (when (lsp-workspaces)
      (ccls-request-code-lens)
      (add-hook 'lsp-after-diagnostics-hook 'ccls-code-lens--request-when-idle t t)))
   (t
    (remove-hook 'lsp-after-diagnostics-hook 'ccls-code-lens--request-when-idle t)
    (ccls-clear-code-lens))))

(provide 'ccls-code-lens)
