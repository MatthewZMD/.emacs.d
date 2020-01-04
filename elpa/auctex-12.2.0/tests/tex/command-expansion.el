;;; command-expansion.el --- tests for TeX command expansion

;; Copyright (C) 2014, 2018 Free Software Foundation, Inc.

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

(require 'ert)
(require 'tex-buf)

(ert-deftest TeX-command-expansion ()
  "Check whether \"%%%%\" is correctly expanded when before \"%`\"."
  (should (string=
           (let ((TeX-command-list
		  (list (cons "Test" '("%%%% %`%'" TeX-run-command t t)))))
	     (TeX-command-expand (nth 1 (assoc "Test" TeX-command-list))
				 'TeX-master-file))
           "%% ")))

(ert-deftest TeX-command-expansion-errors ()
  "Test error handling in `TeX-command-expand'."
  (should-error
   ;; This error is actually thrown by `TeX-engine-in-engine-alist', but we want
   ;; to be sure that `TeX-command-expand' fails when the engine is not valid.
   (let ((TeX-engine 'non-existing-engine))
     (TeX-command-expand "%l" 'TeX-master-file))))

(ert-deftest TeX-view-command-raw-errors ()
  "Tests to trigger errors in `TeX-view-command-raw'."
  ;; Viewer specification should be either a command line string or a Lisp
  ;; function name to be executed.  This test makes sure that the functions
  ;; throws an error if the selected viewer has a wrong specification (for
  ;; example a function call, not the function name) such that the returned
  ;; value `command' isn't a string.  This prevents an infinite loop in
  ;; `TeX-command-expand'.
  (should-error
   (with-temp-buffer
     (let ((TeX-view-program-list '(("viewer"
				     (wrong-specification))))
	   (TeX-view-program-selection
	    '((output-pdf "viewer"))))
       (TeX-mode)
       (TeX-view-command-raw)))
   :type 'error)
  ;; Signal an error when a nonexistent viewer is selected.
  (should-error
   (with-temp-buffer
     (let ((TeX-view-program-selection
	    '((output-pdf "does-not-exist"))))
       (TeX-mode)
       (TeX-view-command-raw)))
   :type 'error)
  ;; Signal an error if the binary associated to the viewer cannot be found.
  (should-error
   (with-temp-buffer
     (let ((TeX-view-program-list
	    '(("viewer" "viewer %o" "**this-program-does-not-exist**")))
	   (TeX-view-program-selection
	    '((output-pdf "viewer"))))
       (TeX-mode)
       (TeX-view-command-raw)))
   :type 'error)
  ;; Error if there is no selected viewer for current buffer.
  (should-error
   (with-temp-buffer
     (let (TeX-view-program-selection)
       (TeX-mode)
       (TeX-view-command-raw)))
   :type 'error))

(ert-deftest TeX-command-detokenize ()
  "Check whether \"\\input\" and \"\\detokenize\" are supplied when necessary."
  ;; Skip on w32 because the quoting style of `shell-quote-argument'
  ;; is different.
  (skip-unless (not (eq system-type 'windows-nt)))
  (should (string=
           (let ((major-mode 'latex-mode)
		 (TeX-engine 'default)
		 (TeX-master "/tmp/abc")
		 (TeX-command-extra-options " \"\\foo\""))
	     (TeX-command-expand "%`%(extraopts)%' %T" #'TeX-master-file))
	   " \"\\foo\" \"\\input\" \\\\detokenize\\{\\ abc.tex\\ \\}")))

(ert-deftest TeX-command-expand-skip-file-name ()
  "Check whether file name is not subject to further expansion.
File names obtained as expansion of \"%t\", \"%s\" and so on should be
skipped for the following expansion to avoid possible endless loop.
See <https://lists.gnu.org/r/bug-auctex/2014-08/msg00012.html>."
  ;; Skip on w32 because the quoting style of `shell-quote-argument'
  ;; is different.
  (skip-unless (not (eq system-type 'windows-nt)))
  (let ((TeX-master "abc-def")
	(TeX-expand-list '(("-" (lambda () ":")))))
    (should (string=
	     (TeX-command-expand "%s" #'TeX-master-file)
	     TeX-master))
    (should (string=
	     (TeX-command-expand "%t" #'TeX-master-file)
	     (TeX-master-file "tex" t)))
    (should (string=
	     (TeX-command-expand "%T" #'TeX-master-file)
	     (TeX-master-file "tex" t)))
    (should (string=
	     (TeX-command-expand "%d" #'TeX-master-file)
	     (TeX-master-file "dvi" t)))
    (should (string=
	     (TeX-command-expand "%f" #'TeX-master-file)
	     (TeX-master-file "ps" t)))
    ;; The expander of "%o" does not yet cater for this possible endless
    ;; loop.
    ;; (should (string=
    ;; 	     (TeX-command-expand "%o" #'TeX-master-file)
    ;; 	     (TeX-master-file "pdf" t)))
    ))

(ert-deftest TeX-command-expand-active-master ()
  "Test whether `TeX-active-master' is valid argument for `TeX-command-expand'."
  ;; Skip on w32 because the quoting style of `shell-quote-argument'
  ;; is different.
  (skip-unless (not (eq system-type 'windows-nt)))
  (let ((TeX-master "abc")
	TeX-current-process-region-p)
    (setq TeX-current-process-region-p nil)
    (should (string=
	     (TeX-command-expand "%s" #'TeX-active-master)
	     TeX-master))
    (setq TeX-current-process-region-p t)
    (should (string=
	     (TeX-command-expand "%s" #'TeX-active-master)
	     TeX-region))))

;;; command-expansion.el ends here
