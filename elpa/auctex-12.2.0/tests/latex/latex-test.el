;;; latex-test.el --- tests for LaTeX mode

;; Copyright (C) 2014--2018 Free Software Foundation, Inc.

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
(require 'latex)

(AUCTeX-set-ert-path
 'LaTeX-indent-tabular-test/in
 "tabular-in.tex"
 'LaTeX-indent-tabular-test/out
 "tabular-out.tex"
 'LaTeX-filling/in
 "latex-filling-in.tex"
 'LaTeX-filling/out
 "latex-filling-out.tex"
 'LaTeX-math-indent/in
 "math-indent-in.tex"
 'LaTeX-math-indent/out
 "math-indent-out.tex"
 'tabular-count-ampersands/in
 "tabular-count-ampersands-in.tex"
 'tabular-count-ampersands/out
 "tabular-count-ampersands-out.tex")

;; Test for detecting \& in a table cell added; see
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26010
;; Test for missing & in row added; see
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26032
(ert-deftest LaTeX-indent-tabular ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/in)
             (LaTeX-mode)
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/out)
             (buffer-string)))))

;; Another test for indentation, but for math mode, see
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20227 Let's keep those tests
;; separated so it would be easier to find the culprit of a future failure.
(ert-deftest LaTeX-math-indent ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-math-indent/in)
             (LaTeX-mode)
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-math-indent/out)
             (buffer-string)))))

;; Test LaTeX code with math modes is indented as expected.  This has mostly to
;; do with the value of `LaTeX-fill-break-at-separators' and how
;; `LaTeX-fill-move-to-break-point' handles it.  If the test fails, try to look
;; there.  The second part of the test looks for unambiguousness of
;; macros starting a paragraph
;; (http://lists.gnu.org/archive/html/auctex/2017-03/msg00009.html)
(ert-deftest LaTeX-filling ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-filling/in)
             (LaTeX-mode)
	     (let ((fill-column 70))
	       (fill-paragraph)
	       (let ((cmds '("captionsetup" "caption"
			     "parencite"    "par")))
		 (dolist (cmd cmds)
		   (search-forward (concat "\\" cmd))
		   (save-excursion
		     (end-of-line 0)
		     (fill-paragraph)))))
	     (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-filling/out)
             (buffer-string)))))

;; Test for bug#19281 (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19281):
;; make sure AUCTeX is able to insert and modify an environment containing a
;; TeX-esc and braces in its name.
(ert-deftest LaTeX-change-environment-with-esc ()
  (should (string=
           (with-temp-buffer
             (LaTeX-mode)
	     (LaTeX-insert-environment (concat TeX-esc "foo{bar}"))
	     (LaTeX-modify-environment "foobar")
             (buffer-string))
           (with-temp-buffer
             (LaTeX-mode)
	     (LaTeX-insert-environment "foobar")
             (buffer-string)))))

;; Test for inserting &'s with `M-RET' in various tabular environment.
;; Following styles are loaded: tabularx, tabulary, longtable,
;; dcolumn, siunitx
(ert-deftest LaTeX-count-ampersands-inserted-in-tabular ()
  (should (string=
	   (with-temp-buffer
	     (insert-file-contents tabular-count-ampersands/in)
	     (setq TeX-parse-self t)
	     (LaTeX-mode)
	     (goto-char (point-min))
	     ;; Do not ask for opt. argument in (TeX-insert-macro "\\"):
	     (let ((TeX-insert-macro-default-style 'mandatory-args-only))
	       (while (search-forward "LaTeX-insert-item" nil t)
		 (LaTeX-insert-item)))
	     (buffer-string))
	   (with-temp-buffer
	     (insert-file-contents tabular-count-ampersands/out)
	     (LaTeX-mode)
	     (buffer-string)))))

(ert-deftest LaTeX-addbibresource ()
  "Check parsing of bibliography files added with addbibresource.

In particular, make sure dots are treated correctly and only the
last extension is stripped."
  (should
   (equal
    (with-temp-buffer
      (insert "\\addbibresource{../foo-1.bar_2.qux3.ext}")
      (LaTeX-mode)
      (let ((TeX-parse-self t))
	(TeX-update-style t))
      (LaTeX-bibliography-list))
    '(("../foo-1.bar_2.qux3")))))

(ert-deftest LaTeX-auto-class-regexp ()
  "Check parsing optional argument with comment correctly.

Test against RequirePackage."
  (with-temp-buffer
    (insert "\\RequirePackage[
backend=biber % here is a comment
]{biblatex}
")
    (latex-mode)
    (let ((TeX-parse-self t))
      (TeX-update-style t))
    (should (member "biblatex" (TeX-style-list)))
    (should (LaTeX-provided-package-options-member
	     "biblatex" "backend=biber"))))

(ert-deftest LaTeX-includegraphics-extensions ()
  "Check correct extensions are generated accoding to `TeX-engine'."
  ;; Emacs 26.1 has a bug in byte compile optimization, which makes
  ;; compiled `LaTeX-includegraphics-extensions-list' to return wrong
  ;; value when `TeX-engine' is neither `default', `xetex' nor
  ;; `luatex'.
  ;; c.f. https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31718
  :expected-result (if (and (= emacs-major-version 26)
			    (= emacs-minor-version 1))
		       :failed
		     :passed)
  (with-temp-buffer
    (LaTeX-mode)
    (TeX-load-style "graphicx")
    (let (TeX-engine TeX-PDF-mode TeX-PDF-from-DVI
		     TeX-PDF-via-dvips-ps2pdf TeX-DVI-via-PDFTeX)
      ;; tests for default engine
      (setq TeX-engine 'default)
      ;; default 1
      (setq TeX-PDF-mode t
	    TeX-PDF-from-DVI nil
	    TeX-DVI-via-PDFTeX nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("png" "pdf" "jpe?g" "jbig2" "jb2" "mps"
		      "PNG" "PDF" "JPE?G" "JBIG2" "JB2" "eps") #'string<)))
      ;; default 2
      (setq TeX-PDF-mode t
	    TeX-PDF-from-DVI "Dvips"
	    TeX-DVI-via-PDFTeX nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("eps" "mps" "EPS") #'string<)))
      ;; default 3
      (setq TeX-PDF-mode nil
	    TeX-PDF-from-DVI nil
	    TeX-DVI-via-PDFTeX nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("eps" "mps" "EPS") #'string<)))
      ;; default 4
      (setq TeX-PDF-mode nil
	    TeX-PDF-from-DVI nil
	    TeX-DVI-via-PDFTeX t)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("png" "pdf" "jpe?g" "jbig2" "jb2" "mps"
		      "PNG" "PDF" "JPE?G" "JBIG2" "JB2" "eps") #'string<)))
      ;; default 5
      (setq TeX-PDF-mode t
	    TeX-PDF-from-DVI "Dvipdfmx"
	    TeX-DVI-via-PDFTeX nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("eps" "mps" "EPS" "jpe?g" "pdf" "png") #'string<)))

      ;; tests for luatex engine
      (setq TeX-engine 'luatex)
      ;; luatex 1
      (setq TeX-PDF-mode t)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("png" "pdf" "jpe?g" "jbig2" "jb2" "mps"
		      "PNG" "PDF" "JPE?G" "JBIG2" "JB2" "eps") #'string<)))
      ;; luatex 2
      (setq TeX-PDF-mode nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("eps" "mps" "EPS") #'string<)))

      ;; test for xetex engine
      (setq TeX-engine 'xetex)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("pdf" "eps" "mps" "ps" "png" "jpe?g" "jp2" "jpf"
		      "PDF" "EPS" "MPS" "PS" "PNG" "JPE?G" "JP2" "JPF"
		      "bmp" "pict" "psd" "mac" "tga" "gif" "tif" "tiff"
		      "BMP" "PICT" "PSD" "MAC" "TGA" "GIF" "TIF" "TIFF")
		    #'string<)))

      ;; test for other engine
      (setq TeX-engine 'omega)
      ;; other 1
      (setq TeX-PDF-mode t
	    TeX-PDF-from-DVI "Dvipdfmx")
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("eps" "mps" "EPS" "jpe?g" "pdf" "png") #'string<)))
      ;; other 2
      (setq TeX-PDF-mode nil
	    TeX-PDF-from-DVI nil)
      (should
       (equal (sort (LaTeX-includegraphics-extensions-list) #'string<)
	      (sort '("eps" "jpe?g" "pdf" "png") #'string<))))))

(ert-deftest LaTeX-style-hook-with-class-option ()
  "Check style hooks associated with class option are processed."
  (with-temp-buffer
    (let ((TeX-parse-self t))
      ;; test for dvips option
      ;; This depends on the following code in latex.el:
      ;; (TeX-add-style-hook "dvips"
      ;;		      (lambda ()
      ;;			(setq TeX-PDF-from-DVI "Dvips"))
      ;;		      :classopt)
      (insert "\\documentclass[dvips]{article}\n")
      (latex-mode)
      (TeX-update-style)
      (should (equal (TeX-PDF-from-DVI) "Dvips"))
      (should (not (member "dvips" TeX-active-styles)))

      ;; test for dvipdfmx option
      (erase-buffer)
      ;; This depends on the following code in latex.el:
      ;; (TeX-add-style-hook "dvipdfmx"
      ;; 		      (lambda ()
      ;; 			(TeX-PDF-mode-on)
      ;; 			;; XeLaTeX normally don't use dvipdfmx
      ;; 			;; explicitly.
      ;; 			(unless (eq TeX-engine 'xetex)
      ;; 			  (setq TeX-PDF-from-DVI "Dvipdfmx")))
      ;; 		      :classopt)
      (insert "\\documentclass[dvipdfmx]{article}\n")
      (latex-mode)
      (TeX-update-style)
      (should TeX-PDF-mode)
      (should (equal (TeX-PDF-from-DVI) "Dvipdfmx"))
      (should (not (member "dvipdfmx" TeX-active-styles)))

      ;; dvipdfmx option should not trigger `TeX-PDF-from-DVI' for
      ;; XeLaTeX document
      (latex-mode)
      (let ((TeX-engine 'xetex))
	(TeX-update-style))
      (should TeX-PDF-mode)
      (should (not (TeX-PDF-from-DVI)))
      (should (not (member "dvipdfmx" TeX-active-styles)))

      ;; test for pdftricks option
      (erase-buffer)
      ;; This depends on the following code in latex.el:
      ;; (TeX-add-style-hook "pdftricks" #'TeX-PDF-mode-on :classopt)
      (insert "\\documentclass[pdftricks]{article}\n")
      (latex-mode)
      (TeX-update-style)
      (should TeX-PDF-mode)
      (should (not (member "pdftricks" TeX-active-styles)))

      ;; test for psfrag option
      (erase-buffer)
      ;; This depends on the following code in latex.el:
      ;; (TeX-add-style-hook "psfrag" #'TeX-PDF-mode-off :classopt)
      (insert "\\documentclass[psfrag]{article}\n")
      (latex-mode)
      (TeX-update-style)
      (should (not TeX-PDF-mode))
      (should (not (member "psfrag" TeX-active-styles))))))

;;; latex-test.el ends here
