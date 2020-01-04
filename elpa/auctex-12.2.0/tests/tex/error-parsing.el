;;; error-parsing.el --- tests for error parsing

;; Copyright (C) 2016 Free Software Foundation, Inc.

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

(AUCTeX-set-ert-path
 'TeX-test-compilation-log
 "compilation-log.txt")

(ert-deftest TeX-error-parsing ()
  "Test error parsing functions."
  (should (equal
	   (with-temp-buffer
	     (setq TeX-debug-warnings t
		   TeX-debug-bad-boxes t)
             (insert-file-contents TeX-test-compilation-log)
             (TeX-parse-all-errors)
	     TeX-error-list)
	   '((warning
	      "./nice-class.cls" 32
	      "Package nice-class Warning: ******************************************"
	      0
	      "Package nice-class Warning: ******************************************
(nice-class)                * THIS IS JUST A WARNING WITH A PESKY
(nice-class)                * UNMATCHED CLOSED PARENTHESIS :-)
(nice-class)                ****************************************** on input line 32.\n"
	      nil 32 nil 376 nil)
	     (error
              "./test.tex" 2
              "Class nice-class Error: ***********************************" 0
              "\n(nice-class)              * This is a very bad error!
(nice-class)              ************************************.

See the suftesi class documentation for explanation.
Type  H <return>  for immediate help.
 ...                                              
                                                  
l.2 \\begin{document}

(/other/packages/loaded.sty)
ABD: EveryShipout initializing macros"
              "\\begin{document}\n\n(/other/packages/loaded.sty)" nil nil 971 nil)
	     (warning "./test.tex" 3
              "Package foo Warning: This is a warning! on input line 3." 0
	      "Package foo Warning: This is a warning! on input line 3.\n"
	      nil 3 nil 1030 nil)
	     (bad-box
	      "./secondary-file.tex" 131
	      "Underfull \\hbox (badness 6608) in paragraph at lines 131--132"
	      0 "\n[]|\\T1/jkpl/m/n/10.95 (+20) Something bla" "bla"
	      132 t 1268 nil)
             (warning "./test.tex" 4
              "LaTeX Warning: Reference `wrong' on page 1 undefined on input line 4."
              0
              "LaTeX Warning: Reference `wrong' on page 1 undefined on input line 4.\n"
              "wrong" 4 nil 1351 nil)
             (warning "./test.tex" 70 "LaTeX Font Warning: Font shape `OML/cmm/b/it' in size <5.5> not available"
              0 "LaTeX Font Warning: Font shape `OML/cmm/b/it' in size <5.5> not available
(Font)              size <5> substituted on input line 70.\n" nil 70 nil 1485 nil)
             (bad-box "./test.lof" nil "Underfull \\vbox (badness 1048) has occurred while \\output is active [7]"
              0 "\nUnderfull \\vbox (badness 1048) has occurred while \\output is active [7]"
              nil nil t 1651 nil)
             ;; It is possible there are two different bad box warnings in two
             ;; consecutive lines (for example it happens if the first one is a
             ;; vertical bad box which doesn't provide additional information),
             ;; this test makes sure the second warning is not mistaken as
             ;; context of the first one.
             (bad-box "./test.lof" 31 "Overfull \\hbox (0.93071pt too wide) detected at line 31"
              0 "\n []\\T1/jkpl/m/n/10.95 144" "144" 31 t 1733 nil)
             ;; The line of this warning ends with a new file opened.  This test
             ;; makes sure point stays exactly at the end of the warning so that
             ;; the next the next warning in the list has the right file
             ;; (otherwise it would be nil).
             (bad-box "./file  name/with spaces.tex" nil "Underfull \\vbox (badness 3942) has occurred while \\output is active [87 <./image/another.pdf> <./image/another2.pdf>]"
              0 "\nUnderfull \\vbox (badness 3942) has occurred while \\output is active [87 <./image/another.pdf> <./image/another2.pdf>]"
              nil nil t 2142 nil)
             (bad-box "./file  name/with spaces.tex" 367 "Overfull \\hbox (13.59138pt too wide) in paragraph at lines 367--368"
              0 "\n[]\\T1/pplj/m/n/10.95 Un qua-dri-vet-to-re co-va-rian-te $\\OMS/zplm/m/n/10.95 f\\OML/zplm/m/it/10.95 A[]\\OMS/zplm/m/n/10.95 g$ \\T1/pplj/m/n/10.95 e un in-sie-me di quat-tro quan-ti-ta $\\OT1/zplm/m/n/10.95 (\\OML/zplm/m/it/10.95 A[]; A[]; A[]; A[]\\OT1/zplm/m/n/10.95 )$" "$"
              368 t 2600 nil)
             (error "./test.tex" 11 "Undefined control sequence." 0
		    "\nl.11 \\doesnotexist\n\n(./foo.aux) )"
		    "\\doesnotexist\n" nil nil 2682 nil)
             (warning "./test.tex" 48
              "LaTeX Warning: Citation 'Knuth:TeXbook-1984' undefined on input line 48." 0
              "LaTeX Warning: Citation 'Knuth:TeXbook-1984' undefined on input line 48.\n"
              "Knuth:TeXbook-1984" 48 nil 2756 nil)
             (warning "./test.tex" nil "LaTeX Warning: There were undefined references."
              0 "LaTeX Warning: There were undefined references.\n" nil nil nil 2805 nil)
))))

(ert-deftest TeX-error-parsing-expl3-warnings ()
  "Test parsing of expl3 warnings."
  (should (equal
	   (with-temp-buffer
	     (setq TeX-debug-warnings t
		   TeX-debug-bad-boxes t)
             (insert
	      "*************************************************
* xsim warning: \"rerun\"
* 
* Exercise properties may have changed. Rerun to get them synchronized.
*************************************************
")
             (TeX-parse-all-errors)
	     TeX-error-list)
	   '((warning nil nil "* xsim warning: \"rerun\"" 0
		      "* xsim warning: \"rerun\"\n* \n* Exercise properties may have changed. Rerun to get them synchronized.\n*************************************************\n"
		      nil nil nil 200 nil)))))

;; See https://lists.gnu.org/archive/html/auctex/2017-04/msg00007.html.
(ert-deftest TeX-LaTeX2e-date ()
  "Test parsing of different LaTeX2e date formats."
  (should
   (equal
    (with-temp-buffer
      (insert "LaTeX2e <2017-04-15>")
      (goto-char (point-min))
      (let ((TeX-command-buffer (buffer-name)))
	(TeX-LaTeX-sentinel nil "LaTeX")
	TeX-command-next))
    "View"))
  (should
   (equal
    (with-temp-buffer
      (insert "LaTeX2e <2017/01/01> patch level 3")
      (goto-char (point-min))
      (let ((TeX-command-buffer (buffer-name)))
	(TeX-LaTeX-sentinel nil "LaTeX")
	TeX-command-next))
    "View")))

;;; error-parsing.el ends here
