;;; error-parsing.el --- tests for error parsing

;; Copyright (C) 2017 Free Software Foundation, Inc.

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
(setq japanese-TeX-error-messages t)
(require 'tex-jp)

(ert-deftest japanese-TeX-help-message ()
  "Test the fallback behavior of `TeX-help-error'.
It should pick up error messages from the log file even if
tex-jp.el modified `TeX-error-description-list'."
  (should (let* ((dummyfile (make-temp-file "japanese-TeX-ert"))
		 (logfile (concat dummyfile ".log")))
	    (find-file logfile)
	    (insert "\
./errorsamp.tex:3: EROOR NEVER COVERED BY TeX-error-description-list.
l.3 }
     
FOO BAR
HOGE FUGA
")
	    (save-buffer 0)
	    (find-file dummyfile)
	    ;; Actually, the contents of dummyfile is irrelavent to
	    ;; this test.  It is only used to make the log file name
	    ;; which is analysed in `TeX-help-error' to be
	    ;; predictable.
	    (let ((TeX-command-buffer (current-buffer)))
	      (TeX-help-error
	       "EROOR NEVER COVERED BY TeX-error-description-list."
	       "" (current-buffer) 'error))
	    (delete-other-windows)
	    (kill-buffer (get-file-buffer logfile))
	    (delete-file logfile)
	    (kill-buffer (get-file-buffer dummyfile))
	    (delete-file dummyfile)
	    (set-buffer "*TeX Help*")
	    (goto-char (point-min))
	    (prog1
		(search-forward "From the .log file...")
	      (kill-buffer)))))

;;; error-parsing.el ends here
