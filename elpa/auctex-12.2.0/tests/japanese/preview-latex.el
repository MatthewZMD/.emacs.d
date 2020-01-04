;;; preview-latex.el --- tests for preview-latex compatibility

;; Copyright (C) 2017, 2018 Free Software Foundation, Inc.

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
(let ((japanese-TeX-error-messages nil))
  (require 'tex-jp))
(require 'preview)

(AUCTeX-set-ert-path
 'platex-shift-jis
 "preview-error-test.tex"
 'preserve-kanji-option
 "preview-error-test2.tex"
 'different-coding-system
 "prv-dif-code.tex"
)

;; Make sure coding system output from tex process to be expected
;; value.
(setq japanese-TeX-use-kanji-opt-flag t)

(setq TeX-process-asynchronous t)
(setq TeX-after-start-process-function #'TeX-adjust-process-coding-system)

(ert-deftest japanese-preview-shift-jis ()
  "Coding system `shift_jis' is harmless to preview-latex or not.
The second byte in `shift_jis' encoding which coincides with a regexp meta
character used to cause trouble.  Such patterns are tested."
  ;; The test is meaningful only in interactive session.  Skip in
  ;; batch mode.
  (skip-unless (not noninteractive))
  (let ((TeX-clean-confirm nil)
	(preview-auto-cache-preamble nil)
	(process-environment (copy-sequence process-environment))
	(locale-coding-system 'shift_jis)
	(TeX-japanese-process-output-coding-system nil)
	(TeX-japanese-process-input-coding-system nil))
    ;; Make platex binary to output in `shift_jis' encoding.
    (setenv "LC_ALL" "ja_JP.SJIS")
    ;; If your startup script for `TeX-shell' (normally "/bin/sh")
    ;; overwrites LC_ALL, you cannot trust the result of this test.
    ;; I.e., the positive result can be reported as negative, and the
    ;; negative can be as positive.
    (unwind-protect
	(save-window-excursion
	  (find-file platex-shift-jis)
	  (delete-other-windows)
	  (preview-document)
	  (message "Please wait for asynchronous process to finish...")
	  (sleep-for 5)
	  ;; Actually, this type of trouble seems to be captured early by
	  ;; ert mechanism as error and not to reach here.
	  (should-not (string-match "error in process sentinel:"
				    (current-message)))
	  (message "Please wait for asynchronous process to finish...done")
	  (message "Type %s when checking is done."
		   (substitute-command-keys "\\[exit-recursive-edit]"))
	  (recursive-edit)
	  (should (yes-or-no-p "\
Did all images come out at the correct position? ")))
      ;; Cleanup.
      (set-buffer (get-file-buffer platex-shift-jis))
      (let* ((buffer (TeX-process-buffer-name (TeX-master-file nil t)))
	     (process (get-buffer-process buffer)))
	(if process (delete-process process))
	(kill-buffer buffer))
      (preview-clearout-document)
      (TeX-clean t)
      (dolist (dir preview-temp-dirs)
	(if (file-exists-p (directory-file-name dir))
	    (delete-directory dir t)))
      (kill-buffer))))

(ert-deftest japanese-preview-different-coding-system ()
  "Different coding systems between file and process are OK or not.
Japanese TeX by itself converts encoding of Japanese text, so sometimes
`buffer-file-coding-system' and the coding system of the output from
the process differ."
  ;; The test is meaningful only in interactive session.  Skip in
  ;; batch mode.
  (skip-unless (not noninteractive))
  (let ((TeX-clean-confirm nil)
	(preview-auto-cache-preamble nil)
	(process-environment (copy-sequence process-environment))
	(locale-coding-system 'shift_jis)
	(TeX-japanese-process-output-coding-system nil)
	(TeX-japanese-process-input-coding-system nil))
    ;; Make platex binary to output in `shift_jis' encoding.
    (setenv "LC_ALL" "ja_JP.SJIS")
    ;; If your startup script for `TeX-shell' (normally "/bin/sh")
    ;; overwrites LC_ALL, you cannot trust the result of this test.
    ;; I.e., the positive result can be reported as negative, and the
    ;; negative can be as positive.
    (unwind-protect
	(save-window-excursion
	  (find-file different-coding-system)
	  (delete-other-windows)
	  (preview-document)
	  (message "Please wait for asynchronous process to finish...")
	  (sleep-for 5)
	  ;; Actually, this type of trouble seems to be captured early by
	  ;; ert mechanism as error and not to reach here.
	  (should-not (string-match "error in process sentinel:"
				    (current-message)))
	  (message "Please wait for asynchronous process to finish...done")
	  (message "Type %s when checking is done."
		   (substitute-command-keys "\\[exit-recursive-edit]"))
	  (recursive-edit)
	  (should (yes-or-no-p "\
Did all images come out at the correct position? ")))
      ;; Cleanup.
      (set-buffer (get-file-buffer different-coding-system))
      (let* ((buffer (TeX-process-buffer-name (TeX-master-file nil t)))
	     (process (get-buffer-process buffer)))
	(if process (delete-process process))
	(kill-buffer buffer))
      (preview-clearout-document)
      (TeX-clean t)
      (dolist (dir preview-temp-dirs)
	(if (file-exists-p (directory-file-name dir))
	    (delete-directory dir t)))
      (kill-buffer))))

(ert-deftest japanese-preview-preserve-kanji-option ()
  "`TeX-inline-preview-internal' preserves kanji option or not.
Internal Japanese encoding of `platex' is utf-8 by default in TeXLive of
unix flavors.  So the document encoded in `euc-jp' is not processed
correctly without kanji option, which used to be dropped during the
command substitutions performed within preview-latex when preamble cache
is enabled."
  ;; The test is meaningful only in interactive session.  Skip in
  ;; batch mode.
  (skip-unless (not noninteractive))
  (let ((TeX-clean-confirm nil)
	(preview-auto-cache-preamble t)
	(TeX-japanese-process-output-coding-system nil)
	(TeX-japanese-process-input-coding-system nil))
    (unwind-protect
	(save-window-excursion
	  (find-file preserve-kanji-option)
	  (delete-other-windows)
	  (preview-document)
	  (message "Please wait for asynchronous process to finish...")
	  (sleep-for 3)
	  (message "Please wait for asynchronous process to finish...done")
	  (message "Type %s when checking is done."
		   (substitute-command-keys "\\[exit-recursive-edit]"))
	  (recursive-edit)
	  (should (yes-or-no-p "\
Did the image come out at the correct position? ")))
      ;; Cleanup.
      (set-buffer (get-file-buffer preserve-kanji-option))
      (let* ((buffer (TeX-process-buffer-name (TeX-master-file nil t)))
	     (process (get-buffer-process buffer)))
	(if process (delete-process process))
	(kill-buffer buffer))
      (preview-clearout-document)
      (TeX-clean t)
      (dolist (dir preview-temp-dirs)
	(if (file-exists-p (directory-file-name dir))
	    (delete-directory dir t)))
      (kill-buffer))))

;; The following tests the individual parts fixed in May 2017 and can be
;; automated with batch mode.  Note that these tests just check specific
;; parts of preview-latex and do not gurarantee that final outcome of
;; the preview images are fine in total even if all these tests pass.

(ert-deftest japanese-preview-error-quote-shift-jis ()
  "`preview-error-quote' is robust against `shift_jis' or not.
String encoded in `shift_jis' can have regexp meta characters in it."
  (let (case-fold-search
	(buffer-file-coding-system 'shift_jis))
    (dolist (str '("表(1)" "予{a}" "能|" "{あ} %能" "アース" "型"))
      (should (string-match (preview-error-quote str) str)))))

(ert-deftest japanese-preview-decode-^^ab ()
  "`preview--decode-^^ab' doesn't leave regexp meta characters in results."
  (let (case-fold-search)
    ;; "あ" is encoded as \x82 \xa0 in SJIS.
    (should (string= (preview--decode-^^ab "^^82^^a0" 'shift_jis) "あ"))
    ;; "表" is encoded as \x95 '\' in SJIS.
    (should (string= (preview--decode-^^ab "^^95\\" 'shift_jis) "表"))
    ;; "ー" is encoded as \x81 '[' in SJIS.
    (should (string= (preview--decode-^^ab "^^81[^^Ab" 'shift_jis) "ー^^Ab"))
    ;; "型" is encoded as \x8c '^' in SJIS.
    (should (string= (preview--decode-^^ab "型^ab" 'shift_jis) "型^ab"))))

(ert-deftest japanese-preview-convert-^^ab ()
  "`preview--convert-^^ab' converts ^^ab to raw 8bits and leaves ^^Ab."
  (let (case-fold-search)
    (should (string= (preview--convert-^^ab "^^80") "\x80"))
    (should (string= (preview--convert-^^ab "^^80^^f0") "\x80\xf0"))
    (should (string= (preview--convert-^^ab "^^^a0") "^\xa0"))
    (should (string= (preview--convert-^^ab "^^c0^^Ab") "\xc0^^Ab"))))

(ert-deftest japanese-preview-preserve-kanji-option2 ()
  "Test command to use dumped format preserves kanji option or not."
  (let ((TeX-clean-confirm nil)
	;; Make `preview-call-hook' inactive.
	(preview-image-creators nil)
	dummyfile process)
    (unwind-protect
	(save-window-excursion
	  (find-file preserve-kanji-option)
	  (setq dummyfile (TeX-master-file))
	  (delete-other-windows)
	  (setq process (TeX-inline-preview-internal
			 (preview-do-replacements
			  (TeX-command-expand
			   (preview-string-expand preview-LaTeX-command)
			   'TeX-master-file)
			  preview-LaTeX-command-replacements)
			 dummyfile '(nil . nil) (current-buffer)
			 '(nil . (t . t)) dummyfile '(nil nil nil)))
	  (let ((cmd (process-command process)))
	    (should (string-match "-kanji" (nth (1- (length cmd)) cmd)))))
      ;; Cleanup.
      (accept-process-output process)
      (set-buffer (get-file-buffer preserve-kanji-option))
      (let* ((buffer (TeX-process-buffer-name (TeX-master-file nil t)))
	     (process (get-buffer-process buffer)))
	(if process (delete-process process))
	(kill-buffer buffer))
      (TeX-clean t)
      (dolist (dir preview-temp-dirs)
	(if (file-exists-p (directory-file-name dir))
	    (delete-directory dir t)))
      (kill-buffer))))

(ert-deftest japanese-preview-preserve-kanji-option3 ()
  "Test command to dump format file preserves kanji option or not."
  (let ((TeX-clean-confirm nil)
	;; Make `preview-call-hook' inactive.
	(preview-image-creators nil)
	(preview-format-name "dummy")
	dummyfile process)
    (unwind-protect
	(save-window-excursion
	  (find-file preserve-kanji-option)
	  (setq dummyfile (TeX-master-file))
	  (delete-other-windows)
	  (setq process (TeX-inline-preview-internal
			 (preview-do-replacements
			  (TeX-command-expand
			   (preview-string-expand preview-LaTeX-command)
			   'TeX-master-file)
			  preview-dump-replacements)
			 dummyfile '(nil . nil) (current-buffer)
			 nil dummyfile '(nil nil nil)))
	  (let ((cmd (process-command process)))
	    (should (string-match "-kanji" (nth (1- (length cmd)) cmd)))))
      ;; Cleanup.
      (accept-process-output process)
      (set-buffer (get-file-buffer preserve-kanji-option))
      (let* ((buffer (TeX-process-buffer-name (TeX-master-file nil t)))
	     (process (get-buffer-process buffer)))
	(if process (delete-process process))
	(kill-buffer buffer))
      (TeX-clean t)
      (dolist (dir preview-temp-dirs)
	(if (file-exists-p (directory-file-name dir))
	    (delete-directory dir t)))
      (kill-buffer))))

;;; preview-latex.el ends here

;; Local Variables:
;; coding: iso-2022-jp
;; End:
