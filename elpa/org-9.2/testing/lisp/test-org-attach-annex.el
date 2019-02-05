;;; test-org-annex-attach.el --- Tests for Org Attach with git-annex
;;
;; Copyright (c) 2016 Erik Hetzner
;; Authors: Erik Hetzner

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(org-test-for-executable "git-annex")
(require 'org-attach)
(require 'cl-lib)

(defmacro test-org-attach-annex/with-annex (&rest body)
  `(let ((tmpdir (make-temp-file "org-annex-test" t "/")))
     (unwind-protect
	 (let ((default-directory tmpdir)
	       (org-attach-directory tmpdir))
	   (shell-command "git init")
	   (shell-command "git annex init")
	   ,@body))))

(ert-deftest test-org-attach/use-annex ()
  (test-org-attach-annex/with-annex
   (let ((org-attach-git-annex-cutoff 1))
     (should (org-attach-use-annex)))

   (let ((org-attach-git-annex-cutoff nil))
     (should-not (org-attach-use-annex))))

  ;; test with non annex directory
  (let ((tmpdir (make-temp-file "org-annex-test" t "/")))
     (unwind-protect
	 (let ((default-directory tmpdir)
	       (org-attach-directory tmpdir))
	   (shell-command "git init")
	   (should-not (org-attach-use-annex)))
       (delete-directory tmpdir 'recursive))))

(ert-deftest test-org-attach/get-maybe ()
  (test-org-attach-annex/with-annex
   (let ((path (expand-file-name "test-file"))
	 (annex-dup (make-temp-file "org-annex-test" t "/")))
     (with-temp-buffer
       (insert "hello world\n")
       (write-file path))
     (shell-command "git annex add test-file")
     (shell-command "git annex sync")
     ;; Set up remote & copy files there
     (let ((annex-original default-directory)
	   (default-directory annex-dup))
       (shell-command (format "git clone %s ." (shell-quote-argument annex-original)))
       (shell-command "git annex init dup")
       (shell-command (format "git remote add original %s" (shell-quote-argument annex-original)))
       (shell-command "git annex get test-file")
       (shell-command "git annex sync"))
     (shell-command (format "git remote add dup %s" (shell-quote-argument annex-dup)))
     (shell-command "git annex sync")
     (shell-command "git annex drop --force test-file")
     ;; test getting the file from the dup when we should ALWAYS get
     (should (not (file-exists-p (file-symlink-p (expand-file-name "test-file")))))
     (let ((org-attach-annex-auto-get t))
       (org-attach-annex-get-maybe (expand-file-name "test-file"))
       ;; check that the file has the right contents
       (with-temp-buffer
	 (insert-file-contents path)
	 (should (string-equal "hello world\n" (buffer-string)))))
     ;; test getting the file from the dup when we should NEVER get
     (shell-command "git annex drop --force test-file")
     (let ((org-attach-annex-auto-get nil))
       (should-error (org-attach-annex-get-maybe (expand-file-name "test-file"))))
     (let ((org-attach-annex-auto-get 'ask)
	   (called nil))
       (cl-letf (((symbol-function 'y-or-n-p)
		  (lambda (_) (setq called 'was-called) t)))
	 (org-attach-annex-get-maybe (expand-file-name "test-file"))
	 ;; check that the file has the right contents
	 (with-temp-buffer
	   (insert-file-contents path)
	   (should (string-equal "hello world\n" (buffer-string))))
	 (should (eq called 'was-called)))))))

;;; test-org-attach-annex.el ends here
