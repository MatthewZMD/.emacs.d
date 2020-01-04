;;; sudo-edit.el --- Open files as another user       -*- lexical-binding: t -*-

;; Copyright (C) 2014 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; URL: https://github.com/nflath/sudo-edit
;; Package-Version: 20180731.1908
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package allows to open files as another user, by default "root":
;;
;;     `sudo-edit'
;;
;; Suggested keybinding:
;;
;;     (global-set-key (kbd "C-c C-r") 'sudo-edit)
;;
;; Installation:
;;
;; To use this mode, put the following in your init.el:
;;
;;     (require 'sudo-edit)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x nil 'no-error))

(require 'tramp)

;; Compatibility for Emacs 24.3 and earlier
(eval-and-compile
  (unless (fboundp 'string-prefix-p)
    (defun string-prefix-p (prefix string &optional ignore-case)
      "Return non-nil if PREFIX is a prefix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
      (let ((prefix-length (length prefix)))
        (if (> prefix-length (length string)) nil
          (eq t (compare-strings prefix 0 prefix-length string
                                 0 prefix-length ignore-case))))))

  (unless (fboundp 'string-suffix-p)
    (defun string-suffix-p (suffix string  &optional ignore-case)
      "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
      (let ((start-pos (- (length string) (length suffix))))
        (and (>= start-pos 0)
             (eq t (compare-strings suffix nil nil
                                    string start-pos nil ignore-case))))))

  (unless (featurep 'subr-x)
    (defsubst string-remove-prefix (prefix string)
      "Remove PREFIX from STRING if present."
      (if (string-prefix-p prefix string)
          (substring string (length prefix))
        string))

    (defsubst string-remove-suffix (suffix string)
      "Remove SUFFIX from STRING if present."
      (if (string-suffix-p suffix string)
          (substring string 0 (- (length string) (length suffix)))
        string))

    (defsubst string-blank-p (string)
      "Check whether STRING is either empty or only whitespace."
      (string-match-p "\\`[ \t\n\r]*\\'" string))))

(defgroup sudo-edit nil
  "Edit files as another user."
  :prefix "sudo-edit-"
  :group 'convenience)

(defcustom sudo-edit-user "root"
  "Default user to edit a file with `sudo-edit'."
  :type 'string
  :group 'sudo-edit)

(defface sudo-edit-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root."
  :group 'sudo-edit)

;;;###autoload
(defun sudo-edit-set-header ()
  "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook' and `dired-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (setq header-line-format
          (propertize "--- WARNING: EDITING FILE AS ROOT! %-"
                      'face 'sudo-edit-header-face))))

;;;###autoload
(define-minor-mode sudo-edit-indicator-mode
  "Indicates editing as root by displaying a message in the header line."
  :global t
  :lighter nil
  :group 'sudo-edit

  (if sudo-edit-indicator-mode
      (progn
        (add-hook 'find-file-hook #'sudo-edit-set-header)
        (add-hook 'dired-mode-hook #'sudo-edit-set-header))
    (remove-hook 'find-file-hook #'sudo-edit-set-header)
    (remove-hook 'dired-mode-hook #'sudo-edit-set-header)))

(defvar sudo-edit-user-history nil)

;; NB: TRAMP 2.3.2 introduced `tramp-file-name' struct which offers these
;;     functions to access the slots.
(or (fboundp 'tramp-file-name-domain) (defalias 'tramp-file-name-domain #'ignore))
(or (fboundp 'tramp-file-name-port) (defalias 'tramp-file-name-port #'ignore))

(defalias 'sudo-edit-make-tramp-file-name
  (if (version< tramp-version "2.3.2")
      (with-no-warnings
        (lambda (method user _domain host _port localname &optional hop)
          (tramp-make-tramp-file-name method user host localname hop)))
    #'tramp-make-tramp-file-name))

(defun sudo-edit-tramp-get-parameter (vec param)
  "Return from tramp VEC a parameter PARAM."
  (or (tramp-get-method-parameter vec param)
      ;; NB: Compatibility old versions of TRAMP 2.2.x shipped with Emacs 24.3
      ;;     and earlier.  Ignore errors when the method doesn't have the
      ;;     parameter as static value defined in `tramp-methods'.
      (ignore-errors (tramp-get-method-parameter (tramp-file-name-method vec) param))))

(defun sudo-edit-out-of-band-ssh-p (filename)
  "Check if tramp FILENAME is a out-of-band method and use ssh."
  (or (let ((vec (tramp-dissect-file-name filename)))
        (and (sudo-edit-tramp-get-parameter vec 'tramp-copy-program)
             (string-equal (sudo-edit-tramp-get-parameter vec 'tramp-login-program) "ssh")))
      (tramp-gvfs-file-name-p filename)))

(defun sudo-edit-filename (filename user)
  "Return a tramp edit name for a FILENAME as USER."
  (if (file-remote-p filename)
      (let* ((vec (tramp-dissect-file-name filename))
             ;; XXX: Change to ssh method on out-of-band ssh based methods
             (method (if (sudo-edit-out-of-band-ssh-p filename)
                         "ssh"
                       (tramp-file-name-method vec)))
             (hop (sudo-edit-make-tramp-file-name
                   method
                   (tramp-file-name-user vec)
                   (tramp-file-name-domain vec)
                   (tramp-file-name-host vec)
                   (tramp-file-name-port vec)
                   ""
                   (tramp-file-name-hop vec))))
        (setq hop (string-remove-prefix (if (fboundp 'tramp-prefix-format) (tramp-prefix-format) (bound-and-true-p tramp-prefix-format)) hop))
        (setq hop (string-remove-suffix (if (fboundp 'tramp-postfix-host-format) (tramp-postfix-host-format) (bound-and-true-p tramp-postfix-host-format)) hop))
        (setq hop (concat hop tramp-postfix-hop-format))
        (if (and (string= user (tramp-file-name-user vec))
                 (string-match tramp-local-host-regexp (tramp-file-name-host vec)))
            (tramp-file-name-localname vec)
          (sudo-edit-make-tramp-file-name "sudo" user (tramp-file-name-domain vec) (tramp-file-name-host vec) (tramp-file-name-port vec) (tramp-file-name-localname vec) hop)))
    (sudo-edit-make-tramp-file-name "sudo" user nil "localhost" nil (expand-file-name filename))))

;;;###autoload
(defun sudo-edit (&optional arg)
  "Edit currently visited file as another user, by default `sudo-edit-user'.

With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (let ((user (if arg
                  (completing-read "User: " (and (fboundp 'system-users) (system-users)) nil nil nil 'sudo-edit-user-history sudo-edit-user)
                sudo-edit-user))
        (filename (or buffer-file-name
                      (and (derived-mode-p 'dired-mode) default-directory))))
    (cl-assert (not (string-blank-p user)) nil "User must not be a empty string")
    (if (or arg (not filename))
        (find-file (sudo-edit-filename (read-file-name (format "Find file (as \"%s\"): " user)) user))
      (let ((position (point)))
        (find-alternate-file (sudo-edit-filename filename user))
        (goto-char position)))))

(define-obsolete-function-alias 'sudo-edit-current-file 'sudo-edit)

(provide 'sudo-edit)
;;; sudo-edit.el ends here
