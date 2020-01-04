;;; mu4e-overview.el --- Show overview of maildir    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: mail, tools
;; Package-Version: 20191020.842
;; Version: 0.1.0
;; Homepage: https://github.com/mkcms/mu4e-overview
;; Package-Requires: ((emacs "26"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package lets you see a hierarchy of maildirs in a separate buffer:
;;
;; mymail@gmail.com
;;  [Gmail]
;;   .Trash [0/25]
;;   .Starred [0/0]
;;   .Spam [0/3]
;;   .Sent Mail [0/4]
;;   .Important [0/21]
;;   .Drafts [0/0]
;;   .All Mail [0/56]
;;  Trash [0/0]
;;  Inbox [2/45]
;;
;; M-x `mu4e-overview' displays a buffer with a list of all maildirs.  The root
;; maildir can be set via the variable `mu4e-maildir'.
;;
;; The number of unread and total number of emails is displayed next to each
;; maildir in this format: [UNREAD-COUNT/TOTAL-COUNT].  If UNREAD-COUNT is not
;; zero, that maildir is highlighted.
;;
;; Use n and p keys to go to next/previous maildir (or alternatively,
;; tab/backtab).  Use [ and ] keys to go to previous/next *unread* maildir.
;;
;; When you click on a maildir or type RET when point is on it, mu4e headers
;; view is displayed, which shows messages only in that maildir.  Type C-u RET
;; to show only unread messages.

;;; Code:

(require 'cl-lib)
(require 'mu4e)
(require 'subr-x)

(defgroup mu4e-overview nil
  "Show overview of maildir"
  :group 'mail
  :group 'tools)

(defcustom mu4e-overview-maildir-separators '(?/)
  "List of characters used to split maildir paths into folders."
  :type '(repeat character))

(defcustom mu4e-overview-indent-width 2
  "Number of characters that subfolders are indented per level."
  :type 'integer)

(defface mu4e-overview-folder
  '((t))
  "Base face used for all folders.")

(defface mu4e-overview-group-folder
  '((t :inherit (mu4e-overview-folder shadow)))
  "Face used for folders which are not real maildirs.")

(defface mu4e-overview-unread
  '((t :inherit mu4e-overview-folder :weight bold))
  "Face used for folders which contain unread mail.")

(cl-defstruct mu4e-overview-folder
  name maildir unread-count count children)

(define-button-type 'mu4e-overview-folder
  'action #'mu4e-overview-action
  'keymap (let ((map (make-sparse-keymap)))
            (define-key map [mouse-1] #'push-button)
            map)
  'mouse-face 'highlight
  'help-echo "mouse-1, RET: show mu4e headers view for this maildir")

(defvar mu4e-overview-folders nil "List of known folders.")

(defun mu4e-overview--insert-entry (depth folder)
  "Insert an entry in current buffer for FOLDER at DEPTH.
DEPTH must be an integer that says how many spaces the line
should be indented with.  FOLDER must be an instance of
`mu4e-overview-folder'.

Children of FOLDER are also inserted recursively below the
inserted entry."
  (let ((maildir (mu4e-overview-folder-maildir folder))
        (count (mu4e-overview-folder-count folder))
        (unread-count (mu4e-overview-folder-unread-count folder))
        (face 'mu4e-overview-folder))
    (insert (make-string (* depth mu4e-overview-indent-width) ?\s)
            (mu4e-overview-folder-name folder))
    (cond ((null maildir)
           (setq face 'mu4e-overview-group-folder))
          ((or (null count) (null unread-count))
           (insert " (checking...)"))
          (t
           (insert (format " [%d/%d]" unread-count count))
           (when (cl-plusp unread-count)
             (setq face 'mu4e-overview-unread))))
    (make-text-button (point-at-bol) (point)
                      :type 'mu4e-overview-folder
                      'face face
                      'mu4e-overview-folder folder)
    (insert "\n"))
  (dolist (child (mu4e-overview-folder-children folder))
    (mu4e-overview--insert-entry (1+ depth) child)))

(defun mu4e-overview--insert-entries ()
  "Erase current buffer, then insert all folders in `mu4e-overview-folders'."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos (point)))
        (offset-from-bol (- (point) (point-at-bol))))
    (erase-buffer)
    (dolist (entry mu4e-overview-folders)
      (mu4e-overview--insert-entry 0 entry))
    (goto-char (point-min))
    (forward-line (1- line))
    (beginning-of-line)
    (forward-char (min (- (point-at-eol) (point-at-bol))
                       offset-from-bol))))

(defvar mu4e-overview--processes nil
  "List of running or stopped 'mu find' processes.")

(defun mu4e-overview--count (maildir callback)
  "Count number of total/unread messages in MAILDIR and call CALLBACK.
This function starts two mu processes which find all/unread
messages in MAILDIR, and counts the number of those messages.
When both processes are done, it calls CALLBACK with two
arguments: (UNREAD-COUNT TOTAL-COUNT).

Both started processes are added to `mu4e-overview--processes'.

If the processes take too long to finish, they're killed after
some time.

CALLBACK is called regardless if the processes finished
successfully.  If any process failed or timed out, the argument
passed to CALLBACK will be 0."
  (let (unread-count total-count)
    (dolist (unread-only '(t nil))
      (let ((count 0)
            (output "")
            (process
             (start-process-shell-command
              "mu4e-overview-maildir-counter" nil
              (format "%s find -f p '%s' %s | wc -l"
                      mu4e-mu-binary
                      (format "maildir:/%s" maildir)
                      (if unread-only " and flag:unread" "")))))

        (set-process-filter
         process
         (lambda (_proc text) (setq output (concat output text))))
        (set-process-sentinel
         process
         (lambda (proc _status)
           (unless (process-live-p proc)
             (save-match-data
               (when (string-match "^\\([0-9]+\\)\n$" output)
                 (setq count (string-to-number
                              (match-string 1 output)))))
             (if unread-only
                 (setq unread-count count)
               (setq total-count count))
             (when (and total-count unread-count)
               (funcall callback unread-count total-count)))))

        (push process mu4e-overview--processes)

        ;; Kill this process after some time.
        (run-with-timer 10.0 nil #'delete-process process)))))

(defun mu4e-overview-update ()
  "Update the list of maildirs and count the number of unread/total messages."
  (interactive)
  (mapc #'delete-process mu4e-overview--processes)
  (setq mu4e-overview--processes nil)
  (let* ((folders (mapcar
                   (lambda (dir)
                     (setq dir (substring dir 1)) ;remove prefix "/" character
                     (make-mu4e-overview-folder
                      :name dir
                      :maildir dir
                      :count nil
                      :unread-count nil
                      :children nil))
                   (mu4e-get-maildirs)))
         (separators (concat
                      (if (memq ?- mu4e-overview-maildir-separators)
                          ;; Move "-" to the front to avoid later using
                          ;; it to indicate a regexp range, like "[a-z]".
                          (cons ?- (remq ?- mu4e-overview-maildir-separators))
                        mu4e-overview-maildir-separators)))
         (n-processes-done 0)
         (pr (make-progress-reporter "Updating maildir status"
                                     0 (length folders)))
         (timer-for-refresh nil)
         (done nil)
         (buffer (current-buffer)))

    ;; Gather unread/total counts for each folder.
    (dolist (folder folders)
      (when-let ((maildir (mu4e-overview-folder-maildir folder)))
        (mu4e-overview--count
         maildir
         (lambda (unread-count total-count)
           (setf (mu4e-overview-folder-count folder) total-count)
           (setf (mu4e-overview-folder-unread-count folder) unread-count)
           (progress-reporter-update pr (cl-incf n-processes-done))
           (when (= n-processes-done (length folders))
             (progress-reporter-done pr))
           (when timer-for-refresh (cancel-timer timer-for-refresh))
           (setq timer-for-refresh
                 (run-with-idle-timer
                  0.3 nil
                  (lambda ()
                    (setq timer-for-refresh nil)
                    (with-current-buffer buffer
                      (mu4e-overview--insert-entries)))))))))

    ;; Create folder hierarchy.  `folders' is a flat list of folders.  We need
    ;; to have a list of root folders, which have children, and their children
    ;; have children, etc.

    ;; Sort the list so that the most deeply nested folders are before less
    ;; deeply nested folders.
    (setq folders (cl-sort folders #'> :key
                           (lambda (folder)
                             (cl-count-if
                              (lambda (c)
                                (memq c mu4e-overview-maildir-separators))
                              (mu4e-overview-folder-name folder)))))

    ;; Now create the hierarchy.  For each folder in `folders', find it's
    ;; parent, creating it if necessary.  Insert the folder into it's parent
    ;; `children' slot, and remove the folder from `folders' list.
    (while (not done)
      (setq done t)
      (dolist (folder folders)
        (let ((name (mu4e-overview-folder-name folder)))
          (when (string-match (format "[%s][^%s]+\\'" separators separators)
                              name)
            (setq done nil)
            (setf (mu4e-overview-folder-name folder)
                  (substring name (1+ (match-beginning 0))))
            (let* ((parent-name (substring name 0 (match-beginning 0)))
                   (parent-folder (cl-find parent-name folders
                                           :key #'mu4e-overview-folder-name
                                           :test #'string=)))
              (unless parent-folder
                (setq parent-folder
                      (make-mu4e-overview-folder
                       :name parent-name
                       :maildir nil))
                (push parent-folder folders))
              (setf (mu4e-overview-folder-children parent-folder)
                    (cl-sort
                     (cons folder
                           (mu4e-overview-folder-children parent-folder))
                     #'string< :key #'mu4e-overview-folder-name))
              (setq folders (delete folder folders)))))))

    (setq mu4e-overview-folders folders)))

(defun mu4e-overview-action (button &optional unread-only)
  "Show mu4e headers view for folder associated with BUTTON.
This function is called when user clicks on, or types RET when
point is on a folder.  If UNREAD-ONLY is non-nil, show the
headers view only for unread messages."
  (interactive)
  (let* ((folder (button-get button 'mu4e-overview-folder))
         (maildir (mu4e-overview-folder-maildir folder)))
    (mu4e-headers-search
     (format "maildir:\"/%s\"%s" maildir
             (if (or unread-only (and current-prefix-arg
                                      (eq this-command 'push-button)))
                 " and flag:unread"
               "")))))

(defun mu4e-overview-next-unread-folder (&optional n)
  "Go to next unread folder.
If N is provided, go to Nth next unread folder.
If N is negative, go to -Nth previous unread folder."
  (interactive "p")
  (or n (setq n 1))
  (let ((saved-point (point))
        (amnt (cl-signum n))
        (end (if (cl-plusp n) (point-max) (point-min))))
    (cl-loop until (or (zerop n) (= (point) end))
             do (forward-line amnt)
             (when-let*
                 ((folder
                   (get-text-property (point) 'mu4e-overview-folder))
                  (count (mu4e-overview-folder-unread-count folder)))
               (when (cl-plusp count)
                 (cl-decf n amnt))))
    (unless (zerop n)
      (goto-char saved-point)
      (error "No more unread folders"))))

(defun mu4e-overview-previous-unread-folder (&optional n)
  "Go to previous unread folder.
If argument N is provided, go to Nth previous unread folder."
  (interactive "p")
  (mu4e-overview-next-unread-folder (- (or n 1))))

(defvar mu4e-overview-mode-map
  (let ((map (copy-keymap button-buffer-map)))
    (define-key map "n" #'next-line)
    (define-key map "]" #'mu4e-overview-next-unread-folder)
    (define-key map "p" #'previous-line)
    (define-key map "[" #'mu4e-overview-previous-unread-folder)
    (define-key map "g" #'mu4e-overview-update)
    (define-key map "\C-m" #'push-button)
    map)
  "Keymap used in `mu4e-overview-mode'.")

(define-derived-mode mu4e-overview-mode special-mode "mu4e:overview"
  (setq buffer-undo-list t))

;;;###autoload
(defun mu4e-overview ()
  "Display a buffer with a list of known mail folders.
The buffer shows a hierarchy of maildirs used by `mu4e'.

The available keybindings are:
\\{mu4e-overview-mode-map}"
  (interactive)
  (with-current-buffer (get-buffer-create "*mu4e overview*")
    (mu4e-overview-mode)
    (mu4e-overview-update)
    (pop-to-buffer (current-buffer))))

(provide 'mu4e-overview)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; mu4e-overview.el ends here
