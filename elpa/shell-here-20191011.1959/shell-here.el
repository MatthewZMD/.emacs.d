;;; shell-here.el --- Open a shell relative to the working directory

;; Copyright (C) 2009-2012, 2015  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Version: 1.3
;; Package-Version: 20191011.1959
;; Keywords: unix, tools, processes

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

;;; Commentary:

;; Open a shell buffer in (or relative to) default-directory,
;; e.g. whatever directory the current buffer is in. If you have
;; find-file-in-project installed, you can also move around relative
;; to the root of the current project.

;; I use Emacs shell buffers for everything, and shell-here is great
;; for getting where you need to quickly. The =find-file-in-project=
;; integration makes it very easy to manage multiple shells and
;; maintain your path / history / scrollback when switching between
;; projects.
;;
;; Recommended binding: =C-c !=
;;
;;   (require 'shell-here)
;;   (define-key (current-global-map) "\C-c!" 'shell-here)
;;
;; Usage:
;;
;; | =C-c !=         | Open a shell in the current directory         |
;; | =C-1 C-c !=     | Open a shell one level up from current        |
;; | =C-2 C-c !=     | Open a shell two levels up from current (etc) |
;; | =C-u C-c !=     | Open a new shell in the current directory     |
;; | =C-- C-c !=     | Open a shell in the current project root      |
;; | =C-- C-1 C-c != | Open a shell one level up from root           |
;; | =C-- C-2 C-c != | Open a shell two levels up from root (etc)    |
;; | =C-- C-u C-c != | Open a new shell in the project root          |

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defvar shell-here-project-root-functions
  '(projectile-project-root ffip-project-root)
  "Functions to attempt evaluating to determine the project root.")

(defvar shell-here-project-root-files
  '(".git")
  "Dominating files to look for to determine the project root.")

(defun shell-here-walk-up (base steps)
  "Return the location STEPS levels up from directory BASE"
  (if (= steps 0) base
    (shell-here-walk-up (shell-here-stripslash
                         (file-name-directory base)) (- steps 1))))

(defun shell-here-stripslash (path)
  "Return PATH with the trailing slash, if any, removed."
  (if (and (> (length path) 1) (string= (substring path -1) "/"))
      (substring path 0 -1)
    path))

(defun shell-here-normalize (path)
  "Return a canonicalized PATH, with trailing slash, if any, removed."
  (when path (shell-here-stripslash (expand-file-name path))))

(defun shell-here-project-root ()
  "Return the project root.

   Tries calling `shell-here-project-root-functions', returning the
   result of evaluating the first one which is bound. If none are
   bound, looks for a parent directory containing a file in
   `shell-here-project-root-files'."
  (or (cl-loop with found = nil
               for func in shell-here-project-root-functions
               if (setq found (and (fboundp func) (apply func nil)))
               return found)
      (cl-loop with found = nil
               for file in shell-here-project-root-files
               if (setq found (locate-dominating-file default-directory file))
               return found)
      default-directory))

;;;###autoload
(defun shell-here (&optional arg)
  "Open a shell relative to `default-directory'.

With no argument, open a shell in `default-directory'.
With a positive numeric argument, open a shell ARG levels up from
`default-directory'.
With a plain negative argument, open a shell in the project root.
With a negative numeric argument, open a shell ARG levels up from the
project root.

Shell buffer names include the name of the current project's
directory, if available; otherwise *shell*. If a shell buffer already
exists, it will be reused.

With the universal argument, open a new shell in `default-directory'.
With a negative universal argument, open a new shell in the project
root."
  (interactive "P")
  (let* ((root-relative (< (prefix-numeric-value arg) 0))
         (new (consp arg))
         (levels (if (and arg (not (eq arg '-)) (not new))
                    (abs (prefix-numeric-value arg)) 0)))

    (let* ((root (shell-here-normalize (shell-here-project-root)))
           (start (or (and root-relative root)
                      (shell-here-normalize default-directory)))
           (target (shell-here-walk-up start levels))

         (base-name
          (format "*shell%s*"
                  (if root (format " %s" (file-name-nondirectory root)) "")))
         (buf (pop-to-buffer
               (let ((default-directory (format "%s/" target)))
                 (cond
                  (new (generate-new-buffer base-name))
                  ((eq major-mode 'shell-mode) (current-buffer))
                  (t (get-buffer-create base-name)))))))

      (unless (let ((proc (get-buffer-process buf)))
                (and proc (process-live-p proc)))
        (shell buf))
      (goto-char (point-max))

      ;; We need to `cd'
      (when (not (string= (shell-here-stripslash
                           (expand-file-name default-directory)) target))
        (when (tramp-tramp-file-p target)
          (with-parsed-tramp-file-name target path
            (setq target path-localname)))

        ;; Save any input on the command line; `comint-kill-input'
        ;; calls `kill-region', which we have flet with a function
        ;; that returns the region before deleting it.
        ;; The (insert (prog1 …)) inserts it (or an empty string, if
        ;; we know we have a new buffer) back into the shell buffer
        ;; after having changed directories.
        (cl-flet ((kill-region (start end)
                 (prog1
                     (buffer-substring start end) (delete-region start end))))
          (insert (prog1 (or (when (not new) (comint-kill-input)) "")
                    (insert (concat "cd " (shell-quote-argument target)))
                    (comint-send-input))))))))

(provide 'shell-here)
;;; shell-here.el ends here
