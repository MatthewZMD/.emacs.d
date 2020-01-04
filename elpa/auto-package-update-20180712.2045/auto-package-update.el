;;; auto-package-update.el --- Automatically update Emacs packages.

;; Copyright (C) 2014 Renan Ranelli <renanranelli at google mail>

;; Author: Renan Ranelli
;; URL: http://github.com/rranelli/auto-package-update.el
;; Package-Version: 20180712.2045
;; Version: 1.7
;; Keywords: package, update
;; Package-Requires: ((emacs "24.4") (dash "2.1.0"))

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Commentary:
;;
;; This package provides functionality for automatically updating your Emacs
;; packages periodically. It is specially useful for people that work in
;; multiple machines and tend to forget to manually update packages from time to
;; time.

;; The main idea is that you set a desired periodicity for the updates, and when
;; you start Emacs, the packages will be automatically updated if enough days
;; have passed since the last update.

;;; Requirements:
;;
;; This package was tested for GNU Emacs 24.4 and above. Older Emacsen are not
;; supported yet.

;;; Installation:
;;
;; You can install via `MELPA`, or manually by downloading `auto-package-update.el` and
;; adding the following to your init file:
;;
;; ```elisp
;; (add-to-list 'load-path "/path/to/auto-package-update")
;; (require 'auto-package-update)
;; ```

;;; Usage:
;;
;; If `auto-package-update.el` is installed properly, you can add the following
;; line to your `.emacs`.
;;
;; ```elisp
;; (auto-package-update-maybe)
;; ```
;;
;; This will update your installed packages at startup if there is an update
;; pending.
;;
;; You can register a check every day at a given time using `auto-package-update-at-time':
;;
;; ```elisp
;; (auto-package-update-at-time "03:00")
;; ```
;;
;; will check for pending updates every three o'clock a.m..
;;
;; You can also use the function `auto-package-update-now' to update your
;; packages immediatelly at any given time.

;;; Customization:
;;
;; The periodicity (in days) of the update is given by the custom
;; variable `auto-package-update-interval`. The default interval is 7
;; days but if you want to change it, all you need is:
;;
;; ```elisp
;; (setq auto-package-update-interval 14)
;; ```
;;
;; Sometimes it is useful to skip an automatic update, e.g. when you're in a hurry
;; or don't have a working internet connection.
;; Use this setting to show a manual prompt before automatic updates:
;;
;; ```elisp
;; (setq auto-package-update-prompt-before-update t)
;; ```
;;
;; To delete residual old version directory when updating, set to
;; true variable `auto-package-update-delete-old-versions`. The
;; default value is `nil`. If you want to enable deleting:
;;
;; ```elisp
;; (setq auto-package-update-delete-old-versions t)
;; ```
;;
;;; Hooks
;;
;; If you want to add functions to run *before* and *after* the package update, you can
;; use the `auto-package-update-before-hook' and `auto-package-update-after-hook' hooks.
;; For example:
;;
;; ```elisp
;; (add-hook 'auto-package-update-before-hook
;;           (lambda () (message "I will update packages now")))
;; ```

;;; Changelog:
;; 1.7 - Add option to prompt user before running auto-package-update-maybe <br/>
;; 1.6.1 - Replace deprecated `toggle-read-only' with `read-only-mode' to remove byte compile warnings. Thanx to @syohex. <br/>
;; 1.6 - Add option to remove old packages from `.emacs.d/elpa' when updating. Thanks to @JesusMtnez. <br/>
;; 1.5 - Allow user to check for updates every day at specified time. <br/>
;; 1.4 - Add before and after update hooks. <br/>
;; 1.3 - Do not break if a package is not available in the repositories.
;;       Show update results in a temporary buffer instead of the echo area<br/>
;; 1.2 - Refactor for independence on package-menu functions. <br/>
;; 1.1 - Support GNU Emacs 24.3. <br/>
;; 1.0 - First release. <br/>

;;; Code:
(require 'dash)

(require 'package)
(unless package--initialized
  (package-initialize))

;;
;;; Customization
;;

(defgroup auto-package-update nil
  "Automatically update Emacs packages."
  :group 'package)

(defcustom auto-package-update-interval
  7
  "Interval in DAYS for automatic package update."
  :group 'auto-package-update
  :type 'integer)

(defcustom auto-package-update-before-hook '()
  "List of functions to be called before running an automatic package update."
  :type 'hook
  :group 'auto-package-update)

(defcustom auto-package-update-after-hook '()
  "List of functions to be called after running an automatic package update."
  :type 'hook
  :group 'auto-package-update)

(defcustom auto-package-update-last-update-day-filename
  ".last-package-update-day"
  "Name of the file in which the last update day is going to be stored."
  :type 'string
  :group 'auto-package-update)

(defcustom auto-package-update-buffer-name
  "*package update results*"
  "Name of the buffer that shows updated packages and error after execution."
  :type 'string
  :group 'auto-package-update)

(defcustom auto-package-update-delete-old-versions
  nil
  "If not nil, delete old versions directories."
  :type 'boolean
  :group 'auto-package-update)

(defcustom auto-package-update-prompt-before-update
  nil
  "Prompt user (y/n) before running auto-package-update-maybe"
  :type 'boolean
  :group 'auto-package-update)

(defcustom auto-package-update-hide-results
  nil
  "If not nil, the result of auto package update in buffer
`auto-package-update-buffer-name' will not be shown."
  :type 'boolean
  :group 'auto-package-update)

(defvar auto-package-update-last-update-day-path
  (expand-file-name auto-package-update-last-update-day-filename user-emacs-directory)
  "Path to the file that will hold the day in which the last update was run.")

(defvar apu--old-versions-dirs-list
  ()
  "List with old versions directories to delete.")

;;
;;; File read/write helpers
;;
(defun apu--read-file-as-string (file)
  "Read FILE contents."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun apu--write-string-to-file (file string)
  "Substitute FILE contents with STRING."
  (with-temp-buffer
    (insert string)
    (when (file-writable-p file)
      (write-region (point-min)
                    (point-max)
                    file))))

;;
;;; Update day read/write functions
;;
(defun apu--today-day ()
  (time-to-days (current-time)))

(defun apu--write-current-day ()
  "Store current day."
  (apu--write-string-to-file
   auto-package-update-last-update-day-path
   (int-to-string (apu--today-day))))

(defun apu--read-last-update-day ()
  "Read last update day."
  (string-to-number
   (apu--read-file-as-string auto-package-update-last-update-day-path)))

;;
;;; Package update
;;
(defun apu--should-update-packages-p ()
  "Return non-nil when an update is due."
  (and
   (or
    (not (file-exists-p auto-package-update-last-update-day-path))
    (let* ((last-update-day (apu--read-last-update-day))
           (days-since (- (apu--today-day) last-update-day)))
      (>=
       (/ days-since auto-package-update-interval)
       1)))
   (apu--get-permission-to-update-p)))

(defun apu--get-permission-to-update-p ()
  "(Optionally) Prompt permission to perform update"
  (if auto-package-update-prompt-before-update
      (y-or-n-p "Auto-update packages now?")
    t))

(defun apu--package-up-to-date-p (package)
  (when (and (package-installed-p package)
             (cadr (assq package package-archive-contents)))
    (let* ((newest-desc (cadr (assq package package-archive-contents)))
           (installed-desc (cadr (or (assq package package-alist)
                                     (assq package package--builtins))))
           (newest-version  (package-desc-version newest-desc))
           (installed-version (package-desc-version installed-desc)))
      (version-list-<= newest-version installed-version))))

(defun apu--package-out-of-date-p (package)
  (not (apu--package-up-to-date-p package)))

(defun apu--packages-to-install ()
  (delete-dups (-filter 'apu--package-out-of-date-p package-activated-list)))

(defun apu--add-to-old-versions-dirs-list (package)
  "Add package old version dir to apu--old-versions-dirs-list"
  (let ((desc (cadr (assq package package-alist))))
    (add-to-list 'apu--old-versions-dirs-list (package-desc-dir desc))))

(defun apu--delete-old-versions-dirs-list ()
  "Delete package old version dirs saved in variable apu--old-versions-dirs-list"
  (dolist (old-version-dir-to-delete apu--old-versions-dirs-list)
    (delete-directory old-version-dir-to-delete t))
  ;; Clear list
  (setq apu--old-versions-dirs-list ()))

(defun apu--safe-package-install (package)
  (condition-case ex
      (progn
        (when auto-package-update-delete-old-versions
          (apu--add-to-old-versions-dirs-list package))
        (package-install-from-archive (cadr (assoc package package-archive-contents)))
        (add-to-list 'apu--package-installation-results
                     (format "%s up to date." (symbol-name package))))
    ('error (add-to-list 'apu--package-installation-results
                         (format "Error installing %s" (symbol-name package))))))

(defun apu--safe-install-packages (packages)
  (let (apu--package-installation-results)
    (dolist (package-to-update packages)
      (apu--safe-package-install package-to-update))
    (when auto-package-update-delete-old-versions
      (apu--delete-old-versions-dirs-list))
    apu--package-installation-results))

(defun apu--write-results-buffer (contents)
  (let ((inhibit-read-only t))
    (if (not auto-package-update-hide-results)
        (pop-to-buffer auto-package-update-buffer-name)
      (set-buffer
       (get-buffer-create auto-package-update-buffer-name))
      (bury-buffer
       auto-package-update-buffer-name))
    (erase-buffer)
    (insert contents)
    (read-only-mode 1)
    (auto-package-update-minor-mode 1)))

(define-minor-mode auto-package-update-minor-mode
  "Minor mode for displaying package update results."
  :group 'auto-package-update
  :keymap '(("q" . quit-window)))

;;;###autoload
(defun auto-package-update-now ()
  "Update installed Emacs packages."
  (interactive)
  (run-hooks 'auto-package-update-before-hook)

  (package-refresh-contents)

  (let ((installation-report (apu--safe-install-packages (apu--packages-to-install))))
    (apu--write-current-day)
    (apu--write-results-buffer
     (mapconcat #'identity
                (cons "[PACKAGES UPDATED]:" installation-report)
                "\n")))

  (run-hooks 'auto-package-update-after-hook))

;;;###autoload
(defun auto-package-update-at-time (time)
  "Try to update every day at the specified TIME."
  (run-at-time time 86400 'auto-package-update-maybe))

;;;###autoload
(defun auto-package-update-maybe ()
  "Update installed Emacs packages if at least \
`auto-package-update-interval' days have passed since the last update."
  (when (apu--should-update-packages-p)
    (auto-package-update-now)))

(provide 'auto-package-update)
;;; auto-package-update.el ends here
