;;; all-the-icons-dired.el --- Shows icons for each file in dired mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  jtbm37

;; Author: jtbm37
;; Version: 1.0
;; Package-Version: 20170418.2131
;; Keywords: files icons dired
;; Package-Requires: ((emacs "24.4") (all-the-icons "2.2.0"))

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
;; To use this package, simply add this to your init.el:
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; To manually install, add this to your init.el before the hook mentioned above.
;; (add-to-load-path (expand-file-name "~/path/to/all-the-icons-dired"))
;; (load "all-the-icons-dired.el")


;;; Code:
(require 'dired)
(require 'all-the-icons)

(defface all-the-icons-dired-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon"
  :group 'all-the-icons-faces)

(defcustom all-the-icons-dired-v-adjust 0.01
  "The default vertical adjustment of the icon in the dired buffer."
  :group 'all-the-icons
  :type 'number)

(defvar-local all-the-icons-dired-displayed nil
  "Flags whether icons have been added.")

(defun all-the-icons-dired--display ()
  "Display the icons of files in a dired buffer."
  (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
    (setq-local all-the-icons-dired-displayed t)
    (let ((inhibit-read-only t)
	  (remote-p (and (fboundp 'tramp-tramp-file-p)
                         (tramp-tramp-file-p default-directory))))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (when (dired-move-to-filename nil)
	    (let ((file (dired-get-filename 'verbatim t)))
	      (unless (member file '("." ".."))
		(let ((filename (dired-get-filename nil t)))
		  (if (file-directory-p filename)
		      (let* ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist))
			     (icon (cond
				    (remote-p
				     (all-the-icons-octicon "file-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
				    ((file-symlink-p filename)
				     (all-the-icons-octicon "file-symlink-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
				    ((all-the-icons-dir-is-submodule filename)
				     (all-the-icons-octicon "file-submodule" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
				    ((file-exists-p (format "%s/.git" filename))
				     (all-the-icons-octicon "repo" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
				    (t (apply (car matcher) (list (cadr matcher) :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))))))
			(insert (concat icon " ")))
		    (insert (concat (all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust) " ")))))))
	  (forward-line 1))))))

(defun all-the-icons-dired--reset (&optional _arg _noconfirm)
  "Functions used as advice when redisplaying buffer."
  (setq-local all-the-icons-dired-displayed nil))

;;;###autoload
(define-minor-mode all-the-icons-dired-mode
  "Display all-the-icons icon for each files in a dired buffer."
  :lighter " all-the-icons-dired-mode"
  (if (and (display-graphic-p) all-the-icons-dired-mode)
      (progn
        (add-hook 'dired-after-readin-hook 'all-the-icons-dired--display t t)
        (when (derived-mode-p 'dired-mode)
          (all-the-icons-dired--display)))
    (remove-hook 'dired-after-readin-hook 'all-the-icons-dired--display t)
    (dired-revert)))

(advice-add 'dired-revert :before #'all-the-icons-dired--reset)

(provide 'all-the-icons-dired)
;;; all-the-icons-dired.el ends here
