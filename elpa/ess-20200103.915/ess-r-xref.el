;;; ess-r-xref.el --- An xref backend for R. -*- lexical-binding: t -*-
;;
;; Author: Aaron Jacobs
;; Created: 21 January 2018
;; Maintainer: ESS-core <ESS-core@r-project.org>
;;
;; Keywords: languages, statistics, xref
;;
;; This file is part of ESS.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/

;;; Commentary:

;; This file contains an xref backend for `ess-r-mode'.

;;; Code:

(require 'xref)
(require 'ess-inf)
(require 'ess-r-package)
(require 'ess-tracebug)

;; Silence the byte compiler. OK because this file is only loaded by ess-r-mode.
(declare-function inferior-ess-r-force "ess-r-mode")


(defvar ess-r-xref-pkg-sources nil
  "Alist of R package->directory associations.
This variable is used as a cache of package->directory
associations, but could be used by the users for a more refined
control of package locations than `ess-r-package-library-paths'.")

(defun ess-r-xref-backend ()
  "An `xref-backend-functions' implementation for `ess-r-mode'.
R's xref backend searches for `ess-r-package-library-paths' when
srcrefs point to temporary locations."
  'ess-r)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ess-r)))
  (let ((sym (ess-symbol-at-point)))
    (when sym
      (symbol-name sym))))

(cl-defmethod xref-backend-definitions ((_backend (eql ess-r)) symbol)
  (let ((xref (ess-r-xref--xref symbol)))
    (when xref
      (list xref))))

(cl-defmethod xref-backend-apropos ((_backend (eql ess-r)))
  ;; Not yet supported.
  nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ess-r)))
  (inferior-ess-r-force)
  (ess-get-words-from-vector ".ess_all_functions()\n"))

(defun ess-r-xref--srcref (symbol)
  (inferior-ess-r-force)
  ;; Look for `symbol' inside the package namespace
  (let* ((pkg (ess-r-package-name))
         (pkg (if pkg
                  (concat "\"" pkg "\"")
                "NULL")))
    (with-current-buffer (ess-command (format ".ess_srcref(\"%s\", %s)\n" symbol pkg))
      (goto-char (point-min))
      (when (re-search-forward "(" nil 'noerror)
        (goto-char (match-beginning 0))
        (read (current-buffer))))))

(defun ess-r-xref--pkg-srcfile (symbol r-src-file)
  "Look in the source directory of the R package containing symbol SYMBOL for R-SRC-FILE."
  (let* ((env-name (ess-string-command (format ".ess_fn_pkg(\"%s\")\n" symbol)))
         (pkg (if (string-equal env-name "")
                  (user-error "Can't find package for symbol %s" symbol)
                env-name))
         (dir (or (assoc-default pkg ess-r-xref-pkg-sources)
                  (cond ((stringp ess-r-package-library-paths)
                         (expand-file-name pkg ess-r-package-library-paths))
                        ((listp ess-r-package-library-paths)
                         (cl-loop for d in ess-r-package-library-paths
                                  for p = (expand-file-name pkg d)
                                  when (file-exists-p p) return p))
                        (t (user-error "Invalid value of `ess-r-package-library-paths'")))))
         (file (when dir (expand-file-name r-src-file dir))))
    (when file
      (unless (file-readable-p file)
        (user-error "Can't read %s" file))
      ;; Cache package's source directory.
      (unless (assoc pkg ess-r-xref-pkg-sources)
        (push `(,pkg . ,dir) ess-r-xref-pkg-sources))
      file)))

(defun ess-r-xref--xref (symbol)
  "Create an xref for the source file reference of R symbol SYMBOL."
  (let ((ref (ess-r-xref--srcref symbol)))
    (when ref
      (let ((file (nth 0 ref))
            (line (nth 1 ref))
            (col  (nth 2 ref)))
        (or
         ;; 1) Result of ESS evaluation
         (let* ((ess-ref (gethash file ess--srcrefs))
                (ess-buff (when ess-ref (ess--dbg-find-buffer (car ess-ref)))))
           (when ess-buff
             ;; FIXME: this breaks when eval is on larger spans than function
             (xref-make symbol (xref-make-buffer-location ess-buff (nth 2 ess-ref)))))
         ;; 2) Actual file location
         (when (file-readable-p file)
           (xref-make symbol (xref-make-file-location file line col)))
         ;; 3) Temporary sources - truncate and locate in ess-r-package-library-paths
         (when (string-match "/\\(R/.*\\)$" file)
           (let ((pkg-file (ess-r-xref--pkg-srcfile symbol (match-string 1 file))))
             (when pkg-file
               (xref-make symbol (xref-make-file-location
                                  (expand-file-name pkg-file) line col))))))))))

(provide 'ess-r-xref)

;;; ess-r-xref.el ends here
