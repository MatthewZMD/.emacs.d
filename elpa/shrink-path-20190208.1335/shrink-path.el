;;; shrink-path.el --- fish-style path -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Benjamin Andresen

;; Author: Benjamin Andresen
;; Version: 0.3.1
;; Package-Version: 20190208.1335
;; URL: https://gitlab.com/bennya/shrink-path.el
;; Package-Requires: ((emacs "24") (s "1.6.1") (dash "1.8.0") (f "0.10.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides functions that offer fish shell[1] path truncation.
;; Directory /usr/share/emacs/site-lisp => /u/s/e/site-lisp
;;
;; Also includes utility functions that make integration in eshell or the
;; modeline easier.
;;
;; [1] https://fishshell.com/


;;; Code:
(require 'dash)
(require 's)
(require 'f)
(require 'rx)

(defun shrink-path--truncate (str)
  "Return STR's first character or first two characters if hidden."
  (substring str 0 (if (s-starts-with? "." str) 2 1)))

(defun shrink-path--dirs-internal (full-path &optional truncate-all)
  "Return fish-style truncated string based on FULL-PATH.
Optional parameter TRUNCATE-ALL will cause the function to truncate the last
directory too."
  (let* ((home (expand-file-name "~"))
         (path (replace-regexp-in-string
                (s-concat "^" home) "~" full-path))
         (split (s-split "/" path 'omit-nulls))
         (split-len (length split))
         shrunk)
    (->> split
         (--map-indexed (if (= it-index (1- split-len))
                            (if truncate-all (shrink-path--truncate it) it)
                          (shrink-path--truncate it)))
         (s-join "/")
         (setq shrunk))
    (s-concat (unless (s-matches? (rx bos (or "~" "/")) shrunk) "/")
              shrunk
              (unless (s-ends-with? "/" shrunk) "/"))))


(defun shrink-path-dirs (&optional path truncate-tail)
  "Given PATH return fish-styled shrunken down path.
TRUNCATE-TAIL will cause the function to truncate the last directory too."
  (let* ((path (or path default-directory))
         (path (f-full path)))
    (cond
     ((s-equals? (f-short path) "/") "/")
     ((s-matches? (rx bos (or "~" "/") eos) "~/"))
     (t (shrink-path--dirs-internal path truncate-tail)))))

(defun shrink-path-expand (str &optional absolute-p)
  "Return expanded path from STR if found or list of matches on multiple.
The path referred to by STR has to exist for this to work.
If ABSOLUTE-P is t the returned path will be absolute."
  (let* ((str-split (s-split "/" str 'omit-nulls))
         (head (car str-split)))
    (if (= (length str-split) 1)
        (s-concat "/" str-split)
      (--> (-drop 1 str-split)   ;; drop head
           (-map (lambda (e) (s-concat e "*")) it)
           (-drop-last 1 it)     ;; drop tail as it may not exist
           (s-join "/" it)
           (s-concat (if (s-equals? head "~") "~/" head) it)
           (f-glob it)
           (-map (lambda (e) (s-concat e "/" (-last-item str-split))) it)
           (if absolute-p (-map #'f-full it) (-map #'f-abbrev it))
           (if (= (length it) 1) (car it) it)))))

(defun shrink-path-prompt (&optional pwd)
  "Return cons of BASE and DIR for PWD.
If PWD isn't provided will default to `default-directory'."
  (let* ((pwd (or pwd default-directory))
         (shrunk (shrink-path-dirs pwd))
         (split (--> shrunk (s-split "/" it 'omit-nulls)))
         base dir)
    (setq dir (or (-last-item split) "/"))
    (setq base (if (s-equals? dir "/") ""
                 (s-chop-suffix (s-concat dir "/") shrunk)))
    (cons base dir)))

(defun shrink-path-file (file &optional truncate-tail)
  "Return FILE's shrunk down path and filename.
TRUNCATE-TAIL controls if the last directory should also be shortened."
  (let ((filename (f-filename file))
        (dirname (f-dirname file)))
    (s-concat (shrink-path-dirs dirname truncate-tail) filename)))

(defun shrink-path-file-expand (str &optional exists-p absolute-p)
  "Return STR's expanded filename.
The path referred to by STR has to exist for this to work.
If EXISTS-P is t the filename also has to exist.
If ABSOLUTE-P is t the returned path will be absolute."
  (let ((expanded (shrink-path-expand str absolute-p)))
    (if (and expanded exists-p)
        (if (f-exists? expanded) expanded)
      expanded)))

(defun shrink-path-file-mixed (shrink-path rel-path filename)
  "Returns list of mixed truncated file name locations.

Consists of SHRINK-PATH's parent, SHRINK-PATH basename, relative REL-PATH and
FILENAME.
For use in modeline or prompts, etc."
  (let ((shrunk-dirs (shrink-path-prompt shrink-path))
        sp-parent sp-rel rel-rel nd-file)

    (when (f-descendant-of? filename shrink-path)
      (when shrunk-dirs
        (setq sp-parent (car shrunk-dirs)
              sp-rel (cdr shrunk-dirs)))
      (setq rel-rel (if (or (f-same? rel-path shrink-path)
                            (s-equals? (f-relative rel-path shrink-path) "."))
                        nil
                      (f-relative rel-path shrink-path)))
      (setq nd-file (file-name-nondirectory filename))

      (list sp-parent sp-rel rel-rel nd-file))))

(provide 'shrink-path)
;;; shrink-path.el ends here
