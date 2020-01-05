;;; -*- lexical-binding: t -*-
;;; idris-log.el --- Logging of Idris

;; Copyright (C) 2013-2014 Hannes Mehnert and David Raymond Christiansen

;; Authors: Hannes Mehnert <hannes@mehnert.org>
;;          David Raymond Christiansen <drc@itu.dk>
;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'idris-core)
(require 'idris-common-utils)

(defvar idris-log-buffer-name (idris-buffer-name :log)
  "The name of the Idris log buffer.")

(defface idris-log-timestamp-face
  '((t :foreground "#211ab0"
       :weight bold))
  "The face used for timestamps in the Idris log"
  :group 'idris-faces)

(defface idris-log-level-face
  '((t :weight bold))
  "General properties for displaying Idris log levels"
  :group 'idris-faces)

(defface idris-log-level-1-face
  '((t :foreground "#ff0011"))
  "The face used for log level 1 in the Idris log"
  :group 'idris-faces)

(defface idris-log-level-2-face
  '((t :foreground "#dd0033"))
  "The face used for log level 2 in the Idris log"
  :group 'idris-faces)

(defface idris-log-level-3-face
  '((t :foreground "#bb0055"))
  "The face used for log level 3 in the Idris log"
  :group 'idris-faces)

(defface idris-log-level-4-face
  '((t :foreground "#990077"))
  "The face used for log level 4 in the Idris log"
  :group 'idris-faces)

(defface idris-log-level-5-face
  '((t :foreground "#770099"))
  "The face used for log level 5 in the Idris log"
  :group 'idris-faces)

(defface idris-log-level-higher-face
  '((t :foreground "#550099"))
  "The face used for log levels over 5 in the Idris log"
  :group 'idris-faces)

(defun idris-get-log-level-face (level)
  (cond ((= level 1) 'idris-log-level-1-face)
        ((= level 2) 'idris-log-level-2-face)
        ((= level 3) 'idris-log-level-3-face)
        ((= level 4) 'idris-log-level-4-face)
        ((= level 5) 'idris-log-level-5-face)
        (t 'idris-log-level-higher-face)))

(defvar idris-log-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map) ; remove self-inserting char commands
    map))

(define-derived-mode idris-log-mode fundamental-mode "Idris Log"
  "Major mode used to show Idris compiler internals logs
      \\{idris-log-mode-map}
Invokes `idris-log-mode-hook'."
  (buffer-disable-undo)
  (set (make-local-variable 'outline-regexp) "^(")
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'left-margin-width) 22)
  (setq buffer-read-only t)
  (view-mode 1))

(defun idris-log-buffer ()
  "Return or create the log buffer."
  (or (get-buffer idris-log-buffer-name)
      (let ((buffer (get-buffer-create idris-log-buffer-name)))
        (with-current-buffer buffer
          (idris-log-mode))
        buffer)))

(defun idris-log (level message)
  "Record the fact that MESSAGE occured."
  ;; TODO: Different faces for different log levels
  (with-current-buffer (idris-log-buffer)
    (goto-char (point-max))
    (let* ((buffer-read-only nil)
           (time (format-time-string "%Y-%m-%d %H:%M:%S"))
           (meta-info (concat (propertize time 'face 'idris-log-timestamp-face)
                              (propertize (format "%2s" level)
                                          'face (idris-get-log-level-face level))))
           (meta (propertize " "
                             'display `((margin left-margin)
                                        ,meta-info))))
      (save-excursion
        (insert meta)
        (insert message)
        (insert "\n")))
    (goto-char (point-max))))

(defun idris-log-hook-function (event)
  (pcase event
    (`(:log (,level ,message) ,_target)
     (idris-log level message)
     t)
    (_ nil)))

(provide 'idris-log)
