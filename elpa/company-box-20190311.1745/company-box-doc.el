;;; company-box-doc.el --- Company front-end  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/company-box

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Display candidate's documentation in another frame

;;; Code:

(require 'dash)
(require 'company)

(defgroup company-box-doc nil
  "Display documentation popups alongside company-box"
  :group 'company)

(defcustom company-box-doc-enable t
  "Enable company-box-doc by default."
  :type 'boolean
  :safe #'booleanp
  :group 'company-box-doc)

(defcustom company-box-doc-delay 0.5
  "The number of seconds to wait before displaying the popup."
  :group 'company-box-doc)

(defvar company-box-doc-frame-parameters
  '((internal-border-width . 10))
  "Frame parameters to use on the doc frame.
`company-box-frame-parameters' is then append to this variable.")

(declare-function company-box--get-frame 'company-box)
(declare-function company-box--set-frame 'company-box)
(declare-function company-box--get-buffer 'company-box)
(declare-function company-box--make-frame 'company-box)

(defvar company-box-frame-parameters)
(defvar company-box--bottom)

(defvar-local company-box-doc--timer nil)

(defun company-box-doc--fetch-doc-buffer (candidate)
  (let ((inhibit-message t))
    (-some-> (company-call-backend 'doc-buffer candidate)
             (get-buffer))))

(defun company-box-doc--set-frame-position (frame)
  (-let* ((box-position (frame-position (company-box--get-frame)))
          (box-width (frame-pixel-width (company-box--get-frame)))
          (window (frame-root-window frame))
          (frame-resize-pixelwise t)
          ((width . height) (window-text-pixel-size window nil nil 10000 10000))
          (bottom (+ company-box--bottom (window-pixel-top) (frame-border-width)))
          (x (+ (car box-position) box-width (/ (frame-char-width) 2)))
          (y (cdr box-position))
          (y (if (> (+ y height 20) bottom)
                 (- y (- (+ y height) bottom) 20)
               y))
          (space-right (- (frame-pixel-width) x))
          (space-left (car box-position))
          (x (or (let ((border (* (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0)
                                  2)))
                   (and (> width space-right)
                        (> space-left (+ width border (/ (frame-char-width) 2)))
                        (- (car box-position) width border (/ (frame-char-width) 2))))
                 x)))
    (set-frame-position frame (max x 0) (max y 0))
    (set-frame-size frame width height t)))

(defun company-box-doc--make-buffer (object)
  (let ((string (cond ((stringp object) object)
                      ((bufferp object) (with-current-buffer object (buffer-string))))))
    (when (> (length (string-trim string)) 0)
      (with-current-buffer (company-box--get-buffer "doc")
        (erase-buffer)
        (insert string)
        (setq mode-line-format nil
              display-line-numbers nil
              header-line-format nil
              cursor-in-non-selected-windows nil)
        (current-buffer)))))

(defun company-box-doc--make-frame (buffer)
  (let* ((company-box-frame-parameters
          (append company-box-doc-frame-parameters
                  company-box-frame-parameters))
         (frame (company-box--make-frame buffer)))
    ;; (set-face-background 'internal-border "white" frame)
    (set-frame-parameter frame 'name "")
    frame))

(defun company-box-doc--show (selection frame)
  (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                 (company-box--get-frame)
                                 (frame-visible-p (company-box--get-frame))))
               (candidate (nth selection company-candidates))
               (doc (or (company-call-backend 'quickhelp-string candidate)
                        (company-box-doc--fetch-doc-buffer candidate)))
               (doc (company-box-doc--make-buffer doc)))
    (unless (frame-live-p (frame-parameter nil 'company-box-doc-frame))
      (set-frame-parameter nil 'company-box-doc-frame (company-box-doc--make-frame doc)))
    (company-box-doc--set-frame-position (frame-parameter nil 'company-box-doc-frame))
    (unless (frame-visible-p (frame-parameter nil 'company-box-doc-frame))
      (make-frame-visible (frame-parameter nil 'company-box-doc-frame)))))

(defun company-box-doc (selection frame)
  (when company-box-doc-enable
    (-some-> (frame-parameter frame 'company-box-doc-frame)
             (make-frame-invisible))
    (when (timerp company-box-doc--timer)
      (cancel-timer company-box-doc--timer))
    (setq company-box-doc--timer
          (run-with-idle-timer
           company-box-doc-delay nil
           (lambda nil
             (company-box-doc--show selection frame)
             (company-ensure-emulation-alist))))))

(defun company-box-doc--hide (frame)
  (-some-> (frame-parameter frame 'company-box-doc-frame)
           (make-frame-invisible)))

(defun company-box-doc-manually ()
  (interactive)
  (company-box-doc--show company-selection (or (frame-parent) (selected-frame))))

(define-key company-active-map [remap company-show-doc-buffer] 'company-box-doc-manually)

(provide 'company-box-doc)
;;; company-box-doc.el ends here
