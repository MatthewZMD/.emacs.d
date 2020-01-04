;;; erc-image.el --- Show received image urls in the ERC buffer -*- lexical-binding: t -*-

;; Copyright (C) 2012  Jon de Andrés Frías
;; Copyright (C) 2012  Raimon Grau Cuscó
;; Copyright (C) 2012  David Vázquez

;; Author: Jon de Andrés Frías <jondeandres@gmail.com>
;;         Raimon Grau Cuscó <raimonster@gmail.com>
;; Version: 0.9
;; Package-Version: 20180522.1424
;; Keywords: multimedia

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
;;
;; Show inlined images (png/jpg/gif/svg) in erc buffers.  Requires
;; Emacs 24.2
;;
;; (require 'erc-image)
;; (add-to-list 'erc-modules 'image)
;; (erc-update-modules)
;;
;; Or `(require 'erc-image)` and  `M-x customize-option erc-modules RET`
;;
;; This plugin subscribes to hooks `erc-insert-modify-hook' and
;; `erc-send-modify-hook' to download and show images.  In this early
;; version it's doing this synchronously.
;;
;; The function used to display the image is bound to the variable
;; `erc-image-display-func'. There are two possible values for that,
;; `erc-image-insert-inline' and `erc-image-insert-other-buffer'.
;;
;; Set the value of erc-image-inline-rescale to a number for max size
;; of images to be displayed (resized if bigger). set the value to
;; 'window to take window height as this value
;;
;;; Code:


(require 'erc)
(require 'url-queue)
(require 'image-dired)

(defgroup erc-image nil
  "Enable image."
  :group 'erc)

(defcustom erc-image-regex-alist
  '(("https?://\\(www\\.\\)?giphy\\.com" .
     erc-image-get-giphy-url)
    ("https?://\\(www\\.\\)?imgur\\.com" .
     erc-image-get-imgur-url)
    ("\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)$" .
     erc-image-show-url-image))
  "Pairs of regex and function to match URLs to be downloaded.
The function needs to have one argument to which the url will be
supplied and it should return the real URL to download an image.
If several regex match prior occurring have higher priority."
  :group 'erc-image
  :type '(alist :key-type string :value-type function))

(defcustom erc-image-images-path temporary-file-directory
  "Path where to store downloaded images."
  :group 'erc-image)

(defcustom erc-image-display-func 'erc-image-insert-inline
  "Function to use to display the image."
  :group 'erc-image
  :type '(choice (const :tag "Inline" 'erc-image-insert-inline)
                 (const :tag "Other buffer" 'erc-image-insert-other-buffer)
                 function))

(defcustom erc-image-inline-rescale nil
  "Rescale the inline displayed image if non-nil. If the value is 'window, scale the
  image down to the window size if it is bigger. When entering a
  numeric value, the image will be scaled to that size."
  :group 'erc-image
  :type '(choice
          (const :tag "No scaling" nil)
          (const :tag "Scale down to window-size" window)
          (integer :tag "Scale down to specific value")))


(when (version< emacs-version "24.4")
  (fset 'image-multi-frame-p 'image-animated-p))

(defun erc-image-insert-other-buffer (status file-name marker)
  "Open a new buffer and display file-name image there, scaled."
  (if (equal (car status) :error)
      (message "%s" (error-message-string (cadr status)))
    (goto-char (point-min))
    (search-forward "\n\n")
    (let ((coding-system-for-write 'binary))
      (write-region (point) (point-max) file-name))
    (image-dired-create-display-image-buffer)
    (display-buffer image-dired-display-image-buffer)
    (image-dired-display-image file-name)))

(defun erc-image-insert-inline (status file-name marker)
  "Open file-name image in the marker position."
  (if (equal (car status) :error)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (marker-position marker))
            (insert-before-markers
             (or erc-fill-prefix "")
             (format "%s\n" (error-message-string (cadr status)))))))
    (goto-char (point-min))
    (search-forward "\n\n")
    (let ((coding-system-for-write 'binary))
      (write-region (point) (point-max) file-name))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (let ((inhibit-read-only t)
              (im (erc-image-create-image file-name))
              (map (make-sparse-keymap)))
          (goto-char (marker-position marker))
          (let ((pt-before (point)))
            (insert-before-markers
             (or erc-fill-prefix "")
             (propertize " " 'display im 'keymap map
                         'help-echo (when (image-multi-frame-p im)
                                      "mouse-2 or RET: replay animation"))
             "\n")
            (when (image-multi-frame-p im)
              (image-animate im)
              (let ((animate (lambda () (interactive) (image-animate im))))
                (define-key map [mouse-2] animate)
                (define-key map (kbd "RET") animate)))
            (put-text-property pt-before (point) 'read-only t)))))))

(defun erc-image-create-image (file-name)
  "Create an image suitably scaled according to the setting of
'ERC-IMAGE-RESCALE."
  (let* ((positions (window-inside-absolute-pixel-edges))
         (width (- (nth 2 positions) (nth 0 positions)))
         (height (- (nth 3 positions) (nth 1 positions)))
         (image (create-image file-name))
         (dimensions (image-size image t)))
                                        ; See if we want to rescale the image
    (if (and (fboundp 'imagemagick-types) erc-image-inline-rescale
             (not (image-multi-frame-p image)))
        ;; Rescale based on erc-image-rescale
        (cond (;; Numeric: scale down to that size
               (numberp erc-image-inline-rescale)
               (create-image file-name 'imagemagick nil :height erc-image-inline-rescale))
              (;; 'window: scale down to window size, if bigger
               (eq erc-image-inline-rescale 'window)
               ;; But only if the image is greater than the window size
               (if (or (> (car dimensions) width)
                       (> (cdr dimensions) height))
                   ;; Figure out in which direction we need to scale
                   (if (> width height)
                       (create-image file-name 'imagemagick nil :height  height)
                     (create-image file-name 'imagemagick nil :width width))
                 ;; Image is smaller than window, just give that back
                 image))
              (t (progn (message "Error: none of the rescaling options matched") image)))
      ;; No rescale
      image)))

                                        ;(image-dired-display-image FILE &optional ORIGINAL-SIZE)

(defun erc-image-show-url-image (url)
  (when (and url (display-graphic-p))
    (let ((file-name (expand-file-name (md5 url) erc-image-images-path)))
      (goto-char (point-max))
      (url-queue-retrieve url
                          erc-image-display-func
                          (list
                           file-name
                           (point-marker))
                          t))))

(defun erc-image-show-url ()
  "Calls the proper function to process an URL"
  (goto-char (point-min))
  (search-forward "http" nil t)
  (let ((url (thing-at-point 'url)))
    (when url
      (catch 'download-url
        (dolist (pair erc-image-regex-alist)
          (let ((re (car pair))
                (f (cdr pair)))
            (when (string-match-p re url)
              (throw 'download-url (funcall f url)))))))))

(defun erc-image-get-giphy-url (url)
  "Return the download URL for the giphy `url'."
  (let ((id (progn (string-match "\\([0-9a-zA-Z]*\\)$" url)
                   (match-string 1 url))))
    (erc-image-show-url-image (format "https://media.giphy.com/media/%s/200w_d.gif" id))))

(defun erc-image-get-imgur-url (url)
  "Return the download URL for the imgur `url'."
  (let ((id (progn (string-match "/\\([^/]*?\\)$" url)
                   (match-string 1 url))))
    (erc-image-show-url-image (format "http://imgur.com/download/%s" id))))

;;;###autoload
(eval-after-load 'erc
  '(define-erc-module image nil
     "Display inlined images in ERC buffer"
     ((add-hook 'erc-insert-modify-hook 'erc-image-show-url t)
      (add-hook 'erc-send-modify-hook 'erc-image-show-url t))
     ((remove-hook 'erc-insert-modify-hook 'erc-image-show-url)
      (remove-hook 'erc-send-modify-hook 'erc-image-show-url))
     t))

(provide 'erc-image)
;;; erc-image.el ends here
