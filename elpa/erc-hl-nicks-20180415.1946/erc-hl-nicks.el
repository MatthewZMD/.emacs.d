;;; erc-hl-nicks.el --- ERC nick highlighter that ignores uniquifying chars when colorizing

;; Copyright (C) 2011-2013  David Leatherman

;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/erc-hl-nicks
;; Package-Version: 20180415.1946
;; Version: 1.3.3

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file was originally erc-highlight-nicknames.  It was modified
;; to optionally ignore the uniquifying characters that IRC clients
;; add to nicknames

;; History

;; 1.3.3
;;
;; - Pull request #9 - switch from cl to cl-lib

;; 1.3.2
;;
;; - Pull request #7 - remove the list membership check on autoload

;; 1.3.1
;;
;; - Fix a require issue

;; 1.3.0 (was uploaded as 1.2.4, accidentally)
;;
;; - Fix autoloads - erc-hl-nicks should require itself as needed
;;
;; - reset face table is now interactive
;;
;; - reworked how colors are chosen (should continue to work the same
;;   for everyone, though). See `erc-hl-nicks-color-contrast-strategy'
;;   for details.
;;
;; - Added `erc-hl-nicks-bg-color' to allow terminal users to specify
;;   their background colors
;;
;; - Added `erc-hl-nicks-alias-nick' to allow setting up several nicks
;;   to use the same color
;;
;; - Added `erc-hl-nicks-force-nick-face' to force a nick to use a
;;   specific color

;; 1.2.3 - Updated copyright date
;;
;;       - Updated some formatting
;;
;;       - added highlighting on erc-send-modify-hook

;; 1.2.2 - Added dash to the list of characters to ignore
;;
;;       - Fixed an issue where timestamps could prevent highlighting
;;         from occurring

;; 1.2.1 - Remove accidental use of 'some' which comes from cl

;; 1.2.0 - Added erc-hl-nicks-skip-nicks to give a way to prevent
;;         certain nicks from being highlighted.
;;
;;       - Added erc-hl-nicks-skip-faces to give a way to prevent
;;         highlighting over other faces.  Defaults to:
;;         (erc-notice-face erc-fool-face erc-pal-face)

;; 1.1.0 - Remove use of cl package (was using 'reduce').
;;
;;       - The hook is called with a narrowed buffer, so it makes
;;         more sense to iterate over each word, one by one.  This
;;         is more efficient and has a secondary benefit of fixing a
;;         case issue.
;;
;;       - Added an option to not highlight fools

;; 1.0.4 - Use erc-channel-users instead of erc-server-users
;;
;;       - Ignore leading characters, too.

;; 1.0.3 - Was finding but not highlighting nicks with differing
;;         cases. Fixed. Ignore leading characters, too. Doc changes.

;; 1.0.2 - Fixed a recur issue, prevented another, and fixed a
;;         spelling issue.

;; 1.0.1 - tweaked so that the re-search will pick up instances of the
;;         trimmed nick, settled on 'nick' as the variable name
;;         instead of kw, keyword, word, etc

;; 1.0.0 - initial release

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

;;; Code:
(require 'erc)
(require 'erc-button)
(require 'cl-lib)
(require 'color)

(defgroup erc-hl-nicks nil
  "Highlighting nicknames in erc buffers"
  :group 'erc)

(defcustom erc-hl-nicks-trim-nick-for-face t
  "Ignore some characters when determining nick face"
  :group 'erc-hl-nicks
  :type 'boolean)

(defcustom erc-hl-nicks-ignore-chars ",`'_-"
  "Characters at the end of a nick to ignore while highlighting"
  :group 'erc-hl-nicks
  :type 'string)

(defcustom erc-hl-nicks-skip-nicks nil
  "Nicks to skip when highlighting"
  :group 'erc-hl-nicks
  :type '(repeat string))

(defcustom erc-hl-nicks-skip-faces
  '("erc-notice-face" "erc-pal-face" "erc-fool-face")
  "Faces to avoid overriding when highlighting"
  :group 'erc-hl-nicks
  :type  '(repeat string))

(defface erc-hl-nicks-nick-base-face
  '((t nil))
  "Base face used for highlighting nicks. (Before the nick
  color is added)"
  :group 'erc-hl-nicks)

(defvar erc-hl-nicks-minimum-luminence 85
  "The threshold to invert when the background-mode is dark")

(defvar erc-hl-nicks-maximum-luminence 170
  "The threshold to invert when the background-mode is light")

(defvar erc-hl-nicks-bg-color (cdr (assoc 'background-color (frame-parameters)))
  "The background color to use when calculating the contrast. This var is
  exposed so it can be manually set in the case of terminal emacs (which doesn't
  necessarily know the bg color).")

(defvar erc-hl-nicks-minimum-contrast-ratio 3.5
  "The amount of contrast desired between the buffer background color
  and the foreground color chosen by erc-hl-nicks. The higher the
  number the greater the contrast. A high number on a dark background
  would make all of the nicks appear in pastel/washed-out colors while
  on a dark background they may appear close to black. Somewhere
  between 3.0 and 4.5 seems to be the sweet spot.")

(defvar erc-hl-nicks-color-contrast-strategies
  '((invert . erc-hl-nicks-invert-for-visibility)
    (contrast . erc-hl-nicks-fix-color-contrast))
  "An alist of strategies available and their functions:

  'invert - if the color is too dark/light to be seen based on the
  bg-mode (dark or light) of the frame, simply invert the color.

  'contrast - attempt to achieve a decent contrast ratio (specified by
  `erc-hl-nicks-minimum-contrast-ratio') by brightening or darkening
  the color")

(defvar erc-hl-nicks-color-contrast-strategy 'invert
  "How should erc-hl-nicks attempt to make the nick colors visible?
  The options are listed in `erc-hl-nicks-color-contrast-strategies'

  This option can be a list and will be applied in the order defined.
  That is, '(invert contrast) will invert as needed and then adjust
  the color as needed.")

(defvar erc-hl-nicks-face-table
  (make-hash-table :test 'equal)
  "The hash table that contains unique nick faces.")

;; for debugging
(defun erc-hl-nicks-reset-face-table ()
  (interactive)
  (setq erc-hl-nicks-face-table
        (make-hash-table :test 'equal)))

(defun erc-hl-nicks-hexcolor-luminance (color)
  "Returns the luminance of color COLOR. COLOR is a string \(e.g.
  \"#ffaa00\", \"blue\"\) `color-values' accepts. Luminance is a
  value of 0.299 red + 0.587 green + 0.114 blue and is always
  between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (floor (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 256)))

(defun erc-hl-nicks-invert-color (color)
  "Returns the inverted color of COLOR."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (format "#%04x%04x%04x"
            (- 65535 r) (- 65535 g) (- 65535 b))))

(defun erc-hl-nicks-trim-irc-nick (nick)
  "Removes instances of erc-hl-nicks-ignore-chars from both sides of NICK"
  (let ((stripped (replace-regexp-in-string
                   (format "\\([%s]\\)+$" erc-hl-nicks-ignore-chars)
                   "" nick)))
    (replace-regexp-in-string
     (format "^\\([%s]\\)+" erc-hl-nicks-ignore-chars)
     "" stripped)))

(defun erc-hl-nicks-brightness-contrast (c1 c2)
  "Determines the amount of contrast between C1 and C2"
  (let* ((l1 (erc-hl-nicks-hexcolor-luminance c1))
         (l2 (erc-hl-nicks-hexcolor-luminance c2))
         (d (if (< l1 l2) l1 l2))
         (b (if (equal d l1) l2 l1)))
    (/ (+ 0.05 b) (+ 0.05 d))))

(defun erc-hl-nicks-fix-color-contrast (color)
  "Adjusts COLOR by blending it with white or black, based on
  background-mode until there is enough contrast between COLOR and
  the background color. See `erc-hl-nicks-minimum-contrast-ratio' to
  adjust how far to blend the color."
  (if (and erc-hl-nicks-minimum-contrast-ratio
           (< 0 erc-hl-nicks-minimum-contrast-ratio))
      (cl-some
       (lambda (c)
         (let ((hex (color-rgb-to-hex (nth 0 c) (nth 1 c) (nth 2 c))))
           (when (> (erc-hl-nicks-brightness-contrast erc-hl-nicks-bg-color hex)
                    erc-hl-nicks-minimum-contrast-ratio)
             hex)))
       (let ((bg-mode (cdr (assoc 'background-mode (frame-parameters)))))
         (color-gradient
          (color-name-to-rgb color)
          (color-name-to-rgb
           (if (equal 'dark bg-mode) "white" "black"))
          512)))
    color))

(defun erc-hl-nicks-invert-for-visibility (color)
  "Inverts the given color based on luminence and background-mode
  (dark or light)."
  (let ((bg-mode (cdr (assoc 'background-mode (frame-parameters)))))
    (cond
     ((and (equal 'dark bg-mode)
           (< (erc-hl-nicks-hexcolor-luminance color)
              erc-hl-nicks-minimum-luminence))
      (erc-hl-nicks-invert-color color))
     ((and (equal 'light bg-mode)
           (> (erc-hl-nicks-hexcolor-luminance color)
              erc-hl-nicks-maximum-luminence))
      (erc-hl-nicks-invert-color color))
     (t color))))

(defun erc-hl-nicks-color-for-nick (nick)
  "Get the color to use for the given nick by calculating the color
  and applying the contrast strategies to it."
  (let ((color (concat "#" (substring (md5 (downcase nick)) 0 12))))
    (cl-reduce
     (lambda (color strategy)
       (let ((fn (cdr (assq strategy erc-hl-nicks-color-contrast-strategies))))
         (if fn
             (funcall fn color)
           color)))
     (erc-hl-nicks-ensure-list erc-hl-nicks-color-contrast-strategy)
     :initial-value color)))

(defun erc-hl-nicks-face-name (nick)
  (make-symbol (concat "erc-hl-nicks-nick-" nick "-face")))

(defun erc-hl-nicks-make-face (nick)
  "Create and cache a new face for the given nick"
  (or (gethash nick erc-hl-nicks-face-table)
      (let ((color (erc-hl-nicks-color-for-nick nick))
            (new-nick-face (erc-hl-nicks-face-name nick)))
        (copy-face 'erc-hl-nicks-nick-base-face new-nick-face)
        (set-face-foreground new-nick-face color)
        (puthash nick new-nick-face erc-hl-nicks-face-table))))

(defun erc-hl-nicks-ensure-list (maybe-list)
  (if (listp maybe-list)
      maybe-list
    (list maybe-list)))

(defun erc-hl-nicks-has-skip-face-p (pt)
  (remq nil (mapcar (lambda (face)
                      (member (symbol-name face) erc-hl-nicks-skip-faces))
                    (erc-hl-nicks-ensure-list
                     (get-text-property pt 'face)))))

(defun erc-hl-nicks-highlight-p (nick trimmed bounds)
  (and erc-channel-users
       (erc-get-channel-user nick)
       (not (member trimmed erc-hl-nicks-skip-nicks))
       (not (erc-hl-nicks-has-skip-face-p (car bounds)))))

;;;###autoload
(defun erc-hl-nicks-force-nick-face (nick color)
  "Force nick highlighting to be a certain color for a nick. Both NICK and COLOR
  should be strings."
  (let ((new-nick-face (erc-hl-nicks-face-name nick)))
    (copy-face 'erc-hl-nicks-nick-base-face new-nick-face)
    (set-face-foreground new-nick-face color)
    (puthash nick new-nick-face erc-hl-nicks-face-table)))

;;;###autoload
(defun erc-hl-nicks-alias-nick (nick &rest nick-aliases)
  "Manually handle the really wacked out nickname transformations."
  (erc-hl-nicks-make-face nick)
  (let ((nick-face (gethash nick erc-hl-nicks-face-table)))
    (dolist (nick-alias nick-aliases)
      (puthash nick-alias nick-face erc-hl-nicks-face-table))))

;;;###autoload
(defun erc-hl-nicks ()
  "Retrieves a list of usernames from the server and highlights them"
  (save-excursion
    (with-syntax-table erc-button-syntax-table
      (let ((inhibit-field-text-motion t))
        (goto-char (point-min))
        (while (forward-word 1)
          (let ((word (word-at-point)))
            (when word
              (let ((trimmed (erc-hl-nicks-trim-irc-nick word))
                    (bounds (bounds-of-thing-at-point 'word))
                    (inhibit-read-only t))
                (when (erc-hl-nicks-highlight-p word trimmed bounds)
                  (erc-button-add-face (car bounds) (cdr bounds)
                                       (erc-hl-nicks-make-face trimmed)))))))))))

(defun erc-hl-nicks-fix-hook-order (&rest _)
  (remove-hook 'erc-insert-modify-hook 'erc-hl-nicks)
  (add-hook 'erc-insert-modify-hook 'erc-hl-nicks t)
  (remove-hook 'erc-send-modify-hook 'erc-hl-nicks)
  (add-hook 'erc-send-modify-hook 'erc-hl-nicks t))

(define-erc-module hl-nicks nil
  "Highlight usernames in the buffer"
  ((add-hook 'erc-insert-modify-hook 'erc-hl-nicks t)
   (add-hook 'erc-send-modify-hook 'erc-hl-nicks t)
   (add-hook 'erc-connect-pre-hook 'erc-hl-nicks-fix-hook-order t))
  ((remove-hook 'erc-insert-modify-hook 'erc-hl-nicks)
   (remove-hook 'erc-send-modify-hook 'erc-hl-nicks)
   (remove-hook 'erc-connect-pre-hook 'erc-hl-nicks-fix-hook-order)))

;; For first time use
;;;###autoload
(when (boundp 'erc-modules)
  (add-to-list 'erc-modules 'hl-nicks))

(provide 'erc-hl-nicks)

;;;###autoload
(eval-after-load 'erc
  '(progn
     (unless (featurep 'erc-hl-nicks)
       (require 'erc-hl-nicks))
     (add-to-list 'erc-modules 'hl-nicks t)))

;;; erc-hl-nicks.el ends here
