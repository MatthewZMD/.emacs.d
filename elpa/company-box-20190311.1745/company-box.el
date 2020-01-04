;;; company-box.el --- Company front-end with icons  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/company-box
;; Keywords: company, completion, front-end, convenience
;; Package-Requires: ((emacs "26.0.91") (dash "2.13") (dash-functional "1.2.0") (company "0.9.6"))
;; Version: 0.0.1

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
;; A company front-end
;;
;; Differences with the built-in front-end:
;;
;; - Different colors for differents backends.
;; - Icons associated to functions/variables/.. and their backends
;; - Display candidate's documentation (support quickhelp-string)
;; - Not limited by the current window size, buffer's text properties, ..
;;   (it's better than you might think)
;;
;; This package requires Emacs 26.
;; Also, not compatible with Emacs in a tty.
;;
;;
;; Installation:
;;
;; With use-package:
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))
;; Or:
;; (require 'company-box)
;; (add-hook 'company-mode-hook 'company-box-mode)
;;
;; To customize:
;; M-x customize-group [RET] company-box [RET]
;;
;;
;; For more informations, see the homepage:
;; https://github.com/sebastiencs/company-box

;;; Code:

(unless (require 'icons-in-terminal nil t)
  (defun icons-in-terminal (&rest _) " "))

(require 'dash)
(require 'dash-functional)
(require 'company)
(require 'company-box-icons)
(require 'company-box-doc)

(defgroup company-box nil
  "Front-end for Company."
  :prefix "company-box-"
  :group 'company)

(defface company-box-candidate
  '((((background light)) :foreground "black")
    (t :foreground "white"))
  "Face used to color candidates."
  :group 'company-box)

(defface company-box-annotation
  '((t :inherit company-tooltip-annotation))
  "Face used to color annotations."
  :group 'company-box)

(defface company-box-selection
  '((t :inherit company-tooltip-selection))
  "Face used to color the selected candidate."
  :group 'company-box)

(defface company-box-background
  '((t :inherit company-tooltip))
  "Face used for frame's background.
Only the 'background' color is used in this face."
  :group 'company-box)

(defface company-box-scrollbar
  '((t :inherit company-box-selection))
  "Face used for the scrollbar.
Only the 'background' color is used in this face."
  :group 'company-box)

(defcustom company-box-color-icon t
  "Whether or not to color icons.
Note that icons from images cannot be colored."
  :type 'boolean
  :group 'company-box)

(defcustom company-box-enable-icon t
  "Whether or not to display icons."
  :type 'boolean
  :group 'company-box)

(defcustom company-box-max-candidates 100
  "Maximum number of candidates.
A big number might slowndown the rendering.
To change the number of _visible_ chandidates, see `company-tooltip-limit'"
  :type 'integer
  :group 'company-box)

(defcustom company-box-show-single-candidate nil
  "Whether or not to display the candidate if there is only one."
  :type 'boolean
  :group 'company-box)

(defcustom company-box-icons-functions
  '(company-box-icons--yasnippet company-box-icons--lsp company-box-icons--elisp company-box-icons--acphp)
  "Functions to call on each candidate that should return an icon.
The functions takes 1 parameter, the completion candidate.

It should return an ICON or nil.
An ICON can be either a SYMBOL, an IMAGE, a LIST, a STRING:

- SYMBOL:  It is the name of the icon (from `icons-in-terminal').
- IMAGE:   An image descriptor [1]
           Example: '(image :type png :file \"/path/to/image.png\")
- LIST:    The list is then `apply' to `icons-in-terminal' function.
           Example: '(fa_icon :face some-face :foreground \"red\")
- STRING:  A simple string which is inserted, should be of length 1

If a function returns nil, it call the next function in the list.
If all functions returns nil, `company-box-icons-unknown' is used.

[1] https://www.gnu.org/software/emacs/manual/html_node/elisp/Image-Descriptors.html"
  :type 'list
  :group 'company-box)


(defvar company-box-backends-colors
  '((company-yasnippet . (:all "lime green" :selected (:background "lime green" :foreground "black"))))
  "List of colors to use for specific backends.

Each element has the form (BACKEND . COLOR)

BACKEND is the backend's symbol for which the color applies
COLOR can be a LIST or a STRING:

- LIST:    A property list with the following keys:
                `:candidate'  : Color to use on the candidate
                `:annotation' : Color to use on the annotation
                `:icon'       : Color to use on the icon. Does nothing if the
                                icon is an image.
                `:all'        : Replace (:candidate X :annotation X :icon X)
           For those 4 attributes, values can be a face, a plist
           or a string (a color)
                `:selected'   : Color to use when the candidate is selected.
           It can be a plist or a face, not a string.
           It needs to define the background and foreground colors

- STRING:  A color string which is used everywhere
           (similar to (:all \"red\"))

Examples:

'((company-yasnippet . (:candidate \"yellow\" :annotation some-face))
  (company-elisp . (:icon \"yellow\" :selected (:background \"orange\"
                                              :foreground \"black\")))
  (company-dabbrev . \"purple\"))")


(defvar company-box-frame-parameters
  '((left . -1)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width  . 0)
    (width  . 0)
    (min-height  . 0)
    (height  . 0)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    ;; (unsplittable . nil)
    (undecorated . t)
    (top . -1)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (drag-internal-border . t)
    (left-fringe . 0)
    (right-fringe . 0)
    (no-special-glyphs . t))
  "Frame parameters used to create the frame.")

(defvar-local company-box--ov nil)
(defvar-local company-box--max 0)
(defvar-local company-box--with-icons-p nil)
(defvar-local company-box--x nil)
(defvar-local company-box--space nil)
(defvar-local company-box--start nil)
(defvar-local company-box--height nil)
(defvar-local company-box--scrollbar-window nil)

(defvar company-box-selection-hook nil
  "Hook run when the selection changed.")

(defun company-box--get-frame ()
  "Return the child frame."
  (frame-parameter nil 'company-box-frame))

(defsubst company-box--set-frame (frame)
  "Set the frame parameter ‘company-box-frame’ to FRAME."
  (set-frame-parameter nil 'company-box-frame frame))

(defun company-box--get-buffer (&optional suffix)
  "Construct the buffer name, it should be unique for each frame."
  (get-buffer-create
   (concat " *company-box-"
           (or (frame-parameter nil 'window-id)
               (frame-parameter nil 'name))
           suffix
           "*")))

(defun company-box--with-icons-p nil
  (let ((spaces (+ (- (current-column) (string-width company-prefix))
                   (/ (or (car (nth 2 (posn-at-point (line-beginning-position)))) 0)
                      (frame-char-width))
                   (car (window-edges nil t)))))
    (setq company-box--space spaces)
    (and company-box-enable-icon
         (> spaces 1))))

(defun company-box--make-frame (&optional buf)
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (buffer (or buf (company-box--get-buffer)))
         (params (append company-box-frame-parameters
                         `((default-minibuffer-frame . ,(selected-frame))
                           (minibuffer . ,(minibuffer-window))
                           (background-color . ,(face-background 'company-box-background nil t)))))
         (window (display-buffer-in-child-frame buffer `((child-frame-parameters . ,params))))
         (frame (window-frame window)))
    (set-frame-parameter frame 'company-box-buffer buffer)
    (set-frame-parameter frame 'desktop-dont-save t)
    (unless buf
      (set-frame-parameter nil 'company-box-window window))
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (set-frame-parameter frame 'name "")
    frame))

(defun company-box--get-ov nil
  (or company-box--ov
      (setq company-box--ov (make-overlay 1 1))))

(defun company-box--extract-background (color)
  "COLOR can be a string, face or plist."
  `(:background
    ,(or (and (stringp color) color)
         (and (facep color) (face-background color nil t))
         (let ((back (plist-get color :background)))
           (if (facep back) (face-background back nil t) back)))))

(defun company-box--update-image (&optional color)
  "Change the image background color, because the overlay doesn't apply on it.
The function either restore the original image or apply the COLOR.
It doesn't nothing if a font icon is used."
  (-when-let* ((bol (line-beginning-position))
               (point (text-property-any bol (min (+ bol 2) (point-max)) 'company-box-image t))
               (image (get-text-property point 'display-origin))
               (new-image (append image (and color (company-box--extract-background color)))))
    (put-text-property point (1+ point) 'display new-image)))

(defun company-box--update-line (selection)
  (company-box--update-image)
  (goto-char 1)
  (forward-line selection)
  (move-overlay (company-box--get-ov)
                (line-beginning-position)
                (line-beginning-position 2))
  (let ((color (or (get-text-property (point) 'company-box--color)
                   'company-box-selection)))
    (overlay-put (company-box--get-ov) 'face color)
    (company-box--update-image color))
  (run-hook-with-args 'company-box-selection-hook selection
                      (or (frame-parent) (selected-frame))))

(defun company-box--render-buffer (string)
  (let ((selection company-selection))
    (with-current-buffer (company-box--get-buffer)
      (erase-buffer)
      (insert string "\n")
      (setq mode-line-format nil
            display-line-numbers nil
            truncate-lines t
            cursor-in-non-selected-windows nil)
      (setq-local scroll-step 1)
      (setq-local scroll-conservatively 10000)
      (setq-local scroll-margin  0)
      (setq-local scroll-preserve-screen-position t)
      (add-hook 'window-configuration-change-hook 'company-box--prevent-changes t t)
      (company-box--update-line selection))))

(defvar-local company-box--bottom nil)

(defun company-box--point-bottom nil
  (or company-box--bottom
      (setq company-box--bottom
            (let* ((win (let ((tmp nil))
                          (while (window-in-direction 'below tmp)
                            (setq tmp (window-in-direction 'below tmp)))
                          tmp)))
              (+ (or (nth 2 (or (window-line-height 'mode-line win)
                                (and (redisplay t) (window-line-height 'mode-line win))))
                     0)
                 (or (and win (nth 1 (window-edges win t nil t))) 0))))))

(defvar-local company-box--prefix-pos nil)
(defvar-local company-box--edges nil)

(defun company-box--prefix-pos nil
  (or company-box--prefix-pos
      (setq company-box--prefix-pos
            (nth 2 (posn-at-point (- (point) (length company-prefix)))))))

(defun company-box--edges nil
  (or company-box--edges
      (setq company-box--edges (window-edges nil t nil t))))

(defun company-box--set-frame-position (frame)
  (-let* (((left top _right _bottom) (company-box--edges))
          (char-height (frame-char-height frame))
          (char-width (frame-char-width frame))
          (height (* (min company-candidates-length company-tooltip-limit) char-height))
          (frame-resize-pixelwise t)
          (mode-line-y (company-box--point-bottom))
          ((p-x . p-y) (company-box--prefix-pos))
          (p-y-abs (+ top p-y))
          (y (or (and (> p-y-abs (/ mode-line-y 2))
                      (<= (- mode-line-y p-y) (+ char-height height))
                      (> (- p-y-abs height) 0)
                      (- p-y height))
                 (+ p-y char-height)))
          (height (or (and (> y p-y)
                           (> height (- mode-line-y y))
                           (- mode-line-y y))
                      height))
          (height (- height (mod height char-height)))
          (x (if company-box--with-icons-p
                 (- p-x (* char-width (if (= company-box--space 2) 2 3)))
               (- p-x (if (= company-box--space 0) 0 char-width)))))
    ;; Debug
    ;; (message "X+LEFT: %s P-X: %s X: %s LEFT: %s space: %s with-icon: %s LESS: %s"
    ;;          (+ x left) p-x x left company-box--space company-box--with-icons-p (+ (* char-width 3) (/ char-width 2)))
    (setq company-box--x (+ x left)
          company-box--start (or company-box--start (window-start))
          company-box--height height)
    (set-frame-size frame (company-box--update-width t (/ height char-height))
                    height t)
    (set-frame-position frame (max (+ x left) 0) (+ y top))
    (set-frame-parameter frame 'company-box-window-origin (selected-window))
    (set-frame-parameter frame 'company-box-buffer-origin (current-buffer))
    (with-selected-frame frame (set-fringe-style 0))))

(defun company-box--display (string)
  "Display the completions."
  (company-box--render-buffer string)
  (unless (company-box--get-frame)
    (company-box--set-frame (company-box--make-frame)))
  (company-box--set-frame-position (company-box--get-frame))
  (unless (frame-visible-p (company-box--get-frame))
    (make-frame-visible (company-box--get-frame)))
  (company-box--update-scrollbar (company-box--get-frame) t))

(defun company-box--get-kind (candidate)
  (let ((list company-box-icons-functions)
        kind)
    (while (and (null kind) list)
      (setq kind (funcall (car list) candidate))
      (pop list))
    (or kind 'Unknown)))

(defun company-box--get-icon (candidate)
  (let ((icon (alist-get (company-box--get-kind candidate)
                         (symbol-value company-box-icons-alist))))
    (cond ((listp icon)
           (cond ((eq 'image (car icon))
                  (propertize " " 'display icon 'company-box-image t
                              'display-origin icon))
                 ((and company-box-color-icon icon)
                  (apply 'icons-in-terminal icon))
                 (t (icons-in-terminal (or (car icon) 'fa_question_circle)))))
          ((symbolp icon)
           (icons-in-terminal (or icon 'fa_question_circle)))
          (t icon))))

(defun company-box--add-icon (candidate)
  (concat
   (company-box--get-icon candidate)
   (propertize " " 'display `(space :align-to (+ left-fringe ,(if (> company-box--space 2) 3 2))))))

(defun company-box--get-color (backend)
  (alist-get backend company-box-backends-colors))

(defun company-box--resolve-color (color key)
  (or (and (stringp color) color)
      (and (listp color) (or (plist-get color key) (plist-get color :all)))))

(defun company-box--resolve-colors (color)
  (when color
    (list
     (company-box--resolve-color color :candidate)
     (company-box--resolve-color color :annotation)
     (company-box--resolve-color color :icon)
     (let ((color (company-box--resolve-color color :selected)))
       (unless (stringp color)
         color)))))

(defun company-box--apply-color (string color)
  (when color
    (add-face-text-property 0 (length string)
                            (if (stringp color) (list :foreground color) color)
                            nil string))
  string)

(defun company-box--make-line (candidate)
  (-let* (((candidate annotation len-c len-a backend) candidate)
          (color (company-box--get-color backend))
          ((c-color a-color i-color s-color) (company-box--resolve-colors color))
          (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
          (candidate-string (propertize candidate 'face 'company-box-candidate))
          (align-string (when annotation
                          (concat " " (and company-tooltip-align-annotations
                                           (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
          (space company-box--space)
          (icon-p company-box-enable-icon)
          (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
          (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                          (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                        (company-box--apply-color icon-string i-color)
                        (company-box--apply-color candidate-string c-color)
                        align-string
                        (company-box--apply-color annotation-string a-color)))
          (len (length line)))
    (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                     'company-box--color s-color)
                         line)
    line))

(defun company-box--backend (candidate)
  (or (get-text-property 0 'company-backend candidate)
      (and (symbolp company-backend) company-backend)
      (--first (and it (not (keywordp it))) company-backend)))

(defun company-box--make-candidate (candidate)
  (let* ((annotation (-some->> (company-call-backend 'annotation candidate)
                               (replace-regexp-in-string "[ \t\n\r]+" " ")
                               (string-trim)))
         (len-candidate (string-width candidate))
         (len-annotation (if annotation (string-width annotation) 0))
         (len-total (+ len-candidate len-annotation))
         (backend (company-box--backend candidate)))
    (when (> len-total company-box--max)
      (setq company-box--max len-total))
    (list candidate
          annotation
          len-candidate
          len-annotation
          backend)))

(defun company-box-show nil
  (setq company-box--max 0
        company-box--with-icons-p (company-box--with-icons-p))
  (--> (-take company-box-max-candidates company-candidates)
       (mapcar (-compose 'company-box--make-line 'company-box--make-candidate) it)
       (mapconcat 'identity it "\n")
       (company-box--display it)))

(defvar company-box-hide-hook nil)

(defun company-box-hide nil
  (setq company-box--bottom nil
        company-box--start nil
        company-box--prefix-pos nil
        company-box--edges nil)
  (-some-> (company-box--get-frame)
           (make-frame-invisible))
  (run-hook-with-args 'company-box-hide-hook (or (frame-parent) (selected-frame))))

(defun company-box--calc-len (buffer start end char-width)
  (let ((max 0))
    (with-current-buffer buffer
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let ((len (or (get-text-property (point) 'company-box--len) 0)))
            (when (> len max)
              (setq max len)))
          (forward-line))))
    (* (+ max (if company-box--with-icons-p 6 2))
       char-width)))

(defun company-box--update-width (&optional no-update height)
  (unless no-update
    (redisplay))
  (-let* ((frame (company-box--get-frame))
          (window (frame-parameter nil 'company-box-window))
          (start (window-start window))
          (char-width (frame-char-width frame))
          (end (or (and height (with-current-buffer (window-buffer window)
                                 (save-excursion
                                   (goto-char start)
                                   (forward-line height)
                                   (point))))
                   (window-end window)))
          (max-width (- (frame-pixel-width) company-box--x char-width))
          (width (+ (company-box--calc-len (window-buffer window) start end char-width)
                    (if (company-box--scrollbar-p frame) (* 2 char-width) 0)
                    char-width))
          (width (max (min width max-width)
                      (* company-tooltip-minimum-width char-width)))
          (diff (abs (- (frame-pixel-width frame) width))))
    (or (and no-update width)
        (and (> diff 2) (set-frame-width frame width nil t)))))

(defun company-box--percent (a b)
  (/ (float a) b))

(defun company-box--scrollbar-p (frame)
  (/= 1 (company-box--percent
         company-box--height
         (* (min company-candidates-length company-box-max-candidates)
            (frame-char-height frame)))))

(defun company-box--update-scrollbar-buffer (height-blank height-scrollbar percent buffer)
  (with-current-buffer buffer
    (erase-buffer)
    (setq header-line-format nil
          mode-line-format nil
          cursor-in-non-selected-windows nil)
    (unless (zerop height-blank)
      (insert (propertize " " 'display `(space :align-to right-fringe :height ,height-blank))
              (propertize "\n" 'face '(:height 1))))
    (setq height-scrollbar (if (= percent 1)
                               ;; Due to float/int casting in the emacs code, there might 1 or 2
                               ;; remainings pixels
                               (+ height-scrollbar 10)
                             height-scrollbar))
    (insert (propertize " " 'face (list :background (face-background 'company-box-scrollbar nil t))
                        'display `(space :align-to right-fringe :height ,height-scrollbar)))
    (current-buffer)))

(defun company-box--update-scrollbar (frame &optional first)
  (let* ((selection company-selection)
         (buffer (company-box--get-buffer "-scrollbar"))
         (h-frame company-box--height)
         (n-elements (min company-candidates-length company-box-max-candidates))
         (percent (company-box--percent selection (1- n-elements)))
         (percent-display (company-box--percent h-frame (* n-elements (frame-char-height frame))))
         (scrollbar-pixels (* h-frame percent-display))
         (height-scrollbar (/ scrollbar-pixels (frame-char-height frame)))
         (blank-pixels (* (- h-frame scrollbar-pixels) percent))
         (height-blank (/ blank-pixels (frame-char-height frame))))
    (cond
     ((and first (= percent-display 1) (window-live-p company-box--scrollbar-window))
      (delete-window company-box--scrollbar-window))
     ((window-live-p company-box--scrollbar-window)
      (company-box--update-scrollbar-buffer height-blank height-scrollbar percent buffer))
     ((/= percent-display 1)
      (setq
       company-box--scrollbar-window
       (with-selected-frame (company-box--get-frame)
         (display-buffer-in-side-window
          (company-box--update-scrollbar-buffer height-blank height-scrollbar percent buffer)
          '((side . right) (window-width . 2)))))
      (set-frame-parameter frame 'company-box-scrollbar (window-buffer company-box--scrollbar-window))
      (window-preserve-size company-box--scrollbar-window t t)))))

;; ;; (message "selection: %s len: %s PERCENT: %s PERCENTS-DISPLAY: %s SIZE-FRAME: %s HEIGHT-S: %s HEIGHT-B: %s h-frame: %s sum: %s"
;; ;;          selection n-elements percent percent-display height height-scrollbar height-blank height (+ height-scrollbar height-blank))
;; ;; (message "HEIGHT-S-1: %s HEIGHT-B-1: %s sum: %s" scrollbar-pixels blank-pixels (+ height-scrollbar-1 height-blank-1))

(defun company-box--change-line nil
  (let ((selection company-selection))
    (with-selected-window (get-buffer-window (company-box--get-buffer) t)
      (company-box--update-line selection))
    (company-box--update-scrollbar (company-box--get-frame))))

(defun company-box--next-line nil
  (interactive)
  (when (< (1+ company-selection) (min company-candidates-length
                                       company-box-max-candidates))
    (setq company-selection (1+ company-selection))
    (company-box--change-line)
    (company-box--update-width)))

(defun company-box--prev-line nil
  (interactive)
  (setq company-selection (max (1- company-selection) 0))
  (company-box--change-line)
  (company-box--update-width))

(defun company-box--start-changed-p nil
  (not (equal company-box--start (window-start))))

(defun company-box--post-command nil
  (cond ((company-box--start-changed-p)
         (company-box--on-start-change))))

(defun company-box--prevent-changes (&rest _)
  (set-window-margins nil 0 0))

(defun company-box--handle-window-changes (&optional on-idle)
  (-when-let* ((frame (company-box--get-frame)))
    (and (frame-live-p frame)
         (frame-visible-p frame)
         (or (not (eq (selected-window) (frame-parameter frame 'company-box-window-origin)))
             (not (eq (current-buffer) (frame-parameter frame 'company-box-buffer-origin))))
         (if on-idle (company-box-hide)
           ;; Handle when this function (in `buffer-list-update-hook') has been
           ;; triggered by a function that select only temporary another window/buffer.
           ;; So we are sure to not be in a false positive case.
           ;; See the docstring of `select-window'
           (run-with-idle-timer 0 nil (lambda nil (company-box--handle-window-changes t)))))))

(defun company-box-frontend (command)
  "`company-mode' frontend using child-frame.
COMMAND: See `company-frontends'."
  ;; (message "\nCOMMMAND: %s" command)
  ;; (message "prefix: %s" company-prefix)
  ;; (message "candidates: %s" company-candidates)
  ;; (message "common: %s" company-common)
  ;; (message "selection: %s" company-selection)
  ;; (message "point: %s" company-point)
  ;; (message "search-string: %s" company-search-string)
  ;;(message "last-command: %s" last-command)
  (cond
   ((eq command 'hide)
    (company-box-hide))
   ((and (equal company-candidates-length 1) (null company-box-show-single-candidate))
    (company-box-hide))
   ((eq command 'update)
    (company-box-show))
   ((eq command 'post-command)
    (company-box--post-command))))

(defun company-box--on-start-change nil
  (setq company-box--prefix-pos nil
        company-box--start nil
        company-box--edges nil)
  (company-box--set-frame-position (company-box--get-frame))
  (company-box--update-scrollbar (company-box--get-frame) t))

(defun company-box--kill-delay (buffer)
  (run-with-idle-timer
   0 nil (lambda nil
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))))

(defun company-box--kill-buffer (frame)
  (company-box--kill-delay (frame-parameter frame 'company-box-buffer))
  (company-box--kill-delay (frame-parameter frame 'company-box-scrollbar)))

(defvar company-box-mode-map nil
  "Keymap when `company-box' is active")

(unless company-box-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap company-select-next] 'company-box--next-line)
    (define-key map [remap company-select-next-or-abort] 'company-box--next-line)
    (define-key map [remap company-select-previous-or-abort] 'company-box--prev-line)
    (define-key map [remap company-select-previous] 'company-box--prev-line)
    (setq company-box-mode-map map)))

(defun company-box--set-mode (&optional frame)
  (cond
   ((and (bound-and-true-p company-box-mode) (not (display-graphic-p frame)))
    (company-box-mode -1))
   ((bound-and-true-p company-box-mode)
    (remove-hook 'after-make-frame-functions 'company-box--set-mode t)
    (add-hook 'delete-frame-functions 'company-box--kill-buffer)
    (add-hook 'buffer-list-update-hook 'company-box--handle-window-changes t)
    (make-local-variable 'company-frontends)
    (setq company-frontends (->> (delq 'company-pseudo-tooltip-frontend company-frontends)
                                 (delq 'company-pseudo-tooltip-unless-just-one-frontend)))
    (add-to-list 'company-frontends 'company-box-frontend)
    (unless (assq 'company-box-frame frameset-filter-alist)
      (push '(company-box-doc-frame . :never) frameset-filter-alist)
      (push '(company-box-frame . :never) frameset-filter-alist)))
   ((memq 'company-box-frontend company-frontends)
    (setq company-frontends (delq 'company-box-frontend  company-frontends))
    (add-to-list 'company-frontends 'company-pseudo-tooltip-unless-just-one-frontend))))

(add-hook 'company-box-selection-hook 'company-box-doc)
(add-hook 'company-box-hide-hook 'company-box-doc--hide)

;;;###autoload
(define-minor-mode company-box-mode
  "Company-box minor mode."
  :group 'company-box
  :lighter " company-box"
  ;; With emacs daemon and:
  ;; `(add-hook 'company-mode-hook 'company-box-mode)'
  ;; `company-box-mode' is called too early to know if we are in a GUI
  (if (and (daemonp)
           (not (frame-parameter nil 'client))
           company-box-mode)
      (add-hook 'after-make-frame-functions 'company-box--set-mode t t)
    (company-box--set-mode)))

(provide 'company-box)
;;; company-box.el ends here
