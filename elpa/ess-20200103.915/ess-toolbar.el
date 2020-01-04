;;; ess-toolbar.el --- Support for a toolbar in ESS.  -*- lexical-binding: t; -*-

;; Copyright (C) 1997--2009 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Stephen Eglen
;; Created: 2004-05-06
;; Revised: 2009-03-16
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/

;;; Commentary:

;; This code adds a toolbar to ESS modes for editing R and S code.
;; Support can be added for other modes (e.g. STATA), just ask!
;;
;; This code is experimental. It has been tested only on Linux
;; machines.  All feedback appreciated.
;;
;; If your Emacs can support images, the ESS toolbar should be loaded.
;;
;; If you see a toolbar, but no icons, check out the value of
;; ess-icon-directory.
;;
;; The toolbar can be customized in several ways.  To see options, do:
;; M-x customize-group RET ess-toolbar RET
;; If you change any of the variables, you _may_ need to restart Emacs
;; to see any effect.  See also the documentation for ess-toolbar-items
;; if you wish to change its value.

;;; Technical issues.

;; 2009-03-16: toolbar code in Emacs 23 has changed slightly to 22,
;; and presumably once Emacs 22 is no longer supported, this code can
;; be cleaned up a bit (i.e. no need to set load-path.)

;;; Code:

(require 'ess-mode)

(defgroup ess-toolbar nil
  "ESS: toolbar support."
  :group 'ess
  :link '(emacs-commentary-link :tag "Commentary" "ess-toolbar.el")
  :prefix "ess-")

(defcustom ess-use-toolbar
  (and (fboundp 'display-images-p) (display-images-p))
  "Non-nil means ESS should support the toolbar."
  :type 'boolean)


(defcustom ess-toolbar-own-icons nil
  "Non-nil means that we only put our toolbar entries in ESS.
Otherwise we get standard toolbar as well as ESS entries.
The standard toolbar items are copied from the default toolbar."
  :type 'boolean)

(defcustom ess-toolbar-global nil
  "*Non-nil means that the ESS toolbar is available in all Emacs buffers.
Otherwise, the ESS toolbar is present only in R/S mode buffers.
For beginners, this is probably better set to a non-nil value."
  :type 'boolean)

(defcustom ess-toolbar-items
  '( (R   "startr")
     ;;(S   "spluslogo" "Start S process")
     (S   "splus_letter_small")
     (ess-eval-line-and-step   "rline")
     (ess-eval-region   "rregion")
     (ess-eval-function-or-paragraph-and-step "rregion")
     (ess-load-file   "rbuffer")
     (ess-eval-function   "rfunction")
     (ess-switch-to-ESS   "switch_ess"))
  "Items to be added to the ESS toolbar.
Each list element has two items:
1. the name of the function to run
2. the icon to be used (without .xpm extension)

General toolbar items are also added to the ESS toolbar
iff `ess-toolbar-own-icons' is nil.

Setting this variable with setq doesn't take effect once you have
loaded ess-site, unless you follow it by a call to
`ess-make-toolbar' afterwards.  Instead, change its value using
Custom, and then on all new ESS buffers you should see the
toolbar has changed."
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (fboundp 'ess-make-toolbar)
             (ess-make-toolbar)))
  :type '(repeat (list (function :tag "Function to run")
                       (string  :tag "Icon"))))

(defvar ess-icon-directory
  (expand-file-name "icons" ess-etc-directory)
  "*Location for ESS icons.
This variable should be set automatically by the ESS install process.
Icons should be found in ESS/etc/icons/ directory.
If `ess-icon-directory' is invalid, please report a bug.")

(unless (file-directory-p ess-icon-directory)
  (ess-write-to-dribble-buffer
   "`ess-icon-directory' does not exist; using `ess-etc-directory'.\n")
  (setq ess-icon-directory ess-etc-directory))

(defvar ess-toolbar nil
  "Toolbar items to be added to ESS editing buffers.")

(defun ess-make-toolbar ()
  "Make the ESS toolbar."
  ;; Under Emacs, only worth building the toolbar if tool-bar-map is
  ;; available.  e.g. when running Emacs within a terminal, tool-bar-map
  ;; is not available, so no need to make the tool-bar.
  (when (boundp 'tool-bar-map)
    (setq ess-toolbar
          (if (or ess-toolbar-own-icons (null tool-bar-map))
              (make-sparse-keymap)
            (copy-keymap tool-bar-map)))
    (let ((tool-bar-map ess-toolbar)
          (load-path (list ess-icon-directory)))
      ;; in Emacs 22, icons are found by examining load-path, bound here
      ;; whereas Emacs 23 seems to want them in image-load-path, set at the
      ;; bottom of this file.
      (mapc #'ess-add-icon ess-toolbar-items))))

(defun ess-add-icon (x)
  "Add an ESS item to the Emacs toolbar.
X should be a list, see `ess-toolbar-items' for the format."
  ;; By using tool-bar-add-item-from-menu instead of tool-bar-add-item
  ;; we get the tooltips "for free" from ess-mode-map.
  (tool-bar-add-item-from-menu (car x) (cadr x) ess-mode-map))

(defun ess-add-toolbar ()
  "Add the ESS toolbar to a particular mode.
The toolbar is added iff `ess-toolbar-global' is nil, else the toolbar
is added globally when ess-toolbar.el is loaded."
  (when (and ess-toolbar (not ess-toolbar-global))
    (setq-local tool-bar-map ess-toolbar)))

;; Make the toolbars.  Each toolbar is hopefully made only when this file
;; is loaded; we don't need it to be remade every time.
(if ess-use-toolbar
    (progn
      (ess-make-toolbar)
      ;; After making the toolbar, if ESS toolbar is needed globally,
      ;; add it here.
      (if ess-toolbar-global
          (setq tool-bar-map ess-toolbar)
        (ess-write-to-dribble-buffer "Creating global Emacs toolbar"))

      ;; Check for toolbar support - needed iff ess-use-toolbar is non-nil.
      (or
       ;; Emacs support for images:
       (and (fboundp 'display-images-p) (display-images-p))
       ;; if above tests failed, give a warning.
       (progn
         (message "Toolbar support for ESS not available in this Emacs.")
         ;; Not sure if we want to delay startup of ESS.
         ;;(sit-for 2)
         ))
      ))

;; Following needed for Emacs 23, not Emacs 22
(when (boundp 'image-load-path)
  (add-to-list 'image-load-path ess-icon-directory))

(provide 'ess-toolbar)

;;; ess-toolbar.el ends here
