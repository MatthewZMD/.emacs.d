;;; term-keys-st.el --- term-keys support for the st terminal emulator

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains supplementary code for aiding in the
;; configuration of the st terminal emulator to interoperate with the
;; term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)


(defgroup term-keys/st nil
  "`term-keys' options for the st terminal emulator."
  :group 'term-keys)


(define-widget 'term-keys/x11-modifier 'lazy
  "Choice for X11 key modifier state flags."
  :type '(choice (const "Shift")
		 (const "Lock")
		 (const "Control")
		 (const "Mod1" :tag "Mod1 (usually Alt)")
		 (const "Mod2" :tag "Mod2 (usually Num Lock)")
		 (const "Mod3")
		 (const "Mod4" :tag "Mod4 (usually the Windows Logo key)")
		 (const "Mod5")
		 (const :tag "(none)" nil)))


(defcustom term-keys/x11-modifier-map ["Shift" "Control" "Mod1" "Mod4" "Mod3" "Mod5"]
  "Map of X11 modifier state flags to Emacs modifiers.

This should be a vector of 6 elements, with each element being a
string indicating the name of the X11 modifier state mask (sans
the -\"Mask\" suffix) corresponding to the Emacs modifiers Shift,
Control, Meta, Super, Hyper and Alt respectively, as they should
appear in the generated st configuration.  nil indicates that
there is no mapping for this modifier."
  :type '(vector
	  (term-keys/x11-modifier :tag "Shift")
	  (term-keys/x11-modifier :tag "Control")
	  (term-keys/x11-modifier :tag "Meta")
	  (term-keys/x11-modifier :tag "Super")
	  (term-keys/x11-modifier :tag "Hyper")
	  (term-keys/x11-modifier :tag "Alt"))
  :group 'term-keys/st)


(defun term-keys/x11-apply-mod-state (key shift lock control mod1 mod2 mod3 mod4 mod5)
  "Apply modifier state flags to an X11 KeySym.

Given a KeySym KEY which would be received by an application with
no modifier flags, return the KeySym that would be received by
the application if SHIFT, LOCK, CONTROL, MOD1, MOD2, MOD3, MOD4
and MOD5 modifier flags are respectively active."
  (cond
   ((and (string-match-p "^[a-z]$" key) shift)
    (upcase key))
   ((string-equal key "Tab")
    "ISO_Left_Tab")
   (t
    key)))


(defun term-keys/x11-apply-mods (key mods)
  "Apply Emacs modifiers to X11 KeySym KEY.

Translate Emacs modifiers MODS to X11 modifiers (according to
`term-keys/x11-modifier-map') and invoke
`term-keys/x11-apply-mod-state')."
  (let (shift lock control mod1 mod2 mod3 mod4 mod5)
    (mapc
     (lambda (n)
       (when (elt mods n)
	 (pcase (downcase (elt term-keys/x11-modifier-map n))
	   ("shift"
	    (setq shift t))
 	   ("lock"
	    (setq lock t))
 	   ("control"
	    (setq control t))
 	   ("mod1"
	    (setq mod1 t))
 	   ("mod2"
	    (setq mod2 t))
 	   ("mod3"
	    (setq mod3 t))
 	   ("mod4"
	    (setq mod4 t))
 	   ("mod5"
	    (setq mod5 t))
	   ('nil)
	   (_ (error "Unknown modifier: %s" (elt term-keys/x11-modifier-map n))))))
     (number-sequence 0 (1- (length mods))))
    (term-keys/x11-apply-mod-state key shift lock control mod1 mod2 mod3 mod4 mod5)))


(defun term-keys/st-config-key ()
  "Construct st key binding configuration (key array entries).

This function returns, as a string, C code of Key entries of the
config.h 'key' array, which can be used to configure st to encode
term-keys key sequences, according to the term-keys
configuration.

The returned string is suitable to be pasted as-is into the 'key'
array in the st config.h configuration file; however, this is
just one half of the necessary configuration (see
`term-keys/st-config-mappedkeys' for the other half)."
  (apply #'concat
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)

	    ;; Skip key combinations with unrepresentable modifiers
	    (unless (cl-reduce (lambda (x y) (or x y)) ; any
			       (mapcar (lambda (n) ; active modifier mapped to nil
					 (and (elt mods n)
					      (not (elt term-keys/x11-modifier-map n))))
				       (number-sequence 0 (1- (length mods))))) ; 0..5
	      (format "{ XK_%-16s, %-40s, \"%s\", 0, 0, 0},\n"
		      (term-keys/x11-apply-mods (elt keymap 1) mods) ; X11 key name
		      (if (cl-reduce (lambda (x y) (or x y)) mods)
			  (mapconcat
			   (lambda (n)
			     (concat
			      (elt term-keys/x11-modifier-map n)
			      "Mask"))
			   (cl-remove-if-not (lambda (n) (elt mods n))
					     (number-sequence 0 (1- (length mods))))
			   "|")
			"XK_NO_MOD")
		      (mapconcat  ; hex-escaped sequence
		       (lambda (x) (format "\\x%02X" x))
		       (append
			term-keys/prefix
			(term-keys/encode-key index mods)
			term-keys/suffix
			nil)
		       "")))))))


(defun term-keys/st-config-mappedkeys ()
  "Construct st key binding configuration (mappedkeys array entries).

This function returns, as a string, C code of KeySym entries of
the config.h 'mappedkeys' array, which can be used to configure
st to encode term-keys key sequences, according to the term-keys
configuration.

The returned string is suitable to be pasted as-is into the
'mappedkeys' array in the st config.h configuration file;
however, this is just one half of the necessary
configuration (see `term-keys/st-config-key' for the other
half)."
  (apply #'concat
	 (delete-dups
	  (term-keys/iterate-keys
	   (lambda (index keymap mods)
	     (format "XK_%s,\n"
		     (term-keys/x11-apply-mods (elt keymap 1) mods)))))))


(provide 'term-keys-st)
;;; term-keys-st.el ends here
