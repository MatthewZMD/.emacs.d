;;; term-keys-linux.el --- term-keys support for the Linux console

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
;; configuration of the Linux console to interoperate with the
;; term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)


(defgroup term-keys/linux nil
  "`term-keys' options for the Linux console."
  :group 'term-keys)


(define-widget 'term-keys/linux-modifier 'lazy
  "Choice for Linux modifiers for keymap files."
  :type '(choice (const "Shift")
		 (const "AltGr")
		 (const "Control")
		 (const "Alt")
		 (const "ShiftL")
		 (const "ShiftR")
		 (const "CtrlL")
		 (const "CtrlR")
		 (const "CapsShift")
		 (const :tag "(none)" nil)))


(defcustom term-keys/linux-modifier-map ["Shift" "Control" "Alt" nil nil "AltGr"]
  "Modifier keys for Linux console keymaps.

This should be a vector of 6 elements, with each element being a
string indicating the name of the modifier key corresponding to
the Emacs modifiers Shift, Control, Meta, Super, Hyper and Alt
respectively, as they should appear in generated .keymap files.
nil indicates that there is no mapping for this modifier."
  :type '(vector
	  (term-keys/linux-modifier :tag "Shift")
	  (term-keys/linux-modifier :tag "Control")
	  (term-keys/linux-modifier :tag "Meta")
	  (term-keys/linux-modifier :tag "Super")
	  (term-keys/linux-modifier :tag "Hyper")
	  (term-keys/linux-modifier :tag "Alt"))
  :group 'term-keys/linux)


(defcustom term-keys/linux-first-function-key 13
  "First Linux console keymap function key to use for term-keys bindings.

The Linux console allows binding custom character sequences to
keys only by assigning them to a \"function key\" (named thus
after the usual F1, F2 etc. function keys found on a computer
keyboard).  Although most PC keyboards today only have 12
function keys, some keyboards/computers had more, and (also for
the purposes of key binding) the Linux kernel allows registering
character sequences for as many as 246 function keys (see output
for 'dumpkeys -l' for the exact figure on your machine).
Crucially, these virtual keys provide the means to configure
arbitrary character sequences for any key combination, which is
necessary for term-keys to work.

This variable specifies the first function key entry that's used
by term-keys in the .keymap files it generates (see
`term-keys/linux-keymap').  The default value (13) will make
`term-keys' use F13 and onwards, and is fine for most uses,
unless you happen to use a keyboard with more than 12 F-keys, or
wish to use the F13-F20 keys through the shifted state of the
F1-F8 keys."
  :type 'integer
  :group 'term-keys)


(defun term-keys/linux-keymap ()
  "Construct Linux console configuration in the form of a keymap.

This function returns, as a string, a keymap which can be used to
configure the Linux kernel's console to encode term-keys key
sequences, according to the term-keys configuration.

The returned string is suitable to be saved as-is in a .keymap
file and loaded by the loadkeys program."
  (apply #'concat
	 (let ((fkey term-keys/linux-first-function-key))
	   (term-keys/iterate-keys
	    (lambda (index keymap mods)

	      ;; Skip key combinations with unrepresentable modifiers
	      (unless (cl-reduce (lambda (x y) (or x y)) ; any
				 (mapcar (lambda (n) ; active modifier mapped to nil
					   (and (elt mods n)
						(not (elt term-keys/linux-modifier-map n))))
					 (number-sequence 0 (1- (length mods))))) ; 0..5
		(prog1
		    (format "# %s\n%s\tkeycode %3d = F%d\nstring F%d = \"%s\"\n\n"
			    ;; Emacs key name for comment
			    (term-keys/format-key (elt keymap 0) mods)

			    (if (cl-reduce (lambda (x y) (or x y)) mods)
				;; tab-separated mod list
				(mapconcat
				 (lambda (n)
				   (if (elt mods n) (elt term-keys/linux-modifier-map n) ""))
				 (number-sequence 0 (1- (length mods)))
				 "\t")
			      ;; "plain" if no mods
			      (concat "plain" (make-string (1- (length mods)) ?\t)))
			    (elt keymap 2) ; keynumber
			    fkey        ; F-key number (use)
			    fkey        ; F-key number (declaration)
			    (mapconcat  ; octal-escaped sequence
			     (lambda (x) (format "\\%03o" x))
			     (append
			      term-keys/prefix
			      (term-keys/encode-key index mods)
			      term-keys/suffix
			      nil)
			     ""))
		  (setq fkey (1+ fkey)))))))))


(provide 'term-keys-linux)
;;; term-keys-linux.el ends here
