;;; term-keys-terminal-app.el --- term-keys support for Terminal.app

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
;; configuration of the macOS Terminal.app terminal emulator to
;; interoperate with the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)
(require 'xml)


(defgroup term-keys/terminal-app nil
  "`term-keys' options for the macOS Terminal.app terminal emulator."
  :group 'term-keys)


(define-widget 'term-keys/terminal-app-modifier 'lazy
  "Choice for Terminal.app key binding modifiers."
  :type '(choice
	  (const :tag "⇧ (Shift)" "$")
	  (const :tag "⌃ (Ctrl)" "^")
	  (const :tag "⌥ (Option / Alt)" "~")
	  ;; (const :tag "⌘ (Command / Windows Logo key)" "")
	  (const :tag "# (Numpad)" "#")
	  (const :tag "(none)" nil)))


(defcustom term-keys/terminal-app-modifier-map ["$" "^" "~" nil nil nil]
  "Modifier keys for Terminal.app key bindings.

This should be a vector of 6 elements, with each element being a
string indicating the Terminal.app modifier corresponding to the
Emacs modifiers Shift, Control, Meta, Super, Hyper and Alt
respectively, as they should appear in generated Terminal.app
configuration.  nil indicates that there is no mapping for this
modifier."
  :type '(vector
	  (term-keys/terminal-app-modifier :tag "Shift")
	  (term-keys/terminal-app-modifier :tag "Control")
	  (term-keys/terminal-app-modifier :tag "Meta")
	  (term-keys/terminal-app-modifier :tag "Super")
	  (term-keys/terminal-app-modifier :tag "Hyper")
	  (term-keys/terminal-app-modifier :tag "Alt"))
  :group 'term-keys/terminal-app)


(defun term-keys/terminal-app-keymap-xml ()
  "Construct Terminal.app key binding configuration.

This function returns, as a string, a Terminal.app keymap as a
property list fragment in XML format, which can be used to
configure the terminal emulator to encode term-keys key sequences,
according to the term-keys configuration.

The returned string is suitable to be injected into the
terminal's configuration, e.g. using the command:

plutil -replace 'Window Settings.Basic.keyMapBoundKeys' \
  -xml \"$(cat keymap.xml)\" \
  ~/Library/Preferences/com.apple.Terminal.plist"

  (with-temp-buffer
    (xml-print
     `((plist ((version . "1.0"))
	      (dict nil .
		    ,(apply #'nconc
			    (term-keys/iterate-keys
			     (lambda (index keymap mods)

			       (unless (and
					;; Skip key combinations with unrepresentable modifiers
					(cl-reduce (lambda (x y) (or x y)) ; any
						   (mapcar (lambda (n) ; active modifier mapped to nil
							     (and (elt mods n)
								  (not (elt term-keys/terminal-app-modifier-map n))))
							   (number-sequence 0 (1- (length mods))))) ; 0..5
					;; Skip keys without a macOS Unicode key code
					(elt keymap 4))

				 `((key nil ,(concat
					      (mapconcat
					       (lambda (n)
						 (when (elt mods n)
						   (elt term-keys/terminal-app-modifier-map n)))
					       (number-sequence 0 (1- (length mods))) ; 0..5
					       "")
					      (format "%04X" (elt keymap 4)))) ; key code
				   (string nil ,(concat
						 term-keys/prefix
						 (term-keys/encode-key index mods)
						 term-keys/suffix
						 nil)))))))))))
    (buffer-string)))


(provide 'term-keys-terminal-app)
;;; term-keys-terminal-app.el ends here
