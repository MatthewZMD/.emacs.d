;;; term-keys-xterm.el --- term-keys support for xterm

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
;; configuration of the xterm terminal emulator to interoperate with
;; the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)


(defconst term-keys/xterm-modifier-names ["Shift" "Ctrl" "Meta" "Super" "Hyper" "Alt"]
  "Modifier key names in xterm key translation syntax.")


(defun term-keys/xterm-format-key (key mods)
  "Format key modifiers in xterm key translation syntax.

Returns the xterm translation string corresponding to the KEY and
modifier state MODS."
  (concat
   (cl-loop for modflag across mods
	    for index from 0
	    concat
	    (concat
	     (if modflag " " "~")
	     (elt term-keys/xterm-modifier-names index)
	     " "))
   "<Key> "
   key))


(defun term-keys/xterm-translations ()
  "Construct xterm configuration in the form of translation entries.

This function returns, as a list of strings (one string per
line), the xterm translation entries necessary to configure xterm
to encode term-keys key sequences, according to the term-keys
configuration."
  (term-keys/iterate-keys
   (lambda (index keymap mods)
     (format "%-55s: %s"
	     (term-keys/xterm-format-key (elt keymap 1) mods)
	     (mapconcat
	      (lambda (c) (format "string(0x%02x)" c))
	      (append
	       term-keys/prefix
	       (term-keys/encode-key index mods)
	       term-keys/suffix
	       nil)
	      " ")))))


(defun term-keys/xterm-xresources ()
  "Construct xterm configuration in the form of .Xresources entries.

This function returns, as a string, the .Xresources entries
necessary to configure xterm to encode term-keys key sequences,
according to the term-keys configuration.

The returned string is suitable to be added as-is to an
~/.Xresources file."
  (concat
   "\n*VT100.Translations: #override \\\n"
   (mapconcat #'identity
	      (term-keys/xterm-translations)
	      " \\n\\\n")
   "\n\n"))


(defun term-keys/xterm-args ()
  "Construct xterm configuration in the form of command line arguments.

This function returns a list of xterm command line arguments
necessary to configure the terminal emulator to encode key
sequences, according to the term-keys configuration."
  (list
   "-xrm"
   (mapconcat #'identity
	      (cons
	       "XTerm.VT100.translations: #override"
	       (term-keys/xterm-translations))
	      "\\n")))


(defun term-keys/xterm-script ()
  "Construct xterm configuration in the form of a shell script.

This function returns, as a string, a shell script which launches
xterm configured to encode term-keys key sequences, according to
the term-keys configuration.

The returned string is suitable to be saved as-is in an
executable file and used for launching xterm."
  (concat
   "#!/bin/sh\n"
   "exec xterm \\\n\t"
   (mapconcat #'shell-quote-argument (term-keys/xterm-args) " \\\n\t")
   " \\\n\t\"$@\"\n"))


(defun term-keys/xterm-run-emacs ()
  "Launch Emacs via xterm enhanced with term-keys.

This function is used for testing and as an example."
  (apply #'start-process "xterm" nil "xterm"
	 (append
	  (term-keys/xterm-args)
	  (list
	   "-xrm" "XTerm*eightBitInput: false"
	    "-e" (car command-line-args) "-nw"
	    "--load" term-keys/main-file-name
	    "--funcall" "term-keys/init"
	    ))))


(provide 'term-keys-xterm)
;;; term-keys-xterm.el ends here
