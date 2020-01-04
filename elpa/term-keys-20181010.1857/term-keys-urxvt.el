;;; term-keys-urxvt.el --- term-keys support for urxvt

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
;; configuration of the urxvt terminal emulator to interoperate with
;; the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)


(defun term-keys/urxvt-format-key (key mods)
  "Format key modifiers in urxvt syntax.

Returns KEY prepended with S-, C-, M-, s-, H-, or A- depending on
the elements of the bool vector MODS are correspondingly non-nil,
additionally upcasing letter keys."
  (if (and (elt mods 0)                 ; Shift
	   (string-match-p "^[a-z]$" key))
      ;; Upcase letter keys
      (term-keys/format-key (upcase key) (bool-vector nil
						      (elt mods 1)
						      (elt mods 2)
						      (elt mods 3)
						      (elt mods 4)
						      (elt mods 5)))
    (term-keys/format-key key mods)))


(defun term-keys/urxvt-args ()
  "Construct urxvt configuration in the form of command line arguments.

This function returns a list of urxvt (rxvt-unicode) command line
arguments necessary to configure the terminal emulator to encode
key sequences, according to the term-keys configuration."
  (apply #'nconc
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)
	    (list
	     (concat
	      "-keysym."
	      (term-keys/urxvt-format-key (elt keymap 1) mods))
	     (concat
	      "string:"
	      term-keys/prefix
	      (term-keys/encode-key index mods)
	      term-keys/suffix))))))


(defun term-keys/urxvt-script ()
  "Construct urxvt configuration in the form of a shell script.

This function returns, as a string, a shell script which launches
urxvt (rxvt-unicode) configured to encode term-keys key
sequences, according to the term-keys configuration.

The returned string is suitable to be saved as-is in an
executable file and used for launching urxvt."
  (concat
   "#!/bin/sh\n"
   "exec urxvt \\\n\t"
   (mapconcat #'shell-quote-argument (term-keys/urxvt-args) " \\\n\t")
   " \\\n\t\"$@\"\n"))


(defun term-keys/urxvt-xresources ()
  "Construct urxvt configuration in the form of .Xresources entries.

This function returns, as a string, the .Xresources entries
necessary to configure urxvt to encode term-keys key
sequences, according to the term-keys configuration.

The returned string is suitable to be added as-is to an
~/.Xresources file."
  (apply #'concat
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)
	    (format "URxvt.keysym.%s: string:%s%s%s\n"
		    (term-keys/urxvt-format-key (elt keymap 1) mods)
		    term-keys/prefix
		    (term-keys/encode-key index mods)
		    term-keys/suffix)))))


(defun term-keys/urxvt-run-emacs ()
  "Launch Emacs via urxvt enhanced with term-keys.

This function is used for testing and as an example."
  (apply #'start-process "urxvt" nil "urxvt"
	 (append
	  (term-keys/urxvt-args)
	  (list
	    "-e" (car command-line-args) "-nw"
	    "--load" term-keys/main-file-name
	    "--funcall" "term-keys/init"
	    ))))


(provide 'term-keys-urxvt)
;;; term-keys-urxvt.el ends here
