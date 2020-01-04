;;; term-keys.el --- Lossless keyboard input in a terminal emulator

;; Version: 0.1.0
;; Author: Vladimir Panteleev
;; Url: https://github.com/CyberShadow/term-keys
;; Keywords: terminals
;; Package-Requires: ((emacs "24.3"))

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

;; This package allows lossless keyboard input when using Emacs from a
;; terminal emulator.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'cl-lib)


(defgroup term-keys nil
  "Lossless keyboard input in a terminal emulator.

term-keys allows lossless keyboard input when using Emacs from a
terminal emulator.

For more information, please see the accompanying README.md
file."
  :group 'convenience)


(defcustom term-keys/mapping
  '(["<escape>"		"Escape"	1	"Esc"		#x001B	]
    ["<f1>"		"F1"		59	"F1"		#xF704	]
    ["<f2>"		"F2"		60	"F2"		#xF705	]
    ["<f3>"		"F3"		61	"F3"		#xF706	]
    ["<f4>"		"F4"		62	"F4"		#xF707	]
    ["<f5>"		"F5"		63	"F5"		#xF708	]
    ["<f6>"		"F6"		64	"F6"		#xF709	]
    ["<f7>"		"F7"		65	"F7"		#xF70A	]
    ["<f8>"		"F8"		66	"F8"		#xF70B	]
    ["<f9>"		"F9"		67	"F9"		#xF70C	]
    ["<f10>"		"F10"		68	"F10"		#xF70D	]
    ["<f11>"		"F11"		87	"F11"		#xF70E	]
    ["<f12>"		"F12"		88	"F12"		#xF70F	]
    ["<print>"		"Print"		99	"Print"		#xF710	]
    ["<Scroll_Lock>"	"Scroll_Lock"	70	"ScrollLock"	nil	]
    ["<pause>"		"Pause"		119	"Pause"		nil	]

    ["`"		"grave"		43	"`"		?`	]
    ["1"		"1"		2	"1"		?1	]
    ["2"		"2"		3	"2"		?2	]
    ["3"		"3"		4	"3"		?3	]
    ["4"		"4"		5	"4"		?4	]
    ["5"		"5"		6	"5"		?5	]
    ["6"		"6"		7	"6"		?6	]
    ["7"		"7"		8	"7"		?7	]
    ["8"		"8"		9	"8"		?8	]
    ["9"		"9"		10	"9"		?9	]
    ["0"		"0"		11	"0"		?0	]
    ["-"		"minus"		12	"-"		?-	]
    ["="		"equal"		13	"="		?=	]
    ["<backspace>"	"BackSpace"	14	"Backspace"	#x007F	]
    ["<tab>"		"Tab"		15	"Tab"		#x0009	]
    ["q"		"q"		16	"Q"		?q	]
    ["w"		"w"		17	"W"		?w	]
    ["e"		"e"		18	"E"		?e	]
    ["r"		"r"		19	"R"		?r	]
    ["t"		"t"		20	"T"		?t	]
    ["y"		"y"		21	"Y"		?y	]
    ["u"		"u"		22	"U"		?u	]
    ["i"		"i"		23	"I"		?i	]
    ["o"		"o"		24	"O"		?o	]
    ["p"		"p"		25	"P"		?p	]
    ["["		"bracketleft"	26	"["		?\[	]
    ["]"		"bracketright"	27	"]"		?\]	]
    ["<return>"		"Return"	28	"Return"	#x000D	]
    ["<Caps_Lock>"	"Caps_Lock"	58	"CapsLock"	nil	]
    ["a"		"a"		30	"A"		?a	]
    ["s"		"s"		31	"S"		?s	]
    ["d"		"d"		32	"D"		?d	]
    ["f"		"f"		33	"F"		?f	]
    ["g"		"g"		34	"G"		?g	]
    ["h"		"h"		35	"H"		?h	]
    ["j"		"j"		36	"J"		?j	]
    ["k"		"k"		37	"K"		?k	]
    ["l"		"l"		38	"L"		?l	]
    [";"		"semicolon"	39	";"		?\;	]
    ["'"		"apostrophe"	40	"'"		?'	]
    [nil		"Shift_L"	42	"Shift"		nil	]
    ["\\"		"backslash"	43	"\\"		?\\	]
    ["z"		"z"		44	"Z"		?z	]
    ["x"		"x"		45	"X"		?x	]
    ["c"		"c"		46	"C"		?c	]
    ["v"		"v"		47	"V"		?v	]
    ["b"		"b"		48	"B"		?b	]
    ["n"		"n"		49	"N"		?n	]
    ["m"		"m"		50	"M"		?m	]
    [","		"comma"		51	","		?,	]
    ["."		"period"	52	"."		?.	]
    ["/"		"slash"		53	"/"		?/	]
    [nil		"Shift_R"	54	"Shift"		nil	]
    [nil		"Ctrl_L"	29	"Ctrl"		nil	]
    [nil		"Super_L"	125	"Meta"		nil	]
    [nil		"Alt_L"		56	"Alt"		nil	]
    ["SPC"		"space"		57	"Space"		#x0020	]
    [nil		"Alt_R"		100	"Alt"		nil	]
    [nil		"Super_R"	126	"Meta"		nil	]
    ["<menu>"		"Menu"		127	"Menu"		#x0010	]
    [nil		"Ctrl_R"	97	"Ctrl"		nil	]

    ["<up>"		"Up"		103	"Up"		#xF700	]
    ["<down>"		"Down"		108	"Down"		#xF701	]
    ["<left>"		"Left"		105	"Left"		#xF702	]
    ["<right>"		"Right"		106	"Right"		#xF703	]

    ["<insert>"		"Insert"	110	"Ins"		#xF746	]
    ["<delete>"		"Delete"	111	"Del"		#xF728	]
    ["<home>"		"Home"		102	"Home"		#xF729	]
    ["<end>"		"End"		107	"End"		#xF72B	]
    ["<prior>"		"Prior"		104	"PgUp"		#xF72C	]
    ["<next>"		"Next"		109	"PgDown"	#xF72D	]

    ;; Add new entries at the end of the list, to avoid disrupting
    ;; existing configurations.

    ;; TODO: numpad
    )
  "List of keys supported by the `term-keys' package.

Each item in the list is a 5-element vector:

The first element is the Emacs key name, as it occurs in
`describe-key' or `kbd'.  nil can be used to indicate keys which
Emacs currently does not recognize (but are still known by other
input systems), such as modifier keys (which Emacs can't process
on its own, only in combination with a non-modifier key).

The second element is the X11 KeySym name, as returned by
XKeysymToString.  Used for urxvt/xterm configuration.

The third element is the keynumber (keycode) from the Linux TTY.
You can obtain a key's keynumber by running the 'showkey' program
in a TTY.

The fourth element is the Qt key name, as returned by
QKeySequence::toString and accepted by QKeySequence::fromString.
An easy way to obtain their name is using any KDE application's
\"Configure Shortcuts\" dialog.

The fifth element is the Unicode character code emitted by the
key on macOS.  The program \"Key Codes\" by developer \"Many
Tricks\" (available on the OS App Store) can display these
values."
  :type '(repeat
	  (vector
	   :tag "Key mapping"
	   (choice
	    :tag "Emacs key"
	    (const
	     :tag "No corresponding Emacs key"
	     nil)
	    (string
	     :tag "Emacs key name"))
	   (string
	    :tag "X11 KeySym")
	   (integer
	    :tag "Linux TTY keynumber")
	   (string
	    :tag "Qt key name")
	   (choice
	    :tag "macOS Unicode character code"
	    (const
	     :tag "None"
	     nil)
	    (integer
	     :tag "Character code"))))
  :group 'term-keys)


(defcustom term-keys/prefix "\033\037"
  "Key sequence prefix.

Indicates the byte string to be sent before a term-keys key code.

The default value is \\033\\037 (0x1B 0x1F, or ^[^_).

The prefix, or any starting substring of it, or any sequence
beginning with it, should not be already bound to an action in
Emacs.  E.g. with the default, neither ^[, ^[^_, or ^[^_abc
should by themselves be bound to an Emacs action."
  :type 'string
  :group 'term-keys)


(defcustom term-keys/suffix "\037"
  "Key sequence suffix.

Indicates the end of the data encoding the pressed key
combination.  Can be any character which isn't used in the
`term-keys/encode-number' encoding scheme."
  :type 'string
  :group 'term-keys)


(defun term-keys/want-key-p-def (key mods)
  "Default implementation for `term-keys/want-key-p-func'.

This function controls which key combinations are to be encoded
and decoded by default using the term-keys protocol extension.
KEY is the KeySym name as listed in `term-keys/mapping'; MODS is
a 6-element bool vector representing the modifiers Shift /
Control / Meta / Super / Hyper / Alt respectively, with t or nil
representing whether they are depressed or not.  Returns non-nil
if the specified key combination should be encoded.

Note that the ALT modifier rarely actually corresponds to the Alt
key on PC keyboards; the META modifier will usually be used
instead."
  (let ((shift   (elt mods 0))
	(control (elt mods 1))
	(meta    (elt mods 2))
	(super   (elt mods 3))
	(hyper   (elt mods 4))
	(alt     (elt mods 5)))
    (and

     ;; We don't care about Super/Hyper/Alt modifiers
     (not super)
     (not hyper)
     (not alt)

     (or
      ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2004-03/msg00306.html
      (and (string-equal key "g") control meta)

      ;; Navigation keys and Control/Alt
      (and (member key '("Up" "Down" "Left" "Right" "Home" "End" "Prior" "Next")) (or meta (and control shift)))

      ;; S-PgUp/PgDn - usually used for scrolling the terminal, but not useful in Emacs
      (and (member key '("Prior" "Next")) shift)

      ;; C-S-x is unrepresentable for letters
      (and (string-match-p "^[a-z]$" key) control shift)

      ;; C-x is unrepresentable for digits
      (and (string-match-p "^[0-9]$" key) control)

      ;; ...as well as punctuation and some special characters
      (and (member key '("Return" "Tab" "BackSpace"
			 "grave" "minus" "equal" "bracketleft" "bracketright" "semicolon"
			 "apostrophe" "backslash" "comma" "period" "slash" "space"))
	   control)

      ;; Shift + special chars
      (and (member key '("Return" "BackSpace")) shift)

      ;; Menu (Apps) key
      (string-equal key "Menu")
      ))))


(defcustom term-keys/want-key-p-func 'term-keys/want-key-p-def
  "Function for deciding whether to encode a key combination.

This should be set to a function with the same signature and
semantics as `term-keys/want-key-p-def'.  Look at that function's
documentation for more details.

Customize this variable to a function or lambda defined by you to
change which key combinations to encode."
  :type 'function
  :group 'term-keys)


(defconst term-keys/modifier-chars "SCMsHA"
  "The characters for the Emacs modifiers supported by term-keys.")


(defun term-keys/format-key (key mods)
  "Format key modifiers in Emacs/urxvt syntax.

Returns KEY prepended with S-, C-, M-, s-, H-, or A- depending on
the elements of the bool vector MODS are correspondingly
non-nil."
  (concat
   (cl-loop for modflag across mods
	    for index from 0
	    if modflag
	    concat (concat (string (elt term-keys/modifier-chars index))
			   "-"))
   key))


(defun term-keys/encode-number (num)
  "Efficiently encode non-negative integer NUM into a string.

Use only characters that can safely occur on a command line or
configuration file.  Current implementation uses base-96 (ASCII
\x20 .. \x7F)."
  (apply #'string
	 (nreverse (cl-loop while (not (zerop num))
			       collect (+ 32 (% num 96))
			       do (setq num (/ num 96))))))


(defun term-keys/decode-number (str)
  "Decode a string STR encoded by `term-keys/encode-number'."
  (cl-do ((bytes (append str nil)
		 (cdr bytes))
	  (num 0 (+ (* num 96) (- (car bytes) 32))))
      ((not bytes) num)))

(cl-loop for n in '(0 1 95 96 97 12345 123456789)
	 do (cl-assert (eq (term-keys/decode-number
			    (term-keys/encode-number n)) n)))


(defun term-keys/encode-key (key mods)
  "Encode a key combination to term-keys' protocol.

Returns a string ready to be sent by a terminal emulator (or
received by Emacs running in a terminal) which encodes the
combination of KEY (the key's index in the `term-keys/mapping'
table) and the modifiers MODS (a 6-element bool vector indicating
whether the respective modifier is pressed or not)."
  (term-keys/encode-number
   (cl-loop for index from 0
	    for factor = 1 then (* factor 2)
	    for modflag across mods
	    if modflag
	    sum factor into modflags
	    finally return (+ modflags (* factor key)))))


(defun term-keys/iterate-keys (fun)
  "Call FUN over every enabled key combination.

Iterate over all elements of `term-keys/mapping' and modifier key
combinations, filter the enabled ones using
`term-keys/want-key-p-func', and call (FUN INDEX KEYMAP MODS),
where INDEX is the key index in `term-keys/mapping', KEYMAP is
the `term-keys/mapping' element vector at that index, and MODS is
a bool vector for the active modifier keys.

Collect FUN's return values in a list and return it."
  (cl-loop
   for keymap in term-keys/mapping
   for index from 0
   append
   (cl-loop
    ;; Iterate from 0 to 2^64-1 for a bitmask of all modifier combinations
    for modnum from 0 to (1- (lsh 1 (length term-keys/modifier-chars)))
    ;; Convert the integer bitmask to a bool-vector
    for mods = (apply #'bool-vector (mapcar (lambda (n) (not (zerop (logand modnum (lsh 1 n)))))
					    (number-sequence 0 (1- (length term-keys/modifier-chars)))))
    if (and
	(elt keymap 0)                  ; Representable in Emacs?
	(funcall term-keys/want-key-p-func (elt keymap 1) mods)) ; Want this key combination?
    collect (funcall fun index keymap mods))))


;;;###autoload
(defun term-keys/init ()
  "Set up configured key sequences for the current terminal."
  (interactive)
  (term-keys/iterate-keys
   (lambda (index keymap mods)
     (define-key
       input-decode-map
       (concat
	term-keys/prefix
	(term-keys/encode-key index mods)
	term-keys/suffix)
       (kbd (term-keys/format-key
	     (elt keymap 0) mods))))))


;;;###autoload
(define-minor-mode term-keys-mode
  "`term-keys' global minor mode.

When enabled, automatically set up configured keys for new frames
on TTY terminals.  If the current frame is on a TTY, set it up as
well."
  :global t
  :require 'term-keys
  (if term-keys-mode
      (progn
	(add-hook 'tty-setup-hook 'term-keys/init)
	(if (eq (framep-on-display) t)
	    (term-keys/init)))
    (remove-hook 'tty-setup-hook 'term-keys/init)))


(defconst term-keys/main-file-name (or load-file-name buffer-file-name)
  "Path to this file.  Used for interop.")


(provide 'term-keys)
;;; term-keys.el ends here
