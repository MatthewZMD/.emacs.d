;;; tex-style.el --- Customizable variables for AUCTeX style files

;; Copyright (C) 2005, 2007, 2009, 2012-2017
;;   Free Software Foundation, Inc.

;; Author: Reiner Steib <Reiner.Steib@gmx.de>
;; Keywords: tex, wp, convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides customizable variables for AUCTeX style files.

;;; Code:

(defgroup LaTeX-style nil
  "Support for special LaTeX style files in AUCTeX."
  :group 'LaTeX-macro)

;; Note: We don't have any defcustom in plain TeX style files yet.  Else we
;; should also create a TeX-style group.

;; Common

(defcustom LaTeX-reftex-ref-style-auto-activate t
  "Whether to activate automatically RefTeX reference styles."
  :type 'boolean
  :group 'LaTeX-style)

(defcustom LaTeX-reftex-cite-format-auto-activate t
  "Whether to activate automatically RefTeX citation format."
  :type 'boolean
  :group 'LaTeX-style)

;; style/amsmath.el

(defcustom LaTeX-amsmath-label nil
  "Default prefix to amsmath equation labels.

Amsmath equations include \"align\", \"alignat\", \"xalignat\",
\"multline\", \"flalign\" and \"gather\".  If it is nil,
`LaTeX-equation-label' is used."
  :group 'LaTeX-label
  :type '(choice (const :tag "Use `LaTeX-equation-label'" nil)
		 (string)))

;; style/beamer.el

(defcustom LaTeX-beamer-section-labels-flag nil
  "If non-nil section labels are added"
  :type 'boolean
  :group 'LaTeX-style)

(defcustom LaTeX-beamer-item-overlay-flag t
  "If non-nil do prompt for an overlay in itemize-like environments."
  :type 'boolean
  :group 'LaTeX-style)

(defcustom LaTeX-beamer-themes 'local
  "Presentation themes for the LaTeX beamer package.
It can be a list of themes or a function.  If it is the symbol
`local', search only once per buffer."
  :group 'LaTeX-style
  :type
  '(choice
    (const :tag "TeX search" LaTeX-beamer-search-themes)
    (const :tag "Search once per buffer" local)
    (function :tag "Other function")
    (list
     :value
     ;; Work around (bug in customize?), see
     ;; <news:v9is48jrj1.fsf@marauder.physik.uni-ulm.de>
     ("Antibes" "Bergen" "Berkeley" "Berlin" "Boadilla" "Copenhagen"
      "Darmstadt" "Dresden" "Frankfurt" "Goettingen" "Hannover"
      "Ilmenau" "JuanLesPins" "Luebeck" "Madrid" "Malmoe" "Marburg"
      "Montpellier" "PaloAlto" "Pittsburgh" "Rochester" "Singapore"
      "Szeged" "Warsaw")
     (set :inline t
	  (const "Antibes")
	  (const "Bergen")
	  (const "Berkeley")
	  (const "Berlin")
	  (const "Boadilla")
	  (const "Copenhagen")
	  (const "Darmstadt")
	  (const "Dresden")
	  (const "Frankfurt")
	  (const "Goettingen")
	  (const "Hannover")
	  (const "Ilmenau")
	  (const "JuanLesPins")
	  (const "Luebeck")
	  (const "Madrid")
	  (const "Malmoe")
	  (const "Marburg")
	  (const "Montpellier")
	  (const "PaloAlto")
	  (const "Pittsburgh")
	  (const "Rochester")
	  (const "Singapore")
	  (const "Szeged")
	  (const "Warsaw"))
     (repeat :inline t
	     :tag "Other"
	     (string)))))

(defcustom LaTeX-beamer-inner-themes 'local
  "Presentation inner themes for the LaTeX beamer package.
It can be a list of themes or a function.  If it is the symbol
`local', search only once per buffer."
  :group 'LaTeX-style
  :type '(choice
    (const :tag "TeX search" LaTeX-beamer-search-inner-themes)
    (const :tag "Search once per buffer" local)
    (function :tag "Other function")
    (list
     :value ("circles" "default" "inmargin" "rectangles" "rounded")
     (set :inline t
	  (const "circles")
	  (const "default")
	  (const "inmargin")
	  (const "rectangles")
	  (const "rounded"))
     (repeat :inline t
	     :tag "Other"
	     (string)))))

(defcustom LaTeX-beamer-outer-themes 'local
  "Presentation outer themes for the LaTeX beamer package.
It can be a list of themes or a function.  If it is the symbol
`local', search only once per buffer."
  :group 'LaTeX-style
  :type
  '(choice
    (const :tag "TeX search" LaTeX-beamer-search-outer-themes)
    (const :tag "Search once per buffer" local)
    (function :tag "Other function")
    (list
     :value
     ("default" "infolines" "miniframes" "shadow" "sidebar" "smoothbars"
      "smoothtree" "split" "tree")
     (set :inline t
	  (const "default")
	  (const "infolines")
	  (const "miniframes")
	  (const "shadow")
	  (const "sidebar")
	  (const "smoothbars")
	  (const "smoothtree")
	  (const "split")
	  (const "tree"))
     (repeat :inline t
	     :tag "Other"
	     (string)))))

(defcustom LaTeX-beamer-color-themes 'local
  "Presentation color themes for the LaTeX beamer package.
It can be a list of themes or a function.  If it is the symbol
`local', search only once per buffer."
  :group 'LaTeX-style
  :type
  '(choice
    (const :tag "TeX search" LaTeX-beamer-search-color-themes)
    (const :tag "Search once per buffer" local)
    (function :tag "Other function")
    (list
     :value
     ("albatross" "beetle" "crane" "default" "dolphin" "dove" "fly" "lily"
      "orchid" "rose" "seagull" "seahorse" "sidebartab" "structure" "whale")
     (set :inline t
	  (const "albatross")
	  (const "beetle")
	  (const "crane")
	  (const "default")
	  (const "dolphin")
	  (const "dove")
	  (const "fly")
	  (const "lily")
	  (const "orchid")
	  (const "rose")
	  (const "seagull")
	  (const "seahorse")
	  (const "sidebartab")
	  (const "structure")
	  (const "whale"))
     (repeat :inline t
	     :tag "Other"
	     (string)))))

(defcustom LaTeX-beamer-font-themes 'local
  "Presentation font themes for the LaTeX beamer package.
It can be a list of themes or a function.  If it is the symbol
`local', search only once per buffer."
  :group 'LaTeX-style
  :type
  '(choice
    (const :tag "TeX search" LaTeX-beamer-search-font-themes)
    (const :tag "Search once per buffer" local)
    (function :tag "Other function")
    (list
     :value
     ("default" "professionalfonts" "serif" "structurebold"
      "structureitalicserif" "structuresmallcapsserif")
     (set :inline t
	  (const "default")
	  (const "professionalfonts")
	  (const "serif")
	  (const "structurebold")
	  (const "structureitalicserif")
	  (const "structuresmallcapsserif"))
     (repeat :inline t
	     :tag "Other"
	     (string)))))

;; style/biblatex.el

(defvar LaTeX-biblatex-use-Biber t
  "Whether to use Biber with biblatex.

This variable is intended to be used as a file local variable to
override the autodetection of the biblatex backend.")
(make-variable-buffer-local 'LaTeX-biblatex-use-Biber)
(put 'LaTeX-biblatex-use-Biber 'safe-local-variable #'booleanp)

;; style/comment.el

(defcustom LaTeX-comment-env-list '("comment")
  "List of environment names defined with comment.sty.
Setting this variable does not take effect unless you
reinitialize affected buffers."
  :type '(repeat string)
  :group 'LaTeX-style)

;; style/csquotes.el

(defcustom LaTeX-csquotes-quote-after-quote nil
  "Initial value of `TeX-quote-after-quote' for `csquotes.el'"
  :type 'boolean
  :group 'LaTeX-style)

(defcustom LaTeX-csquotes-open-quote ""
  "Opening quotation mark to be used with the csquotes package.
The specified string will be used for `TeX-open-quote' (and override
any language-specific setting) only if both `LaTeX-csquotes-open-quote'
and `LaTeX-csquotes-close-quote' are non-empty strings."
  :type 'string
  :group 'LaTeX-style)

(defcustom LaTeX-csquotes-close-quote ""
  "Closing quotation mark to be used with the csquotes package.
The specified string will be used for `TeX-close-quote' (and override
any language-specific setting) only if both `LaTeX-csquotes-open-quote'
and `LaTeX-csquotes-close-quote' are non-empty strings."
  :type 'string
  :group 'LaTeX-style)

;; style/emp.el

(defcustom LaTeX-write18-enabled-p t
  "*If non-nil, insert automatically the \\write18 calling metapost.
When disabled, you have to use mpost on the mp files automatically 
produced by emp.sty and then re-LaTeX the document."
  :type 'boolean
  :group 'LaTeX-style)

;; style/exam.el

(defcustom LaTeX-exam-reftex-quick-id-key ?x
  "Unique letter identifying exam class macros in RefTeX.

A character argument for quick identification when RefTeX inserts
new labels with `reftex-label'.  It must be unique.  It is
initialized to ?x."
  :group 'LaTeX-style
  :type 'character)

(defcustom LaTeX-exam-label "exm:"
  "Default prefix to labels in environments of exam class."
  :group 'LaTeX-style
  :type 'string)

;; style/fontspec.el

(defcustom LaTeX-fontspec-arg-font-search t
  "If `LaTeX-fontspec-arg-font' should search for fonts.
If the value is t, fonts are retrieved automatically and provided
for completion.  If the value is nil,
`LaTeX-fontspec-font-list-default' is used for completion.  If
the value is `ask', you are asked for the method to use every
time `LaTeX-fontspec-arg-font' is called.

`LaTeX-fontspec-arg-font' calls `luaotf-load --list=basename' to
automatically get the list of fonts.  This requires
`luaotfload-tool' version 2.3 or higher in order to work."
  :group 'LaTeX-style
  :type '(choice
	  (const :tag "Search automatically" t)
	  (const :tag "Use default font list" nil)
	  (const :tag "Ask what to do" ask)))

(defcustom LaTeX-fontspec-font-list-default nil
  "List of default fonts to be used as completion for
`LaTeX-fontspec-arg-font'."
  :group 'LaTeX-style
  :type '(repeat (string :tag "Font")))

;; style/graphicx.el

(defcustom LaTeX-includegraphics-extensions
  '("eps" "jpe?g" "pdf" "png")
  "Extensions for images files used by \\includegraphics."
  :group 'LaTeX-style
  :type '(list (set :inline t
		    (const "eps")
		    (const "jpe?g")
		    (const "pdf")
		    (const "png"))
	       (repeat :inline t
		       :tag "Other"
		       (string))))

(defcustom LaTeX-includegraphics-strip-extension-flag t
  "Non-nil means to strip known extensions from image file name."
  :group 'LaTeX-style
  :type 'boolean)

(defcustom LaTeX-includegraphics-read-file
  'LaTeX-includegraphics-read-file-TeX
  "Function for reading \\includegraphics files.

`LaTeX-includegraphics-read-file-TeX' lists all graphic files
found in the TeX search path.

`LaTeX-includegraphics-read-file-relative' lists all graphic files
in the master directory and its subdirectories and inserts the
relative file name.

The custom option `simple' works as
`LaTeX-includegraphics-read-file-relative' but it lists all kind of
files.

Inserting the subdirectory in the filename (as
`LaTeX-includegraphics-read-file-relative') is discouraged by
`epslatex.ps'."
;; ,----[ epslatex.ps; Section 12; (page 26) ]
;; | Instead of embedding the subdirectory in the filename, there are two
;; | other options
;; |   1. The best method is to modify the TeX search path [...]
;; |   2. Another method is to specify sub/ in a \graphicspath command
;; |      [...].  However this is much less efficient than modifying the
;; |      TeX search path
;; `----
;; See "Inefficiency" and "Unportability" in the same section for more
;; information.
  :group 'LaTeX-style
  :type '(choice (const :tag "TeX" LaTeX-includegraphics-read-file-TeX)
		 (const :tag "relative"
			LaTeX-includegraphics-read-file-relative)
		 (const :tag "simple" (lambda ()
					(file-relative-name
					 (read-file-name "Image file: ")
					 (TeX-master-directory))))
		 (function :tag "other")))

;; style/splitidx.el

(defcustom LaTeX-splitidx-sindex-reftex-quick-id-key ?s
  "Unique letter identifying \"\\sindex\" macro in RefTeX.

A character argument for quick identification of \"\\sindex\"
when RefTeX inserts new index entries with `reftex-index'.  It
must be unique.  It is initialized to ?s when added to
`reftex-index-macros'."
  :group 'LaTeX-style
  :type 'character)

(provide 'tex-style)

;;; tex-style.el ends here
