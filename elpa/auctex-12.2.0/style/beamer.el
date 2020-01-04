;;; beamer.el --- AUCTeX style for the latex-beamer class

;; Copyright (C) 2003, 2004, 2005, 2008, 2013-2016, 2018 Free Software Foundation

;; Author: Thomas Baumann <thomas.baumann@ch.tum.de>
;; Created: 2003-12-20
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for the latex-beamer class.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(declare-function font-latex-update-font-lock
		  "font-latex"
		  (&optional syntactic-kws))

(defun LaTeX-beamer-after-insert-env (env start _end)
  "Do beamer-specific stuff after the insertion of an environment."
  ;; Add `fragile' as an optional argument to the frame environment if
  ;; a verbatim environment is inserted.
  (when (and (TeX-member env (LaTeX-verbatim-environments) 'string-equal)
	     (save-excursion
	       (goto-char start)
	       (string-equal (LaTeX-current-environment) "frame")))
    (save-excursion
      (when (re-search-backward "\\\\begin[ \t]*{frame}" nil t)
	(let ((end-of-begin (match-end 0)))
	  (goto-char end-of-begin)
	  (while (forward-comment 1))
	  (if (eq (char-after) (string-to-char LaTeX-optop))
	      (progn
		(forward-char)
		(insert "fragile")
		(unless (looking-at (concat "[ \t]*" LaTeX-optcl))
		  (insert ",")))
	    (goto-char end-of-begin)
	    (insert "[fragile]")))))))

(defvar LaTeX-beamer-frametitle-history nil
  "History of frame titles in beamer.")

(TeX-add-style-hook
 "beamer"
 (lambda ()
   (add-hook 'LaTeX-after-insert-env-hooks 'LaTeX-beamer-after-insert-env nil t)

   (TeX-run-style-hooks "amsmath" "amssymb" "amsthm" "color" "geometry"
			"hyperref" "inputenc" "translator" "xcolor")

   (unless LaTeX-beamer-section-labels-flag
     (make-local-variable 'LaTeX-section-hook)
     (setq LaTeX-section-hook
	   '(LaTeX-section-heading
	     LaTeX-section-title
	     LaTeX-section-section)))

   (setq LaTeX-item-list
	 (append '(("itemize" . LaTeX-item-beamer)
		   ("enumerate" . LaTeX-item-beamer))
		 LaTeX-item-list))

   (setq LaTeX-default-document-environment "frame")

   (LaTeX-paragraph-commands-add-locally "frametitle")

   (TeX-add-symbols
    '("alert" [ TeX-arg-beamer-overlay-spec ] 1)
    '("alt" [ TeX-arg-beamer-overlay-spec ] 2)
    '("beamerbutton" 1)
    '("beamergotobutton" 1)
    '("beamerreturnbutton" 1)
    '("beamerskipbutton" 1)
    '("frame" TeX-arg-beamer-frametitle)
    '("frametitle"
      (TeX-arg-eval TeX-read-string "Title: " nil 'LaTeX-beamer-frametitle-history))
    '("hyperlink" [ TeX-arg-beamer-overlay-spec ] 2)
    '("hyperlinkslideprev" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkslidenext" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkframestart" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkframeend" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkframestartnext" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkframeendprev" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkpresentationstart" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkpresentationend" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkappendixstart" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkappendixend" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkdocumentstart" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hyperlinkdocumentend" [ TeX-arg-beamer-overlay-spec ] 1)
    '("hypertarget" [ TeX-arg-beamer-overlay-spec ] 2)
    '("institute" 1)
    '("invisible" [ TeX-arg-beamer-overlay-spec ] 1)
    '("label" [ TeX-arg-beamer-overlay-spec ] 1)
    '("logo" 1)
    '("note" TeX-arg-beamer-note 1)
    '("only" [ TeX-arg-beamer-overlay-spec ] 1)
    '("onslide" [ TeX-arg-beamer-overlay-spec ])
    '("partpage")
    '("pause" ["Slide number"])
    '("structure" [ TeX-arg-beamer-overlay-spec ] 1)
    '("temporal" [ TeX-arg-beamer-overlay-spec ] 3)
    '("titlepage")
    '("titlegraphic" 1)
    '("uncover" [ TeX-arg-beamer-overlay-spec ] 1)
    '("usetheme" LaTeX-arg-beamer-theme)
    '("useinnertheme" LaTeX-arg-beamer-inner-theme)
    '("useoutertheme" LaTeX-arg-beamer-outer-theme)
    '("usecolortheme" LaTeX-arg-beamer-color-theme)
    '("usefonttheme" LaTeX-arg-beamer-font-theme)
    '("usetheme" LaTeX-arg-beamer-theme)
    '("visible" [ TeX-arg-beamer-overlay-spec ] 1))

   (LaTeX-add-environments
    '("actionenv")
    '("alertblock" 1)
    '("beamerboxesrounded" 1)
    '("block" (lambda (env &rest ignore)
		(LaTeX-insert-environment
		 env (format "{%s}" (TeX-read-string "Title: ")))))
    '("column" "Width")
    "columns"
    "columnsonlytextwidth"
    '("exampleblock" 1)
    '("frame"  (lambda (env &rest ignore)
		 (let ((title (TeX-read-string "(Optional) Title: " nil
					       'LaTeX-beamer-frametitle-history)))
		   (LaTeX-insert-environment env)
		   (unless (zerop (length title))
		     (save-excursion
		       (LaTeX-find-matching-begin)
		       (end-of-line)
		       (LaTeX-newline)
		       (insert (format "\\frametitle{%s}" title))
		       ;; This works because \frametitle is a
		       ;; paragraph command.
		       (backward-char)
		       (LaTeX-fill-paragraph))))))
    '("onlyenv" (lambda (env &rest ignore)
		  (LaTeX-insert-environment
		   env
		   (let ((overlay (TeX-read-string "(Optional) Overlay: ")))
		     (unless (zerop (length overlay))
		       (format "<%s>" overlay))))))
    '("overlayarea" "Area width" "Area height")
    '("overprint"  (lambda (env &rest ignore)
		     (LaTeX-insert-environment
		      env
		      (let ((width (TeX-read-string "(Optional) Area width: ")))
			(unless (zerop (length width))
			  (format "[%s]" width))))))
    "semiverbatim")

   (LaTeX-largest-level-set "section")
   (LaTeX-add-counters "lecture" "part" "section" "subsection" "subsubsection"
		       "subsectionslide" "framenumber" "figure" "table"
		       "beamerpauses")
   (LaTeX-add-pagestyles "navigation")
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
		'("semiverbatim" current-indentation) t)
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("title" "[{")
				("subtitle" "[{")
				("author" "[{")
				("date" "[{")
				("frametitle" "<[{")) 'slide-title)
     (font-latex-update-font-lock t)))
 LaTeX-dialect)

(defun TeX-arg-beamer-overlay-spec (optional &optional prompt)
  "Prompt for overlay specification.
If OPTIONAL is non-nil, insert the specification only if
non-empty and enclosed in \"<>\".  PROMPT replaces the standard
one."
  (let ((TeX-arg-opening-brace "<")
        (TeX-arg-closing-brace ">"))
    (TeX-argument-insert
     (TeX-read-string
      (TeX-argument-prompt optional prompt "Overlay"))
     optional)
    (indent-according-to-mode)))

(defun TeX-arg-beamer-frametitle (_optional &optional _prompt)
  "Prompt for the frametitle."
  (let ((title (TeX-read-string "Title: " nil 'LaTeX-beamer-frametitle-history)))
    (if (not (zerop (length title)))
        (insert TeX-grop TeX-esc "frametitle" TeX-grop
		title TeX-grcl TeX-grcl)
      (insert TeX-grop TeX-grcl))))

(defun LaTeX-item-beamer ()
  "Insert a new item with an optional overlay argument. You
can turn off the prompt for the overlay argument by setting
`LaTeX-beamer-item-overlay-flag' to nil. Calling the function
with a prefix argument prompts for the overlay specification
unconditionally."
  (if (listp current-prefix-arg)
      (setq current-prefix-arg (car current-prefix-arg))
    current-prefix-arg)
  (TeX-insert-macro "item")
  (delete-horizontal-space)
  (if (or current-prefix-arg LaTeX-beamer-item-overlay-flag)
      (TeX-arg-beamer-overlay-spec 0))
  (insert " ")
  (indent-according-to-mode))

(defun TeX-arg-beamer-note (_optional &optional _prompt)
  "Prompt for overlay specification and optional argument."
  (let ((overlay (TeX-read-string "(Optional) Overlay: "))
        (options (TeX-read-string "(Optional) Options: ")))
    (unless (zerop (length overlay))
      (insert "<" overlay ">"))
    (unless (zerop (length options))
      (insert "[" options "]"))
    (indent-according-to-mode)))

(defun LaTeX-beamer-search-themes (&optional regexp extensions length)
  "Search for beamer themes matching REGEXP with EXTENSIONS.
The function removes the first LENGTH characters and the
extension of the file and returns a list of strings.  LENGTH may
also be a string.  Then the length of the string is used."
  (let* ((match (or regexp "^beamertheme[A-Z]"))
	 (exts  (or extensions '("tex" "sty")))
	 (chars (cond ((integerp length)
		       length)
		      ((stringp length)
		       (string-width length))
		      ;; Try some DWIM magic...
		      ((and (not length)
			    (string-match "beamer[A-Za-z0-9]*theme" match))
		       (- (match-end 0) (match-beginning 0)))
		      (t (error "Invalid length: `%s'" length)))))
    ;; (message "match=`%s' chars=`%s'" match chars)
    (TeX-delete-duplicate-strings
     (delete nil
	     (mapcar
	      (lambda (file)
		(let ((case-fold-search nil))
		  (and (numberp (string-match match file))
		       (substring file chars))))
	      (TeX-search-files nil exts t t))))))

(defun LaTeX-arg-beamer-theme (&rest _ignore)
  "Prompt for beamer theme with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt nil nil "Theme")
    (mapcar 'list
	    (cond ((eq LaTeX-beamer-themes 'local)
		   (set (make-local-variable 'LaTeX-beamer-themes)
			(LaTeX-beamer-search-themes)))
		  ((functionp LaTeX-beamer-themes)
		   (funcall LaTeX-beamer-themes))
		  ((listp LaTeX-beamer-themes)
		   LaTeX-beamer-themes)
		  (t (error
		      "`LaTeX-beamer-themes' should be a list: `%s'"
		      LaTeX-beamer-themes))))
    nil nil nil)
   t))

(defun LaTeX-arg-beamer-inner-theme (&rest _ignore)
  "Prompt for beamer inner theme with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt nil nil "Theme")
    (mapcar 'list
	    (cond ((eq LaTeX-beamer-inner-themes 'local)
		   (set (make-local-variable 'LaTeX-beamer-inner-themes)
			(LaTeX-beamer-search-themes "^beamerinnertheme")))
		  ((functionp LaTeX-beamer-inner-themes)
		   (funcall LaTeX-beamer-inner-themes))
		  ((listp LaTeX-beamer-inner-themes)
		   LaTeX-beamer-inner-themes)
		  (t (error
		      "`LaTeX-beamer-inner-themes' should be a list: `%s'"
		      LaTeX-beamer-inner-themes))))
    nil nil nil)
   t))

(defun LaTeX-arg-beamer-outer-theme (&rest _ignore)
  "Prompt for beamer outer theme with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt nil nil "Theme")
    (mapcar 'list
	    (cond ((eq LaTeX-beamer-outer-themes 'local)
		   (set (make-local-variable 'LaTeX-beamer-outer-themes)
			(LaTeX-beamer-search-themes "^beameroutertheme")))
		  ((functionp LaTeX-beamer-outer-themes)
		   (funcall LaTeX-beamer-outer-themes))
		  ((listp LaTeX-beamer-outer-themes)
		   LaTeX-beamer-outer-themes)
		  (t (error
		      "`LaTeX-beamer-outer-themes' should be a list: `%s'"
		      LaTeX-beamer-outer-themes))))
    nil nil nil)
   t))

(defun LaTeX-arg-beamer-color-theme (&rest _ignore)
  "Prompt for beamer color theme with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt nil nil "Theme")
    (mapcar 'list
	    (cond ((eq LaTeX-beamer-color-themes 'local)
		   (set (make-local-variable 'LaTeX-beamer-color-themes)
			(LaTeX-beamer-search-themes "^beamercolortheme")))
		  ((functionp LaTeX-beamer-color-themes)
		   (funcall LaTeX-beamer-color-themes))
		  ((listp LaTeX-beamer-color-themes)
		   LaTeX-beamer-color-themes)
		  (t (error
		      "`LaTeX-beamer-color-themes' should be a list: `%s'"
		      LaTeX-beamer-color-themes))))
    nil nil nil)
   t))

(defun LaTeX-arg-beamer-font-theme (&rest _ignore)
  "Prompt for beamer font theme with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt nil nil "Theme")
    (mapcar 'list
	    (cond ((eq LaTeX-beamer-font-themes 'local)
		   (set (make-local-variable 'LaTeX-beamer-font-themes)
			(LaTeX-beamer-search-themes "^beamerfonttheme")))
		  ((functionp LaTeX-beamer-font-themes)
		   (funcall LaTeX-beamer-font-themes))
		  ((listp LaTeX-beamer-font-themes)
		   LaTeX-beamer-font-themes)
		  (t (error
		      "`LaTeX-beamer-font-themes' should be a list: `%s'"
		      LaTeX-beamer-font-themes))))
    nil nil nil)
   t))

(defun LaTeX-beamer-class-options ()
  "Read the beamer class options from the user."
  (TeX-load-style "hyperref")
  (TeX-read-key-val t '(("usepdftitle" ("false")) ("envcountsect")
			("notheorems") ("noamsthm") ("compress") ("t") ("c")
			("leqno") ("fleqn") ("handout") ("trans") ("pdftex")
			("nativepdf") ("pdfmark") ("dvips") ("dviwindo")
			("dvipsone") ("vtex") ("ps2pdf") ("ignorenonframetext")
			("noamssymb") ("bigger") ("smaller") ("8pt") ("9pt")
			("10pt") ("11pt") ("12pt") ("14pt") ("17pt") ("20pt")
			("draft") ("CJK") ("cjk") ("pgf")
			("hyperref" LaTeX-hyperref-package-options-list)
			("color") ("xcolor") ("ucs") ("utf8x") ("utf8")
			("aspectratio" ("1610" "169" "149" "54" "43" "32")))))

;;; beamer.el ends here
