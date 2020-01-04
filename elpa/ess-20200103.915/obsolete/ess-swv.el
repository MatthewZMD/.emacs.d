;;; ess-swv.el --- Some simple functions for ESS and Sweave

;; Copyright (C) 2005 David Whiting, A.J. Rossini, Richard M. Heiberger, Martin
;;     Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2006-2008 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;     Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: David Whiting <david.whiting@ncl.ac.uk>
;; Created: 15 April 2005
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: statistics, tools

;; This file is part of ESS.

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

;;
;; Some simple functions for ESS and Sweave
;; david.whiting at ncl.ac.uk
;; Wed Sep 1 14:55:52 CEST 2004

;; I have written some very simple elisp functions that I have found
;; useful and thought others might like to see. I dabble with elisp
;; occasionally at best so there are probably better ways to do what I
;; have done, but so far this seems to work for me. There are several
;; things that are hard-coded, I use Linux and I think that this would
;; probably not work in Windows (not as it is now anyway).

;; With these functions and key bindings all I need to do is open a .Rnw
;; file and, assuming R is running, I press:

;; M-n s to Sweave the file, then
;; M-n l to run latex on the results of Sweave, then
;; M-n p to make and display a postscript file , or
;; M-n P to make and display a PDF version.

;; David  Whiting to Anthony Rossini,  Mar 30
;; On Wed, Mar 30, 2005 at 11:51:26AM +0200, A.J. Rossini wrote:
;; > I'm going to go ahead and add this stuff to the distribution tonight
;; > if you don't mind.  I'd forgotten about it!
;; It would make me very happy indeed.
;; > (however, need permission to do so).
;; Permission granted!

;; Dave
;; --
;; David Whiting
;; School of Clinical Medical Sciences, The Medical School
;; University of Newcastle upon Tyne, NE2 4HH, UK.


;;; TODO:
;;;
;;; 1. I want to be able to send ess-swv-latex a parameter to tell it
;;; the number of times to run LaTeX (to get references updated
;;; correctly).
;;;
;;; 2. Also need to add ess-swv-Bibtex.
;;;
;;; 3. Might be good to have a way to chain commands.
;;;
;;; 4. ADD to the ../doc/ess.texi !!


;;; Code:

;;; Autoloads and Requires

(eval-when-compile
  (require 'ess-custom))

(require 'ess-utils)
(require 'ess-noweb-mode)
(require 'ess-r-mode); for Rnw-mode
(require 'easymenu)



(defvar TeX-command-list)
(defvar TeX-command-default)
(defvar TeX-file-extensions)
(declare-function TeX-normal-mode "tex")

(defgroup ess-sweave nil
  "Mode for editing Sweave (*.[SR]nw) files."
  :group 'ess-S
  :prefix "ess-")

(defcustom ess-pdf-viewer-pref nil
  "External pdf viewer you like to use from ESS.
Can be a string giving a name of the program or a list with car
giving heprogram and the tail giving the arguments. For example
'(\"okular\" \"--unique\")."
  :type '(choice (const nil) (repeat :tag "Command with arguments" string) (string :tag "Command")))

(defcustom ess-ps-viewer-pref nil
  "External PostScript viewer you like to use from ESS.
If nil, ESS will try finding one from a list."
  :type '(choice (const nil) string))

(defcustom ess-swv-pdflatex-commands '("texi2pdf" "pdflatex" "make")
  "Commands to run a version of pdflatex in  \\[ess-swv-PDF];
the first entry is the default command."
  :group 'ess-sweave
  :type '(repeat string))

(defcustom ess-swv-plug-into-AUCTeX-p nil
  "Non-nil means add commands to AUCTeX's \\[TeX-command-list]
to sweave the current noweb file and latex the result."
  :group 'ess-sweave
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ess-swv-processing-command ".ess_weave(%s, %s)"
  "Command used by `ess-swv-run-in-R'.

First %s is literally replaced by the processing command (for
example: Sweave) second %s is replaced with a string containing a
processed file and possibly additional argument encoding (example:
\"path/to/foo.Rnw\", encoding='utf-8')

.ess_weave changes the working directory to that of the supplied
file.

If you want to simply call knitr or Sweave in global environment
set this command to \"%s(%s)\"."
  :group 'ess-R
  :type 'string)

;; currently use exactly for "Sweave", "Stangle", "knit", and "purl"
(defun ess-swv-run-in-R (cmd &optional choose-process block)
  "Run \\[cmd] on the current .Rnw file.  Utility function not called by user."
  (let* ((rnw-buf (current-buffer)))
    (if choose-process ;; previous behavior
        (ess-force-buffer-current "R process to load into: ")
      ;; else
      (update-ess-process-name-list)
      (cond ((= 0 (length ess-process-name-list))
             (message "no ESS processes running; starting R")
             (sit-for 1); so the user notices before the next msgs/prompt
             (run-ess-r)
             (set-buffer rnw-buf)
             )
            ((not (string= "R" (ess-make-buffer-current))); e.g. Splus, need R
             (ess-force-buffer-current "R process to load into: "))
            ))

    (save-excursion
      ;; (ess-execute (format "require(tools)")) ;; Make sure tools is loaded.
      (basic-save-buffer); do not Sweave/Stangle old version of file !
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (rnw-file (buffer-file-name))
             (Rnw-dir (file-name-directory rnw-file))
             (buf-coding (symbol-name buffer-file-coding-system))
             ;; could consider other encodings, but utf-8 is "R standard" for non-ASCII:
             (cmd-args (concat "\"" rnw-file "\""
                               (if (string-match "^utf-8" buf-coding)
                                   ", encoding = \"utf-8\"")))
             (Sw-cmd
              (format ess-swv-processing-command cmd cmd-args)))
        (message "%s()ing %S" cmd rnw-file)
        ;; need to block when we are running ess-swv-weave-PDF so we
        ;; know when to start compiling the generated .tex file
        (if block
            (progn
              (ess-eval-linewise (concat Sw-cmd "\n") nil nil nil t)
              (message "Finished %s()ing %S" cmd rnw-file))
          (ess-execute Sw-cmd 'buffer nil nil)
          (pop-to-buffer-same-window rnw-buf)
          (display-buffer (buffer-name sbuffer)))))))

(defcustom ess-swv-processor 'sweave
  "Processor to use for weaving and tangling.
Currently 'sweave or 'knitr"
  :group 'ess-R
  :type '(choice (const sweave) (const knitr)))


(defun ess-swv-tangle ()
  "Run Stangle/purl on the current .Rnw file.
Depending on the `ess-swv-processor' used."
  (interactive)
  (ess-swv-run-in-R (cond ((eq ess-swv-processor 'sweave) "Stangle")
                          ((eq ess-swv-processor 'knitr)  "purl")
                          (t (error "Not a valid processor %s" ess-swv-processor)))))

(defun ess-swv-weave (&optional choose)
  "Run Sweave/knit on the current .Rnw file.
Depending on the `ess-swv-processor' used.

If CHOOSE is non-nil, offer a menu of available weavers.
"
  (interactive "P")
  (let ((processor (if choose
                       (ess-completing-read "Weaver" '("sweave" "knitr") nil t)
                     (symbol-name ess-swv-processor))))
    (ess-swv-run-in-R (cond ((string= processor "sweave") "Sweave")
                            ((string= processor "knitr")  "knit")
                            (t (error "Not a valid processor %s" processor))))))

(defun ess-swv-sweave ()
  "Run Sweave on the current .Rnw file."
  (interactive)
  (ess-swv-run-in-R "Sweave"))

(defun ess-swv-stangle ()
  "Run Stangle on the current .Rnw file."
  (interactive)
  (ess-swv-run-in-R "Stangle"))

(defun ess-swv-knit ()
  "Run knit on the current .Rnw file."
  (interactive)
  (ess-swv-run-in-R "knit"))

(defun ess-swv-purl ()
  "Run purl on the current .Rnw file."
  (interactive)
  (ess-swv-run-in-R "purl"))

;; trying different viewers; thanks to an original patch for
;; ess-swv.el from Leo <sdl@web.de> :
(defun ess-get-ps-viewer ()
  "Get external PostScript viewer to be used from ESS.
Use `ess-ps-viewer-pref' when that is executably found by \\[executable-find].
Otherwise try a list of fixed known viewers."
  (file-name-nondirectory
   (or (and ess-ps-viewer-pref          ; -> ./ess-custom.el
            (executable-find ess-ps-viewer-pref))
       (executable-find "gv")
       (executable-find "evince")
       (executable-find "kghostview"))))

(defun ess-get-pdf-viewer ()
  "Get external PDF viewer to be used from ESS.
Use `ess-pdf-viewer-pref' when that is executably found by \\[executable-find].
Otherwise try a list of fixed known viewers."
  (let ((viewer (or ess-pdf-viewer-pref
                    ;; (and (stringp ess-pdf-viewer-pref)         ; -> ./ess-custom.el
                    ;;      (executable-find ess-pdf-viewer-pref))
                    (executable-find "evince")
                    (executable-find "kpdf")
                    (executable-find "okular")
                    (executable-find "xpdf")
                    (executable-find "acroread")
                    (executable-find "xdg-open")
                    ;; this one is wrong, (ok for time being as it is used only in swv)
                    (when (fboundp 'ess-get-words-from-vector)
                      (car (ess-get-words-from-vector
                            "getOption(\"pdfviewer\")\n"))))))
    (when (stringp viewer)
      (setq viewer (file-name-nondirectory viewer)))
    viewer))

(defun ess-swv-weave-PDF (&optional choose)
  "Sweave/knit, compile TeX, and display PDF.
Run Sweave or knit depending on `ess-swv-processor' used.

If CHOOSE is non-nil, offer a menu of available weavers.
"
  (interactive "P")
  (let ((processor (if choose
                       (ess-completing-read "Weaver" '("sweave" "knitr") nil t)
                     (symbol-name ess-swv-processor))))
    (ess-swv-run-in-R (cond ((string= processor "sweave") "Sweave")
                            ((string= processor "knitr")  "knit")
                            (t (error "Not a valid processor %s" processor)))
                      nil t)
    (ess-swv-PDF nil t)))


(defun ess-swv-latex ()
  "Run LaTeX on the product of Sweave()ing the current file."
  (interactive)
  (save-excursion
    (let* ((namestem (file-name-sans-extension (buffer-file-name)))
           (latex-filename (concat namestem ".tex"))
           (tex-buf (get-buffer-create " *ESS-tex-output*")))
      (message "Running LaTeX on '%s' ..." latex-filename)
      (switch-to-buffer tex-buf)
      (call-process "latex" nil tex-buf 1 latex-filename)
      (switch-to-buffer (buffer-name))
      (display-buffer tex-buf)
      (message "Finished running LaTeX" ))))


(defun ess-swv-PS ()
  "Create a postscript file from a dvi file (name based on the current
Sweave file buffer name) and display it."
  (interactive)
  (let* ((buf (buffer-name))
         (namestem (file-name-sans-extension (buffer-file-name)))
         (dvi-filename (concat namestem ".dvi"))
         (psviewer (ess-get-ps-viewer)))
    (shell-command (concat "dvips -o temp.ps " dvi-filename))
    (shell-command (concat psviewer " temp.ps & "))
    (switch-to-buffer buf)
    ))

(defun ess-swv-PDF (&optional pdflatex-cmd hide-compile-buffer)
  "From LaTeX file, create a PDF (via 'texi2pdf' or 'pdflatex', ...), by
default using the first entry of `ess-swv-pdflatex-commands' and display it."
  (interactive)
  (setq pdflatex-cmd
        (or pdflatex-cmd
            (and (eq (length ess-swv-pdflatex-commands) 1)
                 (car ess-swv-pdflatex-commands))
            (ess-completing-read  "pdf latex command"
                                  ess-swv-pdflatex-commands ; <- collection to choose from
                                  nil 'confirm ; or 'confirm-after-completion
                                  nil nil (car ess-swv-pdflatex-commands))))
  (let* ((buf (buffer-name))
         (namestem (file-name-sans-extension (buffer-file-name)))
         (latex-filename (concat namestem ".tex"))
         (tex-buf (get-buffer-create "*ESS-tex-output*"))
         (pdfviewer (ess-get-pdf-viewer))
         (pdf-status)
         (cmdstr-win (format "start \"%s\" \"%s.pdf\"" pdfviewer namestem))
         (pdffile (format "%s.pdf" namestem))
         (cmd (if (stringp pdfviewer)
                  (list pdfviewer pdffile)
                (append pdfviewer  (list pdffile)))))

    ;;(shell-command (concat "pdflatex " latex-filename))
    (message "Running '%s' on '%s' ..." pdflatex-cmd latex-filename)
    (with-current-buffer tex-buf (erase-buffer))
    (setq pdf-status
          (call-process pdflatex-cmd nil tex-buf t
                        (if (string= "texi2" (substring pdflatex-cmd 0 5))
                            ;; workaround (bug?): texi2pdf or texi2dvi *fail* to work with full path:
                            (file-name-nondirectory latex-filename)
                          latex-filename)))
    (if (not (= 0 pdf-status))
        (progn (message "** OOPS: error in '%s' (%d)!" pdflatex-cmd pdf-status)
               (display-buffer tex-buf))
      ;; else: pdflatex probably ok
      ;; (set-process-sentinel proc 'shell-command-sentinel)
      (if (and (and ess-microsoft-p
                    ;; Silence byte compiler warns about w32-fns
                    (fboundp 'w32-shell-dos-semantics))
               (w32-shell-dos-semantics))
          (shell-command cmdstr-win)
        (message "%s" (mapconcat 'identity cmd " "))
        (apply 'start-process  (car cmd) nil cmd))
      (unless hide-compile-buffer (display-buffer tex-buf))
      (message "%s finished with status %d" pdflatex-cmd pdf-status))))


(defun ess-insert-Sexpr ()
  "Insert Sexpr{} into the buffer at point."
  (interactive)
  (insert "\\Sexpr{}")
  (backward-char))


;;; back-compatible wrappers:
(defun ess-makeSweave () "old *DEPRECATED* version of \\[ess-swv-weave]."
       (interactive) (ding)
       (message
        "** warning: ess-makeSweave is deprecated. Do use (ess-swv-weave) instead!")
       (ess-swv-weave))

(defun ess-makeLatex () "old *DEPRECATED* version of \\[ess-swv-latex]."
       (interactive) (ding)
       (message
        "** warning: ess-makeLatex is deprecated. Do use (ess-swv-latex) instead!")
       (ess-swv-latex))

(defun ess-makePS () "old *DEPRECATED* version of \\[ess-swv-PS]."
       (interactive) (ding)
       (message
        "** warning: ess-makePS is deprecated. Do use (ess-swv-PS) instead!")
       (ess-swv-PS))

(defun ess-makePDF () "old *DEPRECATED* version of \\[ess-swv-PDF]."
       (interactive) (ding)
       (message
        "** warning: ess-makePDF is deprecated. Do use (ess-swv-PDF) instead!")
       (ess-swv-PDF))


;; AUCTeX integration.  This is independent of this library, but it fits
;; here nonetheless since it's an alternative way of Sweave'ing without
;; starting iESS.

(defun ess-swv-add-TeX-commands ()
  "Add commands to AUCTeX's \\[TeX-command-list]."
  (unless (and (featurep 'tex-site) (featurep 'tex))
    (error "AUCTeX does not seem to be loaded"))
  (add-to-list 'TeX-command-list
               '("Sweave" "R CMD Sweave %t"
                 TeX-run-command nil (latex-mode) :help
                 "Run Sweave") t)
  (add-to-list 'TeX-command-list
               '("LaTeXSweave" "%l %(mode) %s"
                 TeX-run-TeX nil (latex-mode) :help
                 "Run LaTeX after Sweave") t)
  (setq TeX-command-default "Sweave")
  (mapc (lambda (suffix)
          (add-to-list 'TeX-file-extensions suffix))
        '("nw" "Snw" "Rnw")))

(defun ess-swv-remove-TeX-commands (x)
  "Helper function: check if car of X is one of the Sweave strings"
  (let ((swv-cmds '("Sweave" "LaTeXSweave")))
    (unless (member (car x) swv-cmds) x)))

(defun ess-swv-plug-into-AUCTeX ()
  "Add commands to AUCTeX's \\[TeX-command-list] to sweave the current noweb
file and latex the result."
  (if ess-swv-plug-into-AUCTeX-p
      (add-hook 'Rnw-mode-hook 'ess-swv-add-TeX-commands)
    (remove-hook 'Rnw-mode-hook 'ess-swv-add-TeX-commands)
    (setq TeX-command-list (mapcar 'ess-swv-remove-TeX-commands TeX-command-list)
          ;; this will remove the items, leaving nils, so remove them.
          TeX-command-list (delq nil TeX-command-list))))
;; as ess-swv-plug-into-AUCTeX-p is customizable ... :
(if ess-swv-plug-into-AUCTeX-p
    (eval-after-load "tex" '(ess-swv-plug-into-AUCTeX)))

(defun ess-swv-toggle-plug-into-AUCTeX ()
  "Toggle inclusion of commands to sweave noweb files and latex the results in
\\[TeX-command-list] on and off.  Commands are added via \\[Rnw-mode-hook]."
  (interactive)
  (unless (and (featurep 'tex-site) (featurep 'tex))
    (error "AUCTeX are not available"))
  (setq ess-swv-plug-into-AUCTeX-p (not ess-swv-plug-into-AUCTeX-p))
  (ess-swv-plug-into-AUCTeX)
  (TeX-normal-mode t)
  (if ess-swv-plug-into-AUCTeX-p
      (message "Sweave and LaTeXSweave are activated in AUCTeX.")
    (message "Sweave and LaTeXSweave are de-activated in AUCTeX.")))


;;; Now bind some keys.
(define-key ess-noweb-minor-mode-map "\M-ns" 'ess-swv-sweave)
(define-key ess-noweb-minor-mode-map "\M-nT" 'ess-swv-tangle)
(define-key ess-noweb-minor-mode-map "\M-nl" 'ess-swv-latex)
(define-key ess-noweb-minor-mode-map "\M-np" 'ess-swv-PS)
(define-key ess-noweb-minor-mode-map "\M-nP" 'ess-swv-PDF)
(define-key ess-noweb-minor-mode-map "\M-nr" 'ess-swv-knit)
(define-key ess-noweb-minor-mode-map "\M-nu" 'ess-swv-purl)
(define-key ess-noweb-minor-mode-map "\M-nv" 'ess-swv-weave-PDF)
(define-key ess-noweb-minor-mode-map "\M-nw" 'ess-swv-weave); depends on proc.

(define-key ess-noweb-minor-mode-map "\M-nx" 'ess-insert-Sexpr)

;; AND add these to the noweb menu we have anyway ! :
(easy-menu-define ess-swv-menu
  ess-noweb-minor-mode-menu
  "Submenu for use in `Rnw-mode'."

  '("Sweaving, Tangling, ..."
    ["Sweave" ess-swv-sweave  t]
    ["Tangle" ess-swv-tangle  t]
    ["LaTeX"  ess-swv-latex   t]
    ["PDF(LaTeX)" ess-swv-PDF t]
    ["PS (dvips)" ess-swv-PS  t]
    ["Knit" ess-swv-knit   t]
    ["Purl" ess-swv-purl   t]
    ["View PDF" ess-swv-weave-PDF t]
    ["Weave (Sweave/Knit)" ess-swv-weave t]
    ["Insert Sexpr" ess-insert-Sexpr t]
    ["AUCTeX Interface" ess-swv-toggle-plug-into-AUCTeX
     :style toggle :selected ess-swv-plug-into-AUCTeX-p]
    ))

(easy-menu-add-item ess-noweb-minor-mode-menu
                    nil ;; <= path
                    ess-swv-menu)

 ; provides

(provide 'ess-swv)

;;; ess-swv.el ends here
