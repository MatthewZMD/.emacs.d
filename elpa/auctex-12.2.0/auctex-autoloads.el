;;; auctex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bib-cite" "bib-cite.el" (0 0 0 0))
;;; Generated autoloads from bib-cite.el

(autoload 'bib-cite-minor-mode "bib-cite" "\
Toggle bib-cite mode.
When bib-cite mode is enabled, citations, labels and refs are highlighted
when the mouse is over them.  Clicking on these highlights with [mouse-2]
runs bib-find, and [mouse-3] runs bib-display.

\(fn ARG)" t nil)

(autoload 'turn-on-bib-cite "bib-cite" "\
Unconditionally turn on Bib Cite mode.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bib-cite" '("psg-" "bib-" "search-directory-tree" "member-cis" "create-alist-from-list" "LaTeX-")))

;;;***

;;;### (autoloads nil "context" "context.el" (0 0 0 0))
;;; Generated autoloads from context.el

(defalias 'ConTeXt-mode 'context-mode)

(autoload 'context-mode "context" "\
Major mode in AUCTeX for editing ConTeXt files.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of ConTeXt-mode-hook.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "context" '("context-guess-current-interface" "ConTeXt-" "TeX-ConTeXt-sentinel")))

;;;***

;;;### (autoloads nil "context-en" "context-en.el" (0 0 0 0))
;;; Generated autoloads from context-en.el

(autoload 'context-en-mode "context-en" "\
Major mode for editing files for ConTeXt using its english interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "context-en" '("ConTeXt-")))

;;;***

;;;### (autoloads nil "context-nl" "context-nl.el" (0 0 0 0))
;;; Generated autoloads from context-nl.el

(autoload 'context-nl-mode "context-nl" "\
Major mode for editing files for ConTeXt using its dutch interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "context-nl" '("ConTeXt-")))

;;;***

;;;### (autoloads nil "font-latex" "font-latex.el" (0 0 0 0))
;;; Generated autoloads from font-latex.el

(autoload 'font-latex-setup "font-latex" "\
Setup this buffer for LaTeX font-lock.  Usually called from a hook.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "font-latex" '("font-l")))

;;;***

;;;### (autoloads nil "latex" "latex.el" (0 0 0 0))
;;; Generated autoloads from latex.el

(autoload 'BibTeX-auto-store "latex" "\
This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file.

\(fn)" nil nil)

(add-to-list 'auto-mode-alist '("\\.drv\\'" . latex-mode))

(add-to-list 'auto-mode-alist '("\\.hva\\'" . latex-mode))

(autoload 'TeX-latex-mode "latex" "\
Major mode in AUCTeX for editing LaTeX files.
See info under AUCTeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `LaTeX-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dtx\\'" . doctex-mode))

(autoload 'docTeX-mode "latex" "\
Major mode in AUCTeX for editing .dtx files derived from `LaTeX-mode'.
Runs `LaTeX-mode', sets a few variables and
runs the hooks in `docTeX-mode-hook'.

\(fn)" t nil)

(defalias 'TeX-doctex-mode 'docTeX-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "latex" '("LaTeX-" "docTeX-" "TeX-" "latex-math-mode" "Bib")))

;;;***

;;;### (autoloads nil "latex-flymake" "latex-flymake.el" (0 0 0 0))
;;; Generated autoloads from latex-flymake.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "latex-flymake" '("LaTeX-")))

;;;***

;;;### (autoloads nil "multi-prompt" "multi-prompt.el" (0 0 0 0))
;;; Generated autoloads from multi-prompt.el

(autoload 'multi-prompt "multi-prompt" "\
Completing prompt for a list of strings.  
The first argument SEPARATOR should be the string (of length 1) to
separate the elements in the list.  The second argument UNIQUE should
be non-nil, if each element must be unique.  The remaining elements
are the arguments to `completing-read'.  See that.

\(fn SEPARATOR UNIQUE PROMPT TABLE &optional MP-PREDICATE REQUIRE-MATCH INITIAL HISTORY)" nil nil)

(autoload 'multi-prompt-key-value "multi-prompt" "\
Read multiple strings, with completion and key=value support.
PROMPT is a string to prompt with, usually ending with a colon
and a space.  TABLE is an alist.  The car of each element should
be a string representing a key and the optional cdr should be a
list with strings to be used as values for the key.

See the documentation for `completing-read' for details on the
other arguments: PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST,
DEF, and INHERIT-INPUT-METHOD.

The return value is the string as entered in the minibuffer.

\(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-prompt" '("multi-prompt-")))

;;;***

;;;### (autoloads nil "plain-tex" "plain-tex.el" (0 0 0 0))
;;; Generated autoloads from plain-tex.el

(autoload 'TeX-plain-tex-mode "plain-tex" "\
Major mode in AUCTeX for editing plain TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{plain-TeX-mode-map}

Entering `plain-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `plain-TeX-mode-hook'.

\(fn)" t nil)

(autoload 'ams-tex-mode "plain-tex" "\
Major mode in AUCTeX for editing AmS-TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{AmSTeX-mode-map}

Entering `ams-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `AmS-TeX-mode-hook'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "plain-tex" '("AmS" "plain-TeX-")))

;;;***

;;;### (autoloads nil "preview" "preview.el" (0 0 0 0))
;;; Generated autoloads from preview.el

(autoload 'preview-install-styles "preview" "\
Installs the TeX style files into a permanent location.
This must be in the TeX search path.  If FORCE-OVERWRITE is greater
than 1, files will get overwritten without query, if it is less
than 1 or nil, the operation will fail.  The default of 1 for interactive
use will query.

Similarly FORCE-SAVE can be used for saving
`preview-TeX-style-dir' to record the fact that the uninstalled
files are no longer needed in the search path.

\(fn DIR &optional FORCE-OVERWRITE FORCE-SAVE)" t nil)

(autoload 'LaTeX-preview-setup "preview" "\
Hook function for embedding the preview package into AUCTeX.
This is called by `LaTeX-mode-hook' and changes AUCTeX variables
to add the preview functionality.

\(fn)" nil nil)

(autoload 'preview-report-bug "preview" "\
Report a bug in the preview-latex package.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "preview" '("preview-" "TeX-" "desktop-buffer-preview")))

;;;***

;;;### (autoloads nil "prv-emacs" "prv-emacs.el" (0 0 0 0))
;;; Generated autoloads from prv-emacs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "prv-emacs" '("preview-")))

;;;***

;;;### (autoloads nil "tex" "tex.el" (0 0 0 0))
;;; Generated autoloads from tex.el

(autoload 'TeX-tex-mode "tex" "\
Major mode in AUCTeX for editing TeX or LaTeX files.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) if the file is empty or `TeX-force-default-mode' is not set to nil,
      `TeX-default-mode' is chosen
   2) If \\documentstyle or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, `latex-mode' is selected.
   3) Otherwise, use `plain-tex-mode'

\(fn)" t nil)

(autoload 'TeX-auto-generate "tex" "\
Generate style file for TEX and store it in AUTO.
If TEX is a directory, generate style files for all files in the directory.

\(fn TEX AUTO)" t nil)

(autoload 'TeX-auto-generate-global "tex" "\
Create global auto directory for global TeX macro definitions.

\(fn)" t nil)

(autoload 'TeX-submit-bug-report "tex" "\
Submit a bug report on AUCTeX via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your AUCTeX version and AUCTeX configuration.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex" '("TeX-" "ispell-tex-" "Bib" "docTeX-default-extension" "LaTeX-" "plain-TeX-auto-regexp-list" "VirTeX-common-initialization" "tex-" "ConTeXt-")))

;;;***

;;;### (autoloads nil "tex-bar" "tex-bar.el" (0 0 0 0))
;;; Generated autoloads from tex-bar.el

(autoload 'TeX-install-toolbar "tex-bar" "\
Install toolbar buttons for TeX mode.

\(fn)" t nil)

(autoload 'LaTeX-install-toolbar "tex-bar" "\
Install toolbar buttons for LaTeX mode.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-bar" '("TeX-bar-")))

;;;***

;;;### (autoloads nil "tex-buf" "tex-buf.el" (0 0 0 0))
;;; Generated autoloads from tex-buf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-buf" '("TeX-" "LaTeX-" "compilation-in-progress")))

;;;***

;;;### (autoloads nil "tex-fold" "tex-fold.el" (0 0 0 0))
;;; Generated autoloads from tex-fold.el

(autoload 'TeX-fold-mode "tex-fold" "\
Minor mode for hiding and revealing macros and environments.

Called interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off.

\(fn &optional ARG)" t nil)

(defalias 'tex-fold-mode 'TeX-fold-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-fold" '("TeX-fold-")))

;;;***

;;;### (autoloads nil "tex-font" "tex-font.el" (0 0 0 0))
;;; Generated autoloads from tex-font.el

(autoload 'tex-font-setup "tex-font" "\
Setup font lock support for TeX.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-font" '("tex-")))

;;;***

;;;### (autoloads nil "tex-info" "tex-info.el" (0 0 0 0))
;;; Generated autoloads from tex-info.el

(defalias 'Texinfo-mode 'texinfo-mode)

(autoload 'TeX-texinfo-mode "tex-info" "\
Major mode in AUCTeX for editing Texinfo files.

Special commands:
\\{Texinfo-mode-map}

Entering Texinfo mode calls the value of `text-mode-hook'  and then the
value of `Texinfo-mode-hook'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-info" '("Texinfo-" "texinfo-environment-regexp")))

;;;***

;;;### (autoloads nil "tex-ispell" "tex-ispell.el" (0 0 0 0))
;;; Generated autoloads from tex-ispell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-ispell" '("TeX-ispell-")))

;;;***

;;;### (autoloads nil "tex-jp" "tex-jp.el" (0 0 0 0))
;;; Generated autoloads from tex-jp.el

(autoload 'japanese-plain-tex-mode "tex-jp" "\
Major mode in AUCTeX for editing Japanese plain TeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-plain-tex-mode'.

\(fn)" t nil)

(autoload 'japanese-latex-mode "tex-jp" "\
Major mode in AUCTeX for editing Japanese LaTeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-latex-mode'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-jp" '("TeX-" "japanese-")))

;;;***

;;;### (autoloads nil "tex-site" "tex-site.el" (0 0 0 0))
;;; Generated autoloads from tex-site.el
 (require 'tex-site)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-site" '("preview-TeX-style-dir" "AUCTeX-" "TeX-")))

;;;***

;;;### (autoloads nil "tex-style" "tex-style.el" (0 0 0 0))
;;; Generated autoloads from tex-style.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-style" '("LaTeX-")))

;;;***

;;;### (autoloads nil "tex-wizard" "tex-wizard.el" (0 0 0 0))
;;; Generated autoloads from tex-wizard.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-wizard" '("TeX-wizard")))

;;;***

;;;### (autoloads nil "texmathp" "texmathp.el" (0 0 0 0))
;;; Generated autoloads from texmathp.el

(autoload 'texmathp "texmathp" "\
Determine if point is inside (La)TeX math mode.
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked.

\(fn)" t nil)

(autoload 'texmathp-match-switch "texmathp" "\
Search backward for any of the math switches.
Limit searched to BOUND.

\(fn BOUND)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "texmathp" '("texmathp-")))

;;;***

;;;### (autoloads nil "toolbar-x" "toolbar-x.el" (0 0 0 0))
;;; Generated autoloads from toolbar-x.el
 (autoload 'toolbarx-install-toolbar "toolbar-x")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "toolbar-x" '("toolbarx-")))

;;;***

;;;### (autoloads nil nil ("auctex-pkg.el" "auctex.el" "tex-mik.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auctex-autoloads.el ends here
