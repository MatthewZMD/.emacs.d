;;; ess-custom.el --- Customize variables for ESS  -*- lexical-binding: t; -*-

;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2015 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: A.J. Rossini <blindglobe@gmail.com>
;; Created: 05 June 2000
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/


;;; Commentary:
;; This file holds defcustoms for ESS. It is required by ess-utils.el,
;; which in turn is required by almost every other ESS file.

;;; Code:
(require 'comint)

;; FIXME:  When Emacs is started from Cygwin shell in Windows,
;;         we have (equal window-system 'x) -and should use "--ess" in *d-r.el
(defvar ess-microsoft-p (memq system-type '(ms-dos windows-nt))
  "Value is t if the OS is one of Microsoft's, nil otherwise.")

;; Customization Groups

(defgroup ess nil
  "ESS: Emacs Speaks Statistics."
  :group 'languages
  :link '(info-link "(ESS)")
  :link '(url-link "https://ess.r-project.org/"))

(defgroup ess-edit nil
  "ESS: editing behavior, including comments/indentation."
  :group 'ess
  :prefix "ess-")

(defgroup ess-proc nil
  "ESS: process control."
  :group 'ess
  :prefix "ess-")

(defgroup ess-command nil
  "ESS: Commands for various things."
  :group 'ess
  :prefix "ess-")

(defgroup ess-help nil
  "ESS: help functions."
  :group 'ess
  :prefix "ess-")

(defgroup ess-hooks nil
  "ESS: hooks for customization."
  :group 'ess
  :prefix "ess-")

(defgroup ess-S nil
  "ESS: S Languages."
  :group 'ess
  :prefix "ess-")

(defgroup ess-SPLUS nil
  "ESS: S-PLUS Dialect of S."
  :group 'ess-S
  :prefix "ess-")

(defgroup ess-R nil
  "ESS: R Dialect of S."
  :group 'ess-S
  :prefix "ess-")

(defgroup ess-Julia nil
  "ESS: Julia."
  :group 'ess
  :prefix "julia-")

(defgroup ess-sas nil
  "ESS: SAS."
  :group 'ess
  :prefix "ess-")

(defgroup ess-Stata nil
  "ESS: Stata."
  :group 'ess
  :prefix "ess-")

(defgroup ess-roxy nil
  "Mode for editing in-code Roxygen documentation."
  :group 'ess
  :group 'convenience
  :group 'ess-extras
  :prefix "ess-"
  :group 'tools)

(defgroup ess-extras nil
  "Extra utilities for ESS"
  :group 'ess
  :prefix "ess-")

(defgroup ess-faces nil
  "Faces and face options for ESS modes."
  :group 'ess
  :group 'faces
  :prefix "ess-")


 ; User changeable variables

;;*;; Options and Initialization

;;;###autoload
(defcustom ess-lisp-directory
  (file-name-directory
   (or load-file-name
       buffer-file-name))
  "Directory containing ess-site.el(c) and other ESS Lisp files."
  :group 'ess
  :type 'directory
  :package-version '(ess . "19.04"))

(defcustom ess-etc-directory
  ;; Try to detect the `etc' folder only if not already set up by distribution
  (let (dir)
    (dolist (d '("./etc/" "../../etc/ess/" "../etc/" "../etc/ess/"))
      (setq d (expand-file-name d ess-lisp-directory))
      (when (file-directory-p d)
        (setq dir d)))
    dir)
  "Location of the ESS etc/ directory.
The ESS etc directory stores various auxiliary files that are useful
for ESS, such as icons."
  :group 'ess
  :type 'directory
  :package-version '(ess . "19.04"))

;; Depending on how ESS is loaded the `load-path' might not contain
;; the `lisp' directory. For this reason we need to add it before we
;; start requiring ESS files
;;;###autoload
(add-to-list 'load-path ess-lisp-directory)
;; Add ess-lisp-directory/obsolete to load-path; files here will
;; automatically warn that they are obsolete when loaded.
;;;###autoload
(add-to-list 'load-path (file-name-as-directory (expand-file-name "obsolete" ess-lisp-directory)))

(unless (file-directory-p ess-etc-directory)
  (display-warning 'ess (format "Could not find directory `ess-etc-directory': %s"
                                ess-etc-directory) :error))

;; Menus and pulldowns.

(defcustom ess-imenu-use-p (fboundp 'imenu)
  "Non-nil means use imenu facility.
This value can be overridden by mode-specific variables, such
as `ess-imenu-use-S'."
  :group 'ess
  :type  'boolean)

(defcustom ess-imenu-use-S ess-imenu-use-p
  "Non-nil means include an Imenu menu item in S buffers."
  :group 'ess
  :type  'boolean)

(defcustom ess-auto-width-visible nil
  "When non-nil, echo width setting in the inferior buffer.
See `ess-auto-width'. Be warned that ESS can set the width a
lot."
  :group 'ess
  :type 'boolean
  :package-version '(ess . "19.04"))

(defcustom ess-auto-width nil
  "When non-nil, set the width option when the window configuration changes.
When 'frame, set the width to the frame width. When 'window, set
the width to the window width. If an integer, set the width to
that integer. Anything else is treated as 'window."
  :group 'ess
  :type '(choice (const :tag "Do nothing" :value nil)
                 (const :tag "Frame width" :value frame)
                 (const :tag "Window width" :value window)
                 (integer :tag "Integer value"))
  :package-version '(ess . "19.04"))

(defcustom ess-handy-commands '(("change-directory"     . ess-change-directory)
                                ("install.packages"     . ess-install-library)
                                ("library"              . ess-library)
                                ("objects[ls]"          . ess-execute-objects)
                                ("help-apropos"         . ess-display-help-apropos)
                                ("help-index"           . ess-display-package-index)
                                ("help-object"          . ess-display-help-on-object)
                                ("search"               . ess-execute-search)
                                ("set-width"            . ess-execute-screen-options)
                                ("setRepos"             . ess-setRepositories)
                                ("sos"                  . ess-sos)
                                ("vignettes"            . ess-display-vignettes)
                                )
  "An alist of custom ESS commands.
These are available for call by function `ess-handy-commands' and
`ess-smart-comma' function."
  :group 'ess
  :type 'alist)

(defvar-local ess--local-handy-commands nil
  "Store handy commands locally.")

(defcustom ess-describe-at-point-method nil
  "Whether `ess-describe-object-at-point' should use a tooltip.
If nil display in an electric buffer. If 'tooltip display in
a tooltip.

See also `tooltip-hide-delay' and variable `tooltip-delay'."
  :group 'ess-utils
  :type '(choice (const :tag "buffer" :value nil ) (const tooltip)))

(defvaralias
  'ess-R-describe-object-at-point-commands
  'ess-r-describe-object-at-point-commands)
(defcustom ess-r-describe-object-at-point-commands
  '(("str(%s)")
    (".ess_htsummary(%s, hlength = 20, tlength = 20)")
    ("summary(%s, maxsum = 20)"))
  "A list of commands cycled by `ess-describe-object-at-point'.
%s is substituted with the name at point.

The value of each element is nil and is not used in current
implementation."
  :group 'ess-R
  :type 'alist)

(defcustom ess-S-describe-object-at-point-commands
  ess-R-describe-object-at-point-commands
  "An alist of commands cycled by `ess-describe-object-at-point'.
%s is substitute with the name at point. The value is not used as
 yet."
  :group 'S+
  :type 'alist)


(defcustom ess-can-eval-in-background t
  "If non-nil ESS can perform caching and other background activities.
This allows ESS to call the subprocess on idle time."
  :group 'ess
  :type 'boolean)

(defcustom ess-user-full-name (user-full-name)
  "The full name of the user."
  :group 'ess
  :type 'string)

(defcustom ess-blink-region t
  "If t evaluated region is highlighted for a shortwhile.
See also `ess-blink-delay'"
  :group 'ess
  :type 'boolean)

(defcustom ess-blink-delay .3
  "Number of seconds to highlight the evaluated region."
  :group 'ess
  :type 'number)

(defcustom ess-ask-for-ess-directory t
  "Non-nil means request the process directory each time S is run."
  :group 'ess
  :type 'boolean)

(defcustom ess-ask-about-transfile nil
  "Non-nil means ask about a transcript file before running ESS."
  :group 'ess
  :type 'boolean)

(defcustom ess-display-buffer-reuse-frames t
  "Non-nil means \\[display-buffer] reuses existing frames.
See `display-buffer-reuse-frames'."
  :group 'ess
  :type 'boolean)

(defvar-local ess-language nil
  "Determines the language to use for the current buffer.
See also `ess-dialect'.")

(defvar-local ess-dialect nil
  "String version of the dialect being run for the inferior process.
This, plus `ess-language', should be able to determine the exact
version of the statistical package being executed in the particular
buffer.")

(defcustom ess-directory-function nil
  "Function to return the directory that ESS is run from.
If nil or if the function returns nil then you get `ess-startup-directory'."
  :group 'ess
  :type '(choice (const nil) function))

(defcustom ess-setup-directory-function nil
  "Function to setup the directory that ESS is run from.
This function can be called to set environment variables or to create
a workspace."
  :group 'ess
  :type '(choice (const nil) function))

(defvaralias 'ess-directory 'ess-startup-directory)
(defcustom ess-startup-directory nil
  "The directory ESS is run from.  It must end in a slash.
Provided as a default if `ess-ask-for-ess-directory' is non-nil.
A nil value means use the current buffer's default directory."
  :group 'ess
  :type '(choice (const nil) directory))


(defcustom ess-history-directory nil
  "Directory to pick up `ess-history-file' from.
If this is nil, the history file is relative to the startup
directory of the inferior process (see `ess-startup-directory')."
  :group 'ess
  :type '(choice (const nil) directory))

(defcustom ess-history-file t
  "File to pick up history from.  nil means *no* history is read or written.
t means something like \".Rhistory\".
If this is a relative file name, it is relative to `ess-history-directory'.
Consequently, if that is set explicitly, you will have one history file
for all projects."
  :group 'ess
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)
                 file))

(defcustom ess-plain-first-buffername t
  "When non-nil, the first process buffer created does not have a number.
In other words, it is R:foo rather than R:1:foo. Subsequent
processes buffers are always numbered (e.g. R:2:foo."
  :group 'ess
  :type 'boolean)


(define-obsolete-variable-alias
  'ess-use-inferior-program-name-in-buffer-name
  'ess-use-inferior-program-in-buffer-name "ESS 18.10")
(defcustom ess-use-inferior-program-in-buffer-name nil
  "For R, use e.g., 'R-2.1.0' or 'R-devel' (the program name) for buffer name.
Avoids the plain dialect name."
  :group 'ess
  :type 'boolean)

(defcustom  ess-use-ido t
  "If t ess will try to use ido completion whenever possible.

By default ESS uses enables IDO flex matching. See
`ido-enable-flex-matching' for details on flex matching and
`ess-ido-flex-matching' on how to disable it for ESS, if you
don't want it.

See info node `(ido) Top' for more information about how ido
works."
  :group 'ess
  :require 'ido
  :type 'boolean)


(defcustom ess-tab-complete-in-script nil
  "If non-nil, TAB tries to complete if it does not indent in script buffers.
See also `ess-first-tab-never-complete'."
  :group 'ess
  :type 'boolean)
(make-obsolete-variable 'ess-tab-complete-in-script 'tab-always-indent "ESS 19.04" 'set)

(define-obsolete-variable-alias 'ess-first-tab-never-completes-p
  'ess-first-tab-never-complete "ESS 19.04")
(defcustom ess-first-tab-never-complete 'symbol
  "If t, first TAB never tries to complete in `ess-mode'.
If 'symbol first TAB doesn't try to complete if next char is a
valid symbol constituent.

If 'symbol-or-paren  don't complete if next char is closed paren
)}] or symbol character.

If 'symbol-or-paren-or-punct don't complete if next char is
punctuation +-=% etc, or closed paren or symbol.

If 'unless-eol - first TAB completes only at end of line.

If nil first TAB always tries to complete (this might be too
aggressive and dangerous)."
  :group 'ess
  :type '(choice (const nil)
                 (const symbol)
                 (const symbol-or-paren)
                 (const symbol-or-paren-or-punct)
                 (const unless-eol)
                 (const t)))

(defcustom ess-use-eldoc t
  "If t, activate eldoc in `ess-mode' and `inferior-ess-mode' buffers.
If 'script-only activate in `ess-mode' buffers only.

See also `ess-eldoc-show-on-symbol'."
  :group 'ess-extras
  :type '(choice (const t) (const script-only) (const nil)))


(defcustom ess-eldoc-show-on-symbol nil
  "If non-nil, show help string whenever the point is on a symbol.
If nil show only when the point is in a function call, i.e. after (."
  :group 'ess-extras
  :type  'boolean)

(defcustom ess-eldoc-abbreviation-style 'normal
  "Controls how `eldoc' displays information that does not fit on a line.

A symbol which can be

- nil: do nothing
- mild: Replace TRUE, FALSE with T,F
- normal: Try mild + shorten the default values longer than 10
  characters.
- strong: Try normal + completely remove default values except
  =F,=T,=d where d is a digit.
- aggressive (or t): Try strong + truncate the doc string to fit
  into minibuffer.

The default style is 'normal.

Ess-eldoc also honors the value of
`eldoc-echo-area-use-multiline-p'. If this variable is not t (the
default), doc strings are truncated to fit into minibufer. This
allows the use of different abbreviation styles with the
truncation."
  :group 'ess
  :type '(choice (const nil) (const mild) (const normal) (const strong) (const aggressive) (const t)))

(defcustom ess-use-flymake t
  "If non-nil activate flymake in `ess-mode' buffers.
If 'process, only check if the buffer has an inferior process."
  :group 'ess
  :type '(choice (const :tag "Always" t)
                 (const :tag "With running inferior process" process)
                 (const :tag "Never" nil))
  :package-version '(ess . "18.10"))

(defcustom ess-use-auto-complete t
  "If non-nil, activate auto-complete support.
If t, activate auto-complete support in `ess-mode' and
`inferior-ess-mode' buffers. If 'script-only activate in `ess-mode'
buffers only.

If non-nil add `ac-source-R' and `ac-source-filename' to the
`ac-sources' buffer local variable.

ESS defines three AC sources `ac-source-R',`ac-source-R-objects'
and `ac-source-R-args'. See auto-complete package
documentation (http://cx4a.org/software/auto-complete/) for how
to install your custom sources."
  :group 'ess-extras
  :type '(choice (const t) (const script-only) (const nil)))
(make-obsolete-variable 'ess-use-auto-complete "Auto-complete is unmaintained; use company-mode instead"
                        "ESS 19.04" 'set)

(defcustom ess-use-company t
  "If t, activate company support in `ess-mode' and `inferior-ess-mode' buffers.
If non-nil add `company-R-args' and `company-R-objects' to the
`company-backends'. If 'script-only activate in `ess-mode' buffers
only."
  :group 'ess-extras
  :type '(choice (const t) (const script-only) (const nil)))

(defcustom ess-company-arg-prefix-length nil
  "Minimum prefix for ess company function argument completion."
  :group 'ess-extras
  :type '(choice (const :tag "Default" nil)
                 integer))

(defcustom ess-use-tracebug t
  "If t, load `ess-tracebug' when R process starts."
  :group 'ess-extras
  :type  'boolean)

(defcustom  ess-ido-flex-matching t
  "If t, ido for ESS completion uses flex matching.
See `ido-enable-flex-matching' for details.
If you have an old computer, or you load lot of packages, you
might want to set this to nil."
  :group 'ess
  :type 'boolean)

(defvar ess--completing-hist nil
  "Variable to store completion history.
Used by `ess-completion-read' command.")

(defvar-local ess-smart-operators ()
  "List of smart operators to be used in ESS and IESS modes.
Not to be set by users. It is redefined by mode specific
settings, such as `ess-r-smart-operators'.")

(defvaralias 'ess-R-smart-operators 'ess-r-smart-operators)
(defvar ess-r-smart-operators nil
  "If nil, don't use any of smart operators.
If t, use all. If an explicit list of operators, use only those
operators.

In current version of ESS, it controls the behavior of
`ess-smart-comma' only, but will be enriched in the near future.")


(defvar ess-no-skip-regexp "[ \t\n]*\\'"
  "If `ess-next-code-line' sees this line, it doesn't jump over.

Used to avoid annoying jumping by ess-eval.*-and-step to end of
buffer or end chunks etc.")

(make-obsolete-variable 'ess-smart-S-assign-key nil "ESS 18.10")

(defcustom ess-assign-list (cons (or (bound-and-true-p ess-S-assign) " <- ")
                                 '(" <<- " " = " " -> " " ->> "))
  "List of assignment operators.
`ess-cycle-assign' uses this list.  These strings must
contain spaces on either side."
  ;; Note that spaces on either side is not strictly true (as in the
  ;; function won't error), but matching <-/<<- is broken without
  ;; them.
  :type '(repeat string)
  :group 'ess
  :package-version '(ess "18.10"))
(defvar ess-S-assign)
(make-obsolete-variable 'ess-S-assign 'ess-assign-list "ESS 18.10")

(defcustom ess-r-prettify-symbols
  '(("<-" . (?\s (Br . Bl) ?\s (Bc . Bc) ?←))
    ("->" . (?\s (Br . Bl) ?\s (Bc . Bc) ?→))
    ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                   (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
                   (Bc . Bl) ?- (Br . Br) ?>))
    ("<<-" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                   (Bl . Bl) ?< (Bc . Br) ?- (Bc . Bc) ?-
                   (Bc . Bl) ?< (Br . Br) ?-)))
  ;; Setup prettify-symbols-alist to show "pretty" arrows, but make
  ;; sure that they arrows use the same amount of spacing as <- and
  ;; <<- to ensure indentation does not change when
  ;; prettify-symbols-mode is turned on/off.
  "Alist of symbols prettifications, see `prettify-symbols-alist'.
This gets appended to `prettify-symbols-alist', so set it to nil
if you want to disable R specific prettification."
  :group 'ess-R
  :type '(alist :key-type string :value-type symbol)
  :package-version '(ess . "18.10"))

;;*;; Variables concerning editing behavior

(defcustom ess-filenames-map t
  "If non-nil, filenames and objects are the same in an attached directory.
This is not true for DOS and other OS's with limited filename
lengths. Even if this is set incorrectly, the right things will
probably still happen, however."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-keep-dump-files t
  "Variable controlling whether to delete dump files after a successful load.
If nil: always delete.  If `ask', confirm to delete.  If `check', confirm
to delete, except for files created with `ess-dump-object-into-edit-buffer'.
Anything else, never delete.  This variable only affects the behavior
of `ess-load-file'.  Dump files are never deleted if an error occurs
during the load."
  :group 'ess-edit
  :type '(choice (const :tag "Check" :value  'check)
                 (const :tag "Ask"   :value  'ask)
                 (const :tag "Always keep"   :value t)
                 (const :tag "Always delete"   :value nil)))

(defcustom ess-delete-dump-files nil
  "Non-nil means delete dump files after they are created.
This applies to dump files created with
`ess-dump-object-into-edit-buffer', only.

Boolean flag which determines what to do with the dump files
generated by \\[ess-dump-object-into-edit-buffer], as follows:

        If non-nil: dump files are deleted after each use, and so appear
only transiently. The one exception to this is when a loading error
occurs, in which case the file is retained until the error is
corrected and the file re-loaded.

        If nil: dump files are not deleted, and backups are kept
as usual.  This provides a simple method for keeping an archive of S
functions in text-file form.

Auto-save is always enabled in dump-file buffers to enable recovery
from crashes.

This is useful to prevent source files being created for objects
you don't actually modify.  Once the buffer is modified and saved
however, the file is not subsequently deleted unless
`ess-keep-dump-files' is nil, and the file is successfully loaded
back into S."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-fill-calls t
  "If non-nil, refilling inside a call arranges arguments.
In other words, refilling a paragraph inside a function or
indexing call will arrange the arguments according to
`fill-column' as in:

  fun_call(argument1, argument2,
           argument3, argument4)


Refilling repeatedly cycles through different styles and
eventually to the original formatting.

 The second style formats according to one argument per line:

  fun_call(argument1,
           argument2,
           argument3,
           argument4)

When `ess-fill-calls-newlines' is t, the second style becomes:

  fun_call(
      argument1,
      argument2,
      argument3,
      argument4
  )


Setting `ess-offset-arguments' to `prev-line' or `prev-call'
activates a third style. It keeps one argument per line except
for the first N arguments. N is controlled with a prefix. For
example, calling \\[fill-paragraph] three times sets N to 1 while
calling \\[fill-paragraph] twice then \\[universal-argument] 2
\\[fill-paragraph] sets N to 2. Here what the default produces:

  fun_call(argument1,
      argument2,
      argument3,
      argument4,
      argument5
  )

This style is useful for refilling R6 or ggproto class
definitions.


The blinking of the refilled region can be disabled with
`ess-blink-refilling'."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-fill-continuations t
  "Controls filling of continuations.
If non-nil, refilling a paragraph inside a continuation of
statements (expressions separated by operators) will arrange all
its elements, never going past `fill-column'.

  lm(outcome ~ pred1 + pred2 +
       pred3 + pred4, data)

Refilling repeatedly cycles through different styles and
eventually to the original formatting.

The second style lay out the statements according to one
expression per line:

  lm(outcome ~
       pred1 +
       pred2 +
       pred3 +
       pred4, data)

The blinking of the refilled region can be disabled with
`ess-blink-refilling'."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-fill-calls-newlines nil
  "When non-nil, refilling may place newlines before and after delimiters.
When non-nil, the second refilling style produces newlines
after and before the opening and closing delimiters. This is
intended for example for dplyr-style code:

  fun_call(
      argument1,
      argument2,
      argument3,
      argument4
  )

Note that this setting is temporary and likely to be replaced in
a future ESS version by a more comprehensive and flexible way to
set refill styles."
  :group 'ess-edit
  :type 'boolean)

(defcustom ess-blink-refilling t
  "When non-nil, refilling blinks the filling region."
  :group 'ess-edit
  :type 'boolean)

(define-obsolete-variable-alias 'ess-mode-silently-save 'ess-save-silently "ESS 19.04")
(defcustom ess-save-silently 'auto
  "If non-nil, possibly save buffers without asking.
If t, save without asking. If 'auto, save without asking if
either `compilation-ask-about-save' or variable `auto-save-visited-mode'
is non-nil. Affects `ess-save-file'."
  :group 'ess-edit
  :type '(choice (const :tag "Do not save without asking." :value nil)
                 (const :tag "Use compilation-ask-about-save and auto-save-visited-mode."
                        :value auto)
                 (const :tag "Save without asking." :value t))
  :package-version '(ess . "19.04"))

;;*;; Variables controlling editing

;;;*;;; Edit buffer processing
(defcustom ess-function-template " <- function( )\n{\n\n}\n"
  "If non-nil, function template used when editing nonexistent objects.

The edit buffer will contain the object name in quotes, followed by
this string. Point will be placed after the first parenthesis or
bracket."
  :group 'ess-edit
  :type 'string)

;;;*;;; Indentation parameters

(define-obsolete-variable-alias 'ess-tab-always-indent 'tab-always-indent "ESS 19.04")

(define-obsolete-variable-alias 'ess-indent-level 'ess-indent-offset "15.09")
(defvar ess-indent-offset 2
  "Main indentation offset that is commonly inherited by other offsets.
See `ess-style-alist' for all available offsets.")
;;;###autoload
(put 'ess-indent-offset 'safe-local-variable #'numberp)


(defvar ess-offset-arguments 'open-delim
  "Indent for arguments of function calls or indexing brackets.
This variables has an effect only when the ( or [ are not
directly followed by a new line. See
`ess-offset-arguments-newline' for indentation after closing
newline.

When set to `open-delim', arguments are indented relative to the
opening parenthesis of the closest function call:

  object <- call(argument, other_call(argument,
                                      other_argument))


When set to `prev-call', arguments are indented relative to the
closest function call:

  object <- call(argument, other_call(argument,
                               other_argument))


When set to `prev-line', arguments are indented relative to the
preceding line:

  object <- call(argument, other_call(argument,
      other_argument))

This setting can also be set to a list containing the the offset
type and the offset size, such as `'(prev-call 2)'. Otherwise,
`ess-indent-offset' is used as a default. See `ess-style-alist'
for other offsets controlling indentation.")

(define-obsolete-variable-alias 'ess-arg-function-offset-new-line 'ess-offset-arguments-newline "15.09")
(defvar ess-offset-arguments-newline 'prev-call
  "Indent of arguments when ( or [ is followed by a new line.

When set to `open-delim', arguments on a new line are indented
relative to the opening parenthesis of the closest function call:

  object <- call(argument, other_call(
                                      argument,
                                      other_argument
                                      ))


When set to `prev-call', arguments on a new line are indented relative to
the closest function call:

  object <- call(argument, other_call(
                               argument,
                               other_argument
                           ))

You can control the details of indentation at `prev-call' with
`ess-indent-from-lhs' and `ess-indent-from-chain-start'.


When set to `prev-line', arguments on a new line are indented
relative to the preceding line:

  object <- call(argument, other_call(
      argument,
      other_argument
  ))

This setting can also be set to a list containing the the offset
type and the offset size, such as `'(prev-call 2)'. Otherwise,
`ess-indent-offset' is used as a default. See `ess-style-alist'
for other offsets controlling indentation.")

(defvar ess-offset-block 'prev-line
  "Controls indentation for blocks.
A block is usually declared with braces but a statement wrapped
in anonymous parentheses is also considered a block. This offset
can be either `prev-call', `prev-line' or `open-delim'.

When set to `open-delim', blocks are indented relative to the
opening parenthesis of the closest function call:

  call(argument, other_call(parameter = {
                                stuff
                            }, {
                                stuff
                            }))

  call(argument, lapply(data, function(x) {
                            body
                        }))


When set to `prev-call', blocks are indented relative to the
closest function call:

  call(argument, other_call(parameter = {
                     stuff
                 }, {
                     stuff
                 }))

  call(argument, lapply(data, function(x) {
                     body
                 }))

You can control the details of indentation at `prev-call' with
`ess-indent-from-lhs' and `ess-indent-from-chain-start'.


When set to `prev-line', blocks are indented relative to the
preceding line:

  call(argument, other_call(parameter = {
      stuff
  }, {
      stuff
  }))

  call(argument, lapply(data, function(x) {
      body
  }))

This setting can also be set to a list containing the the offset
type and the offset size, such as `'(prev-call 2)'. Otherwise,
`ess-indent-offset' is used as a default. See `ess-style-alist'
for other offsets controlling indentation.")

(define-obsolete-variable-alias 'ess-first-continued-statement-offset 'ess-offset-continued "15.09")
(define-obsolete-variable-alias 'ess-continued-statement-offset 'ess-offset-continued "15.09")
(defvar ess-offset-continued 'straight
  "This setting controls indentation of continued statements.
That is, consecutive statements separated by operators.

When set to 'straight, continued statements are indented as follows:

  object %>%
      some_function() %>%
      other_function()

When set to 'cascade:

  object %>%
      some_function() %>%
          other_function()

The 'straight and 'cascade settings are actually equivalent to
'(straight . t) and '(cascade . t), where t represents the
base indent size. More generally, you can supply '(straight . N)
to control the size of indentation.

See `ess-style-alist' for for an overview of ESS indentation.")

(defvar ess-align-nested-calls '("ifelse")
  "List of strings for which `ess-offset-arguments-newline' is ignored.
These calls will be vertically aligned instead. For example, if
`ifelse' is a member of the list, nested ifelse calls are
indented like this:

    object <- ifelse(condition1, out1,
              ifelse(condition2, out2, out3))

See `ess-style-alist' for for an overview of ESS indentation.")

(defvar ess-align-arguments-in-calls '("function[ \t]*(")
  "A list of refexes where `ess-offset-arguments' is ignored.
List of regexes specifying the calls where `ess-offset-arguments'
should have no effect on function declarations. The arguments of
those calls will be aligned from the opening parenthesis.

By default, function declarations are overridden. If for example
`ess-offset-arguments' is set to `prev-line', then function calls
are normally indented as in:

  some_function(argument1,
      argument2, argument3
  )

However, the parameters of function declarations will be
vertically aligned:

  fun <- function(argument1,
                  argument2
                  argument3) {
      body
  }

See `ess-style-alist' for further details.")

(defvar ess-align-continuations-in-calls t
  "Whether continuations inside calls are indented from the opening delimiter.
This produces the following indentation:

  10 + (1 + 2 +
        3 + 4)
  object[variable1 +
         variable2]

  if (test1 || test2 ||
      test3 || test4) {
      any(test5 &
          test6)
  }

instead of

  10 + (1 + 2 +
            3 + 4)
  object[variable1 +
             variable2]

  if (test1 || test2 ||
        test3 || test4) {
      any(test5 &
            test6)
  }

Definition operators (`<-', `=', `:=' and `~') still trigger an
indentation in all cases. Also, operators at top level and in
curly brackets are not affected by this setting and always induce
an offset:

  {
      var1 +
          var2
  }

See `ess-style-alist' for for an overview of ESS indentation.")

(defvar ess-align-blocks '(control-flow)
  "List of block types for which `ess-offset-blocks' is ignored.
The overridden blocks are vertically aligned. The list can
contain either or both of the symbols `control-flow' and
`fun-decl'.

With `control-flow', if, else for and while blocks will always be
aligned vertically. With `fun-decl', the body of a function
declaration will always be aligned with the call to
`function'.")

(define-obsolete-variable-alias 'ess-arg-function-offset 'ess-indent-from-lhs "15.09")
(defvar ess-indent-from-lhs '(arguments fun-decl-opening)
  "List of elements that are indented from the left side of an assignment.
The list accepts the symbol `arguments' and `fun-decl-opening'.

For arguments, this setting only has an effect for offsets set to
`prev-call'. When set, this indentation is produced:

  some_function(parameter = other_function(
                                argument
                            ))

  object <- some_function(
                argument1,
                argument2
            )

instead of:

  some_function(parameter = other_function(
                    argument
                ))

  object <- some_function(
      argument1,
      argument2
  )


`fun-decl-opening' refers to the opening curly following a function
declaration. Setting it produces:

  object <-
      function(argument)
  {
      body
  }

instead of:

  object <-
      function(argument)
      {
          body
      }

This is useful when (a) you have a long function name and want to
break a line after `<-' so that you have room to lay out the
arguments within `fill-column' characters; (b) you still want to
align the function body from the LHS to save horizontal space.

See `ess-style-alist' for for an overview of ESS indentation.")

(defvar ess-indent-from-chain-start t
  "When non-nil, chained calls are treated as if they were one call.
Indentation will start from the first one. This setting only has
an effect for offsets set to `prev-call' or block offsets set to
`opening-delim'.

If nil:

  some_function(other_function(
                    argument
                ))

If t:

  some_function(other_function(
      argument
  ))

See `ess-style-alist' for for an overview of ESS indentation.")

;;added rmh 2Nov97 at request of Terry Therneau
(define-obsolete-variable-alias 'ess-fancy-comments 'ess-indent-with-fancy-comments "15.09")
(defcustom ess-indent-with-fancy-comments t
  "Non-nil means distinguish between #, ##, and ### for indentation.
See `ess-style-alist' for for an overview of ESS indentation."
  :type 'boolean
  :group 'ess-edit)

;;;*;;; Editing styles

(defvar ess-style-alist
  `((BSD
     (ess-indent-offset                . 8)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . prev-call)
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (C++
     (ess-indent-offset                . 4)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . prev-call)
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . (arguments))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    ;; CLB added rmh 2Nov97 at request of Terry Therneau
    (CLB
     (ess-indent-offset                . ,(default-value 'ess-indent-offset))
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . (straight 4))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (GNU
     (ess-indent-offset                . ,(default-value 'ess-indent-offset))
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . (prev-call 4))
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (K&R
     (ess-indent-offset                . 5)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . prev-call)
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    ;; added ajr 17.Feb'04 to match "common R" use (== DEFAULT apart from  offset = 4)
    (RRR
     (ess-indent-offset                . 4)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (RRR+
     (ess-indent-offset                . 4)
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
     (ess-offset-block                 . open-delim)
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
     (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
     (ess-indent-from-lhs              . (arguments))
     (ess-indent-from-chain-start      . nil)
     (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments)))

    (RStudio
     (ess-indent-offset                . ,(default-value 'ess-indent-offset))
     (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
     (ess-offset-arguments-newline     . prev-line)
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . nil)
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . nil)
     (ess-align-blocks                 . nil)
     (ess-indent-from-lhs              . (arguments))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . nil))

    (RStudio-
     (ess-indent-offset                . ,(default-value 'ess-indent-offset))
     (ess-offset-arguments             . prev-line)
     (ess-offset-arguments-newline     . prev-line)
     (ess-offset-block                 . ,(default-value 'ess-offset-block))
     (ess-offset-continued             . ,(default-value 'ess-offset-continued))
     (ess-align-nested-calls           . nil)
     (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
     (ess-align-continuations-in-calls . nil)
     (ess-align-blocks                 . nil)
     (ess-indent-from-lhs              . (arguments))
     (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
     (ess-indent-with-fancy-comments   . nil))

    (DEFAULT
      (ess-indent-offset                . ,(default-value 'ess-indent-offset))
      (ess-offset-arguments             . ,(default-value 'ess-offset-arguments))
      (ess-offset-arguments-newline     . ,(default-value 'ess-offset-arguments-newline))
      (ess-offset-block                 . ,(default-value 'ess-offset-block))
      (ess-offset-continued             . ,(default-value 'ess-offset-continued))
      (ess-align-nested-calls           . ,(default-value 'ess-align-nested-calls))
      (ess-align-arguments-in-calls     . ,(default-value 'ess-align-arguments-in-calls))
      (ess-align-continuations-in-calls . ,(default-value 'ess-align-continuations-in-calls))
      (ess-align-blocks                 . ,(default-value 'ess-align-blocks))
      (ess-indent-from-lhs              . ,(default-value 'ess-indent-from-lhs))
      (ess-indent-from-chain-start      . ,(default-value 'ess-indent-from-chain-start))
      (ess-indent-with-fancy-comments   . ,(default-value 'ess-indent-with-fancy-comments))))

  "Predefined formatting styles for ESS code.
Use `ess-set-style' to apply a style in all R buffers. The values of
all styles except OWN are fixed. To change the value of variables
in the OWN group, customize the variable `ess-own-style-list'.
DEFAULT style picks default (aka global) values from ESS
indentation variables. In addition, ESS provides many indentation
styles, the most important being the RRR and the RStudio
variants.

RRR is the common R style that adheres closely to R internal
standards. RRR+ is the same except it also aligns blocks in
function calls with the opening delimiter, producing more
indentation. The C++ style (named like this for historical
reasons rather than any resemblance to existing C++ indentation
schemes) is halfway between these two styles and indent block
arguments from the start of the surrounding function's name.

The RStudio style closely mimics the indentation of the RStudio
editor. RStudio- is the same except it does not align arguments
in function calls, which corresponds to the settings of some
RStudio users.

ESS indentation is fully specified by the following offsets and
variables. See the documentation of these variables for examples.

Offsets:

 - `ess-indent-offset': main offset inherited by other settings

 - `ess-offset-arguments': offset type for function and bracket
   arguments

 - `ess-offset-arguments-newline': offset type of arguments
   when ( or [ is followed by a new line.

 - `ess-offset-block': offset type for brace and anonymous
   parenthesis blocks

 - `ess-offset-continued': offset type for continuation lines in
   multiline statements


Overrides (implies vertical alignment):

 - `ess-align-nested-calls': functions whose nested calls
   should be aligned.

 - `ess-align-arguments-in-calls': calls where
   `ess-offset-arguments' should be ignored

 - `ess-align-continuations-in-calls': whether to ignore
   `ess-offset-continued' in calls.

 - `ess-align-blocks': whether to ignore `ess-offset-blocks' for
   function declarations or control flow statements.


Control variables:

 - `ess-indent-from-lhs': whether to indent arguments from
   left-hand side of an assignment or parameter declaration.

 - `ess-indent-from-chain-start': whether to indent arguments from
   the first of several consecutive calls.

 - `ess-indent-with-fancy-comments': whether to indent #, ## and
   ### comments distinctly.")

(defun ess-add-style (key entries)
  "Add a new style to `ess-style-list'.
The new style has KEY and ENTRIES. Remove any existing entry with
the same KEY before adding the new one."
  (setq ess-style-alist (assq-delete-all key ess-style-alist))
  (add-to-list 'ess-style-alist (cons key entries)))

(defcustom ess-own-style-list (cdr (assoc 'RRR ess-style-alist))
  "Indentation variables for your own style.
Set `ess-style' to 'OWN to use these values. To change
these values, use the customize interface. See the documentation
of each variable for its meaning."
  :group 'ess-edit
  :type 'alist
  :initialize 'custom-initialize-set
  :set (lambda (symbol value)
         (set symbol value)
         (ess-add-style 'OWN value)))

;;;###autoload
(put 'ess-style 'safe-local-variable #'symbolp)

(define-obsolete-variable-alias 'ess-default-style 'ess-style "ESS 19.04")
(defcustom ess-style 'RRR
  "Current ESS indentation style, see `ess-style-alist' for more.
See the variable `ess-style-alist' for how these groups (RRR,
DEFAULT, GNU, BSD, ...) map onto different settings for
variables. OWN style is defined in `ess-own-style-list' and you
can customize it to your needs. DEFAULT style picks default (aka
global) values from ESS indentation variables.

Prefer `ess-set-style' to set the current style. This variable
has an effect if set before a buffer is visited (e.g. in your
Emacs initialization file) or as a file or directory local
variable (see Info node `(Emacs) File Variables'."
  :type '(choice (const OWN)
                 (const GNU)
                 (const BSD)
                 (const C++)
                 (const CLB)
                 (const K&R)
                 (const RRR)
                 (const RRR+)
                 (const RStudio)
                 (const RStudio-)
                 (const DEFAULT))
  :group 'ess-edit
  :safe #'symbolp)

;;*;; Variables controlling behavior of dump files

(defcustom ess-source-directory
  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") temporary-file-directory)
  "Directory in which to place dump files.
This can be a string (an absolute directory name ending in a slash) or
a lambda expression of no arguments which will return a suitable string
value.  The lambda expression is evaluated with the process buffer as the
current buffer.

This always dumps to a sub-directory (\".Src\") of the current ess
working directory (i.e. first elt of search list)."
  :group 'ess-edit
  :type 'directory
  :package-version '(ess . "18.10"))

(defvar ess-dump-filename-template nil
  "Internal. Initialized by dialects.")

(defcustom ess-dump-filename-template-proto (concat (user-login-name) ".%s.S")
  "Prototype template for filenames of dumped objects.
The ending `S' is replaced by the current \\[ess-suffix], to give
`ess-dump-filename-template' when an inferior ESS process starts.

By default, gives filenames like `user.foofun.S', so as not to clash with
other users if you are using a shared directory. Other alternatives:
\"%s.S\" ; Don't bother uniquifying if using your own directory(ies)
\"dumpdir\"; Always dump to a specific filename. This makes it impossible
to edit more than one object at a time, though.
 (make-temp-name \"scr.\") ; Another way to uniquify"
  ;; MM: The last 3-4 lines above suck (I don't understand them) -- FIXME --

  :group 'ess-edit
  :type 'string)

(defcustom ess-pre-run-hook nil
  "Hook to call before starting up ESS.
Good for setting up your directory."
  :group 'ess-hooks
  :type 'hook)

(defcustom ess-post-run-hook nil
  "Hook to call just after the ESS process starts up.
Good for evaluating ESS code."
  :group 'ess-hooks
  :type 'hook)

(defcustom ess-send-input-hook nil
  "Hook called just before line input is sent to the process."
  :group 'ess-hooks
  :type 'hook)

;; ---- ./ess-roxy.el : ------------

(defcustom ess-roxy-package "roxygen2"
  "The name of the R package to use for Roxygen."
  :group 'ess-roxy
  :type 'string)

(defcustom ess-roxy-tags-noparam '("export" "noRd")
  "The tags used in roxygen fields that can be used alone.
Used to decide highlighting and tag completion."
  :group 'ess-roxy
  :type '(repeat string))

(defcustom ess-roxy-tags-param '("author" "aliases" "concept" "details"
                                 "examples" "format" "keywords"
                                 "method" "exportMethod"
                                 "name" "note" "param"
                                 "include" "references" "return"
                                 "seealso" "source" "docType"
                                 "title" "TODO" "usage" "import"
                                 "exportClass" "exportPattern" "S3method"
                                 "inheritParams"
                                 "importFrom" "importClassesFrom"
                                 "importMethodsFrom" "useDynLib"
                                 "rawNamespace"
                                 "rdname" "section" "slot" "description"
                                 "md" "eval")
  "The tags used in roxygen fields that require a parameter.
Used to decide highlighting and tag completion."
  :group 'ess-roxy
  :type '(repeat string))

(defcustom ess-roxy-template-alist
  (list (cons "description"  ".. content for \\description{} (no empty lines) ..")
        (cons "details" ".. content for \\details{} ..")
        (cons "title" "")
        (cons "param"  "")
        (cons "return" "")
        (cons "author" ess-user-full-name))
  "The tags and defaults to insert when creating empty templates.
Param is a place holder for where to enter
parameters. Description and details do not use @ tags, but are
instead placed at the beginning of the entry (and should
therefore also be at the beginning of this template to give
syntactically correct roxygen entries)"
  :group 'ess-roxy
  :type '(alist :key-type string :value-type string))

(defcustom ess-roxy-fill-param-p nil
  "Non-nil causes parameter descriptions to be filled (word-wrapped) upon `ess-roxy-update-entry'."
  :group 'ess-roxy
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ess-roxy-hide-show-p nil
  "Non-nil means ess-roxy uses function `hs-minor-mode' for block hiding with TAB."
  :group 'ess-roxy
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ess-roxy-start-hidden-p nil
  "Non-nil means all blocks should be hidden from start."
  :group 'ess-roxy
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ess-roxy-str "##'"
  "Prefix string to insert before each line in new roxygen blocks.
In existing roxygen blocks, the prefix is taken from the line at
point"
  :group 'ess-roxy
  :type 'string)

(defvar ess-roxy-insert-prefix-on-newline t
  "When non-nil, `ess-roxy-newline-and-indent' makes newlines start with the roxy prefix.")

 ; System variables

;; SJE -- this should not be defcustom - user does not set it.
(defvaralias 'ess-current-process-name 'ess-local-process-name)
(defvar-local ess-local-process-name nil
  "The name of the ESS process associated with the current buffer.")
(put 'ess-local-process-name 'risky-local-variable t)
(put 'ess-local-process-name 'permanent-local t)

(defcustom ess-switch-to-end-of-proc-buffer t
  "If non-nil, `ess-switch-to-inferior-or-script-buffer' goes to the end of the process buffer."
  :group 'ess
  :type 'boolean)

(defcustom ess-gen-proc-buffer-name-function 'ess-gen-proc-buffer-name:project-or-simple
  "Function used for generation of the buffer name of the new ESS processes.
It should accept one argument PROC-NAME, a string specifying
internal process name (R, R:2, etc).

Provided default options are:

`ess-gen-proc-buffer-name:simple'
 *proc*

`ess-gen-proc-buffer-name:directory'
  *proc:dir*
`ess-gen-proc-buffer-name:abbr-long-directory'
  *proc:abbr-long-dir*
`ess-gen-proc-buffer-name:project-or-simple'
  *proc:project-root* or *proc*
`ess-gen-proc-buffer-name:project-or-directory'
  *proc:project-root* or *proc:dir*

Strategies based on projects default to built-in strategies if
there is no project root in the current directory."
  :group 'ess
  :type '(choice (const :tag "*proc*"     ess-gen-proc-buffer-name:simple)
                 (const :tag "*proc:dir*" ess-gen-proc-buffer-name:directory)
                 (const :tag "*proc:abbr-long-dir*" ess-gen-proc-buffer-name:abbr-long-directory)
                 (const :tag "*proc:project-root* or *proc*"     ess-gen-proc-buffer-name:project-or-simple)
                 (const :tag "*proc:project-root* or *proc:dir*" ess-gen-proc-buffer-name:project-or-directory)
                 function)
  :package-version '(ess . "19.04"))


(defcustom ess-kermit-command "gkermit -T"
  "Kermit command invoked by `ess-kermit-get' and `ess-kermit-send'."
  :group 'ess
  :type  'string)

(defcustom ess-kermit-prefix "#"
  "String files must begin with to use kermit file transfer."
  :group 'ess
  :type  'string)

(defcustom ess-kermit-remote-directory "."
  "Buffer local variable that designates remote directory of file."
  :group 'ess
  :type  'string)
(make-variable-buffer-local 'ess-kermit-remote-directory)

;;*;; Regular expressions

;; -- Note: Some variables not-to-customize moved to ./ess-mode.el :
;; ess-r-set-function-start

;; Fixme: the following is just for S dialects :
(defcustom ess-dumped-missing-re
  "\\(<-\nDumped\n\\'\\)\\|\\(<-\\(\\s \\|\n\\)*\\'\\)"
  "If a dumped object's buffer matches this re, then it is replaced by `ess-function-template'."
  :group 'ess
  :type 'regexp)

(defcustom ess-dump-error-re
  (if (string= ess-language "S") "\nDumped\n\\'"
    "[Ee]rror")
  "Regexp used to detect an error when loading a file."
  :group 'ess
  :type 'regexp)


 ; ess-inf: variables for inferior-ess.

(defvar inferior-ess-own-frame nil
  "Non-nil means that inferior ESS buffers should start in their own frame.
This variable is deprecated.
please add an entry to `display-buffer-alist' instead. See Info
node `(ess)Customizing startup'. For example:

\(add-to-list 'display-buffer-alist
             '(\"*R\"
  (display-buffer-reuse-window display-buffer-pop-up-frame)))")
(make-obsolete-variable 'inferior-ess-own-frame 'display-buffer-alist "ESS 19.04")
(make-obsolete-variable 'inferior-ess-frame-alist "It is ignored. Use `display-buffer-alist' and `pop-up-frame-alist' instead." "ESS 19.04")
(make-obsolete-variable 'inferior-ess-same-window 'display-buffer-alist "ESS 19.04")

(defcustom inferior-ess-jit-lock-chunk-size 10000
  "Default for (buffer local) `jit-lock-chunk-size' in inferior ESS buffers."
  :group 'ess-proc
  :type 'integer)


(defvaralias 'inferior-R-program 'inferior-ess-r-program)
(define-obsolete-variable-alias 'inferior-R-program-name
  'inferior-ess-r-program "ESS 18.10")
(define-obsolete-variable-alias 'inferior-ess-r-program-name
  'inferior-ess-r-program "ESS 18.10")
(defcustom inferior-ess-r-program (if ess-microsoft-p
                                      "Rterm"
                                    "R")
  "Program name for invoking an inferior ESS with \\[R]."
  :group 'ess-R
  :type '(choice (string) file))

(defcustom inferior-R-args ""
  "String of arguments used when starting R.
See also `ess-R-readline'."
  :group 'ess-R
  :type 'string)

(defcustom ess-R-readline nil
  "When non-nil, use readline in R.
nil indicates that \"--no-readline \" should be used as argument
when starting R. This may very slightly speedup interaction. On
the other hand, readline is necessary for expansion of
\"~username/\" in paths. Note that readline interprets
tabs (tabular characters) in R source files as asking for file
name completion. This can mess up evaluation completely."
  :group 'ess-R
  :type 'boolean)

(defcustom inferior-STA-start-args ""
  "String of switches used when starting Stata.
Also see `inferior-STA-program'."
  :group 'ess-Stata
  :type 'string)

(defvaralias 'inferior-R-objects-command 'inferior-ess-r-objects-command)
(defcustom inferior-ess-r-objects-command "print(objects(pos=%d, all.names=TRUE), max=1e6)\n"
  "Format string for R command to get a list of objects at position %d.
Used in e.g., \\[ess-execute-objects] or \\[ess-display-help-on-object]."
  :group 'ess-command
  :type 'string)

(defvar ess-getwd-command nil
  "Command string retrieving the working directory from the process.")

(defvar ess-setwd-command nil
  "Command string to set working directory.
Should contain a formatting %s to be replaced by a
path (as in 'setwd(%s)\\n'.")

(defcustom ess-program-files ;; 32 bit version
  (if (and ess-microsoft-p
           (fboundp 'w32-short-file-name))
      (if (getenv "ProgramW6432")
          (w32-short-file-name (getenv "ProgramFiles(x86)"));; always 32 on 64 bit OS
        (w32-short-file-name (getenv "ProgramFiles")))      ;; always 32 on 32 bit OS
    nil)
  "Safe (no embedded blanks) 8.3 name for 32-bit programs that works across internationalization."
  :group 'ess
  :type '(choice (string) (const nil)))

(defcustom ess-program-files-64 ;; 64 bit version
  (when (and ess-microsoft-p (fboundp 'w32-short-file-name) (getenv "ProgramW6432"))
    (w32-short-file-name (getenv "ProgramW6432")))
  "Safe (no embedded blanks) 8.3 name for 64-bit programs that works across internationalization."
  :group 'ess
  :type '(choice (string) (const nil)))

(defcustom ess-directory-containing-R nil
  "When non-nil, a directory containing R.
nil means the search for all occurrences of R on the machine will
use the default location of the R directory
 (inside \"c:/Program Files\" in English locale Windows systems).
Non-nil values mean use the specified location as the
directory in which \"R/\" is located.  For example, setting
`ess-directory-containing-R' to \"c:\" will tell ESS to search
for R versions with path names of the form \"c:/R/R-x.y.z\".

Currently only used when `ess-microsoft-p'.  If you change the
value of this variable, you need to restart Emacs for it to take
effect.  It also needs to be set before you load ess-site as its
value is used once only when ESS is loaded."
  :group 'ess
  :type '(choice (directory) (const nil)))

(defcustom ess-rterm-version-paths nil
  "Stores the full path file names of Rterm versions computed via \\[ess-find-rterm].
If you have versions of R in locations other than in
../../R-*/bin/Rterm.exe or ../../rw*/bin/Rterm.exe, relative to
the directory in the variable `exec-path' containing your default
location of Rterm, you will need to redefine this variable with a
`custom-set-variables' statement in your site-start.el or .emacs
file."
  :group 'ess-R
  :type '(repeat string))

(define-obsolete-variable-alias 'inferior-S3-program-name
  'inferior-S3-program "ESS 18.10")
(defcustom inferior-S3-program "/disk05/s/S"
  "Program name for invoking an inferior ESS with S3()."
  :group 'ess-S
  :type 'string)

(define-obsolete-variable-alias
  'inferior-S-elsewhere-program-name
  'inferior-S-elsewhere-program "ESS 18.10")
(defcustom inferior-S-elsewhere-program "sh"
  "Program name to invoke an inferior ESS with S on a different computer."
  :group 'ess-proc
  :type 'string)

(defvaralias 'S+6-dialect-name 'S+-dialect-name)
(defcustom S+-dialect-name "S+"
  "Name of 'dialect' for S-PLUS 6.x and later.
Easily changeable in a user's `.emacs'."
  :group 'ess-SPLUS
  :type 'string)

(define-obsolete-variable-alias 'inferior-S+-program-name
  'inferior-S+-program "ESS 18.10")
(defcustom inferior-S+-program
  (if ess-microsoft-p
      (concat ess-program-files "/TIBCO/splus82/cmd/Splus.exe")
    "Splus")
  "Program name to invoke S+.
On Unix/Linux, use the Splus executable.  On Windows, the default
value is correct for a default installation of S-Plus 8.1 and
with bash as the shell.  For any other version or location,
change this value in ess-site.el or site-start.el.  Use the 8.3
version of the path name.  Use double backslashes if you use the
msdos shell."
  :group 'ess-SPLUS
  :type '(choice (string) (file)))

(defvaralias 'inferior-S+6-start-args 'inferior-S+-start-args)
(defvaralias 'inferior-Splus-args 'inferior-S+-start-args)
(defcustom inferior-S+-start-args ""
  "String of arguments used when starting S.
These arguments are currently passed only to S+6 and higher."
  :group 'ess-SPLUS
  :type 'string)

(defcustom inferior-Splus-objects-command "objects(where=%d)\n"
  "Format string for R command to get a list of objects at position %d.
Used in e.g., \\[ess-execute-objects] or \\[ess-display-help-on-object]."
  :group 'ess-command
  :type 'string)

(defcustom ess-S-quit-kill-buffers-p nil
  "Controls whether S buffers should also be killed once a process is killed.
This is used only when an iESS process is killed using \\[ess-quit].
Possible values:
nil - do not kill any S buffers associated with the process.
t - kill S buffers associated with the process.
ask - ask the user whether the S buffers should be killed."
  :group 'ess-S
  :type '(choice (const nil) (const t) (const ask)))

(define-obsolete-variable-alias 'inferior-SAS-program-name
  'inferior-SAS-program "ESS 18.10")
(defcustom inferior-SAS-program "sas"
  "Program name for invoking an inferior ESS with SAS()."
  :group 'ess-sas
  :type '(choice (string) (file)))

(define-obsolete-variable-alias 'inferior-STA-program-name
  'inferior-STA-program "ESS 18.10")
(defcustom inferior-STA-program "stata"
  "Program name for invoking an inferior ESS with stata().
This is NOT Stata, because we need to call Stata with TERM=emacs in
order for it to work right.  And Emacs is too smart for it."
  :group 'ess-Stata
  :type '(choice (string) (file)))

(defvaralias 'R-editor 'ess-r-editor)
(defcustom ess-r-editor "emacsclient"
  "Editor called by R process with 'edit()' command."
  :group 'ess
  :type 'string)

(defvaralias 'R-pager 'ess-r-pager)
(defcustom ess-r-pager 'nil ; Usually nil is correct as ESS and page() cooperate.
  "Pager called by R process with 'page()' command."
  :group 'ess
  :type '(choice (const nil) string))

(defcustom S-editor "emacsclient"
  "Editor called by S process with 'edit()' command."
  :group 'ess
  :type 'string)

(defcustom S-pager
  (if ess-microsoft-p "emacsclientw.exe" "emacsclient")
  "Pager called by S process with 'page()' command."
  :group 'ess
  :type 'string)

(defvar-local ess-editor nil
  "Editor by which the process sends information to an Emacs buffer
for editing and then to be returned to the process.")

(defvar-local ess-pager nil
  "Pager by which the process sends information to an Emacs buffer.")

(defvar-local inferior-ess-language-start nil
  "Initialization commands sent to the ESS process.")

(define-obsolete-variable-alias 'inferior-ess-program-name
  'inferior-ess-program "ESS 18.10")
(defvar-local inferior-ess-program nil
  "Default program name for invoking `inferior-ess'.
The other variables ...-program should be changed, for the
corresponding program.")

(make-obsolete-variable 'inferior-ess-start-args
                        "Use the language specific variables like `inferior-R-args'"
                        "ESS 19.04")
(defvar inferior-ess-start-args ""
  "String of arguments passed to the ESS process.
If you wish to pass arguments to a process, see e.g. `inferior-R-args'.")

(defcustom inferior-ess-pager (if ess-microsoft-p "console" "cat")
  "Pager to use for reporting help files and similar things."
  :group 'ess-proc
  :type 'string)

(defvar-local inferior-ess-primary-prompt "> "
  "Regular expression used by `ess-mode' to detect the primary prompt.")

(defvar-local inferior-ess-secondary-prompt nil
  "Regular expression used by ess-mode to detect the secondary prompt.
This is issued by S to continue an incomplete expression.
Set to nil if language doesn't support secondary prompt.")

(defvar ess-traceback-command nil
  "Command to generate error traceback.")

;; Need this to recognize prompts of the form  + + + > > >
;; and "+ . + ", but not "Abcd. "
(defvar inferior-S-prompt "[]a-zA-Z0-9.[]*[>+] \\(?:[>+.] \\)*"
  "Regexp used in S and R inferior and transcript buffers for prompt navigation.
Must not be anchored to BOL.")

;;*;; Variables controlling interaction with the ESS process

(defcustom ess-execute-in-process-buffer nil
  "Non-nil means the ess-execute- commands output to the process buffer.
Otherwise, they get their own temporary buffer."
  :group 'ess-proc
  :type 'boolean)

(defcustom ess-eval-empty nil
  "Non-nil means `ess-eval-line*' will send empty lines to the ESS process."
  :group 'ess-proc
  :type 'boolean)

(defvaralias 'ess-eval-visibly-p 'ess-eval-visibly)

(defcustom ess-eval-visibly t
  "Non-nil means ess-eval- commands display commands in the process buffer.
If t, ESS waits after each line of the command for the process
output. This results in a nice sequence of input and output but
stalls Emacs on long output (like Sys.sleep(5) in R).

If 'nowait, ESS still shows the input commands, but don't wait
for the process. Thus all the output is printed after the input
lines.

If nil, ESS doesn't print input commands and doesn't wait for the process.

This variable also affect the evaluation of input code in
iESS. The effect is similar to the above. If t then ess waits for
the process output, otherwise not."
  :group 'ess-proc
  :type '(choice (const t) (const nowait) (const nil)))

(defcustom ess-eval-deactivate-mark (fboundp 'deactivate-mark); was nil till 2010-03-22
  "Non-nil means that after ess-eval- commands the mark is deactivated."
  :group 'ess-proc
  :type 'boolean)

(defcustom ess-use-R-completion t
  "Non-nil means use R-builtin completion mechanism when available."
  :group 'ess-proc
  :type 'boolean)

(defcustom ess-sleep-for-shell (if ess-microsoft-p 5 1)
  "Pause before sending output to the shell."
  :group 'ess-proc
  :type  'number)

 ; System variables
;; VS[06-04-2016]: fixme: move all inf vars into ess-inf.el.

;;*;; Variables relating to multiple processes

(defvar-local ess--mode-line-process-indicator '("" ess-local-process-name)
  "List of ESS mode-line indicators.
Local in process buffers and must start with a string. Changes of
this variable are automatically reflected in mode-lines of the
process and all associated with it buffers.

Each symbol must evaluate to one of the standard mode line
objects. See info node `(elisp)Mode Line Data').  Add a symbol
with `add-to-list' and remove with `delq'. Note that the symbols
which are part of this list should better have
'risky-local-variable property set to t, otherwise the text
properties are not displayed.

External utilities such as `ess-tracebug' and `ess-developer'
customize this variable to indicate changes in the process
status.
")
(put 'ess--mode-line-process-indicator 'risky-local-variable t)

(defvar-local ess--local-mode-line-process-indicator '("")
  "List of local process indicators.
See `ess--mode-line-process-indicator' for how to set it.

This is an internal variable used by tools like `ess-developer'
and `ess-tracebug'.")
(put 'ess--local-mode-line-process-indicator 'risky-local-variable t)

;;*;; Inferior ESS commands

(defvar-local ess-load-command "source('%s')\n"
  "Dialect specific format-string for building the ess command to load a file.

This format string should use %s to substitute a file name and should
result in an ESS expression that will command the inferior ESS to load
that file.")

(defvar-local ess-eval-command nil
  "Dialect specific format-string for building the command to evaluate a string.

It is usually faster to send a string to remote processes than a
file.  The latter involves Tramp and can be quite slow.  When
possible, a dialect should implement that command and use it
preferentially.

This format string should use %s as a placeholder for the string
to be evaluated and, optionally, %f for the file name to be
reported in the error references.

The resulting command should not echo code or print any
transitory output.  See also `ess-eval-visibly-command' and
`ess-eval-visibly-noecho-command'.")

(defvar-local ess-build-eval-message-function nil
  "Dialect-specific function for formatting an evaluation message.")

(defcustom inferior-ess-dump-command "dump(\"%s\",file=\"%s\")\n"
  "Format-string for building the ess command to dump an object into a file.
Use first %s to substitute an object name.
Use second %s to substitute the dump file name."
  :group 'ess-command
  :type 'string)

(defvar-local inferior-ess-help-command "help(\"%s\")\n"
  "Format-string for building the ESS command to ask for help on an object.
This format string should use %s to substitute an object name.")

(defcustom inferior-ess-r-help-command ".ess.help('%s')\n"
  "Format-string for building the R command to ask for help on an object.
This format string should use %s to substitute an object name.
If set, changes will take effect when next R session is started."
  :group 'ess-command
  :type 'string)

(defvar-local inferior-ess-exit-command "q()\n"
  "Format-string for building the ess command to exit.
This format string should use %s to substitute an object name.")

(defvar-local inferior-ess-search-list-command nil
  "`ess-language' command that prints out the search list;
i.e. the list of directories and (recursive) objects that `ess-language' uses
when it searches for objects.

Really set in <ess-lang>-customize-alist in ess[dl]-*.el")

(defcustom inferior-ess-safe-names-command
  "tryCatch(base::print(base::names(%s), max=1e6), error=function(e){})\n"
  "Format string for ESS command to extract names from an object *safely*.

%s is replaced by an \"object name\" -- usually a list or data
frame, but in R also e.g., 'package:stats'."
  :group 'ess-command
  :type 'string)

;;*;; Regular expressions
(defvar-local inferior-ess-prompt nil
  "The regular expression  used for recognizing prompts.

It is always used in transcript mode.  In inferior ess mode it is
used only if `comint-use-prompt-regexp' is t.

If not set in language's customize-alist it is constructed at run time
from `inferior-ess-primary-prompt' and `inferior-ess-secondary-prompt'.")


(defvar-local ess-change-sp-regexp ""
  "The regexp for matching the S/R/.. commands that change the search path.")

(defvar ess-S+-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|collection(\\|library(\\|module(\\|source(\\)"
  "The regexp for matching the S-plus commands that change the search path.")

(defvaralias 'ess-R-change-sp-regexp 'ess-r-change-sp-regexp)
(defvar ess-r-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|library(\\|require(\\|source(\\)"
  "The regexp for matching the R commands that change the search path.")

;;*;; Process-dependent variables

(defvar-local ess-sl-modtime-alist nil
  "Alist of modification times for all ess directories accessed this session.")

(defvar-local ess-prev-load-dir/file nil
  "This symbol saves the (directory . file) pair used in the last
`ess-load-file' command.  Used for determining the default in the next one.")

(defvar-local ess-object-list nil
  ;; This is a list of the currently known object names.  It is
  ;; current only for one command entry; it exists under the
  ;; assumption that the list of objects doesn't change while entering
  ;; a command.
  "Cache of object names.")

(defvar-local ess-help-topics-list nil
  ;; List of currently known help topics.
  "Cache of help topics.")

;;*;; Miscellaneous system variables

;; SJE: Wed 29 Dec 2004 - following 3 ess-object* variables can be removed
;; soon if no-one needs the completion code.
(defvar ess-object-name-db-file "ess-namedb"
  "File containing definitions for `ess-object-name-db'.")

(defvar ess-object-name-db-file-loaded '()
  "List of programs whose name-db file has been loaded.")

(defvar-local ess-object-name-db nil
  "Alist of lists of object names, with directory names as keys.
The file ess-namedb.el is loaded (if it exists) to define this variable.
See also function `ess-create-object-name-db'.")

;;;*;;; Font-lock support

;; "Reserved Words" -- part 1 --
(defvar ess-RS-constants
  '("TRUE" "FALSE" "NA" "NULL" "Inf" "NaN"))
(defvar ess-R-constants
  (append ess-RS-constants
          '("NA_integer_" "NA_real_" "NA_complex_" "NA_character_")))
(defvar ess-S-constants
  (append ess-RS-constants
          '("T" "F")))

(defvar ess-R-keywords
  '("if" "else" "repeat" "while" "function" "for" "in" "next" "break"
    "switch" "function" "return" "on.exit" "stop" ".Defunct" "tryCatch"
    "withRestarts" "invokeRestart"
    "recover" "browser")
  "Reserved words or special functions in the R language.")

(defvar ess-S-keywords
  (append ess-R-keywords '("terminate")))

;; only some of these keywords "look like functions but are not":
(defvar ess-S-non-functions
  '("if" "for" "function" "while"))

;; first the common ones
(define-obsolete-variable-alias 'ess-S-modifyiers 'ess-S-modifiers "18.10")
(defvar ess-S-modifiers
  '("library" "attach" "detach" "source" "module"
    "message" "warning"))

(define-obsolete-variable-alias 'ess-R-modifyiers 'ess-R-modifiers "18.10")
(defvar ess-R-modifiers
  '("library" "attach" "detach" "source" "require"
    "setwd" "options" "par" "load" "rm"
    "message" "warning"  ".Deprecated"
    "signalCondition" "withCallingHandlers"))

(defvar ess-R-message-prefixes
  '("Error:" "Error in"
    "Warning:" "Warning in"
    "Warning messages"))
(defvar ess-S-message-prefixes
  (append ess-R-message-prefixes
          '("Syntax error:" "Dumped")))

(defvar ess-R-assign-ops
  ;; don't want "=" here which is not only for assign
  '("<<-" "<-" "->" "->>"))
(defvar ess-S-assign-ops ess-R-assign-ops)

(defvar ess-R-function-name-regexp
  (concat "\\("      "\\sw+" "\\)"
          "[ \t]*"   "\\(<-\\)"
          "[ \t\n]*" "function\\b"))

(defvar ess-S-function-name-regexp
  ess-R-function-name-regexp)

(defvar ess-font-lock-keywords nil
  "A name of the dialect specific font-lock keywords in the current buffer.
See `ess-R-font-lock-keywords' for an example. This is an
internal variable.")

(defvar ess-fl-keyword:fun-calls
  (cons "\\(\\sw+\\)[\t ]*(" '(1 ess-function-call-face keep))
  "Font lock for function calls.")

(defvar ess-fl-keyword:numbers
  (cons "\\b\\.?[0-9]+[.eEL]?[0-9]*\\b" 'ess-numbers-face)
  "Font lock for numbers.")

(defvar ess-fl-keyword:delimiters
  (cons "\\s(\\|\\s)" 'ess-paren-face)
  "Font lock for parenthesis.")

(defvar ess-fl-keyword:=
  (cons "=" 'ess-paren-face)
  "Font lock for equal sign (=).")

(defvar ess-fl-keyword:operators
  (cons "[-=+></]+" 'ess-operator-face)
  "Operators.")

(defvar ess-S-fl-keyword:modifiers
  (cons (regexp-opt ess-S-modifiers 'words)
        'ess-modifiers-face)     ; modify search list or source (i.e. directives)
  "Font lock keyword R modifiers.")

(defvar ess-S-fl-keyword:fun-defs
  (cons ess-S-function-name-regexp
        '(1 font-lock-function-name-face t))
  "Font-lock function definitions keyword.")

(defvar ess-S-fl-keyword:keywords
  (cons (regexp-opt ess-S-keywords 'words) 'ess-keyword-face))

(defvar ess-S-fl-keyword:assign-ops
  (cons (regexp-opt ess-S-assign-ops) 'ess-assignment-face)
  "Font-lock assign operators.")

(defvar ess-S-fl-keyword:constants
  (cons (regexp-opt ess-S-constants 'words) 'ess-constant-face)
  "Font-lock constants keyword.")

(defcustom ess-S-font-lock-keywords
  '((ess-S-fl-keyword:modifiers . t)
    (ess-S-fl-keyword:fun-defs  . t)
    (ess-S-fl-keyword:keywords  . t)
    (ess-S-fl-keyword:assign-ops . t)
    (ess-S-fl-keyword:constants . t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=))
  "An alist of available font-lock keywords for the S mode.
The key of each cons cell is a name of the keyword. The value
should be t or nil to indicate if the keyword is activated by
default or not."
  :group 'ess-S
  :group 'ess-faces
  :type '(repeat (cons symbol boolean)))

(defvar ess-R-fl-keyword:modifiers
  '(eval . (cons (concat "\\(" (regexp-opt ess-R-modifiers 'words) "\\)\\s-*(")
                 '(1 ess-modifiers-face)))
  "Font-lock keyword R modifiers.
See `ess-R-modifiers' for the list of modifiers.")

(defvar ess-R-fl-keyword:fun-defs
  '(eval . (cons ess-R-function-name-regexp
                 '(1 font-lock-function-name-face nil)))
  "Font-lock keyword for function names in function definitions.
When this keyword is on, function names on the left hand side of
<- are highlighted with `font-lock-function-name-face'.")

(defvar ess-R-fl-keyword:keywords
  '(ess-r--find-fl-keyword . ess-keyword-face)
  "Font lock keyword for `ess-R-keywords'.")

(defvar ess-R-fl-keyword:assign-ops
  '(eval . (cons (regexp-opt ess-R-assign-ops) 'ess-assignment-face))
  "Font-lock assign operators.
See `ess-R-assign-ops' for the ops.")

(defvar ess-R-fl-keyword:constants
  '(eval . (cons (regexp-opt ess-R-constants 'words) 'ess-constant-face))
  "Font-lock constants keyword.
See `ess-R-constants' for the list of constants.")

(defvar ess-R-fl-keyword:F&T
  '("\\b[FT]\\b" . ess-constant-face)
  "Highlight T and F in addition to TRUE and FALSE in R.")

(defvar ess-R-fl-keyword:%op% nil
  "Highlight %op% operators.")

(defcustom ess-R-font-lock-keywords
  '((ess-R-fl-keyword:keywords   . t)
    (ess-R-fl-keyword:constants  . t)
    (ess-R-fl-keyword:modifiers  . t)
    (ess-R-fl-keyword:fun-defs   . t)
    (ess-R-fl-keyword:assign-ops . t)
    (ess-R-fl-keyword:%op%       . t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=)
    (ess-R-fl-keyword:F&T))
  "An alist of available font-lock keywords for the R mode.
The key of each cons cell is a name of the keyword. The value
should be t or nil to indicate if the keyword is active or not."
  :group 'ess-R
  :group 'ess-faces
  :type '(repeat (cons symbol boolean)))

(defvar ess-S-fl-keyword:prompt
  (cons (concat "^" inferior-S-prompt) 'comint-highlight-prompt)
  "Highlight prompts missed by comint.")

(defvar ess-fl-keyword:matrix-labels
  ;; also matches subsetting
  (cons "\\[,?[1-9][0-9]*,?\\]" 'ess-matrix-face)
  "Matrix and vector numeric labels.")

(defvar ess-R-fl-keyword:messages
  (cons (regexp-opt ess-R-message-prefixes 'enc-paren)
        'font-lock-warning-face)
  "Inferior-ess problems or errors.")

(defvaralias 'inferior-R-font-lock-keywords 'inferior-ess-r-font-lock-keywords)
(defcustom inferior-ess-r-font-lock-keywords
  '((ess-S-fl-keyword:prompt      . t)
    (ess-R-fl-keyword:keywords    . t)
    (ess-R-fl-keyword:constants   . t)
    (ess-R-fl-keyword:modifiers   . t)
    (ess-R-fl-keyword:messages    . t)
    (ess-R-fl-keyword:fun-defs    . t)
    (ess-R-fl-keyword:assign-ops  . t)
    (ess-fl-keyword:matrix-labels . t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=)
    (ess-R-fl-keyword:F&T))
  "Font-lock patterns used in `inferior-ess-r-mode' buffers.
The key of each cons cell is a name of the keyword.  The value
should be t or nil to indicate if the keyword is active or not."
  :group 'ess-R
  :group 'ess-faces
  :type '(repeat (cons symbol boolean)))

(defvar ess-S-fl-keyword:messages
  (cons (regexp-opt ess-S-message-prefixes 'enc-paren)
        'font-lock-warning-face)
  "Inferior-ess problems or errors.")

(defcustom inferior-S-font-lock-keywords
  '((ess-S-fl-keyword:prompt    . t)
    (ess-S-fl-keyword:messages  . t)
    (ess-S-fl-keyword:modifiers . t)
    (ess-S-fl-keyword:fun-defs  . t)
    (ess-S-fl-keyword:keywords  . t)
    (ess-S-fl-keyword:assign-ops        . t)
    (ess-S-fl-keyword:constants . t)
    (ess-fl-keyword:fun-calls)
    (ess-fl-keyword:numbers)
    (ess-fl-keyword:operators)
    (ess-fl-keyword:delimiters)
    (ess-fl-keyword:=))
  "Font-lock patterns used in inferior-S-mode buffers.
The key of each cons cell is a name of the keyword.  The value
should be t or nil to indicate if the keyword is active by
default."
  :group 'ess-S
  :group 'ess-faces
  :type '(repeat (cons symbol boolean)))


;;;*;;; ess-help variables

;; This will never need to be loaded independently of any of the other modules,
;; but they can all call it so we may as well put it here.

(defcustom ess-help-pop-to-buffer t
  "If non-nil ess-help buffers are given focus during the display.
If non-nil, `ess-display-help' uses `pop-to-buffer' to display
help, otherwise it uses `display-buffer', which does not select
the help window."
  :group 'ess-help
  :type 'boolean)

(defcustom ess-help-own-frame nil
  "Controls whether ESS help buffers should start in a different frame.

Possible values are:
   nil: Display help in current frame.
  'one: All help buffers are shown in one dedicated frame.
     t: Each help buffer gets its own frame.

If this is non-nil,`ess-help-reuse-window' is ignored. The
parameters of the own frame are stored in
`ess-help-frame-alist'."
  :group 'ess-help
  :type '(choice (const :tag "Display in current frame" nil)
                 (const :tag "Display in one frame" one)
                 (const :tag "Always display in a new frame" t)))

(defcustom ess-help-reuse-window t
  "If t, ESS tries to display new help buffers in the existing help window.
This variable is ignored if `ess-help-own-frame' is non-nil."
  :type 'boolean
  :group 'ess-help)

(defcustom ess-help-frame-alist default-frame-alist
  "Alist of frame parameters used to create help frames.
This defaults to `default-frame-alist' and is used only when
the variable `ess-help-own-frame' is non-nil."
  :group 'ess-help
  :type 'alist
  :package-version '(ess . "18.10"))

 ; Faces
;;;=====================================================

(defconst comint-highlight-prompt 'comint-highlight-prompt)

(defconst ess-function-call-face 'ess-function-call-face)
(defface ess-function-call-face
  '((default (:slant normal :inherit font-lock-function-name-face)))
  "Font Lock face used to highlight function calls in ess buffers."
  :group 'ess-faces)

(defconst ess-numbers-face 'ess-numbers-face)
(defface ess-numbers-face
  '((default (:slant normal :inherit font-lock-type-face)))
  "Font Lock face used to highlight numbers in ess-mode buffers."
  :group 'ess-faces)

(defconst ess-operator-face 'ess-operator-face)
(defface ess-operator-face
  '((default (:inherit font-lock-constant-face)))
  "Font Lock face for operators."
  :group 'ess-faces)

(defconst ess-%op%-face 'ess-%op%-face)
(defface ess-%op%-face
  '((default (:inherit ess-operator-face)))
  "Font Lock face used to highlight %op% operators in ess-mode buffers."
  :group 'ess-faces)

(defconst ess-assignment-face 'ess-assignment-face)
(defface ess-assignment-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight assignment operators."
  :group 'ess-faces)

(defconst ess-paren-face 'ess-paren-face)
(defface ess-paren-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight parentheses."
  :group 'ess-faces)

(defconst ess-operator-face 'ess-operator-face)
(defface ess-operator-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight operators."
  :group 'ess-faces)

(defconst ess-modifiers-face 'ess-modifiers-face)
(defface ess-modifiers-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight modifiers.
In `R-mode', for example, this includes \"library,\" \"attach,\"
and others. See `ess-R-modifiers'."
  :group 'ess-faces)

(defconst ess-constant-face 'ess-constant-face)
(defface ess-constant-face
  '((default (:inherit font-lock-type-face)))
  "Font lock face used to highlight constants.
In `ess-r-mode', for example, this includes TRUE, FALSE, Inf and
others. See `ess-R-constants'."
  :group 'ess-faces)

(defconst ess-matrix-face 'ess-matrix-face)
(defface ess-matrix-face
  '((default (:inherit font-lock-constant-face)))
  "Font lock face used to highlight row/column labels in matrices."
  :group 'ess-faces)

(defconst ess-keyword-face 'ess-keyword-face)
(defface ess-keyword-face
  '((default (:inherit font-lock-keyword-face)))
  "Font lock face used to highlight reserved keywords.
In `ess-r-mode', for example, this includes \"while,\" \"if/else\",
\"function,\" and others. See `ess-R-keywords'."
  :group 'ess-faces)

(defconst ess-r-control-flow-keyword-face 'ess-r-control-flow-keyword-face)
(defface ess-r-control-flow-keyword-face
  '((default (:inherit ess-keyword-face)))
  "Font lock face used to highlight control flow keywords.
In `R-mode', for example, this includes \"switch(),\"
\"tryCatch()\", and \"stop(),\". See
`ess-R-control-flow-keywords'. By default, these keywords are
highlighted with the same face as `ess-R-keywords'"
  :group 'ess-faces)

(defcustom ess-help-kill-bogus-buffers t
  "Non-nil means kill ESS help buffers immediately if they are \"bogus\"."
  :group 'ess-help
  :type 'boolean)

(defvar ess-execute-screen-options-command nil
  "Dialect specific command run by `ess-execute-screen-options'.")

(defvar-local ess-funargs-command  nil
  "Dialect specific command to return a list of function arguments.
See `ess-function-arguments' and .ess_funargs command in R and
S+ for details of the format that should be returned.")

 ; System variables
;;;=====================================================
;;; Users note: You will rarely have to change these
;;; variables.

;;*;; Variables relating to ess-help-mode

;;-- ess-help-S-.. and  ess-help-R-.. : in  ess-s-lang.el (are used in ess-inf).

(defvar-local ess-help-sec-keys-alist nil
  "Alist of (key . string) pairs for use in section searching.")

(defvar-local ess-help-sec-regex nil
  "Reg(ular) Ex(pression) of section headers in help file.")

 ; julia-mode
(define-obsolete-variable-alias 'inferior-julia-program-name
  'inferior-julia-program "ESS 18.10")
(defcustom inferior-julia-program (or (executable-find "julia-basic")
                                      "julia")
  "Executable for Julia.
Should be an absolute path to the julia executable."
  :group 'ess-Julia
  :type '(choice (string) (file)))

;; FIXME
(defvar-local ess-mode-completion-syntax-table nil "Completion and help syntax table for `ess-mode'.")

 ; Buffer local customization stuff

(defcustom ess-error-buffer-name "*ESS-errors*"
  "Name of buffer to keep process error messages in.
Created for each process."
  :group 'ess-proc
  :type 'string)

(defvar ess-error-regexp-alist nil
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(defcustom ess-write-to-dribble t
  "Non-nil means write to `ess-dribble-buffer'.
See also `ess-verbose'."
  :group 'ess-proc
  :type 'boolean
  :package-version '(ess . "18.10"))

(defcustom ess-verbose nil
  "Non-nil means write more information to `ess-dribble-buffer' than usual."
  :group 'ess-proc
  :type 'boolean)

(defvar ess-dribble-buffer "*ESS*"
  "Buffer or name of buffer for printing debugging information.")

(defvar-local ess-local-customize-alist nil
  "Buffer local settings for proper behavior.
Used to store the values for passing on to newly created buffers.")

(defvar ess-mode-editing-alist nil
  "Variable settings for `ess-mode'.")

(defvar-local ess-transcript-minor-mode nil
  "Non-nil if using `ess-transcript-mode' as a minor mode of some other mode.")

(defvar-local ess-listing-minor-mode nil
  "Non-nil if using `ess-listing-minor-mode'.")

(defvar ess--enable-experimental-projects nil
  "Enable experimental project support in ESS.")

(defvar ess-STERM nil
  "Placeholder for dialect-specific STERM.")

(make-obsolete-variable 'ess-S-loop-timeout "It is ignored." "ESS 18.10")
(make-obsolete-variable 'ess-mode-load-hook "It is ignored." "ESS 18.10")
(make-obsolete-variable 'ess-speedbar-use-p "It is ignored." "ESS 18.10")
(make-obsolete-variable 'ess-synchronize-evals "It is ignored." "ESS 18.10")
(make-obsolete-variable 'ess-eval-visibly-at-end "It is ignored." "ESS 18.10")
(make-obsolete-variable 'ess-font-lock-mode 'global-font-lock-mode "ESS 18.10")
(provide 'ess-custom)

;;; ess-custom.el ends here
