;;; counsel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "counsel" "counsel.el" (0 0 0 0))
;;; Generated autoloads from counsel.el

(autoload 'counsel-el "counsel" "\
Elisp completion at point.

\(fn)" t nil)

(autoload 'counsel-cl "counsel" "\
Common Lisp completion at point.

\(fn)" t nil)

(autoload 'counsel-clj "counsel" "\
Clojure completion at point.

\(fn)" t nil)

(autoload 'counsel-company "counsel" "\
Complete using `company-candidates'.

\(fn)" t nil)

(autoload 'counsel-irony "counsel" "\
Inline C/C++ completion using Irony.

\(fn)" t nil)

(autoload 'counsel-describe-variable "counsel" "\
Forward to `describe-variable'.

Variables declared using `defcustom' are highlighted according to
`ivy-highlight-face'.

\(fn)" t nil)

(autoload 'counsel-describe-function "counsel" "\
Forward to `describe-function'.

Interactive functions (i.e., commands) are highlighted according
to `ivy-highlight-face'.

\(fn)" t nil)

(defface counsel-variable-documentation '((t :inherit font-lock-comment-face)) "\
Face for displaying Lisp documentation." :group (quote ivy-faces))

(autoload 'counsel-apropos "counsel" "\
Show all matching symbols.
See `apropos' for further information on what is considered
a symbol and how to search for them.

\(fn)" t nil)

(autoload 'counsel-info-lookup-symbol "counsel" "\
Forward SYMBOL to `info-lookup-symbol' with ivy completion.
With prefix arg MODE a query for the symbol help mode is offered.

\(fn SYMBOL &optional MODE)" t nil)

(autoload 'counsel-M-x "counsel" "\
Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.
This function integrates with either the `amx' or `smex' package
when available, in that order of precedence.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-load-library "counsel" "\
Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'.

\(fn)" t nil)

(autoload 'counsel-find-library "counsel" "\
Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'.

\(fn)" t nil)

(autoload 'counsel-load-theme "counsel" "\
Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'.

\(fn)" t nil)

(autoload 'counsel-descbinds "counsel" "\
Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one.

\(fn &optional PREFIX BUFFER)" t nil)

(autoload 'counsel-faces "counsel" "\
Complete faces with preview.
Actions are provided by default for describing or customizing the
selected face.

\(fn)" t nil)

(autoload 'counsel-git "counsel" "\
Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-git-grep "counsel" "\
Grep for a string in the current git repository.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional CMD INITIAL-INPUT)" t nil)

(autoload 'counsel-git-stash "counsel" "\
Search through all available git stashes.

\(fn)" t nil)

(autoload 'counsel-git-change-worktree "counsel" "\
Find the file corresponding to the current buffer on a different worktree.

\(fn)" t nil)

(autoload 'counsel-git-checkout "counsel" "\
Call the \"git checkout\" command.

\(fn)" t nil)

(autoload 'counsel-git-log "counsel" "\
Call the \"git log --grep\" shell command.

\(fn)" t nil)

(autoload 'counsel-find-file "counsel" "\
Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-recentf "counsel" "\
Find a file on `recentf-list'.

\(fn)" t nil)

(autoload 'counsel-bookmark "counsel" "\
Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist.

\(fn)" t nil)

(autoload 'counsel-bookmarked-directory "counsel" "\
Ivy interface for bookmarked directories.

With a prefix argument, this command creates a new bookmark which points to the
current value of `default-directory'.

\(fn)" t nil)

(autoload 'counsel-file-register "counsel" "\
Search file in register.

You cannot use Emacs' normal register commands to create file
registers.  Instead you must use the `set-register' function like
so: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you
can use `C-x r j i' to open that file.

\(fn)" t nil)

(autoload 'counsel-locate "counsel" "\
Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-fzf "counsel" "\
Open a file using the fzf shell command.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY FZF-PROMPT)" t nil)

(autoload 'counsel-dpkg "counsel" "\
Call the \"dpkg\" shell command.

\(fn)" t nil)

(autoload 'counsel-rpm "counsel" "\
Call the \"rpm\" shell command.

\(fn)" t nil)

(autoload 'counsel-file-jump "counsel" "\
Jump to a file below the current directory.
List all files within the current directory or any of its subdirectories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-dired-jump "counsel" "\
Jump to a directory (in dired) below the current directory.
List all subdirectories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-ag "counsel" "\
Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-AG-ARGS AG-PROMPT)" t nil)

(autoload 'counsel-pt "counsel" "\
Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-pt-base-command' instead of
`counsel-ag-base-command'.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-ack "counsel" "\
Grep for a string in the current directory using ack.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-ack-base-command' replacing
`counsel-ag-base-command'.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-rg "counsel" "\
Grep for a string in the current directory using rg.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-RG-ARGS RG-PROMPT)" t nil)

(autoload 'counsel-grep "counsel" "\
Grep for a string in the file visited by the current buffer.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-grep-or-swiper "counsel" "\
Call `swiper' for small buffers and `counsel-grep' for large ones.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel--org-get-tags "counsel" "\


\(fn)" nil nil)

(autoload 'counsel-org-tag-agenda "counsel" "\
Set tags for the current agenda item.

\(fn)" t nil)

(defalias 'counsel-org-goto #'counsel-outline)

(autoload 'counsel-org-goto-all "counsel" "\
Go to a different location in any org file.

\(fn)" t nil)

(autoload 'counsel-org-file "counsel" "\
Browse all attachments for current Org file.

\(fn)" t nil)

(autoload 'counsel-org-entity "counsel" "\
Complete Org entities using Ivy.

\(fn)" t nil)

(autoload 'counsel-org-capture "counsel" "\
Capture something.

\(fn)" t nil)

(autoload 'counsel-org-agenda-headlines "counsel" "\
Choose from headers of `org-mode' files in the agenda.

\(fn)" t nil)

(autoload 'counsel-tmm "counsel" "\
Text-mode emulation of looking and choosing from a menubar.

\(fn)" t nil)

(autoload 'counsel-yank-pop "counsel" "\
Ivy replacement for `yank-pop'.
With a plain prefix argument (\\[universal-argument]),
temporarily toggle the value of `counsel-yank-pop-after-point'.
Any other value of ARG has the same meaning as in `yank-pop', but
`counsel-yank-pop-preselect-last' determines its default value.
See also `counsel-yank-pop-filter' for how to filter candidates.

Note: Duplicate elements of `kill-ring' are always deleted.

\(fn &optional ARG)" t nil)

(autoload 'counsel-imenu "counsel" "\
Jump to a buffer position indexed by imenu.

\(fn)" t nil)

(autoload 'counsel-list-processes "counsel" "\
Offer completion for `process-list'.
The default action deletes the selected process.
An extra action allows to switch to the process buffer.

\(fn)" t nil)

(autoload 'counsel-expression-history "counsel" "\
Select an element of `read-expression-history'.
And insert it into the minibuffer.  Useful during `eval-expression'.

\(fn)" t nil)

(make-obsolete 'counsel-expression-history 'counsel-minibuffer-history '"0.10.0 <2017-11-13 Mon>")

(autoload 'counsel-shell-command-history "counsel" "\
Browse shell command history.

\(fn)" t nil)

(make-obsolete 'counsel-shell-command-history 'counsel-minibuffer-history '"0.10.0 <2017-11-13 Mon>")

(autoload 'counsel-minibuffer-history "counsel" "\
Browse minibuffer history.

\(fn)" t nil)

(autoload 'counsel-esh-history "counsel" "\
Browse Eshell history.

\(fn)" t nil)

(autoload 'counsel-shell-history "counsel" "\
Browse shell history.

\(fn)" t nil)

(autoload 'counsel-outline "counsel" "\
Jump to an outline heading with completion.

\(fn)" t nil)

(autoload 'counsel-ibuffer "counsel" "\
Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\").

\(fn &optional NAME)" t nil)

(autoload 'counsel-switch-to-shell-buffer "counsel" "\
Switch to a shell buffer, or create one.

\(fn)" t nil)

(autoload 'counsel-unicode-char "counsel" "\
Insert COUNT copies of a Unicode character at point.
COUNT defaults to 1.

\(fn &optional COUNT)" t nil)

(autoload 'counsel-colors-emacs "counsel" "\
Show a list of all supported colors for a particular frame.

You can insert or kill the name or hexadecimal RGB value of the
selected color.

\(fn)" t nil)

(autoload 'counsel-colors-web "counsel" "\
Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or hexadecimal RGB value of the
selected color.

\(fn)" t nil)

(autoload 'counsel-rhythmbox "counsel" "\
Choose a song from the Rhythmbox library to play or enqueue.

\(fn &optional ARG)" t nil)

(autoload 'counsel-linux-app "counsel" "\
Launch a Linux desktop application, similar to Alt-<F2>.
When ARG is non-nil, ignore NoDisplay property in *.desktop files.

\(fn &optional ARG)" t nil)

(defvar counsel-mode nil "\
Non-nil if Counsel mode is enabled.
See the `counsel-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `counsel-mode'.")

(custom-autoload 'counsel-mode "counsel" nil)

(autoload 'counsel-mode "counsel" "\
Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements.

Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel" '("counsel-" "tmm-km-list" "ivy-function-called-at-point")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-autoloads.el ends here
