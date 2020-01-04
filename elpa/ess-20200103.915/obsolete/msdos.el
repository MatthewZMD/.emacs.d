;;; msdos.el --- Run an MS-DOS shell in an NTemacs buffer with bash as the shell

;; Copyright (C) 1999 Richard M. Heiberger <rmh@temple.edu>
;; Copyright (C) 2000--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Richard M. Heiberger <rmh@temple.edu>
;; Created: February 1999
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: processes

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
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; The file msdos.el in the next mail message opens an *msdos* buffer
;; in shell-mode and msdos-minor-mode.  When cmdproxy.exe/command.com
;; is the emacs shell, then this gets various setting right that M-x
;; shell currently misses.  The function M-x msdos-minor-mode could be
;; automatically run by emacs in shell-mode in that case.

;; When bash is the emacs shell, msdos.el still opens a
;; cmdproxy.exe/command.com shell in the buffer *msdos*.  There are
;; occasions when it is necessary to run DOS character-based programs
;; in an emacs window.

;; I followed the suggestion by AndrewI to look at M-x shell and modify
;; it.  It turns out not to have been trivial.

;; I've listed it as part of ESS (emacs speaks statistics) for now.  I
;; will be happy to sign it over to FSF and have it become part of the
;; standard distribution for windows machines.

;;; Code:

(require 'shell); and hence 'comint

;;; Customization and Buffer Variables

(defcustom explicit-msdos-shell-file-name "cmdproxy.exe"
  "*If non-nil, is file name to use for explicitly requested msdos
inferior shell."
  :type '(choice (const :tag "None" nil) file)
  :group 'shell)

(defcustom explicit-msdos-comspec-file-name
  (if (w32-using-nt)
      "cmd.exe"
    "command.com")
  "*If non-nil, is file name to use for explicitly requested COMSPEC
environment variable."
  :type '(choice (const :tag "None" nil) file)
  :group 'shell)

(defvar-local msdos-minor-mode nil
  "Non-nil if using msdos-minor mode as a minor mode of some other mode.")

(defun msdos ()
  "Run an inferior msdos shell, with I/O through buffer *msdos*.
This function is intended to be used in an Ntemacs session in which
bash is the primary shell.  But sometimes an MSDOS window, within emacs,
is also needed.

If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, just switch to buffer `*msdos*'.
Program used comes from variable `explicit-msdos-shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The buffer is put into \\[msdos-minor-mode].  See `msdos-minor-mode'.

The COMSPEC environment variable in the inferior shell, but not in the emacs
process, is set to `explicit-msdos-comspec-file-name'.
The SHELL environment variable in the inferior shell, but not in the emacs
process, is set to `explicit-msdos-shell-file-name'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive)
  (if (not (comint-check-proc "*msdos*"))
      (let* ((prog explicit-msdos-shell-file-name)
             (name (file-name-nondirectory prog))
             (startfile (concat "~/.emacs_" name))
             (xargs-name (intern-soft (concat "explicit-" name "-args")))
             shell-buffer
             (comspec (getenv "COMSPEC"))
             (shell (getenv "SHELL"))
             )
        (save-excursion
          (setenv "COMSPEC" explicit-msdos-comspec-file-name)
          (setenv "SHELL" explicit-msdos-shell-file-name)
          (set-buffer (apply 'make-comint "msdos" prog
                             (if (and xargs-name (boundp xargs-name))
                                 (symbol-value xargs-name))
                             (if (file-exists-p startfile)
                                 (concat "/k " startfile))))
          (setenv "COMSPEC" comspec)
          (setenv "SHELL" shell)
          (setq shell-buffer (current-buffer))
          (shell-mode)
          (msdos-minor-mode)
          (sleep-for 4) ; need to wait, else working too fast!
                      ;;; The `exit' warning should precede the "c:\" prompt.
                      ;;; If not, then increase the sleep-for time!
          (goto-char (point-min))
          (insert
           "Remember to exit this buffer with `exit'.  If you kill the
buffer without exiting, you may not be able to shut down Windows cleanly.")
          (goto-char (point-max)))
        (pop-to-buffer shell-buffer))
    (pop-to-buffer "*msdos*")))


(defun msdos-minor-mode ()
  "Minor mode for running msdos in a shell-mode buffer:
a. Uses \\[set-buffer-process-coding-system] to set the coding system
to `'raw-text-dos'.  The DOS C-m C-l end-of-line is critical.  The
shell freezes without it.

b. The variable `comint-completion-addsuffix' is set to `\\' for directories.

c. Prevents echoing of commands.

d. strips ctrl-m from output.
"
  (interactive)
  (setq msdos-minor-mode t)
  (set (make-local-variable 'comint-completion-addsuffix) '("\\" . " "))
  (setq comint-process-echoes t)
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
  (set-process-coding-system (get-buffer-process (current-buffer)) 'raw-text-dos 'raw-text-dos)
  ;; buffer-process-coding-system is critical.
  )

;; Install ourselves:


(put 'msdos-minor-mode 'permanent-local t)
(or (assq 'msdos-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (append minor-mode-alist
                  (list '(msdos-minor-mode " msdos")))))

;; Provide ourselves:

(provide 'msdos)

;;; msdos.el ends here
