;;; reformatter.el --- Define commands which run reformatters on the current buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience, tools
;; Homepage: https://github.com/purcell/reformatter.el
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 20191103.357
;; Package-X-Original-Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library lets elisp authors easily define an idiomatic command
;; to reformat the current buffer using a command-line program,
;; together with an optional minor mode which can apply this command
;; automatically on save.

;; In its initial release it supports only reformatters which read
;; from stdin and write to stdout, but a more versatile interface will
;; be provided as development continues.

;; As an example, let's define a reformat command that applies the
;; "dhall format" command.  We'll assume here that we've already defined a
;; variable `dhall-command' which holds the string name or path of the
;; dhall executable:

;;     (reformatter-define dhall-format
;;       :program dhall-command
;;       :args '("format"))

;; The `reformatter-define' macro expands to code which generates
;; `dhall-format-buffer' and `dhall-format-region' interactive
;; commands, and a local minor mode called
;; `dhall-format-on-save-mode'.  The :args" and :program expressions
;; will be evaluated at runtime, so they can refer to variables that
;; may (later) have a buffer-local value.  A custom variable will be
;; generated for the mode lighter, with the supplied value becoming
;; the default.

;; The generated minor mode allows idiomatic per-directory or per-file
;; customisation, via the "modes" support baked into Emacs' file-local
;; and directory-local variables mechanisms.  For example, users of
;; the above example might add the following to a project-specific
;; .dir-locals.el file:

;;     ((dhall-mode
;;       (mode . dhall-format-on-save)))

;; See the documentation for `reformatter-define', which provides a
;; number of options for customising the generated code.

;; Library authors might like to provide autoloads for the generated
;; code, e.g.:

;;     ;;;###autoload (autoload 'dhall-format-buffer "current-file" nil t)
;;     ;;;###autoload (autoload 'dhall-format-region "current-file" nil t)
;;     ;;;###autoload (autoload 'dhall-format-on-save-mode "current-file" nil t)

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'ansi-color)

;;;###autoload
(cl-defmacro reformatter-define (name &key program args (mode t) lighter keymap group)
  "Define a reformatter command with NAME.

When called, the reformatter will use PROGRAM and any ARGS to
reformat the current buffer.  The contents of the buffer will be
passed as standard input to the reformatter, which should output
them to standard output.  A nonzero exit code will be reported as
failure, and the output of the command to standard error will be
displayed to the user.

The macro accepts the following keyword arguments:

:program (required)

  Provides a form which should evaluate to a string at runtime,
  e.g. a literal string, or the name of a variable which holds
  the program path.

:args

  If provided, this is a form which evaluates to a list of
  strings at runtime.  Default is the empty list.

:mode

  Unless nil, also generate a minor mode that will call the
  reformatter command from `before-save-hook' when enabled.
  Default is t.

:group

  If provided, this is the custom group used for any generated
  modes or custom variables.  Don't forget to declare this group
  using a `defgroup' form.

:lighter

  If provided, this is a mode lighter string which will be used
  for the \"-on-save\" minor mode.  It should have a leading
  space.  The supplied value will be used as the default for a
  generated custom variable which specifies the mode lighter.
  Default is nil, ie. no lighter.

:keymap

  If provided, this is the symbol name of the \"-on-save\" mode's
  keymap, which you must declare yourself.  Default is no keymap.
"
  (declare (indent defun))
  (cl-assert (symbolp name))
  (cl-assert program)
  ;; Note: we skip using `gensym' here because the macro arguments are only
  ;; referred to once below, but this may have to change later.
  (let* ((buffer-fn-name (intern (format "%s-buffer" name)))
         (region-fn-name (intern (format "%s-region" name)))
         (minor-mode-form
          (when mode
            (let ((on-save-mode-name (intern (format "%s-on-save-mode" name)))
                  (lighter-name (intern (format "%s-on-save-mode-lighter" name))))
              `(progn
                 (defcustom ,lighter-name ,lighter
                   ,(format "Mode lighter for `%s'." on-save-mode-name)
                   :group ,group
                   :type 'string)
                 (define-minor-mode ,on-save-mode-name
                   ,(format "When enabled, call `%s' when this buffer is saved.

To enable this unconditionally in a major mode, add this mode
to the major mode's hook.  To enable it in specific files or directories,
use the local variables \"mode\" mechanism, e.g. in \".dir-locals.el\" you
might use:

     ((some-major-mode
        (mode . %s-on-save)))
 " buffer-fn-name name) nil
                   :global nil
                   :lighter ,lighter-name
                   :keymap ,keymap
                   :group ,group
                   (if ,on-save-mode-name
                       (add-hook 'before-save-hook ',buffer-fn-name nil t)
                     (remove-hook 'before-save-hook ',buffer-fn-name t))))))))
    `(progn
       (defun ,region-fn-name (beg end &optional display-errors)
         "Reformats the region from BEG to END.
When called interactively, or with prefix argument
DISPLAY-ERRORS, shows a buffer if the formatting fails."
         (interactive "rp")
         (let* ((err-file (make-temp-file ,(symbol-name name)))
                (out-file (make-temp-file ,(symbol-name name)))
                ;; Setting this coding system might not universally be
                ;; the best default, but was apparently necessary for
                ;; some hand-rolled reformatter functions that this
                ;; library was written to replace.
                (coding-system-for-read 'utf-8)
                (coding-system-for-write 'utf-8))
           (unwind-protect
               (let* ((error-buffer (get-buffer-create ,(format "*%s errors*" name)))
                      (retcode
                       (apply 'call-process-region beg end ,program
                              nil (list (list :file out-file) err-file)
                              nil
                              ,args)))
                 (with-current-buffer error-buffer
                   (let ((inhibit-read-only t))
                     (insert-file-contents err-file nil nil nil t)
                     (ansi-color-apply-on-region (point-min) (point-max)))
                   (special-mode))
                 (if (zerop retcode)
                     (save-restriction
                       ;; This replacement method minimises
                       ;; disruption to marker positions and the
                       ;; undo list
                       (narrow-to-region beg end)
                       (reformatter-replace-buffer-contents-from-file out-file)
                       ;; In future this might be made optional, or a user-provided
                       ;; ":after" form could be inserted for execution
                       (delete-trailing-whitespace))
                   (if display-errors
                       (display-buffer error-buffer)
                     (message ,(concat (symbol-name name) " failed: see %s") (buffer-name error-buffer)))))
             (delete-file err-file)
             (delete-file out-file))))

       (defun ,buffer-fn-name (&optional display-errors)
         "Reformats the current buffer.
When called interactively, or with prefix argument
DISPLAY-ERRORS, shows a buffer if the formatting fails."
         (interactive "p")
         (message "Formatting buffer")
         (,region-fn-name (point-min) (point-max) display-errors))

       ;; This alias will be removed in a future version
       (defalias ',name ',buffer-fn-name)

       ,minor-mode-form)))

(defun reformatter-replace-buffer-contents-from-file (file)
  "Replace the accessible portion of the current buffer with the contents of FILE."
  ;; While the function `replace-buffer-contents' exists in recent
  ;; Emacs versions, it exhibits pathologically slow behaviour in many
  ;; cases, and the simple replacement approach we use instead is well
  ;; proven and typically preserves point and markers to a reasonable
  ;; degree.
  (insert-file-contents file nil nil nil t))


(provide 'reformatter)
;;; reformatter.el ends here
