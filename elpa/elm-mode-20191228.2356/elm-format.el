;;; elm-format.el --- Automatically format an Elm buffer.

;; Copyright (C) 2015  Bogdan Popa

;; Author: Bogdan Popa
;; URL: https://github.com/jcollard/elm-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'reformatter)

(defcustom elm-format-on-save nil
  "When non-nil, run `elm-format-buffer' on save.

This variable is obsolete, and you should prefer to enable
`elm-format-on-save-mode' by adding it to your `elm-mode-hook',
or by placing a clause like the following in the .dir-locals.el
for your project:

    ((elm-mode (mode . elm-format-on-save)))"
  :group 'elm-format
  :type 'boolean)

(defcustom elm-format-elm-version "0.19"
  "The version of Elm against which code should be formatted."
  :group 'elm-format
  :type '(choice (const :tag "Default: 0.19" "0.19")
		 (const :tag "0.18" "0.18")
                 (const :tag "0.17" "0.17")
                 (const :tag "0.16" "0.16")))

(defcustom elm-format-command "elm-format"
  "The name of the `elm-format' command."
  :group 'elm-format
  :type 'string)

;;;###autoload (autoload 'elm-format-buffer "elm-format" nil t)
;;;###autoload (autoload 'elm-format-on-save-mode "elm-format" nil t)
(reformatter-define elm-format
  :program elm-format-command
  :args (list "--stdin" "--elm-version" elm-format-elm-version "--yes")
  :group 'elm-format
  :lighter " ElmFmt")

;;;###autoload
(define-obsolete-function-alias 'elm-mode-format-buffer 'elm-format-buffer "20190113")


(provide 'elm-format)
;;; elm-format.el ends here
