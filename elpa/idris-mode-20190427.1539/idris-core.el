;;; idris-core.el --- Core functionality -*- lexical-binding: t -*-
;; Copyright (C) 2013 Hannes Mehnert

;; Authors: Hannes Mehnert <hannes@mehnert.org>
;;          David Raymond Christiansen <david@davidchristiansen.dk>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
(require 'idris-compat)

(defun idris-is-ident-char-p (ch)
  (or (and (<= ?a ch) (<= ch ?z))
      (and (<= ?A ch) (<= ch ?Z))
      (and (<= ?0 ch) (<= ch ?9))
      (= ch ?_)))

(provide 'idris-core)
;;; idris-core.el ends here
