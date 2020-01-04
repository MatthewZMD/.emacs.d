;;; bui-entry.el --- 'Entry' type  -*- lexical-binding: t -*-

;; Copyright © 2015-2016 Alex Kost <alezost@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides an API for 'entry' type which is just an alist of
;; KEY/VALUE pairs (KEY should be a symbol) with the required 'id' KEY.

;;; Code:

(require 'dash)
(require 'bui-utils)

(defvar bui-void-value 'VOID
  "Value returned by `bui-entry-value' if a parameter does not exist.")

(defun bui-void-value? (value)
  "Return non-nil, if VALUE is `bui-void-value'."
  (eq value bui-void-value))

(defun bui-entry-value (entry param)
  "Return value of the ENTRY PARAM.
If ENTRY does not have PARAM at all, return `bui-void-value'."
  (--if-let (assq param entry)
      (cdr it)
    bui-void-value))

(defun bui-entry-non-void-value (entry param)
  "Like `bui-entry-value' but return nil if value is void."
  (--when-let (bui-entry-value entry param)
    (and (not (bui-void-value? it)) it)))

(defun bui-entry-id (entry)
  "Return ENTRY ID."
  (bui-entry-value entry 'id))

(defun bui-entry-by-id (entries id)
  "Return an entry from ENTRIES by its ID."
  (--find (equal (bui-entry-id it) id)
          entries))

(defun bui-entries-by-ids (entries ids)
  "Return entries with IDS (a list of identifiers) from ENTRIES."
  (--filter (member (bui-entry-id it) ids)
            entries))

(defun bui-entry-by-param (entries param value &optional compare)
  "Return an entry from ENTRIES with PARAM's value equal VALUE.
The values are compared using COMPARE function (`equal' by default)."
  (or compare (setq compare #'equal))
  (--find (funcall compare (bui-entry-value it param) value)
          entries))

(defun bui-replace-entry (entries id new-entry)
  "Replace an entry with ID from ENTRIES by NEW-ENTRY.
Return a list of entries with the replaced entry."
  (--map-first (equal id (bui-entry-id it))
               new-entry
               entries))

(provide 'bui-entry)

;;; bui-entry.el ends here
