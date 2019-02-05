;;; typescript-mode-test-utilities --- This file contains test utilities for typescript-mode.el

;;; Commentary:
;; See typescript-mode-tests.el and typescript-mode-jsdoc-tests.el

;;; Code:

(require 'ert)
(require 'typescript-mode)

;; Adapted from jdee-mode's test suite.
(defmacro test-with-temp-buffer (content &rest body)
  "Fill a temporary buffer with `CONTENT' and eval `BODY' in it."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (typescript-mode)
     (goto-char (point-min))
     ;; We need this so that tests that simulate user actions operate on the right buffer.
     (switch-to-buffer (current-buffer))
     ,@body))

(defmacro test-with-fontified-buffer (content &rest body)
  "Fill a temporary buffer with `CONTENT' and eval `BODY' in it."
  (declare (debug t)
           (indent 1))
  `(test-with-temp-buffer
    ,content
     (font-lock-fontify-buffer)
     ,@body))

(defun get-face-at (loc)
  "Get the face at `LOC'.
If it is not a number, then we `re-search-forward' with `LOC'
as the search pattern."
  (when (not (numberp loc))
    (save-excursion
      (re-search-forward loc)
      (setq loc (match-beginning 0))))
  (get-text-property loc 'face))

(defun font-lock-test (contents expected)
  "Perform a test on our template.
`CONTENTS' is the string to put in the temporary buffer.
`EXPECTED' is the expected results.
It should be a list of (LOCATION . FACE) pairs, where
LOCATION can be either a single location, or list of locations,
that are all expected to have the same face."
  (test-with-fontified-buffer
   contents
   ;; Make sure our propertize function has been applied to the whole
   ;; buffer.
   (syntax-propertize (point-max))
   (dolist (spec expected)
     (if (listp (car spec))
         (dolist (source (car spec))
           (should (eq (get-face-at source) (cdr spec))))
       (should (eq (get-face-at (car spec)) (cdr spec)))))))

(provide 'typescript-mode-test-utilities)

;;; typescript-mode-test-utilities.el ends here
