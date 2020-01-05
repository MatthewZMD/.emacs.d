;;; elm-imenu.el --- imenu support for elm
;;; Commentary:
;;; Code:
(require 'imenu)

(defun elm-imenu-create-index ()
  "Create an imenu index for the current buffer."
  (save-excursion
    (imenu--generic-function
     '(("Type" "^type \\([A-Z][^ \n]+\\)" 1)
       ("Type Alias" "^type alias \\([A-Z][^ \n]+\\)" 1)
       ("Port" "^port \\([^ ]+\\)" 1)
       ("Function" "^\\([^ ]+\\) :" 1)))))


(provide 'elm-imenu)
;;; elm-imenu.el ends here
