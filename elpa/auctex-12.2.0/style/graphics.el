;;; graphics.el --- Handle graphical commands in LaTeX 2e.

;;; Code:

;; Load "graphicx" explicitly to access `LaTeX-graphicx-package-options'
;; before running style hook "graphics".  This is necessary to have
;; support for completion of package options of "usepackage".
(TeX-load-style "graphicx")
(defvar LaTeX-graphics-package-options LaTeX-graphicx-package-options)

(TeX-add-style-hook "graphics"
 (function
  (lambda ()
    (TeX-run-style-hooks "graphicx")))
 LaTeX-dialect)

;;; graphics.el ends here.
