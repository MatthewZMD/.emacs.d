;;; j-book.el - Special code for j-book style.

;;; Code:

(TeX-add-style-hook
 "j-book"
 (lambda ()
   (LaTeX-largest-level-set "part"))
 LaTeX-dialect)

;;; j-book.el ends here
