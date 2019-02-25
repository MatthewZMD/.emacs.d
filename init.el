;;; package --- Summary
;;; This is MT`s personal init.el file for EMACS
;;; Commentary:

;; Loads the README org file which contains the *REAL* meat

;;; Code:

(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
(garbage-collect)

(provide 'init)
;;; init.el ends here
