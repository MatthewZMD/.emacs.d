;;; init.el --- Initialize Emacs -*- lexical-binding: t -*-
;;; This is MT`s personal init.el file for EMACS
;;; Commentary:

;; Loads the README org file which contains the *REAL* meat

;;; Code:

(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

(provide 'init)
;;; init.el ends here
