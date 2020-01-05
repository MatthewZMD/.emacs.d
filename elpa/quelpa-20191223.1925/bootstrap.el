;;; bootstrap.el --- Bootstrap quelpa.el

;; Copyright 2014-2018, Steckerhalter

;; Author: steckerhalter
;; URL: https://framagit.org/steckerhalter/quelpa
;; Version: 0.0.1
;; License: https://framagit.org/steckerhalter/quelpa/LICENSE

;; This file is not part of GNU Emacs.

;;; Code:

(require 'package)

(defvar quelpa-ci-dir nil
  "If non-nil, quelpa will not be loaded from the url but from the given dir.")

;; `package' has to be initialized to install pkgs
(package-initialize)

(let ((temp-dir (make-temp-file "quelpa" t)))
  (unless (require 'quelpa nil t)
    (let ((file (or (when quelpa-ci-dir (concat quelpa-ci-dir "/quelpa.el"))
                    (expand-file-name "quelpa.el" temp-dir))))
      (unless quelpa-ci-dir
        (url-copy-file "https://framagit.org/steckerhalter/quelpa/raw/master/quelpa.el" file t))
      (package-install-file file)))

  (delete-directory temp-dir t))

;;; bootstrap.el ends here
