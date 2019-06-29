;;; init-python.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Mingde Zeng
;;
;; Filename: init-python.el
;; Description: Initialize Python
;; Author: Mingde (Matthew) Zeng
;; Maintainer:
;; Created: Mon Jun 10 18:58:02 2019 (-0400)
;; Version: 2.0.0
;; Version: 2.0.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: lsp-python-ms
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes lsp-python-ms
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-flycheck)
  (require 'init-const))

;; PythonConfig
(use-package python
  :after flycheck
  :mode "\\.py\\'"
  :config
  (setq flycheck-python-pycompile-executable "python3")
  (setq python-shell-interpreter "python3"))
;; -PythonConfig

;; LSPPythonPac
(use-package lsp-python-ms
  :after lsp-mode
  :commands lsp-python-ms-setup
  :hook (python-mode . (lambda ()
                         (lsp-python-ms-setup)
                         (lsp-deferred)))
  :config
  (setq lsp-python-ms-dir (expand-file-name "mspyls/" user-emacs-directory)
        lsp-python-ms-executable (concat lsp-python-ms-dir
                                         "Microsoft.Python.LanguageServer"
                                         (and *sys/win32* ".exe")))

  ;; By twlz0ne https://emacs-china.org/t/lsp-python-ms-vs-pyls/9611/145
  (defun lsp-python-ms-latest-nupkg-url (&optional channel)
    "Get the nupkg url of the latest Microsoft Python Language Server."
    (let ((channel (or channel "stable")))
      (unless (member channel '("stable" "beta" "daily"))
        (error (format "Unknown channel: %s" channel)))
      (with-current-buffer
          (url-retrieve-synchronously
           (format "https://pvsc.blob.core.windows.net/python-language-server-%s\
?restype=container&comp=list&prefix=Python-Language-Server-%s-x64"
                   channel
                   (cond (*sys/mac* "osx")
                         (*sys/linux* "linux")
                         (*sys/win32* "win")
                         (t (error (format "Unsupported system: %s" system-type))))))
        (goto-char (point-min))
        (re-search-forward "\n\n")
        (pcase (xml-parse-region (point) (point-max))
          (`((EnumerationResults
              ((ContainerName . ,_))
              (Prefix nil ,_)
              (Blobs nil . ,blobs)
              (NextMarker nil)))
           (cdar
            (sort
             (mapcar (lambda (blob)
                       (pcase blob
                         (`(Blob
                            nil
                            (Name nil ,_)
                            (Url nil ,url)
                            (Properties nil (Last-Modified nil ,last-modified) . ,_))
                          (cons (encode-time (parse-time-string last-modified)) url))))
                     blobs)
             (lambda (t1 t2)
               (time-less-p (car t2) (car t1))))))))))

  (defun lsp-python-ms-setup (&optional forced)
    "Downloading Microsoft Python Language Server to path specified.
With prefix, FORCED to redownload the server."
    (interactive "P")
    (unless (and (not forced)
                 (file-exists-p lsp-python-ms-dir))
      (let ((temp-file (make-temp-file "mspyls" nil ".zip"))
            (unzip-script (cond ((executable-find "unzip")
                                 "bash -c 'mkdir -p %2$s && unzip -qq %1$s -d %2$s'")
                                ((executable-find "powershell")
                                 "powershell -noprofile -noninteractive \
-nologo -ex bypass Expand-Archive -path '%s' -dest '%s'")
                                (t nil))))
        (url-copy-file (lsp-python-ms-latest-nupkg-url) temp-file 'overwrite)
        (if (file-exists-p lsp-python-ms-dir) (delete-directory lsp-python-ms-dir 'recursive))
        (shell-command (format unzip-script temp-file lsp-python-ms-dir))
        (if (file-exists-p lsp-python-ms-executable) (chmod lsp-python-ms-executable #o755))

        (message "Downloaded Microsoft Python Language Server!"))))

  (defun lsp-python-ms-update-server ()
    "Update Microsoft Python Language Server."
    (interactive)
    (message "Server update started...")
    (lsp-python-ms-setup t)
    (message "Server update finished.")))
;; -LSPPythonPac

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
