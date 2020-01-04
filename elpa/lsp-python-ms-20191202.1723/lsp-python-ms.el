;;; lsp-python-ms.el --- The lsp-mode client for Microsoft python-language-server -*- lexical-binding: t -*-

;; Author: Charl Botha
;; Maintainer: Andrew Christianson, Vincent Zhang
;; Version: 0.5.0
;; Package-Version: 20191202.1723
;; Package-Requires: ((emacs "25.1") (cl-lib "0.6.1") (lsp-mode "6.0"))
;; Homepage: https://github.com/andrew-christianson/lsp-python-ms
;; Keywords: languages tools


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; from https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/

;;; Code:
(require 'cl-lib)
(require 'lsp-mode)
(require 'json)
(require 'projectile nil 'noerror)
(require 'find-file-in-project nil 'noerror)

;; Forward declare functions
(declare-function ffip-get-project-root-directory 'ffip)
(declare-function lsp-client-on-notification 'lsp-mode)

;; Forward declare variable
(defvar lsp-render-markdown-markup-content)

;; Group declaration
(defgroup lsp-python-ms nil
  "LSP support for python using the Microsoft Python Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Microsoft/python-language-server"))

(defcustom lsp-python-ms-dir (expand-file-name "mspyls/" user-emacs-directory)
  "The directory of the Microsoft Python Language Server."
  :type 'directory
  :group 'lsp-python-ms)

;; not used since ms-pyls 0.2.92+
;; see https://github.com/microsoft/vscode-python/blob/master/src/client/activation/languageServer/analysisOptions.ts#L93
;; (defcustom lsp-python-ms-cache-dir
;;   (directory-file-name (locate-user-emacs-file ".lsp-python/"))
;;   "Path to directory where the server will write cache files.

;; If this is nil, the language server will write cache files in a directory
;; sibling to the root of every project you visit")

(defcustom lsp-python-ms-extra-paths '()
  "A list of additional paths to search for python packages.

This should be a list of paths corresponding to additional python
library directories you want to search for completions.  Paths
should be as they are (or would appear) in sys.path.  Paths will
be prepended to the search path, and so will shadow duplicate
names in search paths returned by the interpreter."
  :type '(repeat directory)
  :group 'lsp-python-ms)
(make-variable-buffer-local 'lsp-python-ms-extra-paths)

(defcustom lsp-python-ms-python-executable-cmd "python"
  "Command to specify the Python command for the Microsoft Python Language Server.

Similar to the `python-shell-interpreter', but used only with mspyls.
Useful when there are multiple python versions in system.
e.g, there are `python2' and `python3', both in system PATH,
and the default `python' links to python2,
set as `python3' to let ms-pyls use python 3 environments."
  :type 'string
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-executable (concat lsp-python-ms-dir
                                            "Microsoft.Python.LanguageServer"
                                            (and (eq system-type 'windows-nt) ".exe"))
  "Path to the Microsoft Python LanguageServer binary."
  :type '(file :must-match t)
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-nupkg-channel "stable"
  "The channel of nupkg for the Microsoft Python Language Server:
stable, beta or daily."
  :type 'string
  :group 'lsp-python-ms)

;; See https://github.com/microsoft/python-language-server/blob/master/src/Analysis/Ast/Impl/Definitions/AnalysisOptions.cs
(defcustom lsp-python-ms-cache "None"
  "The cache level of analysis for Microsoft Python Language Server."
  :type '(choice
          (const "None")
          (const "System")
          (const "Library"))
  :group 'lsp-python-ms)

;; See https://github.com/microsoft/python-language-server for more diagnostics
(defcustom lsp-python-ms-errors ["unknown-parameter-name"
                                 "undefined-variable"
                                 "parameter-missing"
                                 "positional-argument-after-keyword"
                                 "too-many-function-arguments"]
  "Microsoft Python Language Server Error types."
  :type 'lsp-string-vector
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-warnings ["unresolved-import"
                                   "parameter-already-specified"
                                   "too-many-positional-arguments-before-star"]
  "Microsoft Python Language Server Warning types."
  :type 'lsp-string-vector
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-information []
  "Microsoft Python Language Server Information types."
  :type 'lsp-string-vector
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-disabled []
  "Microsoft Python Language Server Disabled types."
  :type 'lsp-string-vector
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-parse-dot-env-enabled t
  "Automatically parse .env file in the project root if non-nil."
  :type 'boolean
  :group 'lsp-python-ms)

(defconst lsp-python-ms--base-url "https://pvsc.blob.core.windows.net"
  "The base url to get nupkg package.
The alternative is `https://pvsc.azureedge.net'")

(defcustom lsp-python-ms-log-level "Error"
  "Log Level"
  :type 'string
  :group 'lsp-python-ms
  :options (list "Trace"
                 "Info"
                 "Information"
                 "Error"
                 "Warning"))

(defcustom lsp-python-ms-extra-major-modes '()
    "A list of additional major modes in which to activate.

In addition to the python-mode, you may wish the Microsoft Python
Language Server to activate in other major modes. If so, list them
here."
  :type 'list
  :group 'lsp-python-ms)

(defun lsp-python-ms-latest-nupkg-url (&optional channel)
  "Get the nupkg url of the latest Microsoft Python Language Server."
  (let ((channel (or channel "stable")))
    (unless (member channel '("stable" "beta" "daily"))
      (error (format "Unknown channel: %s" channel)))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "%s/python-language-server-%s?restype=container&comp=list&prefix=Python-Language-Server-%s-x64"
                 lsp-python-ms--base-url
                 channel
                 (cond ((eq system-type 'darwin)  "osx")
                       ((eq system-type 'gnu/linux) "linux")
                       ((eq system-type 'windows-nt) "win")
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
                        (cons (apply #'encode-time (parse-time-string last-modified)) url))))
                   blobs)
           (lambda (t1 t2)
             (time-less-p (car t2) (car t1))))))))))

(defun lsp-python-ms-setup (&optional forced)
  "Downloading Microsoft Python Language Server to path specified.
With prefix, FORCED to redownload the server."
  (interactive "P")
  (unless (and (not forced)
               (executable-find lsp-python-ms-executable))
    (let ((temp-file (make-temp-file "mspyls" nil ".zip"))
          (unzip-script (cond ((executable-find "powershell")
                               "powershell -noprofile -noninteractive \
-nologo -ex bypass Expand-Archive -path '%s' -dest '%s'")
                              ((executable-find "unzip")
                               "bash -c 'mkdir -p %2$s && unzip -qq %1$s -d %2$s'")
                              (t (error "Unable to unzip! You may need to install the `unzip` executable."))))
          (download-reporter (make-progress-reporter
                              (lsp--info "Downloading Microsoft Python Language Server...")
                              0  100)))
      (url-retrieve (lsp-python-ms-latest-nupkg-url lsp-python-ms-nupkg-channel)
                    (lambda (_data bar)
                      ;; Skip http header
                      (re-search-forward "\r?\n\r?\n")

                      ;; Save to the temp file
                      (let ((coding-system-for-write 'binary))
                        (write-region (point) (point-max) temp-file))

                      ;; Report progress
                      (progress-reporter-done bar)

                      ;; Extract the archive
                      (lsp--info "Extracting Microsoft Python Language Server...")
                      (when (file-exists-p lsp-python-ms-dir)
                        (delete-directory lsp-python-ms-dir 'recursive))

                      (set-process-sentinel
                       (start-process-shell-command "extract-mspyls" nil
                                                    (format unzip-script temp-file lsp-python-ms-dir))
                       (lambda (proc _)
                         (let ((status (process-exit-status proc)))
                           (if (and (= 0 status)
                                    (file-exists-p lsp-python-ms-executable))
                               (progn
                                 (lsp--info "Extracting Microsoft Python Language Server...done")
                                 ;; Make the binary executable
                                 (chmod lsp-python-ms-executable #o755)
                                 ;; Start LSP if need
                                 (when lsp-mode (lsp)))
                             (lsp--error "Failed to extract Microsoft Python Language Server: %d" status))))))
                    `(,download-reporter))
      (dotimes (k 100)
        (sit-for 0.1)
        (progress-reporter-update download-reporter k)))))

(defun lsp-python-ms-update-server ()
  "Update Microsoft Python Language Server.

On Windows, if the server is running, the updating will fail.
After stopping or killing the process, retry to update."
  (interactive)
  (lsp-python-ms-setup t))

(defun lsp-python-ms-locate-python ()
  "Look for virtual environments local to the workspace"
  (let* ((venv (locate-dominating-file default-directory "venv/"))
         (sys-python (executable-find lsp-python-ms-python-executable-cmd))
         (venv-python (expand-file-name "venv/bin/python" venv)))
    (cond
     ((and venv (file-executable-p venv-python)) venv-python)
     (sys-python))))

;; it's crucial that we send the correct Python version to MS PYLS,
;; else it returns no docs in many cases furthermore, we send the
;; current Python's (can be virtualenv) sys.path as searchPaths
(defun lsp-python-ms--get-python-ver-and-syspath (workspace-root)
  "Return list with pyver-string and list of python search paths.

The WORKSPACE-ROOT will be prepended to the list of python search
paths and then the entire list will be json-encoded."
  (when-let ((python (lsp-python-ms-locate-python))
             (default-directory workspace-root)
             (init "from __future__ import print_function; import sys; \
sys.path = list(filter(lambda p: p != '', sys.path)); import json;")
             (ver "v=(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));")
             (sp (concat "sys.path.insert(0, '" workspace-root "'); p=sys.path;"))
             (ex "e=sys.executable;")
             (val "print(json.dumps({\"version\":v,\"paths\":p,\"executable\":e}))"))
    (with-temp-buffer
      (call-process python nil t nil "-c" (concat init ver sp ex val))
      (let* ((json-array-type 'vector)
             (json-key-type 'string)
             (json-object-type 'hash-table)
             (json-string (buffer-string))
             (json-hash (json-read-from-string json-string)))
        (list (gethash "version" json-hash)
              (gethash "paths" json-hash)
              (gethash "executable" json-hash))))))

(defun lsp-python-ms--workspace-root ()
  "Get the path of the root of the current workspace.

Use `lsp-workspace-root', which is pressent in the \"new\"
lsp-mode and works when there's an active session.  Next try ffip
or projectile, or just return `default-directory'."
  (cond
   ((fboundp #'lsp-workspace-root) (lsp-workspace-root))
   ((fboundp #'ffip-get-project-root-directory) (ffip-get-project-root-directory))
   ((fboundp #'projectile-project-root) (projectile-project-root))
   ((fboundp #'project-current) (when-let ((project (project-current)))
                                  (car (project-roots project))))
   (t default-directory)))

;; I based most of this on the vs.code implementation:
;; https://github.com/Microsoft/vscode-python/blob/master/src/client/activation/languageServer/languageServer.ts#L219
;; (it still took quite a while to get right, but here we are!)
(defun lsp-python-ms--extra-init-params (&optional workspace)
  "Return form describing parameters for language server.

Old lsp will pass in a WORKSPACE, new lsp has a global
lsp-workspace-root function that finds the current buffer's
workspace root.  If nothing works, default to the current file's
directory"
  (let ((workspace-root (if workspace (lsp--workspace-root workspace) (lsp-python-ms--workspace-root))))
    (when lsp-python-ms-parse-dot-env-enabled
      (lsp-python-ms--parse-dot-env workspace-root))
    (cl-destructuring-bind (pyver pysyspath pyintpath)
        (lsp-python-ms--get-python-ver-and-syspath workspace-root)
      `(:interpreter
        (:properties (
                      :InterpreterPath ,pyintpath
                      :UseDefaultDatabase t
                      :Version ,pyver))
        ;; preferredFormat "markdown" or "plaintext"
        ;; experiment to find what works best -- over here mostly plaintext
        :displayOptions (:preferredFormat "markdown"
                         :trimDocumentationLines :json-false
                         :maxDocumentationLineLength 0
                         :trimDocumentationText :json-false
                         :maxDocumentationTextLength 0)
        :searchPaths ,(if lsp-python-ms-extra-paths
                          (vconcat lsp-python-ms-extra-paths nil)
                        pysyspath)
        :analysisUpdates t
        :asyncStartup t
        :logLevel ,lsp-python-ms-log-level
        :typeStubSearchPaths ,(vector (concat lsp-python-ms-dir "Typeshed"))))))

(defun lsp-python-ms--filter-nbsp (str)
  "Filter nbsp entities from STR."
  (let ((rx "&nbsp;"))
    (when (eq system-type 'windows-nt)
      (setq rx (concat rx "\\|\r")))
    (when str
      (replace-regexp-in-string rx " " str))))

(defun lsp-python-ms--parse-dot-env (root &optional envvar)
  "Set environment variable (default PYTHONPATH) from .env file if this file exists in the project root."
  (let* ((envvar (or envvar "PYTHONPATH"))
         (file (concat (file-name-as-directory root) ".env"))
         (rx (concat "^[:blank:]*" envvar "[:blank:]*=[:blank:]*"))
         val)
    (when (and (file-exists-p file) (file-regular-p file) (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (keep-lines rx (point-min) (point-max))
        (when (string-match (concat rx "\\(.*\\)") (buffer-string))
          (setq val (match-string 1 (buffer-string)))
          (unless (string-empty-p val)
            (setenv envvar val)))))))

(defun lsp-python-ms--language-server-started-callback (workspace _params)
  "Handle the python/languageServerStarted message.

WORKSPACE is just used for logging and _PARAMS is unused."
   (lsp-workspace-status "::Started" workspace)
   (lsp--info "Microsoft Python language server started"))

(defun lsp-python-ms--client-initialized (client)
   "Callback to register and configure client after it's initialized.

After CLIENT is initialized, this function is called to configure
other handlers. "
   (lsp-client-on-notification client "python/languageServerStarted"
                               #'lsp-python-ms--language-server-started-callback)
  (lsp-client-on-notification client "telemetry/event" #'ignore))

;; this gets called when we do lsp-describe-thing-at-point
;; see lsp-methods.el. As always, remove Microsoft's unwanted entities :(
(setq lsp-render-markdown-markup-content #'lsp-python-ms--filter-nbsp)

;; lsp-ui-doc--extract gets called when hover docs are requested
;; as always, we have to remove Microsoft's unnecessary &nbsp; entities
(advice-add 'lsp-ui-doc--extract
            :filter-return #'lsp-python-ms--filter-nbsp)

;; lsp-ui-sideline--format-info gets called when lsp-ui wants to show
;; hover info in the sideline again &nbsp; has to be removed
(advice-add 'lsp-ui-sideline--format-info
            :filter-return #'lsp-python-ms--filter-nbsp)

(defun lsp-python-ms--report-progress-callback (_workspace params)
  "Log progress information."
  (when (and (arrayp params) (> (length params) 0))
    (lsp-log (aref params 0))))

(defun lsp-python-ms--begin-progress-callback (workspace &rest _)
  (with-lsp-workspace workspace
    (--each (lsp--workspace-buffers workspace)
      (when (buffer-live-p it)
        (with-current-buffer it
          (lsp--spinner-start)))))
  (lsp--info "Microsoft Python language server is analyzing..."))

(defun lsp-python-ms--end-progress-callback (workspace &rest _)
  (with-lsp-workspace workspace
    (--each (lsp--workspace-buffers workspace)
      (when (buffer-live-p it)
        (with-current-buffer it
          (lsp--spinner-stop))))
    (lsp--info "Microsoft Python language server is analyzing...done")))

(defun lsp-python-ms--command-string ()
  "Return the command to start the server."
  ;; Try to download server if it doesn't exists
  (unless (executable-find lsp-python-ms-executable)
    (lsp-python-ms-setup))

  (if (executable-find lsp-python-ms-executable)
      lsp-python-ms-executable
    (error (concat "Cannot find Microsoft Python Language Server executable! It's expected to be "
                   lsp-python-ms-executable))))

(lsp-register-custom-settings
 `(("python.analysis.cachingLevel" lsp-python-ms-cache)
   ("python.analysis.errors" lsp-python-ms-errors)
   ("python.analysis.warnings" lsp-python-ms-warnings)
   ("python.analysis.information" lsp-python-ms-information)
   ("python.analysis.disabled" lsp-python-ms-disabled)
   ("python.analysis.autoSearchPaths" ,(<= (length lsp-python-ms-extra-paths) 0) t)))

(dolist (mode lsp-python-ms-extra-major-modes)
  (add-to-list 'lsp-language-id-configuration `(,mode . "python")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection 'lsp-python-ms--command-string)
  :major-modes (append '(python-mode) lsp-python-ms-extra-major-modes)
  :server-id 'mspyls
  :priority 1
  :initialization-options 'lsp-python-ms--extra-init-params
  :notification-handlers (lsp-ht ("python/languageServerStarted" 'lsp-python-ms--language-server-started-callback)
                                 ("telemetry/event" 'ignore)
                                 ("python/reportProgress" 'lsp-python-ms--report-progress-callback)
                                 ("python/beginProgress" 'lsp-python-ms--begin-progress-callback)
                                 ("python/endProgress" 'lsp-python-ms--end-progress-callback))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "python"))))))

(provide 'lsp-python-ms)

;;; lsp-python-ms.el ends here
