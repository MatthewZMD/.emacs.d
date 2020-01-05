;;; lsp-fsharp.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Reed Mullanix

;; Author: Reed Mullanix <reedmullanix@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; lsp-fsharp client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-fsharp nil
  "LSP support for the F# Programming Language, using the FsharpAutoComplete server."
  :link '(url-link "https://github.com/fsharp/FsAutoComplete")
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-server-runtime 'net-core
  "The .NET runtime to use."
  :group 'lsp-fsharp
  :type '(choice (const :tag "Use .Net Core" net-core)
                 (const :tag "Use Mono" mono)
                 (const :tag "Use .Net Framework" net-framework))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-server-install-dir (locate-user-emacs-file "fsautocomplete/")
  "Install directory for fsautocomplete server.
The slash is expected at the end."
  :group 'lsp-fsharp
  :risky t
  :type 'directory
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-server-download-url "https://ci.appveyor.com/api/projects/fsautocomplete/fsautocomplete/artifacts/bin/pkgs/fsautocomplete.netcore.zip?branch=master"
  "Fsautocomplete download url.
To use the mono/.Net framework version, set this to \"https://ci.appveyor.com/api/projects/fsautocomplete/fsautocomplete/artifacts/bin/pkgs/fsautocomplete.zip?branch=master\""
  :group 'lsp-fsharp
  :risky t
  :type 'string
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-server-args nil
  "Extra arguments for the F# language server."
  :type '(repeat string)
  :group 'lsp-fsharp
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-fsharp-keywords-autocomplete t
  "Provides keywords in autocomplete list"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-external-autocomplete nil
  "Provides autocompletion for symbols from not opened namespaces/modules; inserts open on accept"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-linter t
  "Enables FSharpLint integration, provides additional warnings and code action fixes"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-union-case-stub-generation t
  "Enablesa  code action to generate pattern matching cases"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-union-case-stub-generation-body "failwith \"Not Implemented\""
  "defines dummy body used by pattern matching generator"
  :group 'lsp-fsharp
  :type 'string
  :risky t
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-record-stub-generation t
  "Enables code action to generate record stub"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-record-stub-generation-body "failwith \"Not Implemented\""
  "defines dummy body used by record stub generator"
  :group 'lsp-fsharp
  :type 'string
  :risky t
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-interface-stub-generation t
  "Enables code action to generate an interface stub"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-interface-stub-generation-object-identifier "this"
  "Defines object identifier used by interface stub generator, e.g. `this' or `self'"
  :group 'lsp-fsharp
  :type 'string
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-interface-stub-generation-method-body "failwith \"Not Implemented\""
  "Defines dummy body used by interface stub generator"
  :group 'lsp-fsharp
  :type 'string
  :risky t
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-unused-opens-analyzer t
  "Enables unused open detection"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-unused-declarations-analyzer t
  "Enables unused symbol detection"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-simplify-name-analyzer nil
  "Enables simplify name analyzer and remove redundant qualifier quick fix"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-resolve-namespaces t
  "Enables resolve namespace quick fix; adds `open' if symbol is
from not yet opened module/namespace"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-enable-reference-code-lens t
  "Enables reference count code lenses. It is recommended to
disable if `--backgorund-service-enabled' is not used"
  :group 'lsp-fsharp
  :type 'bool
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fsharp-auto-workspace-init nil
  "Enable automatic workspace initialization. Do note that this
  can cause unexpected or challenging behaviors, as solutions
  with test projects are not autoloaded by FSharpAutoComplete."
  :group 'lsp-fsharp
  :type 'bool
  :risky t)

(defun lsp-fsharp--fsac-runtime-cmd ()
  "Get the command required to run fsautocomplete based off of the current runtime."
  (pcase lsp-fsharp-server-runtime
    ('net-core "dotnet")
    ('mono "mono")
    ('net-framework nil)))

(defun lsp-fsharp--fsac-cmd ()
  "The location of fsautocomplete executable."
  (let ((file-ext (if (eq lsp-fsharp-server-runtime 'net-core)
                      ".dll"
                    ".exe")))
    (expand-file-name (concat "fsautocomplete" file-ext) lsp-fsharp-server-install-dir)))

(defun lsp-fsharp--fsac-locate ()
  "Return the location of the fsautocomplete langauge server."
  (let ((fsac (lsp-fsharp--fsac-cmd)))
    (unless (file-exists-p fsac)
      (if (yes-or-no-p "Server is not installed. Do you want to install it?")
          (lsp-fsharp--fsac-install)
        (error "LSP F# cannot be started without FsAutoComplete Server")))
    fsac))

(defun lsp-fsharp--fsac-install ()
  "Download the latest version of fsautocomplete and extract it to `lsp-fsharp-server-install-dir'."
  (let* ((temp-file (make-temp-file "fsautocomplete" nil ".zip"))
         (install-dir-full (expand-file-name lsp-fsharp-server-install-dir))
         (unzip-script (cond ((executable-find "unzip") (format "mkdir -p %s && unzip -qq %s -d %s" install-dir-full temp-file install-dir-full))
                             ((executable-find "powershell") (format "powershell -noprofile -noninteractive -nologo -ex bypass Expand-Archive -path '%s' -dest '%s'" temp-file install-dir-full))
                             (t (user-error (format "Unable to unzip server - file %s cannot be extracted, please extract it manually" temp-file))))))
    (url-copy-file lsp-fsharp-server-download-url temp-file t)
    (shell-command unzip-script)
    (shell-command (format "%s %s --version" (lsp-fsharp--fsac-runtime-cmd) (lsp-fsharp--fsac-cmd)))))

(defun lsp-fsharp-update-fsac ()
  "Update fsautocomplete to the latest version."
  (interactive)
  (-let [install-dir (f-expand lsp-fsharp-server-install-dir)]
    (f-delete install-dir t)
    (lsp-fsharp--fsac-install)))

(defun lsp-fsharp--make-launch-cmd ()
  "Build the command required to launch fsautocomplete."
  (append (list (lsp-fsharp--fsac-runtime-cmd) (lsp-fsharp--fsac-locate) "--background-service-enabled")
          lsp-fsharp-server-args))

(defun lsp-fsharp--project-list ()
  "Get the list of files we need to send to fsharp/workspaceLoad."
  (let* ((response (lsp-request "fsharp/workspacePeek"
                                `(:directory ,(lsp-workspace-root)
                                             :deep 10
                                             :excludedDirs ["paket-files" ".git" "packages" "node_modules"])))
         (data (json-read-from-string (ht-get response "content")))
         (found (cdr (assq 'Found (cdr (assq 'Data data)))))
         (directory (car (seq-filter (lambda (d) (equal "directory" (cdr (assq 'Type d)))) found))))
    (cdr (assq 'Fsprojs (cdr (assq 'Data directory))))))


;;;###autoload
(defun lsp-fsharp--workspace-load (projects)
  "Load all of the provided PROJECTS."
  (lsp-request-async "fsharp/workspaceLoad"
                     `(:textDocuments ,(vconcat [] (mapcar (lambda (p) `(:uri ,p)) projects)))
                     (lambda (_)
                       (lsp--info "Workspace Loaded!"))))

(defvar lsp-fsharp--default-init-options  (list)
  "Default init options to be passed to FSharpAutoComplete,
  updated conditionally by `lsp-fsharp--make-init-options'.")

(defun lsp-fsharp--make-init-options ()
  "Init options for F#."
  (-let [opts lsp-fsharp--default-init-options]
    (if lsp-fsharp-auto-workspace-init
        (push '(:AutomaticWorkspaceInit . t) opts)
      opts)))

(lsp-register-custom-settings
 `(("FSharp.KeywordsAutocomplete" lsp-fsharp-keywords-autocomplete t)
   ("FSharp.ExternalAutocomplete" lsp-fsharp-external-autocomplete t)
   ("FSharp.Linter" lsp-fsharp-linter t)
   ("FSharp.UnionCaseStubGeneration" lsp-fsharp-union-case-stub-generation t)
   ("FSharp.UnionCaseStubGenerationBody" lsp-fsharp-union-case-stub-generation-body)
   ("FSharp.RecordStubGeneration" lsp-fsharp-record-stub-generation t)
   ("FSharp.RecordStubGenerationBody" lsp-fsharp-record-stub-generation-body)
   ("FSharp.InterfaceStubGeneration" lsp-fsharp-interface-stub-generation t)
   ("FSharp.InterfaceStubGenerationObjectIdentifier" lsp-fsharp-interface-stub-generation-object-identifier)
   ("FSharp.InterfaceStubGenerationMethodBody" lsp-fsharp-interface-stub-generation-method-body)
   ("FSharp.UnusedOpensAnalyzer" lsp-fsharp-unused-opens-analyzer t)
   ("FSharp.UnusedDeclarationsAnalyzer" lsp-fsharp-unused-declarations-analyzer t)
   ("FSharp.SimplifyNameAnalyzer" lsp-fsharp-simplify-name-analyzer t)
   ("FSharp.ResolveNamespaces" lsp-fsharp-resolve-namespaces t)
   ("FSharp.EnableReferenceCodeLens" lsp-fsharp-enable-reference-code-lens t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-fsharp--make-launch-cmd)
                  :major-modes '(fsharp-mode)
                  :notification-handlers (ht ("fsharp/notifyCancel" #'ignore)
                                             ("fsharp/notifyWorkspace" #'ignore)
                                             ("fsharp/fileParsed" #'ignore)
                                             ("fsharp/notifyWorkspacePeek" #'ignore))
                  :initialization-options 'lsp-fsharp--make-init-options
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      ;; Something needs to be calling lsp--set-configuration
                                      (progn
                                        (lsp--set-configuration
                                         (lsp-configuration-section "fsharp"))
                                        (lsp-fsharp--workspace-load
                                         (lsp-fsharp--project-list)))))
                  :server-id 'fsac))

(provide 'lsp-fsharp)
;;; lsp-fsharp.el ends here
