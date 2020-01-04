;;; lsp-pwsh.el --- client for PowerShellEditorServices  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kien Nguyen

;; Author: kien.n.quang at gmail.com
;; Keywords: lsp

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

;;

;;; Code:

(require 'lsp-mode)
(require 'f)
(require 'dash)
(require 's)
(require 'ht)

(defgroup lsp-pwsh nil
  "LSP support for PowerShell, using the PowerShellEditorServices."
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.2"))

;; PowerShell vscode flags
(defcustom lsp-pwsh-help-completion "BlockComment"
  "Controls the comment-based help completion behavior triggered by typing '##'.
Set the generated help style with 'BlockComment' or 'LineComment'.
Disable the feature with 'Disabled'."
  :type
  '(choice
    (:tag "Disabled" "BlockComment" "LineComment"))
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-script-analysis-enable t
  "Enables real-time script analysis from PowerShell Script Analyzer.
Uses the newest installed version of the PSScriptAnalyzer module or the version bundled with this extension, if it is newer."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-script-analysis-settings-path ""
  "Specifies the path to a PowerShell Script Analyzer settings file.
To override the default settings for all projects, enter an absolute path, or enter a path relative to your workspace."
  :type 'string
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-folding-enable t
  "Enables syntax based code folding.
When disabled, the default indentation based code folding is used."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-folding-show-last-line t
  "Shows the last line of a folded section similar to the default VSCode folding style.
When disabled, the entire folded region is hidden."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-preset "Custom"
  "Sets the codeformatting options to follow the given indent style in a way that is compatible with PowerShell syntax.
For more information about the brace styles please refer to https://github.com/PoshCode/PowerShellPracticeAndStyle/issues/81."
  :type
  '(choice
    (:tag "Custom" "Allman" "OTBS" "Stroustrup"))
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-open-brace-on-same-line t
  "Places open brace on the same line as its associated statement."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-new-line-after-open-brace t
  "Adds a newline (line break) after an open brace."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-new-line-after-close-brace t
  "Adds a newline (line break) after a closing brace."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-pipeline-indentation-style "NoIndentation"
  "Multi-line pipeline style settings."
  :type
  '(choice
    (:tag "IncreaseIndentationForFirstPipeline" "IncreaseIndentationAfterEveryPipeline" "NoIndentation"))
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-whitespace-before-open-brace t
  "Adds a space between a keyword and its associated scriptblock expression."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-whitespace-before-open-paren t
  "Adds a space between a keyword (if, elseif, while, switch, etc) and its associated conditional expression."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-whitespace-around-operator t
  "Adds spaces before and after an operator ('=', '+', '-', etc.)."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-whitespace-after-separator t
  "Adds a space after a separator (',' and ';')."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-whitespace-inside-brace t
  "Adds a space after an opening brace ('{') and before a closing brace ('}')."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-whitespace-around-pipe t
  "Adds a space before and after the pipeline operator ('|')."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-ignore-one-line-block t
  "Does not reformat one-line code blocks, such as \"if (...) {...} else {...}\"."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-align-property-value-pairs t
  "Align assignment statements in a hashtable or a DSC Configuration."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-code-formatting-use-correct-casing nil
  "Use correct casing for cmdlets."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-developer-editor-services-log-level "Normal"
  "Sets the logging verbosity level for the PowerShell Editor Services host executable.
Valid values are 'Diagnostic', 'Verbose', 'Normal', 'Warning', and 'Error'"
  :type
  '(choice
    (:tag "Diagnostic" "Verbose" "Normal" "Warning" "Error"))
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-developer-editor-services-wait-for-debugger nil
  "Launches the language service with the /waitForDebugger flag to force it to wait for a .NET debugger to attach before proceeding."
  :type 'boolean
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-developer-feature-flags nil
  "An array of strings that enable experimental features in the PowerShell extension."
  :type
  '(repeat string)
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(lsp-register-custom-settings
 '(("powershell.developer.featureFlags" lsp-pwsh-developer-feature-flags)
   ("powershell.developer.editorServicesWaitForDebugger" lsp-pwsh-developer-editor-services-wait-for-debugger t)
   ("powershell.codeFormatting.useCorrectCasing" lsp-pwsh-code-formatting-use-correct-casing t)
   ("powershell.codeFormatting.alignPropertyValuePairs" lsp-pwsh-code-formatting-align-property-value-pairs t)
   ("powershell.codeFormatting.ignoreOneLineBlock" lsp-pwsh-code-formatting-ignore-one-line-block t)
   ("powershell.codeFormatting.whitespaceAroundPipe" lsp-pwsh-code-formatting-whitespace-around-pipe t)
   ("powershell.codeFormatting.whitespaceInsideBrace" lsp-pwsh-code-formatting-whitespace-inside-brace t)
   ("powershell.codeFormatting.whitespaceAfterSeparator" lsp-pwsh-code-formatting-whitespace-after-separator t)
   ("powershell.codeFormatting.whitespaceAroundOperator" lsp-pwsh-code-formatting-whitespace-around-operator t)
   ("powershell.codeFormatting.whitespaceBeforeOpenParen" lsp-pwsh-code-formatting-whitespace-before-open-paren t)
   ("powershell.codeFormatting.whitespaceBeforeOpenBrace" lsp-pwsh-code-formatting-whitespace-before-open-brace t)
   ("powershell.codeFormatting.pipelineIndentationStyle" lsp-pwsh-code-formatting-pipeline-indentation-style)
   ("powershell.codeFormatting.newLineAfterCloseBrace" lsp-pwsh-code-formatting-new-line-after-close-brace t)
   ("powershell.codeFormatting.newLineAfterOpenBrace" lsp-pwsh-code-formatting-new-line-after-open-brace t)
   ("powershell.codeFormatting.openBraceOnSameLine" lsp-pwsh-code-formatting-open-brace-on-same-line t)
   ("powershell.codeFormatting.preset" lsp-pwsh-code-formatting-preset)
   ("powershell.codeFolding.showLastLine" lsp-pwsh-code-folding-show-last-line t)
   ("powershell.codeFolding.enable" lsp-pwsh-code-folding-enable t)
   ("powershell.scriptAnalysis.settingsPath" lsp-pwsh-script-analysis-settings-path)
   ("powershell.scriptAnalysis.enable" lsp-pwsh-script-analysis-enable t)
   ("powershell.helpCompletion" lsp-pwsh-help-completion)))

;; lsp-pwsh custom variables
(defcustom lsp-pwsh-ext-path (expand-file-name ".extension/pwsh"
                                               user-emacs-directory)
  "The path to powershell vscode extension."
  :type 'string
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-exe (or (executable-find "pwsh") (executable-find "powershell"))
  "PowerShell executable."
  :type 'string
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-pwsh-dir (expand-file-name "PowerShellEditorServices" lsp-pwsh-ext-path)
  "Path to PowerShellEditorServices without last slash."
  :type 'string
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defvar lsp-pwsh-log-path (expand-file-name "logs" lsp-pwsh-ext-path)
  "Path to directory where server will write log files.
Must not nil.")

(defvar lsp-pwsh--sess-id (emacs-pid))

(defun lsp-pwsh--command ()
  "Return the command to start server."
  (unless (and lsp-pwsh-exe (file-executable-p lsp-pwsh-exe))
    (user-error "Use `lsp-pwsh-exe' with the value of `%s' is not a valid powershell binary"
                lsp-pwsh-exe))
  ;; Download extension
  (lsp-pwsh-setup)
  `(,lsp-pwsh-exe "-NoProfile" "-NonInteractive" "-NoLogo"
                  ,@(if (eq system-type 'windows-nt) '("-ExecutionPolicy" "Bypass"))
                  "-OutputFormat" "Text"
                  "-File"
                  ,(f-join lsp-pwsh-dir "PowerShellEditorServices/Start-EditorServices.ps1")
                  "-HostName" "'Emacs Host'"
                  "-HostProfileId" "'Emacs.LSP'"
                  "-HostVersion" "0.1"
                  "-LogPath" ,(f-join lsp-pwsh-log-path "emacs-powershell.log")
                  "-LogLevel" ,lsp-pwsh-developer-editor-services-log-level
                  "-SessionDetailsPath"
                  ,(format "%s/PSES-VSCode-%d" lsp-pwsh-log-path lsp-pwsh--sess-id)
                  ;; "-AdditionalModules" "@('PowerShellEditorServices.VSCode')"
                  "-Stdio"
                  "-BundledModulesPath" ,lsp-pwsh-dir
                  "-FeatureFlags" "@()"
                  ))

(defun lsp-pwsh--extra-init-params ()
  "Return form describing parameters for language server.")

(defun lsp-pwsh--apply-code-action-edits (action)
  "Handle ACTION for PowerShell.ApplyCodeActionEdits."
  (-if-let* ((command (gethash "command" action))
             ((&hash "StartLineNumber" "EndLineNumber"
                     "StartColumnNumber" "EndColumnNumber" "Text")
              (lsp-seq-first (gethash "arguments" action)))
             (edits `[,(ht ("range" (ht ("start"
                                         (ht ("line" (- StartLineNumber 1))
                                             ("character" (- StartColumnNumber 1))))
                                        ("end"
                                         (ht ("line" (- EndLineNumber 1))
                                             ("character" (- EndColumnNumber 1))))))
                           ("newText" Text))]))
      (lsp--apply-text-edits edits)
    (lsp-send-execute-command command (gethash "arguments" action))))

(defun lsp-pwsh--show-code-action-document (action)
  "Handle ACTION for PowerShell.ShowCodeActionDocumentation."
  (-if-let* ((rule-raw (lsp-seq-first (gethash "arguments" action)))
             (rule-id (if (s-prefix-p "PS" rule-raw) (substring rule-raw 2) rule-raw)))
      (browse-url
       (concat "https://github.com/PowerShell/PSScriptAnalyzer/blob/master/RuleDocumentation/"
               rule-id
               ".md"))
    (lsp-warn "Cannot show documentation for code action, no ruleName was supplied")))

(defvar lsp-pwsh--major-modes '(powershell-mode))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-pwsh--command)
  :major-modes lsp-pwsh--major-modes
  :server-id 'pwsh-ls
  :priority -1
  :multi-root t
  :initialization-options #'lsp-pwsh--extra-init-params
  :notification-handlers (lsp-ht ("powerShell/executionStatusChanged" #'ignore)
                                 ("output" #'ignore))
  :action-handlers (lsp-ht ("PowerShell.ApplyCodeActionEdits"
                            #'lsp-pwsh--apply-code-action-edits)
                           ("PowerShell.ShowCodeActionDocumentation"
                            #'lsp-pwsh--show-code-action-document))
  :initialized-fn (lambda (w)
                    (with-lsp-workspace w
                      (lsp--set-configuration
                       (lsp-configuration-section "powershell")))
                    (let ((caps (lsp--workspace-server-capabilities w)))
                      (ht-set caps "documentRangeFormattingProvider" t)
                      (ht-set caps "documentFormattingProvider" t)))
  ))

;; Compatibility
(with-eval-after-load 'company-lsp
  (advice-add 'company-tng--supress-post-completion
              :after-while
              (lambda (&rest _)
                (not (memq major-mode lsp-pwsh--major-modes)))
              '((name . --force-post-completion-for-pwsh))))

;;; Utils
(defconst lsp-pwsh-unzip-script "\"%s\" -noprofile -noninteractive -nologo -ex bypass -command Expand-Archive -Path '%s' -DestinationPath '%s'"
  "Powershell script to unzip vscode extension package file.")

(defconst lsp-pwsh-editor-svcs-dl-script "\"%s\" -noprofile -noninteractive -nologo -ex bypass -command Invoke-WebRequest -UseBasicParsing -uri '%s' -outfile '%s'"
  "Command executed via `shell-command' to download the latest PowerShellEditorServices release.")

(defcustom lsp-pwsh-github-asset-url
  "https://github.com/%s/%s/releases/latest/download/%s"
  "GitHub latest asset template url."
  :type 'string
  :group 'lsp-pwsh
  :package-version '(lsp-mode . "6.2"))

(defun lsp-pwsh--get-extension (url dest)
  "Get extension from URL and extract to DEST."
  (let ((temp-file (make-temp-file "ext" nil ".zip")))
    ;; since we know it's installed, use powershell to download the file
    ;; (and avoid url.el bugginess or additional libraries)
    (shell-command (format lsp-pwsh-editor-svcs-dl-script lsp-pwsh-exe url temp-file))
    (message "lsp-pwsh: Downloading done!")
    (if (file-exists-p dest) (delete-directory dest 'recursive))
    (shell-command (format lsp-pwsh-unzip-script lsp-pwsh-exe temp-file dest))
    (message "lsp-pwsh: Finish unzip!")))

(defun lsp-pwsh-setup (&optional forced)
  "Downloading PowerShellEditorServices to `lsp-pwsh-dir'.
FORCED if specified with prefix argument."
  (interactive "P")
  (let ((parent-dir (file-name-directory lsp-pwsh-dir)))
    (unless (and (not forced) (file-exists-p parent-dir))
      (lsp-pwsh--get-extension
       (format lsp-pwsh-github-asset-url "PowerShell" "PowerShellEditorServices" "PowerShellEditorServices.zip")
       parent-dir))))

(provide 'lsp-pwsh)
;;; lsp-pwsh.el ends here
