;;; lsp-java.el --- Java support for lsp-mode  -*- lexical-binding: t; -*-

;; Version: 2.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0") (markdown-mode "2.3") (dash "2.14.1") (f "0.20.0") (ht "2.0") (dash-functional "1.2.0") (request "0.3.0") (treemacs "2.5"))
;; Keywords: java
;; URL: https://github.com/emacs-lsp/lsp-java

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Java specific adapter for LSP mode

;;; Code:
(require 'cc-mode)
(require 'lsp)
(require 'markdown-mode)
(require 'dash)
(require 'ht)
(require 'f)
(require 'tree-widget)
(require 'request)
(require 'cl-lib)

(defgroup lsp-java nil
  "JDT emacs frontend."
  :prefix "lsp-java-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/emacs-lisp/lsp-java"))

(defcustom lsp-java-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/")
  "Install directory for eclipse.jdt.ls-server.
The slash is expected at the end."
  :group 'lsp-java
  :risky t
  :type 'directory)

(defcustom lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
  "JDT JS download url.
Use http://download.eclipse.org/che/che-ls-jdt/snapshots/che-jdt-language-server-latest.tar.gz if you want to use Eclipse Che JDT LS."
  :group 'lsp-java
  :type 'string)

(defcustom lsp-java-java-path "java"
  "Path of the java executable."
  :group 'lsp-java
  :type 'string)

(defvar lsp-java-progress-string ""
  "Java progress status as reported by the language server.")

(defface lsp-java-progress-face
  '((t (:inherit 'success)))
  "face for activity message"
  :group 'lsp-java)

(defcustom lsp-java-workspace-dir (expand-file-name (locate-user-emacs-file "workspace/"))
  "LSP java workspace directory."
  :group 'lsp-java
  :risky t
  :type 'directory)

(defcustom lsp-java-workspace-cache-dir (expand-file-name ".cache/" lsp-java-workspace-dir)
  "LSP java workspace cache directory."
  :group 'lsp-java
  :risky t
  :type 'directory)

(defcustom lsp-java-themes-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons")
  "Directory containing themes."
  :type 'directory
  :group 'lsp-java)

(defcustom lsp-java-theme "vscode"
  "Theme to use."
  :type 'string
  :group 'lsp-java)

(defcustom lsp-jt-root (expand-file-name (locate-user-emacs-file "eclipse.jdt.ls/server/java-test/server"))
  "The `lsp-jt' root."
  :type 'string)

(defcustom lsp-java-pop-buffer-function 'lsp-java-show-buffer
  "The function which will be used for showing the helper windows."
  :type 'function
  :group 'lsp-java)

(defcustom lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication")
  "Specifies extra VM arguments used to launch the Java Language Server.

Eg. use `-noverify -Xmx1G -XX:+UseG1GC
-XX:+UseStringDeduplication` to bypass class
verification,increase the heap size to 1GB and enable String
deduplication with the G1 Garbage collector"
  :group 'lsp-java
  :risky t
  :type '(repeat string))

(defcustom lsp-java-9-args '("--add-modules=ALL-SYSTEM" "--add-opens java.base/java.util=ALL-UNNAMED" "--add-opens java.base/java.lang=ALL-UNNAMED")
  "Specifies arguments specific to java 9 and later."
  :group 'lsp-java
  :risky t
  :type '(repeat string))

(defcustom lsp-java-errors-incomplete-classpath-severity "warning"
  "Specifies the severity of the message when the classpath is
incomplete for a Java file"
  :type '(choice (:tag "ignore" "info" "warning" "error")))

(defcustom lsp-java-dependency-package-representation "flat"
  "Specifies the severity of the message when the classpath is
incomplete for a Java file"
  :type '(choice (:tag "flat" "hierarchical")))

(defcustom lsp-java-configuration-check-project-settings-exclusions t
  "Checks if the extension-generated project settings
files (.project, .classpath, .factorypath, .settings/) should be
excluded from the file explorer."
  :type 'boolean)

(defcustom lsp-java-configuration-update-build-configuration "automatic"
  "Specifies how modifications on build files update the Java
classpath/configuration"
  :type '(choice (:tag "disabled" "interactive" "automatic")))

(defcustom lsp-java-trace-server "off"
  "Traces the communication between VS Code and the Java language
server."
  :type '(choice (:tag "off" "messages" "verbose")))

(defcustom lsp-java-import-gradle-enabled t
  "Enable/disable the Gradle importer."
  :type 'boolean)

(defcustom lsp-java-import-maven-enabled t
  "Enable/disable the Maven importer."
  :type 'boolean)

(defcustom lsp-java-maven-download-sources nil
  "Enable/disable eager download of Maven source artifacts."
  :type 'boolean)

(defcustom lsp-java-references-code-lens-enabled nil
  "Enable/disable the references code lens."
  :type 'boolean)

(defcustom lsp-java-signature-help-enabled t
  "Enable/disable the signature help."
  :type 'boolean)

(defcustom lsp-java-implementations-code-lens-enabled nil
  "Enable/disable the implementations code lens."
  :type 'boolean)

(defcustom lsp-java-configuration-maven-user-settings nil
  "Path to Maven's settings.xml"
  :type 'string)

(defcustom lsp-java-format-enabled t
  "Enable/disable default Java formatter"
  :type 'boolean)

(defcustom lsp-java-save-actions-organize-imports nil
  "Enable/disable auto organize imports on save action"
  :type 'boolean)

(defcustom lsp-java-import-exclusions ["**/node_modules/**" "**/.metadata/**" "**/archetype-resources/**" "**/META-INF/maven/**"]
  "Configure glob patterns for excluding folders"
  :type '(repeat string))

(defcustom lsp-java-content-provider-preferred nil
  "Preferred content provider (a 3rd party decompiler id,
usually)"
  :type 'string)

(defcustom lsp-java-autobuild-enabled t
  "Enable/disable the 'auto build'"
  :type 'boolean)

(defcustom lsp-java-selection-enabled t
  "Enable/disable the 'auto build'"
  :type 'boolean)

(defcustom lsp-java-max-concurrent-builds 1
  "Max simultaneous project builds"
  :type 'number)

(defcustom lsp-java-completion-enabled t
  "Enable/disable code completion support"
  :type 'boolean)

(defcustom lsp-java-completion-overwrite t
  "When set to true, code completion overwrites the current text.
When set to false, code is simply added instead."
  :type 'boolean)

(defcustom lsp-java-completion-guess-method-arguments t
  "When set to true, method arguments are guessed when a method
is selected from as list of code assist proposals."
  :type 'boolean)

(defcustom lsp-java-completion-favorite-static-members ["org.junit.Assert.*" "org.junit.Assume.*" "org.junit.jupiter.api.Assertions.*" "org.junit.jupiter.api.Assumptions.*" "org.junit.jupiter.api.DynamicContainer.*" "org.junit.jupiter.api.DynamicTest.*" "org.mockito.Mockito.*" "org.mockito.ArgumentMatchers.*" "org.mockito.Answers.*"]
  "Defines a list of static members or types with static members.
Content assist will propose those static members even if the
import is missing."
  :type '(repeat string))

(defcustom lsp-java-completion-import-order ["java" "javax" "com" "org"]
  "Defines the sorting order of import statements. A package or
type name prefix (e.g. 'org.eclipse') is a valid entry. An import
is always added to the most specific group."
  :type '(repeat string))

(defcustom lsp-java-folding-range-enabled t
  "Enable/disable smart folding range support. If disabled, it
will use the default indentation-based folding range provided by
VS Code."
  :type 'boolean)

(defcustom lsp-java-progress-reports-enabled t
  "[Experimental] Enable/disable progress reports from background
processes on the server."
  :type 'boolean)

(defcustom lsp-java-format-settings-url nil
  "Specifies the url or file path to the [Eclipse formatter xml
settings](https://github.com/redhat-developer/vscode-java/wiki/Formatter-settings)."
  :type 'string)

(defcustom lsp-java-format-settings-profile nil
  "Optional formatter profile name from the Eclipse formatter
settings."
  :type 'string)

(defcustom lsp-java-format-comments-enabled t
  "Includes the comments during code formatting."
  :type 'boolean)

(defcustom lsp-java-format-on-type-enabled t
  "Enable/disable automatic block formatting when typing `;`,
`<enter>` or `}`"
  :type 'boolean)

(defcustom lsp-java-bundles nil
  "List of bundles that will be loaded in the JDT server."
  :group 'lsp-java
  :type 'list)
(defcustom lsp-java-code-generation-hash-code-equals-use-java7objects nil
  "Use Objects.hash and Objects.equals when generating the
hashCode and equals methods. This setting only applies to Java 7
and higher."
  :type 'boolean)

(defcustom lsp-java-code-generation-hash-code-equals-use-instanceof nil
  "Use 'instanceof' to compare types when generating the hashCode
and equals methods."
  :type 'boolean)

(defcustom lsp-java-code-generation-use-blocks nil
  "Use blocks in 'if' statements when generating the methods."
  :type 'boolean)

(defcustom lsp-java-code-generation-generate-comments nil
  "Generate method comments when generating the methods."
  :type 'boolean)

(defcustom lsp-java-code-generation-to-string-template "${object.className} [${member.name()}=${member.value}, ${otherMembers}]"
  "The template for generating the toString method."
  :type 'string)

(defcustom lsp-java-code-generation-to-string-code-style "STRING_CONCATENATION"
  "The code style for generating the toString method."
  :type '(choice (:tag "STRING_CONCATENATION" "STRING_BUILDER" "STRING_BUILDER_CHAINED" "STRING_FORMAT")))

(defcustom lsp-java-code-generation-to-string-skip-null-values nil
  "Skip null values when generating the toString method."
  :type 'boolean)

(defcustom lsp-java-code-generation-to-string-list-array-contents t
  "List contents of arrays instead of using native toString()."
  :type 'boolean)

(defcustom lsp-java-code-generation-to-string-limit-elements 0
  "Limit number of items in arrays/collections/maps to list, if 0
then list all."
  :type 'number)

(defcustom lsp-java-completion-filtered-types ["java.awt.*" "com.sun.*"]
  "Defines the type filters. All types whose fully qualified name
matches the selected filter strings will be ignored in content
assist or quick fix proposals and when organizing imports. For
example 'java.awt.*' will hide all types from the awt packages."
  :type 'vector)

(lsp-register-custom-settings
 '(("java.codeGeneration.toString.limitElements" lsp-java-code-generation-to-string-limit-elements)
   ("java.codeGeneration.toString.listArrayContents" lsp-java-code-generation-to-string-list-array-contents t)
   ("java.codeGeneration.toString.skipNullValues" lsp-java-code-generation-to-string-skip-null-values t)
   ("java.codeGeneration.toString.codeStyle" lsp-java-code-generation-to-string-code-style)
   ("java.codeGeneration.toString.template" lsp-java-code-generation-to-string-template)
   ("java.codeGeneration.generateComments" lsp-java-code-generation-generate-comments t)
   ("java.codeGeneration.useBlocks" lsp-java-code-generation-use-blocks t)
   ("java.codeGeneration.hashCodeEquals.useInstanceof" lsp-java-code-generation-hash-code-equals-use-instanceof t)
   ("java.codeGeneration.hashCodeEquals.useJava7Objects" lsp-java-code-generation-hash-code-equals-use-java7objects t)
   ("java.format.onType.enabled" lsp-java-format-on-type-enabled t)
   ("java.format.comments.enabled" lsp-java-format-comments-enabled t)
   ("java.format.settings.profile" lsp-java-format-settings-profile)
   ("java.format.settings.url" lsp-java-format-settings-url)
   ("java.progressReports.enabled" lsp-java-progress-reports-enabled t)
   ("java.foldingRange.enabled" lsp-java-folding-range-enabled t)
   ("java.completion.importOrder" lsp-java-completion-import-order)
   ("java.completion.favoriteStaticMembers" lsp-java-completion-favorite-static-members)
   ("java.completion.guessMethodArguments" lsp-java-completion-guess-method-arguments t)
   ("java.completion.overwrite" lsp-java-completion-overwrite t)
   ("java.completion.enabled" lsp-java-completion-enabled t)
   ("java.maxConcurrentBuilds" lsp-java-max-concurrent-builds)
   ("java.autobuild.enabled" lsp-java-autobuild-enabled t)
   ("java.selection.enabled" lsp-java-autobuild-enabled t)
   ("java.contentProvider.preferred" lsp-java-content-provider-preferred)
   ("java.import.exclusions" lsp-java-import-exclusions)
   ("java.saveActions.organizeImports" lsp-java-save-actions-organize-imports t)
   ("java.format.enabled" lsp-java-format-enabled t)
   ("java.configuration.maven.userSettings" lsp-java-configuration-maven-user-settings)
   ("java.implementationsCodeLens.enabled" lsp-java-implementations-code-lens-enabled t)
   ("java.signatureHelp.enabled" lsp-java-signature-help-enabled t)
   ("java.referencesCodeLens.enabled" lsp-java-references-code-lens-enabled t)
   ("java.maven.downloadSources" lsp-java-maven-download-sources t)
   ("java.import.maven.enabled" lsp-java-import-maven-enabled t)
   ("java.import.gradle.enabled" lsp-java-import-gradle-enabled t)
   ("java.trace.server" lsp-java-trace-server)
   ("java.configuration.updateBuildConfiguration" lsp-java-configuration-update-build-configuration)
   ("java.configuration.checkProjectSettingsExclusions" lsp-java-configuration-check-project-settings-exclusions t)
   ("java.errors.incompleteClasspath.severity" lsp-java-errors-incomplete-classpath-severity)
   ("java.dependency.packagePresentation" lsp-java-dependency-package-representation)
   ("java.completion.filteredTypes" lsp-java-completion-filtered-types)))

(defcustom lsp-java-inhibit-message t
  "If non-nil, inhibit java messages echo via `inhibit-message'."
  :type 'boolean
  :group 'lsp-mode)

(defvar lsp-java--download-root "https://raw.githubusercontent.com/emacs-lsp/lsp-java/master/install/")

(defun lsp-java--json-bool (param)
  "Return a PARAM for setting parsable by json.el for booleans."
  (or param :json-false))

(defun lsp-java--list-or-empty (param)
  "Return either PARAM or empty vector in case PARAM is nil."
  (or param (vector)))

(defvar lsp-java-buffer-configurations
  `(("*classpath*" . ((side . right) (slot . 10) (window-width . 0.20)))))

(defun lsp-java-show-buffer (buf)
  "Show BUF according to defined rules."
  (let ((win (display-buffer-in-side-window buf
                                            (or (-> buf
                                                    buffer-name
                                                    (assoc lsp-java-buffer-configurations)
                                                    rest)
                                                '((side . right)
                                                  (slot . 1)
                                                  (window-width . 0.20))))))
    (set-window-dedicated-p win t)
    (select-window win)))

(defun lsp-java--locate-server-jar ()
  "Return the jar file location of the language server.

The entry point of the language server is in `lsp-java-server-install-dir'/plugins/org.eclipse.equinox.launcher_`version'.jar."
  (let ((plugindir (expand-file-name "plugins" lsp-java-server-install-dir)))
    (unless (file-directory-p plugindir)
      (if (yes-or-no-p "Server is not installed. Do you want to install it?")
          (lsp-java--ensure-server)
        (error "LSP Java cannot be started without JDT LS Server")))
    (let ((server-jar-filenames (directory-files plugindir t "org.eclipse.equinox.launcher_.*.jar$")))
      (if (not (= (length server-jar-filenames) 1))
          (error (format "Unable to find single point of entry %s" server-jar-filenames))
        (car server-jar-filenames)))))

(defun lsp-java--locate-server-config ()
  "Return the server config based on OS."
  (let ((config (cond
                 ((string-equal system-type "windows-nt") ; Microsoft Windows
                  "config_win")
                 ((string-equal system-type "darwin") ; Mac OS X
                  "config_mac")
                 (t "config_linux"))))
    (let ((inhibit-message t))
      (message (format "using config for %s" config)))
    (expand-file-name config lsp-java-server-install-dir)))

(defun lsp-java--current-workspace-or-lose ()
  "Look for the jdt-ls workspace."
  (or lsp--cur-workspace
      (lsp-find-workspace 'jdtls (buffer-file-name))
      (error "Unable to find workspace")))

(defmacro lsp-java-with-jdtls  (&rest body)
  "Helper macro for invoking BODY against WORKSPACE context."
  (declare (debug (form body))
           (indent 0))
  `(let ((lsp--cur-workspace (lsp-java--current-workspace-or-lose))) ,@body))

(defun lsp-java-build-project (&optional full)
  "Perform project build action.

FULL specify whether full or incremental build will be performed."
  (interactive "P" )
  (lsp-java-with-jdtls
    (lsp-request-async
     "java/buildWorkspace"
     (lsp-json-bool full)
     (lambda (result)
       (pcase result
         (1 (lsp--info "Successfully build project."))
         (2 (lsp--error "Failed to build project."))))
     :mode 'detached)))

(defun lsp-java-update-project-configuration ()
  "Update project configuration."
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (if (or (string= file-name "pom.xml") (string-match "\\.gradle" file-name))
        (lsp-java-with-jdtls
          (lsp-notify "java/projectConfigurationUpdate"
                      (lsp--text-document-identifier)))
      (error "Update configuration could be called only from build file(pom.xml or gradle build file)"))))

(defun lsp-java--ensure-dir (path)
  "Ensure that directory PATH exists."
  (unless (file-directory-p path)
    (make-directory path t)))

(cl-defmethod lsp-execute-command
  (_server (command (eql java.show.references)) params)
  (if-let (refs (seq-elt params 2))
      (xref--show-xrefs (lsp--locations-to-xref-items refs) nil)
    (user-error "No references")))

(cl-defmethod lsp-execute-command
  (_server (command (eql java.show.implementations)) params)
  (if-let (refs (seq-elt params 2))
      (xref--show-xrefs (lsp--locations-to-xref-items refs) nil)
    (user-error "No implementations")))

(defun lsp-java--get-java-version ()
  "Retrieve the java version from shell command."
  (let* ((java-version-output (shell-command-to-string (concat lsp-java-java-path " -version")))
         (version-string (nth 2 (split-string java-version-output))))
    (string-to-number (replace-regexp-in-string "\"" "" version-string))))

(defun lsp-java--java-9-plus-p ()
  "Check if java version is greater than or equal to 9."
  (let ((java-version (lsp-java--get-java-version)))
    (>= java-version 9)))

(defun lsp-java--ls-command ()
  "LS startup command."
  (let ((server-jar (lsp-java--locate-server-jar))
        (server-config (lsp-java--locate-server-config))
        (java-9-args (when (lsp-java--java-9-plus-p)
                       lsp-java-9-args)))
    (lsp-java--ensure-dir lsp-java-workspace-dir)
    `(,lsp-java-java-path
      "-Declipse.application=org.eclipse.jdt.ls.core.id1"
      "-Dosgi.bundles.defaultStartLevel=4"
      "-Declipse.product=org.eclipse.jdt.ls.core.product"
      "-Dlog.protocol=true"
      "-Dlog.level=ALL"
      ,@lsp-java-vmargs
      "-jar"
      ,server-jar
      "-configuration"
      ,server-config
      "-data"
      ,lsp-java-workspace-dir
      ,@java-9-args)))

(defun lsp-java--get-root ()
  "Retrieves the root directory of the java project root if available.

The current directory is assumed to be the java project’s root otherwise."
  (cond
   ;; the cache directory root
   ((string= default-directory lsp-java-workspace-cache-dir) default-directory)
   ((and (featurep 'projectile) (projectile-project-p)) (projectile-project-root))
   ((vc-backend default-directory) (expand-file-name (vc-root-dir)))
   (t (let ((project-types '("pom.xml" "build.gradle" ".project")))
        (or (seq-some (lambda (file) (locate-dominating-file default-directory file)) project-types)
            default-directory)))))

(defun lsp-java--language-status-callback (workspace params)
  "Callback for client initialized.

WORKSPACE is the currently active workspace.
PARAMS the parameters for language status notifications."
  (let ((status (gethash "type" params))
        (current-status (lsp-workspace-get-metadata "status" workspace)))
    ;; process the status message only if there is no status or if the status is
    ;; starting (workaround for bug https://github.com/eclipse/eclipse.jdt.ls/issues/651)
    (when (not (and (or (string= current-status "Error" )
                        (string= current-status "Started" ))
                    (string= "Starting" status)))
      (lsp-workspace-status (concat "::" status) workspace)
      (lsp-workspace-set-metadata "status" status workspace)
      (let ((inhibit-message lsp-java-inhibit-message))
        (lsp-log "%s[%s]" (gethash "message" params) (gethash "type" params))))))

(defun lsp-java--apply-workspace-edit (action)
  "Callback for java/applyWorkspaceEdit.

ACTION is the action to execute."
  (lsp--apply-workspace-edit (lsp-seq-first (gethash "arguments" action))))

(defun lsp-java--actionable-notification-callback (_workspace params)
  "Handler for actionable notifications.

WORKSPACE is the currently active workspace.
PARAMS the parameters for actionable notifications."
  (lsp--warn (gethash "message" params)))

(defun lsp-java--progress-report (_workspace params)
  "Progress report handling.

PARAMS progress report notification data."
  (-let [(&hash "status" "complete") params]
    (setq lsp-java-progress-string (propertize status 'face 'lsp-java-progress-face))
    (when complete
      (run-with-idle-timer 0.8 nil (lambda ()
                                     (setq lsp-java-progress-string nil))))))

(put 'lsp-java-progress-string 'risky-local-variable t)

(defun lsp-java--render-string (str)
  "Render STR with `java-mode' syntax highlight."
  (condition-case nil
      (with-temp-buffer
        (delay-mode-hooks (java-mode))
        (insert str)
        (font-lock-ensure)
        (buffer-string))
    (error str)))

(defun lsp-java--prepare-mvnw ()
  "Download mvnw."
  (let ((mvn-executable (if (string-equal system-type "windows-nt")
                            "mvnw.cmd"
                          "mvnw"))
        (downloader ".mvn/wrapper/MavenWrapperDownloader.java")
        (properties ".mvn/wrapper/maven-wrapper.properties"))
    (mkdir ".mvn/wrapper/" t)
    (url-copy-file (concat lsp-java--download-root mvn-executable)
                   mvn-executable
                   t)
    (url-copy-file (concat lsp-java--download-root downloader) downloader t)
    (url-copy-file (concat lsp-java--download-root properties) properties t)
    (if (string= system-type "windows-nt")
        mvn-executable
      (concat "sh " mvn-executable))))

(defun lsp-java--bundles-dir ()
  "Get default bundles dir."
  (concat (file-name-as-directory lsp-java-server-install-dir) "bundles"))

(defun lsp-java--ensure-server (&optional async)
  "Ensure that JDT server and the other configuration."
  (let* ((default-directory (make-temp-file "lsp-java-install" t)))
    (unwind-protect
        (progn
          (url-copy-file (concat lsp-java--download-root "pom.xml") "pom.xml" t)
          (let ((full-command (format
                               "%s -Djdt.js.server.root=%s -Djunit.runner.root=%s -Djunit.runner.fileName=%s -Djava.debug.root=%s clean package -Djdt.download.url=%s"
                               (or (let ((mvn-executable (executable-find "mvn")))
                                     ;; Quote path to maven executable if it has spaces.
                                     (if (and mvn-executable (string-match "\s" mvn-executable))
                                         (format "\"%s\"" mvn-executable)
                                       mvn-executable))
                                   (lsp-java--prepare-mvnw))
                               (expand-file-name lsp-java-server-install-dir)
                               (expand-file-name
                                (if (boundp 'dap-java-test-runner)
                                    (file-name-directory dap-java-test-runner)
                                  (concat (file-name-directory lsp-java-server-install-dir) "test-runner")))
                               (if (boundp 'dap-java-test-runner)
                                   (file-name-nondirectory (directory-file-name dap-java-test-runner))
                                 "junit-platform-console-standalone.jar")
                               (expand-file-name (lsp-java--bundles-dir))
                               lsp-java-jdt-download-url)))
            (if async
                (async-shell-command full-command)
              (message "Running %s" full-command)
              (unless (zerop (shell-command full-command))
                (user-error "Failed to install lsp server using '%s'" full-command))
              (delete-directory default-directory t)))))))

(defun lsp-java-update-server ()
  "Update LDT LS server."
  (interactive)
  (lsp--info "Server update started...")
  (lsp-java--ensure-server t))

(defun lsp-java--workspace-notify (&rest _args)
  "Workspace notify handler.")

(defun lsp-java--get-filename (url)
  "Get the name of the buffer calculating it based on URL."
  (or (save-match-data
        (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" url)
          (format "%s.java"
                  (replace-regexp-in-string "/" "." (match-string 2 url) t t))))
      (-when-let ((_ file-name _ jar)
                  (s-match
                   "jdt://.*?/\\(.*?\\)\\?=\\(.*?\\)/.*/\\(.*\\)"
                   (url-unhex-string url)))
        (format "%s(%s)" file-name
                (->> jar
                     (s-replace "/" "")
                     (s-replace "\\" ""))))
      (save-match-data
        (when (string-match "chelib://\\(.*\\)" url)
          (let ((matched (match-string 1 url)))
            (replace-regexp-in-string (regexp-quote ".jar") "jar" matched t t))))
      (error "Unable to match %s" url)))

(defun lsp-java--get-metadata-location (file-location)
  "Given a FILE-LOCATION return the file containing the metadata for the file."
  (format "%s.%s.metadata"
          (file-name-directory file-location)
          (file-name-base file-location)))

(defun lsp-java--resolve-uri (uri)
  "Load a file corresponding to URI executing request to the jdt server."
  (let* ((buffer-name (lsp-java--get-filename uri))
         (file-location (concat lsp-java-workspace-cache-dir buffer-name)))
    (unless (file-readable-p file-location)
      (lsp-java--ensure-dir (file-name-directory file-location))
      (with-lsp-workspace (lsp-find-workspace 'jdtls nil)
        (let ((content (lsp-send-request (lsp-make-request
                                          "java/classFileContents"
                                          (list :uri uri)))))
          (with-temp-file file-location
            (insert content))
          (with-temp-file (lsp-java--get-metadata-location file-location)
            (insert uri)))))
    file-location))

(defun lsp-java-actionable-notifications ()
  "Lists current actionable notifications."
  (interactive)
  (when-let ((notifications (lsp-workspace-get-metadata "actionable-notifications"))
             (selected-notification (completing-read
                                     "Select notification to fix:"
                                     notifications
                                     nil
                                     t))
             (commands (gethash "commands"
                                (gethash selected-notification notifications))))
    (lsp-execute-code-action (lsp--select-action commands))
    (remhash selected-notification notifications)
    (when (= (hash-table-count notifications) 0)
      (lsp-workspace-status (concat "::" (lsp-workspace-get-metadata "status"))))))

(defun lsp-java-execute-matching-action (regexp &optional not-found-message)
  "Execute the code action which title match the REGEXP.
NOT-FOUND-MESSAGE will be used if there is no matching action."
  (let ((actions (cl-remove-if-not
                  (lambda (item) (string-match regexp (gethash "title" item)))
                  (lsp-get-or-calculate-code-actions))))
    (pcase (length actions)
      (0 (error (or not-found-message "Unable to find action")))
      (1 (lsp-execute-code-action (car actions)))
      (_ (lsp-execute-code-action (lsp--select-action actions))))))

(defun lsp-java-extract-to-local-variable (arg)
  "Extract local variable refactoring.
The prefix ARG decide whether to act on all or only on the
current symbol."
  (interactive "P")
  (lsp-java-execute-matching-action
   (if arg
       "Extract to local variable$"
     "Extract to local variable (replace all occurrences)")))

(defun lsp-java-convert-to-static-import (arg)
  "Convert to static import.
The prefix ARG decide whether to act on all or only on the
current symbol."
  (interactive "P")
  (lsp-java-execute-matching-action
   (if arg
       "Convert to static import$"
     "Convert to static import (replace all occurrences)$")))

(defun lsp-java-extract-to-constant ()
  "Extract constant refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Extract to constant"))

(defun lsp-java-add-throws ()
  "Extract constant refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Add throws declaration"))

(defun lsp-java-add-unimplemented-methods ()
  "Extract constant refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Add unimplemented methods"))

(defun lsp-java-create-parameter ()
  "Create parameter refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Create parameter '"))

(defun lsp-java-create-field ()
  "Create field refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Create field '"))

(defun lsp-java-create-local ()
  "Create local refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Create local variable"))

(defun lsp-java-extract-method ()
  "Extract method refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Extract to method"))

(defun lsp-java-assign-to-field ()
  "Assign to new field."
  (interactive)
  (lsp-java-execute-matching-action "Assign parameter to new field"))

(defun lsp-java-assign-all ()
  "Assign to new field."
  (interactive)
  (lsp-java-execute-matching-action "Assign all parameters to new fields"))

(defun lsp-java-add-import ()
  "Add missing import."
  (interactive)
  (lsp-java-execute-matching-action "Import '.*'"))

(defun lsp-java--bundles ()
  "Get lsp java bundles."
  (let ((bundles-dir (lsp-java--bundles-dir)))
    (append lsp-java-bundles (when (file-directory-p bundles-dir)
                               (apply 'vector (directory-files bundles-dir t "\\.jar$"))))))

(defun lsp-java--workspace-folders (workspace)
  "Return WORKSPACE folders."
  (lsp-session-folders (lsp-session)))

(defun lsp-java--find-workspace (file-uri)
  "Return the workspace corresponding FILE-URI."
  (lsp-find-workspace 'jdtls (lsp--uri-to-path file-uri)))

(defun lsp-java--find-project-uri (file-uri)
  "Return the java project corresponding FILE-URI."
  (let ((workspace (lsp-java--current-workspace-or-lose))
        (session-folder (lsp-find-session-folder (lsp-session) (buffer-file-name))))
    (with-lsp-workspace workspace
      ;; look for a maven nested project or fallback to the session folder root.
      (let ((project-folder (or (->> session-folder
                                     lsp--path-to-uri
                                     (lsp-send-execute-command "che.jdt.ls.extension.mavenProjects")
                                     (--filter (f-ancestor-of? (lsp--uri-to-path (file-name-as-directory it)) buffer-file-name))
                                     (--max-by (> (length it) (length other))))
                                (lsp--path-to-uri (file-name-as-directory session-folder)))))
        project-folder))))

(cl-defmethod lsp-execute-command
  (_server (command (eql java.show.references)) params)
  (if-let (refs (cl-third (append params nil)))
      (lsp-show-xrefs (lsp--locations-to-xref-items refs) nil t)
    (user-error "No references")))

(cl-defmethod lsp-execute-command
  (_server (command (eql java.show.implementations)) params)
  (if-let (refs (cl-third (append params nil)))
      (lsp-show-xrefs (lsp--locations-to-xref-items refs) nil t)
    (user-error "No implementations")))

(add-to-list 'global-mode-string (list '(t lsp-java-progress-string)))

(defun lsp-java-boot--workspace-execute-client-command (_jdt-ls-workspace params)
  "PARAMS is the classpath info."
  (-let (((&hash "command" "arguments") params))
    (pcase command
      ("java.action.organizeImports.chooseImports"
       (-let (([file-uri imports] (gethash "arguments" params)))
         (with-current-buffer (find-file (lsp--uri-to-path file-uri))
           (->> imports
                (seq-map (-lambda ((&hash "candidates" "range"))
                           (-let (((beg .  end) (lsp--range-to-region range)))
                             (goto-char beg)
                             (recenter nil))
                           (lsp--completing-read "Select class to import: "
                                                 candidates
                                                 (-partial #'gethash "fullyQualifiedName"))))
                (apply #'vector)))))
      (_ (ignore
          (with-lsp-workspace (lsp-find-workspace 'boot-ls nil)
            (aset arguments 2 (if (seq-elt arguments 2) t :json-false))
            (lsp-request "workspace/executeCommand"
                         (list :command command :arguments arguments))))))))

(defun lsp-java-generate-to-string ()
  "Generate `toString' method."
  (interactive)
  (lsp-execute-code-action-by-kind "source.generate.toString"))

(defun lsp-java-generate-equals-and-hash-code ()
  "Generate `hashCode' and `equals' methods."
  (interactive)
  (lsp-execute-code-action-by-kind "source.generate.hashCodeEquals"))

(defun lsp-java-generate-overrides ()
  "Override methods."
  (interactive)
  (lsp-execute-code-action-by-kind "source.overrideMethods"))

(defun lsp-java-organize-imports ()
  "Generate `toString' method."
  (interactive)
  (lsp-execute-code-action-by-kind "source.organizeImports"))

(defun lsp-java-generate-getters-and-setters ()
  "Generate getter and setters with prompt."
  (interactive)
  (lsp-execute-code-action-by-kind "source.generate.accessors"))

(defun lsp-java-open-super-implementation ()
  "Open super implementation."
  (interactive)
  (if-let ((locations (append (lsp-request "java/findLinks"
                                           (list :type "superImplementation"
                                                 :position (lsp--text-document-position-params)))
                              nil)))
      (lsp-show-xrefs (lsp--locations-to-xref-items locations) nil nil)
    (user-error "No super implementations.")))

(defvar lsp-java--helm-result nil)

(defun lsp-java--completing-read-multiple (message items initial-selection)
  (if (functionp 'helm)
      (progn
        (require 'helm-source)
        (helm :sources (helm-make-source
                           message 'helm-source-sync :candidates items
                           :action '(("Identity" lambda (_)
                                      (setq lsp-java--helm-result (helm-marked-candidates)))))
              :buffer "*lsp-java select*"
              :prompt message)
        lsp-java--helm-result)
    (if (functionp 'ivy-read)
        (let (result)
          (ivy-read message (mapcar #'car items)
                    :action (lambda (c) (setq result (list (cdr (assoc c items)))))
                    :multi-action
                    (lambda (canditates)
                      (setq result (mapcar (lambda (c) (cdr (assoc c items))) canditates))))
          result)
      (let ((deps initial-selection) dep)
        (while (setq dep (cl-rest (lsp--completing-read
                                   (if deps
                                       (format "%s (selected %s): " message (length deps))
                                     (concat message ": "))
                                   items
                                   (-lambda ((name . id))
                                     (if (-contains? deps id)
                                         (concat name " ✓")
                                       name)))))
          (if (-contains? deps dep)
              (setq deps (remove dep deps))
            (cl-pushnew dep deps)))
        deps))))

(defun lsp-java--apply-document-changes (response)
  "Apply document CHANGES."
  (when response
    (ht-amap (with-current-buffer (find-file-noselect (lsp--uri-to-path key))
               (lsp--apply-text-edits value))
             (gethash "changes" response))))

(defun lsp-java--action-generate-to-string (action)
  (lsp-java-with-jdtls
    (-let* ((context (lsp-seq-first (gethash "arguments" action)))
            ((&hash "fields" "exists") (lsp-request "java/checkToStringStatus" context))
            (fields-data (-map (-lambda ((field &as &hash "name" "type"))
                                 (cons (format "%s: %s" name type) field))
                               fields)))
      (when (or (not exists) (y-or-n-p "The equals method already exists. Replace?") )
        (let ((selected-fields (lsp-java--completing-read-multiple
                                "Select fields to include"
                                fields-data
                                (-map #'cl-rest fields-data))))
          (lsp-java--apply-document-changes
           (lsp-request "java/generateToString"
                        (list :fields (apply #'vector selected-fields)
                              :context context))))))))

(defun lsp-java--action-generate-equals-and-hash-code (action)
  (lsp-java-with-jdtls
    (-let* ((context (lsp-seq-first (gethash "arguments" action)))
            ((&hash "fields" "existingMethods" methods) (lsp-request "java/checkHashCodeEqualsStatus" context))
            (fields-data (-map (-lambda ((field &as &hash "name" "type"))
                                 (cons (format "%s: %s" name type) field))
                               fields)))
      (when (or (seq-empty-p methods) (y-or-n-p (format "The %s method already exists. Replace?" methods)) )
        (let* ((selected-fields (lsp-java--completing-read-multiple "Select fields to include"
                                                                    fields-data (-map #'cl-rest fields-data))))
          (lsp-java--apply-document-changes
           (lsp-request "java/generateHashCodeEquals"
                        (list :fields (apply #'vector selected-fields)
                              :context context
                              :regenerate (not (null methods))))))))))

(defun lsp-java--action-organize-imports (action)
  (lsp-java-with-jdtls
    (let ((context (lsp-seq-first (gethash "arguments" action))))
      (lsp-request-async
       "java/organizeImports" context
       (lambda (result)
         (ht-amap (with-current-buffer (find-file-noselect (lsp--uri-to-path key))
                    (lsp--apply-text-edits value))
                  (gethash "changes" result)))
       :mode 'detached))))

(defun lsp-java--override-methods-prompt (action)
  (lsp-java-with-jdtls
    (let* ((context (lsp-seq-first (gethash "arguments" action)))
           (result (lsp-request "java/listOverridableMethods" context))
           (methods-data (-map (-lambda ((field &as &hash "name" "parameters" "declaringClass" class))
                                 (cons (format "%s(%s) class: %s" name (s-join ", " parameters) class) field))
                               (gethash "methods" result)))
           (methods-to-override (lsp-java--completing-read-multiple
                                 "Select methods to override"
                                 methods-data
                                 (-map #'cl-rest methods-data))))
      (lsp-java--apply-document-changes
       (lsp-request "java/addOverridableMethods"
                    (list :overridableMethods (apply #'vector methods-to-override)
                          :context context))))))

(defun lsp-java--generate-accessors-prompt (action)
  (lsp-java-with-jdtls
    (let* ((context (lsp-seq-first (gethash "arguments" action)))
           (result (lsp-request "java/resolveUnimplementedAccessors" context))
           (fields-data (-map (-lambda ((field &as &hash "fieldName" name
                                               "generateGetter" getter?
                                               "generateSetter" setter?
                                               "isStatic" static?))
                                (cons (format "%s" name) field))
                              result))
           (to-generate (lsp-java--completing-read-multiple
                         "Select getters/setters to generate"
                         fields-data
                         (-map #'cl-rest fields-data))))
      (lsp-java--apply-document-changes
       (lsp-request "java/generateAccessors"
                    (list :accessors(apply #'vector to-generate)
                          :context context))))))

(defun lsp-java--generate-constructors-prompt (action)
  (lsp-java-with-jdtls
    (-let* ((context (lsp-seq-first (gethash "arguments" action)))
            ((all &as &hash "constructors" "fields") (lsp-request "java/checkConstructorsStatus" context))
            (constructors (append constructors nil))
            (selection-constructors (-map (-lambda ((field &as &hash "name" "parameters"))
                                            (cons (format "%s(%s)" name (s-join ", " parameters)) field))
                                          (append constructors nil)))

            (to-generate (if (cl-rest selection-constructors)
                             (lsp-java--completing-read-multiple
                              "Select constructors to generate"
                              selection-constructors
                              (-map #'cl-rest  selection-constructors))
                           (append  constructors nil)))
            (fields-source (-map (-lambda ((field &as &hash "name" "type"))
                                   (cons (format "%s: %s" name type) field))
                                 fields))
            (fields (when fields-source
                      (lsp-java--completing-read-multiple
                       "Select fields"
                       fields-source
                       (-map #'cl-rest fields-source)))))
      (lsp-java--apply-document-changes
       (lsp-request "java/generateConstructors"
                    (list :fields (apply #'vector fields)
                          :constructors (apply #'vector to-generate)
                          :context context))))))

(defun lsp-java-move-file (move-uris)
  (-let [destination (lsp--completing-read
                      (format "Select destination for %s: " (buffer-name))
                      (gethash "destinations"
                               (lsp-request "java/getMoveDestinations"
                                            (list :moveKind "moveResource"
                                                  :sourceUris move-uris
                                                  :params nil)))
                      (-lambda ((&hash "displayName" display-name "path"))
                        (format "%s - %s" display-name path)))]
    (when-let (move-uris (if-let (destination-folder (lsp--uri-to-path (gethash "uri" destination)))
                             (-let [(duplicated-files to-move)
                                    (--split-with
                                     (f-exists? (f-join destination-folder (f-filename it)))
                                     (append move-uris nil))]
                               (when duplicated-files
                                 (lsp-warn "The files %s already exist in the package %s. The move operation will ignore them."
                                           duplicated-files destination-folder))
                               (apply #'vector to-move))
                           move-uris))
      (lsp-java--apply-edit
       (lsp-request "java/move"
                    (list :moveKind "moveResource"
                          :sourceUris move-uris
                          :destination destination
                          :updateReferences t))))))

(defun lsp-java--apply-edit (to-apply)
  (-let [(&hash "edit" "message" "command") to-apply]
    (when message
      (lsp--error "%s" message))

    (when edit
      (lsp--apply-workspace-edit edit))

    (when command
      (lsp-execute-code-action command))))

(defun lsp-java--symbol-label (symbol)
  (-let [(&hash "name" "containerName") symbol]
    (format "%s.%s" containerName name)))

(defun lsp-java--move-type (context command-info)
  (-let ((document-uri (->> context
                            (gethash "textDocument")
                            (gethash "uri")))
         ((&hash "enclosingTypeName" enclosing-type-name
                 "displayName" display-name
                 "projectName" project-name
                 "supportedDestinationKinds" supported-destination-kinds) command-info))
    (lsp-java--apply-edit
     (lsp-request "java/move"
                  (if (string= "newFile"
                               (lsp--completing-read "What would you like to do? "
                                                     supported-destination-kinds
                                                     (lambda (kind)
                                                       (if (string= kind "newFile")
                                                           (format "Move type %s to new file" display-name)
                                                         (format "Move type %s to another class" display-name)))
                                                     nil
                                                     t))
                      (list :moveKind "moveTypeToNewFile"
                            :sourceUris (vector document-uri)
                            :params context)
                    (list :moveKind "moveTypeToClass"
                          :sourceUris (vector document-uri)
                          :params context
                          :destination (lsp-java--select-destination-class
                                        (list enclosing-type-name
                                              (format "%s.%s" enclosing-type-name
                                                      display-name))
                                        project-name)))))))

(defun lsp-java--select-destination-class (excluded project-name)
  (lsp--completing-read
   "Select class: "
   (->> (lsp-request "java/searchSymbols"
                     (list :query "*"
                           :projectName project-name
                           :sourceOnly t))
        (-filter (-lambda ((&hash "name" "containerName" container-name))
                   (not (-contains? excluded (format "%s.%s" container-name name)))))
        (--sort (s-less? (gethash "name" it)
                         (gethash "name" other))))
   #'lsp-java--symbol-label))

(defun lsp-java-move-static-member (context command-info)
  (-let [(&hash? "displayName" display-name
                 "enclosingTypeName" enclosing-type-name
                 "memberType" member-type
                 "projectName" project-name) command-info]
    (lsp-java--apply-edit
     (lsp-request "java/move"
                  (list :moveKind "moveStaticMember"
                        :sourceUris (->> context
                                         (gethash "textDocument")
                                         (gethash "uri")
                                         vector)
                        :params context
                        :destination (lsp-java--select-destination-class
                                      (list enclosing-type-name) project-name))))))

(defun lsp-java--move-instance-method (context command-info)
  (-let* ((document-uri (->> context
                             (gethash "textDocument")
                             (gethash "uri")))
          ((&hash "destinations" "errorMessage" message)
           (lsp-request "java/getMoveDestinations"
                        (list :moveKind "moveInstanceMethod"
                              :sourceUris (vector document-uri)
                              :params context))))
    (when message
      (user-error message))

    (when (seq-empty-p destinations)
      (user-error "Cannot find possible class targets to move the selected method to."))

    (lsp-java--apply-edit
     (lsp-request
      "java/move"
      (list
       :moveKind "moveInstanceMethod"
       :params context
       :sourceUris (vector )
       :destination (lsp--completing-read
                     (format "Select the new class for the instance method %s"
                             (gethash "methodName" command-info))
                     destinations
                     (-lambda ((&hash "name" "type" "isField" field?))
                       (format "%s.%s (%s)" type name (if field?
                                                          "Field"
                                                        "Method Parameter")))
                     nil
                     t))))))

(defun lsp-java--apply-refactoring-command (action)
  (lsp-java-with-jdtls
    (-let [(command context command-info) (append (gethash "arguments" action) nil)]
      (cond
       ((-contains? '("extractVariable"
                      "extractVariableAllOccurrence"
                      "extractConstant"
                      "extractMethod"
                      "extractField"
                      "convertVariableToField"
                      "invertVariable"
                      "convertAnonymousClassToNestedCommand")
                    command)
        (-let ((arguments (when (memq command '("extractField" "convertVariableToField"))
                            (when-let (scope (pcase (append (gethash "initializedScopes" command-info) nil)
                                               (`nil nil)
                                               (`(,scope) scope)
                                               (scopes (or (completing-read "Initialize the field in: " scopes nil t)
                                                           (user-error "Cancelled...")))))
                              (vector scope)))))
          (lsp-java--apply-edit
           (lsp-request
            "java/getRefactorEdit"
            (list :command command
                  :context context
                  :options (plist-get (lsp--make-document-formatting-params) :options)
                  :commandArguments arguments)))))
       ((string= command "moveFile") (lsp-java-move-file (vector (gethash "uri" command-info))))
       ((string= command "moveStaticMember") (lsp-java-move-static-member context command-info))
       ((string= command "moveInstanceMethod") (lsp-java--move-instance-method context command-info))
       ((string= command "moveType") (lsp-java--move-type context command-info))))))

(defun lsp-java--action-rename (action)
  (-let* (([(&hash "uri" "offset" "length")] (gethash "arguments" action)))
    (with-current-buffer (find-file (lsp--uri-to-path uri))
      (deactivate-mark)
      (goto-char (1+ offset))
      (set-mark (point))
      (goto-char (+ (point) length))
      (call-interactively 'lsp-rename)
      (deactivate-mark))))

(lsp-register-client
 (make-lsp--client
  :new-connection (lsp-stdio-connection 'lsp-java--ls-command)
  :major-modes '(java-mode)
  :server-id 'jdtls
  :multi-root t
  :notification-handlers (ht ("language/status" #'lsp-java--language-status-callback)
                             ("language/actionableNotification" #'lsp-java--actionable-notification-callback)
                             ("language/progressReport" #'lsp-java--progress-report)
                             ("workspace/notify" #'lsp-java--workspace-notify))
  :request-handlers (ht ("workspace/executeClientCommand" 'lsp-java-boot--workspace-execute-client-command))
  :action-handlers (ht ("java.apply.workspaceEdit" #'lsp-java--apply-workspace-edit)
                       ("java.action.generateToStringPrompt" #'lsp-java--action-generate-to-string)
                       ("java.action.hashCodeEqualsPrompt" #'lsp-java--action-generate-equals-and-hash-code)
                       ("java.action.organizeImports" #'lsp-java--action-organize-imports)
                       ("java.action.overrideMethodsPrompt" #'lsp-java--override-methods-prompt)
                       ("java.action.generateAccessorsPrompt" #'lsp-java--generate-accessors-prompt)
                       ("java.action.generateConstructorsPrompt" #'lsp-java--generate-constructors-prompt)
                       ("java.action.applyRefactoringCommand" #'lsp-java--apply-refactoring-command)
                       ("java.action.rename" 'lsp-java--action-rename))
  :uri-handlers (ht ("jdt" #'lsp-java--resolve-uri)
                    ("chelib" #'lsp-java--resolve-uri))
  :initialization-options (lambda ()
                            (list :settings (lsp-configuration-section "java")
                                  :extendedClientCapabilities (list :progressReportProvider t
                                                                    :classFileContentsSupport t
                                                                    :progressReportProvider t
                                                                    :classFileContentsSupport t
                                                                    :overrideMethodsPromptSupport t
                                                                    :hashCodeEqualsPromptSupport t
                                                                    :advancedOrganizeImportsSupport t
                                                                    :generateConstructorsPromptSupport t
                                                                    :generateToStringPromptSupport t
                                                                    :advancedGenerateAccessorsSupport t
                                                                    :advancedExtractRefactoringSupport t
                                                                    :moveRefactoringSupport t)
                                  :bundles (lsp-java--bundles)
                                  :workspaceFolders (->> (lsp-session)
                                                         lsp-session-server-id->folders
                                                         (gethash 'jdtls)
                                                         (-uniq)
                                                         (-map #'lsp--path-to-uri)
                                                         (apply #'vector))))
  :library-folders-fn (lambda (_workspace) (list lsp-java-workspace-cache-dir))
  :before-file-open-fn (lambda (workspace)
                         (let ((metadata-file-name (lsp-java--get-metadata-location buffer-file-name)))
                           (setq-local lsp-buffer-uri
                                       (when (file-exists-p metadata-file-name)
                                         (with-temp-buffer (insert-file-contents metadata-file-name)
                                                           (buffer-string))))))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "java"))
                      (lsp--server-register-capability
                       (ht ("id" "test-id")
                           ("method" "workspace/didChangeWatchedFiles")
                           ("registerOptions" (ht ("watchers"
                                                   (vector (ht ("globPattern" "**/*.java"))
                                                           (ht ("globPattern" "**/pom.xml"))
                                                           (ht ("globPattern" "**/*.gradle"))
                                                           (ht ("globPattern" "**/.project"))
                                                           (ht ("globPattern" "**/.classpath"))
                                                           (ht ("globPattern" "**/settings/*.prefs"))))))))))
  :completion-in-comments? t))

(defun lsp-java-spring-initializr ()
  "Emacs frontend for https://start.spring.io/."
  (interactive)
  (let ((base-url "https://start.spring.io/"))
    (message "Requesting spring initializr data...")
    (request
     base-url
     :type "GET"
     :parser (lambda () (let ((json-array-type 'list)) (json-read)))
     :headers '(("Accept" . "application/vnd.initializr.v2.1+json"))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (cl-flet ((ask (message key) (alist-get 'id
                                                         (lsp--completing-read message
                                                                               (alist-get 'values (alist-get key data))
                                                                               (-partial 'alist-get 'name)))))
                   (condition-case _err
                       (-let* ((group-id (read-string "Enter group name: " "com.example"))
                               (artifact-id (read-string "Enter artifactId: " "demo"))
                               (description (read-string "Enter description: " "Demo project for Spring Boot"))
                               (boot-version (ask "Select boot-version: " 'bootVersion))
                               (java-version (ask "Select java-version: " 'javaVersion))
                               (language (ask "Select language: " 'language))
                               (packaging (ask "Select packaging: " 'packaging))
                               (base-url "https://start.spring.io/")
                               (package-name (read-string "Select package name: " "com.example.demo"))
                               (type (ask "Select type: " 'type))
                               (target-directory (read-directory-name "Select project directory: " default-directory))
                               (dependenciles-list (->> data
                                                        (alist-get 'dependencies)
                                                        (alist-get 'values)
                                                        (-map (-lambda ((&alist 'name 'values))
                                                                (-map (-lambda ((&alist 'id 'name dep-name 'description))
                                                                        (cons (format "%s / %s (%s)" name dep-name description) id)) values)))
                                                        (apply 'append)))
                               (temp-file (make-temp-file "spring-project" nil ".zip"))
                               (deps (lsp-java--completing-read-multiple "Select dependencies: " dependenciles-list nil)))
                         (let ((download-url (format "%sstarter.zip?type=%s&language=%s&groupId=%s&artifactId=%s&packaging=%s&bootVersion=%s&baseDir=%s&dependencies=%s"
                                                     base-url type language group-id artifact-id packaging boot-version artifact-id (s-join "," deps))))
                           (message "Downloading template from %s" download-url)
                           (if (executable-find "wget")
                               (shell-command (format "wget  -O  %s  '%s' " temp-file download-url))
                             (if (executable-find "curl")
                                   (shell-command (format "curl  -o  %s  '%s' " temp-file download-url))
                                 (url-copy-file download-url temp-file t)))
                           (if (executable-find "unzip")
                               (progn
                                 (shell-command (format "unzip %s -d %s" temp-file target-directory))
                                 (when (yes-or-no-p "Do you want to import the project?")
                                   (lsp-workspace-folders-add (f-join target-directory artifact-id)))
                                 (find-file (f-join target-directory artifact-id)))
                             (user-error "Unable to unzip tool - file %s cannot be extracted, extract it manually" temp-file))))
                     ('quit))))))))

;;;###autoload(with-eval-after-load 'lsp-mode (require 'lsp-java))

(provide 'lsp-java)
;;; lsp-java.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
