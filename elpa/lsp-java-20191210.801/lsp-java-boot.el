;;; lsp-java.el --- Spring boot support for lsp-java               -*- lexical-binding: t; -*-

;; Version: 2.0
;; Keywords: java
;; URL: https://github.com/emacs-lsp/lsp-java

;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0") (markdown-mode "2.3") (dash "2.14.1") (f "0.20.0") (ht "2.0") (dash-functional "1.2.0") (request "0.3.0"))

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

;;; Commentary: LSP Java support for Spring Boot.

;;; Code:

(require 'dash)
(require 'lsp-mode)
(require 'lsp-java)
(require 'cl)

(defcustom lsp-java-boot-enabled t
  "If non-nil start the boot server when opening java files."
  :group 'lsp-java-boot
  :type 'boolean)

(defcustom lsp-java-boot-java-tools-jar nil
  "Path to tools jar. If it is not specified it will be calculated using `JAVA_HOME'."
  :group 'lsp-java-boot
  :type 'file)

(defvar lsp-java-boot--callback nil)

(defun lsp-java-boot--find-tools-jar ()
  "Calculate the path to tools.jar."
  (let ((tools-jar (or lsp-java-boot-java-tools-jar
                       (f-join (getenv "JAVA_HOME") "lib/tools.jar"))))
    (unless (f-exists? tools-jar)
      (error "Please configure either JAVA_HOME or lsp-java-boot-java-tools-jar"))
    tools-jar))

(defun lsp-java-boot--sts-javadoc-hover-link (_workspace params)
  "Handler for java doc hover."
  (with-lsp-workspace (lsp-find-workspace 'jdtls nil)
    (lsp-request "workspace/executeCommand"
                 (list :command "sts.java.addClasspathListener"
                       :arguments (gethash "callbackCommandId" params))
                 :no-wait t)))

(defun lsp-java-boot--sts-add-classpath-listener (_workspace params)
  (ignore
   (with-lsp-workspace (lsp-find-workspace 'jdtls nil)
     (lsp-request "workspace/executeCommand"
                  (list :command "sts.java.addClasspathListener"
                        :arguments (gethash "callbackCommandId" params))
                  :no-wait t))))

(defun lsp-java-boot--lens-backend (_ callback)
  "Boot backend.
Store CALLBACK to use it `sts/highlight'."
  (setq-local lsp-java-boot--callback callback))

;;;###autoload
(define-minor-mode lsp-java-boot-lens-mode
  "Toggle code-lens overlays."
  :group 'lsp-java-boot
  :global nil
  :init-value nil
  :lighter "BLens"
  (cond
   (lsp-java-boot-lens-mode
    (setq-local lsp-lens-backends (pushnew 'lsp-java-boot--lens-backend lsp-lens-backends))
    (lsp-lens-refresh t))
   (t (setq-local lsp-lens-backends (delete 'lsp-java-boot--lens-backend lsp-lens-backends))
      (setq-local lsp-java-boot--callback nil))))

(cl-defmethod lsp-execute-command
  (server (command (eql sts.open.url)) params)
  (browse-url (seq-first params)))

(cl-defmethod lsp-execute-command (server (command (eql sts.showHoverAtPosition)) params)
  (goto-char (lsp--position-to-point (seq-first params)))
  (lsp-describe-thing-at-point))

(defun lsp-java-boot--sts-hightlight (workspace params)
  "WORKSPACE PARAMS."
  (with-lsp-workspace workspace
    (-let (((&hash "doc" (&hash "uri" "version") "codeLenses" code-lenses) params))
      (when-let (buf (find-buffer-visiting (lsp--uri-to-path uri)))
        (with-current-buffer buf
          (when (and lsp-java-boot--callback lsp-java-boot-lens-mode)
            (funcall lsp-java-boot--callback code-lenses version)))))))

(defun lsp-java-boot--server-jar ()
  "Return the spring boot jar."
  (or (-> lsp-java-server-install-dir
          (expand-file-name)
          (f-join "boot-server")
          f-files
          first)
      (lsp-log "Unable to find spring boot server jar.")))

(defun lsp-java-boot--ls-command (port)
  "Create LS command for PORT."
  (-filter 'identity
           (list lsp-java-java-path
                 (unless (lsp-java--java-9-plus-p)
                   (format "-Dloader.path=%s" (lsp-java-boot--find-tools-jar)))
                 (format "-Dspring.lsp.client-port=%s" port)
                 (format "-Dserver.port=%s" port)
                 ;; "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044,quiet=y"
                 "-Dsts.lsp.client=vscode"
                 (concat "-Dsts.log.file=" (make-temp-file "sts-log-file" nil ".log"))
                 (concat "-Dlogging.file=" (make-temp-file "logging-file" nil ".log"))
                 "-jar"
                 (lsp-java-boot--server-jar))))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-tcp-server #'lsp-java-boot--ls-command)
                  :activation-fn (lambda (filename major-mode)
                                   (and lsp-java-boot-enabled
                                        (memq major-mode '(java-mode conf-javaprop-mode yaml-mode))
                                        (lsp-java-boot--server-jar)))
                  :request-handlers  (ht ("sts/addClasspathListener" #'lsp-java-boot--sts-add-classpath-listener)
                                         ("sts/javadocHoverLink" #'lsp-java-boot--sts-javadoc-hover-link))
                  :notification-handlers  (ht ("sts/highlight" #'lsp-java-boot--sts-hightlight)
                                              ("sts/progress" #'ignore))
                  :initialized-fn (lambda (workspace)
                                    (puthash
                                     "triggerCharacters"
                                     '("." "@" "#" "*")
                                     (gethash "completionProvider" (lsp--workspace-server-capabilities workspace))))
                  :multi-root t
                  :add-on? t
                  :server-id 'boot-ls
                  :completion-in-comments? t))

(provide 'lsp-java-boot)
;;; lsp-java-boot.el ends here
