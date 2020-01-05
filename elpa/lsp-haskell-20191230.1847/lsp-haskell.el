;;; lsp-haskell.el --- Haskell support for lsp-mode

;; Version: 1.0
;; Package-Version: 20191230.1847
;; Package-Requires: ((lsp-mode "3.0") (haskell-mode "1.0"))
;; Keywords: haskell
;; URL: https://github.com/emacs-lsp/lsp-haskell

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

(require 'haskell)
(require 'lsp-mode)
(require 'projectile nil 'noerror)

;; ---------------------------------------------------------------------
;; Configuration

;;;###autoload
(defgroup lsp-haskell nil
  "Customization group for ‘lsp-haskell’."
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-haskell-process-path-hie
  ;; "hie"
  "hie-wrapper"
  "The path for starting the haskell-ide-engine
server. hie-wrapper exists on HIE master from 2018-06-10"
  :group 'lsp-haskell
  :type '(choice string))

;;;###autoload
(defcustom lsp-haskell-process-args-hie
  '("-d" "-l" "/tmp/hie.log")
  "The arguments for starting the haskell-ide-engine server.
For a debug log, use `-d -l /tmp/hie.log'."
  :group 'lsp-haskell
  :type '(repeat (string :tag "Argument")))

;;;###autoload
(defcustom lsp-haskell-process-wrapper-function
  #'identity
  "Use this to wrap the haskell-ide-engine process started by lsp-haskell.

For example, use the following the start the hie process in a nix-shell:

(lambda (argv)
  (append
   (append (list \"nix-shell\" \"-I\" \".\" \"--command\" )
           (list (mapconcat 'identity argv \" \"))
           )
   (list (concat (lsp-haskell--get-root) \"/shell.nix\"))
   )
  )"
  :group 'lsp-haskell
  :type '(choice
          (function-item :tag "None" :value identity)
          (function :tag "Custom function")))

;; ---------------------------------------------------------------------
;; HaRe functions

(defun lsp-demote ()
  "Demote a function to the level it is used"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:demote"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

(defun lsp-duplicate-definition (newname)
  "Duplicate a definition"
  (interactive "sNew definition name: ")
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:dupdef"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))
             :text ,newname))))

(defun lsp-if-to-case ()
  "Convert an if statement to a case statement"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:iftocase"
   (vector `(:file      ,(concat "file://" buffer-file-name)
             :start_pos ,(lsp-get-start-position)
             :end_pos   ,(lsp-get-end-position)))))

(defun lsp-lift-level ()
  "Lift a function to the top level"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:liftonelevel"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

(defun lsp-lift-to-top ()
  "Lift a function to the top level"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:lifttotoplevel"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

(defun lsp-delete-definition ()
  "Delete a definition"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:deletedef"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

(defun lsp-generalise-applicative ()
  "Generalise a monadic function to use applicative"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:genapplicative"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

;; ---------------------------------------------------------------------

(defun lsp-haskell--session-cabal-dir ()
  "Get the session cabal-dir."
  (let* ((cabal-file (haskell-cabal-find-file))
         (cabal-dir (if cabal-file
                        (file-name-directory cabal-file)
                      "." ;; no cabal file, use directory only
                      )))
    (progn
      (message "cabal-dir: %s" cabal-dir)
      cabal-dir)))

(defun lsp-haskell--get-root ()
  "Get project root directory.

First searches for root via projectile.  Tries to find cabal file
if projectile way fails"
  ;; (if (and (fboundp 'projectile-project-root) (projectile-project-root))
  (if nil
      (projectile-project-root)
    (let ((dir (lsp-haskell--session-cabal-dir)))
      (if (string= dir "/")
          (user-error (concat "Couldn't find cabal file, using:" dir))
        dir))))

;; ---------------------------------------------------------------------

(defun lsp--haskell-hie-command ()
  "Comamnd and arguments for launching the inferior hie process.
These are assembled from the customizable variables
 `lsp-haskell-process-path-hie' and
 `lsp-haskell-process-args-hie'. If the hie executable is
 installed via its Makefile, there will be compiler-specific
 versions with names like 'hie-8.0.2' or 'hie-8.2.2'."
   (append (list lsp-haskell-process-path-hie "--lsp") lsp-haskell-process-args-hie) )

;; ---------------------------------------------------------------------
;; Supporting the new lsp.el operation, as per
;; https://github.com/emacs-lsp/lsp-mode/blob/master/README-NEXT.md

(eval-after-load 'lsp '(lsp-register-client
    (make-lsp--client
     :new-connection (lsp-stdio-connection (lambda () (lsp-haskell--hie-command)))
     :major-modes '(haskell-mode)
     :server-id 'hie
     ;; :multi-root t
     ;; :initialization-options 'lsp-haskell--make-init-options
     )))

(defun lsp-haskell--hie-command ()
  (funcall lsp-haskell-process-wrapper-function (lsp--haskell-hie-command)))

(cl-defmethod lsp-initialization-options ((_server (eql hie)))
  "Initialization options for haskell."
  `(:languageServerHaskell ,lsp-haskell--config-options))

;; ---------------------------------------------------------------------

(defvar lsp-haskell--config-options (make-hash-table))

;; ---------------------------------------------------------------------

(defun lsp-haskell--set-configuration ()
  (lsp--set-configuration `(:languageServerHaskell ,lsp-haskell--config-options)))

(add-hook 'lsp-after-initialize-hook 'lsp-haskell--set-configuration)

(defun lsp-haskell-set-config (name option)
  "Set config option NAME to value OPTION in the haskell lsp server."
  (puthash name option lsp-haskell--config-options))

  ;; parseJSON = withObject "Config" $ \v -> do
  ;;   s <- v .: "languageServerHaskell"
  ;;   flip (withObject "Config.settings") s $ \o -> Config
  ;;     <$> o .:? "hlintOn"              .!= True
  ;;     <*> o .:? "maxNumberOfProblems"  .!= 100
  ;;     <*> o .:? "liquidOn"             .!= False
  ;;     <*> o .:? "completionSnippetsOn" .!= True

;; -------------------------------------

(defun lsp-haskell-set-hlint (val)
  "Enable(t)/Disable(nil) running hlint."
  (lsp-haskell-set-config "hlintOn" val))

(defun lsp-haskell-set-hlint-on ()
  "Enable running hlint haskell."
  (interactive)
  (lsp-haskell-set-hlint t)
  (lsp-haskell--set-configuration))

(defun lsp-haskell-set-hlint-off ()
  "Disable running hlint."
  (interactive)
  (lsp-haskell-set-hlint :json-false)
  (lsp-haskell--set-configuration))

;; -------------------------------------

(defun lsp-haskell-set-max-problems (val)
  "Set maximum number of problems reported to VAL."
  (lsp-haskell-set-config "maxNumberOfProblems" val))

(defun lsp-haskell-set-max-number-of-problems (val)
  "Set maximum number of problems reported to VAL."
  (interactive "nMax number of problems to report: ")
  (lsp-haskell-set-max-problems val)
  (lsp-haskell--set-configuration))

;; -------------------------------------

(defun lsp-haskell-set-liquid (val)
  "Enable(t)/Disable(nil) running liquid haskell on save."
  (lsp-haskell-set-config "liquidOn" val))

(defun lsp-haskell-set-liquid-on ()
  "Enable running liquid haskell on save."
  (interactive)
  (lsp-haskell-set-liquid t)
  (lsp-haskell--set-configuration))

(defun lsp-haskell-set-liquid-off ()
  "Disable running liquid haskell on save."
  (interactive)
  (lsp-haskell-set-liquid :json-false)
  (lsp-haskell--set-configuration))

;; -------------------------------------

(defun lsp-haskell-set-completion-snippets (val)
  "Enable(t)/Disable(nil) providing completion snippets."
  (lsp-haskell-set-config "completionSnippetsOn" val))

(defun lsp-haskell-set-completion-snippets-on ()
  "Enable providing completion snippets."
  (interactive)
  (lsp-haskell-set-completion-snippets t)
  (lsp-haskell--set-configuration))

(defun lsp-haskell-set-completion-snippets-off ()
  "Disable providing completion snippets."
  (interactive)
  (lsp-haskell-set-completion-snippets :json-false)
  (lsp-haskell--set-configuration))


;; ---------------------------------------------------------------------

(provide 'lsp-haskell)
;;; lsp-haskell.el ends here
