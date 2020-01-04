;;; company-tabnine.el --- A company-mode backend for TabNine
;;
;; Copyright (c) 2018 Tommy Xiang
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;; Keywords: convenience
;; Package-Version: 20200102.2025
;; Version: 0.0.1
;; URL: https://github.com/TommyX12/company-tabnine/
;; Package-Requires: ((emacs "25") (company "0.9.3") (cl-lib "0.5") (dash "2.16.0") (s "1.12.0") (unicode-escape "1.1"))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; Description:
;;
;; TabNine(https://tabnine.com/) is the all-language autocompleter.
;; It uses machine learning to provide responsive, reliable, and relevant suggestions.
;; `company-tabnine' provides TabNine completion backend for `company-mode'(https://github.com/company-mode/company-mode).
;; It takes care of TabNine binaries, so installation is easy.
;;
;; Installation:
;;
;; 1. Make sure `company-mode' is installed and configured.
;; 2. Add `company-tabnine' to `company-backends':
;;
;;   (add-to-list 'company-backends #'company-tabnine)
;;
;; 3. Run M-x company-tabnine-install-binary to install the TabNine binary for your system.
;;
;; Usage:
;;
;; `company-tabnine' should work out of the box.
;; See M-x customize-group RET company-tabnine RET for customizations.
;;
;; Recommended Configuration:
;;
;; - Trigger completion immediately.
;;
;;   (setq company-idle-delay 0)
;;
;; - Number the candidates (use M-1, M-2 etc to select completions).
;;
;;   (setq company-show-numbers t)
;;

;;; Code:

;;
;; Dependencies
;;

(require 'cl-lib)
(require 'company)
(require 'company-template)
(require 'dash)
(require 'json)
(require 's)
(require 'unicode-escape)
(require 'url)


;;
;; Constants
;;

(defconst company-tabnine--process-name "company-tabnine--process")
(defconst company-tabnine--buffer-name "*company-tabnine-log*")
(defconst company-tabnine--hooks-alist nil)
(defconst company-tabnine--protocol-version "1.0.14")

;; tmp file put in company-tabnine-binaries-folder directory
(defconst company-tabnine--version-tempfile "version")

;; current don't know how to use Prefetch and GetIdentifierRegex
(defconst company-tabnine--method-autocomplete "Autocomplete")
(defconst company-tabnine--method-prefetch "Prefetch")
(defconst company-tabnine--method-getidentifierregex "GetIdentifierRegex")

;;
;; Macros
;;

(defmacro company-tabnine-with-disabled (&rest body)
  "Run BODY with `company-tabnine' temporarily disabled.
Useful when binding keys to temporarily query other completion backends."
  `(let ((company-tabnine--disabled t))
     ,@body))

(defmacro company-tabnine--with-destructured-candidate
    (candidate &rest body)
  (declare (indent 1) (debug t))
  `(let-alist ,candidate
     (setq type (company-tabnine--kind-to-type .kind))
     (propertize
      .new_prefix
      'old_suffix .old_suffix
      'new_suffix .new_suffix
      'kind .kind
      'type type
      'detail .detail
      'annotation
      (concat (or .detail "") " " (or type "")))
     ,@body))

(defun company-tabnine--filename-completer-p (extra-info)
  "Check whether candidate's EXTRA-INFO indicates a filename completion."
  (-contains? '("[File]" "[Dir]" "[File&Dir]") extra-info))

(defun company-tabnine--identifier-completer-p (extra-info)
  "Check if candidate's EXTRA-INFO indicates a identifier completion."
  (s-equals? "[ID]" extra-info))

;;
;; Customization
;;

(defgroup company-tabnine nil
  "Options for company-tabnine."
  :link '(url-link :tag "Github" "https://github.com/TommyX12/company-tabnine")
  :group 'company
  :prefix "company-tabnine-")

(defcustom company-tabnine-max-num-results 10
  "Maximum number of results to show."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-context-radius 3000
  "The number of chars before and after point to send for completion.
For example, setting this to 2000 will send 4000 chars in total per query.
It is not recommended to change this.

Note that setting this too small will cause TabNine to not be able to read the entire license activation key."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-max-restart-count 10
  "Maximum number of times TabNine can consecutively restart.
This may be due to errors in or automatic server updates.
Any successful completion will reset the consecutive count."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-wait 0.25
  "Number of seconds to wait for TabNine to respond."
  :group 'company-tabnine
  :type 'float)

(defcustom company-tabnine-always-trigger t
  "Whether to overload company's minimum prefix length.
This allows completion to trigger on as much as possible.
Default is t (strongly recommended)."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-no-continue nil
  "Whether to make company reset idle timer on all keystrokes.
Only useful when `company-idle-delay' is not 0.
Doing so improves performance by reducing number of calls to the completer,
at the cost of less responsive completions."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-binaries-folder "~/.TabNine"
  "Path to TabNine binaries folder.
`company-tabnine-install-binary' will use this directory."
  :group 'company-tabnine
  :type 'string)

(defcustom company-tabnine-install-static-binary (file-exists-p "/etc/nixos/hardware-configuration.nix")
  "Whether to install the musl-linked static binary instead of
the standard glibc-linked dynamic binary.
Only useful on GNU/Linux.  Automatically set if NixOS is detected."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-log-file-path nil
  "If non-nil, next TabNine restart will write debug log to this path."
  :group 'company-tabnine
  :type 'string)

(defcustom company-tabnine-auto-balance t
  "Whether TabNine should insert balanced parentheses upon completion."
  :group 'company-tabnine
  :type 'boolean)

;; (defcustom company-tabnine-async t
;;   "Whether or not to use async operations to fetch data."
;;   :group 'company-tabnine
;;   :type 'boolean)

(defcustom company-tabnine-show-annotation t
  "Whether to show an annotation inline with the candidate."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-auto-fallback t
  "Whether to automatically fallback to other backends when TabNine has no candidates."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-use-native-json t
  "Whether to use native JSON when possible."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-insert-arguments t
  "When non-nil, insert function arguments as a template after completion.
Only supported by modes in `company-tabnine--extended-features-modes'"
  :group 'company-tabnine
  :type 'boolean)


;;
;; Faces
;;

;;
;; Variables
;;

(defvar company-tabnine-executable-args nil
  "Arguments passed to TabNine.")

(defvar company-tabnine--process nil
  "TabNine server process.")

(defvar company-tabnine--restart-count 0
  "Number of times TabNine server has restarted abnormally.
Resets every time successful completion is returned.")

(defvar company-tabnine--response nil
  "Temporarily stored TabNine server responses.")

(defvar company-tabnine--disabled nil
  "Variable to temporarily disable company-tabnine and pass control to next backend.")

(defvar company-tabnine--calling-continue nil
  "Flag for when `company-continue' is being called.")

(defvar company-tabnine--response-chunks nil
  "The string to store response chunks from TabNine server.")

;;
;; Major mode definition
;;

;;
;; Global methods
;;

(defun company-tabnine--prefix-candidate-p (candidate prefix)
  "Return t if CANDIDATE string begins with PREFIX."
  (let ((insertion-text (cdr (assq 'insertion_text candidate))))
    (s-starts-with? prefix insertion-text t)))

(defun company-tabnine--error-no-binaries ()
  "Signal error for when TabNine binary is not found."
  (error "No TabNine binaries found.  Run M-x company-tabnine-install-binary to download binaries"))

(defun company-tabnine--get-target ()
  "Return TabNine's system configuration.  Used for finding the correct binary."
  (let ((architecture
         (cond
          ((string= (s-left 6 system-configuration) "x86_64")
           "x86_64")
          (t
           "i686")))

        (os
         (cond
          ((or (eq system-type 'ms-dos)
               (eq system-type 'windows-nt)
               (eq system-type 'cygwin))
           "pc-windows-gnu")
          ((or (eq system-type 'darwin))
           "apple-darwin")
          (company-tabnine-install-static-binary
           "unknown-linux-musl")
          (t
           "unknown-linux-gnu"))))

    (concat architecture "-" os)))

(defun company-tabnine--get-exe ()
  "Return TabNine's binary file name.  Used for finding the correct binary."
  (cond
   ((or (eq system-type 'ms-dos)
        (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    "TabNine.exe")
   (t
    "TabNine")))

(defun company-tabnine--executable-path ()
  "Find and return the path of the latest TabNine binary for the current system."
  (let ((parent company-tabnine-binaries-folder))
    (if (file-directory-p parent)
        (let* ((children (->> (directory-files parent)
                              (--remove (member it '("." "..")))
                              (--filter (file-directory-p
                                         (expand-file-name
                                          it
                                          (file-name-as-directory
                                           parent))))
                              (--filter (ignore-errors (version-to-list it)))
                              (-non-nil)))
               (sorted (nreverse (sort children #'version<)))
               (target (company-tabnine--get-target))
               (filename (company-tabnine--get-exe)))
          (cl-loop
             for ver in sorted
             for fullpath = (expand-file-name (format "%s/%s/%s"
                                                      ver target filename)
                                              parent)
             if (and (file-exists-p fullpath)
                     (file-regular-p fullpath))
             return fullpath
             finally do (company-tabnine--error-no-binaries)))
      (company-tabnine--error-no-binaries))))

(defun company-tabnine-start-process ()
  "Start TabNine process."
  (company-tabnine-kill-process)
  (let ((process-connection-type nil))
    (setq company-tabnine--process
          (make-process
           :name company-tabnine--process-name
           :command (append
                     (cons (company-tabnine--executable-path)
                           (when company-tabnine-log-file-path
                             (list
                              "--log-file-path"
                              (expand-file-name
                               company-tabnine-log-file-path))))
                     company-tabnine-executable-args)
           :coding 'utf-8
           :connection-type 'pipe
           :filter #'company-tabnine--process-filter
           :sentinel #'company-tabnine--process-sentinel
           :noquery t)))
  ;; hook setup
  (message "TabNine server started.")
  (dolist (hook company-tabnine--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun company-tabnine-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when company-tabnine--process
    (let ((process company-tabnine--process))
      (setq company-tabnine--process nil) ; this happens first so sentinel don't catch the kill
      (delete-process process)))
  ;; hook remove
  (dolist (hook company-tabnine--hooks-alist)
    (remove-hook (car hook) (cdr hook))))

(defun company-tabnine-send-request (request)
  "Send REQUEST to TabNine server.  REQUEST needs to be JSON-serializable object."
  (when (null company-tabnine--process)
    (company-tabnine-start-process))
  (when company-tabnine--process
    ;; TODO make sure utf-8 encoding works
    (let ((encoded (concat
                    (if (and company-tabnine-use-native-json
                             (fboundp 'json-serialize))
                        (json-serialize request
                                        :null-object nil
                                        :false-object json-false)
                      (let ((json-null nil)
                            (json-encoding-pretty-print nil))
                        (json-encode-list request)))
                    "\n")))
      (setq company-tabnine--response nil)
      (process-send-string company-tabnine--process encoded)
      (accept-process-output company-tabnine--process company-tabnine-wait))))

(defun company-tabnine--make-request (method)
  "Create request body for method METHOD and parameters PARAMS."
  (cond
   ((eq method 'autocomplete)
    (let* ((buffer-min 1)
           (buffer-max (1+ (buffer-size)))
           (before-point
            (max (point-min) (- (point) company-tabnine-context-radius)))
           (after-point
            (min (point-max) (+ (point) company-tabnine-context-radius))))

      (list
       :version company-tabnine--protocol-version
       :request
       (list :Autocomplete
             (list
              :before (buffer-substring-no-properties before-point (point))
              :after (buffer-substring-no-properties (point) after-point)
              :filename (or (buffer-file-name) nil)
              :region_includes_beginning (if (= before-point buffer-min)
                                             t json-false)
              :region_includes_end (if (= after-point buffer-max)
                                       t json-false)
              :max_num_results company-tabnine-max-num-results)))))

   ((eq method 'prefetch)
    (list
     :version company-tabnine--protocol-version
     :request
     (list :Prefetch
           (list
            :filename (or (buffer-file-name) nil)
            ))))
   ((eq method 'getidentifierregex)
    (list
     :version company-tabnine--protocol-version
     :request
     (list :GetIdentifierRegex
           (list
            :filename (or (buffer-file-name) nil)
            ))))))

(defun company-tabnine-query ()
  "Query TabNine server for auto-complete."
  (let ((request (company-tabnine--make-request 'autocomplete)))
    (company-tabnine-send-request request)
    ))

(defun company-tabnine--decode (msg)
  "Decode TabNine server response MSG, and return the decoded object."
  (if (and company-tabnine-use-native-json
           (fboundp 'json-parse-string))
      (ignore-errors
        (json-parse-string msg :object-type 'alist))
    (let ((json-array-type 'list)
          (json-object-type 'alist))
      (json-read-from-string msg))))

(defun company-tabnine--process-sentinel (process event)
  "Sentinel for TabNine server process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and company-tabnine--process
             (memq (process-status process) '(exit signal)))

    (message "TabNine process %s received event %s."
             (prin1-to-string process)
             (prin1-to-string event))

    (if (>= company-tabnine--restart-count
            company-tabnine-max-restart-count)
        (progn
          (message "TabNine process restart limit reached.")
          (setq company-tabnine--process nil))

      (message "Restarting TabNine process.")
      (company-tabnine-start-process)
      (setq company-tabnine--restart-count
            (1+ company-tabnine--restart-count)))))

(defun company-tabnine--process-filter (process output)
  "Filter for TabNine server process.
PROCESS is the process under watch, OUTPUT is the output received."
  (push output company-tabnine--response-chunks)
  (when (s-ends-with-p "\n" output)
    (let ((response
           (mapconcat #'identity
                      (nreverse company-tabnine--response-chunks)
                      nil)))
      (setq company-tabnine--response
            (company-tabnine--decode response)
            company-tabnine--response-chunks nil))))

(defun company-tabnine--prefix ()
  "Prefix-command handler for the company backend."
  (if (or (and company-tabnine-no-continue
               company-tabnine--calling-continue)
          company-tabnine--disabled)
      nil
    (company-tabnine-query)
    (let ((prefix
           (and company-tabnine--response
                (> (length (alist-get 'results company-tabnine--response)) 0)
                (alist-get 'old_prefix company-tabnine--response))))
      (unless (or prefix
                  company-tabnine-auto-fallback)
        (setq prefix 'stop))
      (if (and prefix
               company-tabnine-always-trigger)
          (cons prefix t)
        prefix))))

(defun company-tabnine--annotation(candidate)
  "Fetch the annotation text-property from a CANDIDATE string."
  (when company-tabnine-show-annotation
    (-if-let (annotation (get-text-property 0 'annotation candidate))
        annotation
      (let ((kind (get-text-property 0 'kind candidate))
            ;; (return-type (get-text-property 0 'return_type candidate))
            (params (get-text-property 0 'params candidate)))
        (when kind
          (concat params
                  ;; (when (s-present? return-type)
                  ;;   (s-prepend " -> " return-type))
                  (when (s-present? kind)
                    (format " [%s]" kind))))))))

(defun company-tabnine--kind-to-type (kind)
  (pcase kind
    (1 "Text")
    (2 "Method")
    (3 "Function")
    (4 "Constructor")
    (5 "Field")
    (6 "Variable")
    (7 "Class")
    (8 "Interface")
    (9 "Module")
    (10 "Property" )
    (11 "Unit" )
    (12 "Value" )
    (13 "Enum")
    (14 "Keyword" )
    (15 "Snippet")
    (16 "Color")
    (17 "File")
    (18 "Reference")
    (19 "Folder")
    (20 "EnumMember")
    (21 "Constant")
    (22 "Struct")
    (23 "Event")
    (24 "Operator")
    (25 "TypeParameter")))

(defun company-tabnine--construct-candidate-generic (candidate)
  "Generic function to construct completion string from a CANDIDATE."
  (company-tabnine--with-destructured-candidate candidate))

(defun company-tabnine--construct-candidates (results construct-candidate-fn)
  "Use CONSTRUCT-CANDIDATE-FN to construct a list of candidates from RESULTS."
  (let ((completions (mapcar construct-candidate-fn results)))
    (when completions
      (setq company-tabnine--restart-count 0))
    completions))

(defun company-tabnine--get-candidates (response)
  "Get candidates for RESPONSE."
  (company-tabnine--construct-candidates
   (alist-get 'results response)
   #'company-tabnine--construct-candidate-generic))

(defun company-tabnine--candidates (prefix)
  "Candidates-command handler for the company backend for PREFIX.

Return completion candidates.  Must be called after `company-tabnine-query'."
  (company-tabnine--get-candidates company-tabnine--response))

(defun company-tabnine--meta (candidate)
  "Return meta information for CANDIDATE.  Currently used to display user messages."
  (if (null company-tabnine--response)
      nil
    (let ((meta (get-text-property 0 'meta candidate)))
      (if (stringp meta)
          (let ((meta-trimmed (s-trim meta)))
            meta-trimmed)

        (let ((messages (alist-get 'user_message company-tabnine--response)))
          (when messages
            (s-join " " messages)))))))

(defun company-tabnine--post-completion (candidate)
  "Replace old suffix with new suffix for CANDIDATE."
  (when company-tabnine-auto-balance
    (let ((old_suffix (get-text-property 0 'old_suffix candidate))
          (new_suffix (get-text-property 0 'new_suffix candidate)))
      (delete-region (point)
                     (min (+ (point) (length old_suffix))
                          (point-max)))
      (when (stringp new_suffix)
        (save-excursion
          (insert new_suffix))))))

;;
;; Interactive functions
;;

(defun company-tabnine-restart-server ()
  "Start/Restart TabNine server."
  (interactive)
  (company-tabnine-start-process))

(defun company-tabnine-install-binary ()
  "Install TabNine binary into `company-tabnine-binaries-folder'."
  (interactive)
  (let ((version-tempfile (concat
                           (file-name-as-directory
                            company-tabnine-binaries-folder)
                           company-tabnine--version-tempfile))
        (target (company-tabnine--get-target))
        (exe (company-tabnine--get-exe))
        (binaries-dir company-tabnine-binaries-folder))
    (message version-tempfile)
    (message "Getting current version...")
    (make-directory (file-name-directory version-tempfile) t)
    (url-copy-file "https://update.tabnine.com/version" version-tempfile t)
    (let ((version (s-trim (with-temp-buffer (insert-file-contents version-tempfile) (buffer-string)))))
      (when (= (length version) 0)
        (error "TabNine installation failed.  Please try again"))
      (message "Current version is %s" version)
      (let ((url (concat "https://update.tabnine.com/" version "/" target "/" exe)))
        (let ((target-path
               (concat
                (file-name-as-directory
                 (concat
                  (file-name-as-directory
                   (concat (file-name-as-directory binaries-dir) version))
                  target))
                exe)))
          (message "Installing at %s. Downloading %s ..." target-path url)
          (make-directory (file-name-directory target-path) t)
          (url-copy-file url target-path t)
          (set-file-modes target-path (string-to-number "744" 8))
          (delete-file version-tempfile)
          (message "TabNine installation complete."))))))

(defun company-tabnine-call-other-backends ()
  "Invoke company completion but disable TabNine once, passing query to other backends in `company-backends'.

This is actually obsolete, since `company-other-backend' does the same."
  (interactive)
  (company-tabnine-with-disabled
   (company-abort)
   (company-auto-begin)))

;;;###autoload
(defun company-tabnine (command &optional arg &rest ignored)
  "`company-mode' backend for TabNine.

See documentation of `company-backends' for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tabnine))
    (prefix (company-tabnine--prefix))
    (candidates (company-tabnine--candidates arg))
    ;; TODO: should we use async or not?
    ;; '(:async . (lambda (callback)
    ;;              (funcall callback (company-tabnine--candidates) arg))))
    (meta (company-tabnine--meta arg))
    (annotation (company-tabnine--annotation arg))
    (post-completion (company-tabnine--post-completion arg))
    (no-cache t)
    (sorted t)))

;;
;; Advices
;;

(defun company-tabnine--continue-advice (func &rest args)
  "Advice for `company--continue'."
  (let ((company-tabnine--calling-continue t))
    (apply func args)))

(advice-add #'company--continue :around #'company-tabnine--continue-advice)

(defun company-tabnine--insert-candidate-advice (func &rest args)
  "Advice for `company--insert-candidate'."
  (if company-tabnine-auto-balance
      (let ((smartparens-mode nil))
        (apply func args))
    (apply func args)))

;; `smartparens' will add an advice on `company--insert-candidate' in order to
;; add closing parenthesis.
;; If TabNine takes care of parentheses, we disable smartparens temporarily.
(eval-after-load 'smartparens
  '(advice-add #'company--insert-candidate
               :around #'company-tabnine--insert-candidate-advice))

;;
;; Hooks
;;


(provide 'company-tabnine)

;;; company-tabnine.el ends here
