;;; company-lsp.el --- Company completion backend for lsp-mode.  -*- lexical-binding: t -*-

;; Version: 2.1.0
;; Package-Version: 20190612.1553
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0") (company "0.9.0") (s "1.2.0") (dash "2.11.0"))
;; URL: https://github.com/tigersoldier/company-lsp

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

;; `company-lsp' is a `company' completion backend for `lsp-mode'.
;; To use it, add `company-lsp' to `company-backends':

;;     (require 'company-lsp)
;;     (push 'company-lsp company-backends)

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'lsp-mode)
(require 's)
(require 'seq)
(require 'dash)

(defgroup company-lsp nil
  "Company completion backend for lsp-mode."
  :prefix "company-lsp-"
  :group 'tools)

(defcustom company-lsp-cache-candidates nil
  "Whether or not to cache completion candidates.

When set to 'auto, company-lsp caches the completion. It sends
incremental completion requests to the server if and only if the
cached results are incomplete. The candidate list may not be
sorted or filtered as the server would for cached completion
results.

When set to t, company-mode caches the completion. It won't send
incremental completion requests to the server. Candidates are
filtered on client side.

When set to nil, results are not cached at all. Each incremental
completion will send requests to the server. Use this option if
the server handles caching for incremental completion or
sorting/matching provided by the server is critical. If
`company-lsp-filter-candidates' is non-nil for the language
server, returned candidates are filtered by company-lsp.
Otherwise candidates are not filtered."
  :type '(choice (const :tag "Respect server response" auto)
                 (const :tag "Always cache" t)
                 (const :tag "Never cache" nil))
  :group 'company-lsp)

(defcustom company-lsp-filter-candidates
  ;; The server ID of bingo has been renamed to go-bingo. Keeping both for
  ;; backward compatibility.
  '((bingo . nil)
    (ccls . nil)
    (clangd . nil)
    (cquery . nil)
    (go-bingo . nil)
    (gopls . nil)
    (javacomp . nil)
    (jdtls . nil)
    (pyls . nil)
    (rls . nil)
    (t . t))
  "Whether or not to filter completion candidates returned by server.

Some servers return unfiltered candidates while others do
server-side filtering. This option controls whether or not to
filter candidates on client-side when
`company-lsp-cache-candidates' is nil for the current server. This
option doesn't change the filtering behavior when
`company-lsp-cache-candidates' is set to auto or t.

Value can be t, nil, or an alist. When set
to t, always filter candidates regardless of the current language
server. When set to candidates are never filtered.

When set to an alist, the key is either a symbol of the server-id
defined by the LSP client for the server, or t that matches other
servers. The value is a boolean."
  :type '(choice (const :tag "Always filter" t)
                 (const :tag "Never filter" nil)
                 (alist :tag "Depends on language server"
                        :key-type (choice (const :tag "Other servers" t)
                                          (symbol :tag "Server ID"))
                        :value-type boolean))
  :group 'company-lsp)

(defcustom company-lsp-async t
  "Whether or not to use async operations to fetch data."
  :type 'boolean
  :group 'company-lsp)

(defcustom company-lsp-enable-snippet t
  "Whether or not to support expanding completion snippet.

If set to non-nil, company-lsp will register client capabilities
for snippet support. When the server returns completion item with
snippet, company-lsp will replace the label of the completion
item with the snippet and use yas-snippet to expand it."
  :type 'boolean
  :group 'company-lsp)

(defcustom company-lsp-enable-trigger-kind t
  "Whether or not to populate triggerKind field in the completion request."
  :type 'boolean
  :group 'company-lsp)

(defcustom company-lsp-enable-recompletion t
  "Whether or not to re-trigger completion for trigger characters.

If set to non-nil, when company-lsp finishes completion, it checks if
the current point is before any completion trigger characters. If yes,
it re-triggers another completion request.

This is useful in cases such as 'std' is completed as 'std::' in C++."
  :type 'boolean
  :group 'company-lsp)

(defcustom company-lsp-enable-additional-text-edit t
  "Whether or not to apply additional text edit.

If set to non-nil, company-lsp will apply additional text edits
from the server. Otherwise, the additional text edits are
ignored."
  :type 'boolean
  :group 'company-lsp)

(defcustom company-lsp-match-candidate-predicate #'company-lsp-match-candidate-flex
  "Predicate function that determines whether a candidate matches given input.

The function takes two parameters: CANDIDATE and PREFIX.
CANDIDATE is a string created by `company-lsp--make-candidate'.
PREFIX is the symbol before point that should be used for
filtering. If the function returns non-nil, CANDIDATE will
be presented in the completion list.

Company-lsp provides two builtin predicates:
`company-lsp-match-candidate-prefix' and
`company-lsp-match-candidate-flex'."
  :type 'function
  :group 'company-lsp)

(defconst company-lsp--trigger-kind-invoked 1
  "The completion is triggered by typing identifier or invoking `company-lsp'.

Defined in LSP spec as CompletionTriggerKind.Invoked.")

(defconst company-lsp--trigger-kind-trigger-character 2
  "The completion is triggered by typing a trigger character.

Defined in LSP spec as CompletionTriggerKind.TriggerCharacter.")

(defconst company-lsp--trigger-kind-incomplete 3
  "The completion is triggered by narrowing incomplete completion list.

Defined in LSP spec as
CompletionTriggerKind.TriggerForIncompleteCompletions.")

(declare-function yas-expand-snippet "ext:yasnippet.el")

(defun company-lsp--get-config (config server-id)
  "Get the CONFIG value for SERVER-ID.

If CONFIG is a list in the form of (server-id . value), return
the value of key SERVER-ID. When there is no value of key
SERVER-ID, return the value of key t if it's present, or return
nil otherwise.

If CONFIG is not a list, return it directly."
  (if (listp config)
      (if-let (server-config (assq server-id config))
          (cdr server-config)
        (alist-get t config))
    config))

(defvar company-lsp--snippet-functions '(("rust" . company-lsp--rust-completion-snippet))
  "Alist of functions to insert our snippets for each language.")

(defvar-local company-lsp--completion-cache nil
  "Cached completion. It's an alist of (prefix . completion).

PREFIX is the prefix string.
COMPLETION is a cache-item created by `company-lsp--cache-item-new'.")

(defvar-local company-lsp--last-request-id nil
  "The last request ID for completion sent to the language
  server. nil means no outstanding requests.")

(defun company-lsp--trigger-characters ()
  "Return a list of completion trigger characters specified by server."
  (let ((provider (lsp--capability "completionProvider")))
    (and provider (seq-into (gethash "triggerCharacters" provider) 'list))))

(defun company-lsp--completion-prefix ()
  "Return the completion prefix.

Return value is compatible with the `prefix' command of a company backend.

Return nil if no completion should be triggered. Return a string
as the prefix to be completed, or a cons cell of (prefix . t) to bypass
`company-minimum-prefix-length' for trigger characters."
  (if-let ((trigger-chars (company-lsp--trigger-characters)))
      (let* ((max-trigger-len (apply 'max (mapcar (lambda (trigger-char)
                                                    (length trigger-char))
                                                  trigger-chars)))
             (trigger-regex (s-join "\\|" (mapcar #'regexp-quote trigger-chars)))
             (symbol-cons (company-grab-symbol-cons trigger-regex max-trigger-len)))
        ;; Some major modes define trigger characters as part of the symbol. For
        ;; example "@" is considered a vaild part of symbol in java-mode.
        ;; Company will grab the trigger character as part of the prefix while
        ;; the server doesn't. Remove the leading trigger character to solve
        ;; this issue.
        (let* ((symbol (if (consp symbol-cons)
                           (car symbol-cons)
                         symbol-cons))
               (trigger-char (seq-find (lambda (trigger-char)
                                         (s-starts-with? trigger-char symbol))
                                       trigger-chars)))
          (if trigger-char
              (cons (substring symbol (length trigger-char)) t)
            symbol-cons)))
    (company-grab-symbol)))

(defun company-lsp--make-candidate (item prefix)
  "Convert a CompletionItem JSON data to a string.

ITEM is a hashtable representing the CompletionItem interface.
PREFIX is the currently active prefix.

The returned string has a lsp-completion-item property with the
value of ITEM."
  ;; The property has to be the same as added by `lsp--make-completion-item' so
  ;; that `lsp--annotate' can use it.
  (propertize (gethash "label" item) 'lsp-completion-item item 'lsp-completion-prefix prefix))

(defun company-lsp--candidate-item (candidate)
  "Retrieve the CompletionItem hashtable associated with CANDIDATE.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (plist-get (text-properties-at 0 candidate) 'lsp-completion-item))

(defun company-lsp--candidate-prefix (candidate)
  "Retrieves the prefix that was active during creation of the candidate.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (plist-get (text-properties-at 0 candidate) 'lsp-completion-prefix))

(defun company-lsp--resolve-candidate (candidate &rest props)
  "Resolve a completion candidate to fill some properties.

CANDIDATE is a string returned by `company-lsp--make-candidate'.
PROPS are strings of property names of CompletionItem hashtable
to be resolved.

The completionItem/resolve request will only be sent to the
server if the candidate has not been resolved before, and at lest
one of the PROPS of the CompletionItem is missing.

Returns CANDIDATE with the resolved CompletionItem."
  (unless (plist-get (text-properties-at 0 candidate) 'company-lsp-resolved)
    (let ((item (company-lsp--candidate-item candidate)))
      (when (seq-some (lambda (prop)
                        (null (gethash prop item)))
                      props)
        (let ((resolved-item (lsp--resolve-completion item))
              (len (length candidate)))
          (put-text-property 0 len
                             'lsp-completion-item resolved-item
                             candidate)
          (put-text-property 0 len
                             'company-lsp-resolved t
                             candidate)))))
  candidate)

(defun company-lsp--rust-completion-snippet (item)
  "Function providing snippet with the rust language.
It parses the function's signature in ITEM (a CompletionItem)
to expand its arguments."
  (-when-let* ((kind (gethash "kind" item))
               (is-function (= kind 3)))
    (let* ((detail (gethash "detail" item))
           (snippet (when (and detail (s-matches? "^\\(pub \\)?\\(unsafe \\)?fn " detail))
                      (-some--> (substring detail (1+ (s-index-of "(" detail)) (s-index-of ")" detail))
                                (replace-regexp-in-string "^[^,]*self\\(, \\)?" "" it)
                                (and (not (s-blank-str? it)) it)
                                (s-split ", " it)
                                (mapconcat (lambda (x) (format "${%s}" x)) it ", ")))))
      (concat "(" (or snippet "$1") ")$0"))))

(defun company-lsp--fallback-snippet (item)
  "Return the fallback snippet to expand for ITEM.

It looks for function corresponding to the language in
`company-lsp--snippet-functions'.

ITEM is a hashtable of the CompletionItem message.

Return a string of the snippet to expand, or nil if no snippet is available."
  (-when-let* ((fn-cons (assoc (lsp-buffer-language) company-lsp--snippet-functions))
               (fn (cdr fn-cons)))
    (funcall fn item)))

(defun company-lsp--looking-back-trigger-characters-p ()
  "Return non-nil if text before point matches any of the trigger characters."
  (let ((trigger-chars (company-lsp--trigger-characters)))
    (cl-some (lambda (trigger-char)
               (equal (buffer-substring-no-properties (- (point) (length trigger-char)) (point))
                      trigger-char))
             trigger-chars)))

(defun company-lsp--post-completion (candidate)
  "Replace a CompletionItem's label with its insertText. Apply text edits.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (let* ((resolved-candidate (company-lsp--resolve-candidate candidate
                                                             "insertText"
                                                             "textEdit"
                                                             "additionalTextEdits"))
         (item (company-lsp--candidate-item resolved-candidate))
         (prefix (company-lsp--candidate-prefix candidate))
         (label (gethash "label" item))
         (start (- (point) (length label)))
         (insert-text (gethash "insertText" item))
         ;; 1 = plaintext, 2 = snippet
         (insert-text-format (gethash "insertTextFormat" item))
         (text-edit (gethash "textEdit" item))
         (additional-text-edits (gethash "additionalTextEdits" item)))
    (cond
     (text-edit
      (setq insert-text (gethash "newText" text-edit))
      (delete-region (- (point) (length candidate)) (point))
      (insert prefix)
      (let* ((range (gethash "range" text-edit))
             (new-text-length (length insert-text)))
        ;; No that the text edit start may not be equal to prefix/label start.
        ;; For example jdtls can insert "java.util.List" for "java.uti". The
        ;; prefix start is before "uti", while the text edit start is before
        ;; "java".
        ;;
        ;; We need to adjust `start' to be the text edit start, because the
        ;; snippet expansion below will replace text between `start' and point
        ;; with insert-text again.
        (setq start (lsp--position-to-point (gethash "start" range)))
        (lsp--apply-text-edit text-edit)
        (goto-char (+ start new-text-length))))
     ((and insert-text (not (eq insert-text-format 2)))
      (cl-assert (string-equal (buffer-substring-no-properties start (point)) label))
      (goto-char start)
      (delete-char (length label))
      (insert insert-text)))

    (let ((start-marker (set-marker (make-marker) start)))
      (when (and additional-text-edits company-lsp-enable-additional-text-edit)
        (lsp--apply-text-edits additional-text-edits))
      (when (and company-lsp-enable-snippet
                 (fboundp 'yas-expand-snippet))
        (if (and insert-text (eq insert-text-format 2))
            (yas-expand-snippet (company-lsp--to-yasnippet-snippet insert-text)
                                (marker-position start-marker) (point))
          (-when-let (fallback-snippet (company-lsp--fallback-snippet item))
            (yas-expand-snippet fallback-snippet))))
      (set-marker start-marker nil))
    ;; Here we set this-command to a `self-insert-command'
    ;; so that company may retrigger idle completion after the snippet expansion
    ;; (~`company-post-command').
    ;; This is a bit of a hack and maybe that will change in the future.
    ;; This is useful for example when the completed candidate is a namespace
    ;; and the annotation text (inserted snippet) is the scope operator.
    ;;
    ;; std| -> std::   (=> idle completion desired here)
    ;;         stderr
    ;;         ...
    ;;
    ;; See https://github.com/company-mode/company-mode/issues/143
    (when (and company-lsp-enable-recompletion
               (company-lsp--looking-back-trigger-characters-p))
      (setq this-command 'self-insert-command))))

(defun company-lsp--to-yasnippet-snippet (text)
  "Convert VS code snippet TEXT to yasnippet snippet."
  ;; VS code snippet doesn't esscape "{", but yasnippet requires escaping it.
  (let (parts
        (start 0))
    (dolist (range (s-matched-positions-all (regexp-quote "{") text))
      (let ((match-start (car range)))
        (unless (and (> match-start 0) (= (aref text (1- match-start)) ?$))
          ;; Not a start of field. Escape it.
          (when (< start match-start)
            (push (substring text start match-start) parts))
          (push "\\{" parts)
          (setq start (1+ match-start)))))
    (when (< start (length text))
      (push (substring text start) parts))
    (apply #'concat (reverse parts))))

(defun company-lsp--on-completion (response prefix)
  "Handle completion RESPONSE.

PREFIX is a string of the prefix when the completion is requested.

Return a list of strings as the completion candidates."
  (let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
         (items (cond ((hash-table-p response) (gethash "items" response))
                      ((sequencep response) response)))
         (candidates (mapcar (lambda (item)
                               (company-lsp--make-candidate item prefix))
                             (lsp--sort-completions items)))
         (server-id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace)))
         (should-filter (or (eq company-lsp-cache-candidates t)
                            (and (null company-lsp-cache-candidates)
                                 (company-lsp--get-config company-lsp-filter-candidates server-id)))))
    (when (null company-lsp--completion-cache)
      (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache nil t)
      (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache nil t))
    (when (eq company-lsp-cache-candidates 'auto)
      ;; Only cache candidates on auto mode. If it's t company caches the
      ;; candidates for us.
      (company-lsp--cache-put prefix (company-lsp--cache-item-new candidates incomplete)))
    (if should-filter
        (company-lsp--filter-candidates candidates prefix)
      candidates)))

(defun company-lsp--filter-candidates (candidates prefix)
  "Filter CANDIDATES by PREFIX.

CANDIDATES are a list of strings of candidate labels created by
`company-lsp--make-candidate'.

Returns a new list of candidates."
  ;; TODO: Allow customizing matching functions to support fuzzy matching.
  ;; Consider supporting company-flx out of box.
  (let (resort)
    (--> candidates
         ;; candidate -> (score matched candidate)
         (mapcar (lambda (candidate)
                   (let ((match (funcall company-lsp-match-candidate-predicate candidate prefix)))
                     (if (consp match)
                         (progn
                           (setq resort t)
                           (list (car match) (cdr match) candidate))
                       (list -1 match candidate))))
                 it)
         (-filter (lambda (item)
                    (nth 1 item))
                  it)
         (if resort
             (sort it (lambda (a b) (< (car a) (car b))))
           it)
         (mapcar (lambda (item) (nth 2 item))
                 it))))

(defun company-lsp-match-candidate-prefix (candidate prefix)
  "Return non-nil if the filter text of CANDIDATE starts with PREFIX.

The match is case-insensitive."
  (s-starts-with-p prefix (company-lsp--candidate-filter-text candidate) t))

(defun company-lsp-match-candidate-flex (candidate prefix)
  "Return non-nil if the filter text of CANDIDATE matches PREFIX.

See `company-lsp--compute-flex-match' for more details."
  (company-lsp--compute-flex-match (company-lsp--candidate-filter-text candidate)
                                   prefix
                                   t))

(defun company-lsp--candidate-filter-text (candidate)
  "Return filter string of CANDIDATE.

CANDIDATE is a string created by `company-lsp--make-candidate'.
If the CompletionItem of CANDIDATE has filterText field, return
the value of filterText. Otherwise return CANDIDATE itself."
  (let* ((candidate-item (company-lsp--candidate-item candidate))
         (filter-text (gethash "filterText" candidate-item)))
    (or filter-text candidate)))

(defun company-lsp--cleanup-cache (_)
  "Clean up completion cache and company hooks."
  (setq company-lsp--completion-cache nil)
  (remove-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache t)
  (remove-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache t))

(defun company-lsp--cancel-outstanding-request ()
  "Cancels outstanding completion requests.

A cancel command with `company-lsp--last-request-id' will be sent
to the server. `company-lsp--last-request-id' is reset to nil
after cancellation."
  (when company-lsp--last-request-id
    (lsp--cancel-request company-lsp--last-request-id)
    (setq company-lsp--last-request-id nil)))

(defun company-lsp--cache-put (prefix candidates)
  "Set cache for PREFIX to be CANDIDATES.

CANDIDATES is a cache item created by `company-lsp--cache-item-new'."
  (setq company-lsp--completion-cache
        (cons (cons prefix candidates)
              company-lsp--completion-cache)))

(defun company-lsp--cache-get (prefix)
  "Get the cached completion for PREFIX.

Return a cache item if cache for PREFIX exists. Otherwise return nil."
  (-when-let* ((cached (company-lsp--cache-find-closest prefix))
               (subprefix (car cached))
               (cache-item (cdr cached)))
    (cond
     ((string= prefix subprefix)
      ;; Found exact match.
      cache-item)
     ((company-lsp--cache-item-incomplete-p cache-item)
      ;; Closest subprefix has incomplete result. Return nil to ask for narrowed
      ;; down results.
      nil)
     (t
      ;; Narrow down complete results for subprefix.
      (let* ((candidates (company-lsp--cache-item-candidates cache-item))
             (new-candidates (company-lsp--filter-candidates candidates prefix))
             (new-cache (company-lsp--cache-item-new new-candidates nil)))
        (company-lsp--cache-put prefix new-cache)
        new-cache)))))

(defun company-lsp--cache-find-closest (prefix)
  "Find cached completion with the longest sub-prefix of PREFIX.

Return a cons cell of (subprefix . cache-item) or nil."
  (let ((len (length prefix)))
    (cl-dotimes (i (1+ len))
      (when-let (item (assoc (substring prefix 0 (- len i))
                             company-lsp--completion-cache))
        (cl-return item)))))

(defun company-lsp--cache-item-new (candidates incomplete)
  "Create a new cache item.

CANDIDATES: A list of strings. The completion candidates.
INCOMPLETE: t or nil. Whether the candidates are incomplete or not."
  (list :incomplete incomplete :candidates candidates))

(defun company-lsp--cache-item-incomplete-p (cache-item)
  "Determine whether a CACHE-ITEM is incomplete."
  (plist-get cache-item :incomplete))

(defun company-lsp--cache-item-candidates (cache-item)
  "Get candidates from a CACHE-ITEM."
  (plist-get cache-item :candidates))

(defun company-lsp--documentation (candidate)
  "Get the documentation from the item in the CANDIDATE.

The documentation can be either string or MarkupContent. This method
will return markdown string if it is MarkupContent, original string
otherwise. If the documentation is not present, it will return nil
which company can handle."
  (let* ((resolved-candidate (company-lsp--resolve-candidate candidate "documentation"))
         (item (company-lsp--candidate-item resolved-candidate))
         (documentation (gethash "documentation" item)))
    (when documentation
      (lsp--render-element documentation))))

(defun company-lsp--candidates-sync (prefix)
  "Get completion candidates synchronously.

PREFIX is the prefix string for completion.

Return a list of strings as completion candidates."
  (company-lsp--on-completion
   (lsp--send-request (company-lsp--make-completion-request prefix))
   prefix))

(defun company-lsp--candidates-async (prefix callback)
  "Get completion candidates asynchronously.

PREFIX is the prefix string for completion.
CALLBACK is a function that takes a list of strings as completion candidates."
  (let ((req (company-lsp--make-completion-request prefix))
        body)
    (company-lsp--cancel-outstanding-request)
    (setq body
          (lsp--send-request-async req
                                   (lambda (resp)
                                     (setq company-lsp--last-request-id nil)
                                     (funcall callback (company-lsp--on-completion resp prefix)))))
    (setq company-lsp--last-request-id (plist-get body :id))))

(defun company-lsp--make-completion-request (prefix)
  "Make request body for completion.

PREFIX is a string prefix given by company-mode.

Returns the request body that can be used by `lsp-send-request'
or `lsp-send-request-async'."
  (let ((params (lsp--text-document-position-params)))
    (when company-lsp-enable-trigger-kind
      (setq params (plist-put params :context
                              (company-lsp--get-completion-context prefix))))
    (lsp--make-request "textDocument/completion"
                       params)))

(defun company-lsp--get-completion-context (prefix)
  "Return a plist representing a CompletionContext message for PREFIX.

Returns one of `company-lsp--trigger-kind-invoked',
`company-lsp--trigger-kind-trigger-character' and
`company-lsp--trigger-kind-incomplete'."
  (cond
   ((or (eq this-command 'company-lsp)
        (eq this-command 'company-begin-backend)
        (eq this-command 'company-complete)
        (eq this-command 'company-complete-common))
    ;; Explicitly calling completion command.
    (company-lsp--make-completion-context company-lsp--trigger-kind-invoked))
   ((company-lsp--cache-find-closest prefix)
    ;; Has incomplete candidates for sub-prefix. This assumes that if the
    ;; candidates for sub-prefix, completion won't reach this step since the
    ;; candidates for the sub-prefix can be narrowed down and returned directly.
    (company-lsp--make-completion-context
     company-lsp--trigger-kind-incomplete))
   ((or (null company-point) (< company-point (point)))
    ;; `company-point' is updated after backend gets called. So it's nil if
    ;; backend is called for a new completion, or less than current point if
    ;; backend is called after typing. Both cases indicates completion is
    ;; triggered by typing.
    (if-let (trigger-character (company-lsp--get-context-trigger-characters))
        (company-lsp--make-completion-context
         company-lsp--trigger-kind-trigger-character trigger-character)
      (company-lsp--make-completion-context
       company-lsp--trigger-kind-invoked)))
   (t (company-lsp--make-completion-context company-lsp--trigger-kind-invoked))))

(defun company-lsp--make-completion-context (trigger-kind &optional trigger-character)
  "Create a plist representing a CompletionContext message.

TRIGGER-KIND: one of `company-lsp--trigger-kind-invoked',
`company-lsp--trigger-kind-trigger-character' and
`company-lsp--trigger-kind-incomplete'.

TRIGGER-CHARACTER: The trigger characters that triggers
completion of kind `company-lsp--trigger-kind-trigger-character'.
If the length of it is greater than 1, only the last character is
used."
  (let* ((trigger-len (if trigger-character (length trigger-character)
                        0))
         (single-trigger (if (> trigger-len 1)
                             (substring trigger-character (1- trigger-len))
                           trigger-character)))
    (if trigger-character
        (list :triggerKind trigger-kind
              :triggerCharacter single-trigger)
      (list :triggerKind trigger-kind))))

(defun company-lsp--get-context-trigger-characters ()
  "Return the trigger characters after current point.

If there are multiple trigger characters matched (e.g. one is a
suffix of another), return any of them. If no trigger characters
match, return nil."
  (let ((trigger-chars (company-lsp--trigger-characters)))
    (seq-find (lambda (trigger-char)
                (and (>= (point) (length trigger-char))
                     (string= (buffer-substring (- (point) (length trigger-char))
                                                (point))
                              trigger-char)))
              trigger-chars)))

(defun company-lsp--compute-flex-match (label &optional prefix full-match)
  "Perform flex match for PREFIX in LABEL.

This function finds out substrings in LABEL. The concatenation of
those substrings is a prefix of PREFIX if FULL-MATCH is nil, or
is exactly PREFIX if FULL-MATCH is non-nil.

If PREFIX is nil, the return value of
`company-lsp--completion-prefix' is used as PREFIX.

Return a cons cell of (score . substrings). Score is a number for
sorting, the smaller the better. When FULL-MATCH is non-nil and
there is no match, score is always -1. Substrings is an alist
of (substring-start . substring-end), representing the inclusive
start position and exclusive end position of those substrings.
The alist of strings is compatible with the result for the
\"match\" command for company-mode backends. See the \"match\"
section of `company-backends' for more info. Note that if
FULL-MATCH is non-nil and the concatenation of substrings does
not equal to PREFIX, nil is returned."
  (let* ((prefix-obj (or prefix (company-lsp--completion-prefix)))
         (prefix-str (if (consp prefix-obj) (car prefix-obj) prefix-obj))
         (prefix-low (downcase prefix-str))
         (prefix-pos 0)
         (prefix-len (length prefix-low))
         (label-pos 0)
         (label-len (length label))
         (label-low (downcase label))
         substrings
         substring-start
         ;; Initial penalty for the difference of length, but with lower weight.
         (score (abs (- label-len prefix-len))))
    (if (string-empty-p prefix-str)
        '(0 . ((0 . 0)))
      (while (and (< prefix-pos prefix-len)
                  (< label-pos label-len))
        (if (= (aref prefix-low prefix-pos)
               (aref label-low label-pos))
            (progn
              (when (not substring-start)
                (setq substring-start label-pos)
                ;; We simply use the sum of all substring start positions as the
                ;; score. This is a good proxy that prioritize fewer substring parts
                ;; and earlier occurrence of substrings.
                (cl-incf score (* substring-start 100)))
              (when (not (= (aref prefix-str prefix-pos)
                            (aref label label-pos)))
                ;; The prefix and label have different cases. Adding penalty to
                ;; the score. It has lower weight than substring start but higher
                ;; than the length difference.
                (cl-incf score 10))
              (cl-incf prefix-pos))
          (when substring-start
            (push (cons substring-start label-pos) substrings)
            (setq substring-start nil)))
        (cl-incf label-pos))
      (when substring-start
        (push (cons substring-start label-pos) substrings))
      (if (or (not full-match) (= prefix-pos prefix-len))
          (cons score (nreverse substrings))
        (cons -1 nil)))))

;;;###autoload
(defun company-lsp (command &optional arg &rest _)
  "Define a company backend for lsp-mode.

See the documentation of `company-backends' for COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'company-lsp))
    (prefix
     (and
      (bound-and-true-p lsp-mode)
      (lsp--capability "completionProvider")
      (or (--some (lsp--client-completion-in-comments? (lsp--workspace-client it))
                  (lsp-workspaces))
          (not (company-in-string-or-comment)))
      (or (company-lsp--completion-prefix) 'stop)))
    (candidates
     ;; If the completion items in the response have textEdit action populated,
     ;; we'll apply them in `company-lsp--post-completion'. However, textEdit
     ;; actions only apply to the pre-completion content. We backup the current
     ;; prefix and restore it after company completion is done, so the content
     ;; is restored and textEdit actions can be applied.
     (or (company-lsp--cache-item-candidates (company-lsp--cache-get arg))
         (and company-lsp-async
              (cons :async (lambda (callback)
                             (company-lsp--candidates-async arg callback))))
         (company-lsp--candidates-sync arg)))
    (sorted t)
    (no-cache (not (eq company-lsp-cache-candidates t)))
    (annotation (lsp--annotate arg))
    (quickhelp-string (company-lsp--documentation arg))
    (doc-buffer (company-doc-buffer (company-lsp--documentation arg)))
    (match (cdr (company-lsp--compute-flex-match arg)))
    (post-completion (company-lsp--post-completion arg))))

(defun company-lsp--client-capabilities ()
  "Return the extra client capabilities supported by company-lsp."
  (when company-lsp-enable-snippet
    '(:textDocument (:completion (:completionItem (:snippetSupport t))))))

(add-hook 'lsp-before-initialize-hook
          (lambda ()
            (lsp-register-client-capabilities 'company-lsp #'company-lsp--client-capabilities)))

(provide 'company-lsp)
;;; company-lsp.el ends here
