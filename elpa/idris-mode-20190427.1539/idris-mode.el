;;; idris-mode.el --- Major mode for editing Idris code -*- lexical-binding: t -*-

;; Copyright (C) 2013

;; Author:
;; URL: https://github.com/idris-hackers/idris-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24") (prop-menu "0.1") (cl-lib "0.5"))
;; Version: 0.9.18


;;; Commentary:

;; This is an Emacs mode for editing Idris code. It requires the latest
;; version of Idris, and some features may rely on the latest Git version of
;; Idris.

;;; Code:

(require 'prop-menu)
(require 'eldoc)

(require 'idris-core)
(require 'idris-settings)
(require 'idris-syntax)
(require 'idris-simple-indent)
(require 'idris-repl)
(require 'idris-commands)
(require 'idris-warnings)
(require 'idris-common-utils)
(require 'idris-ipkg-mode)


(defun idris-mode-context-menu-items (plist)
  "Compute menu items from PLIST that are specific to editing text in `idris-mode'."
  (let ((ref (plist-get plist 'idris-ref))
        (ref-style (plist-get plist 'idris-ref-style)))
    (when (and ref (equal ref-style :metavar))
      (list (list "Extract lemma"
                  (let ((location (point)))
                    (lambda ()
                      (interactive)
                      (save-excursion
                        (goto-char location)
                        (idris-make-lemma)))))
            (list "Fill with case block"
                  (let ((location (point)))
                    (lambda ()
                      (interactive)
                      (save-excursion
                        (goto-char location)
                        (idris-make-cases-from-hole)))))))))

(defvar idris-mode-map (let ((map (make-sparse-keymap)))
                         (cl-loop for keyer
                                  in '(idris-define-loading-keys
                                       idris-define-docs-keys
                                       idris-define-editing-keys
                                       idris-define-general-keys
                                       idris-define-ipkg-keys
                                       idris-define-ipkg-opening-keys)
                                  do (funcall keyer map))
                         map)
  "Keymap used in Idris mode.")



(easy-menu-define idris-mode-menu idris-mode-map
  "Menu for the Idris major mode"
  `("Idris"
    ["New Project" idris-start-project t]
    "-----------------"
    ["Load file" idris-load-file t]
    ["Choose packages" idris-set-idris-load-packages t]
    ["Compile and execute" idris-compile-and-execute]
    ["Delete IBC file" idris-delete-ibc t]
    ["View compiler log" idris-view-compiler-log (get-buffer idris-log-buffer-name)]
    ["Quit inferior idris process" idris-quit t]
    "-----------------"
    ["Add initial match clause to type declaration" idris-add-clause t]
    ["Add missing cases" idris-add-missing t]
    ["Case split pattern variable" idris-case-split t]
    ["Add with block" idris-make-with-block t]
    ["Extract lemma from hole" idris-make-lemma t]
    ["Solve hole with case expression" idris-make-cases-from-hole t]
    ["Attempt to solve hole" idris-proof-search t]
    ["Display type" idris-type-at-point t]
    "-----------------"
    ["Open package" idris-open-package-file t]
    ["Build package" idris-ipkg-build t]
    ["Install package" idris-ipkg-install t]
    ["Clean package" idris-ipkg-clean t]
    "-----------------"
    ["Get documentation" idris-docs-at-point t]
    ["Search for type" idris-type-search t]
    ["Apropos" idris-apropos t]
    ["Browse namespace" idris-browse-namespace t]
    ["Pretty-print to HTML or LaTeX" idris-pretty-print t]
    "-----------------"
    ("Interpreter options" :active idris-process
     ["Show implicits" (idris-set-option :show-implicits t)
      :visible (not (idris-get-option :show-implicits))]
     ["Hide implicits" (idris-set-option :show-implicits nil)
      :visible (idris-get-option :show-implicits)]
     ["Show error context" (idris-set-option :error-context t)
      :visible (not (idris-get-option :error-context))]
     ["Hide error context" (idris-set-option :error-context nil)
      :visible (idris-get-option :error-context)])
    ["Customize idris-mode" (customize-group 'idris) t]
    ["Customize fonts and colors" (customize-group 'idris-faces) t]))


;;;###autoload
(define-derived-mode idris-mode prog-mode "Idris"
  "Major mode for Idris
     \\{idris-mode-map}
Invokes `idris-mode-hook'."
  :syntax-table idris-syntax-table
  :group 'idris
  (set (make-local-variable 'font-lock-defaults)
       (idris-font-lock-defaults))
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "--")

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'syntax-propertize-function) 'idris-syntax-propertize-function)

  ;; REPL completion for Idris source
  (set (make-local-variable 'completion-at-point-functions)
       (list 'idris-complete-symbol-at-point
             'idris-complete-keyword-at-point))

  ;; imenu support
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (set (make-local-variable 'imenu-generic-expression)
       '(("Data" "^\\s-*data\\s-+\\(\\sw+\\)" 1)
         ("Data" "^\\s-*record\\s-+\\(\\sw+\\)" 1)
         ("Data" "^\\s-*codata\\s-+\\(\\sw+\\)" 1)
         ("Postulates" "^\\s-*postulate\\s-+\\(\\sw+\\)" 1)
         ("Classes" "^\\s-*class\\s-+\\(\\sw+\\)" 1)
         (nil "^\\s-*\\(\\sw+\\)\\s-*:" 1)
         ("Namespaces" "^\\s-*namespace\\s-+\\(\\sw\\|\\.\\)" 1)))

  ;; eldoc support
  (set (make-local-variable 'eldoc-documentation-function) 'idris-eldoc-lookup)

  ;; Filling of comments and docs
  (set (make-local-variable 'fill-paragraph-function) 'idris-fill-paragraph)
  ;; Make dirty if necessary
  (add-hook (make-local-variable 'after-change-functions) 'idris-possibly-make-dirty)
  (setq mode-name `("Idris"
                    (:eval (if idris-rex-continuations "!" ""))
                    " "
                    (:eval (if (idris-current-buffer-dirty-p)
                               "(Not loaded)"
                             "(Loaded)"))))
  ;; Extra hook for LIDR files (to set up extra highlighting, etc)
  (when (idris-lidr-p)
    (run-hooks 'idris-mode-lidr-hook))
  (set (make-local-variable 'prop-menu-item-functions)
       '(idris-context-menu-items idris-mode-context-menu-items)))

;; Automatically use idris-mode for .idr and .lidr files.
;;;###autoload
(push '("\\.idr$" . idris-mode) auto-mode-alist)
;;;###autoload
(push '("\\.lidr$" . idris-mode) auto-mode-alist)


;;; Handy utilities for other modes
(eval-after-load 'flycheck
  '(eval
    '(progn
       (flycheck-define-checker idris
         "An Idris syntax and type checker."
         :command ("idris"
                 "--check" "--nocolor" "--warnpartial"
                 ;; Compute the command-line options similarly to inferior-idris
                 (eval (idris-compute-flags))
                 source)
         :error-patterns
         ((warning line-start (file-name) ":" line ":" column ":Warning - "
                   (message (and (* nonl) (* "\n" (not (any "/" "~")) (* nonl)))))
          (error line-start (file-name) ":" line ":" column ":"
                 (message (and (* nonl) (* "\n" (not (any "/" "~")) (* nonl))))))
         :modes idris-mode)

       (add-to-list 'flycheck-checkers 'idris))))

(provide 'idris-mode)
;;; idris-mode.el ends here
