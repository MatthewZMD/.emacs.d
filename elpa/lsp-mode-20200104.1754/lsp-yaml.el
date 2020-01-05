;;; lsp-yaml.el --- LSP YAML server integration        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Aya Igarashi

;; Author: Aya Igarashi <ladiclexxx@gmail.com>
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

;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-yaml nil
  "LSP support for YAML, using yaml-language-server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/redhat-developer/yaml-language-server")
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-format-enable t
  "Enable/disable default YAML formatter."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-single-quote nil
  "Use single quote instead of double quotes."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-bracket-spacing t
  "Print spaces between brackets in objects."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-prose-wrap "preserve"
  "Options for prose-wrap.
   Always: wrap prose if it exeeds the print width.
   Never: never wrap the prose.
   Preserve: wrap prose as-is."
  :type '(choice
          (const "always")
          (const "never")
          (const "preserve"))
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-print-width 80
  "Specify the line length that the printer will wrap on."
  :type 'number
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-validate t
  "Enable/disable validation feature."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-hover t
  "Enable/disable hover feature."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-completion t
  "Enable/disable completion feature."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-schemas (make-hash-table)
  "Associate schemas to YAML files in a glob pattern."
  :type '(restricted-sexp :match-alternatives (hash-table-p))
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-schema-store-enable t
  "Enable/disable JSON Schema store. When set to true, available YAML
   schemas will be automatically pulled from the store."
  :type 'boolean
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-yaml-custom-tags nil
  "Custom tags for the parser to use."
  :type '(repeat string)
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(lsp-register-custom-settings
 '(("yaml.format.enable" lsp-yaml-format-enable t)
   ("yaml.format.singleQuote" lsp-yaml-single-quote t)
   ("yaml.format.bracketSpacing" lsp-yaml-bracket-spacing)
   ("yaml.format.proseWrap" lsp-yaml-prose-wrap)
   ("yaml.format.printWidth" lsp-yaml-print-width)
   ("yaml.validate" lsp-yaml-validate t)
   ("yaml.hover" lsp-yaml-hover t)
   ("yaml.completion" lsp-yaml-completion t)
   ("yaml.schemas" lsp-yaml-schemas)
   ("yaml.schemaStore.enable" lsp-yaml-schema-store-enable t)
   ("yaml.customTags" lsp-yaml-custom-tags)))

(defcustom lsp-yaml-server-command '("yaml-language-server" "--stdio")
  "Command to start yaml-languageserver."
  :type '(repeat string)
  :group 'lsp-yaml
  :package-version '(lsp-mode . "6.2"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () lsp-yaml-server-command))
                  :major-modes '(yaml-mode)
                  :priority 0
                  :server-id 'yamlls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "yaml"))))))

(provide 'lsp-yaml)
;;; lsp-yaml.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
