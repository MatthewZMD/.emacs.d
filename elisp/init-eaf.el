;;; init-eaf.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-eaf.el
;; Description: Initialize Emacs Application Framework
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Tue Jun  4 00:26:09 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d pdf-tools
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes Emacs Application Framework
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const))

;; EAFPac
(use-package eaf
  :load-path (lambda () (expand-file-name "site-elisp/emacs-application-framework" user-emacs-directory))
  :if eaf-env-p
  :custom
  (browse-url-browser-function #'eaf-open-browser) ;; Make EAF Browser my default browser
  (eaf-browser-continue-where-left-off t)
  (eaf-start-python-process-when-require t)
  (eaf-browser-default-zoom 1.25)
  (eaf-browser-dark-mode nil)
  (eaf-browser-enable-adblocker t)
  (eaf-pdf-dark-mode nil)
  (eaf-browser-enable-autofill t)
  :config
  ;; Require all EAF apps unconditionally, change to apps you're interested in.
  (require 'eaf-file-manager nil t)
  (require 'eaf-music-player nil t)
  (require 'eaf-image-viewer nil t)
  (require 'eaf-camera nil t)
  (require 'eaf-js-video-player nil t)
  (require 'eaf-demo nil t)
  (require 'eaf-airshare nil t)
  (require 'eaf-terminal nil t)
  (require 'eaf-markdown-previewer nil t)
  (require 'eaf-video-player nil t)
  (require 'eaf-vue-demo nil t)
  (require 'eaf-file-sender nil t)
  (require 'eaf-pdf-viewer nil t)
  (require 'eaf-mindmap nil t)
  (require 'eaf-netease-cloud-music nil t)
  (require 'eaf-jupyter nil t)
  (require 'eaf-org-previewer nil t)
  (require 'eaf-system-monitor nil t)
  (require 'eaf-file-browser nil t)
  (require 'eaf-browser nil t)
  (require 'eaf-org)
  (when (display-graphic-p)
    (require 'eaf-all-the-icons))
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
  (eaf-bind-key open_devtools "M-i" eaf-browser-keybinding)
  (eaf-bind-key insert_or_recover_prev_close_page "X" eaf-browser-keybinding)
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key clear_cookies "C-M-q" eaf-browser-keybinding)
  (eaf-bind-key clear_history "C-M-p" eaf-browser-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
  (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_in "C-=" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_out "C--" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key eaf-send-key-sequence "M-]" eaf-terminal-keybinding))
;; -EAFPac


(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
