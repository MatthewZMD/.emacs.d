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
  (require 'init-const)
  (require 'init-global-config))

;; EAFPac
(use-package eaf
  :straight (emacs-application-framework
             :type git
             :host github
             :repo "emacs-eaf/emacs-application-framework"
             :files ("*"))
  :if (and eaf-env-p
           (file-directory-p
            (expand-file-name
             "straight/build/emacs-application-framework/app/browser"
             user-emacs-directory)))
  :custom
  (eaf-start-python-process-when-require nil)
  (browse-url-browser-function #'eaf-open-browser) ;; Make EAF Browser my default browser
  (eaf-start-python-process-when-require t)
  (eaf-browser-dark-mode nil)
  (eaf-browser-enable-adblocker t)
  (eaf-webengine-continue-where-left-off t)
  (eaf-webengine-default-zoom 1.25)
  (eaf-webengine-scroll-step 200)
  (eaf-pdf-dark-mode "ignore")
  :demand
  :bind
  (("M-z r" . eaf-open-rss-reader)
   ("M-m r" . eaf-open-rss-reader)
   ("M-#" . eaf-open-pyqterminal))
  :config
  ;; Require all EAF apps unconditionally, change to apps you're interested in.
  (when (require 'eaf-browser nil t)
    (defalias 'browse-web #'eaf-open-browser)
    (eaf-bind-key nil "M-q" eaf-browser-keybinding)
    (eaf-bind-key nil "M-z" eaf-browser-keybinding)
    (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
    (eaf-bind-key open_devtools "M-i" eaf-browser-keybinding)
    (eaf-bind-key insert_or_recover_prev_close_page "X" eaf-browser-keybinding)
    (eaf-bind-key delete_cookies "C-M-q" eaf-browser-keybinding)
    (eaf-bind-key delete_all_cookies "C-M-Q" eaf-browser-keybinding)
    (eaf-bind-key clear_history "C-M-p" eaf-browser-keybinding))
  (when (require 'eaf-pdf-viewer nil t)
    (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
    (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
    (eaf-bind-key zoom_in "C-=" eaf-pdf-viewer-keybinding)
    (eaf-bind-key zoom_out "C--" eaf-pdf-viewer-keybinding))
  (require 'eaf-file-manager nil t)
  (require 'eaf-music-player nil t)
  (require 'eaf-image-viewer nil t)
  (when (require 'eaf-camera nil t)
    (eaf-bind-key take_photo "p" eaf-camera-keybinding))
  (require 'eaf-demo nil t)
  (require 'eaf-airshare nil t)
  (require 'eaf-markdown-previewer nil t)
  (require 'eaf-video-player nil t)
  (require 'eaf-vue-demo nil t)
  (require 'eaf-file-sender nil t)
  (require 'eaf-mindmap nil t)
  (require 'eaf-netease-cloud-music nil t)
  (require 'eaf-jupyter nil t)
  (require 'eaf-org-previewer nil t)
  (require 'eaf-system-monitor nil t)
  (require 'eaf-rss-reader nil t)
  (require 'eaf-pyqterminal nil t)
  (require 'eaf-file-browser nil t)
  (require 'eaf-git nil t)
  (when (display-graphic-p)
    (require 'eaf-all-the-icons)))
;; -EAFPac

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
