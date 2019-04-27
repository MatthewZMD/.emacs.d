<a id="orgc091f9e"></a>

# M-EMACS


<a id="orgb938723"></a>

# About

M-EMACS is a custom GNU Emacs setup and configurations that aims not only to enhance the default Emacs experience, and hopefully be a sample that everyone can easily navigate and reference through a highly detailed README that contains 99% of the **entire** configuration code.

As a young EMACSer, I have experienced the struggle to find a detailed configuration that is loosely coupled and highly readable. This mostly due to the nature of source codes, sometimes comments are harder to notice or simply not enough. Therefore I decided to construct this README and present any human-readable explanation in a much more human-friendly way. Anyone, particularly Emacs beginners who have no idea where to start with their personal config, is more than welcome to read through this document and copy/paste any part to use it on their own.

This configuration is designed and tested for **GNU Emacs 25.1 and above only**.

```emacs-lisp
(when (version< emacs-version "25.1")
  (error "M-EMACS requires Emacs 25.1 and above!"))
```


<a id="org29f2dc2"></a>

## About README

This README is originated from init.org that is generated using `M-x org-gfm-export-to-markdown`. Every block of code is generated through this function - it exports the section of code from the `elisp/` directory. You will not see their presence in init.org.


<a id="org831d73a"></a>

## Installation

1.  Install latest version of [GNU Emacs](https://www.gnu.org/software/emacs/).
2.  Clone this repo to `HOME` or `~/` path using [git](https://git-scm.com/) and update all the submodules.

    ```text
    git clone --recurse-submodules -j8 https://github.com/MatthewZMD/.emacs.d.git
    cd .emacs.d
    ```
3.  Ensure a stable connection to Melpa Packages, then open Emacs.
4.  Enter `y` when prompted with `Auto-update packages now?`, wait for all packages to install.
5.  In your favorite browser, `Ctrl-F Prerequisite` through this webpage and follow the **Prerequisite** instructions.
6.  Restart Emacs.


### Further Updates

I will be updating M-EMACS from time to time, it is best to `git pull` once a while to stay up to date.

Please also execute `git submodule update --recursive --remote` to sync with all the submodules.


<a id="orgea5ce87"></a>

## Modification

You have the permission to use, modify, distribute however you want. However, what is *free* stays *free*. After all, this is [GPL](LICENSE).

**Remember** you must manually sync this README with all the new changes you made by:

1.  Please do **NOT** edit README.md file, edit init.org instead!
2.  If you add a new mode, create a new `<file-name>.el` file in `elisp/` directory.
3.  Put `(require '<file-name>)` in [init.el](init.el) accordingly.
4.  Add `#+INCLUDE: "~/.emacs.d/elisp/<place-holder>.el" src emacs-lisp :range-begin "<start-line-wrapper-exclusive>" :range-end "<end-line-wrapper-exclusive>"` in the appropriate section in init.org.
5.  Enter `C-x C-s` to save and update `:lines`. (if you don't see the updated effect, run `M-x update-includes` manually)
6.  Call `M-x org-gfm-export-to-markdown` to update this README.


<a id="orge2641f5"></a>

## Spotted a Bug? Suggestions?

Please fill in Issues.


<a id="org9f310a4"></a>

## Special Thanks

Everyone starts somewhere, and I started here.

-   [Vincent Zhang's Centaur Emacs](https://github.com/seagle0128/.emacs.d)
-   [Henrik Lissner's Doom Emacs](https://github.com/hlissner/doom-emacs)
-   [Poncie Reyes's .emacs.d](https://github.com/poncie/.emacs.d)


# Table of Contents     :TOC_2_ORG:

-   [M-EMACS](#orgc091f9e)
-   [About](#orgb938723)
    -   [About README](#org29f2dc2)
    -   [Installation](#org831d73a)
    -   [Modification](#orgea5ce87)
    -   [Spotted a Bug? Suggestions?](#orge2641f5)
    -   [Special Thanks](#org9f310a4)
-   [Startup](#org15f207c)
    -   [Lexical-binding](#org271cd06)
    -   [Disable Unnecessary Interface](#orgad4097c)
    -   [Optimization](#org8447ad9)
    -   [Load-Path](#org557562a)
    -   [Define Constants](#org3caf100)
-   [Package Management](#orgc798ff1)
    -   [Melpa Packages](#orgd57533b)
    -   [Non-Melpa Packages](#org0929616)
    -   [Configure Package Management](#org70de357)
    -   [Use-Package Wrapper](#org4e5f2a8)
    -   [Auto Package Update](#org05ddf85)
-   [Global Functionalities](#org72e086d)
    -   [User Information](#org49ed787)
    -   [Bindings](#org66e0608)
    -   [Directory and Files Management](#org9c64373)
    -   [Search](#org624e7d3)
    -   [Avy](#orgdac7887)
    -   [Ivy](#org4df927d)
    -   [Winner](#org683c47e)
    -   [Which Key](#org4d89a77)
    -   [Popup Kill Ring](#org5bd9304)
    -   [Undo Tree](#org2bd9b3f)
    -   [Shell Here](#org49cd895)
    -   [Discover My Major](#org2c8381a)
    -   [Ace Window](#org392362b)
    -   [Configs](#org92953e8)
    -   [More Functions](#orge997a44)
-   [User Interface Enhancements](#org3d727e6)
    -   [Title Bar](#orgcd16a68)
    -   [All The Icons](#org78732a5)
    -   [Doom Theme](#org0c0820b)
    -   [Doom Modeline](#org96d2402)
    -   [Dashboard](#org6d47f0d)
    -   [Fonts](#org05a5431)
    -   [Zone](#org5d42481)
    -   [Diminish](#org11976d5)
    -   [Dimmer](#orge3ecbdc)
    -   [Smooth Scroll](#orgf2efebd)
    -   [Highlight Indent Guides](#orgaab0bd6)
    -   [Pretty Symbols](#orgb9e554a)
    -   [UI Configs](#org7b652e7)
-   [General Programming](#org8b113ee)
    -   [Magit](#orgd7f7250)
    -   [Projectile](#org818509f)
    -   [Treemacs](#orgb11f313)
    -   [Company](#org3722140)
    -   [YASnippet](#org2898f65)
    -   [Flycheck](#org824e8cc)
    -   [Dumb Jump](#org56e00e0)
    -   [Line Numbers](#orgee6a24f)
    -   [Parenthesis Pairing](#org57ede11)
    -   [Format All](#org664d67d)
    -   [Evil-Nerd-Commenter](#org3ee1a21)
    -   [Iedit](#org3706d3f)
    -   [Header](#org21e13e8)
    -   [LSP](#org244cb85)
-   [Programming](#orgf54d53f)
    -   [Emacs Lisp](#org86cc284)
    -   [Java](#org9ffa36b)
    -   [C/C++](#org1a66611)
    -   [Python](#org2e04eef)
    -   [Arduino](#org68aa797)
    -   [Jupyter Notebook](#orged8c3b0)
-   [Web Development](#orgd412f69)
    -   [Web Mode](#orge8c5d03)
    -   [Emmet](#org4da7bc0)
    -   [JavaScript/TypeScript](#org0958767)
-   [Miscellaneous](#orgcf243cb)
    -   [Org](#org8b6c783)
    -   [Emacs Web Wowser](#org2e43831)
    -   [LeetCode](#org05b8d45)
    -   [Tetris](#orgd186321)
    -   [Speed Type](#orgb005d28)
    -   [2048 Game](#orge86311b)


<a id="org15f207c"></a>

# Startup


<a id="org271cd06"></a>

## Lexical-binding

Use lexical-binding. [Why?](https://nullprogram.com/blog/2016/12/22/)

> Until Emacs 24.1 (June 2012), Elisp only had dynamically scoped variables, a feature, mostly by accident, common to old lisp dialects. While dynamic scope has some selective uses, it’s widely regarded as a mistake for local variables, and virtually no other languages have adopted it.

```emacs-lisp
;;; init.el --- -*- lexical-binding: t -*-
```


<a id="orgad4097c"></a>

## Disable Unnecessary Interface

This need to be in the beginning of initialization to smooth the experience.

```emacs-lisp
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
```


<a id="org8447ad9"></a>

## Optimization

Avoid garbage collection during startup.

```emacs-lisp
(eval-and-compile
  (defun revert-gc ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1))

  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (add-hook 'emacs-startup-hook 'revert-gc))
```

Unset file name handler alist.

```emacs-lisp
(eval-and-compile
  (defun reset-file-name-handler-alist ()
    (setq file-name-handler-alist orig-file-name-handler-alist))

  (defvar orig-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (add-hook 'emacs-startup-hook 'reset-file-name-handler-alist))
```


<a id="org557562a"></a>

## Load-Path

```emacs-lisp
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (add-to-list 'load-path base)
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (add-to-list 'load-path name))))))
```

Since all the configuration files are stored in `elisp/` folder, as well as the `site-elisp/` folder that contains non-Melpa packages, they need to be added to `load-path` on startup.

```emacs-lisp
(update-to-load-path "~/.emacs.d/elisp")
(update-to-load-path "~/.emacs.d/site-elisp")
```


<a id="org3caf100"></a>

## Define Constants

```emacs-lisp
(defconst *gui-emacs* (display-graphic-p))

(defconst *rg* (executable-find "rg"))

(defconst *tr* (executable-find "tr"))

(defconst *mvn* (executable-find "mvn"))

(defconst *clangd* (or (executable-find "clangd")  ;; usually
                       (executable-find "/usr/local/opt/llvm/bin/clangd")))  ;; macOS

(defconst *gcc* (executable-find "gcc"))

(defconst *curl* (executable-find "curl"))
```


<a id="orgc798ff1"></a>

# Package Management

Some packages are turned off because I don't use them very often. Please verify them in `init.el`.


<a id="orgd57533b"></a>

## Melpa Packages

```emacs-lisp
;; Select the folder to store packages
(setq package-user-dir "~/.emacs.d/elpa"
      package-archives
      '(;; Comment / Uncomment when necessary sites are used
        ("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa stable" . "http://stable.melpa.org/packages/")
        ;;("org"   . "http://orgmode.org/elpa/")
        ))
```

Load all Melpa packages in `elisp/` to `load-path` directly to reduce startup time.


<a id="org0929616"></a>

## Non-Melpa Packages

These packages are contained in `site-elisp/`, they are added to


### Add Packages Manually from Git

```text
cd site-elisp/
git submodule add https://github.com/foo/bar.git
```

Verify `/.gitmodules` file that the newly added package exist.


### Update Manually Added Packages

```text
git submodule init
git submodule update
```


<a id="org70de357"></a>

## Configure Package Management

```emacs-lisp
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)

;; Ask package.el to not add (package-initialize) to .emacs.d
(setq package--init-file-ensured t)

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
```


<a id="org4e5f2a8"></a>

## Use-Package Wrapper

My Emacs configuration is almost entirely dependant on a faster implementation of [use-package](https://github.com/jwiegley/use-package) based on [Doom Emacs](https://github.com/hlissner/doom-emacs/blob/master/core/core-packages.el#L323).

> The `use-package` macro allows you to isolate package configuration in your .emacs file in a way that is both performance-oriented and, well, tidy. I created it because I have over 80 packages that I use in Emacs, and things were getting difficult to manage. Yet with this utility my total load time is around 2 seconds, with no loss of functionality!

Add the macro generated list of package.el loadpaths to load-path.

```emacs-lisp
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        ;; (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        ;; (setq use-package-always-ensure t) ;; I will handle this myself
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse
           (apply #'nconc
                  ;; Only keep package.el provided loadpaths.
                  (mapcar #'(lambda (path)
                              (if (string-prefix-p package-user-dir-real path)
                                  (list path)
                                nil))
                          load-path))))))
(eval-when-compile
  (require 'use-package)
  ;; Always ensure package is installed
  (setq use-package-always-ensure t))
(require 'bind-key)
```

The `use-package` wrapper.

```emacs-lisp
(defmacro def-package (name &rest plist)
  "A thin wrapper around `use-package'."
  ;; If byte-compiling, ignore this package if it doesn't meet the condition.
  ;; This avoids false-positive load errors.
  (unless (and (bound-and-true-p byte-compile-current-file)
               (or (and (plist-member plist :if)     (not (eval (plist-get plist :if))))
                   (and (plist-member plist :when)   (not (eval (plist-get plist :when))))
                   (and (plist-member plist :unless) (eval (plist-get plist :unless)))))
    `(use-package ,name ,@plist)))
```


<a id="org05ddf85"></a>

## Auto Package Update

[Auto package update](https://github.com/rranelli/auto-package-update.el) automatically updates installed packages if at least `auto-package-update-interval` days have passed since the last update.

```emacs-lisp
(def-package auto-package-update
  :config
  (setq auto-package-update-interval 7) ;; in days
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
```


<a id="org72e086d"></a>

# Global Functionalities


<a id="org49ed787"></a>

## User Information

**Prerequisite**: Please update this file your personal info.

```emacs-lisp
(setq user-full-name "your-name")
(setq user-mail-address "your-email")
```


<a id="org66e0608"></a>

## Bindings

```emacs-lisp
;; Unbind C-z to use as prefix
(global-set-key (kbd "C-z") nil)
;; Use iBuffer instead of Buffer List
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Truncate lines
(global-set-key (kbd "C-x C-!") 'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd"C--") 'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
```


<a id="org9c64373"></a>

## Directory and Files Management


### Dired

Dired, the directory editor.

```emacs-lisp
(def-package dired
  :ensure nil
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Auto refresh Dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; Quickly copy/move file in Dired
  (setq dired-dwim-target t)

  ;; Move files to trash when deleting
  (setq delete-by-moving-to-trash t)

  ;; Reuse same dired buffer, so doesn't create new buffer each time
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)))
  (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "RET") #'dired-find-alternate-file)))
  (add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "^")
                                          (lambda () (interactive) (find-alternate-file ".."))))))
```


### Automatically Save File and Backup

Create directory where Emacs stores backups files.

```emacs-lisp
(make-directory "~/.emacs.d/autosaves" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t)
```

1.  Auto-Save

    [Auto-save](https://github.com/manateelazycat/auto-save), automatically save files without temporary files.

    ```emacs-lisp
    (def-package auto-save
      :ensure nil
      :init
      (setq auto-save-default nil)
      :config
      (setq auto-save-silent t)
      (setq auto-save-idle 5)
      (auto-save-enable))
    ```


### Rename Both File and Buffer

```emacs-lisp
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
```


### File Configs

```emacs-lisp
;; Load the newest version of a file
(setq load-prefer-newer t)

;; Detect external file changes and auto refresh file
(global-auto-revert-mode t)

;; Transparently open compressed files
(auto-compression-mode t)
```


<a id="org624e7d3"></a>

## Search

**Prerequisite**: Please [install](https://github.com/BurntSushi/ripgrep#installation) ripgrep and add `rg` to the `PATH`.


### Color-RG

[Color-RG](https://github.com/manateelazycat/color-rg), a search and refactoring tool based on ripgrep that is used to search text.

```emacs-lisp
(when *rg*
  (def-package color-rg
    :ensure nil
    :bind
    (("C-z s s" . color-rg-search-input))))
```


### Grep-Dired

[Grep-Dired](https://github.com/manateelazycat/grep-dired), a search and refactoring tool based on ripgrep that is used to search files.

```emacs-lisp
(when *rg*
  (def-package grep-dired
    :ensure nil
    :bind
    (("C-z s f" . grep-dired-dwim)
     ("C-z s d" . grep-dired))))
```


<a id="orgdac7887"></a>

## Avy

[Avy](https://github.com/abo-abo/avy), a nice way to move around text.

```emacs-lisp
(def-package avy
  :defer t
  :bind
  (("C-;" . avy-goto-char-timer)
   ("C-:" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.3)
  (setq avy-style 'pre))
```


<a id="org4df927d"></a>

## Ivy


### Main Ivy

[Ivy](https://github.com/abo-abo/swiper), a generic completion mechanism for Emacs.

```emacs-lisp
(def-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-on-del-error-function nil)
  (setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (setq ivy-count-format "【%d/%d】")
  (setq ivy-wrap t))
```


### Amx

[Amx](https://github.com/DarwinAwardWinner/amx), a M-x enhancement tool forked from [Smex](https://github.com/nonsequitur/smex).

```emacs-lisp
(def-package amx
  :after (:any ivy ido)
  :config (amx-mode))
```


### Counsel

[Counsel](https://github.com/abo-abo/swiper), a collection of Ivy-enhanced versions of common Emacs commands.

```emacs-lisp
(def-package counsel
  :after ivy
  :diminish counsel-mode
  :init (counsel-mode 1))
```


### Swiper

[Swiper](https://github.com/abo-abo/swiper), an Ivy-enhanced alternative to isearch.

```emacs-lisp
(def-package swiper
  :bind ("C-s" . swiper))
```


<a id="org683c47e"></a>

## Winner

Winner mode restores old window layout.

```emacs-lisp
(setq winner-boring-buffers
      '("*Completions*"
        "*Compile-Log*"
        "*inferior-lisp*"
        "*Fuzzy Completions*"
        "*Apropos*"
        "*Help*"
        "*cvs*"
        "*Buffer List*"
        "*Ibuffer*"
        "*esh command on file*"))
(winner-mode 1)
```


<a id="org4d89a77"></a>

## Which Key

[Which key](https://github.com/justbur/emacs-which-key), a feature that displays the key bindings following the incomplete command.

```emacs-lisp
(def-package which-key
  :diminish
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))
```


<a id="org5bd9304"></a>

## Popup Kill Ring

[Popup kill ring](https://github.com/waymondo/popup-kill-ring), a feature that provides the ability to browse Emacs kill ring in autocomplete style popup menu.

```emacs-lisp
(def-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))
```


<a id="org2bd9b3f"></a>

## Undo Tree

[Undo tree](https://www.emacswiki.org/emacs/UndoTree), a feature that provides a visualization of the undos in a file.

```emacs-lisp
(def-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))
```


<a id="org49cd895"></a>

## Shell Here

[Shell-here](https://github.com/ieure/shell-here), a tool that opens a shell buffer in (or relative to) `default-directory`.

```emacs-lisp
(def-package shell-here
  :bind ("C-!" . shell-here))
```


<a id="org2c8381a"></a>

## Discover My Major

[Discover my major](https://github.com/jguenther/discover-my-major), a feature that discovers key bindings and their meaning for the current Emacs major mode.

```emacs-lisp
(def-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)))
```


<a id="org392362b"></a>

## Ace Window

[Ace Window](https://github.com/abo-abo/ace-window), a package for selecting windows to switch to.

```emacs-lisp
(def-package ace-window
  :bind ("C-x C-o" . ace-window))
```


<a id="org92953e8"></a>

## Configs

Some essential configs that make my life a lot easier.


### UTF-8 Coding System

Use UTF-8 as much as possible with unix line endings.

```emacs-lisp
(if (eq system-type 'windows-nt)
    (progn
      (set-clipboard-coding-system 'utf-16-le)
      (set-selection-coding-system 'utf-16-le))
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
```


### Turn Off Cursor Alarms

```emacs-lisp
(setq ring-bell-function 'ignore)
```


### Show Keystrokes in Progress Instantly

```emacs-lisp
(setq echo-keystrokes 0.1)
```


### Optimize Editing Experience

```emacs-lisp
;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Make sentences end with a single space
(setq-default sentence-end-double-space nil)

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Disable Shift mark
(setq shift-select-mode nil)

;; Replace selection on insert
(delete-selection-mode 1)

;; Merge system clipboard with Emacs
(setq-default select-enable-clipboard t)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(c-set-offset 'comment-intro 0)
```


### Automatic Garbage Collect

Garbage collect when Emacs is not in focus.

```emacs-lisp
(add-hook 'focus-out-hook #'garbage-collect)
```


### Don't Lock Files

```emacs-lisp
(setq-default create-lockfiles nil)
```


### Longer History Length

```emacs-lisp
(setq-default history-length 500)
```


### Compilation

Better compilation configurations.

```emacs-lisp
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)
```


### Move Custom-Set-Variables to Different File

```emacs-lisp
(setq custom-file (concat user-emacs-directory "elisp/init-custom.el"))
(load custom-file 'noerror)
```


<a id="orge997a44"></a>

## More Functions

Other important, but longer functions.


### Resize Window Width / Height Functions

```emacs-lisp
;; Resizes the window width based on the input
(defun window-resize-width (w)
  "Resizes the window width based on W."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window width in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (message "%s" w)
  (window-resize nil (- (truncate (* (/ w 10.0) (frame-width))) (window-total-width)) t))

;; Resizes the window height based on the input
(defun window-resize-height (h)
  "Resizes the window height based on H."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window height in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (message "%s" h)
  (window-resize nil (- (truncate (* (/ h 10.0) (frame-height))) (window-total-height)) nil))

;; Setup shorcuts for window resize width and height
(global-set-key (kbd "C-x C-|") #'window-resize-width)
(global-set-key (kbd "C-x C-_") #'window-resize-height)
```


### Edit This Configuration File Shortcut

```emacs-lisp
(defun edit-configs ()
  "Opens the README.org file."
  (interactive)
  (find-file "~/.emacs.d/init.org"))

(global-set-key (kbd "C-z e") #'edit-configs)
```


### Smarter Move Beginning of Line

Smarter navigation to the beginning of a line by [Bozhidar Batsov](https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/).

```emacs-lisp
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.    If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
```


### Update Org Mode Include Automatically

Update Org Mode INCLUDE Statements Automatically from [Artur Malabarba](http://endlessparentheses.com/updating-org-mode-include-statements-on-the-fly.html).

```emacs-lisp
(add-hook 'before-save-hook #'update-includes)

(defun update-includes (&rest ignore)
  "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have either :range-begin or :range-end.
This function does nothing if not in org-mode, so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1)))
               lines begin end)
          (forward-line 0)
          (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
            (setq begin (match-string-no-properties 1)))
          (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
            (setq end (match-string-no-properties 1)))
          (setq lines (decide-line-range file begin end))
          (when lines
            (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                (replace-match lines :fixedcase :literal nil 1)
              (goto-char (line-end-position))
              (insert " :lines \"" lines "\""))))))))

(defun decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file file)
        (goto-char (point-min))
        (if (null begin)
            (setq l "")
          (search-forward-regexp begin)
          (setq l (line-number-at-pos (match-beginning 0))))
        (if (null end)
            (setq r "")
          (search-forward-regexp end)
          (setq r (1+ (line-number-at-pos (match-end 0)))))
        (format "%s-%s" (+ l 1) (- r 1)))))) ;; Exclude wrapper
```


### MiniBuffer

```emacs-lisp
(defun abort-minibuffer-using-mouse ()
  "Abort the minibuffer when using the mouse."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'abort-minibuffer-using-mouse)

;; keep the point out of the minibuffer
(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
```


### Duplicate Line

```emacs-lisp
(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (move-beginning-of-line 1)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(global-set-key (kbd "C-z l") #'duplicate-line)
```


<a id="org3d727e6"></a>

# User Interface Enhancements


<a id="orgcd16a68"></a>

## Title Bar

```emacs-lisp
(setq-default frame-title-format '("M-EMACS - " user-login-name "@" system-name " - %b"))
```


<a id="org78732a5"></a>

## All The Icons

[All The Icons](https://github.com/domtronn/all-the-icons.el), a utility package to collect various Icon Fonts.

**Prerequisite**: Install all fonts from `fonts/all-the-icons-fonts`.

```emacs-lisp
(def-package all-the-icons)
```


### All The Icons Dired

[All The Icons Dired](https://github.com/jtbm37/all-the-icons-dired), an icon set for Dired.

```emacs-lisp
(def-package all-the-icons-dired
  :after all-the-icons
  :diminish
  :config (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  :custom-face (all-the-icons-dired-dir-face ((t `(:foreground ,(face-background 'default))))))
```


### All The Icons Ivy

[All The Icons Ivy](https://github.com/asok/all-the-icons-ivy), an icon set for Ivy.

```emacs-lisp
(def-package all-the-icons-ivy
  :after all-the-icons
  :config
  (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-buffer-commands '())
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))
```


<a id="org0c0820b"></a>

## Doom Theme

[doom-themes](https://github.com/hlissner/emacs-doom-themes), an UI plugin and pack of theme. It is set to default to Dracula theme.

```emacs-lisp
(when *gui-emacs*
  (def-package doom-themes
    :config
    ;; flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
    (load-theme 'doom-dracula t)))
```


<a id="org96d2402"></a>

## Doom Modeline

[Doom modeline](https://github.com/seagle0128/doom-modeline), a modeline from DOOM Emacs, but more powerful and faster.

```emacs-lisp
(when *gui-emacs*
  (def-package doom-modeline
    :hook (after-init . doom-modeline-mode)
    :config
    ;; Don't compact font caches during GC. Windows Laggy Issue
    (setq inhibit-compacting-font-caches t)
    (setq doom-modeline-minor-modes t)
    ;;(setq doom-modeline-github t) ;; requires ghub package
    (setq doom-modeline-icon t)
    (setq doom-modeline-major-mode-color-icon t)
    (setq doom-modeline-height 15)))
```


<a id="org6d47f0d"></a>

## Dashboard


### Dashboard

[Dashboard](https://github.com/rakanalh/emacs-dashboard), an extensible Emacs startup screen.

Use either `KEC_Dark_BK.png` or `KEC_Light_BK.png` depends on the backgrond theme.

```emacs-lisp
(def-package dashboard
  :diminish (dashboard-mode page-break-lines-mode)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Close the world. Open the nExt.")
  (setq dashboard-startup-banner "~/.emacs.d/images/KEC_Dark_BK_Small.png")

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (if (> (length (window-list-1))
           ;; exclude `treemacs' window
           (if (and (fboundp 'treemacs-current-visibility)
                    (eq (treemacs-current-visibility) 'visible)) 2 1))
        (setq dashboard-recover-layout-p t))
    (delete-other-windows))
  (global-set-key (kbd "C-z d") #'open-dashboard)

  ;; Additional Dashboard widgets.
  (defun dashboard-insert-widgets (list-size)
    (insert (format "%d packages loaded in %s.\n" (length package-activated-list) (emacs-init-time)))
    (insert "Navigation: ")
    ;;(insert (make-string (max 0 (floor (/ (- dashboard-banner-length 25) 2))) ?\ ))
    (widget-create 'url-link
                   :tag (propertize "Github" 'face 'font-lock-keyword-face)
                   :help-echo "Open the Emacs Configuration Github page"
                   :mouse-face 'highlight
                   "https://github.com/MatthewZMD/.emacs.d")
    (insert " ")
    (widget-create 'push-button
                   :help-echo "Edit This Emacs' Configuration"
                   :action (lambda (&rest _) (edit-configs))
                   :mouse-face 'highlight
                   :button-prefix ""
                   :button-suffix ""
                   (propertize "Configuration" 'face 'font-lock-keyword-face)))

  (add-to-list 'dashboard-item-generators  '(buttons . dashboard-insert-widgets))
  (add-to-list 'dashboard-items '(buttons)))
```


### Page Break Lines

[Page-break-lines](https://github.com/purcell/page-break-lines), a feature that displays ugly form feed characters as tidy horizontal rules.

```emacs-lisp
(def-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))
```


<a id="org05a5431"></a>

## Fonts

Prepares fonts to use.

**Prerequisite**: Install `Input` and `Love Letter TW` from `fonts/`.

```emacs-lisp
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar fonts '(("Input" . 11) ("SF Mono" . 12) ("Consolas" . 12) ("Love LetterTW" . 12.5))
  "List of fonts and sizes.  The first one available will be used.")
```

Change Font Function.

```emacs-lisp
(defun change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font fonts (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))

    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))

      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

(change-font)
```


<a id="org5d42481"></a>

## Zone

[Zone mode](https://www.emacswiki.org/emacs/ZoneMode), a minor-mode 'zones' Emacs out, choosing one of its random modes to obfuscate the current buffer, which is used as my Emacs screensaver.

```emacs-lisp
(def-package zone
  :ensure nil
  :config
  (zone-when-idle 600) ;; in seconds

  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone))))
```


<a id="org11976d5"></a>

## Diminish

[Diminish](https://github.com/emacsmirror/diminish), a feature that removes certain minor modes from mode-line.

```emacs-lisp
(def-package diminish)
```


<a id="orge3ecbdc"></a>

## Dimmer

[Dimmer](https://github.com/gonewest818/dimmer.el), a feature that visually highlights the selected buffer.

```emacs-lisp
(def-package dimmer
  :init (dimmer-mode)
  :config
  (setq dimmer-fraction 0.2)
  (setq dimmer-exclusion-regexp "\\*Minibuf-[0-9]+\\*\\|\\*dashboard\\*"))
```


<a id="orgf2efebd"></a>

## Smooth Scroll

Smoothens Scrolling.

```emacs-lisp
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
```


<a id="orgaab0bd6"></a>

## Highlight Indent Guides

[Highlight Indent Guides](https://github.com/DarthFennec/highlight-indent-guides), a feature that highlights indentation levels.

```emacs-lisp
(when *gui-emacs*
  (def-package highlight-indent-guides
    :diminish
    :hook ((prog-mode web-mode nxml-mode) . highlight-indent-guides-mode)
    :config
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-responsive 'top)
    (setq highlight-indent-guides-delay 0)
    (setq highlight-indent-guides-auto-character-face-perc 7)))
```


<a id="orgb9e554a"></a>

## Pretty Symbols

Pretty the Symbols.

```emacs-lisp
(global-prettify-symbols-mode 1)
(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols. See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("->" . 8594)
          ("<=" . 8804)
          (">=" . 8805)
          )))
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)
```


<a id="org7b652e7"></a>

## UI Configs


### Maximize Frame

```emacs-lisp
(add-to-list 'default-frame-alist '(fullscreen . maximized))
```


### Disable Splash Screen

```emacs-lisp
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Present Day, Present Time...")
```


### Simplify Yes/No Prompts

```emacs-lisp
(fset 'yes-or-no-p 'y-or-n-p)
```


<a id="org8b113ee"></a>

# General Programming


<a id="orgd7f7250"></a>

## Magit

[Magit](https://magit.vc/), an interface to the version control system Git.

```emacs-lisp
(def-package magit
  :bind ("C-x g" . magit-status))
```


<a id="org818509f"></a>

## Projectile

[Projectile](https://github.com/bbatsov/projectile), a Project Interaction Library for Emacs.

**Prerequisite**: If using Windows OS, [install](https://github.com/bmatzelle/gow/releases) Gow and add to `PATH`.

[Gow](https://github.com/bmatzelle/gow) is a lightweight installer that installs useful open source UNIX applications compiled as native win32 binaries. Specifically, `tr` is needed for Projectile alien indexing.

```emacs-lisp
(def-package projectile
  :bind
  ("C-c p" . projectile-command-map)
  ("C-z i" . projectile-switch-project)
  ("C-z o" . projectile-find-file)
  ("C-z p" . projectile-add-known-project)
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  (when (and (eq system-type 'windows-nt) *tr*)
    (setq projectile-indexing-method 'alien))
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))
```


<a id="orgb11f313"></a>

## Treemacs

[Treemacs](https://github.com/Alexander-Miller/treemacs), a tree layout file explorer for Emacs.


### Treemacs

```emacs-lisp
(def-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs
          (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay     5000
          treemacs-file-follow-delay    0.2
          treemacs-follow-after-init    t
          treemacs-follow-recenter-distance   0.1
          treemacs-git-command-pipe     ""
          treemacs-goto-tag-strategy    'refetch-index
          treemacs-indentation    2
          treemacs-indentation-string   " "
          treemacs-is-never-other-window      nil
          treemacs-max-git-entries      5000
          treemacs-no-png-images        nil
          treemacs-no-delete-other-windows    t
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file   (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-cursor    nil
          treemacs-show-hidden-files    t
          treemacs-silent-filewatch     nil
          treemacs-silent-refresh       nil
          treemacs-sorting        'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup   t
          treemacs-tag-follow-delay     1.5
          treemacs-width    35)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t) (treemacs-git-mode 'deferred))
      (`(t . _) (treemacs-git-mode 'simple))))
  :bind
  ((:map global-map
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t B"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)))
  (:map treemacs-mode-map ("C-p" . treemacs-previous-line)))
```


### Treemacs Magit

```emacs-lisp
(def-package treemacs-magit
  :defer t
  :after (treemacs magit))
```


### Treemacs Projectile

```emacs-lisp
(def-package treemacs-projectile
  :defer t
  :after (treemacs projectile))
```


<a id="org3722140"></a>

## Company

[Company](http://company-mode.github.io/), a text completion framework for Emacs.

```emacs-lisp
(def-package company
  :diminish company-mode
  :defer t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations 't)
  (setq company-idle-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-require-match 'never)
  (setq company-show-numbers t)
  (setq company-global-modes '(not shell-mode))
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))
```


<a id="org2898f65"></a>

## YASnippet


### YASnippet

[YASnippet](https://github.com/joaotavora/yasnippet), a programming template system for Emacs.

```emacs-lisp
(def-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :bind
  (("C-c C-n" . yas-expand-from-trigger-key))
  :config
  (yas-reload-all))
```


### YASnippet Snippets

[YASnippet Snippets](https://github.com/AndreaCrotti/yasnippet-snippets), a collection of yasnippet snippets for many languages.

```emacs-lisp

```


<a id="org824e8cc"></a>

## Flycheck

[Flycheck](https://www.flycheck.org/en/latest/), a syntax checking extension.

```emacs-lisp
(def-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
```


<a id="org56e00e0"></a>

## Dumb Jump

[Dumb jump](https://github.com/jacktasia/dumb-jump), an Emacs "jump to definition" package.

```emacs-lisp
(def-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))
```


<a id="orgee6a24f"></a>

## Line Numbers

Display line numbers, and column numbers in modeline.

```emacs-lisp
;; Hook line numbers to only when files are opened
(if (version< emacs-version "26")
    (progn (add-hook 'find-file-hook #'linum-mode)
           (add-hook 'prog-mode-hook #'linum-mode))
  (progn (add-hook 'find-file-hook #'display-line-numbers-mode)
         (add-hook 'prog-mode-hook #'display-line-numbers-mode)))

;; Display column numbers in modeline
(column-number-mode 1)
```


<a id="org57ede11"></a>

## Parenthesis Pairing

Match and automatically pair parenthesis.

```emacs-lisp
;; Show matching parenthesis
(show-paren-mode 1)
```


### Smartparens

[Smartparens](https://github.com/Fuco1/smartparens), a minor mode for dealing with pairs.

```emacs-lisp
(def-package smartparens
  :demand t
  :diminish smartparens-mode
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ;; C-S-d is bound to dup line
              ("C-S-b" . sp-beginning-of-sexp)
              ("C-S-a" . sp-end-of-sexp)
              ("C-M-e" . sp-up-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-n" . sp-forward-hybrid-sexp)
              ("C-M-p" . sp-backward-hybrid-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-<delete>" . sp-unwrap-sexp)
              ;; I like using M-<backspace> to del backwards
              ;; ("C-<backspace>" . sp-backward-unwrap-sexp)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-<left>" . sp-forward-barf-sexp)
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ("M-D" . sp-splice-sexp)
              ;; This is Ctrl-Alt-Del lol
              ;; ("C-M-<delete>" . sp-splice-sexp-killing-forward)
              ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
              ("C-S-<backspace>" . sp-splice-sexp-killing-around)
              ("C-]" . sp-select-next-thing-exchange)
              ("C-<left_bracket>" . sp-select-previous-thing)
              ("C-M-]" . sp-select-next-thing)
              ("M-F" . sp-forward-symbol)
              ("M-B" . sp-backward-symbol)
              ("C-\"" . sp-change-inner)
              ("M-i" . sp-change-enclosing))
  :config
  (smartparens-global-mode)
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil)
  (setq sp-escape-quotes-after-insert nil))
```


### Awesome Pair

[Awesome Pair](https://github.com/manateelazycat/awesome-pair), a feature that provides grammatical parenthesis completion. All I need is this smart kill.

```emacs-lisp
(def-package awesome-pair
  :ensure nil
  :config
  (add-hook 'prog-mode-hook '(lambda () (awesome-pair-mode 1)))
  :bind ("C-c C-k" . awesome-pair-kill))
```


<a id="org664d67d"></a>

## Format All

[Format all](https://github.com/lassik/emacs-format-all-the-code), a feature that lets you auto-format source code.

**Prerequisite**: Read [Supported Languages](https://github.com/lassik/emacs-format-all-the-code#supported-languages) to see which additional tool you need to install for the specific language.

```emacs-lisp
(def-package format-all
  :bind ("C-z f" . format-all-buffer))
```


<a id="org3ee1a21"></a>

## Evil-Nerd-Commenter

[Evil Nerd Commenter](https://github.com/redguardtoo/evil-nerd-commenter), a tool that helps you comment code efficiently.

```emacs-lisp
(def-package evil-nerd-commenter
  :bind
  (("M-;" . evilnc-comment-or-uncomment-lines)
   ("C-z ; p" . evilnc-comment-or-uncomment-paragraphs)
   ("C-z ; c" . evilnc-copy-and-comment-lines)))
```


<a id="org3706d3f"></a>

## Iedit

[Iedit](https://github.com/victorhge/iedit), a minor mode that allows editing multiple regions simultaneousy in a buffer or a region.

```emacs-lisp
(def-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)
```


<a id="org21e13e8"></a>

## Header

[Header2](https://www.emacswiki.org/emacs/header2.el), a support for creation and update of file headers.

```emacs-lisp
(def-package header2
  :ensure nil
  :config
  (autoload 'auto-make-header "header2")
  (autoload 'auto-update-file-header "header2")
  (add-hook 'write-file-hooks 'auto-update-file-header)
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
  (add-hook 'c-mode-common-hook   'auto-make-header))
```


<a id="org244cb85"></a>

## LSP

[LSP](https://github.com/emacs-lsp/lsp-mode), a client/library for the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).


### LSP Mode

```emacs-lisp
(def-package lsp-mode
  :defer t
  :commands lsp
  :init
  (setq lsp-auto-guess-root nil)
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq lsp-message-project-root-warning t)
  :hook (prog-mode . lsp))
```


### LSP UI

[LSP UI](https://github.com/emacs-lsp/lsp-ui), provides all the higher level UI modules of lsp-mode, like flycheck support and code lenses.

```emacs-lisp
(def-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'top
        lsp-ui-doc-use-webkit t
        lsp-ui-doc-border (face-foreground 'default)

        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-code-actions nil)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))
```


### Company LSP

[Company SLP](https://github.com/tigersoldier/company-lsp), a Company completion backend for lsp-mode.

```emacs-lisp
(def-package company-lsp
  :defer t
  :config
  (setq company-lsp-cache-candidates 'auto))
```


### DAP

[DAP](https://github.com/emacs-lsp/dap-mode), a client/library for the [Debug Adapter Protocol](https://code.visualstudio.com/api/extension-guides/debugger-extension).

```emacs-lisp
(def-package dap-mode
  :after lsp-mode
  :defer t
  :config
  (dap-mode t)
  (dap-ui-mode t))
```


<a id="orgf54d53f"></a>

# Programming


<a id="org86cc284"></a>

## Emacs Lisp


### Shortcut for Evaluating Elisp

Eval-buffer for ELisp Code.

```emacs-lisp
(define-key emacs-lisp-mode-map (kbd "<f5>") #'eval-buffer)
```


<a id="org9ffa36b"></a>

## Java


### LSP Java

[LSP Java](https://github.com/emacs-lsp/lsp-java), Emacs Java IDE using [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls).

**Prerequisite**: [Install](https://maven.apache.org/download.cgi) Maven and add to `PATH`.

```emacs-lisp
(when *mvn*
  (def-package lsp-java
    :after lsp-mode
    :config
    (setq
     lsp-java-server-install-dir (expand-file-name "~/eclipse.jdt.ls/server/")
     lsp-java-workspace-dir (expand-file-name "~/eclipse.jdt.ls/workspace/"))))
```


<a id="org1a66611"></a>

## C/C++

**Prerequisite**:

-   Windows OS: [Install](http://www.mingw.org/wiki/Install_MinGW) MinGW for Compilation and [CMake](https://cmake.org/download/) >= 2.8.3 first.
-   ALl OS: [Install](https://clang.llvm.org/extra/clangd/Installation.html) Clangd and add to `PATH`.

Note: If Displaying `No LSP server for c-mode`, execute `M-x ielm` and verify clangd is installed using `(executable-find "clangd")` or `(executable-find lsp-clients-clangd-executable)`.

Compile using `<f5>` or `compile`. The command `gcc -o <file>.exe <fileA>.c <fileB>.c ...` is to compile C code into `<file>.exe`.


### CC Mode

CC Mode, a mode for editing files containing C, C++, Objective-C, Java, CORBA IDL (and the variants CORBA PSDL and CIDL), Pike and AWK code.

```emacs-lisp
(def-package cc-mode
  :ensure nil
  :defer t
  :bind ("<f5>" . compile)
  :config
  (unless *clangd* (message "Please install clangd and put it in PATH"))
  (unless *gcc* (message "Please install GCC and put it in PATH")))
```

Rest of the features will be provided by [LSP Mode](https://github.com/emacs-lsp/lsp-mode).


<a id="org2e04eef"></a>

## Python


### Microsoft's Python Language Server

[LSP Python MS](https://github.com/andrew-christianson/lsp-python-ms), a lsp-mode client leveraging [Microsoft's Python Language Server](https://github.com/Microsoft/python-language-server).

**Prerequisite**: [Install](https://dotnet.microsoft.com/download) .NET Core SDK, then execute the following commands from your `HOME` or `~` path:

```text
git clone https://github.com/Microsoft/python-language-server.git
cd python-language-server/src/LanguageServer/Impl
dotnet build -c Release
dotnet publish -c Release -r win10-x64
```

Change the value after `-r` flag (`win10-x64`) depending on your architecture and OS. See Microsoft's [Runtime ID Catalog](https://docs.microsoft.com/en-us/dotnet/core/rid-catalog) for the correct value for your OS.

Now, put `~/.emacs.d\python-language-server\output\bin\Release\win10-x64\publish` in your `PATH`.

```emacs-lisp
(def-package lsp-python-ms
  :after lsp-mode
  :hook (python-mode . lsp)
  :config
  ;; for dev build of language server
  (setq lsp-python-ms-dir
        (expand-file-name "~/.emacs.d/python-language-server/output/bin/Release/"))
  ;; for executable of language server, if it's not symlinked on your PATH
  (setq lsp-python-ms-executable
        "~/.emacs.d/python-language-server/output/bin/Release/win10-x64/publish/Microsoft.Python.LanguageServer"))
```


<a id="org68aa797"></a>

## Arduino


### Arduino Mode

[Arduino mode](https://github.com/bookest/arduino-mode), a major mode for editing Arduino sketches.

```emacs-lisp
(def-package arduino-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
  (add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
  (autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t))
```


### Company Arduino

[Company Arduino](https://github.com/yuutayamada/company-arduino), a set of configuration to let you auto-completion by using irony-mode, company-irony and company-c-headers on arduino-mode.

```emacs-lisp
(def-package company-arduino
  :defer t
  :config
  (add-hook 'irony-mode-hook 'company-arduino-turn-on)
  ;; Activate irony-mode on arduino-mode
  (add-hook 'arduino-mode-hook 'irony-mode))
```


<a id="orged8c3b0"></a>

## Jupyter Notebook

[Emacs IPython Notebook](https://github.com/millejoh/emacs-ipython-notebook), a [Jupyter](https://jupyter.org/) (formerly IPython) client in Emacs.


### Usage

1.  Execute `M-x ein:run` to launch a local Jupyter session.
2.  Login with `M-x ein:login` to a local or remote session.
3.  Open `.ipynb` file and press `C-c C-o`.

```emacs-lisp
(def-package ein
  :defer t)
```


<a id="orgd412f69"></a>

# Web Development

**Prerequisite**: [Install](https://nodejs.org/en/download/) NodeJS and add to path. Execute following commands to enable LSP for JavaScript/TypeScript/HTML:

```text
npm i -g typescript-language-server
npm i -g typescript
npm install -g vscode-html-languageserver-bin
```


<a id="orge8c5d03"></a>

## Web Mode

[Web mode](https://github.com/fxbois/web-mode), a major mode for editing web templates.

```emacs-lisp
(def-package web-mode
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'" "\\.tsx\\'"))
```


<a id="org4da7bc0"></a>

## Emmet

[Emmet](https://github.com/smihica/emmet-mode), a feature that allows writing HTML using CSS selectors along with `C-j`. See [usage](https://github.com/smihica/emmet-mode#usage) for more information.

```emacs-lisp
(def-package emmet-mode
  :hook web-mode
  :config
  (add-hook 'css-mode-hooktype  'emmet-mode)) ;; enable Emmet's css abbreviation
```


<a id="org0958767"></a>

## JavaScript/TypeScript


### TypeScript Mode

[TypeScript mode](https://github.com/emacs-typescript/typescript.el), a feature that offers TypeScript support for Emacs.

```emacs-lisp
(def-package typescript-mode
  :defer t
  :commands (typescript-mode)
  :bind (:map typescript-mode-map
              ("M-." . tide-jump-to-definition))
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (defun setup-tide-ts ()
    "Setup tide for typescript."
    (interactive)
    (tide-setup)
    (tide-hl-identifier-mode))
  (add-hook 'typescript-mode-hook #'setup-tide-ts))
```


### Tide

[Tide](https://github.com/ananthakumaran/tide), a TypeScript Interactive Development Environment for Emacs. This is an alternative when LSP is not working.

Tip: enter `M-.` to jump to definition.

```emacs-lisp
(def-package tide
  :defer t
  :bind (:map tide-mode-map
              ("M-." . nil))
  :commands (tide-setup)
  :after (company flycheck))
```


<a id="orgcf243cb"></a>

# Miscellaneous


<a id="org8b6c783"></a>

## Org

[Org](https://orgmode.org/), a Emacs built-in tool for keeping notes, maintaining TODO lists, planning projects, and authoring documents with a fast and effective plain-text system.

**Prerequisite**: Modify `(setq org-agenda-files (list "~/org/agenda/"))` to your agenda folder.

```emacs-lisp
(def-package org
  :ensure nil
  :defer t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switch)
  :config
  (setq org-log-done 'time)
  (setq org-export-backends (quote (ascii html icalendar latex md odt)))
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))
  (setq org-use-speed-commands t)
  (setq org-todo-keywords
        '((sequence "TODO" "PROCESS" "REVIEW" "|" "DONE"))))
```


### TOC Org

[TOC Org](https://github.com/snosov1/toc-org) generates table of contents for `.org` files

```emacs-lisp
(def-package toc-org
  :hook (org-mode . toc-org-mode))
```


### HTMLize

[HTMLize](https://github.com/hniksic/emacs-htmlize), a tool that converts buffer text and decorations to HTML.

```emacs-lisp
(def-package htmlize :defer t)
```


### OX-GFM

[OX-GFM](https://github.com/larstvei/ox-gfm), a Github Flavored Markdown exporter for Org Mode.

```emacs-lisp
(def-package ox-gfm :defer t)
```


<a id="org2e43831"></a>

## Emacs Web Wowser

EWW, the Emacs Web Wowser.


### Set EWW as Default Browser

In Eww, hit & to browse this url system browser

```emacs-lisp
(setq browse-url-browser-function 'eww-browse-url)
```


### Auto-Rename New EWW Buffers

```emacs-lisp
(defun xah-rename-eww-hook ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook #'xah-rename-eww-hook)

;; C-u M-x eww will force a new eww buffer
(defun force-new-eww-buffer (orig-fun &rest args)
  "ORIG-FUN ARGS When prefix argument is used, a new eww buffer will be created,
  regardless of whether the current buffer is in `eww-mode'."
  (if current-prefix-arg
      (with-temp-buffer
        (apply orig-fun args))
    (apply orig-fun args)))
(advice-add 'eww :around #'force-new-eww-buffer)
```


<a id="org05b8d45"></a>

## LeetCode

[LeetCode](https://github.com/kaiwk/leetcode.el), an Emacs LeetCode client. Note that this package is dependant on [Request](https://github.com/tkf/emacs-request), [Request-Deferred](https://github.com/tkf/emacs-request/blob/master/request-deferred.el) and [GraphQL](https://github.com/davazp/graphql-mode).

**Prerequisite**: Ensure `cURL` is installed added to `PATH`. If you are using Windows, it is suggested to use `curl` provided by Git, therefore add `<path to Git>\mingw64\bin` to `PATH`.

```emacs-lisp
(when *curl*
  (def-package leetcode
    :ensure nil
    :init
    (def-package request)
    (def-package request-deferred)
    (def-package graphql)
    :config
    ;; (setq request-message-level 'debug)
    ;; (setq request-log-level 'debug)
    (setq leetcode-prefer-language "python3")))
```


<a id="orgd186321"></a>

## Tetris

Although [Tetris](https://www.emacswiki.org/emacs/TetrisMode) is part of Emacs, but there still could be some configurations.

```emacs-lisp
(defvar tetris-mode-map
  (make-sparse-keymap 'tetris-mode-map))
(define-key tetris-mode-map (kbd "C-p") 'tetris-rotate-prev)
(define-key tetris-mode-map (kbd "C-n") 'tetris-move-down)
(define-key tetris-mode-map (kbd "C-b") 'tetris-move-left)
(define-key tetris-mode-map (kbd "C-f") 'tetris-move-right)
(define-key tetris-mode-map (kbd "C-SPC") 'tetris-move-bottom)
(defadvice tetris-end-game (around zap-scores activate)
  (save-window-excursion ad-do-it))
```


<a id="orgb005d28"></a>

## Speed Type

[Speed type](https://github.com/hagleitn/speed-type), a game to practice touch/speed typing in Emacs.

```emacs-lisp
(def-package speed-type
  :defer t)
```


<a id="orge86311b"></a>

## 2048 Game

[2048 Game](https://bitbucket.org/zck/2048.el), an implementation of 2048 in Emacs.

```emacs-lisp
(def-package 2048-game
  :defer t)
```
