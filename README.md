<a id="orgc66da90"></a>

# M-EMACS


<a id="orgc16277e"></a>

# About

M-EMACS is a custom GNU Emacs setup and configurations that aims to enhance the default Emacs experience.

This configuration is designed and tested for **GNU Emacs 25.1 and above only**.

```emacs-lisp
(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))
```


### Cool Thing About This README

This README originated from init.org that is generated using `M-x org-gfm-export-to-markdown`. Every block of code is generated through this function - it exports the section of code from `elisp/` directory. You will not see their presence in init.org.


# Table of Contents     :TOC_2_ORG:

-   [M-EMACS](#orgc66da90)
-   [About](#orgc16277e)
-   [Installation](#org1f07f92)
-   [Modification](#orge0bfa34)
-   [Startup](#org30516d6)
    -   [Lexical-binding](#org5b6fa78)
    -   [Disable Unnecessary Interface](#orgdaa243a)
    -   [Optimization](#orge906f0e)
    -   [Define Constants](#org847df62)
-   [Package Management](#orge04be26)
    -   [Melpa Packages](#orga7b4849)
    -   [Configure Package Management](#org817810f)
    -   [Use-Package Wrapper](#orge4de67f)
    -   [Auto Package Update](#orgded3664)
-   [Global Functionalities](#orgaac07ec)
    -   [Owner Information](#orgeacc7c6)
    -   [Bindings](#org010aa44)
    -   [Dired](#org22b8af6)
    -   [Search](#org807e67e)
    -   [Avy](#org5f5bbea)
    -   [Ivy](#org62d9d02)
    -   [Winner](#org01f763d)
    -   [Which Key](#org4232e12)
    -   [Popup Kill Ring](#org1fd4d07)
    -   [Undo Tree](#orgca9ca97)
    -   [Shell Here](#org5c02ccd)
    -   [Discover My Major](#org63d9cd0)
    -   [Configs](#orgb6a21db)
    -   [More Functions](#orgc7813d0)
-   [User Interface Enhancements](#org176885c)
    -   [Title Bar](#org5e332b0)
    -   [All The Icons](#org3480b53)
    -   [Doom Theme](#org5596b0d)
    -   [Doom Modeline](#org7e9505a)
    -   [Dashboard](#org462653b)
    -   [Fonts](#org4e22814)
    -   [Zone](#orgac33670)
    -   [Diminish](#org40e92c0)
    -   [Dimmer](#orgf183832)
    -   [Smooth Scroll](#orgd2d4e52)
    -   [Highlight Indent Guides](#orgeeb3b46)
    -   [Pretty Symbols](#org8154296)
    -   [UI Configs](#org01849da)
-   [General Programming](#orgd7bfa62)
    -   [Magit](#org0bcc7bf)
    -   [Projectile](#org6bf982c)
    -   [Treemacs](#org73c19fd)
    -   [Company](#org2146ade)
    -   [Flycheck](#orgc576c30)
    -   [Dumb Jump](#org0e2fe79)
    -   [Line Numbers](#org19f5a26)
    -   [Parenthesis Pairing](#org62e92a5)
    -   [Format All](#org3a351a9)
    -   [Evil-Nerd-Commenter](#orgf27ea4c)
    -   [Iedit](#orgf6eb957)
    -   [Header](#org37736b6)
    -   [LSP](#org7049e62)
-   [Programming](#orgc915557)
    -   [Emacs Lisp](#org72ad986)
    -   [Java](#orgc91694c)
    -   [C/C++](#org58cbfc2)
    -   [Python](#org62e1fa0)
    -   [Arduino](#org6abc606)
    -   [Jupyter Notebook](#orga6d1369)
-   [Web Development](#orgf6e5b3b)
    -   [Web Mode](#org31bdef9)
    -   [Emmet](#org1233dd8)
    -   [JavaScript/TypeScript](#org2a52d1b)
-   [Miscellaneous](#orgaf8ebd2)
    -   [Org](#orgd6016e3)
    -   [EWW](#org0c8bbdc)
    -   [Tetris](#orgd5026ce)
    -   [Speed Type](#org19c0533)
    -   [2048 Game](#orgbb031b7)
-   [Special Thanks](#org3897a89)


<a id="org1f07f92"></a>

# Installation

1.  Install latest version of [GNU Emacs](https://www.gnu.org/software/emacs/).
2.  Clone this repo to `HOME` or `~/` path using [git](https://git-scm.com/) and update all the submodules.

    ```text
    git clone https://github.com/MatthewZMD/.emacs.d.git
    cd .emacs.d
    git submodule init
    git submodule update
    ```
3.  Ensure a stable connection to Melpa Packages, then open Emacs.
4.  Enter `y` when prompted with `Auto-update packages now?`, wait for all packages to install.
5.  In your favourite browser, `Ctrl-F Prerequisite` through this webpage and follow the **Prerequisite** instructions.
6.  Restart Emacs.


<a id="orge0bfa34"></a>

# Modification

You have the permission to use, modify, distribute however you want and free of charge, but without any warranty. (See [LICENSE](LICENSE))

**Remember** you must manually sync this README with all the new changes you made by:

1.  Please do **NOT** edit README.md file, edit init.org instead!
2.  If you add a new mode, create a new `<file-name>.el` file in `elisp/` directory.
3.  Put `(require '<file-name>)` in [init.el](init.el) accordingly.
4.  Add `#+INCLUDE: "~/.emacs.d/elisp/<place-holder>.el" src emacs-lisp :range-begin "<start-line-wrapper-exclusive>" :range-end "<end-line-wrapper-exclusive>"` in the appropriate section in init.org.
5.  Following any modification in `#+INCLUDE:` statement as well as any of the `init-.el` files, please press `C-x C-s` to update the `:lines`.
6.  Call `M-x org-gfm-export-to-markdown` to update this README.


<a id="org30516d6"></a>

# Startup


<a id="org5b6fa78"></a>

## Lexical-binding

Use lexical-binding. [Why?](https://nullprogram.com/blog/2016/12/22/)

> Until Emacs 24.1 (June 2012), Elisp only had dynamically scoped variables, a feature, mostly by accident, common to old lisp dialects. While dynamic scope has some selective uses, it’s widely regarded as a mistake for local variables, and virtually no other languages have adopted it.

```emacs-lisp
;;; init.el --- -*- lexical-binding: t -*-
```


<a id="orgdaa243a"></a>

## Disable Unnecessary Interface

This need to be in the beginning of initialization to smooth the experience.

```emacs-lisp
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
```


<a id="orge906f0e"></a>

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

Load all elisp files in `elisp/` and `site-elisp/` to reduce startup time.

```emacs-lisp
(defun update-load-path (&rest _)
  "Update `load-path'."
  (interactive)
  (push (expand-file-name "site-elisp" user-emacs-directory) load-path)
  (push (expand-file-name "elisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-elisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
```


<a id="org847df62"></a>

## Define Constants

```emacs-lisp
(defconst *gui-emacs* (display-graphic-p))

(defconst *rg* (executable-find "rg"))

(defconst *tr* (executable-find "tr"))

(defconst *mvn* (executable-find "mvn"))

;(defconst *npm* (executable-find "npm"))

(defconst *clangd* (or (executable-find "clangd")  ;; usually
                       (executable-find "/usr/local/opt/llvm/bin/clangd")))  ;; macOS

(defconst *gcc* (executable-find "gcc"))
```


<a id="orge04be26"></a>

# Package Management


<a id="orga7b4849"></a>

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


<a id="org817810f"></a>

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


<a id="orge4de67f"></a>

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
          (nreverse (apply #'nconc
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


<a id="orgded3664"></a>

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


<a id="orgaac07ec"></a>

# Global Functionalities


<a id="orgeacc7c6"></a>

## Owner Information

**Prerequisite**: Change this to your information.

```emacs-lisp
(setq user-full-name "Mingde (Matthew) Zeng")
(setq user-mail-address "matthewzmd@gmail.com")
```


<a id="org010aa44"></a>

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


<a id="org22b8af6"></a>

## Dired


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


### Autosave and Backup

Create directory where Emacs stores backups and autosave files.

```emacs-lisp
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)
```

Set autosave and backup directory.

```emacs-lisp
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms  '((".*" "~/.emacs.d/autosaves/\\1" t))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t)
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


<a id="org807e67e"></a>

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


<a id="org5f5bbea"></a>

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


<a id="org62d9d02"></a>

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
  (setq ivy-magic-slash-non-match-action nil)
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


<a id="org01f763d"></a>

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


<a id="org4232e12"></a>

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


<a id="org1fd4d07"></a>

## Popup Kill Ring

[Popup kill ring](https://github.com/waymondo/popup-kill-ring), a feature that provides the ability to browse Emacs kill ring in autocomplete style popup menu.

```emacs-lisp
(def-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))
```


<a id="orgca9ca97"></a>

## Undo Tree

[Undo tree](https://www.emacswiki.org/emacs/UndoTree), a feature that provides a visualization of the undos in a file.

```emacs-lisp
(def-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))
```


<a id="org5c02ccd"></a>

## Shell Here

[Shell-here](https://github.com/ieure/shell-here), a tool that opens a shell buffer in (or relative to) `default-directory`.

```emacs-lisp
(def-package shell-here
  :bind ("C-!" . shell-here))
```


<a id="org63d9cd0"></a>

## Discover My Major

[Discover my major](https://github.com/jguenther/discover-my-major), a feature that discovers key bindings and their meaning for the current Emacs major mode.

```emacs-lisp
(def-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)))
```


<a id="orgb6a21db"></a>

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
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
```


### Automatic Garbage Collect

Garbage collect when Emacs is not in focus.

```emacs-lisp
(add-hook 'focus-out-hook #'garbage-collect)
```


### MiniBuffer

```emacs-lisp
;; disable recursive minibuffers
(setq-default enable-recursive-minibuffers nil)

;; keep the point out of the minibuffer
(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
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


<a id="orgc7813d0"></a>

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

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
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


<a id="org176885c"></a>

# User Interface Enhancements


<a id="org5e332b0"></a>

## Title Bar

```emacs-lisp
(setq-default frame-title-format '("M-EMACS - " user-login-name "@" system-name " - %b"))
```


<a id="org3480b53"></a>

## All The Icons

[All The Icons](https://github.com/domtronn/all-the-icons.el), a utility package to collect various Icon Fonts and propertize them within Emacs.

**Prerequisite**: Install all fonts from `/fonts/all-the-icons-fonts`.

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


<a id="org5596b0d"></a>

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


<a id="org7e9505a"></a>

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


<a id="org462653b"></a>

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


<a id="org4e22814"></a>

## Fonts

Prepares fonts to use.

**Prerequisite**: Install `Input` and `Love Letter TW` from `/fonts`.

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


<a id="orgac33670"></a>

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


<a id="org40e92c0"></a>

## Diminish

[Diminish](https://github.com/emacsmirror/diminish), a feature that removes certain minor modes from mode-line.

```emacs-lisp
(def-package diminish)
```


<a id="orgf183832"></a>

## Dimmer

[Dimmer](https://github.com/gonewest818/dimmer.el), a feature that visually highlights the selected buffer.

```emacs-lisp
(def-package dimmer
  :init (dimmer-mode)
  :config
  (setq dimmer-fraction 0.2)
  (setq dimmer-exclusion-regexp "\\*Minibuf-[0-9]+\\*\\|\\*dashboard\\*"))
```


<a id="orgd2d4e52"></a>

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


<a id="orgeeb3b46"></a>

## Highlight Indent Guides

[Highlight Indent Guides](https://github.com/DarthFennec/highlight-indent-guides), a feature that highlights indentation levels.

```emacs-lisp
(when *gui-emacs*
  (def-package highlight-indent-guides
    :diminish
    :defer t
    :hook ((prog-mode web-mode nxml-mode) . highlight-indent-guides-mode)
    :config
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-responsive 'top)
    (setq highlight-indent-guides-delay 0)
    (setq highlight-indent-guides-auto-character-face-perc 7)))
```


<a id="org8154296"></a>

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
          ("!=" . 8800)
          ("map" . 8614)
          )))
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)
```


<a id="org01849da"></a>

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


<a id="orgd7bfa62"></a>

# General Programming


<a id="org0bcc7bf"></a>

## Magit

[Magit](https://magit.vc/), an interface to the version control system Git.

```emacs-lisp
(def-package magit
  :bind ("C-x g" . magit-status))
```


<a id="org6bf982c"></a>

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


<a id="org73c19fd"></a>

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


<a id="org2146ade"></a>

## Company

[Company](http://company-mode.github.io/), short for \*Comp\*lete \*any\*thing, a text completion framework for Emacs.

```emacs-lisp
(def-package company
  :diminish company-mode
  :defer t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations 't) ; align annotations to the right tooltip border
  (setq company-idle-delay 0) ; decrease delay before autocompletion popup shows
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (setq company-require-match 'never)
  (setq company-global-modes '(not shell-mode)))
```


<a id="orgc576c30"></a>

## Flycheck

[Flycheck](https://www.flycheck.org/en/latest/), a syntax checking extension.

```emacs-lisp
(def-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-add-mode 'typescript-tslint 'js2-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
```


<a id="org0e2fe79"></a>

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


<a id="org19f5a26"></a>

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


<a id="org62e92a5"></a>

## Parenthesis Pairing

Match and automatically pair parenthesis.

```emacs-lisp
;; Show matching parenthesis
(setq show-paren-delay 0)
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
(use-package awesome-pair
  :ensure nil
  :config
  (add-hook 'prog-mode-hook '(lambda () (awesome-pair-mode 1)))
  :bind ("C-c C-k" . awesome-pair-kill))
```


<a id="org3a351a9"></a>

## Format All

[Format all](https://github.com/lassik/emacs-format-all-the-code), a feature that lets you auto-format source code.

**Prerequisite**: Read [Supported Languages](https://github.com/lassik/emacs-format-all-the-code#supported-languages) to see which additional tool you need to install for the specific language.

```emacs-lisp
(def-package format-all
  :bind ("C-z f" . format-all-buffer)
  :config (add-hook 'prog-mode-hook 'format-all-mode))
```


<a id="orgf27ea4c"></a>

## Evil-Nerd-Commenter

[Evil Nerd Commenter](https://github.com/redguardtoo/evil-nerd-commenter), a tool that helps you comment code efficiently.

```emacs-lisp
(def-package evil-nerd-commenter
  :bind
  (("M-;" . evilnc-comment-or-uncomment-lines)
   ("C-z ; p" . evilnc-comment-or-uncomment-paragraphs)
   ("C-z ; c" . evilnc-copy-and-comment-lines)))
```


<a id="orgf6eb957"></a>

## Iedit

[Iedit](https://github.com/victorhge/iedit), a minor mode that allows editing multiple regions simultaneousy in a buffer or a region.

```emacs-lisp
(def-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)
```


<a id="org37736b6"></a>

## Header

[Header2](https://www.emacswiki.org/emacs/header2.el), a support for creation and update of file headers.

```emacs-lisp
(use-package header2
  :ensure nil
  :config
  (autoload 'auto-make-header "header2")
  (autoload 'auto-update-file-header "header2")
  (add-hook 'write-file-hooks 'auto-update-file-header)
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
  (add-hook 'c-mode-common-hook   'auto-make-header))
```


<a id="org7049e62"></a>

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

        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t)
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
  (setq company-lsp-cache-candidates 'auto)
  (push 'company-lsp company-backends))
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


<a id="orgc915557"></a>

# Programming


<a id="org72ad986"></a>

## Emacs Lisp


### Shortcut for Evaluating Elisp

Eval-buffer for ELisp Code.

```emacs-lisp
(define-key emacs-lisp-mode-map (kbd "<f5>") #'eval-buffer)
```


<a id="orgc91694c"></a>

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


<a id="org58cbfc2"></a>

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


<a id="org62e1fa0"></a>

## Python


### TODO Microsoft's Python Language Server - [Use Melpa Once it's Ready](https://github.com/melpa/melpa/pull/6027)

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
  :ensure nil
  :hook (python-mode . lsp)
  :config
  ;; for dev build of language server
  (setq lsp-python-ms-dir
        (expand-file-name "~/.emacs.d/python-language-server/output/bin/Release/"))
  ;; for executable of language server, if it's not symlinked on your PATH
  (setq lsp-python-ms-executable
        "~/.emacs.d/python-language-server/output/bin/Release/win10-x64/publish/Microsoft.Python.LanguageServer"))
```


<a id="org6abc606"></a>

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


<a id="orga6d1369"></a>

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


<a id="orgf6e5b3b"></a>

# Web Development

**Prerequisite**: [Install](https://nodejs.org/en/download/) NodeJS and add to path. Execute following commands to enable LSP for JavaScript/TypeScript/HTML:

```text
npm i -g typescript-language-server
npm i -g typescript
npm install -g vscode-html-languageserver-bin
```


<a id="org31bdef9"></a>

## Web Mode

[Web mode](https://github.com/fxbois/web-mode), a major mode for editing web templates.

```emacs-lisp
(def-package web-mode
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'" "\\.tsx\\'"))
```


<a id="org1233dd8"></a>

## Emmet

[Emmet](https://github.com/smihica/emmet-mode), a feature that allows writing HTML using CSS selectors along with `C-j`. See [usage](https://github.com/smihica/emmet-mode#usage) for more information.

```emacs-lisp
(def-package emmet-mode
  :hook web-mode
  :config
  (add-hook 'css-mode-hooktype  'emmet-mode)) ;; enable Emmet's css abbreviation
```


<a id="org2a52d1b"></a>

## JavaScript/TypeScript


### JavaScript2 Mode

[JS2 mode](https://github.com/mooz/js2-mode), a feature that offers improved JavsScript editing mode.

```emacs-lisp
(def-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")
```


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

[Tide](https://github.com/ananthakumaran/tide), a \*T\*ypeScript \*I\*nteractive \*D\*evelopment \*E\*nvironment for \*E\*macs. This is an alternative when LSP is not working.

Tip: enter `M-.` to jump to definition.

```emacs-lisp
(def-package tide
  :defer t
  :bind (:map tide-mode-map
              ("M-." . nil))
  :commands (tide-setup)
  :after (company flycheck))
```


<a id="orgaf8ebd2"></a>

# Miscellaneous


<a id="orgd6016e3"></a>

## Org

[Org](https://orgmode.org/) is for keeping notes, maintaining TODO lists, planning projects, and authoring documents with a fast and effective plain-text system.

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
(use-package htmlize :defer t)
```


### OX-GFM

[OX-GFM](https://github.com/larstvei/ox-gfm), a Github Flavored Markdown exporter for Org Mode.

```emacs-lisp
(def-package ox-gfm :defer t)
```


<a id="org0c8bbdc"></a>

## EWW

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


<a id="orgd5026ce"></a>

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


<a id="org19c0533"></a>

## Speed Type

[Speed type](https://github.com/hagleitn/speed-type), a game to practice touch/speed typing in Emacs.

```emacs-lisp
(def-package speed-type
  :defer t)
```


<a id="orgbb031b7"></a>

## 2048 Game

[2048 Game](https://bitbucket.org/zck/2048.el), an implementation of 2048 in Emacs.

```emacs-lisp
(def-package 2048-game
  :defer t)
```


<a id="org3897a89"></a>

# Special Thanks

This Emacs configuration was influenced and inspired by the following configurations.

-   [Vincent Zhang's Centaur Emacs](https://github.com/seagle0128/.emacs.d)
-   [Henrik Lissner's Doom Emacs](https://github.com/hlissner/doom-emacs)
-   [Poncie Reyes's .emacs.d](https://github.com/poncie/.emacs.d)
