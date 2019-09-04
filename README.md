<a id="orgfb63c34"></a>

# M-EMACS


# Table of Contents     :TOC_2_ORG:

-   [M-EMACS](#orgfb63c34)
-   [About EMACS](#orgc37b08e)
-   [About M-EMACS](#orgb967293)
    -   [Screenshot](#org833dba3)
    -   [About README](#org3ada03a)
    -   [Installation](#orgf03b0a1)
    -   [Modification](#org415df22)
    -   [Contribution](#org2eb5eb4)
    -   [Special Thanks](#org7775561)
-   [Startup](#org0db1145)
    -   [Lexical Binding](#orge2c505b)
    -   [Early Init](#org57a8350)
    -   [Garbage Collection](#orgc1bc45a)
    -   [Load Path](#orgbc2079a)
    -   [Define Constants](#org207baec)
-   [Package Management](#org36f3c39)
    -   [Melpa Packages](#org3d5096b)
    -   [Non-Melpa Packages](#orgffc726e)
    -   [Configure Package Manager](#orgc7b2b67)
    -   [Use-Package](#orgd6b0be3)
    -   [Auto Package Update](#org489eac2)
    -   [Diminish](#orgaba2c1a)
    -   [GNU Elpa Keyring Update](#orgb1d5cce)
-   [Global Functionalities](#org4f4774a)
    -   [User Information](#orgb7d73fb)
    -   [Bindings](#org025f4e5)
    -   [File Management](#org08428b9)
    -   [Ivy](#org70a8082)
    -   [Color RipGrep](#orgf5e6dc8)
    -   [Snails](#orga41a56d)
    -   [Avy](#orgc1ebde6)
    -   [Shell](#org0f93a9f)
    -   [Winner](#orgc297dad)
    -   [Which Key](#org6128e4a)
    -   [Popup Kill Ring](#org1b2afb2)
    -   [Undo Tree](#orgadb7567)
    -   [Discover My Major](#orgf58e5e1)
    -   [Ace Window](#orgcb4fa1c)
    -   [Configs](#org24410e1)
    -   [Functions](#org99af91c)
-   [User Interface Enhancements](#org640ecae)
    -   [Doom Themes](#org18aa49a)
    -   [Doom Modeline](#orgd0c47e4)
    -   [Dashboard](#orgfc0282b)
    -   [Fonts and Icons](#orgfd2e664)
    -   [Smooth Scrolling](#orgf9c4099)
    -   [Emojify](#orgafee26a)
    -   [Prettify Symbols](#org6ba6599)
    -   [UI Configs](#org04e428d)
-   [General Programming](#org381b086)
    -   [Magit](#orgf68a110)
    -   [Projectile](#org61434e9)
    -   [Treemacs](#orgad76276)
    -   [YASnippet](#org16fe588)
    -   [Flycheck](#org7a32d76)
    -   [Dumb Jump](#orgce23385)
    -   [Parenthesis](#org2b3ee84)
    -   [Indentation](#org8bcbd86)
    -   [Quickrun](#org232d831)
    -   [Format All](#org7f88c84)
    -   [Evil Nerd Commenter](#orgf2a02bd)
    -   [Iedit](#org3aa414e)
    -   [Header](#org8482e65)
    -   [Jupyter Notebook](#org6fb6364)
    -   [Language Server Protocol](#org8018e37)
    -   [Debug Adapter Protocol](#orgb65fa61)
    -   [Company](#org26377f2)
-   [Programming](#org4761093)
    -   [Java](#orga918c4b)
    -   [C/C++/Objective C](#org573dbfa)
    -   [Golang](#org67eaead)
    -   [Python](#org295bf33)
    -   [Arduino](#org53fcba8)
    -   [Emacs Speaks Statistics](#orgffa2a59)
-   [Web Development](#org3fa664e)
    -   [Web Mode](#orgf68d422)
    -   [JavaScript/TypeScript](#org05112b8)
    -   [Emmet](#orgd58cd54)
    -   [Instant Rename Tag](#org0049721)
    -   [JSON](#org1f7639f)
-   [Miscellaneous](#org262100e)
    -   [Org](#orgf6f3e55)
    -   [Emacs Application Framework](#org8ff23ba)
    -   [Emacs Relay Chat](#orgc5b67d2)
    -   [Emacs Web Wowser](#orge08e955)
    -   [Tramp](#orgb6f3f16)
    -   [PDF Tools](#org7aee3a6)
    -   [LeetCode](#org01a4196)
    -   [Pyim](#org466fc25)
    -   [Tetris](#orgf4210ac)
    -   [Speed Type](#org7ea82c0)
    -   [2048 Game](#orge3d1a3d)
    -   [Zone](#orgaeedb9b)


<a id="orgc37b08e"></a>

# About EMACS

Emacs changes how you *think* about programming.

Emacs is **totally introspectable**. You can always find out 'what code runs when I press this button?'.

Emacs is an **incremental programming environment**. There's no edit-compile-run cycle. There isn't even an edit-run cycle. You can execute snippets of code and gradually turn them into a finished project. There's no distinction between your editor and your interpreter.

Emacs is a **mutable environment**. You can set variables, tweak functions with advice, or redefine entire functions. Nothing is off-limits.

Emacs provides **functionality without applications**. Rather than separate applications, functionality is all integrated into your Emacs instance. Amazingly, this works. Ever wanted to use the same snippet tool for writing C++ classes as well as emails?

Emacs is full of **incredible software concepts that haven't hit the mainstream yet**. For example:

-   Many platforms have a single item clipboard. Emacs has an **infinite clipboard**.
-   If you undo a change, and then continue editing, you can't redo the original change. Emacs allows **undoing to any historical state**, even allowing tree-based exploration of history.
-   Emacs supports a **reverse variable search**: you can find variables with a given value.
-   You can perform **structural editing** of code, allowing you to make changes without breaking syntax. This works for lisps (paredit) and non-lisps (smartparens).
-   Many applications use a modal GUI: for example, you can't do other edits during a find-and-replace operation. Emacs provides **recursive editing** that allow you to suspend what you're currently doing, perform other edits, then continue the original task.

Emacs has a **documentation culture**. Emacs includes a usage manual, a lisp programming manual, pervasive docstrings and even an interactive tutorial.

Emacs has a **broad ecosystem**. If you want to edit code in a niche language, there's probably an Emacs package for it.

Emacs doesn't have a monopoly on good ideas, and there are other great tools out there. Nonetheless, we believe the [Emacs learning curve](https://i.stack.imgur.com/7Cu9Z.jpg) pays off.

*This beautifully written **About EMACS** section credits to [Remacs](https://github.com/remacs/remacs).*


<a id="orgb967293"></a>

# About M-EMACS

M-EMACS is a custom GNU Emacs setup and configurations that aims not only to enhance the default Emacs experience, and hopefully be a sample that everyone can easily navigate and reference through a highly detailed README that contains 99% of the **entire** configuration code.

As a young EMACSer, I have experienced the struggle to find a detailed configuration that is loosely coupled and highly readable. This mostly due to the nature of source codes, sometimes comments are harder to notice or simply not enough. Therefore I decided to construct this README and present any human-readable explanation in a much more human-friendly way. Anyone, particularly Emacs beginners who have no idea where to start with their personal config, is more than welcome to read through this document and copy/paste any part to use it on their own.

This configuration is designed and tested for **GNU Emacs 26.1 and above only**. However, it is suggested to use **emacs27**, the latest version currently available.

```emacs-lisp
(when (version< emacs-version "26.1")
  (error "M-EMACS requires Emacs 26.1 and above!"))
```


<a id="org833dba3"></a>

## Screenshot

![img](images/Sample.png)


<a id="org3ada03a"></a>

## About README

This README is originated from init.org that is generated using `M-x org-gfm-export-to-markdown`. Every block of code is generated through this function - it exports the section of code from the `elisp/` directory. You will not see their presence in init.org.


<a id="orgf03b0a1"></a>

## Installation

1.  Install [GNU Emacs](https://www.gnu.org/software/emacs/).
    -   (Optional) On Ubuntu, `emacs-snapshot` is a great way to get latest version of Emacs.

        ```text
        sudo add-apt-repository -y ppa:ubuntu-elisp
        sudo apt-get update
        sudo apt-get install emacs-snapshot
        ```
    -   (Optional) Build latest Emacs from source.

        ```text
        # Install essential build tools
        sudo apt-get install build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev gnutls-dev libgtk-3-dev git autoconf
        # Clone source
        git clone --depth=1 https://github.com/emacs-mirror/emacs.git
        # Go to source
        cd emacs/
        # Build Emacs
        ./autogen.sh
        ./configure --with-mailutils
        make
        # Install (optional)
        sudo make install
        ```
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


<a id="org415df22"></a>

## Modification

You have the permission to use, modify, distribute in any way you want.

However, what is *free* stays *free*. After all, this is [GPL](LICENSE).

**Remember** you must manually sync this README with all the new changes you made by:

1.  Please do **NOT** edit this `README.md` file, edit `init.org` instead!
2.  If you add a new mode, create a new `<file-name>.el` file in `elisp/` directory.
3.  Put `(require '<file-name>)` in [init.el](init.el) accordingly.
4.  Add `#+INCLUDE: "~/.emacs.d/elisp/<place-holder>.el" src emacs-lisp :range-begin "<start-line-wrapper-exclusive>" :range-end "<end-line-wrapper-exclusive>"` in the appropriate section in `init.org`.
5.  Enter `C-x C-s` to save and update `:lines`. (if you don't see the updated effect, run `M-x update-includes` manually)
6.  Call `M-x org-gfm-export-to-markdown` to update `README.md` automatically.


<a id="org2eb5eb4"></a>

## Contribution

If you spotted a bug or you have any suggestions, please fill in an issue. If you have something to fix, feel free to create a pull request.


<a id="org7775561"></a>

## Special Thanks

Everyone starts somewhere, and I started here.

-   [Vincent Zhang's Centaur Emacs](https://github.com/seagle0128/.emacs.d)
-   [Henrik Lissner's Doom Emacs](https://github.com/hlissner/doom-emacs)
-   [Poncie Reyes's .emacs.d](https://github.com/poncie/.emacs.d)


<a id="org0db1145"></a>

# Startup


<a id="orge2c505b"></a>

## Lexical Binding

Use lexical-binding. [Why?](https://nullprogram.com/blog/2016/12/22/)

> Until Emacs 24.1 (June 2012), Elisp only had dynamically scoped variables, a feature, mostly by accident, common to old lisp dialects. While dynamic scope has some selective uses, it’s widely regarded as a mistake for local variables, and virtually no other languages have adopted it.

```emacs-lisp
;;; init.el --- -*- lexical-binding: t -*-
```


<a id="org57a8350"></a>

## Early Init

Emacs27 introduces `early-init.el`, which is run before `init.el`, before package and UI initialization happens.


### Defer Garbage Collection

Defer garbage collection further back in the startup process, according to [hlissner](https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast).

> The GC eats up quite a bit of time, easily doubling startup time. The trick is to turn up the memory threshold as early as possible.

```emacs-lisp
(setq gc-cons-threshold 100000000)
```


### Disable `package-enable-at-startup`

Package initialize occurs automatically, before `user-init-file` is loaded, but after `early-init-file`. We handle package initialization, so we must prevent Emacs from doing it early!

```emacs-lisp
(setq package-enable-at-startup nil)
```


### Unset `file-name-handler-alist`

Every file opened and loaded by Emacs will run through this list to check for a proper handler for the file, but during startup, it won’t need any of them.

```emacs-lisp
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
```


### Disable `site-run-file`

```emacs-lisp
(setq site-run-file nil)
```


### Disable Unnecessary Interface

It will be faster to disable them here before they've been initialized.

```emacs-lisp
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
```


<a id="orgc1bc45a"></a>

## Garbage Collection


### Set `gc-cons-threshold` Smaller for Interactive Use

A large `gc-cons-threshold` may cause freezing and stuttering during long-term interactive use.

```emacs-lisp
(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
```


### Garbage Collect When Emacs is Out of Focus

**Note:** Since Emacs27.1, `focus-out-hook` is obsolete.

```emacs-lisp
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
```


### Avoid Garbage Collect When Using Minibuffer

```emacs-lisp
(defun gc-minibuffer-setup-hook ()
  (setq gc-cons-threshold (* better-gc-cons-threshold 2))

(defun gc-minibuffer-exit-hook ()
  (garbage-collect)
  (setq gc-cons-threshold better-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook))))
```


<a id="orgbc2079a"></a>

## Load Path

Since all the configuration files are stored in `elisp/` folder, they need to be added to `load-path` on startup.

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

(update-to-load-path "~/.emacs.d/elisp")
```


<a id="org207baec"></a>

## Define Constants

```emacs-lisp
(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst *sys/root*
  (string-equal "root" (getenv "USER"))
  "Are you a ROOT user?")

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst *python*
  (executable-find "python")
  "Do we have python?")

(defconst *python3*
  (executable-find "python3")
  "Do we have python3?")

(defconst *tr*
  (executable-find "tr")
  "Do we have tr?")

(defconst *mvn*
  (executable-find "mvn")
  "Do we have Maven?")

(defconst *clangd*
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
  "Do we have clangd?")

(defconst *gcc*
  (executable-find "gcc")
  "Do we have gcc?")

(defconst *git*
  (executable-find "git")
  "Do we have git?")

(defconst *curl*
  (executable-find "curl")
  "Do we have curl?")
```


<a id="org36f3c39"></a>

# Package Management

Some packages are disabled with the `:disabled` tag, because I don't use them very often. They might not work.


<a id="org3d5096b"></a>

## Melpa Packages

```emacs-lisp
;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir "~/.emacs.d/elpa"
      package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ))
```

Load all Melpa packages in `elisp/` to `load-path` directly to reduce startup time.


<a id="orgffc726e"></a>

## Non-Melpa Packages

Add packages contained in `site-elisp/` to `load-path` too.


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


<a id="orgc7b2b67"></a>

## Configure Package Manager

```emacs-lisp
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
```


<a id="orgd6b0be3"></a>

## Use-Package

My Emacs configuration is almost entirely dependant on [use-package](https://github.com/jwiegley/use-package).

> The `use-package` macro allows you to isolate package configuration in your .emacs file in a way that is both performance-oriented and, well, tidy. I created it because I have over 80 packages that I use in Emacs, and things were getting difficult to manage. Yet with this utility my total load time is around 2 seconds, with no loss of functionality!

```emacs-lisp
;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))
```


<a id="org489eac2"></a>

## Auto Package Update

[Auto package update](https://github.com/rranelli/auto-package-update.el) automatically updates installed packages if at least `auto-package-update-interval` days have passed since the last update.

```emacs-lisp
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))
```


<a id="orgaba2c1a"></a>

## Diminish

[Diminish](https://github.com/emacsmirror/diminish), a feature that removes certain minor modes from mode-line.

```emacs-lisp
(use-package diminish)
```


<a id="orgb1d5cce"></a>

## GNU Elpa Keyring Update

[GNU Elpa Keyring Update](http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html), Update Emacs's GPG keyring for GNU ELPA.

> The GPG key used to sign the GNU ELPA archives is nearing retirement: it expires this September. Which means that if you don't get the new key before, you won't be able to check the signature of new packages after that date.

```emacs-lisp
(use-package gnu-elpa-keyring-update)
```


<a id="org4f4774a"></a>

# Global Functionalities


<a id="orgb7d73fb"></a>

## User Information

**Prerequisite**: Please update this file your personal info.

```emacs-lisp
(setq user-full-name "Mingde (Matthew) Zeng")
(setq user-mail-address "matthewzmd@gmail.com")
```


<a id="org025f4e5"></a>

## Bindings

```emacs-lisp
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Use iBuffer instead of Buffer List
(global-set-key (kbd "C-x C-b") #'ibuffer)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd"C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
```


<a id="org08428b9"></a>

## File Management


### Dired

Dired, the directory editor.

```emacs-lisp
(use-package dired
  :ensure nil
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  :config
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))
```


### Auto Save File and Backup

Set autosave and backup directory.

```emacs-lisp
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms  '((".*" "~/.emacs.d/autosaves/\\1" t))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
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


<a id="org70a8082"></a>

## Ivy


### Ivy, Amx, Counsel, Swiper

[Ivy](https://github.com/abo-abo/swiper), a generic completion mechanism for Emacs. It utilizes [Amx](https://github.com/DarwinAwardWinner/amx), [Counsel](https://github.com/abo-abo/swiper) and [Swiper](https://github.com/abo-abo/swiper).

```emacs-lisp
(use-package ivy
  :bind ("C-s" . swiper-isearch)
  :diminish
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (bind-key "C-r" 'ivy-previous-line-or-history ivy-minibuffer-map))
```


### Ivy Posframe

[Ivy Posframe](https://github.com/tumashu/ivy-posframe), an Ivy extension that uses posframe to show its candidate menu.

```emacs-lisp
(use-package ivy-posframe
  :if *sys/gui*
  :after ivy
  :diminish
  :custom-face
  (ivy-posframe ((t (:background "#303640"))))
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  :config
  (ivy-posframe-mode 1))
```


<a id="orgf5e6dc8"></a>

## Color RipGrep

[Color-RG](https://github.com/manateelazycat/color-rg), a search and refactoring tool based on ripgrep that is used to search text.

**Prerequisite**: Please install [ripgrep](https://github.com/BurntSushi/ripgrep#installation) and add `rg` to the `PATH`.

```emacs-lisp
(use-package color-rg
  :load-path "~/.emacs.d/site-elisp/color-rg"
  :if *rg*
  :bind ("C-M-s" . color-rg-search-input))
```


<a id="orga41a56d"></a>

## Snails

[Snails](https://github.com/manateelazycat/snails), a fuzzy search framework. It utilizes [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) if you are using Mac.

```emacs-lisp
(use-package snails
  :load-path "~/.emacs.d/site-elisp/snails/"
  :if *sys/gui*
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
  :config
  (use-package exec-path-from-shell
    :if (featurep 'cocoa) :defer t)

  ;; Functions for specific backends
  (defun snails-current-project ()
    (interactive)
    (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd)))
  (defun snails-active-recent-buffers ()
    (interactive)
    (snails '(snails-backend-buffer snails-backend-recentf)))
  (defun snails-everywhere ()
    (interactive)
    (snails '(snails-backend-everything snails-backend-mdfind)))
  :bind
  (("M-s s" . snails)
   ("M-s g" . snails-current-project)
   ("M-s b" . snails-active-recent-buffers)
   ("M-s e" . snails-everywhere)))
```


<a id="orgc1ebde6"></a>

## Avy

[Avy](https://github.com/abo-abo/avy), a nice way to move around text.

```emacs-lisp
(use-package avy
  :defer t
  :bind
  (("C-z ;" . avy-goto-char-timer)
   ("C-z :" . avy-goto-line))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  :custom-face
  (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))));
```


<a id="org0f93a9f"></a>

## Shell


### Shell Here

[Shell Here](https://github.com/ieure/shell-here), a tool that opens a shell buffer in (or relative to) `default-directory`.

```emacs-lisp
(use-package shell-here
  :bind ("M-#" . shell-here))
```


### Term Keys

[Term Keys](https://github.com/CyberShadow/term-keys), a lossless keyboard input for Emacs in terminal emulators.

```emacs-lisp
(use-package term-keys
  :if (not *sys/gui*)
  :config (term-keys-mode t))
```


<a id="orgc297dad"></a>

## Winner

Winner, a mode to restore previous window layouts.

```emacs-lisp
(use-package winner
  :ensure nil
  :custom
  (winner-boring-buffers
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
  :config
  (winner-mode 1))
```


<a id="org6128e4a"></a>

## Which Key

[Which Key](https://github.com/justbur/emacs-which-key), a feature that displays the key bindings following the incomplete command.

```emacs-lisp
(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))
```


<a id="org1b2afb2"></a>

## Popup Kill Ring

[Popup Kill Ring](https://github.com/waymondo/popup-kill-ring), a feature that provides the ability to browse Emacs kill ring in autocomplete style popup menu.

```emacs-lisp
(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))
```


<a id="orgadb7567"></a>

## Undo Tree

[Undo tree](https://www.emacswiki.org/emacs/UndoTree), a feature that provides a visualization of the undos in a file.

```emacs-lisp
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))
```


<a id="orgf58e5e1"></a>

## Discover My Major

[Discover my major](https://github.com/jguenther/discover-my-major), a feature that discovers key bindings and their meaning for the current Emacs major mode.

```emacs-lisp
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))
```


<a id="orgcb4fa1c"></a>

## Ace Window

[Ace Window](https://github.com/abo-abo/ace-window), a package for selecting windows to switch to.

```emacs-lisp
(use-package ace-window
  :bind ("C-x C-o" . ace-window))
```


<a id="org24410e1"></a>

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
(when *sys/gui*
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
```


### Optimize Editing Experience

```emacs-lisp
;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Make sentences end with a single space
(setq-default sentence-end-double-space nil)

;; Disable Shift mark
(setq shift-select-mode nil)

;; Replace selection on insert
(delete-selection-mode 1)

;; Merge system clipboard with Emacs
(setq-default select-enable-clipboard t)

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)
```


### History

```emacs-lisp
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)
```


### Turn Off Cursor Alarms

```emacs-lisp
(setq ring-bell-function 'ignore)
```


### Show Keystrokes in Progress Instantly

```emacs-lisp
(setq echo-keystrokes 0.1)
```


### Don't Lock Files

```emacs-lisp
(setq-default create-lockfiles nil)
```


### Compilation

Better compilation configurations.

```emacs-lisp
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)
```


### Suppress ad-handle-definition warnings

These warnings are generated when functions are redefined with `defadvice`, they are not helpful.

```emacs-lisp
(setq ad-redefinition-action 'accept)
```


### Move Custom-Set-Variables to Different File

```emacs-lisp
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)
```


### So-Long

So Long mitigates slowness due to extremely long lines. Currently available in Emacs master branch **only**!

```emacs-lisp
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))
```


<a id="org99af91c"></a>

## Functions

Important functions.


### Resize Window Width / Height Functions

```emacs-lisp
;; Resizes the window width based on the input
(defun resize-window-width (w)
  "Resizes the window width based on W."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window width in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (message "%s" w)
  (window-resize nil (- (truncate (* (/ w 10.0) (frame-width))) (window-total-width)) t))

;; Resizes the window height based on the input
(defun resize-window-height (h)
  "Resizes the window height based on H."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window height in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (message "%s" h)
  (window-resize nil (- (truncate (* (/ h 10.0) (frame-height))) (window-total-height)) nil))

;; Setup shorcuts for window resize width and height
(global-set-key (kbd "C-z w") #'resize-window-width)
(global-set-key (kbd "C-z h") #'resize-window-height)
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
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
```


### Update Org Mode Include Automatically

Update Org Mode INCLUDE Statements Automatically from [Artur Malabarba](http://endlessparentheses.com/updating-org-mode-include-statements-on-the-fly.html).

```emacs-lisp
(defun save-and-update-includes (&rest ignore)
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

(add-hook 'before-save-hook #'save-and-update-includes)

(defun decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file-contents file)
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


### Save All Buffers

```emacs-lisp
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)
```


### Display Line Overlay

```emacs-lisp
(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:background null :inherit highlight)))
    ol))
```


### Read Lines From File

```emacs-lisp
(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer (insert-file-contents filePath)
                    (split-string (buffer-string) "\n" t)))
```


### Where Am I

Displays the file path when visiting a file, otherwise displays the buffer name.

```emacs-lisp
(defun where-am-i ()
    "An interactive function that displays `buffer-file-name' when visiting a file.
Otherwise the function displays `buffer-name'."
    (interactive)
    (let ((dir-file (buffer-file-name)))
      (if dir-file
          (message dir-file)
        (message (buffer-name)))))
```


<a id="org640ecae"></a>

# User Interface Enhancements


<a id="org18aa49a"></a>

## Doom Themes

[Doom Themes](https://github.com/hlissner/emacs-doom-themes), an UI plugin and pack of themes.

```emacs-lisp
(use-package doom-themes
  :custom-face
  (cursor ((t (:background "BlanchedAlmond"))))
  :config
  ;; flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-one t))
```


<a id="orgd0c47e4"></a>

## Doom Modeline

[Doom Modeline](https://github.com/seagle0128/doom-modeline), a modeline from DOOM Emacs, but more powerful and faster.

```emacs-lisp
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15))
```


<a id="orgfc0282b"></a>

## Dashboard


### Dashboard

[Dashboard](https://github.com/rakanalh/emacs-dashboard), an extensible Emacs startup screen.

Use either `KEC_Dark_BK.png` or `KEC_Light_BK.png` depends on the backgrond theme.

```emacs-lisp
(use-package dashboard
  :demand
  :diminish (dashboard-mode page-break-lines-mode)
  :bind ("C-z d" . open-dashboard)
  :custom
  (dashboard-banner-logo-title "Close the world. Open the nExt.")
  (dashboard-startup-banner "~/.emacs.d/images/KEC_Dark_BK_Small.png")
  (dashboard-items '((recents  . 7)
                     (bookmarks . 5)
                     (agenda . 5)))
  :custom-face
  (dashboard-banner-logo-title ((t (:family "Love LetterTW" :height 123))))
  :config
  (dashboard-setup-startup-hook)
  ;; Additional Dashboard widgets.
  (defun dashboard-insert-widgets (list-size)
    ;; (insert (format "%d packages loaded in %s.\n" (length package-activated-list) (emacs-init-time)))
    (insert "Navigation: ")
    ;;(insert (make-string (max 0 (floor (/ (- dashboard-banner-length 25) 2))) ?\ ))
    (widget-create 'url-link
                   :tag (propertize "Github" 'face 'font-lock-keyword-face)
                   :help-echo "Open M-EMACS Github"
                   :mouse-face 'highlight
                   "https://github.com/MatthewZMD/.emacs.d")
    (insert " ")
    (widget-create 'push-button
                   :help-echo "Edit M-EMACS configuration"
                   :action (lambda (&rest _) (edit-configs))
                   :mouse-face 'highlight
                   :button-prefix ""
                   :button-suffix ""
                   (propertize "Configuration" 'face 'font-lock-keyword-face)))
  (add-to-list 'dashboard-item-generators  '(buttons . dashboard-insert-widgets))
  (add-to-list 'dashboard-items '(buttons))
  ;; Open Dashboard function
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
    (delete-other-windows)))
```


### Page Break Lines

[Page-break-lines](https://github.com/purcell/page-break-lines), a feature that displays ugly form feed characters as tidy horizontal rules.

```emacs-lisp
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))
```


<a id="orgfd2e664"></a>

## Fonts and Icons

**Prerequisite**: Install all the available fonts and icons from `fonts/`.


### Fonts

```emacs-lisp
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar font-list '(("Input" . 11) ("SF Mono" . 12) ("Consolas" . 12) ("Love LetterTW" . 12.5))
  "List of fonts and sizes.  The first one available will be used.")
```

Function to switch between fonts.

```emacs-lisp
(defun change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
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

(when *sys/gui*
  (change-font))
```


### Icons

[All The Icons](https://github.com/domtronn/all-the-icons.el), a utility package to collect various Icon Fonts. Enable only in GUI Emacs.

```emacs-lisp
(use-package all-the-icons :if *sys/gui*)
```

[All The Icons Dired](https://github.com/jtbm37/all-the-icons-dired), an icon set for Dired.

```emacs-lisp
(use-package all-the-icons-dired
  :after all-the-icons
  :if *sys/gui*
  :diminish
  :custom-face
  (all-the-icons-dired-dir-face ((t `(:foreground ,(face-background 'default)))))
  :config
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  ;; Workaround for all-the-icons bug until PR merged https://github.com/domtronn/all-the-icons.el/pull/150
  (when (require 'all-the-icons nil 'noerror)
    (setq all-the-icons-mode-icon-alist
          (delete '(erc-mode all-the-icons-faicon "commenting-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-white) all-the-icons-mode-icon-alist))
    (add-to-list 'all-the-icons-mode-icon-alist '(erc-mode all-the-icons-faicon "commenting-o" :height 1.0 :v-adjust 0.0))))
```


<a id="orgf9c4099"></a>

## Smooth Scrolling

Configurations to smooth scrolling.

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


<a id="orgafee26a"></a>

## Emojify

[Emojify](https://github.com/iqbalansari/emacs-emojify), a mode for displaying Emojis in Emacs.

```emacs-lisp
(use-package emojify
  :if *sys/gui*
  :hook (after-init . global-emojify-mode)
  :bind ("M-i" . emojify-insert-emoji))
```


<a id="org6ba6599"></a>

## Prettify Symbols

[Prettify symbols mode](https://www.emacswiki.org/emacs/PrettySymbol), a built-in mode for displaying sequences of characters as fancy characters or symbols.

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


<a id="org04e428d"></a>

## UI Configs


### Title Bar

```emacs-lisp
(setq-default frame-title-format '("M-EMACS - " user-login-name "@" system-name " - %b"))
```


### Simplify Yes/No Prompts

```emacs-lisp
(fset 'yes-or-no-p 'y-or-n-p)
```


### Disable Splash Screen

```emacs-lisp
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "Present Day, Present Time...")
```


### Line Numbers

Display line numbers, and column numbers in modeline.

```emacs-lisp
;; Hook line numbers to only when files are opened
(add-hook 'find-file-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Display column numbers in modeline
(column-number-mode 1)
```


### Modeline Time and Battery

Display time and battery information in modeline.

```emacs-lisp
(display-time-mode 1)
(display-battery-mode 1)
```


<a id="org381b086"></a>

# General Programming


<a id="orgf68a110"></a>

## Magit

[Magit](https://magit.vc/), an interface to the version control system Git.

```emacs-lisp
(use-package magit
  :if *git*
  :bind ("C-x g" . magit-status))
```


<a id="org61434e9"></a>

## Projectile

[Projectile](https://github.com/bbatsov/projectile), a Project Interaction Library for Emacs.

**Prerequisite**: Windows OS: Install [Gow](https://github.com/bmatzelle/gow/releases) and add to `PATH`.

[Gow](https://github.com/bmatzelle/gow) is a lightweight installer that installs useful open source UNIX applications compiled as native win32 binaries. Specifically, `tr` is needed for Projectile alien indexing.

```emacs-lisp
(use-package projectile
  :bind
  ("C-c p" . projectile-command-map)
  ("C-z o" . projectile-find-file)
  ("C-z p" . projectile-add-known-project)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (when (and *sys/win32* *tr*)
    (setq projectile-indexing-method 'alien))
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))
```


<a id="orgad76276"></a>

## Treemacs

[Treemacs](https://github.com/Alexander-Miller/treemacs), a tree layout file explorer for Emacs.


### Treemacs

```emacs-lisp
(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-display-in-side-window t)
  (treemacs-file-event-delay 5000)
  (treemacs-file-follow-delay 0.2)
  (treemacs-follow-after-init t)
  (treemacs-follow-recenter-distance 0.1)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-desc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-width 35)
  :config
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map ("C-p" . treemacs-previous-line)))
```


### Treemacs Magit

```emacs-lisp
(use-package treemacs-magit
  :defer t
  :after (treemacs magit))
```


### Treemacs Projectile

```emacs-lisp
(use-package treemacs-projectile
  :defer t
  :after (treemacs projectile))
```


<a id="org16fe588"></a>

## YASnippet


### YASnippet

[YASnippet](https://github.com/joaotavora/yasnippet), a programming template system for Emacs. It loads [YASnippet Snippets](https://github.com/AndreaCrotti/yasnippet-snippets), a collection of yasnippet snippets for many languages.

```emacs-lisp
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all))
```


<a id="org7a32d76"></a>

## Flycheck

[Flycheck](https://www.flycheck.org/en/latest/), a syntax checking extension.

```emacs-lisp
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
```


<a id="orgce23385"></a>

## Dumb Jump

[Dumb jump](https://github.com/jacktasia/dumb-jump), an Emacs "jump to definition" package.

```emacs-lisp
(use-package dumb-jump
  :bind
  (:map prog-mode-map
        (("C-c C-o" . dumb-jump-go-other-window)
         ("C-c C-j" . dumb-jump-go)
         ("C-c C-i" . dumb-jump-go-prompt)))
  :custom (dumb-jump-selector 'ivy))
```


<a id="org2b3ee84"></a>

## Parenthesis


### Smartparens

[Smartparens](https://github.com/Fuco1/smartparens), a minor mode for dealing with pairs.

```emacs-lisp
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-a" . sp-backward-down-sexp)
        ("C-M-e" . sp-up-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-k" . sp-change-enclosing)
        ("M-k" . sp-kill-sexp)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" . sp-splice-sexp-killing-around)
        ("C-]" . sp-select-next-thing-exchange))
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil)
  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
  ;; <https://github.com/Fuco1/smartparens/issues/963>.
  (unless (version< emacs-version "27")
    (dolist (fun '(c-electric-paren c-electric-brace))
      (add-to-list 'sp--special-self-insert-commands fun))))
```


### Awesome Pair

[Awesome Pair](https://github.com/manateelazycat/awesome-pair), a feature that provides grammatical parenthesis completion.

```emacs-lisp
(use-package awesome-pair
  :load-path "~/.emacs.d/site-elisp/awesome-pair"
  :bind
  (:map prog-mode-map
        (("C-c C-k" . awesome-pair-kill)
         ("SPC" . awesome-pair-space)
         ("=" . awesome-pair-equal)
         ("M-F" . awesome-pair-jump-right)
         ("M-B" . awesome-pair-jump-left)))
  :config
  (add-hook 'prog-mode-hook '(lambda () (awesome-pair-mode 1))))
```


### Delete Block

[Delete Block](https://github.com/manateelazycat/delete-block), a feature that deletes block efficiently.

```emacs-lisp
(use-package delete-block
  :load-path "~/.emacs.d/site-elisp/delete-block"
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)))
```


### Match Parenthesis

Match and automatically pair parenthesis, and show parenthesis even when it went offscreen from [Clemens Radermacher](https://with-emacs.com/posts/editing/show-matching-lines-when-parentheses-go-off-screen/).

```emacs-lisp
;; Show matching parenthesis
(show-paren-mode 1)
;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)

;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)
(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
    (defun show-paren--off-screen+ (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp ov)
        (delete-overlay ov))
      ;; check if it's appropriate to show match info,
      ;; see `blink-paren-post-self-insert-function'
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; rebind `minibuffer-message' called by
        ;; `blink-matching-open' to handle the overlay display
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq ov (display-line-overlay+
                                 (window-start) msg))))))
          (blink-matching-open))))))
```


<a id="org8bcbd86"></a>

## Indentation

[Highlight Indent Guides](https://github.com/DarthFennec/highlight-indent-guides), a feature that highlights indentation levels.

```emacs-lisp
(use-package highlight-indent-guides
  :if *sys/gui*
  :diminish
  :hook ((prog-mode web-mode nxml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 7))
```

Indentation Configuration

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default js-switch-indent-offset 4)
(c-set-offset 'comment-intro 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'case-label '+)
```


<a id="org232d831"></a>

## Quickrun

[Quickrun](https://github.com/syohex/emacs-quickrun), compile and run source code quickly.

```emacs-lisp
(use-package quickrun
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)))
```


<a id="org7f88c84"></a>

## Format All

[Format all](https://github.com/lassik/emacs-format-all-the-code), a feature that lets you auto-format source code.

**Prerequisite**: Read [Supported Languages](https://github.com/lassik/emacs-format-all-the-code#supported-languages) to see which additional tool you need to install for the specific language.

```emacs-lisp
(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))
```


<a id="orgf2a02bd"></a>

## Evil Nerd Commenter

[Evil Nerd Commenter](https://github.com/redguardtoo/evil-nerd-commenter), a tool that helps you comment code efficiently.

```emacs-lisp
(use-package evil-nerd-commenter
  :bind
  ("M-;" . evilnc-comment-or-uncomment-lines))
```


<a id="org3aa414e"></a>

## Iedit

[Iedit](https://github.com/victorhge/iedit), a minor mode that allows editing multiple regions simultaneousy in a buffer or a region.

```emacs-lisp
(use-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)
```


<a id="org8482e65"></a>

## Header

[Header2](https://www.emacswiki.org/emacs/header2.el), a support for creation and update of file headers.

```emacs-lisp
(use-package header2
  :load-path "~/.emacs.d/site-elisp/header2"
  :custom
  (header-copyright-notice (concat "Copyright (C) 2019 " (user-full-name) "\n"))
  :config
  (autoload 'auto-make-header "header2")
  (autoload 'auto-update-file-header "header2")
  (add-hook 'write-file-hooks 'auto-update-file-header)
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
  (add-hook 'c-mode-common-hook   'auto-make-header))
```


<a id="org6fb6364"></a>

## Jupyter Notebook

[Emacs IPython Notebook](https://github.com/millejoh/emacs-ipython-notebook), a [Jupyter](https://jupyter.org/) (formerly IPython) client in Emacs.


### Usage

1.  Execute `M-x ein:run` to launch a local Jupyter session.
2.  Login with `M-x ein:login` to a local or remote session.
3.  Open `.ipynb` file and press `C-c C-o`.

```emacs-lisp
(use-package ein
  :disabled
  :defer t)
```


<a id="org8018e37"></a>

## Language Server Protocol


### LSP Mode

[LSP Mode](https://github.com/emacs-lsp/lsp-mode), a client/library for the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). M-EMACS tries to use lsp-mode whenever possible.

```emacs-lisp
(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((java-mode python-mode go-mode
          js-mode js2-mode typescript-mode web-mode
          c-mode c++-mode objc-mode) . lsp))
```


### LSP UI

[LSP UI](https://github.com/emacs-lsp/lsp-ui), provides all the higher level UI modules of lsp-mode, like flycheck support and code lenses.

```emacs-lisp
(use-package lsp-ui
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
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if *sys/gui*
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))
```


<a id="orgb65fa61"></a>

## Debug Adapter Protocol

[DAP Mode](https://github.com/emacs-lsp/dap-mode), a client/library for the [Debug Adapter Protocol](https://code.visualstudio.com/api/extension-guides/debugger-extension).

**Prerequisite**: See [Configuration](https://github.com/emacs-lsp/dap-mode#configuration) to configure DAP appropriately.

```emacs-lisp
(use-package dap-mode
  :diminish
  :bind
  (:map dap-mode-map
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle)))
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (python-mode . (lambda () (require 'dap-python)))
         (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
         (php-mode . (lambda () (require 'dap-php)))
         (elixir-mode . (lambda () (require 'dap-elixir)))
         ((js-mode js2-mode typescript-mode) . (lambda () (require 'dap-chrome)))))
```


<a id="org26377f2"></a>

## Company


### Company Mode

[Company](http://company-mode.github.io/), a text completion framework for Emacs.

```emacs-lisp
(use-package company
  :diminish company-mode
  :hook (prog-mode . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations 't)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common))
```


### Company LSP

[Company LSP](https://github.com/tigersoldier/company-lsp), a Company completion backend for lsp-mode.

```emacs-lisp
(use-package company-lsp
  :defer t
  :custom (company-lsp-cache-candidates 'auto))
```


### Company TabNine

[Company TabNine](https://github.com/TommyX12/company-tabnine), A company-mode backend for [TabNine](https://tabnine.com/), the all-language autocompleter.

This is enabled by default, if ever you find it not good enough for a particular completion, simply use `M-q` to immediately switch to default backends.

**Prerequisite**: Execute `M-x company-tabnine-install-binary` to install the TabNine binary for your system.

```emacs-lisp
(use-package company-tabnine
  :demand
  :custom
  (company-tabnine-max-num-results 3)
  :bind
  (("M-q" . company-other-backend)
   ("C-z t" . company-tabnine))
  :config
  ;; Enable TabNine on default
  (add-to-list 'company-backends #'company-tabnine)

  ;; Integrate company-tabnine with lsp-mode
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6)))))
  (add-hook 'lsp-after-open-hook
            (lambda ()
              (add-to-list 'company-transformers 'company//sort-by-tabnine t)
              (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate)))))
```


<a id="org4761093"></a>

# Programming


<a id="orga918c4b"></a>

## Java


### LSP Java

[LSP Java](https://github.com/emacs-lsp/lsp-java), Emacs Java IDE using [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls). Note that this package is dependant on [Request](https://github.com/tkf/emacs-request).

**Prerequisite**: Install [Maven](https://maven.apache.org/download.cgi) and add to `PATH`.

```emacs-lisp
(use-package lsp-java
  :after lsp-mode
  :if *mvn*
  :config
  (use-package request :defer t)
  :custom
  (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/")))
```


<a id="org573dbfa"></a>

## C/C++/Objective C

**Prerequisite**: Since all completion features are provided by [LSP Mode](https://github.com/emacs-lsp/lsp-mode), it needs to setup.

-   Install [CMake](https://cmake.org/download/) >= 3.8 for all OS.
-   \*nix OS:
    -   It is suggested to use [CCLS](https://github.com/MaskRay/ccls) as LSP server. Now [build](https://github.com/MaskRay/ccls/wiki/Build) it.
    -   Set `ccls-executable` to the directory where your ccls is built.
-   Windows OS:
    -   Install [MinGW](http://www.mingw.org/wiki/Install_MinGW) for Compilation.
    -   It is a pain to build CCLS on Windows, install [Clangd](https://clang.llvm.org/extra/clangd/Installation.html) and add to `PATH` instead.


### CCLS

[Emacs CCLS](https://github.com/MaskRay/emacs-ccls), a client for [CCLS](https://github.com/MaskRay/ccls), a C/C++/Objective-C language server supporting multi-million line C++ code-bases, powered by libclang.

```emacs-lisp
(use-package ccls
  :defer t
  :if (not *sys/win32*)
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable "~/tools/ccls/Release/ccls"))
```


### Modern C++ Font Lock

[Modern CPP Font Lock](https://github.com/ludwigpacifici/modern-cpp-font-lock), font-locking for "Modern C++".

```emacs-lisp
(use-package modern-cpp-font-lock
  :diminish
  :init (modern-c++-font-lock-global-mode t))
```


<a id="org67eaead"></a>

## Golang

[Go Mode](https://github.com/dominikh/go-mode.el), an Emacs mode for editing Golang code.

**Prerequisite**: [gopls](https://github.com/golang/go/wiki/gopls) is suggested for Golang's LSP support.

```text
go get golang.org/x/tools/gopls@latest
```

```emacs-lisp
(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save))
```


<a id="org295bf33"></a>

## Python


### Python Configuration

```emacs-lisp
(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))
```


### LSP Python MS

[LSP Python MS](https://github.com/andrew-christianson/lsp-python-ms), a lsp-mode client leveraging [Microsoft's Python Language Server](https://github.com/Microsoft/python-language-server).

```emacs-lisp
(use-package lsp-python-ms
  :after lsp-mode python
  :if (or *python3* *python*)
  :custom
  (lsp-python-executable-cmd "python3"))
```


<a id="org53fcba8"></a>

## Arduino


### Arduino Mode

[Arduino mode](https://github.com/bookest/arduino-mode), a major mode for editing Arduino sketches.

```emacs-lisp
(use-package arduino-mode
  :disabled
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
  (add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
  (autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t))
```


### Company Arduino

[Company Arduino](https://github.com/yuutayamada/company-arduino), a set of configuration to let you auto-completion by using irony-mode, company-irony and company-c-headers on arduino-mode.

```emacs-lisp
(use-package company-arduino
  :disabled
  :defer t
  :config
  (add-hook 'irony-mode-hook 'company-arduino-turn-on)
  ;; Activate irony-mode on arduino-mode
  (add-hook 'arduino-mode-hook 'irony-mode))
```


<a id="orgffa2a59"></a>

## Emacs Speaks Statistics

[Emacs Speaks Statistics](https://ess.r-project.org/), short for ESS, it's designed to support editing of scripts and interaction with various statistical analysis programs such as R, S-Plus, SAS, Stata and OpenBUGS/JAGS.

**Prerequisite**: Install [R](https://cran.r-project.org/mirrors.html) to start using ESS with R.

```emacs-lisp
(use-package ess
  :config
  (load "ess-autoloads"))
```


<a id="org3fa664e"></a>

# Web Development

**Prerequisite**: Install [NodeJS](https://nodejs.org/en/download/) and add to path. Execute following commands to enable LSP for JavaScript/TypeScript/HTML:

```text
npm i -g typescript
npm i -g typescript-language-server
```


<a id="orgf68d422"></a>

## Web Mode

[Web mode](https://github.com/fxbois/web-mode), a major mode for editing web templates.

```emacs-lisp
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))
```


<a id="org05112b8"></a>

## JavaScript/TypeScript


### JavaScript2 Mode

[JS2 mode](https://github.com/mooz/js2-mode), a feature that offers improved JavsScript editing mode.

```emacs-lisp
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")
```


### TypeScript Mode

[TypeScript mode](https://github.com/emacs-typescript/typescript.el), a feature that offers TypeScript support for Emacs.

```emacs-lisp
(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode))
```


<a id="orgd58cd54"></a>

## Emmet

[Emmet](https://github.com/smihica/emmet-mode), a feature that allows writing HTML using CSS selectors along with `C-j`. See [usage](https://github.com/smihica/emmet-mode#usage) for more information.

```emacs-lisp
(use-package emmet-mode
  :hook web-mode
  :config
  (add-hook 'css-mode-hooktype  'emmet-mode)) ;; enable Emmet's css abbreviation
```


<a id="org0049721"></a>

## Instant Rename Tag

[Instant Rename Tag](https://github.com/manateelazycat/instant-rename-tag), a plugin that provides ability to rename html tag pairs instantly.

```emacs-lisp
(use-package instant-rename-tag
  :load-path "~/.emacs.d/site-elisp/instant-rename-tag"
  :bind ("C-z <" . instant-rename-tag))
```


<a id="org1f7639f"></a>

## JSON

[JSON Mode](https://github.com/joshwnj/json-mode), a major mode for editing JSON files.

```emacs-lisp
(use-package json-mode
  :mode "\\.json\\'")
```


<a id="org262100e"></a>

# Miscellaneous


<a id="orgf6f3e55"></a>

## Org

[Org](https://orgmode.org/), a Emacs built-in tool for keeping notes, maintaining TODO lists, planning projects, and authoring documents with a fast and effective plain-text system.

**Prerequisite**: Modify `(setq org-agenda-files (list "~/org/agenda/"))` to your agenda folder.

```emacs-lisp
(use-package org
  :ensure nil
  :defer t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switch)
  :custom
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE")))
  :config
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/"))))
```


### TOC Org

[TOC Org](https://github.com/snosov1/toc-org) generates table of contents for `.org` files

```emacs-lisp
(use-package toc-org
  :hook (org-mode . toc-org-mode))
```


### HTMLize

[HTMLize](https://github.com/hniksic/emacs-htmlize), a tool that converts buffer text and decorations to HTML.

```emacs-lisp
(use-package htmlize :defer t)
```


### GFM Exporter

[OX-GFM](https://github.com/larstvei/ox-gfm), a Github Flavored Markdown exporter for Org Mode.

```emacs-lisp
(use-package ox-gfm :defer t)
```


<a id="org8ff23ba"></a>

## Emacs Application Framework

[Emacs Application Framework](https://github.com/manateelazycat/emacs-application-framework), a development framework that integrates any PyQt program into Emacs.

**Note**: I am using my own [fork](https://github.com/MatthewZMD/emacs-application-framework), which has more functions and capabilities that are yet to be merged into upstream. If you want to use this config, clone my fork instead. Otherwise it won't work.

**Prerequisite**: Please install `python3` and `pip3`. Then execute below command:

```text
sudo pip3 install dbus-python PyMuPDF grip qrcode pyqt5 python-xlib PyQtWebEngine
```

Note that If you are using Debian/Ubuntu, it is possible that `QtWebEngine` is [not working](https://marc.info/?l=kde-core-devel&m=142954900813235&w=2). Install the following:

```text
sudo apt-get install python3-pyqt5.qtwebengine python3-pyqt5.qtmultimedia
```

```emacs-lisp
(use-package eaf
  :load-path "~/.emacs.d/site-elisp/emacs-application-framework"
  :if (and *sys/linux* *sys/gui* *python3*)
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  ;; I already bind "RET", "<mouse-2>", "^" to `dired-find-alternate-file' in `init-dired.el'.
  ;; Comment this line out of you don't want to use EAF to open available files in dired.
  (global-set-key [remap dired-find-alternate-file] #'eaf-file-open-in-dired)

  (defun eaf-open-google ()
    "Open Google using EAF."
    (interactive)
    (eaf-open-browser "https://www.google.com")))
```


<a id="orgc5b67d2"></a>

## Emacs Relay Chat

[ERC](https://www.emacswiki.org/emacs/ERC), a powerful, modular, and extensible [IRC](http://www.irc.org/) client for Emacs. It utilizes [erc-hl-nicks](https://github.com/leathekd/erc-hl-nicks) for nickname highlighting and [erc-image](https://github.com/kidd/erc-image.el) to fetch and show received images in ERC.

**Prerequisite**: Put IRC credentials like the following in the file `~/.authinfo`:

```text
machine irc.freenode.net login <nickname> password <password>
```

```emacs-lisp
(use-package erc
  :ensure nil
  :custom-face
  (erc-notice-face ((t (:foreground "#ababab"))))
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
  (erc-track-exclude-types '("NICK" "PART" "MODE" "324" "329" "332" "333" "353" "477"))
  ; (erc-hide-list '("JOIN" "PART" "QUIT"))
  ; (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-server-coding-system '(utf-8 . utf-8))
  (erc-interpret-mirc-color t)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-lurker-threshold-time 43200)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  :config
  (use-package erc-hl-nicks :defer t)
  (use-package erc-image :defer t)
  (add-to-list 'erc-modules 'notifications)
  (erc-track-mode t)
  (erc-services-mode 1)
  :preface
  (defun erc-start-or-switch ()
    "Start ERC or switch to ERC buffer if it has started already."
    (interactive)
    (if (get-buffer "irc.freenode.net:6697")
        (erc-track-switch-buffer 1)
      (if (file-exists-p "~/.authinfo")
          (let ((auth-list (read-lines "~/.authinfo"))
                (nick-regexp "^machine irc.freenode.net login \\(\\w+\\)")
                (auth))
            (while (> (length auth-list) 0)
              (setq auth (car auth-list))
              (cond ((string-match nick-regexp auth)
                     (setq erc-prompt-for-nickserv-password 'nil)
                     (erc-tls :server "irc.freenode.net" :port 6697
                              :nick (match-string 1 auth)))
                    ((= (length auth-list) 1) (call-interactively #'erc-tls)))
              (setq auth-list (cdr auth-list))))
        (call-interactively #'erc-tls))))

  (defun erc-count-users ()
    "Displays the number of users and ops connected on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6697")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let ((hash-table (with-current-buffer (erc-server-buffer)
                                  erc-server-users))
                    (users 0)
                    (ops 0))
                (maphash (lambda (k v)
                           (when (member (current-buffer)
                                         (erc-server-user-buffers v))
                             (cl-incf users))
                           (when (erc-channel-user-op-p k)
                             (cl-incf ops)))
                         hash-table)
                (message "%d users (%s ops) are online on %s" users ops channel))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun erc-get-ops ()
    "Displays the names of ops users on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6697")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let (ops)
                (maphash (lambda (nick cdata)
                           (if (and (cdr cdata)
                                    (erc-channel-user-op (cdr cdata)))
                               (setq ops (cons nick ops))))
                         erc-channel-users)
                (if ops
                    (message "The online ops users are: %s"  (mapconcat 'identity ops " "))
                  (message "There are no ops users online on %s" channel)))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title)))
  :bind
  ("C-z i" . erc-start-or-switch)
  :hook
  (ercn-notify . erc-notify))
```


<a id="orge08e955"></a>

## Emacs Web Wowser

EWW, the Emacs Web Wowser.

```emacs-lisp
(use-package eww
  :ensure nil
  :commands (eww)
  :config
  ;; Hit & to browse this url system browser
  (setq browse-url-browser-function 'eww-browse-url)
  (defun rename-eww-hook ()
    "Rename EWW's buffer so sites open in new page."
    (rename-buffer "eww" t))
  (add-hook 'eww-mode-hook #'rename-eww-hook))
```


<a id="orgb6f3f16"></a>

## Tramp

[Tramp](https://www.emacswiki.org/emacs/TrampMode), short for Transparent Remote Access, Multiple Protocols is a package for editing remote files using a remote shell connection (rlogin, telnet, ssh).


### Google Cloud Platform

Connect to Google Cloud Platform using the following:

```text
/gssh:some-instance:/path/to/file
```

```emacs-lisp
(use-package tramp
  :ensure nil
  :defer 1
  :config
  ;; TRAMP gcloud ssh
  (add-to-list 'tramp-methods
               '("gssh"
                 (tramp-login-program        "gcloud compute ssh")
                 (tramp-login-args           (("%h")))
                 (tramp-async-args           (("-q")))
                 (tramp-remote-shell         "/bin/sh")
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                              ("-o" "UserKnownHostsFile=/dev/null")
                                              ("-o" "StrictHostKeyChecking=no")))
                 (tramp-default-port         22))))
```


<a id="org7aee3a6"></a>

## PDF Tools

[PDF Tools](https://github.com/politza/pdf-tools), an Emacs support library for PDF files. It works best on non-Windows OS.

```emacs-lisp
(use-package pdf-tools
  :if (and *sys/gui* (not *sys/win32*))
  :custom
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :config
  (pdf-loader-install))
```


<a id="org01a4196"></a>

## LeetCode

[LeetCode](https://github.com/kaiwk/leetcode.el), an Emacs LeetCode client. Note that this package is dependant on [Request-Deferred](https://github.com/tkf/emacs-request/blob/master/request-deferred.el) and [GraphQL](https://github.com/davazp/graphql-mode).

**Prerequisite**: Ensure `cURL` is installed added to `PATH`. If you are using Windows, it is suggested to use `curl` provided by Git, therefore add `<path to Git>\mingw64\bin` to `PATH`.

```emacs-lisp
(use-package leetcode
  :load-path "~/.emacs.d/site-elisp/leetcode.el"
  :if *curl*
  :commands (leetcode)
  :init
  (use-package request-deferred :defer t)
  (use-package graphql :defer t)
  :custom
  ;; (request-message-level 'debug)
  ;; (request-log-level 'debug)
  (leetcode-prefer-language "python3"))
```


<a id="org466fc25"></a>

## Pyim

[Pyim](https://github.com/tumashu/pyim), an Emacs Chinese Pinyin Input.

```emacs-lisp
(use-package pyim
  :custom
  (default-input-method "pyim")
  (pyim-default-scheme 'quanpin)
  (pyim-page-tooltip 'popup)
  (pyim-page-length 9)
  :config
  (pyim-isearch-mode 1)
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换速度 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  :bind
  ("M-j" . pyim-convert-string-at-point))
```


### Pyim BaseDict

[Pyim BaseDict](https://github.com/tumashu/pyim-basedict), the default Chinese-Pyim dictionary.

```emacs-lisp
(use-package pyim-basedict
  :after pyim
  :config (pyim-basedict-enable))
```


<a id="orgf4210ac"></a>

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


<a id="org7ea82c0"></a>

## Speed Type

[Speed type](https://github.com/hagleitn/speed-type), a game to practice touch/speed typing in Emacs.

```emacs-lisp
(use-package speed-type
  :defer t)
```


<a id="orge3d1a3d"></a>

## 2048 Game

[2048 Game](https://bitbucket.org/zck/2048.el), an implementation of 2048 in Emacs.

```emacs-lisp
(use-package 2048-game
  :defer t)
```


<a id="orgaeedb9b"></a>

## Zone

[Zone](https://www.emacswiki.org/emacs/ZoneMode), a minor-mode 'zones' Emacs out, choosing one of its random modes to obfuscate the current buffer.

```emacs-lisp
(use-package zone
  :ensure nil
  :config
  ;; (zone-when-idle 600) ; in seconds
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
