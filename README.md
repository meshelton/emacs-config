- [Emacs Initialization File](#orga471eeb)
  - [Bootstrapping](#orgaf064ac)
    - [[init.el](init.el)](#org3a8a9e8)
    - [[README.org](README.md)](#orgf50da4b)
  - [Configuration](#orgc83075d)
    - [Per System Configuration](#orgde78b47)
    - [Personal Information](#org324c803)
    - [Backup](#org71bb7e0)
    - [Appearance](#org104f571)
    - [Multiple Cursors](#org031a121)
    - [Visual Regular Expressions](#org1c9ceb4)
    - [Magit](#org392be84)
    - [string-inflection](#org7ac9f0c)
  - [TODOs](#org4234c66)



<a id="orga471eeb"></a>

# Emacs Initialization File


<a id="orgaf064ac"></a>

## Bootstrapping

The endgoal here is to have an easily shareable, readable, and reproducable emacs setup. When you clone this repository you'll have two main files: [init.el](init.el) and [README.org](README.md).


<a id="org3a8a9e8"></a>

### [init.el](init.el)

This is the entry point to the entire configuration process. When you first clone this repo [init.el](init.el) should look like this:

```emacs-lisp
;;; .emacs --- Emacs initialization file -*- lexical-binding: t; -*-

;; This file replaces itself with the actual configuration at first run.

;; We can't tangle without org!
(require 'org)
;; Open the configuration
(find-file (concat user-emacs-directory "README.org"))
;; tangle it
(org-babel-tangle)
;; load it
(load-file (concat user-emacs-directory "init.el"))
;; finally byte-compile it
(byte-compile-file (concat user-emacs-directory "init.el"))
```

This code will load org mode, move specified code blocks from [README.org](README.md) to [init.el](init.el) and then byte compile it.


<a id="orgf50da4b"></a>

### [README.org](README.md)

This is where the main configuration goes. Any code blocks that have the `:tangle ./init.el` will be used to construct the final init.el file through the function `(org-babel-tangle)`. The initial processesing of [README.org](README.md) will be triggered by [init.el](init.el).

Then we load up a couple packages:

1.  [use-package](https://github.com/jwiegley/use-package) :: A nicer package configuration framework
2.  [org-mode](https://orgmode.org/) :: A package for literate programming and so much more

```emacs-lisp
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package org
             :bind ("C-c l" . 'org-store-link))
```

Subsequent init.el generations are through this `after-save-hook` on [README.org](README.md)

```emacs-lisp
(defun tangle-init ()
  "If the current buffer is 'README.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "README.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)
```

We also add an after save hook to automatically generate a new [README.md](README.md)

```emacs-lisp
(require 'ox-md)
(use-package ox-gfm)
(defun export-readme ()
  "If the current buffer is 'README.org' export it to 'README.md'"
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "README.org")))
    (org-gfm-export-to-markdown)))
(add-hook 'after-save-hook 'export-readme)
```


<a id="orgc83075d"></a>

## Configuration


<a id="orgde78b47"></a>

### Per System Configuration

```emacs-lisp
(defun system-is-workstation ()
  (interactive)
  "Return true if the system we are running is my work desktop at Google"
  (string-equal (system-name) "meshelton.nyc.corp.google.com"))
(defun system-is-home-desktop ()
  (interactive)
  "Return true if the system we are running is my personal desktop"
  (string-equal (system-name) "DESKTOP-C2EK1OP"))
(defun system-is-home-desktop-wsl ()
  (interactive)
  "Return true if the system we are running is my personal desktop"
  (string-equal (system-name) "DESKTOP-6PNIEF5"))
(defun system-is-indify-laptop ()
  (interactive)
  "Return true if the system we are running is my indify laptop"
  (string-equal (system-name) "MacBook-Pro-3.local"))
```


<a id="org324c803"></a>

### Personal Information

```emacs-lisp
(setq user-full-name "Michael Shelton"
      user-mail-address "michael.e.shelton@gmail.com")
(if (system-is-indify-laptop)
    (setq user-mail-address "michael@indify.io"))
```


<a id="org71bb7e0"></a>

### Backup

```emacs-lisp
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-by-copying t)
```


<a id="org104f571"></a>

### Appearance

```emacs-lisp
(load-theme 'wombat 1)
(global-linum-mode 0)
(column-number-mode 1)
(line-number-mode 1)
(setq inhibit-startup-screen 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq compilation-scroll-output 'first-error)
```


<a id="org031a121"></a>

### Multiple Cursors

```emacs-lisp
(use-package multiple-cursors
  :bind (("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-^" . 'mc/mark-all-like-this)))

```


<a id="org1c9ceb4"></a>

### Visual Regular Expressions

```emacs-lisp
(use-package visual-regexp
  :bind (("C-M-r" . 'vr/replace)
         ("C-M-q" . 'vr/query-replace)))
(use-package visual-regexp-steroids
  :bind ("C-M-m" . 'vr/mc-mark))
```


<a id="org392be84"></a>

### Magit

```emacs-lisp
(use-package magit
  :bind ("C-x g" . 'magit-status))

```


<a id="org7ac9f0c"></a>

### string-inflection

```emacs-lisp
(use-package string-inflection
  :bind ("C-c i" . 'string-inflection-cycle))
```


<a id="org4234c66"></a>

## TODOs