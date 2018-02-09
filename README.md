- [Emacs Initialization File](#org871a90e)
  - [Bootstrapping](#org4e47def)
    - [[init.el](init.el)](#org0a0f8e2)
    - [[README.org](README.md)](#orgf88f30b)
  - [Configuration](#orgee31601)
    - [Per System Configuration](#orgc2638f2)
    - [Personal Information](#org472fd91)
    - [Backup](#org274e32a)
    - [Google specific emacs packages](#orgd8ad416)
    - [Appearance](#orgdf5b777)
    - [Multiple Cursors](#org4fbd92c)
    - [Visual Regular Expressions](#org683dd77)
    - [Magit](#orgc9580d0)
    - [Helm](#orgd226405)
    - [sql-indent](#orga2297a6)
    - [Org Mode Tweaks](#orgcbdcce2)
  - [s](#org01a8b79)
    - [make `(C-c C-l)` use file completion when `file:` is used](#org9135cd5)
    - [figure out how to quickly reindent code blocks](#orgb259713)
    - [remove straight :(](#org7c58738)
    - [Debug why <s[TAB] quick expansion isn't working](#orgab344ce)
    - [Make a better SQL mode](#orgdc2a947)
    - [Figure out better way to load use-package the first time](#orgbd74a51)
    - [Find better way of storing github.com secret access token](#org0f69edc)



<a id="org871a90e"></a>

# Emacs Initialization File


<a id="org4e47def"></a>

## Bootstrapping

The endgoal here is to have an easily shareable, readable, and reproducable emacs setup. When you clone this repository you'll have two main files: [init.el](init.el) and [README.org](README.md).


<a id="org0a0f8e2"></a>

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


<a id="orgf88f30b"></a>

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


<a id="orgee31601"></a>

## Configuration


<a id="orgc2638f2"></a>

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
```


<a id="org472fd91"></a>

### Personal Information

```emacs-lisp
(setq user-full-name "Michael Shelton"
      user-mail-address "michael.e.shelton@gmail.com")
(if (system-is-workstation)
    (setq user-mail-address "meshelton@google.com"))
```


<a id="org274e32a"></a>

### Backup

```emacs-lisp
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-by-copying t)
```


<a id="orgd8ad416"></a>

### Google specific emacs packages

```emacs-lisp
(when (system-is-workstation)
  (require 'google)
  (require 'p4-google)                ;; g4-annotate, improves find-file-at-point
  (require 'compilation-colorization) ;; colorizes output of (i)grep
  (require 'rotate-clients)           ;; google-rotate-client
  (require 'rotate-among-files)       ;; google-rotate-among-files
  (require 'googlemenu)               ;; handy Google menu bar
  (require 'p4-files)                 ;; transparent support for Perforce filesystem
  (require 'google3-build)            ;; support for blaze builds
  (require 'csearch)                  ;; Search the whole Google code base.
  (require 'sql-dremel))              ;; run dremel queries through emacs

```


<a id="orgdf5b777"></a>

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


<a id="org4fbd92c"></a>

### Multiple Cursors

```emacs-lisp
(use-package multiple-cursors
  :bind (("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-^" . 'mc/mark-all-like-this)))

```


<a id="org683dd77"></a>

### Visual Regular Expressions

```emacs-lisp
(use-package visual-regexp
  :bind (("C-M-r" . 'vr/replace)
         ("C-M-q" . 'vr/query-replace)))
(use-package visual-regexp-steroids
  :bind ("C-M-m" . 'vr/mc-mark))
```


<a id="orgc9580d0"></a>

### Magit

```emacs-lisp
(use-package magit
  :bind ("C-x g" . 'magit-status))
```


<a id="orgd226405"></a>

### Helm

```emacs-lisp
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds))) 
```


<a id="orga2297a6"></a>

### sql-indent

```emacs-lisp
(use-package sql-indent
  :hook (sql-mode-hook . sqlind-minor-mode))
```


<a id="orgcbdcce2"></a>

### Org Mode Tweaks

First we set up org to use our google drive as a working directory

```emacs-lisp
(defvar google-drive-dir
  (cond ((system-is-home-desktop) "/mnt/c/Users/micha/Google Drive/"))
  "The location of google drive on this system")

(setq org-directory (concat google-drive-dir "Org/"))

(setq initial-buffer-choice org-directory)

(setq org-agenda-files (list org-directory))
```


<a id="org01a8b79"></a>

## TODO s


<a id="org9135cd5"></a>

### TODO make `(C-c C-l)` use file completion when `file:` is used


<a id="orgb259713"></a>

### TODO figure out how to quickly reindent code blocks


<a id="org7c58738"></a>

### DONE remove straight :(


<a id="orgab344ce"></a>

### TODO Debug why <s[TAB] quick expansion isn't working


<a id="orgdc2a947"></a>

### TODO Make a better SQL mode

[emacswiki link for modes](https://www.emacswiki.org/emacs/ModeTutorial) [remacs guide to creating a major mode](http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/)


<a id="orgbd74a51"></a>

### TODO Figure out better way to load use-package the first time


<a id="org0f69edc"></a>

### TODO Find better way of storing github.com secret access token