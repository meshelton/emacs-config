- [Emacs Initialization File](#org37d1be4)
  - [Bootstrapping](#orgd726142)
    - [[init.el](init.el)](#orgc2e551a)
    - [[README.org](README.md)](#org1f3f907)
  - [Configuration](#orgc0e0d0f)
    - [Per System Configuration](#orgf37050f)
    - [Personal Information](#orga19da4b)
    - [Backup](#orge151edc)
    - [Google specific emacs packages](#orga709477)
    - [Appearance](#org5774a8b)
    - [Multiple Cursors](#org2ec2bdf)
    - [Visual Regular Expressions](#org8a3c495)
    - [Magit](#org3a631a1)
    - [Helm](#orgdb3aaf4)
    - [sql-indent](#orgefe3895)
    - [Org Mode Tweaks](#org0716371)
  - [s](#org4842ebb)
    - [make `(C-c C-l)` use file completion when `file:` is used](#orgc28d331)
    - [figure out how to quickly reindent code blocks](#org921ca8b)
    - [remove straight :(](#org863ef95)
    - [Debug why <s[TAB] quick expansion isn't working](#org3bfc41b)
    - [Make a better SQL mode](#org438eeed)
    - [Figure out better way to load use-package the first time](#org9063d90)
    - [Find better way of storing github.com secret access token](#org2dd7279)



<a id="org37d1be4"></a>

# Emacs Initialization File


<a id="orgd726142"></a>

## Bootstrapping

The endgoal here is to have an easily shareable, readable, and reproducable emacs setup. When you clone this repository you'll have two main files: [init.el](init.el) and [README.org](README.md).


<a id="orgc2e551a"></a>

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


<a id="org1f3f907"></a>

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


<a id="orgc0e0d0f"></a>

## Configuration


<a id="orgf37050f"></a>

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


<a id="orga19da4b"></a>

### Personal Information

```emacs-lisp
(setq user-full-name "Michael Shelton"
      user-mail-address "michael.e.shelton@gmail.com")
(if (system-is-workstation)
    (setq user-mail-address "meshelton@google.com"))
```


<a id="orge151edc"></a>

### Backup

```emacs-lisp
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-by-copying t)
```


<a id="orga709477"></a>

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


<a id="org5774a8b"></a>

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


<a id="org2ec2bdf"></a>

### Multiple Cursors

```emacs-lisp
(use-package multiple-cursors
  :bind (("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-^" . 'mc/mark-all-like-this)))

```


<a id="org8a3c495"></a>

### Visual Regular Expressions

```emacs-lisp
(use-package visual-regexp
  :bind (("C-M-r" . 'vr/replace)
         ("C-M-q" . 'vr/query-replace)))
(use-package visual-regexp-steroids
  :bind ("C-M-m" . 'vr/mc-mark))
```


<a id="org3a631a1"></a>

### Magit

```emacs-lisp
(use-package magit
  :bind ("C-x g" . 'magit-status))
```


<a id="orgdb3aaf4"></a>

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


<a id="orgefe3895"></a>

### sql-indent

```emacs-lisp
(use-package sql-indent
  :hook (sql-mode-hook . sqlind-minor-mode))
```


<a id="org0716371"></a>

### Org Mode Tweaks

First we want to define where on the computer our google drive folder is located:

```emacs-lisp
(defvar google-drive-dir
  (cond ((system-is-home-desktop) "/mnt/c/Users/micha/Google Drive/Org/"))
  "The location of google drive on this system")

(setq initial-buffer-choice google-drive-dir)
```


<a id="org4842ebb"></a>

## TODO s


<a id="orgc28d331"></a>

### TODO make `(C-c C-l)` use file completion when `file:` is used


<a id="org921ca8b"></a>

### TODO figure out how to quickly reindent code blocks


<a id="org863ef95"></a>

### DONE remove straight :(


<a id="org3bfc41b"></a>

### TODO Debug why <s[TAB] quick expansion isn't working


<a id="org438eeed"></a>

### TODO Make a better SQL mode

[emacswiki link for modes](https://www.emacswiki.org/emacs/ModeTutorial) [remacs guide to creating a major mode](http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/)


<a id="org9063d90"></a>

### TODO Figure out better way to load use-package the first time


<a id="org2dd7279"></a>

### TODO Find better way of storing github.com secret access token