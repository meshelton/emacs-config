- [Emacs Initialization File](#orgf6009e2)
  - [make `(C-c C-l)` use file completion when `file:` is used](#org4bb6101)
  - [figure out how to quickly reindent code blocks](#org7d9ff16)
  - [Bootstrapping](#org9aa8887)
    - [[init.el](init.el)](#org8b70644)
    - [[README.org](README.md)](#orgce4e807)
      - [Fix `use-package` weirdness with `org-mode`](#orgffc891f)
      - [Add hook to automatically generate readme on save](#orgbac4986)
  - [Configuration](#orgd6428b5)



<a id="orgf6009e2"></a>

# Emacs Initialization File



<a id="org4bb6101"></a>

## TODO make `(C-c C-l)` use file completion when `file:` is used


<a id="org7d9ff16"></a>

## TODO figure out how to quickly reindent code blocks


<a id="org9aa8887"></a>

## Bootstrapping

The endgoal here is to have an easily shareable, readable, and reproducable emacs setup. When you clone this repository you'll have two main files: [init.el](init.el) and [README.org](README.md).


<a id="org8b70644"></a>

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


<a id="orgce4e807"></a>

### [README.org](README.md)

This is where the main configuration goes. Any code blocks that have the `:tangle yes` will be used to construct the final init.el file through the function `(org-babel-tangle)`. The initial processesing of [README.org](README.md) will be triggered by [init.el](init.el). Subsequent init.el generations are through this `after-save-hook` on [README.org](README.md)

```emacs-lisp
(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "README.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle "init.el")
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)
```

We then load up a couple packages:

1.  Fancy package manager [straight.el](https://github.com/raxod502/straight.el)
2.  A nicer package configuration framework [use-package](https://github.com/jwiegley/use-package)
3.  An updated version of [org-mode](https://orgmode.org/)

```emacs-lisp
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t)
(require 'bind-key)
(use-package org)
```


<a id="orgffc891f"></a>

#### TODO Fix `use-package` weirdness with `org-mode`


<a id="orgbac4986"></a>

#### TODO Add hook to automatically generate readme on save

```emacs-lisp
(require 'ox-md)
(use-package ox-gfm)
```


<a id="orgd6428b5"></a>

## Configuration

Defining functions for computer specific configuration

```emacs-lisp
(defun system-is-workstation ()
  (interactive)
  "Return true if the system we are running is my work desktop at Google"
  (string-equal (system-name) "meshelton.nyc.corp.google.com")
  )
```

Personal Information

```emacs-lisp
(setq user-full-name "Michael Shelton"
      user-mail-address "michael.e.shelton@gmail.com")
(if (system-is-workstation)
    (setq user-mail-address "meshelton@google.com")
  )
```

Backup

```emacs-lisp
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-by-copying t)
```

Google specific emacs packages

```emacs-lisp
(if (system-is-workstation)
    (require 'google)
  (require 'p4-google)                ;; g4-annotate, improves find-file-at-point
  (require 'compilation-colorization) ;; colorizes output of (i)grep
  (require 'rotate-clients)           ;; google-rotate-client
  (require 'rotate-among-files)       ;; google-rotate-among-files
  (require 'googlemenu)               ;; handy Google menu bar
  (require 'p4-files)                 ;; transparent support for Perforce filesystem
  (require 'google3-build)            ;; support for blaze builds
  (require 'csearch)                  ;; Search the whole Google code base.
  )
```

Appearance

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

Multiple Cursors

```emacs-lisp
(use-package multiple-cursors
  :bind (("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-^" . 'mc/mark-all-like-this)))

```

Visual Regular Expressions

```emacs-lisp
(use-package visual-regexp
  :bind (("C-c C-r" . 'vr/replace)
         ("C-c C-q" . 'vr/query-replace)))
(use-package visual-regexp-steroids
  :bind ("C-c C-m" . 'vr/mc-mark))
```

Magit

```emacs-lisp
(use-package magit)
```
