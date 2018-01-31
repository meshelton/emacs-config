- [Emacs Initialization File](#org8417548)
  - [Bootstrapping](#org36e3c29)
    - [[init.el](init.el)](#orgc60f28f)
    - [[README.org](README.md)](#org79dcd90)
  - [Configuration](#orgcb08104)
    - [Per System Configuration](#org0505952)
    - [Personal Information](#org67b1846)
    - [Backup](#orgb7eb07c)
    - [Google specific emacs packages](#orgc69f974)
    - [Appearance](#org89ca490)
    - [Multiple Cursors](#org4930541)
    - [Visual Regular Expressions](#org000cf9a)
    - [Magit](#org560ad68)
    - [Helm](#orga983076)
  - [s](#orgd515c37)
    - [make `(C-c C-l)` use file completion when `file:` is used](#orgc517963)
    - [figure out how to quickly reindent code blocks](#org72904c2)
    - [Figure out how to load org from straight before anything else](#org9caf568)
    - [remove straight :(](#org8d3b473)
    - [Debug why <s[TAB] quick expansion isn't working](#org44b46f1)



<a id="org8417548"></a>

# Emacs Initialization File


<a id="org36e3c29"></a>

## Bootstrapping

The endgoal here is to have an easily shareable, readable, and reproducable emacs setup. When you clone this repository you'll have two main files: [init.el](init.el) and [README.org](README.md).


<a id="orgc60f28f"></a>

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


<a id="org79dcd90"></a>

### [README.org](README.md)

This is where the main configuration goes. Any code blocks that have the `:tangle ./init.el` will be used to construct the final init.el file through the function `(org-babel-tangle)`. The initial processesing of [README.org](README.md) will be triggered by [init.el](init.el).

Then we load up a couple packages:

1.  [straight.el](https://github.com/raxod502/straight.el) :: Fancy package manager
2.  [use-package](https://github.com/jwiegley/use-package) :: A nicer package configuration framework
3.  [org-mode](https://orgmode.org/) :: A package for literate programming and so much more

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
(use-package org
  :load-path "straight/repos/org"
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


<a id="orgcb08104"></a>

## Configuration


<a id="org0505952"></a>

### Per System Configuration

```emacs-lisp
(defun system-is-workstation ()
  (interactive)
  "Return true if the system we are running is my work desktop at Google"
  (string-equal (system-name) "meshelton.nyc.corp.google.com"))
```


<a id="org67b1846"></a>

### Personal Information

```emacs-lisp
(setq user-full-name "Michael Shelton"
      user-mail-address "michael.e.shelton@gmail.com")
(if (system-is-workstation)
    (setq user-mail-address "meshelton@google.com"))
```


<a id="orgb7eb07c"></a>

### Backup

```emacs-lisp
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-by-copying t)
```


<a id="orgc69f974"></a>

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


<a id="org89ca490"></a>

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


<a id="org4930541"></a>

### Multiple Cursors

```emacs-lisp
(use-package multiple-cursors
  :bind (("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-^" . 'mc/mark-all-like-this)))

```


<a id="org000cf9a"></a>

### Visual Regular Expressions

```emacs-lisp
(use-package visual-regexp
  :bind (("C-M-r" . 'vr/replace)
         ("C-M-q" . 'vr/query-replace)))
(use-package visual-regexp-steroids
  :bind ("C-M-m" . 'vr/mc-mark))
```


<a id="org560ad68"></a>

### Magit

```emacs-lisp
(use-package magit
  :bind ("C-x g" . 'magit-status))
```


<a id="orga983076"></a>

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


<a id="orgd515c37"></a>

## TODO s


<a id="orgc517963"></a>

### TODO make `(C-c C-l)` use file completion when `file:` is used


<a id="org72904c2"></a>

### TODO figure out how to quickly reindent code blocks


<a id="org9caf568"></a>

### TODO Figure out how to load org from straight before anything else


<a id="org8d3b473"></a>

### TODO remove straight :(


<a id="org44b46f1"></a>

### TODO Debug why <s[TAB] quick expansion isn't working
