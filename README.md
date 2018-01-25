- [Emacs Initialization File](#org983f098)
  - [Bootstrapping a literate configuration file](#org62bcc61)
    - [Requirements](#orgd471a62)



<a id="org983f098"></a>

# Emacs Initialization File

todo: need to add better complete support


<a id="org62bcc61"></a>

## Bootstrapping a literate configuration file

The endgoal here is to have an easily shareable, readable, and reproducable emacs setup


<a id="orgd471a62"></a>

### Requirements

1.  emacs that is bundled with org-mode v7.0.0

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

```emacs-lisp
(require 'ox-md)
(use-package ox-gfm)
```

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

```emacs-lisp
(use-package magit)
```
