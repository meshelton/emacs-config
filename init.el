;;;; Package initializer
(defun init-package (package-name &optional first-time-setup)
"Installs a package from the package repository if it is not already installed"
(unless (package-installed-p package-name)
  ;; Install the library and do any first time setup
  (condition-case nil
  (package-install package-name)
    (error
     (package-refresh-contents)
     (package-install package-name)
     ))
  first-time-setup
  t)
t)

;;;; Appearance
(load-theme 'wombat 1)
(global-linum-mode 0)
(column-number-mode 1) 
(line-number-mode 1)
(setq inhibit-startup-screen 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq tab-width 4) 

;;;; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;;;; IMENU
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;;; Font
(when window-system
	(set-face-attribute 'default nil :font "Terminus:pixelsize=16:foundry=xos4:weight=normal:slant=normal:width=normal:spacing=110:scalable=false")
	)

;;;; CEDET
;; Load CEDET.
(load-file "~/.emacs.d/cedet-1.1/common/cedet.el")
;; Enable EDE (Project Management) features
(global-ede-mode 1)
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
;;;; ECB
(add-to-list 'load-path "~/.emacs.d/ecb-master")
(require 'ecb)
(setq ecb-options-version "2.40")
;;;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")
;;;; Ido
(require 'ido)
(ido-mode t)
;;;; Scala-Mode-2
(init-package 'scala-mode2)
(setq scala-indent:step 4)
;;;; Ensime
(add-to-list 'load-path "~/.emacs.d/ensime/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq scala-indent:align-parameters t)
;;;; Scamacs
(add-to-list 'load-path "~/.emacs.d/scamacs-master")
;(require 'ensime-ecb)
;;;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;;;; Rainbow Mode
(init-package 'rainbow-mode)
;;;; Lua Mode
(init-package 'lua-mode)
;;;; es-lib
(init-package 'es-lib)
;;;; zeal-at-point
(init-package 'zeal-at-point)
(require 'zeal-at-point)
;;;; Project Explorer
(add-to-list 'load-path "~/.emacs.d/project-explorer")
(require 'project-explorer)
;;;; iedit
(add-to-list 'load-path "~/.emacs.d/iedit")
(require 'iedit)
;;;; smartparens
(init-package 'smartparens)
(smartparens-global-mode t)
(require 'smartparens-config)
;;;; git-gutter
(init-package 'git-gutter)
(global-git-gutter-mode t)
;;;; visual-regexp
(init-package 'visual-regexp)
(require 'visual-regexp) 
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)


;;;; Settings
(setq ring-bell-function 'ignore)

;;;; Auto modes
(add-to-list 'auto-mode-alist '("\\.ic$" . java-mode))

;;;; Backup
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ecb-options-version "2.40"))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
