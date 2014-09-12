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


;;;; General Keybinds
(global-set-key (kbd "<f1>") 'compile)
;;;; Appearance
(load-theme 'wombat 1)
(global-linum-mode 0)
(column-number-mode 1) 
(line-number-mode 1)
(setq inhibit-startup-screen 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq tab-width 2) 

;;;; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
;;;; Manually managed stuff
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

;;;; IMENU
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;;; Font
(when window-system
  (set-face-attribute 'default nil :font "Terminus:pixelsize=16:foundry=xos4:weight=normal:slant=normal:width=normal:spacing=110:scalable=false")
  )

;;;; epc
;;(init-package 'epc)
;;;; webkit
;;(require 'webkit)
;;;; subword-mode
(add-hook 'prog-mode-hook 'subword-mode)
;;;; yafolding
(init-package 'yafolding)
(require 'yafolding)
(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))
(define-key yafolding-mode-map (kbd "<C-M-return>") 'yafolding-toggle-all)
(define-key yafolding-mode-map (kbd "<C-return>") 'yafolding-toggle-element)
;;;; Ido
(require 'ido)
(ido-mode t)
;;;; flx
(init-package 'flx)
(init-package 'flx-ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
;;;; ido-at-point
(init-package 'ido-at-point)
(require 'ido-at-point)
(ido-at-point-mode)
;;;;  ido-vertical-mode
(init-package 'ido-vertical-mode)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
;;;; ido-ubiquitous
(init-package 'ido-ubiquitous)
(setq ido-everywhere t)
;;;; smex
(init-package 'smex)
(require 'smex)
(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command 
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))
(define-key global-map (kbd "M-x") 'smex)
(define-key global-map (kbd "M-X") 'smex-major-mode-commands)
(define-key global-map (kbd "C-c C-c M-x") 'execute-extended-command)
;;;; ggtags
(init-package 'ggtags)
;;;; projectile
(init-package 'projectile)
(projectile-global-mode)
;;;; Scala-Mode-2
(init-package 'scala-mode2)
(setq scala-indent:step 2)
(setq scala-indent:align-parameters t)
(setq scala-indent:indent-value-expression t)
(setq scala-indent:align-forms t)
;;;; ensime
(init-package 'ensime)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq ensime-sem-high-enabled-p nil)
;;;; sbt-mode
(init-package 'sbt-mode)
;;;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;;;; Rainbow Mode
(init-package 'rainbow-mode)
(add-hook 'prog-mode-hook (lambda () (rainbow-mode 1)))
;;;; Lua Mode
(init-package 'lua-mode)
;;;; Markdown Mode
(init-package 'markdown-mode)
(init-package 'markdown-mode+)
;;;; GLSL Mode
(init-package 'glsl-mode)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;;;; es-lib
(init-package 'es-lib)
;;;; zeal-at-point
(init-package 'zeal-at-point)
(require 'zeal-at-point)
;;;; smartparens
(init-package 'smartparens)
(smartparens-global-mode t)
(require 'smartparens-config)
;;;; magit
(init-package 'magit)
;;;; visual-regexp
(init-package 'visual-regexp)
(init-package 'visual-regexp-steroids)
(require 'visual-regexp-steroids) 
(define-key global-map (kbd "C-c C-r") 'vr/replace)
(define-key global-map (kbd "C-c C-q") 'vr/query-replace)
(define-key global-map (kbd "C-c C-m") 'vr/mc-mark)
;;;; multiple-cursors
(init-package 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-^") 'mc/mark-all-like-this)
;;;; yasnippet
(init-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)
;; Used in yasnippet doc template for scala mode
(defun scala-mode-find-clstrtobj-name-doc ()
  (save-excursion
    (if (re-search-forward "\\(class\\|object\\|trait\\)[ \t\n]+\\([a-zA-Z0-9_:=]+\\)[ \t\n]*" nil t)
	
        (buffer-substring (match-beginning 2) (match-end 2))
      "NONAME")))
(defun scala-mode-def-and-args-doc ()
  (save-excursion
    (if (re-search-forward
	 (concat
	  ;; function name
	  "[ \t\n]*def[ \t\n]+\\([a-zA-Z0-9_:=]+\\)[ \t\n]*"
          
	  ;; arguments
	  "\\((\\([a-zA-Z0-9_:* \t\n]*\\))\\)?"
	  ) nil t)

	;; TODO: output args in a sane format to use in yasnippet, look at doxymancs line 1441 
	(let* ((func (buffer-substring (match-beginning 1) (match-end 1)))
                                        ;(args (buffer-substring (match-beginning 3) (match-end 3)))
	       )
	  (concat "${1:" func "} $0"))
      "${1:name} $0")))

;;;; hasekll-mode
(init-package 'haskell-mode)
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
;;;; xkcd
(init-package 'xkcd)
;;;; flycheck
(init-package 'flycheck)
;;;; ag
(init-package 'ag)
;;;; company
(init-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
;;;; web-mode
(init-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;;; org
(init-package 'org)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-default-notes-file "~/Notes/notes.org")
(setq org-return-follows-link t)
(setq org-log-done 'note)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c b") 'org-iswitchb)
(setq org-link-abbrev-alist 
      '( ("google"    . "http://www.google.com/search?q=")
         ("gmap"      . "http://maps.google.com/maps?q=%s")))
(setq org-capture-templates 
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))
(setq org-export-backends '(md html ascii icalender latex))
;;;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")
(add-to-list 'tramp-default-method-alist '("127.0.0.1" "michael" "scp"))
;;;; cider
(init-package 'cider)
;;;; erc
(setq erc-echo-notices-in-minibuffer-flag t)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;;; Settings
(setq visible-bell t)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

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
