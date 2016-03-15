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
(global-set-key (kbd "<f1>") 'projectile-compile-project)
;;;; Appearance
(load-theme 'wombat 1)
(global-linum-mode 0)
(column-number-mode 1) 
(line-number-mode 1)
(setq inhibit-startup-screen 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq compilation-scroll-output 'first-error)

;;;; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("SC" . "http://joseito.republika.pl/sunrise-commander/")))
(package-initialize)
;;;; Manually managed elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

;;;; exec-path-from-shell
;; https://github.com/purcell/exec-path-from-shell
(init-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;;; Font
(when window-system
  (set-face-attribute 'default nil :font "Terminus:pixelsize=16:foundry=xos4:weight=normal:slant=normal:width=normal:spacing=110:scalable=false"))
;;;; hydra
;; https://github.com/abo-abo/hydra
(init-package 'hydra)
(defhydra hydra-error (global-map "M-g")
  "goto-error"
  ("h" first-error "first")
  ("n" next-error "next")
  ("p" previous-error "prev")
  ("l" recenter-top-bottom "recenter")
  ("q" nil "quit"))
(defhydra hydra-zoom (global-map "<f9>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("q" nil "quit"))
;;;; discover
;; https://github.com/mickeynp/discover.el
(init-package 'discover)
(global-discover-mode 1)
;;;; company
;; http://company-mode.github.io/
(init-package 'company)
(add-hook 'after-init-hook 'global-company-mode)     
                                        ; bigger popup window
(setq company-tooltip-limit 20)                      
                                        ; align annotations to the right tooltip border
(setq company-tooltip-align-annotations 't)          
                                        ; decrease delay before autocompletion popup shows
(setq company-idle-delay .3)                         
                                        ; start autocompletion only after typing
(setq company-begin-commands '(self-insert-command)) 
                                        ; Force complete file names on "C-c /" key
(global-set-key (kbd "C-c /") 'company-files)        
;;;; ycmd
;; https://github.com/abingham/emacs-ycmd
(init-package 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)
(set-variable 'ycmd-server-command '("python2" "/opt/ycmd/ycmd"))
;;;; company-ycmd
;; https://github.com/abingham/emacs-ycmd
(init-package 'company-ycmd)
(company-ycmd-setup)
;;;; ace-jump-mode
;; https://github.com/winterTTr/ace-jump-mode
(init-package 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;;; ace-window
;; https://github.com/abo-abo/ace-window
(init-package 'ace-window)
(define-key (current-global-map) [remap other-window] 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;;; subword-mode
(add-hook 'prog-mode-hook 'subword-mode)
;;;; Ido
(require 'ido)
(ido-mode t)
(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match) 
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook #'bind-ido-keys)
;;;; flx
;; https://github.com/lewang/flx
(init-package 'flx-ido)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
;;;;  ido-vertical-mode
;; https://github.com/creichert/ido-vertical-mode.el
(init-package 'ido-vertical-mode)
(ido-vertical-mode 1)
;;;; ido-ubiquitous
;; https://github.com/DarwinAwardWinner/ido-ubiquitous
(init-package 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
;;;; smex
;; https://github.com/nonsequitur/smex
(init-package 'smex)
(smex-initialize) 
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
;;;; projectile
;; https://github.com/bbatsov/projectile
(init-package 'projectile)
(projectile-global-mode)
;;;; string-inflection
;;https://github.com/akicho8/string-inflection
(init-package 'string-inflection)
(require 'string-inflection)
(global-set-key (kbd "C-c C-i") 'string-inflection-cycle)
;;;; less-css-mode
(init-package 'less-css-mode)
;;;; company-web
;; https://github.com/osv/company-web
(init-package 'company-web)
(require 'company-web-html)
;;;; emacs-eclim
;; https://github.com/senny/emacs-eclim
(init-package 'emacs-eclim)
(require 'eclimd)
(global-eclim-mode)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
;;;; rust-mode
(init-package 'rust-mode)
;;;; toml-mode
(init-package 'toml-mode)
(require 'toml-mode)
;;;; yaml-mode
(init-package 'yaml-mode)
;;;; go-mode
(init-package 'go-mode)
;;;; company-go
(init-package 'company-go)
(require 'company-go)
;;;; go config
(setenv "GOPATH" "/home/michael/go")
(add-to-list 'exec-path "/home/michael/go/bin")
(defun my-go-mode-hook ()
                                        ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
                                        ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
                                        ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (set (make-local-variable 'company-backends) '(company-go))
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)
;;;; golint
(init-package 'golint)
;;;; csharp-mode
(init-package 'csharp-mode)
;;;; fsharp-mode
(init-package 'fsharp-mode)
;;;; Scala-Mode-2
(init-package 'scala-mode2)
(setq scala-indent:step 2)
(setq scala-indent:align-parameters t)
(setq scala-indent:indent-value-expression t)
(setq scala-indent:align-forms t)
;;;; javap-mode
(init-package 'javap-mode)
;;;; groovy-mode
(init-package 'groovy-mode)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
;;;; ensime
(init-package 'ensime)
(require 'ensime)
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq ensime-sem-high-enabled-p nil)
;;;; sbt-mode
(init-package 'sbt-mode)
;;;; gradle-mode
(init-package 'gradle-mode)
(setq gradle-use-gradlew 1)
;;;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;;;; Rainbow Mode
(init-package 'rainbow-mode)
(add-hook 'prog-mode-hook (lambda () (rainbow-mode 1)))
;;;; image+
(init-package 'image+)
(eval-after-load 'image '(require 'image+))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(magit-diff-arguments (quote ("--no-ext-diff" "--stat")))
 '(magit-fetch-arguments (quote ("--prune")))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(markdown-command "marked"))
(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
                                  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
;;;; hi2
(init-package 'hi2)
(add-hook 'haskell-mode-hook 'turn-on-hi2)
;;;; ghc
(init-package 'ghc)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook '(lambda ()
                                (local-set-key (kbd "RET") 'newline)))
;;;; symon
(init-package 'symon)
                                        ;(symon-mode)
;;;; xkcd
(init-package 'xkcd)
;;;; flycheck
(init-package 'flycheck)
;;;; ag
(init-package 'ag)
;;;; json-mode
(init-package 'json-mode)
;;;; web-mode
(init-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;;; tide
(init-package 'tide)
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (eldoc-mode +1)
            (company-mode-on)))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (eldoc-mode +1)
              (company-mode-on))))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
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
(setq erc-nick "meshelton")
;;;; 2048-game
(init-package '2048-game)
;;;; restclient
(init-package 'restclient)
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
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun sort-lines-by-length (b e)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region b e)
      (let ((items (sort 
                    (split-string 
                     (buffer-substring (point-min) (point-max)) "[\n]")
                    (lambda(x y) (< (length x) (length y)))))
            )
        (delete-region (point-min) (point-max))
        (save-excursion
          (point-min)
          (insert (apply 'concat (map 'list (lambda (x) (format "%s\n" x)) items))))))))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))
