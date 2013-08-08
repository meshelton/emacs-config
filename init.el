;;;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

;;;; IMENU
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
 
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;;; Font
(when window-system
	(set-face-attribute 'default nil :font "Terminus:pixelsize=18:foundry=xos4:weight=normal:slant=normal:width=normal:spacing=110:scalable=false")
	)
;;;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")
;;;; Ido
(require 'ido)
(ido-mode t)
;;;; Scala-Mode-2
;(require 'scala-mode2)
;(add-to-list 'load-path "~/.emacs.d/ensime/elisp/")
;;;; Ensime
;(require 'ensime)
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;(setq scala-indent:align-parameters t)
;;;; Pyregexp
;(require 'pyregexp)
;(define-key global-map (kbd "C-c r") 'pyregexp-replace)
;(define-key global-map (kbd "C-c q") 'pyregexp-query-replace)
;;;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;;; Appearance
(load-theme 'wombat t)
(global-linum-mode 1)
(column-number-mode 1) 
(line-number-mode 1)
(electric-pair-mode 1)
(setq inhibit-startup-screen t)
(show-paren-mode t)

;;;; Backup
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;;; Functions
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

