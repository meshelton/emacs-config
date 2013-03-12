(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))


(require 'ido)
(ido-mode t)

(require 'scala-mode2)
(add-to-list 'load-path "~/.emacs.d/ensime/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq scala-indent:align-parameters t)

(load-theme 'wombat t)

(global-linum-mode 1)
(column-number-mode 1) 
(line-number-mode 1)

(setq inhibit-startup-screen t)

(show-paren-mode t)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
