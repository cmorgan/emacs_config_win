(package-initialize)
(evil-mode 1)
(setq evil-default-cursor t)
(desktop-save-mode 0)
(global-auto-revert-mode t)
(setq auto-save-default nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq ido-use-filename-at-point nil)
(setq make-backup-files nil) 

(setq exec-path (append exec-path '("C:\\Users\\cmorgan\\AppData\\Local\\Programs\\Git\\bin")))


;; Set the number to the number of columns to use.
(setq-default fill-column 79)

(load-file "~/.emacs.d/util.el")

(global-set-key "\C-x\C-b" 'buffer-menu)


(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; Repos
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; Packages
(require 'package)
(require 'use-package)


(use-package magit
  :ensure t
  :config (progn
	    (define-key evil-normal-state-map (kbd ",gb") 'magit-blame-mode)
	    (global-set-key (kbd "C-x G") 'magit-status)
        ))

(use-package ido
  :ensure t
  :init (timeit
	 "IDO"
	 (ido-mode t)
	 (ido-everywhere t)
	 (setq ido-enable-flex-matching t)
	 (setq ido-file-extensions-order '(".py" ".js" ".emacs" t))
	 ))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (progn
	    (setq indent-tabs-mode nil)
	    (setq evil-shift-width 2)
	    (setq tab-width 2)))


;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)


(defun bars-off ()
  "Toggles barsvisibility."
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(bars-off)

(autoload 'markdown-mode "markdown-mode"
		     "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))



(provide 'user)
;;; user.el ends here
