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

(use-package ido
  :ensure t
  :init (timeit
	 "IDO"
	 (ido-mode t)
	 (ido-everywhere t)
	 (setq ido-enable-flex-matching t)
	 (setq ido-file-extensions-order '(".py" ".js" ".emacs" t))
	 ))


;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(defun toggle-full-screen ()
  "Toggles full-screen mode for Emacs window on Win32."
  (interactive)
  (shell-command "emacs_fullscreen.exe"))

(defun toggle-bars ()
  "Toggles bars visibility."
  (interactive)
  (menu-bar-mode)
  (tool-bar-mode)
  (scroll-bar-mode))

(defun toggle-full-screen-and-bars ()
  "Toggles full-screen mode and bars."
  (interactive)
  (toggle-bars)
  (toggle-full-screen))

;; Use F11 to switch between windowed and full-screen modes
(global-set-key [f11] 'toggle-full-screen-and-bars)

;; Switch to full-screen mode during startup
(toggle-full-screen-and-bars)


(provide 'user)
;;; user.el ends here
