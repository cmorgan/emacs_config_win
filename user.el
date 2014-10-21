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
(load-file "~/.emacs.d/desktop.el")
 
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


(use-package electric
  :init (timeit
	 "ELECTRIC"
	 ;; Ignoring electric indentation
	 (defun electric-indent-ignore-python (char)
	   "Ignore electric indentation for 'python-mode' after CHAR."
	   (if (equal major-mode 'python-mode)
	       `no-indent'
	     nil))
	 (electric-indent-mode t)
	 (add-hook 'electric-indent-functions
		   'electric-indent-ignore-python)))


;; python autocompletion
(use-package jedi
  :ensure t
  :commands jedi:setup
  :config (timeit
	   "JEDI"
	   (setq jedi:complete-on-dot t)
	   (setq jedi:tooltip-method nil)))

(eval-after-load "jedi"
    '(setq jedi:server-command (list "C:\\dev\\bin\\Anaconda\\envs\\emacs-jedi\\python" jedi:server-script)))


(use-package org
  :init (progn
	    (global-set-key (kbd "C-c o a") 'org-agenda)
	    (global-set-key (kbd "C-c o c") 'org-capture)
	    (global-set-key (kbd "C-c o l") 'org-store-link)
		(setq org-log-done t)
		))

(use-package paren
  :init (progn
	  (show-paren-mode)))


;; highlight whitespace
(use-package whitespace
  :ensure t
  :init (timeit
	 "WHITESPACE"
	 (hook-into-modes 'whitespace-mode '(python-mode-hook))
	 ;; Highlight portion of lines >79
	 (setq whitespace-line-column 79)
	 (setq whitespace-style '(face lines-tail))))

(use-package sql
  :init (progn
	  (add-hook 'sql-mode-hook
		    (lambda ()
		      (modify-syntax-entry ?\_ "w")))))

(use-package help-mode
  :init (progn
	  (add-hook 'help-mode-hook
		    (lambda ()
		      (evil-motion-state 0)))))

    
;; tools for managing files in a project
(use-package projectile
  :ensure t
  :init (timeit
	 "PROJECTILE"
	  (projectile-global-mode)
	  (define-key evil-normal-state-map "\C-p" 'projectile-find-file)))


;; turn off cruft
(use-package tool-bar
  :init (progn
	  (tool-bar-mode 0)))
(use-package scroll-bar
  :init (progn
	  (scroll-bar-mode 0)))
(use-package menu-bar
  :init (progn
	  (menu-bar-mode 0)))


;; highligh current line
(use-package hl-line
  :init (progn
	  (global-hl-line-mode)))


(use-package evil
  :ensure t
  :init (timeit
	 "EVIL"

	 (defun new-tab ()
	   "Open file in new tab."
	   (interactive)
	   (ido-find-file-other-frame))

	 (defun delete-tab ()
	   "Delete current tab."
	   (interactive)
	   (delete-frame))

	 (defun next-tab ()
	   "Switch to next tab."
	   (interactive)
	   (other-frame 1))

	 (defun previous-tab ()
	   "Switch to previous tab."
	   (interactive)
	   (other-frame -1))

	 (use-package evil-nerd-commenter
	   :ensure t)

	 (evil-mode t)
	 ;; (define-key evil-normal-state-map (kbd ",t") 'new-tab)
	 (define-key evil-normal-state-map (kbd "C-w t") 'new-tab)
	 (define-key evil-normal-state-map (kbd "C-w x") 'delete-tab)
	 ;(define-key evil-normal-state-map (kbd "lL") 'next-tab)
	 (define-key evil-normal-state-map (kbd "gT") 'previous-tab)
	 (define-key evil-normal-state-map (kbd ",gg") 'vc-git-grep)
	 (define-key evil-normal-state-map (kbd ",G") 'rgrep)
	 (define-key evil-normal-state-map (kbd ",m") 'menu-bar-mode)
	 (define-key evil-visual-state-map (kbd ",c") 'evilnc-comment-or-uncomment-lines)
	 ))

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
     (setq 
      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
      ;; ido-ignore-buffers ;; ignore these guys
      ;; '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
      ;;   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
      ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
      ido-case-fold  t                 ; be case-insensitive

      ido-enable-last-directory-history t ; remember last used dirs
      ido-max-work-directory-list 30   ; should be enough
      ido-max-work-file-list      50   ; remember many
      ido-use-filename-at-point nil    ; don't use filename at point (annoying)
      ido-use-url-at-point nil         ; don't use url at point (annoying)

      ido-enable-flex-matching nil     ; don't try to be too smart
      ido-max-prospects 8              ; don't spam my minibuffer
      ido-confirm-unique-completion t) ; wait for RET, even with unique completion

      ;; when using ido, the confirmation is rather annoying...
      (setq confirm-nonexistent-file-or-buffer nil)
      ))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config (progn
	    (setq indent-tabs-mode nil)
	    (setq evil-shift-width 2)
	    (setq tab-width 2)
        (setq auto-fill-mode -1)
        ))


(use-package starter-kit :ensure t)
  

;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; (autoload 'markdown-mode "markdown-mode"
;; 		     "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


(setq
 python-shell-interpreter "C:\\dev\\bin\\Anaconda\\python.exe"
 python-shell-interpreter-args "-i C:\\dev\\bin\\Anaconda\\Scripts\\ipython-script.py"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

;; keep visual mode active when indenting
(define-key evil-visual-state-map (kbd "<") (lambda ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore)))
(define-key evil-visual-state-map (kbd ">") (lambda ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore)))

;; this mode-hook is taken straight from the comments in autopair.el
(add-hook 'python-mode-hook
    (lambda()
        ;;(local-set-key (kbd "C-c l") 'hs-show-block)
        ;;(local-set-key (kbd "C-c h")  'hs-hide-block)
        (local-set-key (kbd "C-c h")    'hs-toggle-hiding)
        (local-set-key (kbd "C-c j")    'hs-hide-all)
        (local-set-key (kbd "C-c k")    'hs-show-all)
        (hs-minor-mode t)))


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))

(defun toggle-window-split-horiz-vert ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c s") 'toggle-window-split-horiz-vert)

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")

     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

           ;; magit stuff!!


(provide 'user)
;;; user.el ends here
