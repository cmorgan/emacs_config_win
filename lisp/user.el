(package-initialize)
;;(elpy-enable)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))
(setq org-src-fontify-natively t)

(evil-mode 1)
(setq evil-want-fine-undo 'fine)
(setq evil-default-cursor t)
(global-auto-revert-mode t)
(setq auto-save-default nil)
(blink-cursor-mode -1) 
;(setq grep-program "\"C:/dev/bin/cygwin/bin/grep.exe\"")
;(setq find-program "\"C:/dev/bin/cygwin/bin/find.exe\"")
;(setq image-dired-external-viewer "C:\\Program Files (x86)\\IrfanView\\i_view32.exe")
;(setq exec-path (append exec-path '("C:\\Users\\cmorgan\\AppData\\Local\\Programs\\Git\\bin")))
;;(pdf-tools-install)
(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "C:\\Program Files (x86)\\Adobe\\Reader 11.0\\Reader\\AcroRd32.exe" (file))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq ido-use-filename-at-point nil)
;;(setq make-backup-files nil) 
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(server-start)
;; (setq max-specpdl-size 32000)
;; (require 'pytest)
;;(add-to-list 'pytest-project-names "C:\\dev\\bin\\Anaconda\\Scripts\\py.test.exe")
;;(plist-put org-format-latex-options :scale 1.5)


;; Set the number to the number of columns to use.
(setq-default fill-column 79)

(global-set-key "\C-x\C-b" 'buffer-menu)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Smex
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")

(global-set-key (kbd "M-x") 'smex)

;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
	ad-do-it))
(ad-activate 'grep-compute-defaults)

;; Packages
(require 'my-python)
(require 'themes)
(require 'package)
(require 'use-package)
(use-package starter-kit :ensure t)
(require 'util)
(require 'desktop)
;;(require 'python-docstring)
(require 'ox-gfm)


;;(warning-minimum-level :error) 
(setq warning-minimum-level :emergency)

(use-package flycheck
  :ensure t
  :init (timeit
	 "FLYCHECK"
	 (add-hook 'after-init-hook #'global-flycheck-mode)
	 (setq flycheck-highlighting-mode 'lines)
	 (setq flycheck-ghc-language-extensions ())
	 (setq python-check-function "flake8")
	 (flycheck-define-checker javascript-flow
	   "A JavaScript syntax and style checker using Flow. 
See URL `http://flowtype.org/'."
	   :command ("flow" source-original)
	   :error-patterns
	   ((error line-start
	   	   (file-name)
	   	   ":"
	   	   line
	   	   ":"
	   	   (minimal-match (one-or-more (not (any ":"))))
	   	   ": "
	   	   (message (minimal-match (and (one-or-more anything) "\n")))
	   	   line-end))
	   :modes js-mode)
	 (add-to-list 'flycheck-checkers 'javascript-flow t)
	 (flycheck-add-next-checker 'javascript-gjslint 'javascript-flow)))


;; (add-hook 'image-mode-hook 'eimp-mode)


(use-package web-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
    (add-hook 'webmode-mode-hook '(auto-fill-mode -1))

    ))


(add-hook 'html-mode-hook 'turn-off-auto-fill)


(use-package evil-escape
  :init
  (evil-escape-mode 1))


(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 150)
  (setq recentf-max-menu-items 150)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  (global-set-key (kbd "C-x C-r") 'ido-recentf-open)

  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
)


;; turn off syntax highlight and 79 char highlight when reading
(defun read-code-mode ()
    (interactive)
    (flycheck-mode -1)
    (whitespace-mode -1)
    )

(defun code-mode ()
    (interactive)
    (flycheck-mode 1)
    (whitespace-mode 1)
    )

(use-package iedit
  :init
  (defun quit-iedit-mode ()
    "Turn off iedit-mode."
    (interactive)
    (iedit-mode -1)
    (evil-mode 1))
  (define-key iedit-mode-keymap (kbd "RET") 'quit-iedit-mode)
  (bind-key "C-c e" 'iedit-mode)
   ;;http://stackoverflow.com/questions/13051632/emacs-efficient-buffer-switchi
   ;;(global-set-key (kbd "C-x o") 'next-multiframe-window)
  )


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init (progn
	  (defun python-insert-breakpoint ()
	    "Insert Python breakpoint above point."
	    (interactive)
	    (evil-open-above 1)
	    (insert "import pdb; pdb.set_trace()  # BREAKPOINT")
	    (evil-normal-state))

        ;; Remove trailing whitespace manually by typing C-t C-w.
        (add-hook 'python-mode-hook
                  (lambda ()
                    (local-set-key (kbd "C-t C-w")
                                   'delete-trailing-whitespace)))

        ;; Automatically remove trailing whitespace when file is saved.
        (add-hook 'python-mode-hook
              (lambda()
                (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))
      )

  :config (timeit
	 "PYTHON"
	  (add-hook 'python-mode-hook
		    (lambda ()
		      ;; Underscore part of word in Python
		      (modify-syntax-entry ?\_ "w" python-mode-syntax-table)
		      ;; Autocompletion
		      ;(jedi:setup)
		      ;; Keybidings
		      (define-key evil-normal-state-map (kbd ",b") 'python-insert-breakpoint)

              (define-key evil-normal-state-map (kbd ",t") 'python-pytest-buffer)
		      ;; Enter key executes newline-and-indent
		      (local-set-key (kbd "RET") 'newline-and-indent)))

   
    (defun python-current-function ()
           (save-excursion
             (end-of-line)
             (beginning-of-defun)
             (search-forward-regexp " *def \\(\\w+\\)")
             (message (match-string-no-properties 1))))

    (defun python-pytest-buffer ()
        (interactive)
        (async-shell-command
        ;;(start-process "tests" "test"
            (concat 
            "nosetests -s"
            (buffer-file-name)) "test" ))


    ;; don't switch focus to a async window
    (defadvice async-shell-command (around hide-async-windows activate)
       (save-window-excursion
          ad-do-it))

    ))


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

;;enable jedi autocompletion in python
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; python autocompletion
;; (use-package jedi
;;   :ensure t
;;   :commands jedi:setup
;;   :config (timeit
;; 	   "JEDI"
;; 	   (setq jedi:complete-on-dot t)
;; 	   (setq jedi:tooltip-method nil)))

;; (eval-after-load "jedi"
;;     '(setq jedi:server-command (list "C:\\dev\\bin\\Anaconda\\envs\\emacs-jedi\\python" jedi:server-script)))


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
	  ;;(define-key evil-normal-state-map "\C-p" 'projectile-find-file)
    )
    )


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


(use-package evil-leader
  :ensure t
  :init 
  (evil-leader/set-leader ",")
  (evil-leader/set-key "w" 'save-buffer)
  (evil-leader/set-key "q" 'kill-buffer-and-window)
  (evil-leader/set-key
    "pD" 'projectile-dired
    "pd" 'projectile-find-dir
    "pj" 'projectile-find-tag
    "pR" 'projectile-regenerate-tags
	  "f" 'projectile-find-file
    "k" 'projectile-kill-buffers
    "s" 'projectile-switch-project
    "a" 'projectile-switch-to-buffer
    )

  (global-evil-leader-mode) 
  )

;; remap M-x to something else
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)


(global-set-key "\C-x\C-m" 'smex)

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
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
	 ))



(use-package evil-escape
  ;; use fd to escape
  :init
  (evil-escape-mode 1)
  (setq evil-escape-key-sequence (kbd "jk"))
  )

(use-package magit
  :ensure t
  :config (progn
	    (define-key evil-normal-state-map (kbd ",gb") 'magit-blame-mode)
	    (global-set-key (kbd "C-x C-g") 'magit-status)
        ;; somereason on windows git bin directory is on the path but not elisp
        ;; exec path
        (setq exec-path (append exec-path '("C:\\dev\\bin\\git\\bin"))) 
        ))

(require 'flx-ido)
;; disable ido faces to see flx highlights.

(use-package ido
  :ensure t
  :init (timeit
	 "IDO"
	 (ido-mode t)
	 (ido-everywhere t)
   (flx-ido-mode 1)
	 (setq ido-enable-flex-matching t)
   (setq ido-use-faces nil)
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
	    (setq evil-shift-width 4)
	    (setq tab-width 4)
        (setq indent-line-function 'insert-tab)
        (setq tab-stop-list (number-sequence 4 200 4))
        (setq auto-fill-mode -1)
        ))

  
;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; (autoload 'markdown-mode "markdown-mode"
;; 		     "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


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
        (local-set-key (kbd "C-c l") 'hs-hide-level)
        ;;(local-set-key (kbd "C-c h")  'hs-hide-block)
        (local-set-key (kbd "C-c h")    'hs-toggle-hiding)
        (local-set-key (kbd "C-c j")    'hs-hide-all)
        (local-set-key (kbd "C-c k")    'hs-show-all)
        (hs-minor-mode t)))


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


(defun test-fmt ()
  (message "Hello, %s!" "message to stderr")
  (fmt-stdout "Hello, %s!\n" "fmt-stdout, explict newline")
  (fmtln-stdout "Hello, %s!" "fmtln-stdout, implicit newline"))

(global-set-key (kbd "C-c s") 'toggle-window-split-horiz-vert)

(set-face-attribute 'default nil :height 110)
;;(set-default-font "Monaco 24")
; Test char and monospace:
; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
(cond 
 ;; ((find-font (font-spec :name "inconsolata"))
 ;;  (set-frame-font "inconsolata"))
 ;; ((find-font (font-spec :name "Droid Sans Mono"))
 ;;  (set-frame-font "Droid Sans Mono"))
 ;; ((find-font (font-spec :name "Ubuntu Mono"))
 ;;  (set-frame-font "Ubuntu Mono"))
 ;; ((find-font (font-spec :name "Source Code Pro"))
 ;;  (set-frame-font "Source Code Pro"))
 ;;  )
 ((find-font (font-spec :name "Consolas"))
  (set-frame-font "Consolas"))
  )
 ;; ((find-font (font-spec :name "Anonymous Pro"))
 ;;  (set-frame-font "Anonymous Pro"))
 ;;  )
 ;; (set-frame-font "inconsolata-12"))
 ;; ((find-font (font-spec :name "DejaVu Sans Mono"))
 ;;  (set-frame-font "DejaVu Sans Mono-12"))
 ;; ((find-font (font-spec :name "Lucida Console"))
 ;;  (set-frame-font "Lucida Console-12"))
 ;; ((find-font (font-spec :name "courier"))
 ;;  (set-frame-font "courier-12")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")

     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

           ;; magit stuff!!

;;(global-set-key (kbd "C-x k") 'windmove-up)
;;(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x l") 'windmove-right)
(global-set-key (kbd "C-x h") 'windmove-left)
;;http://stackoverflow.com/questions/13051632/emacs-efficient-buffer-switching-across-dual-monitors
(global-set-key (kbd "M-o") 'next-multiframe-window)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))



(use-package org
  :init (progn
	    (global-set-key (kbd "C-c a") 'org-agenda)
	    (global-set-key (kbd "C-c o c") 'org-capture)
	    (global-set-key (kbd "C-c o l") 'org-store-link)
      (setq org-log-done t)
      ;;(setq org-indent-mode t)
      ;;(setq org-startup-indented t)
      (setq org-startup-indented nil)
      (setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
      (setq default-tab-width 2)
      (setq org-agenda-files (list "~/org/gaz/projects.org"
                                   "~/org/ml.org"
                                   "~/org/pers.org"
                                   "~/org/q.org"
                                   ))
      (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))
))


(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(mmm-add-classes
 '((python-rst
    :submode rst-mode
    :front "^ *[ru]?\"\"\"[^\"]*$"
    :back "^ *\"\"\""
    :include-front t
    :include-back t
    :end-not-begin t)))
(mmm-add-mode-ext-class 'python-mode nil 'python-rst)


(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
;;(add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
(add-to-list 'default-frame-alist '(foreground-color . "#72716a"))


(provide 'user)
;;; user.el ends here

