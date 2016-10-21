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
(setq image-dired-external-viewer "C:\\Program Files (x86)\\IrfanView\\i_view32.exe")


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

(provide 'code)

