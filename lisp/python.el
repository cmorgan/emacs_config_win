;; python stuff
(setq
 ;;python-shell-interpreter "C:\\dev\\bin\\Anaconda\\envs\\bmra\\python.exe"
 python-shell-interpreter "C:/dev/bin/Anaconda/envs/dev/python.exe"
 python-shell-interpreter-args "-i C:/dev/bin/Anaconda/Scripts/ipython-script.py"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 ;; python-shell-completion-setup-code
 ;;   "from IPython.core.completerlib import module_completion"
 ;; python-shell-completion-module-string-code
 ;;   "';'.join(module_completion('''%s'''))\n"
 ;; python-shell-completion-string-code
 ;;   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
  )

(defun toggle-pyenv ()
  (interactive)
  (message "test!!")
(princ "Hello world!  I'm writing to STDOUT but I'm not in quotes!")
 ;;python-shell-interpreter "C:\\dev\\bin\\Anaconda\\envs\\dev\\python.exe"
  )
(global-set-key (kbd "C-c C-d") 'toggle-pyenv)


(provide 'python)
