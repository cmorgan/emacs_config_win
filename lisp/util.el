(defun read-file-in-string (fn)
  "Read FN and return its content as a string."
  (with-temp-buffer
    (insert-file-contents fn)
    (buffer-string)))


(defun my-beep ()
  "Play an alert sound."
  (let ((alert "/usr/share/sounds/gnome/default/alerts/glass.ogg"))
    (start-process "beep" nil "mplayer" (expand-file-name alert))))


(defun cycle (lst)
  "Cycle elements of LST."
  (let ((item (pop lst)))
    (append lst `(,item))))


(defmacro timeit (what &rest body)
  "Time WHAT and run BODY and report real time taken to do so."
  `(let ((start-time (float-time)))
     (progn ,@body)
     (let ((elapsed-time (- (float-time) start-time)))
       (message "Completed %s in %.4f seconds" ,what elapsed-time)
       elapsed-time)))


(defmacro hook-into-modes (func modes)
  "Add FUNC to MODES hooks."
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))


;; ;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;; (defun rename-file-and-buffer (new-name)


;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
(require 'ido)
(require 'cl-lib)
(require 'shell)

(defvar my-dir-selected nil "Flag to indicate that user has selected the directory")

(defun my-filter-cd-input (current-input)
  "Takes current user input for `cd' the a list
    whose car is the 'maximum possible directory path'
    and cdr is remaining string.

    Examples:
    '~/.emacs.d/in => ('~./emacs.d/' 'in')
    '/home/gue' => ('/home/' 'gue')
    '~/../' => ('~/../' '')"
  (let* ((unquoted-input (shell-unquote-argument current-input))
     (components (split-string unquoted-input "/"))
         (directory-parts (butlast components))
         (possible-prefix (car (last components))))
    (list (if (string= possible-prefix "")
              unquoted-input
            (concat (mapconcat 'identity directory-parts "/")
                    (when directory-parts "/")))
          possible-prefix)))


(defun my-complete-directory-name (directory current-input)
  "Prompts user for directories in `directory', `current-input'
    is the string entered by the user till now"
  (let* ((filtered-input (my-filter-cd-input current-input))
         (directory-path (car filtered-input))
         (partial-input (cadr filtered-input))
         (directory-choices (mapcar 'file-name-nondirectory
                                    (condition-case nil
                                        (cl-remove-if-not 'file-directory-p
                                                          (directory-files (concat directory directory-path) t))
                                      ('file-error (list)))))
         (selected-name (ido-completing-read "Directory: "
                                             directory-choices
                                             nil nil partial-input)))
    (comint-delete-input)
    (insert (concat "cd " 
            (shell-quote-argument (concat directory-path selected-name "/"))))))


(defun my-prompt-for-dir-or-fallback ()
  "If current shell command is `cd' prompt for directory
    using ido otherwise fallback to normal completion"
  (interactive)
  (let* ((user-input (buffer-substring-no-properties (comint-line-beginning-position)
                                                     (point-max))))
    (if (and (>= (length user-input) 3)
             (string= (substring user-input 0 3) "cd "))
        (progn 
          (setq my-dir-selected nil)
          (while (not my-dir-selected)
            (my-complete-directory-name default-directory 
                    (buffer-substring-no-properties (+ (comint-line-beginning-position) 3) 
                                    (point-max))))
          (comint-send-input))
      (call-interactively 'completion-at-point))))

(define-key shell-mode-map (kbd "<tab>") 'my-prompt-for-dir-or-fallback)

(add-hook 'ido-setup-hook 'ido-my-keys)


(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map (kbd "<C-return>") (lambda ()
                                                        (interactive)
                                                        (setq my-dir-selected t)
                                                        (ido-exit-minibuffer))))

(defun revert-all-buffers ()
          "Refreshes all open buffers from their respective files"
          (interactive)
          (let* ((list (buffer-list))
                 (buffer (car list)))
            (while buffer
              (when (and (buffer-file-name buffer) 
                         (not (buffer-modified-p buffer)))
                (set-buffer buffer)
                (revert-buffer t t t))
              (setq list (cdr list))
              (setq buffer (car list))))
          (message "Refreshed open files"))


(provide 'util)

