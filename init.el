(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
(require 'user)

;;; Code:
(setq package-archives '(
                         ;;("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http:/elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
