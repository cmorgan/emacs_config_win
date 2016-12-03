;;; python-test-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "python-test" "python-test.el" (22504 59079
;;;;;;  0 0))
;;; Generated autoloads from python-test.el

(autoload 'python-test-function "python-test" "\
Test a python function inside PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS.

\(fn PROJECT-ROOT BACKEND-NAME EXTRA-ARGS)" t nil)

(autoload 'python-test-method "python-test" "\
Test a single python method inside PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS.

\(fn PROJECT-ROOT BACKEND-NAME EXTRA-ARGS)" t nil)

(autoload 'python-test-class "python-test" "\
Test a single python class inside PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS.

\(fn PROJECT-ROOT BACKEND-NAME EXTRA-ARGS)" t nil)

(autoload 'python-test-file "python-test" "\
Test a single python file inside PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS.

\(fn PROJECT-ROOT BACKEND-NAME EXTRA-ARGS)" t nil)

(defalias 'python-test #'python-test-project)

(autoload 'python-test-project "python-test" "\
Test a python project from PROJECT-ROOT using BACKEND-NAME with EXTRA-ARGS.

\(fn PROJECT-ROOT BACKEND-NAME EXTRA-ARGS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; python-test-autoloads.el ends here
