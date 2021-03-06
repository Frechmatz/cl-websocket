
;;; Initialize quicklisp (copied from .sbclrc)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system "cl-websocket-test" :force t)
(in-package :cl-websocket-test)
(format t "~%Running tests...~%")
;; uncomment, when tests fail
(setf lisp-unit:*print-failures* t)
(use-debugger)
(run-tests)

