(in-package :cl-websocket-test)


(define-test test-register-resource-handler-1 ()
	     (let ((server (make-instance 'test-server)))
	       (assert-true server)
	       (assert-false (clws.server::find-resource-handler server "/test"))
	       (clws.server:register-resource-handler server "/test" 'silent-handler nil)
	       (assert-true (clws.server::find-resource-handler server "/test"))
	       (assert-false (clws.server::find-resource-handler server "/test1"))))

(define-test test-register-resource-handler-add-duplicate ()
	     (let ((server (make-instance 'test-server)))
	       (assert-true server)
	       (clws.server:register-resource-handler server "/test" 'silent-handler nil)
	       (expect-resource-already-registered-exception
		 (clws.server:register-resource-handler server "/test" 'silent-handler nil))))

(define-test test-find-resource-handler-1 ()
	     (let ((server (make-instance 'test-server)))
	       (assert-true server)
	       (clws.server:register-resource-handler server "/test" 'silent-handler nil)
	       (clws.server:register-resource-handler server "/test2" 'silent-handler nil)
	       (let ((h (clws.server::find-resource-handler server "/test")))
		 (assert-equal "/test" (slot-value h 'clws.server::uri-path)))
	       (let ((h (clws.server::find-resource-handler server "/test2")))
		 (assert-equal "/test2" (slot-value h 'clws.server::uri-path)))))



