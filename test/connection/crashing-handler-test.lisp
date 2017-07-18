(in-package :cl-websocket-test)


(define-test test-crashing-on-open-1 ()
	     (with-test-connection () 'crashing-on-open-handler socket con h
	       (assert-true h)
	       (assert-true con)
	       (let ((is (mock-socket.output socket)))
		(expect-close-frame is clws.frame:+STATUS-CODE-UNEXPECTED-CONDITION+ nil))))


(define-test test-crashing-on-message-1 ()
	     (with-test-connection () 'crashing-on-message-handler socket con h
	       (assert-true h)
	       (assert-true con)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "Hello, World")
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		(expect-close-frame is clws.frame:+STATUS-CODE-UNEXPECTED-CONDITION+ nil))))
