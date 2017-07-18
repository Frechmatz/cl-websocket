(in-package :cl-websocket-test)

;; Continuation   PendingFrames    Fin    Action
;; t              nil              *      Close connection
;; nil            t                *      Close connection
;; nil            nil              t      Add and flush
;; nil            nil              nil    Add
;; t              t                t      Add and flush
;; t              t                nil    Add

(define-test unexpected-continuation-test-1 ()
	     (with-test-connection () 'silent-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "Hello" :fin t :continuation t)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		 (expect-close-frame
		  is
		  clws.frame:+STATUS-CODE-PROTOCOL-ERROR+
		  clws.connection::+MSG-UNEXPECTED-CONTINUATION+))))

(define-test continuation-expected-test-1 ()
	     (with-test-connection () 'silent-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "Hello" :fin nil)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) ", World" :fin nil :continuation nil)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		 (expect-close-frame
		  is
		  clws.frame:+STATUS-CODE-PROTOCOL-ERROR+
		  clws.connection::+MSG-CONTINUATION-EXPECTED+))))

