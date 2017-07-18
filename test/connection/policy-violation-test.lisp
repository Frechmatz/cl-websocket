
(in-package :cl-websocket-test)


(define-test test-payload-too-big-1 ()
	     (with-test-connection (:max-payload-length 10) 'silent-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "01234567890")
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		(expect-close-frame is clws.frame:+STATUS-CODE-POLICY-VIOLATION+ nil))))

(define-test test-too-many-continuations-test-1 ()
	     (with-test-connection (:max-frame-count 5) 'echo-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "1" :fin nil)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "2" :fin nil :continuation t)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "3" :fin nil :continuation t)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "4" :fin nil :continuation t)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "5" :fin t :continuation t)

	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "1-1" :fin nil)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "1-2" :fin nil :continuation t)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "1-3" :fin nil :continuation t)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "1-4" :fin nil :continuation t)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "1-5" :fin nil :continuation t)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "1-6" :fin t :continuation t)

	       ;; read first chunk
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)

	       ;; read second chunk
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       
	       (let ((is (mock-socket.output socket)))
		 (expect-text-frame is "12345")
		 (expect-close-frame is clws.frame:+STATUS-CODE-POLICY-VIOLATION+ nil))))

