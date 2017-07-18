(in-package :cl-websocket-test)


(define-test ping-test-1 ()
	     (let ((buf
		    (make-array
		     '(3)
		     :element-type 'unsigned-byte
		     :initial-contents (list #x01 #x02 #x03))))
	     (with-test-connection () 'echo-and-close-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "Hello," :fin nil)
	       (cl-websocket-test::write-ping-frame (mock-socket.input socket) buf)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) " World" :fin t :continuation t)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		 (expect-pong-frame is buf)
		 (expect-text-frame is "Hello, World")
		 (expect-close-frame is +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ +ECHO-AND-CLOSE-HANDLER-REASON+)))))
