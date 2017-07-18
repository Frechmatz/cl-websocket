
(in-package :cl-websocket-test)


(define-test multiple-frame-text-message-test-1 ()
	     (with-test-connection () 'echo-and-close-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "Hello," :fin nil)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) " " :fin nil :continuation t)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "World" :fin t :continuation t)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		 (expect-text-frame is "Hello, World")
		 (expect-close-frame is +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ +ECHO-AND-CLOSE-HANDLER-REASON+))))

(define-test multiple-frame-binary-message-test-1 ()
	     (let ((buf1
		    (make-array
		     '(3)
		     :element-type 'unsigned-byte
		     :initial-contents (list #x01 #x02 #x03)))
		   (buf2
		    (make-array
		     '(4)
		     :element-type 'unsigned-byte
		     :initial-contents (list #x10 #x11 #x13 #x14))))
	       (with-test-connection () 'echo-and-close-handler socket con h
		 (assert-true h)
		 (cl-websocket-test::write-binary-frame (mock-socket.input socket) buf1 :fin nil)
		 (cl-websocket-test::write-binary-frame (mock-socket.input socket) buf2 :fin t :continuation :t)
		 (clws.connection:read-frame con)
		 (clws.connection:read-frame con)
		 (let ((is (mock-socket.output socket)))
		   (expect-binary-frame is (concatenate '(vector (unsigned-byte 8)) buf1 buf2))
		   (expect-close-frame is +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ +ECHO-AND-CLOSE-HANDLER-REASON+)))))
