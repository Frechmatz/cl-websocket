
(in-package :cl-websocket-test)


(define-test test-echo-and-close-text-1 ()
	     (with-test-connection () 'echo-and-close-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "Hello, World")
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		(expect-text-frame is "Hello, World")
		(expect-close-frame is +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ +ECHO-AND-CLOSE-HANDLER-REASON+))))

(define-test test-echo-and-close-text-2 ()
	     (with-test-connection () 'echo-2-and-close-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "Hello, World")
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "How is business?")
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		 (expect-text-frame is "Hello, World")
		 (expect-text-frame is "How is business?")
		 (expect-close-frame is +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ +ECHO-AND-CLOSE-HANDLER-REASON+))))

(define-test test-echo-and-close-binary-1 ()
	     (let ((buf
		(make-array
		 '(6)
		 :element-type 'unsigned-byte
		 :initial-contents (list #xff #x48 #x65 #x6c #x6c #x6f))))
	     (with-test-connection () 'echo-and-close-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-binary-frame (mock-socket.input socket) buf)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		 (expect-binary-frame is buf)
		 (expect-close-frame is +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ +ECHO-AND-CLOSE-HANDLER-REASON+)))))

(define-test test-echo-and-close-binary-2 ()
	     (let ((buf1
		    (make-array
		     '(6)
		     :element-type 'unsigned-byte
		     :initial-contents (list #xff #x48 #x65 #x6c #x6c #x6f)))
		   (buf2
		    (make-array
		     '(3)
		     :element-type 'unsigned-byte
		     :initial-contents (list #x01 #x02 #x03))))
	       (with-test-connection () 'echo-2-and-close-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-binary-frame (mock-socket.input socket) buf1)
	       (cl-websocket-test::write-binary-frame (mock-socket.input socket) buf2)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		 (expect-binary-frame is buf1)
		 (expect-binary-frame is buf2)
		 (expect-close-frame is +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ +ECHO-AND-CLOSE-HANDLER-REASON+)))))

(define-test test-echo-and-close-mixed-1 ()
	     (let ((buf
		    (make-array
		     '(6)
		     :element-type 'unsigned-byte
		     :initial-contents (list #xff #x48 #x65 #x6c #x6c #x6f))))
	       (with-test-connection () 'echo-2-and-close-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-binary-frame (mock-socket.input socket) buf)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "Hello, World")
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		 (expect-binary-frame is buf)
		 (expect-text-frame is "Hello, World")
		 (expect-close-frame is +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ +ECHO-AND-CLOSE-HANDLER-REASON+)))))

(define-test test-echo-and-close-mixed-2 ()
	     (let ((buf
		    (make-array
		     '(6)
		     :element-type 'unsigned-byte
		     :initial-contents (list #xff #x48 #x65 #x6c #x6c #x6f))))
	       (with-test-connection () 'echo-2-and-close-handler socket con h
	       (assert-true h)
	       (cl-websocket-test::write-text-frame (mock-socket.input socket) "Hello, World")
	       (cl-websocket-test::write-binary-frame (mock-socket.input socket) buf)
	       (clws.connection:read-frame con)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		 (expect-text-frame is "Hello, World")
		 (expect-binary-frame is buf)
		 (expect-close-frame is +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ +ECHO-AND-CLOSE-HANDLER-REASON+)))))

