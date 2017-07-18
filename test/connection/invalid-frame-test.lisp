(in-package :cl-websocket-test)

;; test, that connection:read-frame properly closes the connection on
;; protocol error related conditions signalled by frame::read-frame
(define-test test-invalid-frame-test-1 ()
	     (with-test-connection () 'silent-handler socket con h
	       (assert-true h)
	       (clws.frame:write-frame
		(mock-socket.input socket)
		:opcode #xB ;; 0xb is an invalid opcode
		:buf (string-to-payload "")
		:fin t)
	       (clws.connection:read-frame con)
	       (let ((is (mock-socket.output socket)))
		(expect-close-frame is clws.frame:+STATUS-CODE-PROTOCOL-ERROR+ nil))))

