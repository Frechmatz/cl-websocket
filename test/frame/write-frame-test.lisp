
(in-package :cl-websocket-test)

;; Input stream taken from https://tools.ietf.org/html/rfc6455
;; #x81 #x05 #x48 #x65 #x6c #x6c #x6f
(define-test test-write-single-frame-unmasked-text-message ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.frame::write-frame
		ostream
		:fin t
		:rsv 0
		:opcode clws.frame::+OPCODE-TEXT+
		:mask nil
		:buf (make-array
		 '(5)
		 :element-type 'unsigned-byte
		 :initial-contents (list #x48 #x65 #x6c #x6c #x6f)))
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql 7 (length seq)))
		 (assert-true (eql #x81 (aref seq 0)))
		 (assert-true (eql #x05 (aref seq 1)))
		 (assert-true (eql #x48 (aref seq 2)))
		 (assert-true (eql #x65 (aref seq 3)))
		 (assert-true (eql #x6c (aref seq 4)))
		 (assert-true (eql #x6c (aref seq 5)))
		 (assert-true (eql #x6f (aref seq 6))))))

;; Input stream taken from https://tools.ietf.org/html/rfc6455
;; #x81 #x85 #x37 #xfa #x21 #x3d #x7f #x9f #x4d #x51 #x58
(define-test test-write-single-frame-masked-text-message-1 ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.frame::write-frame
		ostream
		:fin t
		:rsv 0
		:opcode clws.frame::+OPCODE-TEXT+
		:mask #x37fa213d
		:buf (make-array
		 '(5)
		 :element-type 'unsigned-byte
		 :initial-contents (list #x48 #x65 #x6c #x6c #x6f)))
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql 11 (length seq)))
		 (assert-true (eql #x81 (aref seq 0)))
		 (assert-true (eql #x85 (aref seq 1)))
		 (assert-true (eql #x37 (aref seq 2)))
		 (assert-true (eql #xfa (aref seq 3)))
		 (assert-true (eql #x21 (aref seq 4)))
		 (assert-true (eql #x3d (aref seq 5)))
		 (assert-true (eql #x7f (aref seq 6)))
		 (assert-true (eql #x9f (aref seq 7)))
		 (assert-true (eql #x4d (aref seq 8)))
		 (assert-true (eql #x51 (aref seq 9)))
		 (assert-true (eql #x58 (aref seq 10)))
		 )))
	     
(define-test test-write-single-frame-unmasked-text-message-2 ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.frame::write-frame
		ostream
		:fin t
		:rsv 0
		:opcode clws.frame::+OPCODE-TEXT+
		:mask nil
		:buf (make-array
		 '(6)
		 :element-type 'unsigned-byte
		 :initial-contents (list #xff #x48 #x65 #x6c #x6c #x6f))
		:start 1
		)
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql 7 (length seq)))
		 (assert-true (eql #x81 (aref seq 0)))
		 (assert-true (eql #x05 (aref seq 1)))
		 (assert-true (eql #x48 (aref seq 2)))
		 (assert-true (eql #x65 (aref seq 3)))
		 (assert-true (eql #x6c (aref seq 4)))
		 (assert-true (eql #x6c (aref seq 5)))
		 (assert-true (eql #x6f (aref seq 6))))))

(define-test test-write-single-frame-unmasked-text-message-3 ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.frame::write-frame
		ostream
		:fin t
		:rsv 0
		:opcode clws.frame::+OPCODE-TEXT+
		:mask nil
		:buf (make-array
		 '(6)
		 :element-type 'unsigned-byte
		 :initial-contents (list #xff #x48 #x65 #x6c #x6c #x6f))
		:start 1
		:end 2
		)
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql 3 (length seq)))
		 (assert-true (eql #x81 (aref seq 0)))
		 (assert-true (eql #x01 (aref seq 1)))
		 (assert-true (eql #x48 (aref seq 2))))))

(define-test test-write-single-frame-unmasked-text-message-4 ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.frame::write-frame
		ostream
		:fin t
		:rsv 0
		:opcode clws.frame::+OPCODE-TEXT+
		:mask nil
		:buf (make-array
		 '(6)
		 :element-type 'unsigned-byte
		 :initial-contents (list #xff #x48 #x65 #x6c #x6c #x6f))
		:start 1
		:end 4
		)
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql 5 (length seq)))
		 (assert-true (eql #x81 (aref seq 0)))
		 (assert-true (eql #x03 (aref seq 1)))
		 (assert-true (eql #x48 (aref seq 2)))
		 (assert-true (eql #x65 (aref seq 3)))
		 (assert-true (eql #x6c (aref seq 4))))))

(define-test test-write-read-single-frame-unmasked-long-binary-message ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.frame::write-frame
		ostream
		:fin t
		:rsv 0
		:opcode clws.frame::+OPCODE-BINARY+
		:mask nil
		:buf (make-array
		 '(999)
		 :element-type 'unsigned-byte
		 :initial-element 126))
	       (let ((istream (create-parser-input-stream (flexi-streams:get-output-stream-sequence ostream))))
		 (let ((frame (clws.frame::read-frame istream (lambda (f) (declare (ignore f)) t))))
		   (assert-true (eql clws.frame:+OPCODE-BINARY+ (clws.frame:get-opcode frame)))
		   (assert-true (eql 999 (clws.frame:get-payload-length frame)))
		   (assert-true (eql 999 (length (clws.frame:get-payload frame))))
		   ))))

(define-test test-write-read-single-frame-unmasked-very-long-binary-message ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.frame::write-frame
		ostream
		:fin t
		:rsv 0
		:opcode clws.frame::+OPCODE-BINARY+
		:mask nil
		:buf (make-array
		 '(128000)
		 :element-type 'unsigned-byte
		 :initial-element 126))
	       (let ((istream (create-parser-input-stream (flexi-streams:get-output-stream-sequence ostream))))
		 (let ((frame (clws.frame::read-frame istream (lambda (f) (declare (ignore f)) t))))
		   (assert-true (eql clws.frame:+OPCODE-BINARY+ (clws.frame:get-opcode frame)))
		   (assert-true (eql 128000 (clws.frame:get-payload-length frame)))
		   (assert-true (eql 128000 (length (clws.frame:get-payload frame))))
		   ))))

(define-test test-write-pong-frame-1 ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.frame::write-frame
		ostream
		:opcode clws.frame::+OPCODE-PONG+
		:rsv 0
		:buf nil)
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql 2 (length seq)))
		 (assert-true (eql #b10001010 (aref seq 0)))
		 )))

