(in-package :cl-websocket-test)

(define-test test-read-uint-16-1 ()
	     (assert-true
	      (eql 128 (clws.binary-types:read-uint-16
			(create-parser-input-stream
			 0 128)))))

(define-test test-read-uint-16-2 ()
	     (assert-true
	      (eql 256 (clws.binary-types:read-uint-16
			(create-parser-input-stream
			 1 0)))))

(define-test test-read-uint-32-1 ()
	     (assert-true
	      (eql 1 (clws.binary-types:read-uint-32
			(create-parser-input-stream
			 0 0 0 1)))))

(define-test test-read-uint-32-2 ()
	     (assert-true
	      (eql (+ 16 (* 256 32) (* 65536 64) (* 16777216 128))
		   (clws.binary-types:read-uint-32
			(create-parser-input-stream
			 #b10000000
			 #b01000000
			 #b00100000
			 #b00010000)))))

(define-test test-read-uint-64-1 ()
	     (assert-true
	      (eql 1
		   (clws.binary-types:read-uint-64
			(create-parser-input-stream
			 0 0 0 0 0 0 0 1)))))

(define-test test-read-uint-64-2 ()
	     (assert-true
	      (eql 256
		   (clws.binary-types:read-uint-64
			(create-parser-input-stream
			 0 0 0 0 0 0 1 0)))))

(define-test test-read-uint-64-3 ()
	     (assert-true
	      (eql 65536
		   (clws.binary-types:read-uint-64
			(create-parser-input-stream
			 0 0 0 0 0 1 0 0)))))

(define-test test-read-uint-64-4 ()
	     (assert-true
	      (eql (* 65536 256)
		   (clws.binary-types:read-uint-64
			(create-parser-input-stream
			 0 0 0 0 1 0 0 0)))))

(define-test test-read-uint-64-5 ()
	     (assert-true
	      (eql (* 65536 256 256)
		   (clws.binary-types:read-uint-64
			(create-parser-input-stream
			 0 0 0 1 0 0 0 0)))))

(define-test test-read-uint-64-6 ()
	     (assert-true
	      (eql (* 65536 256 256 256)
		   (clws.binary-types:read-uint-64
			(create-parser-input-stream
			 0 0 1 0 0 0 0 0)))))

(define-test test-read-uint-64-7 ()
	     (assert-true
	      (eql (* 65536 256 256 256 256)
		   (clws.binary-types:read-uint-64
			(create-parser-input-stream
			 0 1 0 0 0 0 0 0)))))

(define-test test-read-uint-64-8 ()
	     (assert-true
	      (eql (* 65536 256 256 256 256 256)
		   (clws.binary-types:read-uint-64
			(create-parser-input-stream
			 1 0 0 0 0 0 0 0)))))

(define-test test-read-uint-64-9 ()
	     (assert-true
	      (eql (+ 1 (* 65536 256 256 256 256 256))
		   (clws.binary-types:read-uint-64
			(create-parser-input-stream
			 1 0 0 0 0 0 0 1)))))

(define-test write-read-uint-16-1 ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.binary-types:write-uint-16 65535 ostream)
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql 255 (aref seq 0)))
		 (assert-true (eql 255 (aref seq 1))))))

(define-test write-uint-16-2 ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.binary-types:write-uint-16 #xfe21 ostream)
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql #xfe (aref seq 0)))
		 (assert-true (eql #x21 (aref seq 1))))))

(define-test write-uint-32-1 ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.binary-types:write-uint-32 #xfedc3421 ostream)
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql #xfe (aref seq 0)))
		 (assert-true (eql #xdc (aref seq 1)))
		 (assert-true (eql #x34 (aref seq 2)))
		 (assert-true (eql #x21 (aref seq 3))))))

(define-test write-uint-64-1 ()
	     (let ((ostream (flexi-streams:make-in-memory-output-stream)))
	       (clws.binary-types:write-uint-64 #xfedc3421ab0a1234 ostream)
	       (let ((seq (flexi-streams:get-output-stream-sequence ostream)))
		 (assert-true (eql #xfe (aref seq 0)))
		 (assert-true (eql #xdc (aref seq 1)))
		 (assert-true (eql #x34 (aref seq 2)))
		 (assert-true (eql #x21 (aref seq 3)))
		 (assert-true (eql #xab (aref seq 4)))
		 (assert-true (eql #x0a (aref seq 5)))
		 (assert-true (eql #x12 (aref seq 6)))
		 (assert-true (eql #x34 (aref seq 7))))))
