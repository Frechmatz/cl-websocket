
(in-package :cl-websocket-test)

(defparameter *ACCEPT-FRAME* (lambda (f)
			       (declare (ignore f))
			       t))
(defparameter *REJECT-FRAME* (lambda (f)
			       (declare (ignore f))
			       nil))


;; Input stream taken from https://tools.ietf.org/html/rfc6455
(define-test test-read-single-frame-unmasked-text-message ()
	     (let ((istream (create-parser-input-stream
			     #x81 #x05 #x48 #x65 #x6c #x6c #x6f)))
	       (let ((frame (clws.frame::read-frame istream *ACCEPT-FRAME*)))
		 (assert-true
		  (clws.frame::fin-frame-p frame))
		 (assert-true
		  (eql 0 (clws.frame::get-rsv frame)))
		 (assert-true
		  (eql 5 (clws.frame::get-payload-length frame)))
		 (assert-true
		  (not (clws.frame::get-mask frame)))
		 (assert-true
		  (eql clws.frame::+OPCODE-TEXT+
		       (clws.frame::get-opcode frame)))
		 (let ((payload-stream
			(create-parser-input-stream
			 (clws.frame::get-payload frame))))
		   (setf (flexi-streams:flexi-stream-external-format payload-stream)
			 (flexi-streams:make-external-format :utf-8 :little-endian nil))
		   (let ((hello (read-line payload-stream)))
		     (assert-true
		      (string= "Hello" hello))))
		 )))

;; Input stream taken from https://tools.ietf.org/html/rfc6455
(define-test test-read-single-frame-masked-text-message-1 ()
	     (let ((istream (create-parser-input-stream
			   #x81 #x85 #x37 #xfa #x21 #x3d #x7f #x9f #x4d #x51 #x58)))
	       (let ((frame (clws.frame::read-frame istream *ACCEPT-FRAME*)))
		 (assert-true
		  (clws.frame::fin-frame-p frame))
		 (assert-true
		  (eql 0 (clws.frame::get-rsv frame)))
		 (assert-true
		  (eql 5 (clws.frame::get-payload-length frame)))
		 (assert-true
		  (clws.frame::get-mask frame))
		 (assert-true
		  (eql clws.frame::+OPCODE-TEXT+
		       (clws.frame::get-opcode frame)))
		 ;; Mask: 0x37 0xfa 0x21 0x3d
		 (assert-true
		  (eql
		   #x37fa213d
		   (clws.frame::get-mask frame)))
		 (let ((payload-stream
			(create-parser-input-stream
			 (clws.frame::get-payload frame))))
		   (setf (flexi-streams:flexi-stream-external-format payload-stream)
			 (flexi-streams:make-external-format :utf-8 :little-endian nil))
		   (let ((hello (read-line payload-stream)))
		     (assert-true
		      (string= "Hello" hello))))
		 )))


;; Input stream taken from https://tools.ietf.org/html/rfc6455
(define-test test-read-fragmented-unmasked-text-message ()
	     (let ((istream (create-parser-input-stream
			     #x01 #x03 #x48 #x65 #x6c ;; Hel
			     #x80 #x02 #x6c #x6f))) ;; lo
	       (let ((frame1 (clws.frame::read-frame istream *ACCEPT-FRAME*)))
		 (assert-true
		  (not (clws.frame::fin-frame-p frame1)))
		 (assert-true
		  (eql 0 (clws.frame::get-rsv frame1)))
		 (assert-true
		  (eql 3 (clws.frame::get-payload-length frame1)))
		 (assert-true
		  (not (clws.frame::get-mask frame1)))
		 (assert-true
		  (eql clws.frame::+OPCODE-TEXT+
		       (clws.frame::get-opcode frame1)))
		 (let ((payload-stream
			(create-parser-input-stream
			 (clws.frame::get-payload frame1))))
		   (setf (flexi-streams:flexi-stream-external-format payload-stream)
			 (flexi-streams:make-external-format :utf-8 :little-endian nil))
		   (let ((hello (read-line payload-stream)))
		     (assert-true
		      (string= "Hel" hello))))
		 (let ((frame2 (clws.frame::read-frame istream *ACCEPT-FRAME*)))
		   (assert-true
		    (clws.frame::fin-frame-p frame2))
		   (assert-true
		    (eql 0 (clws.frame::get-rsv frame2)))
		   (assert-true
		    (eql 2 (clws.frame::get-payload-length frame2)))
		   (assert-true
		    (not (clws.frame::get-mask frame2)))
		   (assert-true
		    (eql clws.frame::+OPCODE-CONTINUATION+
			 (clws.frame::get-opcode frame2)))
		   (let ((payload-stream
			  (create-parser-input-stream
			   (clws.frame::get-payload frame2))))
		     (setf (flexi-streams:flexi-stream-external-format payload-stream)
			   (flexi-streams:make-external-format :utf-8 :little-endian nil))
		     (let ((hello (read-line payload-stream)))
		       (assert-true
			(string= "lo" hello)))))
		 )))

(define-test test-read-frame-rsv-1 ()
	     (let ((istream (create-parser-input-stream
			     #xa1 #x00)))
	       (let ((frame (clws.frame::read-frame istream *ACCEPT-FRAME*)))
		 (assert-true
		  (clws.frame::fin-frame-p frame))
		 (assert-true
		  (eql 2 (clws.frame::get-rsv frame)))
		 (assert-true
		  (eql 0 (clws.frame::get-payload-length frame)))
		 (assert-true
		  (not (clws.frame::get-mask frame)))
		 (assert-true
		  (eql clws.frame::+OPCODE-TEXT+
		       (clws.frame::get-opcode frame)))
		 )))

;; 16 bit payload len must be > 125
(define-test test-read-frame-payload-length-1 ()
	     (let ((istream (create-parser-input-stream
			     #x81 126 #x00 #x01)))
	       (let ((catched-error nil))
		 (handler-case
		     (clws.frame::read-frame istream *REJECT-FRAME*)
		   (clws.frame::protocol-error (err) (setf catched-error err)))
		 (assert-true catched-error))))

;; 64 bit payload len must be > 65535
(define-test test-read-frame-payload-length-2 ()
	     (let ((istream (create-parser-input-stream
			     #x81 127 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff)))
	       (let ((catched-error nil))
		 (handler-case
		     (clws.frame::read-frame istream *REJECT-FRAME*)
		   (clws.frame::protocol-error (err) (setf catched-error err)))
		 (assert-true catched-error))))

;; Most significant bit of 64 bit payload len must be 0
(define-test test-read-frame-payload-length-3 ()
	     (let ((istream (create-parser-input-stream
			     #x81 127 #x80 #x00 #x00 #x00 #x00 #x00 #xff #xff)))
	       (let ((catched-error nil))
		 (handler-case
		     (clws.frame::read-frame istream *REJECT-FRAME*)
		   (clws.frame::protocol-error (err) (setf catched-error err)))
		 (assert-true catched-error))))

(define-test test-read-single-frame-unmasked-long-text-message ()
	     (let ((istream (create-parser-input-stream 
			     #x81 126 #xff #xff
			     (make-array '(65535) :element-type 'unsigned-byte :initial-element 126))))
	       (let ((frame (clws.frame::read-frame istream *ACCEPT-FRAME*)))
		 (assert-true
		  (clws.frame::fin-frame-p frame))
		 (assert-true
		  (eql 0 (clws.frame::get-rsv frame)))
		 (assert-true
		  (eql 65535 (clws.frame::get-payload-length frame)))
		 (assert-true
		  (eql 65535 (length (clws.frame::get-payload frame))))
		 (assert-true
		  (not (clws.frame::get-mask frame)))
		 (assert-true
		  (eql clws.frame::+OPCODE-TEXT+
		       (clws.frame::get-opcode frame))))))

(define-test test-read-single-frame-unmasked-very-long-text-message ()
	     (let ((istream (create-parser-input-stream 
			     #x81 127 #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00
			     (make-array '(65536) :element-type 'unsigned-byte :initial-element 126))))
	       (let ((frame (clws.frame::read-frame istream *ACCEPT-FRAME*)))
		 (assert-true
		  (clws.frame::fin-frame-p frame))
		 (assert-true
		  (eql 0 (clws.frame::get-rsv frame)))
		 (assert-true
		  (eql 65536 (clws.frame::get-payload-length frame)))
		 (assert-true
		  (eql 65536 (length (clws.frame::get-payload frame))))
		 (assert-true
		  (not (clws.frame::get-mask frame)))
		 (assert-true
		  (eql clws.frame::+OPCODE-TEXT+
		       (clws.frame::get-opcode frame))))))
