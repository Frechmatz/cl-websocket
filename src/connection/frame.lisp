
(in-package :clws.frame)

(defparameter +OPCODE-CONTINUATION+ #x0)
(defparameter +OPCODE-TEXT+ #x1)
(defparameter +OPCODE-BINARY+ #x2)
(defparameter +OPCODE-CLOSE+ #x8)
(defparameter +OPCODE-PING+ #x9)
(defparameter +OPCODE-PONG+ #xA)

(defun valid-opcode-p (opcode)
  (cond
    ((eq opcode +OPCODE-TEXT+) t)
    ((eq opcode +OPCODE-BINARY+) t)
    ((eq opcode +OPCODE-CLOSE+) t)
    ((eq opcode +OPCODE-CONTINUATION+) t)
    ((eq opcode +OPCODE-PING+) t)
    ((eq opcode +OPCODE-PONG+) t)
    (t nil)))

(defun opcode-name (opcode)
  (cond
    ((eq opcode +OPCODE-TEXT+) "TEXT")
    ((eq opcode +OPCODE-BINARY+) "BINARY")
    ((eq opcode +OPCODE-CLOSE+) "CLOSE")
    ((eq opcode +OPCODE-CONTINUATION+) "CONTINUATION")
    ((eq opcode +OPCODE-PING+) "PING")
    ((eq opcode +OPCODE-PONG+) "PONG")
    (t (format nil "UNDEFINED: ~a" opcode))))

(defclass frame ()
  ((fin :initform nil)
   (rsv :initform 0)
   (opcode :initform 0)
   (payload-length :initform 0)
   (mask :initform nil)
   (payload :initform nil :documentation
	    "nil or the un-masked payload of the frame")))

(define-condition protocol-error (error) ())
(define-condition inconsistent-data-error (error) ())

(defun fin-frame-p (frame)
  (slot-value frame 'fin))

(defun get-rsv (frame)
  (slot-value frame 'rsv))

(defun get-opcode (frame)
  (slot-value frame 'opcode))

(defun continuation-frame-p (frame)
  (eq (slot-value frame 'opcode) +OPCODE-CONTINUATION+))

(defun text-frame-p (frame)
  (eq (slot-value frame 'opcode) +OPCODE-TEXT+))

(defun binary-frame-p (frame)
  (eq (slot-value frame 'opcode) +OPCODE-BINARY+))

(defun close-frame-p (frame)
  (eq (slot-value frame 'opcode) +OPCODE-CLOSE+))

(defun ping-frame-p (frame)
  (eq (slot-value frame 'opcode) +OPCODE-PING+))

(defun pong-frame-p (frame)
  (eq (slot-value frame 'opcode) +OPCODE-PONG+))

(defun control-opcode-p (opcode)
  (cond
    ((eq opcode +OPCODE-CLOSE+) t)
    ((eq opcode +OPCODE-PING+) t)
    ((eq opcode +OPCODE-PONG+) t)
    (t nil)))


(defparameter +STATUS-CODE-NORMAL-CLOSURE+ 1000)
(defparameter +STATUS-CODE-GOING-AWAY+ 1001)
(defparameter +STATUS-CODE-PROTOCOL-ERROR+ 1002)
(defparameter +STATUS-CODE-CANNOT-ACCEPT-DATA+ 1003)
(defparameter +STATUS-CODE-INCONSISTENT-DATA+ 1007)
(defparameter +STATUS-CODE-POLICY-VIOLATION+ 1008)
(defparameter +STATUS-CODE-DATA-TOO-BIG+ 1009)
(defparameter +STATUS-CODE-UNNEGOTIATED-EXTENSION+ 1010)
(defparameter +STATUS-CODE-UNEXPECTED-CONDITION+ 1011)
  
(defparameter *valid-close-status-codes*
  ;; https://tools.ietf.org/html/rfc6455#section-7.4.1
  '(1000 1001 1002 1003 1007 1008 1009 1010 1011 3000 3999 4000 4999))

(defun valid-close-status-code-p (status-code)
  (find status-code *valid-close-status-codes*))

(defun get-payload-length (frame)
  (slot-value frame 'payload-length))

(defun get-mask (frame)
  (slot-value frame 'mask))

(defun get-payload (frame)
  (slot-value frame 'payload))

(defun mask-to-byte-array (mask)
  (make-array
   '(4)
   :initial-contents
   (list (ldb (byte 8 24) mask)
     (ldb (byte 8 16) mask)
     (ldb (byte 8 8) mask)
     (ldb (byte 8 0) mask))))


(defun get-payload-mask-processor (mask)
  (if (not mask)
      (lambda (b) b)
      (let ((mask-array (mask-to-byte-array mask))
	    (pos 0))
	(lambda (b)
	  (let ((r (logxor b (aref mask-array (mod pos 4)))))
	    (setf pos (+ pos 1))
	    r)))))

(defun log-frame (&key fin rsv opcode mask payload-length)
  (format nil
	  "Fin: ~a Rsv: ~a Opcode: ~a Payload-Length: ~a Mask: ~a"
	  fin
	  rsv
	  (opcode-name opcode)
	  payload-length
	  mask))

(defun read-frame (stream may-accept-frame-fn)
  "Read a frame. May signal a protocol-error. 
   stream: the input stream
   may-accept-frame-fn: a function that is called with the frame after the 
     frame header data has been read. This function is meant to apply
     configuration and state dependent checks (such as to check if a 
     requested extension has been negotiated or if the payload is too big)."
  (let ((frame (make-instance 'frame)))
    ;; first byte: fin flag, rsv, opcode
    (let ((i (clws.binary-types:read-uint-8 stream)))
      (setf (slot-value frame 'fin) (ldb-test (byte 8 7) i))
      (setf (slot-value frame 'rsv) (ldb (byte 3 4) i))
      (setf (slot-value frame 'opcode) (ldb (byte 4 0) i))
      (if (not (valid-opcode-p (slot-value frame 'opcode)))
	  (error (make-condition
		  'clws.frame:protocol-error
		  :format-control "Invalid opcode: ~a"
		  :format-arguments (list (slot-value frame 'opcode)))))
      (if (and (control-opcode-p (slot-value frame 'opcode)) (not (slot-value frame 'fin)))
       (error (make-condition
	       'protocol-error
	       :format-control "A control frame cannot be fragmented")))
      ;; second byte: mask flag, payload len 
      (setf i (clws.binary-types:read-uint-8 stream))
      (let ((payload-len (ldb (byte 7 0) i)))
	(cond
	  ((and (control-opcode-p (slot-value frame 'opcode)) (< 125 payload-len))
	   (error (make-condition
		   'protocol-error
		   :format-control "Payload length of control code must be <= 125: ~a"
		   :format-arguments (list payload-len))))
	  ((eql payload-len 126)
	   (setf payload-len (clws.binary-types:read-uint-16 stream))
	   (if (<= payload-len 125)
	       (error (make-condition
		       'protocol-error
		       :format-control "Payload length must be > 126: ~a"
		       :format-arguments (list payload-len)))))
	  ((eql payload-len 127)
	   (setf payload-len (clws.binary-types:read-uint-64 stream))
	   (if (< payload-len 65536)
	       (error (make-condition
		       'protocol-error
		       :format-control "64-bit payload length must be greater than 65535: ~a"
		       :format-arguments (list payload-len))))
	   (if (ldb-test (byte 64 63) payload-len)
	       (error (make-condition
		       'protocol-error
		       :format-control "Most significant bit of 64-bit payload length must be 0: ~a"
		       :format-arguments (list payload-len))))))
	(setf (slot-value frame 'payload-length) payload-len)
	;; Check mask bit and read mask
	(if (ldb-test (byte 8 7) i)
	    (setf (slot-value frame 'mask) 
		  (clws.binary-types:read-uint-32 stream)))
	;; Check if frame is eligible for further processing (will throw a protocol error)
	(funcall may-accept-frame-fn frame)
	(if (< 0 payload-len)
	    ;; Read payload
	    ;; todo: use read-sequence to read blob. Don't get it work for now :(
	    ;; todo: more optimizations (type decls payload-mask-processor)
	    (let ((buf (make-array payload-len :element-type '(unsigned-byte 8) :adjustable nil))
		  (payload-mask-processor (get-payload-mask-processor (get-mask frame))))
	      ;;(read-sequence buf stream)
	      (dotimes (i payload-len)
		;;(setf (elt buf i) (funcall payload-mask-processor (elt buf 1)))
		(setf (elt buf i) (funcall payload-mask-processor (read-byte stream))))
	      (setf (slot-value frame 'payload) buf))))
      (v:trace :clws.frame
	      "Received frame: ~a"
	      (log-frame
	       :fin (fin-frame-p frame)
	       :rsv (get-rsv frame)
	       :opcode (get-opcode frame)
	       :mask (get-mask frame)
	       :payload-length (get-payload-length frame)))
      frame)))

(defun write-frame (output-stream
		    &key
		      opcode
		      (fin t) (rsv 0) (mask nil)
		      (buf nil)
		      (start 0) (end nil))
  "start, end: bounding index designators
   buf: a sequence of unsigned-byte or nil"
  ;; Write fin, rsv, opcode
  (write-byte (+ (if fin 128 0) (* rsv 16) opcode) output-stream)
  (let ((len (if (not buf) 0 (- (if end end (length buf)) start))))
    ;; write mask indicator and payload length
    (let ((mask-byte (if mask 128 0)))
      (cond
	((<= len 125)
	 (write-byte (+ mask-byte len) output-stream))
	((< len 65536)
	 (write-byte (+ mask-byte 126) output-stream)
	 (clws.binary-types:write-uint-16 len output-stream))
	(t
	 (write-byte (+ mask-byte 127) output-stream)
	 (clws.binary-types:write-uint-64 len output-stream))))
    ;; write mask
    (if mask
	(clws.binary-types:write-uint-32 mask output-stream))
    ;; write payload
    (if (> len 0)
	(let ((payload-mask-processor (get-payload-mask-processor mask)))
	  (dotimes (i len)
	    (write-byte
	     (funcall payload-mask-processor (elt buf (+ start i)))
	     output-stream))))
    (v:trace :clws.frame
	    "Sent frame: ~a"
	    (log-frame
	     :fin fin
	     :rsv rsv
	     :opcode opcode
	     :mask mask
	     :payload-length len))))

			
(defun parse-close-payload (buf)
  "Parse the payload of a close frame
   buf: a byte array
   returns a list containing the status code and the reason. Both values might be nil.
   Parse errors won't be propagated. In the case of an error nil values will be returned
   for both the status code and the reason"
  (if (or (not buf) (= 0 (length buf)))
      (list nil nil)
      (flet ((read-reason (stream)
	       (let ((str (make-array 50 :element-type 'character :adjustable t :fill-pointer 0)))
		 (loop
		    (let ((cur-char (read-char stream nil nil)))
		      (if (not cur-char)
			  (return)
			  (vector-push-extend cur-char str))))
		 (if (< 0 (length str)) str nil))))
	(let ((stream
	       (flexi-streams:make-flexi-stream
		(flexi-streams:make-in-memory-input-stream buf)
		:external-format (flexi-streams:make-external-format :utf-8 :little-endian nil))))
	  (handler-case
	      (list
	       (clws.binary-types:read-uint-16 stream)
	       (read-reason stream))
	    (condition (err)
	      (progn
		(v:error :clws.frame "Received error while parsing the payload of a close frame: ~a" err)
		(list nil nil))))))))

(defun make-close-payload (status-code reason)
  (if (not status-code)
      nil
      (let* ((memory-stream (flexi-streams:make-in-memory-output-stream))
	     (output-stream
	      (flexi-streams:make-flexi-stream 
	       memory-stream
	       :external-format (flexi-streams:make-external-format :utf-8 :little-endian nil))))
	(clws.binary-types:write-uint-16 status-code output-stream)
	(if reason
	    (write-string reason output-stream))
	(force-output output-stream)
	(flexi-streams:get-output-stream-sequence memory-stream))))
