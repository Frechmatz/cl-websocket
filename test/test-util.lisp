(in-package :cl-websocket-test)

(defun create-parser-input-stream (&rest args)
  (let ((stream (flexi-streams:make-in-memory-output-stream)))
    (apply #'clws.http::write-to-stream stream args)
    (flexi-streams:make-flexi-stream
     (flexi-streams:make-in-memory-input-stream
      (flexi-streams:get-output-stream-sequence stream))
     :external-format (flexi-streams:make-external-format :us-ascii :eol-style :crlf) 
     )))

(defmacro expect-parse-exception(&body body)
  `(handler-case
       (progn
	 ,@body
	 (assert-true nil))
     (parse-error (err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (assert-false err)
       nil)))

(defmacro expect-eof-exception (&body body)
  `(handler-case
       (progn
	 ,@body
	 (assert-true nil)
	 nil)
     (end-of-file (err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (assert-false err)
       nil)))

(defmacro expect-resource-already-registered-exception (&body body)
  `(handler-case
       (progn
	 ,@body
	 (assert-true nil)
	 nil)
     (clws.server:resource-already-registered-error (err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (assert-false err)
       nil)))

;;
;;
;;


(defun string-to-payload (str)
  (flexi-streams:string-to-octets
   str
   :external-format (flexi-streams:make-external-format :utf-8 :little-endian nil)))

(defun payload-to-string (payload)
  (flexi-streams:octets-to-string
   payload
   :external-format (flexi-streams:make-external-format :utf-8 :little-endian nil)))

(defun write-text-frame (stream str &key (fin t) (continuation nil))
  (clws.frame:write-frame
   stream
   :opcode (if continuation clws.frame:+OPCODE-CONTINUATION+ clws.frame:+OPCODE-TEXT+)
   :buf (string-to-payload str)
   :fin fin))

(defun write-binary-frame (stream buf &key (fin t) (continuation nil))
  (clws.frame:write-frame
   stream
   :opcode (if continuation clws.frame:+OPCODE-CONTINUATION+ clws.frame:+OPCODE-BINARY+)
   :buf buf
   :fin fin))

(defun write-ping-frame (stream buf)
  (clws.frame:write-frame
   stream
   :opcode clws.frame:+OPCODE-PING+
   :buf buf
   :fin t))

;;
;; 
;; 

(defun may-accept-frame (frame)
  "Limit maximum payload size of test frames (garbage protection)"
  (if (< 32768 (clws.frame:get-payload-length frame))
      (error (make-condition
	      'clws.frame:protocol-error
	      :format-control "Payload of test frame is too big: ~a"
	      :format-arguments (list (clws.frame:get-payload-length frame))))
      t))

(defun expect-same-buffers (buf1 buf2)
  (assert-equal (length buf1) (length buf2))
  (let ((is-equal t))
    (dotimes (i (length buf1))
      (if is-equal
	  (setf is-equal (equal (aref buf1 i) (aref buf2 i)))))
    (assert-true is-equal)))


(defmacro with-frame (is frame err &body body)
  "Read frame and set up a lexical context with the frame and the error that may have been signalled 
by the read function."
  (let ((f (gensym)) (e (gensym)))
    `(let ((,e nil) (,f nil))
       (handler-case
	   (setf ,f (clws.frame:read-frame ,is #'may-accept-frame))
	 (condition (c)
	   (setf ,e c)))
       (let ((,frame ,f) (,err ,e))
	 ,@body))))

(defun expect-text-frame (istream text)
  "Read and validate a text frame"
  (with-frame istream frame err
    (assert-false err)
    (if (not err)
	(progn
	  (assert-true (clws.frame:text-frame-p frame))
	  (if (clws.frame:text-frame-p frame)
	      (assert-equal text (payload-to-string (clws.frame:get-payload frame))))))))

(defun expect-close-frame (istream status-code reason)
  "Read and validate a close frame"
  (with-frame istream frame err
    (assert-false err)
    (if (not err)
	(progn
	  (assert-true (clws.frame:close-frame-p frame))
	  (if (clws.frame:close-frame-p frame)
	      (let ((close-payload
		     (clws.frame:parse-close-payload (clws.frame:get-payload frame))))
		(assert-equal status-code (first close-payload))
		(if reason
		    (assert-equal reason (second close-payload)))))))))

(defun expect-binary-frame (istream buf)
  "Read and validate a binary frame"
  (with-frame istream frame err
    (assert-false err)
    (if (not err)
	(progn
	  (assert-true (clws.frame:binary-frame-p frame))
	  (if (clws.frame:binary-frame-p frame)
	      (expect-same-buffers buf (clws.frame:get-payload frame)))))))

(defun expect-pong-frame (istream buf)
  "Read and validate a pong frame"
  (with-frame istream frame err
    (assert-false err)
    (if (not err)
	(progn
	  (assert-true (clws.frame:pong-frame-p frame))
	  (if (clws.frame:pong-frame-p frame)
	      (expect-same-buffers buf (clws.frame:get-payload frame)))))))

;;
;;
;;


(defmacro with-test-connection ((&key
				 (max-payload-length 65536)
				 (max-frame-count 100)
				 (connection-class 'clws.connection:websocketconnection)
				 (start-connection t)
				 (server nil)
				 (http-request nil))
				   handler-class socket connection handler
				&body body)
  (let ((con (gensym)) (s (gensym)) (h (gensym)))
    `(let* ((,s (make-instance 'connection-socket-mock))
	    (,h (make-instance ,handler-class))
	    (,con (make-instance ',connection-class
				 :handler ,h
				 :socket ,s
				 :server ,server
				 :http-request ,http-request
				 :options (list
					   (list :max-payload-length ,max-payload-length)
					   (list :max-frame-count ,max-frame-count)))))
       (setf (slot-value ,h 'clws.handler::connection) ,con)
       (if ,start-connection
	   (handler-case 
	       (clws.connection:start-connection ,con :enter-dispatch-loop nil)
	     (condition (err)
	       (v:debug :cl-websocket-test "Error while starting test connection: ~a" err))))
       (let ((,socket ,s) (,connection ,con) (,handler ,h))
	 ,@body))))

;;
;;
;;

(defun assert-equal-handlers (expected-handlers found-handlers)
    (assert-equal (length expected-handlers) (length found-handlers))
    (dolist (h expected-handlers)
      (assert-true (find-if (lambda (srv-h) (eq h srv-h)) found-handlers))))
  
(defun expect-server-handlers-by-handler (current-handler handlers)
  ""
  (let ((found-handlers '()))
    (clws.server::do-connection-handlers-by-handler current-handler cur-handler
      (push cur-handler found-handlers))
    (assert-equal-handlers handlers found-handlers)))

(defun expect-server-handlers-by-uri (server uri-path handlers)
  ""
  (let ((found-handlers '()))
    (clws.server::do-connection-handlers-by-uri server uri-path cur-handler
      (push cur-handler found-handlers))
    (assert-equal-handlers handlers found-handlers)))

(defun expect-server-handlers (server handlers)
  ""
  (let ((found-handlers '()))
    (clws.server::do-connection-handlers server cur-handler
      (push cur-handler found-handlers))
    (assert-equal-handlers handlers found-handlers)))


