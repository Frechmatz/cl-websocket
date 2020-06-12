(in-package :clws.connection)

(define-condition policy-violation-error (error) ())

(defclass websocketconnection ()
  ((connection-handler :initform nil)
   (max-payload-length :initform nil)
   (max-frame-count :initform nil)
   (connection-socket :initform nil)
   (connection-state :initform nil :documentation ":running or :stopping or :stopped")
   (connection-state-lock :initform (bt:make-lock "websocketconnection-state-lock"))
   (sending-data-lock :initform (bt:make-lock "websocketconnection-sending-data-lock"))
   (connection-frame-buffer :initform nil :documentation "Data frame buffer")))

(defmethod initialize-instance :after ((c websocketconnection)
				&key
				  (handler nil)
				  (socket nil)
				  (options nil)
				  &allow-other-keys)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (setf (slot-value c 'connection-handler) handler)
  (setf (slot-value c 'connection-socket) socket)
  (let ((max-payload-length (second (assoc :max-payload-length options)))
	(max-frame-count (second (assoc :max-frame-count options))))
    (if max-payload-length
	(setf (slot-value c 'max-payload-length) max-payload-length))
    (if max-frame-count
	(setf (slot-value c 'max-frame-count) max-frame-count))))

(defgeneric close-connection (connection status-code reason))

(defun may-accept-frame (connection frame)
  "Performs configuration/state dependent checks. Signals a protocol error if the frame cannot be accepted."
  ;; (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (not (eql 0 (clws.frame:get-rsv frame)))
      (error (make-condition
	      'clws.frame:protocol-error
	      :format-control "RSV must be 0 since no extension has been negotiated")))
  (let ((max-payload-length (slot-value connection 'max-payload-length)))
    (if (and max-payload-length (> (clws.frame:get-payload-length frame) max-payload-length))
      (error (make-condition
	      'clws.connection:policy-violation-error
	      :format-control "Payload too big"))))
  (let ((max-frame-count (slot-value connection 'max-frame-count)))
    (if (and max-frame-count (clws.frame:continuation-frame-p frame)
	   (not (clws.frame:fin-frame-p frame))
	   (>= (length (slot-value connection 'connection-frame-buffer))
	       (+ -1 max-frame-count)))
      (error (make-condition
	      'clws.connection:policy-violation-error
	      :format-control "Too many continuation frames"))))
  t)

(defun connection-send-message (connection opcode payload)
  (bt:with-lock-held ((slot-value connection 'sending-data-lock))
    (let ((io-stream (clws.socket:socket-stream (slot-value connection 'connection-socket))))
      (clws.frame:write-frame io-stream :opcode opcode :buf payload)
      (force-output io-stream))))
  
(defun connection-send-binary-message (connection payload)
  (connection-send-message
   connection
   clws.frame:+OPCODE-BINARY+
   payload))

(defun connection-send-text-message (connection str)
  (let ((payload
	 (flexi-streams:string-to-octets
	  str
	  :external-format (flexi-streams:make-external-format :utf-8 :little-endian nil))))
    (connection-send-message
     connection
     clws.frame:+OPCODE-TEXT+
     payload)))

(defmethod close-connection ((con websocketconnection) status-code reason)
  (v:trace :clws.connection "close-connection")
  (let ((already-stopped
	 (bt:with-lock-held ((slot-value con 'connection-state-lock))
	   (if (member (slot-value con 'connection-state) '(:stopped :stopping))
	       t
	       (progn 
		 (setf (slot-value con 'connection-state) :stopped)
		 nil)))))
    (if (not already-stopped)
	(progn
	  ;; send close frame
	  (unwind-protect
	       (connection-send-message
		con
		clws.frame:+OPCODE-CLOSE+
		(clws.frame:make-close-payload status-code reason)))
	  ;; close socket
	  (v:trace :clws.connection "Closing connection socket")
	  (handler-case
	      (clws.socket:socket-close (slot-value con 'connection-socket))
	    (condition (err)
	      (v:warn :clws.connection
		       "Got error on closing socket of connection: ~a"
		       err)))
	  ;; notify handler
	  (handler-case 
	      (clws.handler:on-close-connection (slot-value con 'connection-handler) status-code reason)
	    (condition (err)
	      (v:warn :clws.connection
		       "Got error on calling on-close handler of connection: ~a"
		       err)))))))

(defun process-close-frame (connection frame)
  (let ((parsed-payload (clws.frame:parse-close-payload (clws.frame:get-payload frame))))
    (if (and (first parsed-payload) (not (clws.frame:valid-close-status-code-p (first parsed-payload))))
	(error (make-condition
		'clws.frame:protocol-error
		:format-control "Invalid status code in close event: ~a"
		:format-arguments (list (first parsed-payload)))))
    (close-connection connection (first parsed-payload) (second parsed-payload))))

(defun process-ping-frame (connection frame)
  (connection-send-message
   connection
   clws.frame:+OPCODE-PONG+
   ;; echo payload of ping frame
   (clws.frame:get-payload frame)))

(defun flush-frame-buffer (connection)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (v:trace :clws.connection "Flushing frame buffer")
  (let ((frames (reverse (slot-value connection 'connection-frame-buffer))))
    (setf (slot-value connection 'connection-frame-buffer) nil)
    (let ((payload
	   (apply #'concatenate
		  '(vector (unsigned-byte 8))
		  (mapcar (lambda (f) (clws.frame:get-payload f)) 
			  (remove-if
			   (lambda (f) (not (clws.frame:get-payload f)))
			   frames)))))
      (if (eq (clws.frame:get-opcode (first frames)) clws.frame:+OPCODE-TEXT+)
	  (let ((str nil))
	    (handler-case
		(setf str
		       (flexi-streams:octets-to-string
			payload
			:external-format (flexi-streams:make-external-format :utf-8 :little-endian nil)))
	      (error (err)
		(error (make-condition
			'clws.frame:inconsistent-data-error
			:format-control "Cannot parse UTF-8 data: ~a"
			:format-arguments err))))
	    (clws.handler:on-text-message (slot-value connection 'connection-handler) str))
	  (clws.handler:on-binary-message (slot-value connection 'connection-handler) payload)))))


;; Continuation   PendingFrames    Fin    Action
;; t              nil              *      Close connection
;; nil            t                *      Close connection
;; nil            nil              t      Add and flush
;; nil            nil              nil    Add
;; t              t                t      Add and flush
;; t              t                nil    Add

(defparameter +MSG-UNEXPECTED-CONTINUATION+ "Continuation frame without initiating frame")
(defparameter +MSG-CONTINUATION-EXPECTED+ "Continuation frame expected")

(defun process-data-frame (connection frame)
  (v:trace :clws.connection "Processing data frame")
  (let ((fb (slot-value connection 'connection-frame-buffer))
	(fin (clws.frame:fin-frame-p frame))
	(continuation (clws.frame:continuation-frame-p frame)))
    (cond
      ((and continuation (not fb))
       (v:warn :clws.connection +MSG-UNEXPECTED-CONTINUATION+)
       (close-connection connection clws.frame:+STATUS-CODE-PROTOCOL-ERROR+ +MSG-UNEXPECTED-CONTINUATION+))
      ((and (not continuation) fb)
       (v:warn :clws.connection +MSG-CONTINUATION-EXPECTED+)
       (close-connection connection clws.frame:+STATUS-CODE-PROTOCOL-ERROR+ +MSG-CONTINUATION-EXPECTED+))
      ((and (not continuation) (not fb) fin)
       (push frame (slot-value connection 'connection-frame-buffer))
       (flush-frame-buffer connection))
      ((and (not continuation) (not fb) (not fin))
       (push frame (slot-value connection 'connection-frame-buffer)))
      ((and continuation fb fin)
       (push frame (slot-value connection 'connection-frame-buffer))
       (flush-frame-buffer connection))
      ((and continuation fb)
       (push frame (slot-value connection 'connection-frame-buffer)))
      (t
       (v:error :clws.connection "Fatal: Cannot process frame. Dont know how to handle state")
       (close-connection connection 1011 "Internal server error")))))

(defun read-frame (connection)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (handler-case
      (let ((frame
	     (clws.frame:read-frame
	      (clws.socket:socket-stream (slot-value connection 'connection-socket))
	      (lambda (frame) (may-accept-frame connection frame)))))
	(cond
	  ((clws.frame:close-frame-p frame)
	   (process-close-frame connection frame))
	  ((clws.frame:ping-frame-p frame)
	   (process-ping-frame connection frame))
	  ((clws.frame:pong-frame-p frame)
	   nil) ;; ignore unsolicited pong 
	  ((or (clws.frame:binary-frame-p frame)
	       (clws.frame:text-frame-p frame)
	       (clws.frame:continuation-frame-p frame))
	   (process-data-frame connection frame))
	  (t (close-connection
	      connection
	      clws.frame:+STATUS-CODE-UNEXPECTED-CONDITION+
	      (format nil "Internal server error. Dont know how to handle frame opcode ~a" (clws.frame:get-opcode frame))))))
    (clws.frame::protocol-error	(err)
      (progn
	(v:warn :clws.connection "Received protocol error in read-frame: ~a" err)
	(close-connection connection clws.frame:+STATUS-CODE-PROTOCOL-ERROR+ "Protocol error")))
    (clws.connection:policy-violation-error (err)
      (progn
	(v:warn :clws.connection "Reveived policy violation in read-frame: ~a" err)
	(close-connection connection clws.frame:+STATUS-CODE-POLICY-VIOLATION+ "Policy violation")))
    (clws.frame:inconsistent-data-error (err)
	(progn
	  (v:warn :clws.connection "Reveived inconsistent data error in read-frame: ~a" err)
	  (close-connection connection clws.frame:+STATUS-CODE-INCONSISTENT-DATA+ "Inconsistent data")))
    (error (err)
      (progn
	(v:warn :clws.connection "Received error in read-frame: ~a" err)
	;; todo: extract status-code and reason from condition
	(close-connection connection clws.frame:+STATUS-CODE-UNEXPECTED-CONDITION+ "Internal server error")))))

(defun start-connection (connection &key (enter-dispatch-loop t))
  (bt:with-lock-held ((slot-value connection 'connection-state-lock))
    (setf (slot-value connection 'connection-state) :running))
  (let ((e nil))
    (handler-case
	(clws.handler:on-open-connection (slot-value connection 'connection-handler))
      (error (err)
	(progn
	  (v:warn :clws.connection "Received error while notifying handler: ~a" err)
	  (close-connection connection clws.frame:+STATUS-CODE-UNEXPECTED-CONDITION+ nil)
	  (setf e err))))
    (if (and (not e) enter-dispatch-loop)
      (bt:make-thread
       (lambda ()
	 (declare (optimize (debug 3) (speed 0) (space 0)))
	 (v:debug :clws.connection "Starting dispatch loop")
	 (loop
	    (read-frame connection)
	    (let ((state
		   (bt:with-lock-held ((slot-value connection 'connection-state-lock))
		     (slot-value connection 'connection-state))))
	      (if (eq :stopped state)
		  (progn
		    (v:debug :clws.connection "Dispatch loop has stopped")
		    (return))))))
       :name "Connection-Worker-Thread"))))


