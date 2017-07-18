(in-package :cl-websocket-test)

;;
;; A flexi-stream based mock socket
;;
(defclass connection-socket-mock (clws.socket:connection-socket)
  ((connection-socket-stream :initform nil)
   ;; an output stream from which the socket will read
   (socket.input
    :initform
    (flexi-streams:make-in-memory-output-stream))
   (socket.output-buffer
    :initform
    (flexi-streams:make-in-memory-output-stream))
   (called-sock.output :initform nil)))

(defmethod initialize-instance :after ((s connection-socket-mock) &rest args)
  (declare (ignore args)))

(defmethod mock-socket.input ((s connection-socket-mock))
  (slot-value s 'socket.input))

(defmethod mock-socket.output ((s connection-socket-mock))
  ;; one shot only
  (if (slot-value s 'called-sock.output)
      (error "Already requested output stream from mock-socket"))
  (setf (slot-value s 'called-sock.output) t)
  (flexi-streams:make-flexi-stream
   (flexi-streams:make-in-memory-input-stream
    (flexi-streams:get-output-stream-sequence (slot-value s 'socket.output-buffer)))
   :external-format (flexi-streams:make-external-format :us-ascii :eol-style :crlf)))

(defmethod clws.socket:connection-socket-close ((s connection-socket-mock))
  ;; todo: flush the streams
  nil)

(defmethod clws.socket:connection-socket-socket-stream ((s connection-socket-mock))
  (if (not (slot-value s 'connection-socket-stream))
      (setf (slot-value s 'connection-socket-stream)
	     (make-two-way-stream
	      ;; create input-stream out of socket.input (which is an output stream)
	      (flexi-streams:make-flexi-stream
	       (flexi-streams:make-in-memory-input-stream
		(flexi-streams:get-output-stream-sequence (slot-value s 'socket.input)))
	       :external-format (flexi-streams:make-external-format :us-ascii :eol-style :crlf))
	      ;; create output-stream out of socket.output
	      (flexi-streams:make-flexi-stream
	       (slot-value s 'socket.output-buffer)
	       :external-format (flexi-streams:make-external-format :us-ascii :eol-style :crlf)))))
  (slot-value s 'connection-socket-stream))

