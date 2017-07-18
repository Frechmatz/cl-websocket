(in-package :clws.socket)


;;
;; Abstraction of a socket
;; connection-socket-close -- closes the socket
;; connection-socket-socket-stream -- a Flexi-Streams input/output stream
;;
(defclass connection-socket ()())

(defgeneric connection-socket-close (connection-socket))
(defgeneric connection-socket-socket-stream (connection-socket))

;;
;; a usocket based implementation of connection-socket
;;
(defclass connection-socket-usocket (connection-socket)
  ((socket :initform nil)
   (connection-socket-stream :initform nil)))

(defmethod initialize-instance :after ((s connection-socket-usocket) &key socket)
  (setf (slot-value s 'socket) socket)
  (setf (slot-value s 'connection-socket-stream) 
	(flexi-streams:make-flexi-stream
	 (usocket:socket-stream socket)
	 :external-format (flexi-streams:make-external-format :us-ascii :eol-style :crlf))))

(defmethod connection-socket-close ((s connection-socket-usocket))
  (finish-output (slot-value s 'connection-socket-stream))
  (force-output (usocket:socket-stream (slot-value s 'socket)))
  (usocket:socket-close (slot-value s 'socket)))

(defmethod connection-socket-socket-stream ((s connection-socket-usocket))
  (slot-value s 'connection-socket-stream))




