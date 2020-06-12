(in-package :clws.socket)


;;
;; Socket layer of cl-websocket
;;

(defclass socket ()
  ((native-socket :initform nil)
   (native-stream :initform nil)
   (flexi-stream :initform nil))
  (:documentation "Representation of a socket"))

(defun assert-is-socket (s)
  (if (not (typep s 'socket))
      (error (format nil "Socket ~a is not of type clws.socket::socket" s))))

;;
;; The socket interface
;;

(defun socket-stream (socket)
  "Get the i/o stream of a socket. Returns a flexi stream."
  (assert-is-socket socket)
  (slot-value socket 'flexi-stream))

(defun socket-close (socket)
  "Close a socket. Flushes the socket stream and closes the socket."
  (assert-is-socket socket)
  (if (slot-value socket 'flexi-stream)
      (finish-output (slot-value socket 'flexi-stream)))
  (force-output (slot-value socket 'native-stream))
  (clws.socket.socket-impl-wrapper:socket-close (slot-value socket 'native-socket)))

(defun socket-listen (host port)
  "Open host/port. Returns a socket"
  (let ((s (clws.socket.socket-impl-wrapper:socket-listen host port)))
    (make-instance 'socket :native-socket s)))

(defun socket-accept (socket)
  "Blocking function that waits for connection requests. Returns a socket representing a connection."
  (assert-is-socket socket)
  (let ((s (clws.socket.socket-impl-wrapper:socket-accept (slot-value socket 'native-socket))))
    (make-instance 'socket :native-socket s)))

(defmethod initialize-instance :after ((s socket) &key native-socket)
  (setf (slot-value s 'native-socket) s)
  (setf (slot-value s 'native-stream)
	(clws.socket.socket-impl-wrapper:socket-stream native-socket))
  (setf (slot-value s 'stream)
	(flexi-streams:make-flexi-stream
	 (slot-value s 'native-stream)
	 :external-format (flexi-streams:make-external-format :us-ascii :eol-style :crlf))))

