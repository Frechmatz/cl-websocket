
(defpackage :echo-server
  (:use :cl))
(in-package :echo-server)

(defparameter *server* nil)

(defclass echo-handler (clws.handler:connection-handler) ())

(defmethod clws.handler:on-open-connection ((handler echo-handler))
  (v:info :echo-server "echo-handler:on-open-connection"))

(defmethod clws.handler:on-close-connection ((handler echo-handler) status-code reason)
  (v:info :echo-server "echo-handler:on-close-connection. StatusCode: ~a Reason: ~a" status-code reason))

(defmethod clws.handler:on-text-message ((handler echo-handler) message)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (v:info :echo-server "echo-handler:on-text-message: ~a~%" (if (< 1000 (length message)) "<A long message>" message))
  (clws.handler:send-text-message handler message))

(defmethod clws.handler:on-binary-message ((handler echo-handler) message)
  (v:info :echo-server "echo-handler:on-binary-message. Length of payload is ~a~%" (length message))
  (clws.handler:send-binary-message handler message))

(defun start ()
  (setf (v:repl-level) :trace)
  (setf v:*process-locally* t)
  (if *server*
      (format t "Echo Server is already running. To stop the server enter echo-server::stop")
      (progn
	(setf *server* (clws.server:make-websocketserver "localhost" 9001 :threadpool-size 2))
	;;(clws.server:register-resource-handler *server* "/echo" 'echo-handler '())
	(clws.server:register-resource-handler *server* "/" 'echo-handler '())
	(format t "Starting Echo-Server...~%")
	(clws.server:start *server*)
	(format t "Echo-Server started~%")
	;;(v:info :cl-websocket "The Echo-Server will be stopped in 30s...")
	;;(sleep 30)
	;;(clws.server:stop echo-server)))
	)))

(defun stop ()
  (if (not *server*)
      (format t "The server has not been started")
      (let ((srv *server*))
	(setf *server* nil)
	(format t "Stopping the server...~%")
	(clws.server:stop srv)
	(format t "The server has stopped~%"))))

	

