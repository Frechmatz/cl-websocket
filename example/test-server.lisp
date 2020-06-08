
(defpackage :test-server
  (:use :cl))
(in-package :test-server)

(defparameter *server* nil)

(defclass test-handler (clws.handler:connection-handler) ())

(defmethod clws.handler:on-open-connection ((handler test-handler))
  (v:info :test-server "test-handler:on-open-connection")
  (let ((nickname (clws.server:get-uri-query-param handler "name")))
    (if (not nickname)
	(setf nickname "anonymous"))
    (clws.server:do-connection-handlers-by-handler handler cur-handler
      (clws.handler:send-text-message
       cur-handler
       (format nil "~a has joined the server" nickname)))))

(defmethod clws.handler:on-close-connection ((handler test-handler) status-code reason)
  (v:info :test-server "test-handler:on-close-connection. StatusCode: ~a Reason: ~a" status-code reason))

(defmethod clws.handler:on-text-message ((handler test-handler) message)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (v:info :test-server "test-handler:on-text-message: ~a~%" (if (< 1000 (length message)) "<A long message>" message))
  (if (string= message "CLOSE")
      (clws.handler:close-connection handler 1000 "Server closed connection")
      (clws.handler:send-text-message handler message)))

(defmethod clws.handler:on-binary-message ((handler test-handler) message)
  (v:info :test-server "test-handler:on-binary-message. Length of payload is ~a~%" (length message))
  (clws.handler:send-binary-message handler message))

(defun start ()
  (setf (v:repl-level) :trace)
  (if *server*
      (format t "Test Server is already running. To stop the server enter test-server::stop")
      (progn
	(setf *server* (clws.server:make-websocketserver "localhost" 7998))
	(clws.server:register-resource-handler
	 *server*
	 "/test"
	 'test-handler
	 '((:max-payload-length 65536)
	   (:max-frame-count 10)))
	(format t "Starting Test-Server...~%")
	(clws.server:start *server*)
	(format t "Test-Server started~%")
	;;(v:info :cl-websocket "The Test-Server will be stopped in 30s...")
	;;(sleep 30)
	;;(clws.server:stop test-server)))
	)))

(defun stop ()
  (if (not *server*)
      (format t "The server has not been started")
      (let ((srv *server*))
	(setf *server* nil)
	(format t "Stopping the server...~%")
	(clws.server:stop srv)
	(format t "The server has stopped~%"))))

	

