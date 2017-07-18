(in-package :clws.server.connection)

(defclass server-websocketconnection (clws.connection:websocketconnection)
  ((server :initform nil)
   (connection-http-request :initform nil :documentation "The initial http request")
   (connection-id :initform (gensym))))

(defmethod initialize-instance :after ((c server-websocketconnection)
				       &key (server nil)
					 http-request
					 &allow-other-keys)
  (setf (slot-value c 'connection-http-request) http-request)
  (setf (slot-value c 'server) server))
	   
(defmethod clws.connection:close-connection ((con server-websocketconnection) status-code reason)
  (unwind-protect
       (progn
	 (v:debug :clws.server.connection "Close server connection")
	 (funcall #'clws.server::remove-connection (slot-value con 'server) con)
	 (call-next-method))))


