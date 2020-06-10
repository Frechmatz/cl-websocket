(in-package :clws.server)

;;
;; Classes
;;

(defclass websocketserver ()
  ((state :initform :instantiated)
   (state-lock :initform (bt:make-lock "websocketserver-state-lock"))
   (host :initform nil)
   (port :initform nil)
   (server-socket :initform nil)
   (resource-handlers-lock :initform (bt:make-recursive-lock "resource-handlers-lock"))
   (resource-handlers :initform nil)
   (connections-lock :initform (bt:make-lock "websocketserver-connections-lock"))
   (connections :initform nil)))

(defgeneric start (websocketserver) (:documentation  "Start the server"))
(defgeneric stop (websocketserver) (:documentation "Stop the server"))
(defgeneric register-resource-handler (websocketserver uri-path class options)
  (:documentation "Register a resource handler"))
(defgeneric add-connection (websocketserver connection))
(defgeneric remove-connection (websocketserver connection))
(defgeneric register-resource-handler (websocketserver uri-path class options))
(defgeneric find-resource-handler (websocketserver uri-path))

(defclass server-websocketconnection (clws.connection:websocketconnection)
  ((server :initform nil)
   (connection-http-request :initform nil :documentation "The initial http request")
   (connection-id :initform (gensym))))

(defclass resource-handler ()
  ((uri-path :initform nil)
   (class :initform nil)
   (options :initform nil)))

(defgeneric get-resource-handler-uri-path (resource-handler))
(defgeneric get-resource-handler-class (resource-handler))
(defgeneric get-resource-handler-options (resource-handler))

(define-condition resource-already-registered-error (error) ())

;;
;; Accept handler
;;

(defun calc-web-socket-accept-header (str)
  "Calculates the Sec-WebSocket-Accept header
  str The value of the Sec-WebSocket-Key header field" 
  (let ((digester (ironclad:make-digest :SHA1)))
    (ironclad:update-digest
     digester
     (ironclad:ascii-string-to-byte-array
      (concatenate 'string str "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
    (cl-base64:usb8-array-to-base64-string (ironclad:produce-digest digester))))

(defun may-accept-connection (http-request)
  (and (clws.http:is-get-request http-request)
       (clws.http:get-header http-request "host")
       (clws.http:is-header-value http-request "upgrade" "websocket" :ignore-case t)
       ;; TODO implement according to spec
       (clws.http:is-header-value http-request "connection" "upgrade" :ignore-case t)
       (clws.http:get-header http-request "Sec-WebSocket-Key")
       (clws.http:is-header-value http-request "Sec-WebSocket-Version" "13")))

(defun send-accept-response (http-request stream)
  (let ((response
	 (make-instance
	  'clws.http:http-response
	  :version "HTTP/1.1"
	  :status-code "101"
	  :header (list
		   (list "upgrade" "websocket")
		   (list "connection" "upgrade")
		   (list "sec-websocket-accept"
			 (calc-web-socket-accept-header
			  (clws.http:get-header http-request "Sec-WebSocket-Key")))))))
    (clws.http:serialize-response response stream)
    (finish-output stream)))

(defun handle-accept (server socket)
  "Handle a connection request.
   - reads the HTTP request from the socket
   - validates the request
   - looks up a connection handler and instantiates it
   - sends back an acknowledge message to the client
   - adds the connection handler instance to the server
   - starts the connection handler.
   - closes the socket on any error"
  (v:trace :clws.server "Processing connection request")
  (flet ((instantiate-connection (server http-request socket)
	   "Returns a connection instance or nil if for the given
           http-request no connection handler matches."
	   ;; (declare (optimize (debug 3) (speed 0) (space 0)))
	   (let ((resource-handler
		  (find-resource-handler
		   server
		   (clws.http:get-uri-path http-request))))
	     (if resource-handler
		 (let ((c (make-instance
			   'server-websocketconnection
			   :server server
			   :handler (make-instance (get-resource-handler-class resource-handler))
			   :http-request http-request
			   :socket socket
			   :options (get-resource-handler-options resource-handler))))
		   ;; attach connection to handler
		   (setf (slot-value
			  (slot-value c 'clws.connection:connection-handler)
			  'clws.handler::connection) c)
		   c))))
	 (read-request (stream)
	   "Helper function to read a http request"
	   (let ((http-request (clws.http:parse-request stream)))
	     (v:trace
	      :clws.server
	      "~%Parsed Http-Request:~%~a~%" (clws.http:pretty-print-http-request http-request))
	     http-request))
	 (close-socket (socket)
	   "Helper function to close a socket"
	   (v:trace :clws.server "Closing connection request socket")
	   (handler-case
	       (usocket:socket-close socket)
	     (condition (err)
	       (v:trace :clws.server "Got error on closing socket: ~a" err)))))
    (handler-case
	(progn
	  (let ((s (make-instance 'clws.socket:connection-socket-usocket :socket socket)))
	    (let ((http-request (read-request (clws.socket:connection-socket-socket-stream s))))
	      (if (may-accept-connection http-request)
		  (progn 
		    (let ((c
			   (instantiate-connection
			    server
			    http-request
			    s)))
		      (if c
			  (handler-case
			      (progn 
				(send-accept-response
				 http-request
				 (clws.socket:connection-socket-socket-stream s))
				(add-connection server c)
				(clws.connection:start-connection c))
			    (condition (err)
			      (progn
				(v:warn :clws.server
					"Error while sending connect response: ~a" err)
				(close-socket socket))))
			  (progn 
			    (v:debug :clws.server "No handler found.")
			    (close-socket socket)))))
		  (progn
		    (v:debug :clws.server "Not a valid connection request")
		    (close-socket socket))))))
      (condition (err)
	(progn
	  (v:warn :clws.server
		  "Error while processing connection request: ~a" err)
	  (close-socket socket))))))

;;
;; Server
;;

(defmacro do-connection-handlers-by-uri (server uri-path handler &body body)
  (let ((cons (gensym)) (con (gensym)))
    `(let ((,cons (bt:with-lock-held ((slot-value ,server 'connections-lock))
		    (remove-if-not
		     (lambda (cur-con)
		       (string=
			,uri-path
			(clws.http:get-uri-path
			 (slot-value cur-con 'connection-http-request))))
		     (slot-value ,server 'connections)))))
	 (dolist (,con ,cons)
	   (let ((,handler (slot-value ,con 'clws.connection::connection-handler)))
	     ,@body)))))

(defmacro do-connection-handlers (server handler &body body)
  (let ((cons (gensym)) (con (gensym)))
    `(let ((,cons (bt:with-lock-held ((slot-value ,server 'connections-lock))
		    (copy-list (slot-value ,server 'connections)))))
	 (dolist (,con ,cons)
	   (let ((,handler (slot-value ,con 'clws.connection::connection-handler)))
	     ,@body)))))

(defmacro do-connection-handlers-by-handler (connection-handler handler &body body)
  (let ((matching-cons (gensym))
	(connection (gensym))
	(uri (gensym))
	(server (gensym))
	(con-iter (gensym))
	(http-request (gensym)))
    `(let ((,connection (slot-value ,connection-handler 'clws.handler::connection)))
       (assert (typep ,connection 'server-websocketconnection))
       (let ((,server (slot-value ,connection 'server))
	     (,http-request (slot-value ,connection 'connection-http-request)))
	 (let ((,uri (clws.http:get-uri-path ,http-request)))
	   (let ((,matching-cons
		  (bt:with-lock-held ((slot-value ,server 'connections-lock))
		    (remove-if-not
		     (lambda (cur-con)
		       (string=
			  ,uri
			  (clws.http:get-uri-path
			   (slot-value cur-con 'connection-http-request))))
		       (slot-value ,server 'connections)))))
	     (dolist (,con-iter ,matching-cons)
	       (let ((,handler (slot-value ,con-iter 'clws.connection::connection-handler)))
		 ,@body))))))))

(defun get-uri-query-param (connection-handler param-name)
  (let ((connection (slot-value connection-handler 'clws.handler::connection)))
    (assert (typep connection 'server-websocketconnection))
    (clws.http:get-uri-query-param
     (slot-value connection 'connection-http-request)
     param-name)))

;;
;; Connection
;;

(defmethod initialize-instance :after ((c server-websocketconnection)
				       &key (server nil)
					 http-request
					 &allow-other-keys)
  (setf (slot-value c 'connection-http-request) http-request)
  (setf (slot-value c 'server) server))
	   
(defmethod clws.connection:close-connection ((con server-websocketconnection) status-code reason)
  (unwind-protect
       (progn
	 (v:debug :clws.server "Close server connection")
	 (funcall #'remove-connection (slot-value con 'server) con)
	 (call-next-method))))

;;
;; Resource handler
;;

(defmethod initialize-instance :after
    ((rh resource-handler) &key uri-path class options)
  (setf (slot-value rh 'uri-path) uri-path)
  (setf (slot-value rh 'class) class)
  (setf (slot-value rh 'options) options))

(defmethod get-resource-handler-uri-path ((rh resource-handler))
  (slot-value rh 'uri-path))

(defmethod get-resource-handler-class ((rh resource-handler))
  (slot-value rh 'class))

(defmethod get-resource-handler-options ((rh resource-handler))
  (slot-value rh 'options))


;;
;; Server
;;

(defun make-websocketserver (host port)
  (let ((server (make-instance 'websocketserver)))
    (setf (slot-value server 'host) host)
    (setf (slot-value server 'port) port)
    server))

(defmethod register-resource-handler ((server websocketserver) uri-path class options)
  (bt:with-recursive-lock-held ((slot-value server 'resource-handlers-lock))
    (if (find-resource-handler server uri-path)
      (error (make-condition
	      'resource-already-registered-error
	      :format-control "Resource handler already registered: ~a"
	      :format-arguments (list uri-path))))
    (push (make-instance 'resource-handler :uri-path uri-path :class class :options options)
	  (slot-value server 'resource-handlers))))

(defmethod find-resource-handler ((server websocketserver) uri-path)
  (bt:with-recursive-lock-held ((slot-value server 'resource-handlers-lock))
    (find-if (lambda (i) (string= (get-resource-handler-uri-path i) uri-path))
	     (slot-value server 'resource-handlers))))
  
(defmethod add-connection ((server websocketserver) connection)
  (assert (typep connection 'server-websocketconnection))
  (bt:with-lock-held ((slot-value server 'connections-lock))
    (push connection (slot-value server 'connections))
    (v:trace :clws.server
	     "add-connection: connection count: ~a" (length (slot-value server 'connections)))))

(defmethod remove-connection ((server websocketserver) connection)
  (v:trace :clws.server "Remove connection")
  (if (not (typep connection 'server-websocketconnection))
      (error "Not a server connection"))
  (bt:with-lock-held ((slot-value server 'connections-lock))
    (setf (slot-value server 'connections)
	  (remove-if
	   (lambda (item)
	     (eq (slot-value connection 'connection-id)
		 (slot-value item 'connection-id)))
	   (slot-value server 'connections)))
    (v:trace :clws.server
	     "remove-connection: Remaining connection count: ~a"
	     (length (slot-value server 'connections)))))

(defmethod clws.server:start ((server websocketserver))
  (v:info :clws.server "Starting WebsocketServer")
  (bt:with-lock-held ((slot-value server 'state-lock))
    ;; TODO check, if already running or stopped
    (setf (slot-value server 'state) :running)
    (setf (slot-value server 'server-socket)
	  (usocket:socket-listen
	   (slot-value server 'host)
	   (slot-value server 'port)
	   :element-type '(unsigned-byte 8)))
    (bt:make-thread
     (lambda ()
       (v:info :clws.server "Socket-Accept listener thread has started")
       (loop
	  (let ((socket nil))
	    (handler-case
		(progn
		  (setf socket (usocket:socket-accept (slot-value server 'server-socket)))
		  (v:trace :clws.server "Received connect request")
		  (bt:with-lock-held ((slot-value server 'state-lock))
		    (if (not (eq :running (slot-value server 'state)))
			(progn
			  (v:info :clws.server
				  "Received connect request while stopping server. Closing socket.")
			  (usocket:socket-close socket)
			  (return))
			(progn 
			  (handle-accept server socket)))))
	      (condition (err)
		(progn
		  (if socket
		      (usocket:socket-close socket))
		  (v:warn :clws.server "Received signal from server-socket: ~S" err)
		  (return)
		  )))))
       (v:info :clws.server "Socket-accept listener thread has stopped"))
     :name "Accept-listener thread"))
  (v:info :clws.server "WebsocketServer started"))

(defmethod clws.server:stop ((server websocketserver))
  (bt:with-lock-held ((slot-value server 'state-lock))
    ;; TODO check, if already stopping or stopped
    (setf (slot-value server 'state) :stopping)
    (v:info :clws.server "Stopping WebsocketServer...")
    (v:info :clws.server "Closing connections...")
    (let ((con-buf '()))
      (dolist (c (slot-value server 'connections))
	(push c con-buf))
      ;; Close all connections
      (dolist (c con-buf)
	(v:info :clws.server
		"Closing connection ~a"
		(slot-value c 'connection-id))
	(clws.connection:close-connection
	 c
	 clws.frame:+STATUS-CODE-GOING-AWAY+
	 "Server is shutting down")))
    (v:info :clws.server "Closing server socket")
    (usocket:socket-close (slot-value server 'server-socket))
    (setf (slot-value server 'state) :stopped)
    (assert (eq 0 (length (slot-value server 'connections)))))
  (v:info :clws.server "WebsocketServer stopped"))
