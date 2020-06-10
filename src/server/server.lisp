(in-package :clws.server)

;;
;; Ckasses
;;


;;(in-package :clws.server.connection)

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

(defclass server-websocketconnection (clws.connection:websocketconnection)
  ((server :initform nil)
   (connection-http-request :initform nil :documentation "The initial http request")
   (connection-id :initform (gensym))))


;;
;; Server
;;


(defgeneric start (websocketserver)
  (:documentation
   "Start the server"))

(defgeneric stop (websocketserver)
  (:documentation
   "Stop the server"))

(defgeneric register-resource-handler (websocketserver uri-path class options)
  (:documentation
   "Register a resource handler"))

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
		  (bt:with-lock-held ((slot-value ,server 'clws.server::connections-lock))
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

;;(in-package :clws.server.connection)

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


;;
;;
;;



(defun make-websocketserver (host port)
  (let ((server (make-instance 'websocketserver)))
    (setf (slot-value server 'host) host)
    (setf (slot-value server 'port) port)
    server))

(defun add-connection (server connection)
  (assert (typep connection 'server-websocketconnection))
  (bt:with-lock-held ((slot-value server 'connections-lock))
    (push connection (slot-value server 'connections))
    (v:trace :clws.server "add-connection: connection count: ~a" (length (slot-value server 'connections)))))

(defun remove-connection (server connection)
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
    (v:trace :clws.server "remove-connection: Remaining connection count: ~a" (length (slot-value server 'connections)))))

;;
;;
;;

(define-condition resource-already-registered-error (error) ())

(defclass resource-handler ()
  ((uri-path :initform nil)
   (class :initform nil)
   (options :initform nil)))

(defgeneric get-resource-handler-uri-path (resource-handler))
(defgeneric get-resource-handler-class (resource-handler))
(defgeneric get-resource-handler-options (resource-handler))

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

(defmethod register-resource-handler ((server websocketserver) uri-path class options)
  (bt:with-recursive-lock-held ((slot-value server 'resource-handlers-lock))
    (if (find-resource-handler server uri-path)
      (error (make-condition
	      'clws.server:resource-already-registered-error
	      :format-control "Resource handler already registered: ~a"
	      :format-arguments (list uri-path))))
    (push (make-instance 'resource-handler :uri-path uri-path :class class :options options)
	  (slot-value server 'resource-handlers))))

(defun find-resource-handler (server uri-path)
  (bt:with-recursive-lock-held ((slot-value server 'resource-handlers-lock))
    (find-if (lambda (i) (string= (get-resource-handler-uri-path i) uri-path))
	     (slot-value server 'resource-handlers))))
  

;;
;;
;;

(defmethod clws.server:start ((server websocketserver))
  (v:info :clws.server "Starting WebsocketServer")
  (bt:with-lock-held ((slot-value server 'state-lock))
    ;; todo: check, if already running or stopped
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
			  ;; (cl-threadpool:add-job
			  ;; (slot-value server 'threadpool)
			  ;; (clws.server.request-processor:get-processor server socket))
			  (let ((worker
				 (clws.server.request-processor:get-processor server socket)))
			    ;; Create a thread as preliminary workaround in order to get
			    ;; rid of cl-threadpool dependency
			    ;; TODO Implement accept-handler as synchronous function
			    (bt:make-thread worker :name "Accept-Handler-Thread"))
			    

			  ))))
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
    ;; todo: check, if already stopping or stopped
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
