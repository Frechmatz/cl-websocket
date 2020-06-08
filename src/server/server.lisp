(in-package :clws.server)

(defun make-websocketserver (host port)
  (let ((server (make-instance 'websocketserver)))
    (setf (slot-value server 'host) host)
    (setf (slot-value server 'port) port)
    server))

(defun add-connection (server connection)
  (assert (typep connection 'clws.server.connection::server-websocketconnection))
  (bt:with-lock-held ((slot-value server 'connections-lock))
    (push connection (slot-value server 'connections))
    (v:trace :clws.server "add-connection: connection count: ~a" (length (slot-value server 'connections)))))

(defun remove-connection (server connection)
  (v:trace :clws.server "Remove connection")
  (if (not (typep connection 'clws.server.connection:server-websocketconnection))
      (error "Not a server connection"))
  (bt:with-lock-held ((slot-value server 'connections-lock))
    (setf (slot-value server 'connections)
	  (remove-if
	   (lambda (item)
	     (eq (slot-value connection 'clws.server.connection::connection-id)
		 (slot-value item 'clws.server.connection::connection-id)))
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
			  (v:info :clws.server "Received connect request while stopping server. Closing socket.")
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
		(slot-value c 'clws.server.connection::connection-id))
	(clws.connection:close-connection
	 c
	 clws.frame:+STATUS-CODE-GOING-AWAY+
	 "Server is shutting down")))
    (v:info :clws.server "Closing server socket")
    (usocket:socket-close (slot-value server 'server-socket))
    (setf (slot-value server 'state) :stopped)
    (assert (eq 0 (length (slot-value server 'connections)))))
  (v:info :clws.server "WebsocketServer stopped"))
