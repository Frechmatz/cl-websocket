(in-package :clws.server)

(defclass websocketserver ()
  ((state :initform :instantiated)
   (state-lock :initform (bt:make-lock "websocketserver-state-lock"))
   (threadpool :initform nil)
   (host :initform nil)
   (port :initform nil)
   (server-socket :initform nil)
   (resource-handlers-lock :initform (bt:make-recursive-lock "resource-handlers-lock"))
   (resource-handlers :initform nil)
   (connections-lock :initform (bt:make-lock "websocketserver-connections-lock"))
   (connections :initform nil)))

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
			 (slot-value cur-con 'clws.server.connection::connection-http-request))))
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
       (assert (typep ,connection 'clws.server.connection::server-websocketconnection))
       (let ((,server (slot-value ,connection 'clws.server.connection::server))
	     (,http-request (slot-value ,connection 'clws.server.connection::connection-http-request)))
	 (let ((,uri (clws.http:get-uri-path ,http-request)))
	   (let ((,matching-cons
		  (bt:with-lock-held ((slot-value ,server 'clws.server::connections-lock))
		    (remove-if-not
		     (lambda (cur-con)
		       (string=
			  ,uri
			  (clws.http:get-uri-path
			   (slot-value cur-con 'clws.server.connection::connection-http-request))))
		       (slot-value ,server 'connections)))))
	     (dolist (,con-iter ,matching-cons)
	       (let ((,handler (slot-value ,con-iter 'clws.connection::connection-handler)))
		 ,@body))))))))

(defun get-uri-query-param (connection-handler param-name)
  (let ((connection (slot-value connection-handler 'clws.handler::connection)))
    (assert (typep connection 'clws.server.connection::server-websocketconnection))
    (clws.http:get-uri-query-param
     (slot-value connection 'clws.server.connection::connection-http-request)
     param-name)))
    
  
