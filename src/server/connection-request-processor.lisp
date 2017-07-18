(in-package :clws.server.request-processor)

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
       ;; todo: implement according to spec
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

(defun instantiate-connection (server http-request socket)
  "Returns a connection instance or nil if for the given
   http-request no connection handler matches."
  ;; (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((resource-handler
	 (clws.server::find-resource-handler
	  server
	  (clws.http:get-uri-path http-request))))
    (if resource-handler
	(let ((c (make-instance
		  'clws.server.connection:server-websocketconnection
		  :server server
		  :handler (make-instance (clws.server::get-resource-handler-class resource-handler))
		  :http-request http-request
		  :socket socket
		  :options (clws.server::get-resource-handler-options resource-handler))))
	  ;; attach connection to handler
	  (setf (slot-value
		 (slot-value c 'clws.connection:connection-handler)
		 'clws.handler::connection) c)
	  c))))

;; Helper function to close a socket
(defun close-socket (socket)
  (v:trace :clws.server.request-processor "Closing connection request socket")
  (handler-case
      (usocket:socket-close socket)
    (condition (err)
      (v:trace :clws.server.request-processor "Got error on closing socket: ~a" err)
      )))

;; Helper function to read a http request and log it.
(defun read-request (stream)
  (let ((http-request (clws.http:parse-request stream)))
    ;; TODO Better logging https://httpd.apache.org/docs/1.3/logs.html
    (v:trace
     :clws.server.request-processor "~%Parsed Http-Request:~%~a~%" (clws.http:pretty-print-http-request http-request))
    http-request))

(defun get-processor (server socket)
  "Returns a function that accepts (or not) a websocket 
   connect request.
   The function 
   - reads an HTTP request from the socket
   - validates the request
   - looks up a connection handler and instantiates it
   - sends back an acknowledge message to the client
   - adds the connection handler instance to the server
   - starts the connection handler.
   - closes the socket on any error"
  (let ((worker
	 (lambda ()
	   (v:trace :clws.server.request-processor "Processing connection request")
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
				       (clws.server::add-connection server c)
				       (clws.connection:start-connection c))
				   (condition (err)
				     (progn
				       (v:warn :clws.server.request-processor
						"Error while sending connect response: ~a" err)
				       (close-socket socket))))
				 (progn 
				   (v:debug :clws.server.request-processor "No handler found.")
				   (close-socket socket)))))
			 (progn
			   (v:debug :clws.server.request-processor "Not a valid connection request")
			   (close-socket socket))))))
	     (condition (err)
	       (progn
		 (v:warn :clws.server.request-processor "Error while processing connection request: ~a" err)
		 (close-socket socket))))
	   )))
    worker))

