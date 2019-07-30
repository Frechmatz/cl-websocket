(in-package :cl-websocket-test)

(define-test test-do-connection-handlers-uri-path-1 ()
	     (flet ((make-test-http-request (uri)
		      (make-instance 'clws.http:http-request
				     :method clws.http:+GET+
				     :request-uri uri
				     :http-version "1.1"
				     :headers nil)))
	       (let ((server (make-instance 'test-server)))
		 (assert-true server)
		 (let ((handlers '()))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test"))
		       'silent-handler socket con h
		     (assert-true h)
		     (clws.server::add-connection server con)
		     (push h handlers))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test"))
		       'silent-handler socket con h
		     (assert-true h)
		     (assert-true (typep con 'clws.server.connection::server-websocketconnection))
		     (clws.server::add-connection server con)
		     (push h handlers))
		   (expect-server-handlers-by-uri server "/test" handlers)
		   (expect-server-handlers server handlers)
		   ))))



(define-test test-do-connection-handlers-uri-path-2 ()
	     (flet ((make-test-http-request (uri)
		      (make-instance 'clws.http:http-request
				     :method clws.http:+GET+
				     :request-uri uri
				     :http-version "1.1"
				     :headers nil)))
	       (let ((server (make-instance 'test-server)))
		 (assert-true server)
		 (let ((handlers '()) (all-handlers '()))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test"))
		       'silent-handler socket con h
		     (assert-true h)
		     (clws.server::add-connection server con)
		     (push h handlers)
		     (push h all-handlers))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test2"))
		       'silent-handler socket con h
		     (assert-true h)
		     (clws.server::add-connection server con)
		     (push h all-handlers))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test"))
		       'silent-handler socket con h
		     (assert-true h)
		     (assert-true (typep con 'clws.server.connection::server-websocketconnection))
		     (clws.server::add-connection server con)
		     (push h handlers)
		     (push h all-handlers))
		   (expect-server-handlers-by-uri server "/test" handlers)
		   (expect-server-handlers server all-handlers)
		   ))))

(define-test test-do-connection-handlers-handler-1 ()
	     (flet ((make-test-http-request (uri)
		      (make-instance 'clws.http:http-request
				     :method clws.http:+GET+
				     :request-uri uri
				     :http-version "1.1"
				     :headers nil)))
	       (let ((server (make-instance 'test-server)))
		 (assert-true server)
		 (let ((handlers '()) (connection-handler nil))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test"))
		       'silent-handler socket con h
		     (assert-true h)
		     (clws.server::add-connection server con)
		     (setf connection-handler h)
		     (push h handlers))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test"))
		       'silent-handler socket con h
		     (assert-true h)
		     (assert-true (typep con 'clws.server.connection::server-websocketconnection))
		     (clws.server::add-connection server con)
		     (push h handlers))
		   (expect-server-handlers-by-handler connection-handler handlers)
		   ))))


(define-test test-do-connection-handlers-handler-2 ()
	     (flet ((make-test-http-request (uri)
		      (make-instance 'clws.http:http-request
				     :method clws.http:+GET+
				     :request-uri uri
				     :http-version "1.1"
				     :headers nil)))
	       (let ((server (make-instance 'test-server)))
		 (assert-true server)
		 (let ((handlers '()) (connection-handler nil))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test"))
		       'silent-handler socket con h
		     (assert-true h)
		     (clws.server::add-connection server con)
		     (setf connection-handler h)
		     (push h handlers))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test2"))
		       'silent-handler socket con h
		     (assert-true h)
		     (clws.server::add-connection server con))
		   (with-test-connection
		       (:server server
				:connection-class clws.server.connection::server-websocketconnection
				:start-connection nil
				:http-request (make-test-http-request "/test"))
		       'silent-handler socket con h
		     (assert-true h)
		     (clws.server::add-connection server con)
		     (push h handlers))
		   (expect-server-handlers-by-handler connection-handler handlers)
		   ))))

