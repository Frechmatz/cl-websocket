(in-package :clws.http)

(defparameter +OPTIONS+ "OPTIONS")
(defparameter +GET+ "GET")
(defparameter +HEAD+ "HEAD")
(defparameter +POST+ "POST")
(defparameter +PUT+ "PUT")
(defparameter +DELETE+ "DELETE")
(defparameter +TRACE+ "TRACE")
(defparameter +CONNECT+ "CONNECT")

(defclass http-request (http-base)
  ((method :initform nil :accessor http-request-method)
   (request-uri :initform nil :accessor http-request-request-uri)
   (parsed-request-uri :initform nil)
   (http-version :initform nil :accessor http-request-http-version)))

(defmethod initialize-instance :after ((r http-request) &key
				       method request-uri
				       http-version headers)
  (setf (slot-value r 'method) method)
  (setf (slot-value r 'request-uri) request-uri)
  (setf (slot-value r 'parsed-request-uri) (quri:uri request-uri))
  (setf (slot-value r 'http-version) http-version)
  (dolist (h headers)
    (add-header r (first h) (second h))))

(defun is-get-request (r)
  (string= "GET" (http-request-method r)))

(defun get-uri-path (request)
  (quri:uri-path (slot-value request 'parsed-request-uri)))

(defun pretty-print-http-request (r)
  (format nil "Method: ~a~%Request-Uri: ~a~%Http-Version: ~a~%~a"
	  (http-request-method r)
	  (http-request-request-uri r)
	  (http-request-http-version r)
	  (pretty-print-headers r)))

;;
;; no get-uri-query-params yet due to different representations
;; of headers (list of list) and query params (list of cons) 
;;

(defun get-uri-query-param (request param-name)
  (let ((r (assoc param-name (quri::uri-query-params (slot-value request 'parsed-request-uri)) :test #'string=)))
    (cdr r)))

