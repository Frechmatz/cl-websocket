;;
;; Tests if invalid http requests are detected.
;;
;;
(in-package :cl-websocket-test)


(define-test test-parse-request-fail-1 ()
	     "Missing CRLF after request line"
	     (expect-eof-exception
	       (clws.http:parse-request
		(create-parser-input-stream
		 "GET URI V"))))

(define-test test-parse-request-fail-2 ()
	     "Missing CRLF that terminates the header section"
	     (expect-eof-exception
	       (clws.http:parse-request
		(create-parser-input-stream
		 "GET URI V" 13 10))))

(define-test test-parse-request-fail-3 ()
	     "Too many tokens in request line"
	     (expect-parse-exception
	       (clws.http:parse-request
		(create-parser-input-stream
		 "GET U V STUFF" 13 10 13 10))))

(define-test test-parse-request-fail-4 ()
	     "Missing tokens in request line"
	     (expect-eof-exception
	       (clws.http:parse-request
		(create-parser-input-stream
		 "GET U" 13 10 13 10))))

(define-test test-parse-request-fail-5 ()
	     "Last token of request line not properly terminated"
	     (expect-parse-exception
		 (clws.http:parse-request
		  (create-parser-input-stream
		   "GET U V " 13 10 13 10))))

