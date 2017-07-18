(in-package :cl-websocket-test)


(define-test test-parse-request-line ()
	     (let ( (r (clws.http:parse-request
			(create-parser-input-stream
			 "METHOD REQUEST-URI HTTP-VERSION" 13 10
			 13 10))))
	       (assert-true (string= "METHOD" (clws.http:http-request-method r)))
	       (assert-true (string= "REQUEST-URI" (clws.http:http-request-request-uri r)))
	       (assert-true (string= "HTTP-VERSION" (clws.http:http-request-http-version r)))
	       (assert-true (not (clws.http:get-header r "some-header")))
	       ))


(define-test test-parse-request-line-leading-newlines ()
	     (let ( (r (clws.http:parse-request
			(create-parser-input-stream
			 13 10
			 13 10
			 "METHOD REQUEST-URI HTTP-VERSION" 13 10
			 13 10))))
	       (assert-true (string= "METHOD" (clws.http:http-request-method r)))
	       (assert-true (string= "REQUEST-URI" (clws.http:http-request-request-uri r)))
	       (assert-true (string= "HTTP-VERSION" (clws.http:http-request-http-version r)))
	       (assert-true (not (clws.http:get-header r "some-header")))
	       ))

(define-test test-parse-header-1 ()
  (let ( (r (clws.http:parse-request (create-parser-input-stream
	  "METHOD REQUEST-URI HTTP-VERSION"
	  13 10
	  "user-agent: android 4.4" 13 10
	  "accept:     application/json" 13 10
	  "content-type:application/xml" 13 10
	  13 10
	  ))))
    (assert-true (string= "android 4.4" (clws.http:get-header r "user-agent")))
    (assert-true (string= "android 4.4" (clws.http:get-header r "USER-AGENT")))
    (assert-true (string= "android 4.4" (clws.http:get-header r "User-aGENT")))
    (assert-true (string= "application/json" (clws.http:get-header r "accept")))
    (assert-true (string= "application/xml" (clws.http:get-header r "content-type")))
    ))

(define-test test-parse-header-unfolding ()
  (let ( (r (clws.http:parse-request (create-parser-input-stream
	  "METHOD REQUEST-URI HTTP-VERSION"
	  13 10
	  "x-0: 0" 13 10
	  "x-folded-1: a" 13 10 "     b" 13 10
	  "x-1: 1" 13 10
	  "x-folded-2: cc" 13 10 9 9 "     dd" 13 10
	  "x-2: 2" 13 10
	  13 10
	  ))))
    (assert-true (string= "0" (clws.http:get-header r "x-0")))
    (assert-true (string= "1" (clws.http:get-header r "x-1")))
    (assert-true (string= "2" (clws.http:get-header r "x-2")))
    (assert-true (string= "a b" (clws.http:get-header r "x-folded-1")))
    (assert-true (string= "cc dd" (clws.http:get-header r "x-folded-2")))
    ))

