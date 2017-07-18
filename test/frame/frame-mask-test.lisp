
(in-package :cl-websocket-test)


;; https://tools.ietf.org/html/rfc6455#section-5.7
;; A single-frame masked text message (contains Hello)
(define-test test-payload-processor-1 ()
	     (let ((p (clws.frame::get-payload-mask-processor #x37fa213d)))
	       (assert-true (eql (funcall p #x7f) (char-code #\H)))
	       (assert-true (eql (funcall p #x9f) (char-code #\e)))
	       (assert-true (eql (funcall p #x4d) (char-code #\l)))
	       (assert-true (eql (funcall p #x51) (char-code #\l)))
	       (assert-true (eql (funcall p #x58) (char-code #\o)))))

(define-test test-payload-processor-2 ()
	     (let ((p (clws.frame::get-payload-mask-processor nil)))
	       (assert-true (eql (funcall p 1) 1))
	       (assert-true (eql (funcall p 1) 1))
	       (assert-true (eql (funcall p 20) 20))
	       (assert-true (eql (funcall p 20) 20))))
