(in-package :cl-websocket-test)


(define-test test-calc-web-socket-accept-header ()
  "Tests calculation of Sec-Websocket-Accept header field value.
   Test values taken from RFC 6455"
  (assert-equal
   (clws.server::calc-web-socket-accept-header "dGhlIHNhbXBsZSBub25jZQ==")
   "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))

