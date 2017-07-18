(in-package :clws.http)

(defun serialize-response (response stream)
  (clws.http::write-to-stream
   stream
   (http-response-http-version response)
   #\Space
   (http-response-status-code response)
   #\Space
   (http-response-reason-phrase response)
   13 10)
  (for-each-header
   response
   (lambda (field-name field-value)
     (clws.http::write-to-stream stream field-name #\: #\Space field-value 13 10)))
  (clws.http::write-to-stream stream 13 10))

