(in-package :clws.http)

;; todo: proper implementation
(defun validate-method (str)
  str)

(defun parse-request (inputstream)
  "Parses a HTTP request
   inputstream An input flexi-stream
   Returns an instance of http-request"
  (eat-newlines inputstream)
  (let ((method (validate-method (expect-token inputstream (character #\Space))))
	(request-uri (expect-token inputstream (character #\Space)))
	(http-version (expect-token inputstream (character #\Newline)))
	(headers (parse-headers inputstream)))
    (make-instance 'http-request :method method :request-uri request-uri :http-version http-version :headers headers)))


