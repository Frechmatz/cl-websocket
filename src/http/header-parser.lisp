(in-package :clws.http)

(defun parse-header (inputstream)
  "Reads a single header definition from the stream"
  (let ((field-name (expect-token inputstream #\:)))
    (eat-ws inputstream)
    (list field-name (expect-newline-terminated-string
		 inputstream
		 :allow-folding t))))

(defun parse-headers (inputstream)
  "Reads all header definitions from the stream"
  (let ((headers '()))
    (loop
       (let ((ch (read-char inputstream)))
	 (if (char= ch #\Newline)
	     (return)
	     (unread-char ch inputstream))
	 (push (parse-header inputstream) headers)))
    headers))


