(in-package :clws.http)


(defun throw-parse-error ()
  (error 'parse-error :text (format nil "Parse error")))


;; todo: create a macro
(defun is-ws (ch)
  (or (char= ch #\Space) (char= ch #\Tab)))

(defun eat-newlines (inputstream)
  "Consume line breaks. Returns on non-linebreak or EOF."
  (loop
     (let ((ch (read-char inputstream nil nil)))
       (if (not ch)
	   (return))
       (if (not (char= ch #\Newline))
	   (progn
	     (unread-char ch inputstream)
	     (return))))))

(defun eat-ws (inputstream)
  "Consume whitespace characters. Returns on non-ws character or EOF."
  (loop
     (let ((ch (read-char inputstream nil nil)))
       (if (not ch)
	   (return))
       (if (not (is-ws ch))
	   (progn
	     (unread-char ch inputstream)
	     (return))))))

(defun next-char-ws-p (inputstream)
  "Return true if the next character of the stream represents whitespace."
  (let ((ch (read-char inputstream nil nil)))
    (if (not ch)
	nil
	(progn
	  (unread-char ch inputstream)
	  (is-ws ch)))))

(defun expect-token (inputstream separator)
  "Reads a token from the stream. A token must not contain whitespace. 
   Returns the token."
  (let ((str (make-array 50 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
       (let ((ch (read-char inputstream)))
	 (if (and (char= ch separator) (= (length str) 0))
	     (throw-parse-error))
	 (if (char= ch separator)
	     (return))
	 (if (is-ws ch)
	     (throw-parse-error))
	 (vector-push-extend ch str)))
    str))

(defun expect-newline-terminated-string (inputstream &key (allow-folding nil))
  "Reads a linebreak terminated string.
   Returns the string."
  (let ((str (make-array 50 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
       (let ((ch (read-char inputstream)))
	 (if (char= ch #\Newline)
	     (progn
	       (if (not allow-folding)
		   (return))
	       (if (not (next-char-ws-p inputstream))
		   (return))
	       (eat-ws inputstream)
	       (setf ch #\Space)))
	 (vector-push-extend ch str)))
    str))
