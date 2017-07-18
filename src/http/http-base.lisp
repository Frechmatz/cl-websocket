(in-package :clws.http)

(defclass http-base ()
  ((header :initform '())))

;; todo: handling of headers with same name

(defun add-header (base field-name field-value)
  (push (list field-name field-value) (slot-value base 'header)))

(defmethod initialize-instance :after ((base http-base) &key (header nil))
  (dolist (h header)
    (add-header base (first h) (second h))))

(defun get-header (base field-name)
  (second (assoc
	   field-name
	   (slot-value base 'header)
	   :test (lambda (a b) (string= (string-upcase a) (string-upcase b))))))
  
(defun for-each-header (base fn)
  (dolist (h (reverse (slot-value base 'header)))
    (funcall fn (first h) (second h))))

(defun is-header-value (request field-name value &key (ignore-case nil))
  (let ((v (get-header request field-name)))
    (if v
	(if ignore-case
	    (string= (string-upcase value) (string-upcase v))
	    (string= value v))
	nil)))

(defun pretty-print-headers (r)
  (let ((str (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s str)
      (for-each-header
       r
       (lambda (f v)
	 (format s "~a:~a~%" f v))))
    str))


