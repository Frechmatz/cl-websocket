(in-package :clws.http)

(defun write-to-stream (stream &rest args)
  "stream A flexi-stream"
    (dolist (arg args)
      (cond
	((stringp arg)
	  (write-sequence 
	   (flexi-streams:string-to-octets arg)
	   stream))
	((typep arg 'sequence)
	 (write-sequence arg stream))
	((characterp arg)
	 (write-char arg stream))
	((numberp arg)
	 (write-byte arg stream)))))

