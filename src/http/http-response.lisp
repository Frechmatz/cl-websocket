(in-package :clws.http)

(defclass http-response (http-base)
  ((version :initform nil :accessor http-response-http-version)
   (status-code :initform nil :accessor http-response-status-code)
   (reason-phrase :initform nil :accessor http-response-reason-phrase)))

(defmethod initialize-instance :after ((r http-response)
				       &key
					 (version nil)
					 (status-code nil)
					 (reason-phrase nil))
  (setf (slot-value r 'version) version)
  (setf (slot-value r 'status-code) status-code)
  (setf (slot-value r 'reason-phrase) reason-phrase))

  

