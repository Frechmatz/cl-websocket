(in-package :clws.socket.socket-impl-wrapper)

;;
;; Wrapper for invoking "native" socket implementations.
;; Delegates all calls to the package defined by *socket-implementation-package-name*
;;

(defparameter *socket-implementation-package-name* "CLWS.SOCKET.USOCKET")

(defparameter *impl-socket-close* nil)
(defparameter *impl-socket-stream* nil)
(defparameter *impl-socket-listen* nil)
(defparameter *impl-socket-accept* nil)

(defparameter *initialized* nil)
(defparameter *init-lock* (bt:make-lock "Socket wrapper initialization lock"))

(defun lookup-function (name)
  (let ((f (find-symbol name  *socket-implementation-package-name*)))
    (if (not f)
	(error (format nil "Symbol ~a not found in package ~a"
		       name *socket-implementation-package-name*)))
    f))

(defun init ()
  (bt:with-lock-held (*init-lock*))
    (if (not *initialized*)
	(progn
	  (setf *initialized* t)
	  (setf *impl-socket-close* (lookup-function "SOCKET-CLOSE"))
	  (setf *impl-socket-stream* (lookup-function "SOCKET-STREAM"))
	  (setf *impl-socket-listen* (lookup-function "SOCKET-LISTEN"))
	  (setf *impl-socket-accept* (lookup-function "SOCKET-ACCEPT")))))

(defun socket-stream (socket)
  "Return stream of socket.
   - socket A native socket as implemented by package *socket-implementation-package-name*"
  (apply *impl-socket-stream* socket))

(defun socket-close (socket)
  "Close a socket.
   - socket A native socket as implemented by package *socket-implementation-package-name*"
  (apply *impl-socket-close* socket))

(defun socket-listen (host port)
  "Open port for listening on accept request.
   Returns a native socket as implemented by package *socket-implementation-package-name*"
  (apply *impl-socket-listen* host port))

(defun socket-accept (socket)
  "Blocking function that returns a socket.
   - socket A native socket as implemented by package *socket-implementation-package-name*
   Returns a native socket as implemented by package *socket-implementation-package-name*"
  (apply *impl-socket-accept* (slot-value socket 'native-socket)))
