(in-package :clws.socket.usocket)

;;
;; A usocket based socket implementation
;;

(defun socket-close (socket)
  (usocket:socket-close socket))

(defun socket-stream (socket)
  (usocket:socket-stream socket))

(defun socket-listen (host port)
  (usocket:socket-listen host port :element-type '(unsigned-byte 8)))

(defun socket-accept (socket)
  (usocket:socket-accept socket))

