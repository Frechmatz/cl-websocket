(in-package :clws.socket.implementation.usocket)

;;
;; A usocket based socket implementation
;;

(defclass socket-implementation-usocket (clws.socket.implementation:socket-implementation) ())

(defmethod clws.socket.implementation:socket-stream
    ((us clws.socket.implementation:socket-implementation) socket)
  (usocket:socket-stream socket))

(defmethod clws.socket.implementation:socket-close
    ((us clws.socket.implementation:socket-implementation) socket)
  (usocket:socket-close socket))
  
(defmethod clws.socket.implementation:socket-listen
    ((us clws.socket.implementation:socket-implementation) host port) 
  (usocket:socket-listen host port :element-type '(unsigned-byte 8)))

(defmethod clws.socket.implementation:socket-accept
    ((us clws.socket.implementation:socket-implementation) socket) 
  (usocket:socket-accept socket))

