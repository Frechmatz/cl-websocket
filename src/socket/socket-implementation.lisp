(in-package :clws.socket.implementation)

(defclass socket-implementation () ())

(defgeneric socket-stream (socket-implementation socket)
  (:documentation ""))

(defgeneric socket-close (socket-implementation socket)
  (:documentation ""))

(defgeneric socket-listen (socket-implementation host port)
  (:documentation "Returns a listen socket"))

(defgeneric socket-accept (socket-implementation listen-socket)
  (:documentation "Returns a connection socket"))
