(in-package :clws.socket.implementation)

(defclass socket-implementation () ())

(defgeneric socket-stream (socket-implementation socket) (:documentation ""))
(defgeneric socket-close (socket-implementation socket) (:documentation ""))
(defgeneric socket-listen (socket-implementation host port) (:documentation "Returns a socket"))
(defgeneric socket-accept (socket-implementation socket) (:documentation "Returns a socket"))
