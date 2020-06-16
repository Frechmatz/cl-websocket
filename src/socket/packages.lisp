(defpackage :clws.socket.implementation
  (:use :cl)
  (:export :socket-implementation)
  (:export :socket-close)
  (:export :socket-stream)
  (:export :socket-listen)
  (:export :socket-accept))
  
(defpackage :clws.socket
  (:use :cl)
  (:export :*socket-implementation*)
  (:export :socket-close)
  (:export :socket-stream)
  (:export :socket-listen)
  (:export :socket-accept))

(defpackage :clws.socket.implementation.usocket
  (:use :cl))

