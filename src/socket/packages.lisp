(defpackage :clws.socket
  (:use :cl)
  (:export :socket-close)
  (:export :socket-stream)
  (:export :socket-listen)
  (:export :socket-accept))

(defpackage :clws.socket.socket-impl-wrapper
  (:use :cl)
  (:export *socket-implementation-package-name*)
  (:export :init)
  (:export :socket-close)
  (:export :socket-stream)
  (:export :socket-listen)
  (:export :socket-accept))

(defpackage :clws.socket.usocket
  (:use :cl)
  (:export :socket-close)
  (:export :socket-stream)
  (:export :socket-listen)
  (:export :socket-accept))

