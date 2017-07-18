(in-package :clws.handler)

(defclass connection-handler ()
  ((connection :initform nil)))

(defgeneric on-open-connection (connection-handler))
(defgeneric on-close-connection (connection-handler status-code reason))
(defgeneric on-text-message (connection-handler payload))
(defgeneric on-binary-message (connection-handler payload))

(defgeneric close-connection (connection-handler status-code reason))
(defmethod close-connection ((handler connection-handler) status-code reason)
    (clws.connection::close-connection (slot-value handler 'connection) status-code reason))

(defgeneric send-text-message (connection-handler message))
(defmethod send-text-message ((handler connection-handler) message)
  (clws.connection::connection-send-text-message (slot-value handler 'connection) message))

(defgeneric send-binary-message (connection-handler message))
(defmethod send-binary-message ((handler connection-handler) message)
  (clws.connection::connection-send-binary-message (slot-value handler 'connection) message))


