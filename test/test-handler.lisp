(in-package :cl-websocket-test)

;;
;;
;;

(defclass silent-handler (clws.handler:connection-handler) ())

(defmethod clws.handler:on-open-connection ((handler silent-handler))
  (declare (ignore handler))
  nil)

(defmethod clws.handler:on-close-connection ((handler silent-handler) status-code reason)
  (declare (ignore handler))
  (declare (ignore status-code))
  (declare (ignore reason))
  nil)

(defmethod clws.handler:on-text-message ((handler silent-handler) message)
  (declare (ignore handler))
  nil)

(defmethod clws.handler:on-binary-message ((handler silent-handler) message)
  (declare (ignore handler))
  nil)


;;
;;
;;

(defclass echo-handler (silent-handler)
  ())

(defmethod clws.handler:on-text-message ((handler echo-handler) message)
  (v:debug :cl-websocket-test "On-Text-Message: ~a" message)
  (clws.handler:send-text-message handler message))

(defmethod clws.handler:on-binary-message ((handler echo-handler) buf)
  (v:debug :cl-websocket-test "On-Binary-Message: Length: ~a" (length buf))
  (clws.handler:send-binary-message handler buf))

;;
;;
;;

(defparameter +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+ 1000)
(defparameter +ECHO-AND-CLOSE-HANDLER-REASON+ "Server closed connection")

(defclass echo-and-close-handler (silent-handler)
  ((echo-count :initform 1)))

(defmethod close-after-echo ((handler echo-and-close-handler))
  (setf (slot-value handler 'echo-count) (+ -1 (slot-value handler 'echo-count)))
  (if (= 0 (slot-value handler 'echo-count))
      (clws.handler:close-connection
       handler
       +ECHO-AND-CLOSE-HANDLER-STATUS-CODE+
       +ECHO-AND-CLOSE-HANDLER-REASON+))
  (if (> 0 (slot-value handler 'echo-count))
      (error "Echo-And-Closed-Handler: Echoed too many times")))

(defmethod clws.handler:on-text-message ((handler echo-and-close-handler) message)
  (v:debug :cl-websocket-test "On-Text-Message: ~a" message)
  (clws.handler:send-text-message handler message)
  (close-after-echo handler))

(defmethod clws.handler:on-binary-message ((handler echo-and-close-handler) buf)
  (v:debug :cl-websocket-test "On-Binary-Message: Length: ~a" (length buf))
  (clws.handler:send-binary-message handler buf)
  (close-after-echo handler))

;;
;;
;;

(defclass echo-2-and-close-handler (echo-and-close-handler)())

(defmethod initialize-instance :after ((handler echo-2-and-close-handler) &rest rest)
  (declare (ignore rest))
  (setf (slot-value handler 'echo-count) 2))

;;
;;
;;

(defclass crashing-on-open-handler (silent-handler)
  ())

(defmethod clws.handler:on-open-connection ((handler crashing-on-open-handler))
  (v:debug :cl-websocket-test "On-Open")
  (error "Crashing on open"))

(defclass crashing-on-message-handler (silent-handler)
  ())

(defmethod clws.handler:on-text-message ((handler crashing-on-message-handler) message)
  (v:debug :cl-websocket-test "On-Text-Message: ~a" message)
  (error "Crashing on text message"))

(defmethod clws.handler:on-binary-message ((handler crashing-on-message-handler) buf)
  (v:debug :cl-websocket-test "On-Binary-Message: Length: ~a" (length buf))
  (error "Crashing on binary message"))


