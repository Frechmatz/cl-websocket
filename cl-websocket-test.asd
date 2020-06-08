(defsystem :cl-websocket-test
  :serial t
  :description "cl-websocket tests"
  :long-description "cl-websocket tests"
  :depends-on (:cl-websocket
	       :lisp-unit)
  :components ((:module "test"
			:serial t
			:components ((:file "packages")
				     (:file "test-handler")
				     (:file "test-server")
				     (:file "mock-socket")
				     (:file "test-util")))
               (:module "test/httprequest"
			:serial t
			:components ((:file "parser-test")
				     (:file "invalid-request-test")))
               (:module "test/binary-types"
			:serial t
			:components ((:file "binary-types-test")))
               (:module "test/frame"
			:serial t
			:components ((:file "frame-mask-test")
				     (:file "read-frame-test")
				     (:file "write-frame-test")))
               (:module "test/websocket"
			:serial t
			:components ((:file "sec-websocket-test")))
	       (:module "test/connection"
			:serial t
			:components ((:file "echo-and-close-test")
				     (:file "multiple-frame-message-test")
				     (:file "invalid-frame-sequence-test")
				     (:file "crashing-handler-test")
				     (:file "invalid-frame-test")
				     (:file "ping-test")
				     (:file "policy-violation-test")
				     ))
	       (:module "test/server"
			:serial t
			:components ((:file "register-resource-handler-test")
				     (:file "do-connection-handlers-test")))))
