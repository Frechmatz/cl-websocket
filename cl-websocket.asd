(defsystem :cl-websocket
  :serial t
  :version "0.0.1"
  :licence "Public Domain / 0-clause MIT"
  :description "Experimental Web-Socket-Server"
  :long-description "Experimental Web-Socket-Server"
  :depends-on (:usocket
	       :flexi-streams
	       :cl-base64
	       :ironclad
	       :bordeaux-threads
	       :verbose
	       :quri)
  :components ((:module "src/http"
			:serial t
			:components ((:file "packages")
				     (:file "parse-util")
				     (:file "stream-util")
				     (:file "http-base")
				     (:file "http-request")
				     (:file "http-response")
				     (:file "header-parser")
				     (:file "request-parser")
				     (:file "response-serializer")))
	       (:module "src/connection"
			:serial t
			:components ((:file "packages")
				     (:file "binary-types")
				     (:file "frame")
				     (:file "connection-socket")
				     (:file "connection")
				     (:file "connection-handler")
				     ))
	       (:module "src/server"
			:serial t
			:components ((:file "packages")
				     (:file "server-class")
				     (:file "server-connection")
				     (:file "connection-request-processor")
				     (:file "server")))))
