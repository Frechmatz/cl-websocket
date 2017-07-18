# cl-websocket

An experimental WebSocket-Server implemented in Common Lisp.

The server passes all tests of version v0.7.5/v0.10.9 of the [autobahn testsuite](https://github.com/crossbario/autobahn-testsuite) except

* Compression: The server does not support the compression extension
* Fast fail-over on invalid UTF-8 data: The server reads _all_ frames of a text message before
signalling an inconsistent-data error (1007) instead of failing on the first frame that contains an invalid character.

Installation
------------

This project is not available via quicklisp, but all its dependencies are. The easiest
way to install is to clone cl-websocket into the local-projects directory of
quicklisp and then to quickload it.

Clone:

    cd ~/quicklisp/local-projects
    git clone https://github.com/Frechmatz/cl-websocket.git

Load and install all dependencies:

    (ql:quickload "cl-websocket")

Dependencies:

* [bordeaux-threads](https://github.com/sionescu/bordeaux-threads) 
* [usocket](https://github.com/usocket/usocket)
* [flexi-streams](http://weitz.de/flexi-streams/)
* [cl-base64](http://quickdocs.org/cl-base64/)
* [ironclad](https://github.com/froydnj/ironclad)
* [quri](https://github.com/fukamachi/quri)
* [cl-threadpool](https://github.com/Frechmatz/cl-threadpool)
* [verbose](https://github.com/Shinmera/verbose)


Example: Echo-Service
---------------------

**Load cl-websocket**

    (asdf:load-system "cl-websocket")

**Define a handler class**

    (defclass echo-handler (clws.handler:connection-handler) ())

    (defmethod clws.handler:on-open-connection ((handler echo-handler))
        (v:info :echo-handler "on-open-connection"))

    (defmethod clws.handler:on-close-connection ((handler echo-handler) status-code reason)
        (v:info :echo-handler "on-close-connection"))

    (defmethod clws.handler:on-text-message ((handler echo-handler) message)
        (v:info :echo-handler "on-text-message: ~a" message))
        (clws.handler:send-text-message handler message))

    (defmethod clws.handler:on-binary-message ((handler echo-handler) message)
        (v:info :echo-handler "on-binary-message")
        (clws.handler:send-binary-message handler message))

**Instantiate the server**

    (defparameter *server* (clws.server:make-websocketserver "localhost" 9001))

**Register the handler class**

    (clws.server:register-resource-handler *server* "/echo" 'echo-handler '())

**Start the server**

    (clws.server:start *server*)

The server can now be reached via ws://localhost:9001/echo

**Stop the server**

    (clws.server:stop *server*)

API
---

### Package clws.server

* **make-websocketserver** (host port &key (threadpool-size 5)) => server

* **start** (server)

* **stop** (server)

* **register-resource-handler** (server uri-path class options)

    * __uri-path__ The path of the resource, for example /echo   
    * __class__ Class of the handler to be instantiated when a connection has been established
    * __options__ An alist specifying connection options. The following options are available
    
        * __:max-payload-length__ Maximum payload size of a frame
        * __:max-frame-count__ Maximum number of frames of which a message may consist

* [Macro] **do-connection-handlers-by-uri** (server uri-path handler &body body)

   Iterates over the active connection handlers belonging to a given resource. Executes the
   body with _handler_ bound to the connection handler.
   
    * __uri-path__ The path of the resource, for example /chat or nil for all resources

   Example:

        (defun broadcast (message)
            (clws.server:do-connection-handlers-by-uri
                *server* "/chat" cur-handler
                (clws.handler:send-text-message cur-handler message)))


* [Macro] **do-connection-handlers-by-handler** (connection-handler handler &body body)

   Iterates over the active connection handlers belonging to the resource (uri-path)
   of a given handler. Executes the body with _handler_ bound to the connection handler.
   
    * __connection-handler__ A connection handler

   Example:

        (defmethod clws.handler:on-open-connection ((handler chat-handler))
            (let ((nickname (clws.server:get-uri-query-param chat-handler "name")))
                (clws.server:do-connection-handlers-by-handler
                    chat-handler cur-handler
                    (clws.handler:send-text-message
                        cur-handler
                        (format nil "~a has joined the chat" nickname)))))

* **get-uri-query-param** (connection-handler param-name) => value

   Returns the value of a query parameter of the WebSocket request-uri that has
   been used to open the connection.

### Package clws.handler

* [Class] **connection-handler**

   A _connection-handler_ represents an endpoint of a WebSocket connection.
   Connection handlers are instantiated by the server. Applications must
   derive their handlers from this class.

   See also **clws.server:register-resource-handler**

* **on-open-connection** (connection-handler)

* **on-close-connection** (connection-handler status-code reason)

* **on-text-message** (connection-handler message)

* **on-binary-message** (connection-handler payload)

    * __payload__ An instance of '(vector (unsigned-byte 8))

* **close-connection** (connection-handler status-code reason)

* **send-text-message** (connection-handler message)

* **send-binary-message** (connection-handler payload)

    * __payload__ A sequence of '(unsigned-byte 8)
    
Logging
-------

The server uses the [Verbose](https://github.com/Shinmera/verbose) framework for logging.

Set logging level (globally)

    (setf (v:repl-level) :error)

Running the tests
-----------------

The tests are using the [lisp-unit](https://github.com/OdonataResearchLLC/lisp-unit) framework.
The system definition file is cl-websocket-test.asd.

Run all tests

    (asdf:load-system "cl-websocket-test")
    (in-package :cl-websocket-test)
    (setf lisp-unit:*print-failures* t)
    (use-debugger)
    (run-tests)

Run a specific test

    (asdf:load-system "cl-websocket-test")
    (in-package :cl-websocket-test)
    (setf lisp-unit:*print-failures* t)
    (use-debugger)
    (run-tests '(ping-test-1))

References
----------

The Websocket Protocol: [RFC 6455](https://tools.ietf.org/html/rfc6455)
