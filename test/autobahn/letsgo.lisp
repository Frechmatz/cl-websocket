(asdf:load-system "cl-websocket" :force t)
(load "/Users/olli/src/lisp/cl-websocket/example/echo-server.lisp")
(in-package :echo-server)
(start)
