(in-package :extreload)

(defmacro filter (predicate list-form)
  "Returns a sequence that only includes elements of `list-form` that satisfy
the test of `predicate`."
  `(remove-if-not ,predicate ,list-form))

(defmacro with-websocket-connection ((client) &body body)
  "Open a WebSocket connection on `client` and run `body` forms. The connection
is automatically closed at the end of execution."
  `(progn
     (wsd:start-connection ,client)
     (unwind-protect (progn ,@body)
       (wsd:close-connection ,client))))
