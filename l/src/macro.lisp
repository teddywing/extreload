(in-package :extreload)

(defmacro filter (predicate list-form)
  `(remove-if-not ,predicate ,list-form))

(defmacro with-websocket-connection ((client) &body body)
  `(progn
     (wsd:start-connection ,client)
     (unwind-protect (progn ,@body)
       (wsd:close-connection ,client))))
