(in-package :extreload)

(defvar *client* (wsd:make-client "ws://127.0.0.1:53954/devtools/browser/0a276302-a6e8-4f7e-9fbf-6ea97b55aa99"))

(defun main ()
  (wsd:start-connection *client*)

  (wsd:on :message *client*
          (lambda (message)
            (format t "~&Got: ~A~%" message)))

  (wsd:send *client* "msg")

  (wsd:close-connection *client*))
