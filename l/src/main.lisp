(in-package :extreload)

(defvar *client* (wsd:make-client "ws://127.0.0.1:55755/devtools/browser/93f390a0-8ba9-4469-8e2d-69c9feb171bc"))

(defun main ()
  (wsd:start-connection *client*)

  (wsd:on :message *client*
          (lambda (message)
            (format t "~&Got: ~A~%" message)))

  (wsd:send *client* "msg")

  (wsd:close-connection *client*))
