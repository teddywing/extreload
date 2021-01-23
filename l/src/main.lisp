(in-package :extreload)

(defvar *client* (wsd:make-client "ws://127.0.0.1:55755/devtools/browser/93f390a0-8ba9-4469-8e2d-69c9feb171bc"))

(defun main ()
  (wsd:start-connection *client*)

  (wsd:on :message *client*
          (lambda (message)
            (format t "~&Got: ~A~%" message)))

  (wsd:send *client* (get-targets-msg 1))

  (sleep 1)

  (wsd:close-connection *client*))

(defun get-targets-msg (call-id)
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("method" . "Target.getTargets"))))
