(in-package :extreload)

(defvar *client* (wsd:make-client "ws://127.0.0.1:55755/devtools/browser/93f390a0-8ba9-4469-8e2d-69c9feb171bc"))

(defun main ()
  (wsd:start-connection *client*)

  (wsd:on :message *client* #'ws-on-message)

  (wsd:send *client* (get-targets-msg 1))

  (sleep 1)

  (wsd:close-connection *client*))

(defun get-targets-msg (call-id)
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("method" . "Target.getTargets"))))

(defun ws-on-message (message)
  (let* ((response (jsown:parse message))
         (targets (parse-get-targets-response response)))
    (format t "~&Got: ~A~%" targets)))

(defun parse-get-targets-response (response)
  (let* ((result (json-obj-get response "result"))
         (targetInfos (json-obj-get result "targetInfos")))
    targetInfos))

(defun json-obj-get (obj key)
  (handler-case
    (jsown:val obj key)
    (simple-error (e)
                  (let ((s (format nil "~A" e)))
                    (if (search "not available" s)
                        nil)))))
