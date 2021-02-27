;;;; DevTools Protocol messages.

(in-package :extreload)

(defun target-get-targets-msg (call-id)
  "DevTools Protocol `Target.getTargets` message."
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("method" . "Target.getTargets"))))

(defun target-attach-to-target-msg (call-id target-id)
  "DevTools Protocol `Target.attachToTarget` message."
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("method" . "Target.attachToTarget")
           ("params" . (:obj ("targetId" . ,target-id)
                             ("flatten" . t))))))

(defun target-attached-to-target-msg-p (message)
  "Returns true if `message` is `Target.attachedToTarget`."
  (equal
    (json-obj-get message "method")
    "Target.attachedToTarget"))

(defun runtime-evaluate-msg (call-id session-id expression)
  "DevTools Protocol `Runtime.evaluate` message."
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("sessionId" . ,session-id)
           ("method" . "Runtime.evaluate")
           ("params" . (:obj ("expression" . ,expression))))))

(defun runtime-evaluate-msg-p (message)
  "Returns true if `message` is a response to `Runtime.evaluate`."
  (jsown:keyp (json-obj-get message "result") "sessionId"))

(defun parse-get-targets-response (response)
  "Parses a list of target info objects from the response to `Target.getTargets`."
  (let* ((result (json-obj-get response "result"))
         (targetInfos (json-obj-get result "targetInfos")))
    targetInfos))
