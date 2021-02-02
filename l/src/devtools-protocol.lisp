(in-package :extreload)

(defun target-get-targets-msg (call-id)
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("method" . "Target.getTargets"))))

(defun target-attach-to-target-msg (call-id target-id)
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("method" . "Target.attachToTarget")
           ("params" . (:obj ("targetId" . ,target-id)
                             ("flatten" . t))))))

(defun target-attached-to-target-msg-p (message)
  (equal
    (json-obj-get message "method")
    "Target.attachedToTarget"))

(defun runtime-evaluate-msg (call-id session-id expression)
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("sessionId" . ,session-id)
           ("method" . "Runtime.evaluate")
           ("params" . (:obj ("expression" . ,expression))))))

(defun parse-get-targets-response (response)
  (let* ((result (json-obj-get response "result"))
         (targetInfos (json-obj-get result "targetInfos")))
    targetInfos))
