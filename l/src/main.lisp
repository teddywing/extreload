(in-package :extreload)

(defvar *client* (wsd:make-client "ws://127.0.0.1:55755/devtools/browser/ec1d4b1c-ced0-47ab-a32e-6fdd5b51e1ba"))

(defun main ()
  (wsd:start-connection *client*)

  (wsd:on :message *client* #'ws-on-message)

  (wsd:send *client* (target-get-targets-msg 1))

  (sleep 5)

  (wsd:close-connection *client*))

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

(defun ws-on-message (message)
  (let* ((response (jsown:parse message))
         (targets (parse-get-targets-response response)))
    (if targets
        (reload-extensions
          (extension-targets targets)
          '("pacpdcpgfbpkdpmhfaljffnfbdanmblh")))

    (if (target-attached-to-target-msg-p response)
        (reload-extension
          (json-obj-get
            (json-obj-get response "params")
            "sessionId")))))

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

(defun reload-extensions (targets extension-ids)
  (labels ((requested-extension-p (target)
             (dolist (id extension-ids)
               (if (uiop:string-prefix-p
                     (concatenate 'string "chrome-extension://" id)
                     (json-obj-get target "url"))
                   (return-from requested-extension-p t)))

             nil))

    (dolist (extension (filter #'requested-extension-p targets))
      (attach-to-target extension))))

(defun attach-to-target (extension)
  (let ((target-id (json-obj-get extension "targetId")))
    (wsd:send *client*
              (target-attach-to-target-msg 2 target-id))))

(defun reload-extension (session-id)
  (wsd:send *client*
            (runtime-evaluate-msg 1 session-id "chrome.runtime.reload()")))

(defun extension-targets (targets)
  (labels ((extensionp (target)
             (string= (json-obj-get target "type")
                      "background_page")))

    (filter #'extensionp targets)))

(defun filter (predicate list-form)
  (let ((newl '()))
    (dolist (el list-form)
      (if (funcall predicate el)
          (push el newl)))

    newl))
