(in-package :extreload)

(defvar *client* (wsd:make-client "ws://127.0.0.1:55755/devtools/browser/20c6f8a1-6540-4226-a876-44b72c176ad5"))

(defvar *wg* (wait-group:make-wait-group))

(opts:define-opts
  (:name :socket-url
         :description "DevTools protocol WebSocket URL"
         :long "socket-url"
         :arg-parser #'identity
         :meta-var "SOCKET_URL")
  (:name :help
         :description "print this help menu"
         :short #\h
         :long "help")
  (:name :version
         :description "show the program version"
         :short #\V
         :long "version"))

(defun main ()
  (multiple-value-bind (options free-args) (opts:get-opts)
    (when-option (options :help)
      (opts:describe
        :usage-of "extreload"
        :args "EXTENSION_ID...")

      (opts:exit 64))

    (when-option (options :version)
      (format t "~a~%" (asdf:component-version (asdf:find-system :extreload)))

      (opts:exit 0))

    (let* ((socket-url (getf options :socket-url))
           (client (wsd:make-client socket-url)))
      (with-websocket-connection (*client*)
        (wsd:on :message *client* #'ws-on-message)

        (websocket-send *client* (target-get-targets-msg 1))

        (wait-group:wait *wg*)))))

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
            "sessionId")))

    (wait-group:done *wg*)))

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
             (find-if
               #'(lambda (id)
                   (uiop:string-prefix-p
                     (concatenate 'string "chrome-extension://" id)
                     (json-obj-get target "url")))
               extension-ids)))

    (dolist (extension (filter #'requested-extension-p targets))
      (attach-to-target extension))))

(defun attach-to-target (extension)
  (let ((target-id (json-obj-get extension "targetId")))
    (websocket-send *client*
              (target-attach-to-target-msg 2 target-id))))

(defun reload-extension (session-id)
  (websocket-send *client*
            (runtime-evaluate-msg 1 session-id "chrome.runtime.reload()")))

(defun extension-targets (targets)
  (labels ((extensionp (target)
             (string= (json-obj-get target "type")
                      "background_page")))

    (filter #'extensionp targets)))

(defun websocket-send (client data)
  (wsd:send client data)
  (wait-group:add *wg*))
