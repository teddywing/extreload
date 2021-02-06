(in-package :extreload)

(defvar *wg* (wait-group:make-wait-group))
(defvar *devtools-root-call-id* (make-instance 'call-id))

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
  (handler-bind ((error
                   #'(lambda (e)
                       ;; TODO: generic function for this and `handle-option-error`
                       (format *error-output* "error: ~a~%" e)

                       (opts:exit 69))))

    (let ((config (parse-options)))
      ;; Store the WebSocket client as a global.
      (defvar *client* (ws-client config))

      ;; TODO: error if no `socket-url`
      (with-websocket-connection (*client*)
        (wsd:on :message *client*
                #'(lambda (message)
                    (ws-on-message message (extension-ids config))))
        ; (wsd:on :message *client* #'(lambda (message) (ws-on-message message)))
        ;; TODO: Maybe defvar *config* and store client in the config

        (websocket-send *client* (target-get-targets-msg
                                   (next-call-id *devtools-root-call-id*)))

        (wait-group:wait *wg*)))))

(defun ws-on-message (message extension-ids)
  (let* ((response (jsown:parse message))
         (targets (parse-get-targets-response response)))
    (if targets
        (reload-extensions
          (extension-targets targets)
          extension-ids))

    (if (target-attached-to-target-msg-p response)
        (reload-extension
          (json-obj-get
            (json-obj-get response "params")
            "sessionId")))

    (wait-group:done *wg*)))

(defun json-obj-get (obj key)
  (handler-case
    (jsown:val obj key)
    (simple-error (e)
                  (let ((s (format nil "~A" e)))
                    (if (search "not available" s)
                        nil)))))

;;; TODO: Rename to attach-extensions
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
              (target-attach-to-target-msg
                (next-call-id *devtools-root-call-id*)
                target-id))))

(defun reload-extension (session-id)
  ;; Use call ID "1" as this is the first message sent to the attached target.
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
