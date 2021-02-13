(in-package :extreload)

(defvar *wg* (wait-group:make-wait-group))
(defvar *devtools-root-call-id* (make-instance 'call-id))
(defvar *reloaded-count* 0)
(defvar *extension-targets-count* 0)
(defvar *last-session-id* "")

(opts:define-opts
  (:name :socket-url
         :description "DevTools protocol WebSocket URL"
         :long "socket-url"
         :arg-parser #'identity
         :meta-var "SOCKET_URL")
  (:name :reload-current-tab
         :description "pass this to reload the active Chrome tab"
         :long "reload-current-tab")
  (:name :help
         :description "print this help menu"
         :short #\h
         :long "help")
  (:name :version
         :description "show the program version"
         :short #\V
         :long "version"))

(defun main ()
  (handler-bind ((error #'(lambda (e) (exit-with-error e 69))))

    (let ((config (parse-options)))
      ;; Store the WebSocket client as a global.
      (defvar *client* (ws-client config))

      ;; TODO: error if no `socket-url`
      (with-websocket-connection (*client*)
        (wsd:on :message *client*
                #'(lambda (message)
                    (ws-on-message
                      message
                      (extension-ids config)
                      (reload-current-tab config))))
        ; (wsd:on :message *client* #'(lambda (message) (ws-on-message message)))
        ;; TODO: Maybe defvar *config* and store client in the config

        (websocket-send *client* (target-get-targets-msg
                                   (next-call-id *devtools-root-call-id*)))

        (wait-group:wait *wg*)))))

(defun ws-on-message (message extension-ids reload-current-tab)
  (let* ((response (jsown:parse message))
         (targets (parse-get-targets-response response)))
    (when targets
      (let ((targets (extension-targets targets)))
        (setf *extension-targets-count* (length targets))

        (reload-extensions targets extension-ids)))

    ;; TODO: How to know it's the last message so we only reload the current tab once?

    (if (target-attached-to-target-msg-p response)
        ;; TODO: Need a waitgroup:add for each occurrence of extension in extension-ids
        (reload-extension (json-obj-get
                            (json-obj-get response "params")
                            "sessionId")))

    ;; TODO: only if config.reload-current-tab

;; If last session ID corresponds,
; Response: (OBJ (id . 2)
;            (result OBJ (sessionId . C24A99CA53CBD76EB68BCBD0D172A4E7)))

    (when (and reload-current-tab
               (runtime-evaluate-msg-p response))
      ; (when (and (= (or (json-obj-get response "id") -1) 1)
      ; (when (and 
                 ; (= *reloaded-count*
                 ;    ;; TODO: Probably want to reload on all extension reload calls
                 ;    *extension-targets-count*)
                 ;
                 ; (string= (json-obj-get
                 ;            (json-obj-get response "result")
                 ;            "sessionId")
                 ;          *last-session-id*))
      ; (let ((current-call-id (json-obj-get response "id")))
      ;   (when (and current-call-id
      ;              (= current-call-id
      ;                 (id *devtools-root-call-id*)))
      ;
          ; (sleep 1)
          (reload-tab (json-obj-get
                        (json-obj-get response "result")
                        "sessionId")))

    ;; Failed to reload tab.
    (when (jsown:keyp (json-obj-get response "result") "exceptionDetails")
      (reload-tab (json-obj-get response "sessionId")))

    (format t "Response: ~a~%" response)
    (format t "~a~%" *wg*)

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
  (format t "reloading EXTENSION~%")
  (setf *last-session-id* session-id)
  (websocket-send *client*
            (runtime-evaluate-msg 1 session-id "chrome.runtime.reload()"))

  (incf *reloaded-count*))

(defun reload-tab (session-id)
  ;; Use call ID "2" as this will always be sent after a `reload-extension`
  ;; message.
  (format t "reloading NOW~%")
  (websocket-send
    *client*
    (runtime-evaluate-msg 2 session-id "chrome.tabs.reload()")))

(defun extension-targets (targets)
  (labels ((extensionp (target)
             (string= (json-obj-get target "type")
                      "background_page")))

    (filter #'extensionp targets)))

(defun websocket-send (client data)
  (wsd:send client data)
  (wait-group:add *wg*))
