(in-package :extreload)

(defvar *wg* (wait-group:make-wait-group))
(defvar *devtools-root-call-id* (make-instance 'call-id))
(defvar *devtools-secondary-call-id* (make-instance 'call-id))
(defvar *reloaded-count* 0)
(defvar *extension-targets-count* 0)
(defvar *last-session-id* "")

(defconstant +timeout-seconds+ 5)

(opts:define-opts
  (:name :socket-url
         :description "DevTools protocol WebSocket URL"
         :long "socket-url"
         :arg-parser #'identity
         :meta-var "SOCKET_URL")
  (:name :reload-current-tab
         :description "pass this to reload the active Chrome tab"
         :long "reload-current-tab")
  (:name :debug
         :description "print debug output"
         :long "debug")
  (:name :help
         :description "print this help menu"
         :short #\h
         :long "help")
  (:name :version
         :description "show the program version"
         :short #\V
         :long "version"))

(defun main ()
  (handler-bind ((error #'(lambda (e)
                            (exit-with-error e sysexits:+unavailable+))))

    ;; Store the config as a global.
    (defvar *config* (parse-options))

    (trivial-timeout:with-timeout (+timeout-seconds+)
      (with-websocket-connection ((ws-client *config*))
        (wsd:on :message (ws-client *config*)
                #'(lambda (message)
                    (ws-on-message
                      message
                      (extension-ids *config*)
                      *config*)))

        (websocket-send
          (ws-client *config*)
          (target-get-targets-msg
            (next-call-id *devtools-root-call-id*)))

        (wait-group:wait *wg*)))))

(defun ws-on-message (message extension-ids config)
  (let* ((response (jsown:parse message))
         (targets (parse-get-targets-response response)))
    (when (debug-output config)
      (format t "Response: ~a~%" response)
      (format t "~a~%" *wg*))

    (when targets
      (let ((targets (extension-targets targets)))
        (setf *extension-targets-count* (length targets))

        (attach-extensions targets extension-ids)))

    (if (target-attached-to-target-msg-p response)
        (reload-extension (json-obj-get
                            (json-obj-get response "params")
                            "sessionId")))

    (when (and (reload-current-tab config)
               (runtime-evaluate-msg-p response))
      (reload-tab (json-obj-get
                    (json-obj-get response "result")
                    "sessionId")))

    ;; Failed to reload tab.
    (when (jsown:keyp (json-obj-get response "result") "exceptionDetails")
      ;; `reload-tab` adds an extra increment to the wait group. If the call
      ;; fails, we only receive one message instead of two, so the wait group
      ;; must be decremented to match.
      (wait-group:done *wg*)

      (reload-tab (json-obj-get response "sessionId")))

    (wait-group:done *wg*)))

(defun json-obj-get (obj key)
  (handler-case
    (jsown:val obj key)
    (simple-error (e)
                  (let ((s (format nil "~A" e)))
                    (if (search "not available" s)
                        nil)))))

(defun attach-extensions (targets extension-ids)
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
    (websocket-send (ws-client *config*)
              (target-attach-to-target-msg
                (next-call-id *devtools-root-call-id*)
                target-id))))

(defun reload-extension (session-id)
  (setf *last-session-id* session-id)
  (websocket-send
    (ws-client *config*)
    (runtime-evaluate-msg
      (next-call-id *devtools-secondary-call-id*)
      session-id
      "chrome.runtime.reload()"))

  (incf *reloaded-count*))

(defun reload-tab (session-id)
  ;; Two response messages always come back from the `chrome.tabs.reload()`
  ;; messages, so we need to add a second increment to the wait group.
  (wait-group:add *wg*)

  (websocket-send
    (ws-client *config*)
    (runtime-evaluate-msg
      (next-call-id *devtools-secondary-call-id*)
      session-id
      "chrome.tabs.reload()")))

(defun extension-targets (targets)
  (labels ((extensionp (target)
             (string= (json-obj-get target "type")
                      "background_page")))

    (filter #'extensionp targets)))

(defun websocket-send (client data)
  (when (debug-output *config*)
    (format t "Sending: ~a~%" data))

  (wsd:send client data)
  (wait-group:add *wg*))
