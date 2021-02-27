(in-package :extreload)

(defvar *wg* (wait-group:make-wait-group))
(defvar *devtools-root-call-id* (make-instance 'call-id)
  "DevTools Protocol call ID.")
(defvar *devtools-secondary-call-id* (make-instance 'call-id)
  "DevTools Protocol call ID used for messages to individual target sessions.")

(defconstant +timeout-seconds+ 5
  "Global timeout. The program will exit at the end of this delay.")

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
  "Called when a WebSocket message is received."
  (let* ((response (jsown:parse message))
         (targets (parse-get-targets-response response)))
    (when (debug-output config)
      (format t "Response: ~a~%" response)
      (format t "~a~%" *wg*))

    (when targets
      (let ((targets (extension-targets targets)))

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
    (when (runtime-evaluate-exception-p response)
      ;; `reload-tab` adds an extra increment to the wait group. If the call
      ;; fails, we only receive one message instead of two, so the wait group
      ;; must be decremented to match.
      (wait-group:done *wg*)

      (reload-tab (json-obj-get response "sessionId")))

    (wait-group:done *wg*)))

(defun json-obj-get (obj key)
  "Get the value of `key` from `obj` (a `jsown` object). Return nil if `key` is
not defined."
  (handler-case
    (jsown:val obj key)
    (simple-error (e)
                  (let ((s (format nil "~A" e)))
                    (if (search "not available" s)
                        nil)))))

(defun attach-extensions (targets extension-ids)
  "Attach to all extensions in `targets` that match the IDs in
`extension-ids`."
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
  "Send a message to the target in `extension` asking to attach to the target."
  (let ((target-id (json-obj-get extension "targetId")))
    (websocket-send (ws-client *config*)
              (target-attach-to-target-msg
                (next-call-id *devtools-root-call-id*)
                target-id))))

(defun reload-extension (session-id)
  "Send a message to an extension page corresponding to `session-id`, telling
the target extension to reload itself."
  (websocket-send
    (ws-client *config*)
    (runtime-evaluate-msg
      (next-call-id *devtools-secondary-call-id*)
      session-id
      "chrome.runtime.reload()")))

(defun reload-tab (session-id)
  "Send a message to an extension page corresponding to `session-id`, telling
the target to reload the current tab."

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
  "Filter `targets`, returning a list of targets corresponding to extensions."
  (labels ((extensionp (target)
             (string= (json-obj-get target "type")
                      "background_page")))

    (filter #'extensionp targets)))

(defun websocket-send (client data)
  "Send `data` to WebSocket `client` and increment `*wg*`."
  (when (debug-output *config*)
    (format t "Sending: ~a~%" data))

  (wsd:send client data)
  (wait-group:add *wg*))
