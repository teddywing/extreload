;;; Copyright (c) 2021  Teddy Wing
;;;
;;; This file is part of Extreload.
;;;
;;; Extreload is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Extreload is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Extreload. If not, see <https://www.gnu.org/licenses/>.


(in-package :extreload)

(defvar *wg* (wait-group:make-wait-group))
(defvar *devtools-root-call-id* (make-instance 'call-id)
  "DevTools Protocol call ID.")
(defvar *devtools-secondary-call-id* (make-instance 'call-id)
  "DevTools Protocol call ID used for messages to individual target sessions.")
(defvar *extensions* '()
  "TODO")

(defconstant +timeout-seconds+ 10
  "Global timeout. The program will exit at the end of this delay.")

(defun main ()
  (handler-case
      (interrupt:with-user-abort
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

    ;; Control-c
    (interrupt:user-abort ()
      (opts:exit sysexits:+ok+))))

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

    ;; TODO: Force to attach a second time
    (when (target-attached-to-target-msg-p response)
      (when (and (reload-current-tab config)
                 (target-attached-to-target-msg-manifest-v3-extension-p response))
        ;; response is an extension in *extensions*
        (format t "RELOADING TAB: ~a~%"
                (json-obj-get
                  (json-obj-get response "params")
                  "sessionId"))

        (reload-tab (json-obj-get
                      (json-obj-get response "params")
                      "sessionId"))

        ; (return-from ws-on-message)
        )

      (track-service-worker-target response)

      (reload-extension (json-obj-get
                          (json-obj-get response "params")
                          "sessionId")))

    (format t "EXTENSIONS: ~a~%" *extensions*)

    (when (and (reload-current-tab config)
               (inspector-target-crashed-msg-p response))
      ;; Attach to target again
      ;; then somehow reload the tab
      ;; And need to only do this for MV3 extensions

      ;; Loop through *extensions*, if sessionId matches, then send attach message to extension
      ;; Need to get new targets, and attach to all MV3 extensions again
      (websocket-send
        (ws-client *config*)
        (target-get-targets-msg
          (next-call-id *devtools-root-call-id*)))
      )

    (when (and (reload-current-tab config)
               (runtime-evaluate-msg-p response))
      (websocket-send
        (ws-client *config*)
        (target-get-targets-msg
          (next-call-id *devtools-root-call-id*)))
      ; (reload-tab (json-obj-get
      ;               (json-obj-get response "result")
      ;               "sessionId"))
      )

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

  (format t "DO THE RELOAD~%")

  ;; Two response messages always come back from the `chrome.tabs.reload()`
  ;; messages, so we need to add a second increment to the wait group.
  (wait-group:add *wg*)

  (sleep 1)
  (format t "DO THE RELOAD~%")

  (websocket-send
    (ws-client *config*)
    (runtime-evaluate-msg
      (next-call-id *devtools-secondary-call-id*)
      session-id
      "chrome.tabs.reload()"))

  (format t "Did THE RELOAD~%")
  )

(defun extension-targets (targets)
  "Filter `targets`, returning a list of targets corresponding to extensions."
  (labels ((extensionp (target)
             (or
               (string= (json-obj-get target "type")
                        "background_page")
               ;; TODO: This might require us to re-attach to the service worker after chrome.runtime.reload
               ;; Compare MV2 and MV3 debug output
               (string= (json-obj-get target "type")
                        "service_worker"))))

    (filter #'extensionp targets)))

(defun track-service-worker-target (target)
  "TODO"
  (let* ((params (json-obj-get target "params"))
         (target-info (json-obj-get params "targetInfo")))

    (when (string= (json-obj-get target-info "type")
                   "service_worker")
      (push
        (make-instance 'extension
                       :id (subseq
                             (json-obj-get target-info "url")

                             ;; Remove "chrome-extension://".
                             19

                             ;; Extension IDs are 32 characters long.
                             (+ 19 32))
                       :url (json-obj-get target-info "url")
                       :session-id (json-obj-get params "sessionId"))
        *extensions*))))

(defun target-attached-to-target-msg-manifest-v3-extension-p (msg)
  "Return true if the Target.attachedToTarget message `msg` corresonds to a
tracked Manifest V3 extension."
  (>=
    (length
      (filter
        #'(lambda (extension)
            (string= (json-obj-get
                       (json-obj-get
                         (json-obj-get msg "params")
                         "targetInfo")
                       "url")
                     (url extension)))
        *extensions*))
    1))

(defun manifest-v3-extension-p (extension)
  "Return true if `extension` is in our list of tracked Manifest V3 extensions."
  (filter
    #'(lambda (ext)
        ())
    *extensions*))

(defun websocket-send (client data)
  "Send `data` to WebSocket `client` and increment `*wg*`."
  (when (debug-output *config*)
    (format t "Sending: ~a~%" data))

  (wsd:send client data)
  (wait-group:add *wg*))
