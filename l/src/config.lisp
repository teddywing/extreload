(in-package :extreload)

(defclass config ()
  ((socket-url
     :initarg :socket-url
     :reader socket-url
     :documentation "DevTools WebSocket URL")
   (extension-ids
     :initarg :extension-ids
     :reader extension-ids
     :documentation "Sequence of extension IDs")
   (reload-current-tab
     :initarg :reload-current-tab
     :initform nil
     :reader reload-current-tab
     :documentation "True if the current tab should be reloaded")
   (debug-output
     :initarg :debug-output
     :initform nil
     :reader debug-output
     :documentation "True to enable debug output")

   (ws-client
     :reader ws-client
     :documentation "WebSocket client")))

(defmethod print-object ((object config) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (socket-url
                  extension-ids
                  reload-current-tab
                  debug-output
                  ws-client)
      object
      (format
        stream
        ":socket-url ~s :extension-ids ~s :reload-current-tab ~s :debug-output ~s :ws-client ~s"
        socket-url extension-ids reload-current-tab debug-output ws-client))))

(defun make-config (&key socket-url
                         extension-ids
                         reload-current-tab
                         debug-output)
  (let ((config (make-instance 'config
                               :socket-url socket-url
                               :extension-ids extension-ids
                               :reload-current-tab reload-current-tab
                               :debug-output debug-output)))

    ;; Initialise a new websocket-driver client
    (setf (slot-value config 'ws-client) (wsd:make-client socket-url))

    config))
