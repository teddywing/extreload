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

   (ws-client
     :documentation "WebSocket client")))

(defmethod print-object ((object config) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (socket-url extension-ids reload-current-tab ws-client) object
      (format stream
              ":socket-url ~s :extension-ids ~s :reload-current-tab ~s :ws-client ~s"
              socket-url extension-ids reload-current-tab ws-client))))

;; TODO: (make-config) instead, initialise ws-client in initialiser
(defgeneric (setf socket-url) (url config))

(defmethod (setf socket-url) (url (config config))
  "Set `socket-url` and initialise a new `websocket-driver:client` in the
`ws-client` slot"
  (setf (slot-value config 'socket-url) url)

  (setf (slot-value config 'ws-client) (wsd:make-client url)))

(defun make-config (&key socket-url extension-ids reload-current-tab)
  (let ((config (make-instance 'config
                               :socket-url socket-url
                               :extension-ids extension-ids
                               :reload-current-tab reload-current-tab)))

    ;; Initialise a new websocket-driver client
    (setf (slot-value config 'ws-client) (wsd:make-client socket-url))

    config))
