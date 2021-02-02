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
     :documentation "True if the current tab should be reloaded")))

(defmethod print-object ((object config) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (socket-url extension-ids reload-current-tab) object
      (format stream ":socket-url ~s :extension-ids ~s :reload-current-tab ~s"
              socket-url extension-ids reload-current-tab))))
