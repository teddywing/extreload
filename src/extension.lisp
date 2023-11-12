(in-package :extreload)

(defclass extension ()
  ((id
     :initarg :id
     :reader id
     :documentation "The extension's ID.")
   (session-id
     :initarg :session-id
     :reader session-id
     :documentation "A DevTools Protocol session ID."))

  (:documentation "An extension."))
