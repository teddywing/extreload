(in-package :extreload)

(defclass extension ()
  ((id
     :initarg :id
     :reader id
     :documentation "The extension's ID.")
   (url
     :initarg :url
     :reader url
     :documentation "The DevTools Protocol target URL.")
   (session-id
     :initarg :session-id
     :reader session-id
     :documentation "A DevTools Protocol session ID."))

  (:documentation "An extension."))
