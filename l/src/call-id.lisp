(in-package :extreload)

(defclass call-id ()
  ((id
     :initform 0
     :reader id
     :documentation "Current call ID.")))

(defgeneric next-call-id (call-id)
  (:documentation "Increment the call ID and return the result."))

(defmethod next-call-id ((call-id call-id))
  (incf (slot-value call-id 'id)))
