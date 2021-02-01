(in-package :extreload)

(defmacro when-option ((options option) &body body)
  `(let ((value (getf ,options ,option)))
     (when value
       ,@body)))
