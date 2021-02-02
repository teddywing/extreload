(in-package :extreload)

(defmacro when-option ((options option) &body body)
  `(let ((value (getf ,options ,option)))
     (when value
       ,@body)))

(defun handle-option-error (condition)
  (format *error-output* "error: ~a~%" condition)

  (opts:exit 64))

(defun parse-options ()
  (multiple-value-bind (options free-args)
    (handler-bind
      ((opts:unknown-option #'handle-option-error)
       (opts:missing-arg #'handle-option-error)
       (opts:arg-parser-failed #'handle-option-error)
       (opts:missing-required-option #'handle-option-error))

      (opts:get-opts))

    (when-option (options :help)
      (opts:describe
        :usage-of "extreload"
        :args "EXTENSION_ID...")

      (opts:exit 64))

    (when-option (options :version)
      (format t "~a~%" (asdf:component-version (asdf:find-system :extreload)))

      (opts:exit 0))

    ; (if ) ;; If no socket URL, error 64

    (make-instance 'config
                   :socket-url (getf options :socket-url)
                   :extension-ids free-args)))
