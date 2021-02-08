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

    (when (null (getf options :socket-url))
        (format *error-output* "error: '--socket-url' is required~%")

        (opts:exit 64))

    ;; Error if no extension IDs were given.
    (when (null free-args)
      (format *error-output* "error: missing extension IDs~%")

      (opts:exit 64))

    (make-config :socket-url (getf options :socket-url)
                 :reload-current-tab (getf options :reload-current-tab)
                 :extension-ids free-args)))
