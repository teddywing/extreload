;;;; Command line options.

(in-package :extreload)

(defmacro when-option ((options option) &body body)
  "When `option` is present in `options`, run `body`."
  `(let ((value (getf ,options ,option)))
     (when value
       ,@body)))

(defun exit-with-error (condition exit-code)
  "Print the error associated with `condition` on standard error, then exit
with code `exit-code`."
  (format *error-output* "error: ~a~%" condition)

  (opts:exit exit-code))

(defun handle-option-error (condition)
  "Handle errors related to command line options. Prints the error specified by
`condition` and exits with EX_USAGE."
  (exit-with-error condition sysexits:+usage+))

(defun parse-options ()
  "Parse command line options."
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

      (opts:exit sysexits:+usage+))

    (when-option (options :version)
      (format t "~a~%" (asdf:component-version (asdf:find-system :extreload)))

      (opts:exit sysexits:+ok+))

    (when (null (getf options :socket-url))
        (format *error-output* "error: '--socket-url' is required~%")

        (opts:exit sysexits:+usage+))

    ;; Error if no extension IDs were given.
    (when (null free-args)
      (format *error-output* "error: missing extension IDs~%")

      (opts:exit sysexits:+usage+))

    (make-config :socket-url (getf options :socket-url)
                 :reload-current-tab (getf options :reload-current-tab)
                 :debug-output (getf options :debug)
                 :extension-ids free-args)))
