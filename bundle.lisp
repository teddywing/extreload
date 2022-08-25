(setf ql:*local-project-directories* '("./lib"))

(let ((dependencies (append
                      (asdf:system-depends-on (asdf:find-system :extreload))))
      (local-dependencies '("wait-group")))
  (ql:bundle-systems
    (set-difference
      (sort dependencies #'string-lessp)
      local-dependencies
      :test #'equal)
    :to "./bundle"
    :include-local-projects t))

(quit)
