(asdf:defsystem extreload
  :version "0.0.1"
  :depends-on (:jsown
                :sysexits
                :trivial-timeout
                :unix-opts
                :wait-group
                :websocket-driver-client)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "macro")
                             (:file "config")
                             (:file "option")
                             (:file "call-id")
                             (:file "devtools-protocol")
                             (:file "main"))))

  :build-operation "program-op"
  :build-pathname "extreload"
  :entry-point "extreload:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
