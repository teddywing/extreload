(asdf:defsystem extreload
  :version "0.0.1"
  :depends-on (:jsown
                :unix-opts
                :wait-group
                :websocket-driver-client)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "macro")
                             (:file "config")
                             (:file "option")
                             (:file "main"))))

  :build-operation "program-op"
  :build-pathname "extreload"
  :entry-point "extreload:main")
