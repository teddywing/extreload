(asdf:defsystem extreload
  :version "0.0.1"
  :depends-on (:jsown
               :websocket-driver-client)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "main")))))
