;;; Copyright (c) 2021  Teddy Wing
;;;
;;; This file is part of Extreload.
;;;
;;; Extreload is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Extreload is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Extreload. If not, see <https://www.gnu.org/licenses/>.


(asdf:defsystem extreload
  :version "0.0.1"
  :depends-on (:jsown
               :sysexits
               :trivial-timeout
               :unix-opts
               :wait-group
               :with-user-abort
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
