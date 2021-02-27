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


(in-package :extreload)

(defmacro filter (predicate list-form)
  "Returns a sequence that only includes elements of `list-form` that satisfy
the test of `predicate`."
  `(remove-if-not ,predicate ,list-form))

(defmacro with-websocket-connection ((client) &body body)
  "Open a WebSocket connection on `client` and run `body` forms. The connection
is automatically closed at the end of execution."
  `(progn
     (wsd:start-connection ,client)
     (unwind-protect (progn ,@body)
       (wsd:close-connection ,client))))
