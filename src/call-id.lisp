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

(defclass call-id ()
  ((id
     :initform 0
     :reader id
     :documentation "Current call ID."))

  (:documentation "An incrementing identifier."))

(defgeneric next-call-id (call-id)
  (:documentation "Increment the call ID and return the result."))

(defmethod next-call-id ((call-id call-id))
  (incf (slot-value call-id 'id)))
