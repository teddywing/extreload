;;;; DevTools Protocol messages.

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

(defun target-get-targets-msg (call-id)
  "DevTools Protocol `Target.getTargets` message."
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("method" . "Target.getTargets"))))

(defun target-attach-to-target-msg (call-id target-id)
  "DevTools Protocol `Target.attachToTarget` message."
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("method" . "Target.attachToTarget")
           ("params" . (:obj ("targetId" . ,target-id)
                             ("flatten" . t))))))

(defun target-attached-to-target-msg-p (message)
  "Returns true if `message` is `Target.attachedToTarget`."
  (equal
    (json-obj-get message "method")
    "Target.attachedToTarget"))

(defun runtime-evaluate-msg (call-id session-id expression)
  "DevTools Protocol `Runtime.evaluate` message."
  (jsown:to-json
    `(:obj ("id" . ,call-id)
           ("sessionId" . ,session-id)
           ("method" . "Runtime.evaluate")
           ("params" . (:obj ("expression" . ,expression))))))

(defun runtime-evaluate-msg-p (message)
  "Returns true if `message` is a response to `Runtime.evaluate`."
  (jsown:keyp (json-obj-get message "result") "sessionId"))

(defun runtime-evaluate-exception-p (message)
  "Returns true if `message` describes a runtime exception"
  (jsown:keyp (json-obj-get message "result") "exceptionDetails"))

(defun inspector-target-crashed-msg-p (message)
  "Returns true if `message` describes a target crashed"
  (equal
    (json-obj-get message "method")
    "Inspector.targetCrashed"))

(defun parse-get-targets-response (response)
  "Parses a list of target info objects from the response to `Target.getTargets`."
  (let* ((result (json-obj-get response "result"))
         (targetInfos (json-obj-get result "targetInfos")))
    targetInfos))
