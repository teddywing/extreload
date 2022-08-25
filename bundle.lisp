;;; Copyright (c) 2022  Teddy Wing
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
