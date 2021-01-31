(in-package :extreload)

(defmacro filter (predicate list-form)
  `(remove-if-not ,predicate ,list-form))
