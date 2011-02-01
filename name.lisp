;;;; name.lisp

(in-package #:zps)

(defclass name ()
  ((name
    :initarg :name
    :accessor name)))

(defclass reference (name) ())

(defmethod print-object ((name name) stream)
  (print-unreadable-object (name stream :type t)
    (format stream "~S" (name name))))



