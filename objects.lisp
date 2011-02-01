;;;; objects.lisp

(in-package #:zps)

(defclass ps-object () ())
(defclass readonly-mixin () ())

(defclass array (ps-object)
  ((storage
    :initarg :storage
    :accessor storage)))

(defun create-array (size &optional (class 'array))
  (make-instance class
                 :storage (make-array size)))

(defclass readonly-array (array readonly-mixin) ())
(defclass packed-array (array readonly-mixin) ())

(defclass procedure (array) ())
(defclass readonly-procedure (procedure readonly-mixin) ())

(defclass name (ps-object)
  ((name
    :initarg :name
    :accessor name)))

(defclass reference ()
  ((target
    :initarg :target
    :accessor target)))

(defvar *names* (make-hash-table :test 'equal))

(defun ps-intern (string)
  (or (gethash string *names*)
      (setf (gethash string *names*)
            (make-instance 'name :name string))))

(defun find-name (string)
  (values (gethash string *names*)))

(defclass mark (ps-object) ())
(defclass bool (ps-object) ())
(defclass true (bool) ())
(defclass false (bool) ())

(defvar *mark* (make-instance 'mark))
(defvar *true* (make-instance 'true))
(defvar *false* (make-instance 'false))

(defmethod print-object ((name name) stream)
  (print-unreadable-object (name stream :type t)
    (format stream "~S" (name name))))

(defmethod print-object ((reference reference) stream)
  (print-unreadable-object (reference stream :type t)
    (format stream "to ~S" (name (target reference)))))

(defgeneric display (object)
  (:method (object)
    (write object)))

(defmethod display ((object mark))
  (write-string "mark"))

(defmethod display ((object reference))
  (write-string (name (target object))))

(defmethod display ((object name))
  (write-char #\/)
  (write-string (name object)))

(defmethod display ((object true))
  (write-string "true"))

(defmethod display ((object false))
  (write-string "false"))

(defgeneric display-contents (object)
  (:method ((array array))
    (let* ((storage (storage array))
           (end (1- (length storage))))
      (loop for i from 0 to end
            do (display (aref storage i))
            when (< i end) do
            (write-string " ")))))

(defmethod display ((array array))
  (write-string "[")
  (display-contents array)
  (write-string "]"))

(defmethod display ((procedure procedure))
  (write-string "{ ")
  (display-contents procedure)
  (write-string " }"))
