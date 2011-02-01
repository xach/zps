;;;; dictionary.lisp

(in-package #:zps)

(defclass dictionary (ps-object)
  ((table
    :initarg :table
    :accessor table)
   ;; XXX should this be max-length to match the postscript name?
   (max-size
    :initarg :max-size
    :accessor max-size))
  (:default-initargs
   :max-size 100
   :table (make-hash-table :test 'equal)))

(defclass read-only-dictionary (dictionary readonly-mixin) ())

(defparameter *system-dictionary*
  (make-instance 'read-only-dictionary)
  "AKA systemdict")

(defparameter *global-dictionary*
  (make-instance 'dictionary)
  "AKA globaldict")

(defvar *user-dictionary*
  (make-instance 'dictionary)
  "AKA userdict")

(defparameter *dictionary-stack*
  (let ((stack (make-instance 'stack)))
    (%push *system-dictionary* stack)
    (%push *global-dictionary* stack)
    (%push *user-dictionary* stack)
    stack))

(defun stack-lookup (key)
  (loop for i from (1- (pointer *dictionary-stack*)) downto 0
        for dict = (aref (storage *dictionary-stack*) i)
        for value = (gethash key (table dict))
        when value return (values value dict)))

(defun push-dict (dict)
  (%push dict *dictionary-stack*))

(defun pop-dict ()
  (when (eql (top *dictionary-stack*) *user-dictionary*)
    (error "Can't pop permanent dictionary"))
  (%pop *dictionary-stack*))

(defun %put (key value dict)
  (setf (gethash key (table dict)) value))
                        
(defgeneric put (key value dict))

(defmethod put (key value (dict dictionary))
  (%put key value dict))

(defmethod put (key value (dict readonly-mixin))
  (error "Read-only dictionary"))

(defgeneric get (key dict))

(defmethod get (key (dict dictionary))
  (gethash key (table dict)))
