;;;; stack.lisp

(in-package #:zps)

(defvar *stack-size* 500)

(defclass stack (ps-object)
  ((storage
    :initarg :storage
    :accessor storage)
   (pointer
    :initarg :pointer
    :accessor pointer))
  (:default-initargs
   :storage (make-array *stack-size*)
   :pointer 0))

(defvar *operand-stack* (make-instance 'stack))

(defun %pop (stack)
  (if (plusp (pointer stack))
      (aref (storage stack) (decf (pointer stack)))
      (error "Stack underflow")))

(defun %push (object stack)
  (setf (aref (storage stack) (pointer stack)) object)
  (incf (pointer stack))
  object)

(defun %top (stack)
  (aref (storage stack) (1- (pointer stack))))

(defun %dup (stack)
  (%push (%top stack) stack))

(defun %exch (stack)
  (let ((a (pop stack))
        (b (pop stack)))
    (push a stack)
    (push b stack)))


(defun pop ()
  (%pop *operand-stack*))

(defun push (value)
  (%push value *operand-stack*))

(defun top ()
  (%top *operand-stack*))

(defun dup ()
  (%dup *operand-stack*))

(defun exch ()
  (%exch *operand-stack*))

(defun clear (stack)
  (setf (pointer stack) 0))

(defun highest-mark-position (stack)
  (position *mark* (storage stack)
            :end (pointer stack)
            :from-end t))

(defun pop-mark-array (stack)
  (let ((pos (highest-mark-position stack)))
    (if pos
        (let ((array (subseq (storage stack)
                             (1+ pos) (pointer stack))))
          (setf (pointer stack) pos)
          array)
        (error "No mark"))))

