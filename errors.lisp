;;;; errors.lisp

(in-package #:zps)

;;; Error handling is documented in 3.11

(defun ps-error (type)
  (error "~A" type))