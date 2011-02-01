;;;; procedure.lisp

(in-package #:zps)

(defclass procedure (array) ())

(defclass readonly-procedure (procedure readonly-mixin) ())

