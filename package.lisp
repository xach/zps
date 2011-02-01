;;;; package.lisp

(defpackage #:zps
  (:use #:cl)
  (:shadow #:push
           #:pop
           #:get
           #:array))
        

(in-package #:zps)
