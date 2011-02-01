;;;; interpreter.lisp

(in-package #:zps)

(defgeneric interpret (source))
(defgeneric execute (object))
(defgeneric resolve (reference))
(defgeneric referencep (object))
(defgeneric procedurep (object))

(defmethod interpret ((source pathname))
  (with-open-file (stream source)
    (interpret stream)))

(defmethod interpret ((source string))
  (with-input-from-string (stream source)
    (interpret stream)))

(defmethod interpret (source)
  (loop
   (let ((object (scan source)))
     (cond ((null object)
            (return))
           ((referencep object)
            (execute (resolve object)))
           (t
            (push object))))))

(defmethod interpret ((source procedure))
  (loop for object across (storage source)
        do (if (referencep object)
               (execute object)
               (push object))))

(defmethod execute (object)
  (loop
   (cond ((null object)
          (return))
         ((referencep object)
          (setf object (resolve object)))
         ((procedurep object)
          (interpret object)
          (return))
         ((functionp object)
          (funcall object)
          (return))
         (t
          (push object)
          (return)))))
                

(defmethod referencep (object)
  nil)

(defmethod referencep ((reference reference))
  t)

(defmethod procedurep (object)
  nil)

(defmethod procedurep ((procedure procedure))
  t)

(defmethod resolve ((reference reference))
  (or (stack-lookup (target reference))
      (invoke-resolve-failed-hook reference)))

(defun default-resolve-failed-function (reference)
  (ps-error 'undefined))

(defvar *resolve-failed-hook* 'default-resolve-failed-function)

(defun invoke-resolve-failed-hook (reference)
  (funcall *resolve-failed-hook* reference))

;;; PS repl!

(defun default-prompt-fun ()
  (format t "~&ZPS% "))

(defvar *default-prompt-fun* 'default-prompt-fun)

(defun zps ()
  (catch 'quit-repl
    (loop
     (funcall *default-prompt-fun*)
     (interpret (read-line)))))
