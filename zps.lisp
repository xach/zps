;;;; zps.lisp


;;;; Scanner

(defvar *scanning-dispatch-characters* (make-hash-table))








(defun scan-close-angle-bracket (character stream)
  (declare (ignore character))
  (let ((next (peek-char nil stream nil)))
    (if (eql next #\>)
        (progn
          (read-char stream)
          :double-angle-bracket)
        :single-angle-bracket)))


(defun scan-dictionary (character stream)
  (declare (ignore character))
  (mark-reference 
  (let ((result '()))
    (loop
     (let ((object (scan stream)))
       (case object
         (:double-angle-bracket
          (return (nreverse result)))
         (:single-angle-bracket
        (error "Unexpected single angle bracket"))
       (t
        (cl:push object result)))))))
        
(defun make-adjustable-octet-vector ()
  (make-array 0 :element-type '(unsigned-byte 8)
              :fill-pointer 0 :adjustable t))






(defun %read-char (stream)
  (read-char stream))


  




(defun scan-close-square-bracket (character stream)
  (declare (ignore character stream))
  :close-square-bracket)

(defun scan-array (character stream)
  (declare (ignore character))
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (loop
     (let ((object (scan stream)))
       (case object
         (:close-square-bracket
          (return result))
         (t
          (vector-push-extend object result)))))))

(defun scan-close-curly-bracket (character stream)
  (declare (ignore character stream))
  :close-curly-bracket)

(defun scan-procedure (character stream)
  (declare (ignore character))
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (loop
     (let ((object (scan stream)))
       (case object
         (:close-curly-bracket
          (return (make-instance 'procedure :storage result)))
         (t
          (vector-push-extend object result)))))))








