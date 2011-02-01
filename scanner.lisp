;;;; scanner.lisp

(in-package #:zps)

(defvar *scanning-dispatch-characters* (make-hash-table))

(defun make-reference (string)
  (make-instance 'reference :target (ps-intern string)))

(defvar *mark* (make-reference "mark"))
(defparameter *array-start* (make-reference "["))
(defparameter *array-end* (make-reference "]"))
(defparameter *dict-start* (make-reference "<<"))
(defparameter *dict-end* (make-reference ">>"))

(defun set-dispatch (char fun)
  (setf (gethash char *scanning-dispatch-characters*) fun))

(defun set-multi-dispatch (chars fun)
  (dolist (char chars)
    (set-dispatch char fun)))

(defun specialp (char)
  (member char '(#\( #\) #\< #\> #\[ #\] #\{ #\} #\/ #\%
                 #\Space #\Tab #\Newline #\Return)))

(defun whitespacep (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun make-adjustable-octet-vector ()
  (make-array 0 :element-type '(unsigned-byte 8)
              :fill-pointer 0 :adjustable t))

(defun dispatching-scan (character stream)
  (let ((fun (gethash character *scanning-dispatch-characters*
                      'scan-name)))
    (funcall fun character stream)))

(defgeneric scan (source))

(defmethod scan ((stream stream))
  (loop
   (let ((char (read-char stream nil)))
     (cond ((whitespacep char))
           ((not char)
            (return))
           (t
            (return (dispatching-scan char stream)))))))


;;; Relatively simple objects: comments, names, () strings

(defun scan-comment (character stream)
  (declare (ignore character))
  (loop
   (let ((char (read-char stream nil)))
     (when (or (not char)
               (char= char #\Newline))
       (return (scan stream))))))

(defun scan-name (character stream)
  (make-instance 'reference
                 :target
                 (ps-intern
                  (with-output-to-string (output)
                    (write-char character output)
                    (loop
                     (let ((char (peek-char nil stream nil)))
                       (when (or (not char)
                                 (specialp char))
                         (return))
                       (read-char stream)
                       (write-char char output)))))))

(defun scan-quoted-name (character stream)
  (declare (ignore character))
  (ps-intern
   (with-output-to-string (output)
     (loop
      (let ((char (peek-char nil stream nil)))
        (cond ((or (specialp char) (not char))
               (return))
              (t
               (read-char stream)
               (write-char char output))))))))

(defun scan-string (character stream)
  (declare (ignore character))
  (let ((level 1))
    (with-output-to-string (output)
      (loop
       (let ((char (read-char stream)))
         (case char
           (#\(
            (write-char char output)
            (incf level))
           (#\)
            (when (zerop (decf level))
              (return))
            (write-char char output))
           (t
            (write-char char output))))))))

(defun scan-hexstring (character stream)
  (declare (ignore character))
  (let ((result (make-adjustable-octet-vector)))
    (loop
     (let ((hi (read-char stream))
           (value 0))
       (when (char= hi #\>)
         (return result))
       (setf value (* (digit-char-p hi 16) 16))
       (let ((lo (read-char stream)))
         (when (char= lo #\>)
           (vector-push-extend value result)
           (return result))
         (incf value (digit-char-p lo 16))
         (vector-push-extend value result))))))

;;; Ascii85 strings are <~...~>


(defun a85-char-code (char)
  (let ((value (- (char-code char) 33)))
    (unless (<= 0 value 84)
      (error "Invalid a85 character ~S" char))
    value))

(defun read-a85-tuple (stream)
  (let ((count 0)
        (value 0))
    (loop
     (let ((digit (peek-char nil stream)))
       (case digit
         (#\~
          (let ((padding (- 5 count)))
            (return (values (* value (expt 85 padding)) (1- count)))))
         (#\z
          (error "Unexpected ~S at ~D" digit (file-position stream)))
         (t
          (read-char stream)
          (incf count)
          (setf value (+ (* 85 value) (a85-char-code digit))))))
     (when (= count 5)
       (return (values value (1- count)))))))

(defun read-a85-data (stream)
  (let ((next (peek-char nil stream)))
    (case next
      ((#\Newline #\Space #\Return #\Tab)
       (read-char stream)
       (read-a85-data stream))
      (#\z
       (read-char stream)
       (values 0 4))
      (#\~
       (read-char stream)
       (let ((end (read-char stream)))
         (unless (char= end #\>)
           (error "~~ not followed by >"))
         nil))
      (t
       (read-a85-tuple stream)))))

(defun scan-a85 (character stream)
  (declare (ignore character))
  (let ((result (make-adjustable-octet-vector)))
    (loop
     (multiple-value-bind (value octet-count)
         (read-a85-data stream)
       (unless value
         (return result))
       (let ((start 24))
         (dotimes (i octet-count)
           (vector-push-extend (ldb (byte 8 start) value) result)
           (decf start 8)))))))

;;; Dictionaries

(defun scan-dictionary (character stream)
  (declare (ignore character stream))
  *dict-start*)

(defun scan-end-of-dictionary (character stream)
  (declare (ignore character))
  (let ((next (peek-char nil stream nil)))
    (if (eql next #\>)
        (progn
          (read-char stream)
          *dict-end*)
        (ps-error 'syntaxerror))))

;;; Multiple syntaxes start with #\<

(defun scan-angle-object (character stream)
  (declare (ignore character))
  (let ((next (peek-char nil stream)))
    (case next
      (#\<
       (read-char stream)
       (scan-dictionary nil stream))
      (#\~
       (read-char stream)
       (scan-a85 nil stream))
      (t
       (scan-hexstring nil stream)))))

;;; Arrays

(defun scan-array (character stream)
  (declare (ignore character stream))
  *array-start*)

(defun scan-end-of-array (character stream)
  (declare (ignore character stream))
  *array-end*)

;;; Procedures

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

(defun scan-end-of-procedure (character stream)
  (declare (ignore character stream))
  :close-curly-bracket)


;;; Numbers

(defun scan-to-special (character stream)
  (with-output-to-string (output)
    (write-char character output)
    (loop for char = (peek-char nil stream nil)
          until (or (not char) (specialp char)) do
          (write-char (read-char stream) output))))

(defun scan-number (character stream)
  (let ((possible-number (scan-to-special character stream)))
    (string-number possible-number)))

;;; Dispatching

(set-multi-dispatch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                      #\+ #\-)
                    'scan-number)
(set-dispatch #\% 'scan-comment)
(set-dispatch #\/ 'scan-quoted-name)
(set-dispatch #\( 'scan-string)

(set-dispatch #\< 'scan-angle-object)
(set-dispatch #\> 'scan-end-of-dictionary)

(set-dispatch #\[ 'scan-array)
(set-dispatch #\] 'scan-end-of-array)

(set-dispatch #\{ 'scan-procedure)
(set-dispatch #\} 'scan-end-of-procedure)


;;; Convenient for testing

(defgeneric scan-all (source)
  (:documentation "Read and return all tokens from SOURCE.")
  (:method ((stream stream))
    (loop for object = (scan stream)
          while object collect object))
  (:method ((string string))
    (with-input-from-string (stream string)
      (scan-all stream)))
  (:method ((pathname pathname))
    (with-open-file (stream pathname)
      (scan-all stream))))
