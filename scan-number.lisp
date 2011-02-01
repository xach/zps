;;;; scan-number.lisp

(in-package #:zps)

(defparameter *integer-scanner*
  (cl-ppcre:create-scanner "^[-+]?\\d+$"))

(defparameter *float-scanner*
  (cl-ppcre:create-scanner "^([-+]?\\d+)\\.(\\d*)([eE][-+]?\\d+)?$"))

(defparameter *radix-scanner*
  (cl-ppcre:create-scanner "^(\\d+)#([0-9a-zA-Z]+)$"))

(defun parse-ps-integer (string)
  (when (cl-ppcre:scan *integer-scanner* string)
    (parse-integer string)))

(defun parse-ps-real (string)
  (cl-ppcre:register-groups-bind (integer-string fractional-string
                                                 exponent-string)
      (*float-scanner* string)
    (when integer-string
      (let ((integer (parse-integer integer-string))
            (fractional (or (and fractional-string
                                 (parse-integer fractional-string))
                            0))
            (exponent (or (and exponent-string
                               (parse-integer exponent-string
                                              :start 1))
                          0)))
        (float
         (* (expt 10 exponent)
            (+ integer (* fractional
                          (expt 10 (- (length fractional-string))))))
         1.0d0)))))

(defun parse-ps-radix (string)
  (cl-ppcre:register-groups-bind (radix-string digits-string)
      (*radix-scanner* string)
    (when (and digits-string radix-string)
      (let ((radix (parse-integer radix-string)))
        (ignore-errors (parse-integer digits-string :radix radix))))))
      

(defun string-number (string)
  "If STRING is a number in PostScript syntax, convert it to its
Lisp-native integer or float value. Otherwise, just return STRING."
  ;; Three types of syntax to deal with: integers, radix integers, and
  ;; real numbers.
  (or (parse-ps-integer string)
      (parse-ps-radix string)
      (parse-ps-real string)
      string))
