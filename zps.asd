;;;; zps.asd

(asdf:defsystem #:zps
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "objects"
                      :depends-on ("package"))
               (:file "stack"
                      :depends-on ("objects"))
               (:file "dictionary"
                      :depends-on ("objects"
                                   "stack"))
               (:file "scan-number"
                      :depends-on ("package"))
               (:file "scanner"
                      :depends-on ("package"
                                   "objects"))
               (:file "interpreter"
                      :depends-on ("scanner"))
               (:file "errors"
                      :depends-on ("package"))
               (:file "primitives"
                      :depends-on ("dictionary"))))
