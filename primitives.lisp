;;;; primitives.lisp

(in-package #:zps)

(defun define-primitive (name fun)
  (%put (ps-intern name) fun *system-dictionary*))

(defun pop-type (type)
  (if (typep (top) type)
      (pop)
      (ps-error 'type-error)))

(defmacro defprimitive (name (&rest args) &body body)
  (flet ((name (sym)
           (if (consp sym)
               (first sym)
               sym))
         (type (sym)
           (if (consp sym)
               (second sym)
               t)))
    `(define-primitive ,name
      (lambda ()
        (let (,@(mapcar (lambda (sym)
                          `(,(name sym) (pop-type ',(type sym))))
                        (reverse args)))
          ,@body)))))

(defun not-implemented (&rest args)
  (declare (ignore args))
  (ps-error 'notimplemented))

(defun invoke-primitive (name-string)
  (let* ((name (find-name name-string))
         (fun (gethash name (table *system-dictionary*) 'not-implemented)))
    (funcall fun)))

;;; Generic functions & primitives that go with them

(defmacro define-generic-primitive ((name generic-name) lambda-list &body body)
  `(progn
     (defgeneric ,generic-name ,lambda-list)
     (defprimitive ,name ,lambda-list ,@body)))

(define-generic-primitive ("length" generic-length) (object)
  (push (generic-length object)))

(define-generic-primitive ("get" generic-get) (object key)
  (push (generic-get object key)))

(define-generic-primitive ("put" generic-put) (object key value)
  (generic-put object key value))

(define-generic-primitive ("forall" generic-forall) (object procedure)
  (generic-forall object procedure))

(define-generic-primitive ("getinterval" generic-getinterval)
    (array index count)
  (push (generic-getinterval array index count)))

(define-generic-primitive ("putinterval" generic-putinterval)
    (array1 index array2)
  (generic-putinterval array1 index array2))

(define-generic-primitive ("copy" generic-copy) (top)
  (generic-copy top))

(define-generic-primitive ("readonly" generic-readonly) (object)
  (generic-readonly object))

    
;;; Operand Stack Manipulation Operators

(defun range-check (stack index)
  (unless (<= 0 index (1- (pointer stack)))
    (ps-error 'rangecheck)))

(defun stack-reference (stack i)
  (let ((j (- (pointer stack) i 1)))
    (range-check stack j)
    (aref (storage stack) j)))

(defprimitive "pop" ()
  (pop))

(defprimitive "exch" (a b)
  (push b) (push a))

(defprimitive "dup" (a)
  (push a) (push a))

(defmethod generic-copy ((n integer))
  (dotimes (i n)
    (push (stack-reference *operand-stack* (1- n)))))

(defprimitive "index" (n)
  (push (stack-reference *operand-stack* n)))

(defun swap-partitions (sequence start partition end)
  (let* ((size1 (- partition start))
         (size2 (- end partition)))
    (psetf (subseq sequence start (+ start size1))
           (subseq sequence (- end size1) end)
           (subseq sequence (- end size2) end)
           (subseq sequence start (+ start size2)))
    sequence))

(defprimitive "roll" (n j)
  (let* ((end (pointer *operand-stack*))
         (start (- end n))
         (partition (- end (mod j n) 1)))
    (swap-partitions (storage *operand-stack*) start partition end)))

(defprimitive "clear" ()
  (clear *operand-stack*))

(defprimitive "count" ()
  (push (pointer *operand-stack*)))

(defprimitive "mark" ()
  (push *mark*))

(defprimitive "cleartomark" ()
  (setf (pointer *operand-stack*)
        (highest-mark-position *operand-stack*)))

(defprimitive "counttomark" ()
  (let ((pos (highest-mark-position *operand-stack*)))
    (cond (pos
           (push (- (pointer *operand-stack*) pos 1)))
          (t
           (error "No mark")))))

;;; Arithmetic and Math Operators

(defprimitive "add" ((a number) (b number))
  (push (+ a b)))

(defprimitive "div" (a b)
  (push (float (/ a b))))

(defprimitive "idiv" (a b)
  (push (truncate a b)))

(defprimitive "mod" (a b)
  (push (mod a b)))

(defprimitive "mul" (a b)
  (push (* a b)))

(defprimitive "sub" (a b)
  (push (- a b)))

(defprimitive "abs" (num)
  (push (abs num)))

(defprimitive "neg" (num)
  (push (- num)))

(defprimitive "ceiling" (num)
  (push (ceiling num)))

(defprimitive "floor" (num)
  (push (floor num)))

(defprimitive "round" (num)
  (push (round num)))

(defprimitive "truncate" (num)
  (push (truncate num)))

(defprimitive "sqrt" (num)
  (push (sqrt num)))

(defun radian-degrees (radians)
  (* (/ radians pi) 180))

(defun degree-radians (degrees)
  (/ (* degrees pi) 180))

(defprimitive "atan" (numerator denominator)
  (push (radian-degrees (atan numerator denominator))))

(defprimitive "cos" (degrees)
  (push (radian-degrees (cos (degree-radians degrees)))))

(defprimitive "sin" (degrees)
  (push (radian-degrees (cos (degree-radians degrees)))))

(defprimitive "exp" (base exponent)
  (push (expt base exponent)))

(defprimitive "ln" (num)
  (push (log num)))

(defprimitive "log" (num)
  (push (log num 10)))

(defconstant +rand-max+ (expt 2 31))

(defprimitive "rand" ()
  (push (random +rand-max+)))

(defprimitive "srand" (seed)
  (warn "srand not implemented"))

(defprimitive "rrand" ()
  (warn "rrand not implemented")
  (push 42))


;;; Array Operators

(defprimitive "array" (length)
  (push (create-array length)))

(defprimitive "[" () (push *mark*))

(defprimitive "]" ()
  (let ((data (pop-mark-array *operand-stack*)))
    (push (make-instance 'array :storage data))))

(defmethod generic-length ((array array))
  (length (storage array)))

(defmethod generic-get ((array array) index)
  ;; FIXME: rangecheck?
  (aref (storage array) index))

(defmethod generic-put ((array array) index value)
  ;; FIXME: rangecheck?
  (setf (aref (storage array) index) value))

(defmethod generic-getinterval ((array array) index count)
  (subseq (storage array) index (+ index count)))

(defmethod generic-putinterval ((array1 array) index (array2 array))
  (replace array1 array2 :start1 index))

(defprimitive "astore" (array)
  (let* ((array-size (generic-length array))
         (index (- (pointer *operand-stack*) array-size)))
    (when (minusp index)
      (ps-error 'stackunderflow))
    (replace (storage array) (storage *operand-stack*) :start2 index)
    (setf (pointer *operand-stack*) index)
    (push array)))

(defprimitive "aload" (array)
  (dotimes (i (generic-length array))
    (push (generic-get array i)))
  (push array))

(defmethod generic-copy ((target array))
  (let ((source (pop)))
    ;; FIXME: rangecheck?
    (replace (storage target) (storage source))
    (push source)))
  
(defmethod generic-forall ((array array) procedure)
  (loop for i across (storage array)
        do
        (push i)
        (interpret procedure)))

;;; Packed Array Operators

(defprimitive "packedarray" (size)
  (let ((array (create-array size 'packed-array)))
    (replace (storage array)
             (subseq (storage *operand-stack*)
                     (- (pointer *operand-stack*) size)))
    (push array)))

(defvar *packedp* *false*)

(defprimitive "setpacking" (packedp)
  (setf *packedp* packedp))

(defprimitive "currentpacking" ()
  (push *packedp*))

(defmethod generic-put ((array packed-array) key value)
  (declare (ignore array key value))
  (ps-error 'invalidaccess))

;;; Dictionary Operators

(defprimitive "dict" (max-size)
  (push (make-instance 'dictionary
                       :max-size max-size)))

(defprimitive "<<" ()
  (push *mark*))

(defprimitive ">>" ()
  (let ((data (pop-mark-array *operand-stack*))
        (table (make-hash-table :test 'equal)))
    (loop for i from 0 by 2
          for j from 1 by 2 below (length data)
          do (setf (gethash (aref data i) table) (aref data j)))
    (push (make-instance 'dictionary :table table))))

(defmethod generic-length ((dict dictionary))
  (hash-table-count (table dict)))

(defprimitive "maxlength" (dict)
  (push (max-size dict)))

(defprimitive "begin" (dict)
  (%push dict *dictionary-stack*))

(defprimitive "end" ()
  (%pop *dictionary-stack*))

(defprimitive "def" (key value)
  (put key value (%top *dictionary-stack*)))

(defprimitive "load" (key)
  (let ((value (stack-lookup key)))
    (if value
        (push value)
        (ps-error 'undefined))))

(defprimitive "store" (key value)
  (multiple-value-bind (existing-value dict)
      (stack-lookup key)
    (declare (ignore existing-value))
    (if dict
        (%put key value dict)
        (put key value (%top *dictionary-stack*))))) 

(defprimitive "undef" () (not-implemented))

(defprimitive "known" () (not-implemented))

(defprimitive "where" (key)
  (multiple-value-bind (value dict)
      (stack-lookup key)
    (cond (value
           (push dict)
           (push *true*))
          (t
           (push *false*)))))

;; copy

(defmethod generic-forall ((dict dictionary) procedure)
  (maphash (lambda (k v)
             (push k)
             (push v)
             (interpret procedure))
           (table dict)))

(defprimitive "currentdict" ()
  (push (%top *dictionary-stack*)))

(defprimitive "errordict" () (not-implemented))

(defprimitive "$error" () (not-implemented))

(defprimitive "systemdict" ()
  (push *system-dictionary*))

(defprimitive "userdict" ()
  (push *user-dictionary*))

(defprimitive "globaldict" ()
  (push *global-dictionary*))

(defprimitive "statusdict" () (not-implemented))

(defprimitive "countdictstack" () (not-implemented))

(defprimitive "dictstack" () (not-implemented))

(defprimitive "cleardictstack" ()
  (setf (pointer *dictionary-stack*) 3))


;;; String Operators

(defprimitive "string" () (not-implemented))

;; length get put getinterval putinterval copy forall

(defprimitive "anchorsearch" () (not-implemented))

(defprimitive "search" () (not-implemented))

(defprimitive "token" () (not-implemented))


;;; Relational, Boolean, and Bitwise Operators

(defprimitive "eq" () (not-implemented))

(defprimitive "ne" () (not-implemented))

(defprimitive "ge" () (not-implemented))

(defprimitive "gt" () (not-implemented))

(defprimitive "le" () (not-implemented))

(defprimitive "lt" () (not-implemented))

(defprimitive "and" () (not-implemented))

(defprimitive "not" () (not-implemented))

(defprimitive "or" () (not-implemented))

(defprimitive "xor" () (not-implemented))

(defprimitive "true" () (push *true*))

(defprimitive "false" () (push *false*))

(defprimitive "bitshift" () (not-implemented))


;;;; Control Operators

(defprimitive "exec" () (not-implemented))

(defprimitive "if" () (not-implemented))

(defprimitive "ifelse" () (not-implemented))


;;; Control Operators

(defprimitive "exec" (object)
  (execute object))

(defprimitive "if" (bool proc)
  (when (eql bool *true*)
    (interpret proc)))

(defprimitive "ifelse" (bool true-proc false-proc)
  (if (eql bool *true*)
      (interpret true-proc)
      (interpret false-proc)))

(defprimitive "for" (initial increment limit proc)
  (ps-error 'not-implemented))

(defprimitive "repeat" (count proc)
  (dotimes (i count)
    (interpret proc)))

(defprimitive "loop" (proc)
  (catch 'loop
    (loop
     (interpret proc))))

(defprimitive "exit" ()
  (throw 'loop nil))

(defprimitive "quit" ()
  (clear *operand-stack*)
  (throw 'quit-repl nil))

(defprimitive "start" () (not-implemented))


;;; Type, Attribute, and Conversion Operators

(defprimitive "type" () (not-implemented))

(defprimitive "cvlit" () (not-implemented))

(defprimitive "cvx" () (not-implemented))

(defprimitive "xcheck" () (not-implemented))

(defprimitive "executeonly" () (not-implemented))

(defprimitive "noaccess" () (not-implemented))

(defmethod clone ((object array))
  (make-instance 'array
                 :storage (copy-seq (storage object))))

(defmethod generic-readonly ((object array))
  (let ((clone (clone object)))
    (change-class clone 'readonly-array)
    (push clone)))

(defprimitive "rcheck" () (not-implemented))

(defprimitive "wcheck" () (not-implemented))

(defprimitive "cvi" () (not-implemented))

(defprimitive "cvn" () (not-implemented))

(defprimitive "cvr" () (not-implemented))

(defprimitive "cvrs" () (not-implemented))

(defprimitive "cvs" () (not-implemented))


;;; File Operators

(defprimitive "file" () (not-implemented))

(defprimitive "filter" () (not-implemented))

(defprimitive "closefile" () (not-implemented))

(defprimitive "read" () (not-implemented))

(defprimitive "write" () (not-implemented))

(defprimitive "readhexstring" () (not-implemented))

(defprimitive "writehexstring" () (not-implemented))

(defprimitive "readstring" () (not-implemented))

(defprimitive "writestring" () (not-implemented))

(defprimitive "readline" () (not-implemented))

(defprimitive "token" () (not-implemented))

(defprimitive "bytesavailable" () (not-implemented))

(defprimitive "flush" () (not-implemented))

(defprimitive "flushfile" () (not-implemented))

(defprimitive "resetfile" () (not-implemented))

(defprimitive "status" () (not-implemented))

;;; status on filename

(defprimitive "run" () (not-implemented))

(defprimitive "currentfile" () (not-implemented))

(defprimitive "deletefile" () (not-implemented))

(defprimitive "renamefile" () (not-implemented))

(defprimitive "filenameforall" () (not-implemented))

(defprimitive "setfileposition" () (not-implemented))

(defprimitive "fileposition" () (not-implemented))

(defprimitive "print" () (not-implemented))

(defprimitive "=" ()
  (display (top))
  (pop))

(defprimitive "==" ()
  (display (top))
  (pop))

(defprimitive "stack" ()
  (loop for i from (1- (pointer *operand-stack*)) downto 0
        do (display (aref (storage *operand-stack*) i))
        (terpri)))

(defprimitive "pstack" ()
  (invoke-primitive "stack"))

(defprimitive "printobject" () (not-implemented))

(defprimitive "writeobject" () (not-implemented))

(defprimitive "setobjectformat" () (not-implemented))

(defprimitive "currentobjectformat" () (not-implemented))


;;; Resource Operators

(defprimitive "defineresource" () (not-implemented))

(defprimitive "undefineresource" () (not-implemented))

(defprimitive "findresource" () (not-implemented))

(defprimitive "findcolorrendering" () (not-implemented))

(defprimitive "resourcestatus" () (not-implemented))

(defprimitive "resourceforall" () (not-implemented))


;;; Virtual Memory Operators

(defprimitive "save" () (not-implemented))

(defprimitive "restore" () (not-implemented))

(defprimitive "setglobal" () (not-implemented))

(defprimitive "currentglobal" () (not-implemented))

(defprimitive "gcheck" () (not-implemented))

(defprimitive "startjob" () (not-implemented))

(defprimitive "defineuserobject" () (not-implemented))

(defprimitive "execuserobject" () (not-implemented))

(defprimitive "undefineuserobject" () (not-implemented))

(defprimitive "UserObjects" () (not-implemented))


;;; Miscellaneous Operators

(defprimitive "bind" ()
  (warn "bind has no effect"))

(defprimitive "null" ()
  (push *null*))

(defprimitive "version" () (not-implemented))

(defprimitive "realtime" ()
  (push (get-internal-real-time)))

(defprimitive "usertime" ()
  (push (get-internal-run-time)))

(defprimitive "languagelevel" () (not-implemented))

(defprimitive "product" () (not-implemented))

(defprimitive "revision" () (not-implemented))

(defprimitive "serialnumber" () (not-implemented))

(defprimitive "executive" () (not-implemented))

(defprimitive "echo" () (not-implemented))

(defprimitive "prompt" () (not-implemented))


;;; Graphics State Operators (Device-Independent)

(defprimitive "gsave" () (not-implemented))

(defprimitive "grestore" () (not-implemented))

(defprimitive "clipsave" () (not-implemented))

(defprimitive "cliprestore" () (not-implemented))

(defprimitive "grestoreall" () (not-implemented))

(defprimitive "initgraphics" () (not-implemented))

(defprimitive "gstate" () (not-implemented))

(defprimitive "setgstate" () (not-implemented))

(defprimitive "currentgstate" () (not-implemented))

(defprimitive "setlinewidth" () (not-implemented))

(defprimitive "currentlinewidth" () (not-implemented))

(defprimitive "setlinecap" () (not-implemented))

(defprimitive "currentlinecap" () (not-implemented))

(defprimitive "setlinejoin" () (not-implemented))

(defprimitive "currentlinejoin" () (not-implemented))

(defprimitive "setmiterlimit" () (not-implemented))

(defprimitive "currentmiterlimit" () (not-implemented))

(defprimitive "setstrokeadjust" () (not-implemented))

(defprimitive "currentstrokeadjust" () (not-implemented))

(defprimitive "setdash" () (not-implemented))

(defprimitive "currentdash" () (not-implemented))

(defprimitive "setcolorspace" () (not-implemented))

(defprimitive "currentcolorspace" () (not-implemented))

(defprimitive "setcolor" () (not-implemented))

(defprimitive "currentcolor" () (not-implemented))

(defprimitive "setgray" () (not-implemented))

(defprimitive "currentgray" () (not-implemented))

(defprimitive "sethsbcolor" () (not-implemented))

(defprimitive "currenthsbcolor" () (not-implemented))

(defprimitive "setrgbcolor" () (not-implemented))

(defprimitive "currentrgbcolor" () (not-implemented))

(defprimitive "setcmykcolor" () (not-implemented))

(defprimitive "currentcmykcolor" () (not-implemented))


;;; Graphics State Operators (Device-Dependent)

(defprimitive "sethalftone" () (not-implemented))

(defprimitive "currenthalftone" () (not-implemented))

(defprimitive "setscreen" () (not-implemented))

(defprimitive "currentscreen" () (not-implemented))

(defprimitive "setcolorscreen" () (not-implemented))

(defprimitive "currentcolorscreen" () (not-implemented))

(defprimitive "settransfer" () (not-implemented))

(defprimitive "currenttransfer" () (not-implemented))

(defprimitive "setcolortransfer" () (not-implemented))

(defprimitive "currentcolortransfer" () (not-implemented))

(defprimitive "setblackgeneration" () (not-implemented))

(defprimitive "currentblackgeneration" () (not-implemented))

(defprimitive "setundercolorremoval" () (not-implemented))

(defprimitive "currentundercolorremoval" () (not-implemented))

(defprimitive "setcolorrendering" () (not-implemented))

(defprimitive "currentcolorrendering" () (not-implemented))

(defprimitive "setflat" () (not-implemented))

(defprimitive "currentflat" () (not-implemented))

(defprimitive "setoverprint" () (not-implemented))

(defprimitive "currentoverprint" () (not-implemented))

(defprimitive "setsmoothness" () (not-implemented))

(defprimitive "currentsmoothness" () (not-implemented))


;;; Coordinate System and Matrix Operators

(defprimitive "matrix" () (not-implemented))

(defprimitive "initmatrix" () (not-implemented))

(defprimitive "identmatrix" () (not-implemented))

(defprimitive "defaultmatrix" () (not-implemented))

(defprimitive "currentmatrix" () (not-implemented))

(defprimitive "setmatrix" () (not-implemented))

(defprimitive "translate" () (not-implemented))

(defprimitive "scale" () (not-implemented))

(defprimitive "rotate" () (not-implemented))

(defprimitive "concat" () (not-implemented))

(defprimitive "concatmatrix" () (not-implemented))

(defprimitive "transform" () (not-implemented))

(defprimitive "dtransform" () (not-implemented))

(defprimitive "itransform" () (not-implemented))

(defprimitive "idtransform" () (not-implemented))

(defprimitive "invertmatrix" () (not-implemented))


;;; Path Construction Operators

(defprimitive "newpath" () (not-implemented))

(defprimitive "currentpoint" () (not-implemented))

(defprimitive "moveto" () (not-implemented))

(defprimitive "rmoveto" () (not-implemented))

(defprimitive "lineto" () (not-implemented))

(defprimitive "rlineto" () (not-implemented))

(defprimitive "arc" () (not-implemented))

(defprimitive "arcn" () (not-implemented))

(defprimitive "arct" () (not-implemented))

(defprimitive "arcto" () (not-implemented))

(defprimitive "curveto" () (not-implemented))

(defprimitive "rcurveto" () (not-implemented))

(defprimitive "closepath" () (not-implemented))

(defprimitive "flattenpath" () (not-implemented))

(defprimitive "reversepath" () (not-implemented))

(defprimitive "strokepath" () (not-implemented))

(defprimitive "ustrokepath" () (not-implemented))

(defprimitive "charpath" () (not-implemented))

(defprimitive "uappend" () (not-implemented))

(defprimitive "clippath" () (not-implemented))

(defprimitive "setbbox" () (not-implemented))

(defprimitive "pathbbox" () (not-implemented))

(defprimitive "pathforall" () (not-implemented))

(defprimitive "upath" () (not-implemented))

(defprimitive "initclip" () (not-implemented))

(defprimitive "clip" () (not-implemented))

(defprimitive "eoclip" () (not-implemented))

(defprimitive "rectclip" () (not-implemented))

(defprimitive "ucache" () (not-implemented))


;;; Painting Operators

(defprimitive "erasepage" () (not-implemented))

(defprimitive "stroke" () (not-implemented))

(defprimitive "fill" () (not-implemented))

(defprimitive "eofill" () (not-implemented))

(defprimitive "rectstroke" () (not-implemented))

(defprimitive "rectfill" () (not-implemented))

(defprimitive "ustroke" () (not-implemented))

(defprimitive "ufill" () (not-implemented))

(defprimitive "ueofill" () (not-implemented))

(defprimitive "shfill" () (not-implemented))

(defprimitive "image" () (not-implemented))

(defprimitive "colorimage" () (not-implemented))

(defprimitive "imagemask" () (not-implemented))


;;; Insideness-Testing Operators

(defprimitive "infill" () (not-implemented))

(defprimitive "ineofill" () (not-implemented))

(defprimitive "inufill" () (not-implemented))

(defprimitive "inueofill" () (not-implemented))

(defprimitive "instroke" () (not-implemented))

(defprimitive "inustroke" () (not-implemented))


;;; Form and Pattern Operators

(defprimitive "makepattern" () (not-implemented))

(defprimitive "setpattern" () (not-implemented))

(defprimitive "execform" () (not-implemented))


;;; Device Setup and Output Operators

(defprimitive "showpage" () (not-implemented))

(defprimitive "copypage" () (not-implemented))

(defprimitive "setpagedevice" () (not-implemented))

(defprimitive "currentpagedevice" () (not-implemented))

(defprimitive "nulldevice" () (not-implemented))


;;; Glyph and Font Operators

(defprimitive "definefont" () (not-implemented))

(defprimitive "composefont" () (not-implemented))

(defprimitive "undefinefont" () (not-implemented))

(defprimitive "findfont" () (not-implemented))

(defprimitive "scalefont" () (not-implemented))

(defprimitive "makefont" () (not-implemented))

(defprimitive "setfont" () (not-implemented))

(defprimitive "rootfont" () (not-implemented))

(defprimitive "currentfont" () (not-implemented))

(defprimitive "selectfont" () (not-implemented))

(defprimitive "show" () (not-implemented))

(defprimitive "ashow" () (not-implemented))

(defprimitive "widthshow" () (not-implemented))

(defprimitive "awidthshow" () (not-implemented))

(defprimitive "xshow" () (not-implemented))

(defprimitive "xyshow" () (not-implemented))

(defprimitive "yshow" () (not-implemented))

(defprimitive "glyphshow" () (not-implemented))

(defprimitive "stringwidth" () (not-implemented))

(defprimitive "cshow" () (not-implemented))

(defprimitive "kshow" () (not-implemented))

(defprimitive "FontDirectory" () (not-implemented))

(defprimitive "GlobalFontDirectory" () (not-implemented))

(defprimitive "StandardEncoding" () (not-implemented))

(defprimitive "ISOLatin1Encoding" () (not-implemented))

(defprimitive "findencoding" () (not-implemented))

(defprimitive "setcachedevice" () (not-implemented))

(defprimitive "setcachedevice2" () (not-implemented))

(defprimitive "setcharwidth" () (not-implemented))


;;; Interpreter Parameter Operators

(defprimitive "setsystemparams" () (not-implemented))

(defprimitive "currentsystemparams" () (not-implemented))

(defprimitive "setuserparams" () (not-implemented))

(defprimitive "currentuserparams" () (not-implemented))

(defprimitive "setdevparams" () (not-implemented))

(defprimitive "currentdevparams" () (not-implemented))

(defprimitive "vmreclaim" () (not-implemented))

(defprimitive "setvmthreshold" () (not-implemented))

(defprimitive "vmstatus" () (not-implemented))

(defprimitive "cachestatus" () (not-implemented))

(defprimitive "setcachelimit" () (not-implemented))

(defprimitive "setcacheparams" () (not-implemented))

(defprimitive "currentcacheparams" () (not-implemented))

(defprimitive "setucacheparams" () (not-implemented))

(defprimitive "ucachestatus" () (not-implemented))


;;; Errors

(defmacro define-error (name args &body body)
  `(defprimitive ,name ,args ,@body))

(defprimitive "configurationerror" () (not-implemented))

(defprimitive "dictfull" () (not-implemented))

(defprimitive "dictstackoverflow" () (not-implemented))

(defprimitive "dictstackunderflow" () (not-implemented))

(defprimitive "execstackoverflow" () (not-implemented))

(defprimitive "handleerror" () (not-implemented))

(defprimitive "interrupt" () (not-implemented))

(defprimitive "invalidaccess" () (not-implemented))

(defprimitive "invalidexit" () (not-implemented))

(defprimitive "invalidfileaccess" () (not-implemented))

(defprimitive "invalidfond" () (not-implemented))

(defprimitive "invalidrestore" () (not-implemented))

(defprimitive "ioerror" () (not-implemented))

(defprimitive "limitcheck" () (not-implemented))

(defprimitive "nocurrentpoint" () (not-implemented))

(defprimitive "rangecheck" () (not-implemented))

(defprimitive "stackoverflow" () (not-implemented))

(defprimitive "stackunderflow" () (not-implemented))

(defprimitive "syntaxerror" () (not-implemented))

(defprimitive "timeout" () (not-implemented))

(defprimitive "typecheck" () (not-implemented))

(defprimitive "undefined" () (not-implemented))

(defprimitive "undefinedfilename" () (not-implemented))

(defprimitive "undefinedresource" () (not-implemented))

(defprimitive "undefinedresult" () (not-implemented))

(defprimitive "unmatchedmark" () (not-implemented))

(defprimitive "unregistered" () (not-implemented))

(defprimitive "VMerror" () (not-implemented))

;;; ZPS convenience functions

(defprimitive "zpsdump" ()
  (flet ((dump-dict (dict)
           (maphash (lambda (key value)
                      (display key)
                      (write-string " => ")
                      (display value)
                      (terpri))
                    (table dict))
           (terpri)))
    (loop for i from (1- (pointer *dictionary-stack*)) downto 1
          do (dump-dict (aref (storage *dictionary-stack*) i)))))
