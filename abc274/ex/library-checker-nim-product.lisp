;; FIXME: *RECURSION-DEPTH* should be included within the macro.
(declaim (type (integer 0 #.most-positive-fixnum) *recursion-depth*))
(defparameter *recursion-depth* 0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %enclose-with-trace (fname args form)
    (let ((value (gensym)))
      `(progn
         (format t "~&~A~A: (~A ~{~A~^ ~}) =>"
                 (make-string *recursion-depth*
                              :element-type 'base-char
                              :initial-element #\ )
                 *recursion-depth*
                 ',fname
                 (list ,@args))
         (let ((,value (let ((*recursion-depth* (1+ *recursion-depth*)))
                         ,form)))
           (format t "~&~A~A: (~A ~{~A~^ ~}) => ~A"
                   (make-string *recursion-depth*
                                :element-type 'base-char
                                :initial-element #\ )
                   *recursion-depth*
                   ',fname
                   (list ,@args)
                   ,value)
           ,value))))

  (defun %extract-declarations (body)
    (remove-if-not (lambda (form) (and (consp form) (eql 'declare (car form))))
                   body))

  (defun %parse-cache-form (cache-specifier)
    (let ((cache-type (car cache-specifier))
          (cache-attribs (cdr cache-specifier)))
      (assert (member cache-type '(:hash-table :array)))
      (let* ((dims-with-* (when (eql cache-type :array) (first cache-attribs)))
             (dims (remove '* dims-with-*))
             (rank (length dims))
             (rest-attribs (ecase cache-type
                             (:hash-table cache-attribs)
                             (:array (cdr cache-attribs))))
             (key (prog1 (getf rest-attribs :key) (remf rest-attribs :key)))
             (trace-p (prog1 (getf rest-attribs :trace) (remf rest-attribs :trace)))
             (cache-form (case cache-type
                           (:hash-table `(make-hash-table ,@rest-attribs))
                           (:array `(make-array (list ,@dims) ,@rest-attribs))))
             (initial-element (when (eql cache-type :array)
                                (assert (member :initial-element rest-attribs))
                                (getf rest-attribs :initial-element))))
        (let ((cache (gensym "CACHE"))
              (value (gensym))
              (present-p (gensym))
              (args-lst (gensym))
              (indices (loop repeat rank collect (gensym))))
          (labels
              ((make-cache-querier (cache-type name args)
                 (let ((res (case cache-type
                              (:hash-table
                               `(let ((,args-lst (funcall ,(or key '#'list) ,@args)))
                                  (multiple-value-bind (,value ,present-p)
                                      (gethash ,args-lst ,cache)
                                    (if ,present-p
                                        ,value
                                        (setf (gethash ,args-lst ,cache)
                                              (,name ,@args))))))
                              (:array
                               (assert (= (length args) (length dims-with-*)))
                               (let ((memoized-args (loop for dimension in dims-with-*
                                                          for arg in args
                                                          unless (eql dimension '*)
                                                          collect arg)))
                                 (if key
                                     `(multiple-value-bind ,indices
                                          (funcall ,key ,@memoized-args)
                                        (let ((,value (aref ,cache ,@indices)))
                                          (if (eql ,initial-element ,value)
                                              (setf (aref ,cache ,@indices)
                                                    (,name ,@args))
                                              ,value)))
                                     `(let ((,value (aref ,cache ,@memoized-args)))
                                        (if (eql ,initial-element ,value)
                                            (setf (aref ,cache ,@memoized-args)
                                                  (,name ,@args))
                                            ,value))))))))
                   (if trace-p
                       (%enclose-with-trace name args res)
                       res)))
               (make-reset-form (cache-type)
                 (case cache-type
                   (:hash-table `(setf ,cache (make-hash-table ,@rest-attribs)))
                   (:array `(prog1 nil
                              ;; TODO: portable fill
                              (fill (sb-ext:array-storage-vector ,cache) ,initial-element)))))
               (make-reset-name (name)
                 (intern (format nil "RESET-~A" (symbol-name name)))))
            (values cache cache-form cache-type
                    #'make-reset-name
                    #'make-reset-form
                    #'make-cache-querier)))))))

(defmacro with-cache ((cache-type &rest cache-attribs) def-form)
  "CACHE-TYPE := :HASH-TABLE | :ARRAY.
DEF-FORM := definition form with DEFUN, LABELS, FLET, or SB-INT:NAMED-LET.

Basic usage:

\(with-cache (:hash-table :test #'equal :key #'cons)
  (defun add (a b)
    (+ a b)))

This function caches the returned values for already passed combinations of
arguments. In this case ADD stores the key (CONS A B) and the returned value to
a hash-table when (ADD A B) is evaluated for the first time. When it is called
with the same arguments (w.r.t. EQUAL) again, ADD will return the stored value
instead of recomputing it.

The storage for cache can be hash-table or array. Let's see an example for
array:

\(with-cache (:array (10 20 30) :initial-element -1 :element-type 'fixnum)
  (defun foo (a b c) ... ))

This form stores the value returned by FOO in an array, which was created
by (make-array (list 10 20 30) :initial-element -1 :element-type 'fixnum). Note
that INITIAL-ELEMENT must always be given here as it is used as the flag
expressing `not yet stored'. (Therefore INITIAL-ELEMENT should be a value FOO
never takes.)

If you want to ignore some arguments, you can put `*' in dimensions:

\(with-cache (:array (10 10 * 10) :initial-element -1)
  (defun foo (a b c d) ...)) ; then C is ignored when querying or storing cache

Available definition forms in WITH-CACHE are DEFUN, LABELS, FLET, and
SB-INT:NAMED-LET.

You can trace a memoized function by :TRACE option:

\(with-cache (:array (10 10) :initial-element -1 :trace t)
  (defun foo (x y) ...))

Then FOO is traced as with CL:TRACE.
"
  (multiple-value-bind (cache-symbol cache-form cache-type
                        make-reset-name make-reset-form
                        make-cache-querier)
      (%parse-cache-form (cons cache-type cache-attribs))
    (ecase (car def-form)
      ((defun)
       (destructuring-bind (_ name args &body body) def-form
         (declare (ignore _))
         `(let ((,cache-symbol ,cache-form))
            (defun ,(funcall make-reset-name name) ()
              ,(funcall make-reset-form cache-type))
            (defun ,name ,args
              ,@(%extract-declarations body)
              (flet ((,name ,args ,@body))
                (declare (inline ,name))
                ,(funcall make-cache-querier cache-type name args))))))
      ((labels flet)
       (destructuring-bind (_ definitions &body labels-body) def-form
         (declare (ignore _))
         (destructuring-bind (name args &body body) (car definitions)
           `(let ((,cache-symbol ,cache-form))
              (,(car def-form)
               ((,(funcall make-reset-name name) ()
                 ,(funcall make-reset-form cache-type))
                (,name ,args
                       ,@(%extract-declarations body)
                       (flet ((,name ,args ,@body))
                         (declare (inline ,name))
                         ,(funcall make-cache-querier cache-type name args)))
                ,@(cdr definitions))
               (declare (ignorable #',(funcall make-reset-name name)))
               ,@labels-body)))))
      ((nlet #+sbcl sb-int:named-let)
       (destructuring-bind (_ name bindings &body body) def-form
         (declare (ignore _))
         `(let ((,cache-symbol ,cache-form))
            (,(car def-form) ,name ,bindings
             ,@(%extract-declarations body)
             ,(let ((args (mapcar (lambda (x) (if (atom x) x (car x))) bindings)))
                `(flet ((,name ,args ,@body))
                   (declare (inline ,name))
                   ,(funcall make-cache-querier cache-type name args))))))))))

(defmacro with-caches (cache-specs def-form)
  "DEF-FORM := definition form by LABELS or FLET.

\(with-caches (cache-spec1 cache-spec2)
  (labels ((f (x) ...) (g (y) ...))))
is equivalent to the line up of
\(with-cache cache-spec1 (labels ((f (x) ...))))
and
\(with-cache cache-spec2 (labels ((g (y) ...))))

This macro will be useful to do mutual recursion between memoized local
functions."
  (assert (member (car def-form) '(labels flet)))
  (let (cache-symbol-list cache-form-list cache-type-list make-reset-name-list make-reset-form-list make-cache-querier-list)
    (dolist (cache-spec (reverse cache-specs))
      (multiple-value-bind (cache-symbol cache-form cache-type
                            make-reset-name make-reset-form make-cache-querier)
          (%parse-cache-form cache-spec)
        (push cache-symbol cache-symbol-list)
        (push cache-form cache-form-list)
        (push cache-type cache-type-list)
        (push make-reset-name make-reset-name-list)
        (push make-reset-form make-reset-form-list)
        (push make-cache-querier make-cache-querier-list)))
    (labels ((def-name (def) (first def))
             (def-args (def) (second def))
             (def-body (def) (cddr def)))
      (destructuring-bind (_ definitions &body labels-body) def-form
        (declare (ignore _))
        `(let ,(loop for cache-symbol in cache-symbol-list
                     for cache-form in cache-form-list
                     collect `(,cache-symbol ,cache-form))
           (,(car def-form)
            (,@(loop for def in definitions
                     for cache-type in cache-type-list
                     for make-reset-name in make-reset-name-list
                     for make-reset-form in make-reset-form-list
                     collect `(,(funcall make-reset-name (def-name def)) ()
                               ,(funcall make-reset-form cache-type)))
             ,@(loop for def in definitions
                     for cache-type in cache-type-list
                     for make-cache-querier in make-cache-querier-list
                     collect `(,(def-name def) ,(def-args def)
                               ,@(%extract-declarations (def-body def))
                               (flet ((,(def-name def) ,(def-args def) ,@(def-body def)))
                                 (declare (inline ,(def-name def)))
                                 ,(funcall make-cache-querier cache-type (def-name def) (def-args def))))))
            (declare (ignorable ,@(loop for def in definitions
                                        for make-reset-name in make-reset-name-list
                                        collect `#',(funcall make-reset-name
                                                             (def-name def)))))
            ,@labels-body))))))


(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro mvbind (vars value-form &body body)
  `(multiple-value-bind ,vars ,value-form ,@body))

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression ,@body))

(eval-always
  (defun |#@-aux| (typespec)
    (dbind (type-specifier . args) typespec
      `(type ,type-specifier ,@args)))

  (set-dispatch-macro-character
   #\# #\@
   (lambda (stream char num)
     (declare (ignore char num))
     (let ((typespecs (read stream t nil t)))
       `(declare ,(|#@-aux| typespecs)
                 (optimize speed (safety 1))))))
  ) ;eval-always

(defmacro readlet (vars &body body)
  `(let ,(mapcar (lambda (v)
                   `(,v (read-fixnum)))
          vars)
     ,@body))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym ,(string s))))
          syms)
     ,@body))

;; from serapeum
;; https://github.com/ruricolist/serapeum/blob/master/LICENSE.txt
;; https://github.com/ruricolist/serapeum/blob/master/definitions.lisp#L129
(defmacro defsubst (name params &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params
       ,@body)))

(defmacro defmemo ((name &key (key ''first) (test ''eql)) lambda-list &body body)
  (with-gensyms (table args k val found-p)
    `(let ((,table (make-hash-table :test ,test)))
       (defun ,name (&rest ,args)
         (let ((,k (funcall ,key ,args)))
           (mvbind (,val ,found-p)
               (gethash ,k ,table)
             (if ,found-p ,val
                 (setf (gethash ,k ,table)
                       (dbind ,lambda-list ,args
                         ,@body)))))))))

(eval-always
  (set-dispatch-macro-character
   #\# #\>
   (lambda (stream char num)
     (declare (ignore char num))
     (with-gensyms (x)
       (cond ((char= #\= (peek-char t stream))
              (read-char stream)
              `(lambda (,x) (>= ,x ,(read stream t nil t))))
             (t
              `(lambda (,x) (> ,x ,(read stream t nil t)))))))))

(eval-always
  (make-dispatch-macro-character #\$)

  (set-dispatch-macro-character
   #\$ #\(
   (lambda (stream char num)
     (declare (ignore char num))
     (let ((list (read-delimited-list #\) stream t)))
       `(curry #',(car list) ,@(cdr list))))))

(eval-always
  (make-dispatch-macro-character #\@)

  (set-dispatch-macro-character
   #\@ #\(
   (lambda (stream char num)
     (declare (ignore char num))
     (let ((list (read-delimited-list #\) stream t)))
       `(compose ,@list)))))

(declaim (ftype (function * (values fixnum &optional)) read-fixnum))
(defun read-fixnum (&optional (in *standard-input*))
  "NOTE: cannot read -2^62"
  (macrolet ((%read-byte ()
               `(the (unsigned-byte 8)
                     #+swank (char-code (read-char in nil #\Nul))
                     #-swank (sb-impl::ansi-stream-read-byte in nil #.(char-code #\Nul) nil))))
    (let* ((minus nil)
           (result (loop (let ((byte (%read-byte)))
                           (cond ((<= 48 byte 57)
                                  (return (- byte 48)))
                                 ((zerop byte) ; #\Nul
                                  (error "Read EOF or #\Nul."))
                                 ((= byte #.(char-code #\-))
                                  (setq minus t)))))))
      #@((integer 0 #.most-positive-fixnum) result)
      (loop
        (let* ((byte (%read-byte)))
          (if (<= 48 byte 57)
              (setq result (+ (- byte 48)
                              (* 10 (the (integer 0 #.(floor most-positive-fixnum 10))
                                         result))))
              (return (if minus (- result) result))))))))

(defsubst println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format*
          (if (typep obj 'double-float) 'double-float *read-default-float-format*)))
    (prog1 (princ obj stream) (terpri stream))))

(deftype u62 () '(unsigned-byte 62))
(deftype u64 () '(unsigned-byte 64))
(deftype u32 () '(unsigned-byte 32))
(deftype ash-count () '(integer 0 32))

(defun foldl (function initial-value sequence)
  (reduce function sequence :initial-value initial-value))

;;(declaim (ftype (function (u64 ash-count) u64) lash))
;; (defsubst lash (u64 count)
;;   #@(u64 u64)
;;   #@(ash-count count)
;;   (ash u64 count))

(defsubst lash (integer count)
  #@(u32 integer)
  #@(ash-count count)
  (ash integer count))

;; (declaim (ftype (function (u64 ash-count)) rash))
;; (defsubst rash (u64 count)
;;   #@(u64 u64)
;;   #@(ash-count count)
;;   (ash u64 count))

(defsubst rash (integer count)
  #@(u64 integer)
  #@(ash-count count)
  (ash integer (- count)))

(defun floor-2p (number 2-power)
  #@(u64 number)
  #@(ash-count 2-power)
  (let ((mask (1- (lash 1 2-power))))
    (values (rash number 2-power)
            (logand number mask))))

(defsubst nim+ (&rest integers)
  (apply #'logxor integers))

;; https://hitonanode.github.io/cplib-cpp/number/nimber.hpp
(defun nim*2-rec (a b rec)
;;  #@(u64 a b)
;;  #@(function rec)
  (labels ((rec (bit)
;;             #@(fixnum bit)
             (cond ((plusp (rash a bit))
                    (mvbind (a0 a1) (floor-2p a bit)
                      (mvbind (b0 b1) (floor-2p b bit)
                        (let ((p00 (funcall rec a0 b0))
                              (p01 (funcall rec a0 b1))
                              (p10 (funcall rec a1 b0))
                              (p11 (funcall rec a1 b1)))
;                          #@(fixnum p00 p01 p10 p11)
                          (nim+ p11
                                (lash (nim+ p00 p01 p10) bit)
                                (funcall rec p00 (lash 1 (1- bit))))))))
                   ((plusp (rash b bit))
                    (mvbind (b0 b1) (floor-2p b bit)
;;                      #@(fixnum b0)
                      (nim+ (lash (funcall rec a b0) bit)
                            (funcall rec a b1))))
                   (t
                    (rec (rash bit 1))))))
    (rec 32)))

(defsubst smallp (a)
  (< a 256))

(with-cache (:array (256 256) :initial-element -1 :element-type 'fixnum)
  (defun nim*2-small (a b)
    ;;  #@(fixnum a b)
    (when (> a b)
      (rotatef a b))
    ;; (<= a b)
    (cond ((= a 0) 0)
          ((= a 1) b)
          (t (nim*2-rec a b #'nim*2-small)))))
         
(defun nim*2 (a b)
;;  #@(u64 a b)
  (when (> a b)
    (rotatef a b))
  ;; (<= a b)
  (cond ((= a 0) 0)
        ((= a 1) b)
        ((smallp b) (nim*2-small a b))
        (t (nim*2-rec a b #'nim*2))))

(defun main ()
  (readlet (n)
    (dotimes (_ n)
      (let ((a (read))
            (b (read)))
        (println (nim*2 a b))))))

(main)
