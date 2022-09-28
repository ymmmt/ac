(in-package :cl-user)

#-swank
(unless (member :child-sbcl *features*)
  (quit
   :recklessly-p t
   :unix-status
   (process-exit-code
    (run-program *runtime-pathname*
                 `("--control-stack-size" "256MB"
;;                   "--dynamic-space-size" "64MB"
                   "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                   "--eval" "(push :child-sbcl *features*)"
                   "--script" ,(namestring *load-pathname*))
                 :output t :error t :input t))))

;;; BEGIN_INSERTED_CONTENTS


;;; BEGIN_USE_PACKAGE

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (use-package :cp/ :cl-user))

;;; Utilities

(in-package :cl-user)

(macrolet ((def (b)
             `(progn (deftype ,(intern (format nil "UINT~A" b)) () '(unsigned-byte ,b))
                     (deftype ,(intern (format nil "INT~A" b)) () '(signed-byte ,b))))
           (define-int-types (&rest bits) `(progn ,@(mapcar (lambda (b) `(def ,b)) bits))))
  (define-int-types 2 4 7 8 15 16 31 32 62 63 64))

(defmacro dbg (&rest forms)
  (declare (ignorable forms))
  (if (= (length forms) 1)
      `(format t "~A => ~A~%" ',(car forms) ,(car forms))
      `(format t "~A => ~A~%" ',forms `(,,@forms))))

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-always
  (defun mksym (control-string &rest format-arguments)
    (intern (apply #'format nil control-string format-arguments))))

(defmacro mvbind (vars value-form &body body)
  `(multiple-value-bind ,vars ,value-form ,@body))

(defmacro mvcall (function arg &rest arguments)
  `(multiple-value-call ,function ,arg ,@arguments))

(defmacro mvlist (value-form)
  `(multiple-value-list ,value-form))

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression ,@body))

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

(defmacro with-memo ((name lambda-list &body definition) &body body)
  (with-gensyms (table args val found-p)
    `(let ((,table (make-hash-table :test 'equal)))
       (labels ((,name (&rest ,args)
                  (mvbind (,val ,found-p)
                      (gethash ,args ,table)
                    (if ,found-p ,val
                        (setf (gethash ,args ,table)
                              (dbind ,lambda-list ,args
                                ,@definition))))))
         ,@body))))

(eval-always
  (defun ensure-form (body)
    (cond ((null body)
           nil)
          ((= (length body) 1)
           (car body))
          (t
           `(progn ,@body))))

  (defun definition->memoized-definition (definition table)
    (with-gensyms (args val found-p)
      (dbind (name lambda-list &body body) definition
        `(,name (&rest ,args)
                (mvbind (,val ,found-p)
                    (gethash ,args ,table)
                  (if ,found-p ,val
                      (setf (gethash ,args ,table)
                            (dbind ,lambda-list ,args
                              ,@body))))))))

  ) ; eval-when

(defmacro with-memos (definitions &body body)
  (if (null definitions)
      (ensure-form body)
      (let* ((n (length definitions))
             (tables (loop repeat n collect (gensym "TABLE"))))
        `(let ,(mapcar (lambda (table)
                         `(,table (make-hash-table :test 'equal)))
                tables)
           (labels ,(mapcar (lambda (definition table)
                              (definition->memoized-definition definition table))
                            definitions tables)
             ,@body)))))

(eval-always
  (defun ensure-list (x)
    (if (listp x)
        x
        (list x)))
  (defun string-gensym (symbol)
    (gensym (string symbol))))

;; from serapeum
;; https://github.com/ruricolist/serapeum/blob/master/LICENSE.txt
;; https://github.com/ruricolist/serapeum/blob/master/iter.lisp#L22
(defmacro nlet (name bindings &body body)
  (let* ((bindings (mapcar #'ensure-list bindings))
         (vars  (mapcar #'first bindings))
         (inits (mapcar #'second bindings))
         (temps (mapcar #'string-gensym vars))
         (gtag (string-gensym 'tag)))
    `(block ,name
       (let ,(mapcar #'list temps inits)
         (macrolet ((,name ,vars
                      `(progn
                         (psetq
                          ,@(mapcan #'list
                                    ',temps
                                    (list ,@vars)))
                         (go ,',gtag))))
           (tagbody
              ,gtag (return-from ,name
                      (let ,(mapcar #'list vars temps)
                        ,@body))))))))

(defmacro dlambda (lambda-list &body body)
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (dbind ,lambda-list ,args
         ,@body))))

(defmacro ddefun (name lambda-list &body body)
  (with-gensyms (args)
    `(defun ,name (&rest ,args)
       (dbind ,lambda-list ,args
         ,@body))))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(let ((it ,(car args)))
              (when it
                (aand ,@(cdr args)))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses)))
        (with-gensyms (sym)
          `(let ((,sym ,(car cl1)))
             (if ,sym
                 (let ((it ,sym)) ,@(cdr cl1))
                 (acond ,@(cdr clauses))))))))

(defun until (predicate function value &rest more-values)
  (declare (optimize speed (safety 1)))
  (declare (function predicate function))
  (if (funcall predicate value)
      value
      (mvcall #'until
              predicate
              function
              (apply function value more-values))))

(defun while (predicate function value &rest more-values)
  (declare (optimize speed (safety 1)))
  (declare (function predicate function))
  (if (funcall predicate value)
      (mvcall #'while
              predicate
              function
              (apply function value more-values))
      value))

(eval-always
  (defun extract-function-name (spec)
    (if (and (consp spec)
             (member (first spec) '(quote function)))
        (second spec)
        spec))

  (defun generate-switch-body (whole object clauses test key &optional default)
    (with-gensyms (value)
      (setf test (extract-function-name test))
      (setf key (extract-function-name key))
      (when (and (consp default)
                 (member (first default) '(error cerror)))
        (setf default `(,@default "No keys match in SWITCH. Testing against ~S with ~S."
                                  ,value ',test)))
      `(let ((,value (,key ,object)))
         (cond ,@(mapcar (lambda (clause)
                           (if (member (first clause) '(t otherwise))
                               (progn
                                 (when default
                                   (error "Multiple default clauses or illegal use of a default clause in ~S."
                                          whole))
                                 (setf default `(progn ,@(rest clause)))
                                 '(()))
                               (destructuring-bind (key-form &body forms) clause
                                 `((,test ,value ,key-form)
                                   ,@forms))))
                         clauses)
               (t ,default)))))
  ) ; eval-when

(defmacro switch (&whole whole (object &key (test 'eql) (key 'identity))
                         &body clauses)
  (generate-switch-body whole object clauses test key))

(defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  (generate-switch-body whole object clauses test key '(error)))

(defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  (generate-switch-body whole object clauses test key '(cerror "Return NIL from CSWITCH.")))

(eval-always
  (defmacro ensure-key (key expr)
    `(aif ,key
          (funcall it ,expr)
          ,expr)))

;;; Read macros

(eval-always
  (set-dispatch-macro-character
   #\# #\^
   (lambda (stream char num)
     (declare (ignore char))
     (unless num (setf num 1))
     `(lambda ,(loop for i from 1 to num
                     collect (mksym "%~d" i))
        ,(read stream t nil t)))))

(eval-always
  (set-dispatch-macro-character
   #\# #\<
   (lambda (stream char num)
     (declare (ignore char num))
     (cond ((char= #\= (peek-char t stream))
            (read-char stream)
            `(lambda (x) (<= x ,(read stream t nil t))))
           (t
            `(lambda (x) (< x ,(read stream t nil t))))))))

(eval-always
  (set-dispatch-macro-character
   #\# #\>
   (lambda (stream char num)
     (declare (ignore char num))
     (cond ((char= #\= (peek-char t stream))
            (read-char stream)
            `(lambda (x) (>= x ,(read stream t nil t))))
           (t
            `(lambda (x) (> x ,(read stream t nil t))))))))

(eval-always
  (set-dispatch-macro-character
   #\# #\%
   (lambda (stream char num)
     (declare (ignore char num))
     `(lambda (x) (mod x ,(read stream t nil t))))))

;;; IO

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
      (declare ((integer 0 #.most-positive-fixnum) result))
      (loop
        (let* ((byte (%read-byte)))
          (if (<= 48 byte 57)
              (setq result (+ (- byte 48)
                              (* 10 (the (integer 0 #.(floor most-positive-fixnum 10))
                                         result))))
              (return (if minus (- result) result))))))))

(defmacro collect (n expr)
  `(loop repeat ,n
         collect ,expr))

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

(defun readvector (&rest args)
  (values (read-from-string
           (concatenate 'string "#("
                        (apply #'read-line args)
                        ")"))))

(defun read-matrix (height width &key (read #'read-fixnum) (element-type 'fixnum) padding)
  (let ((mat (make-array (if padding
                             `(,(1+ height) ,(1+ width))
                             `(,height ,width))
                         :element-type element-type)))
    (dotimes (i height)
      (dotimes (j width)
        (if padding
            (setf (aref mat (1+ i) (1+ j)) (funcall read))
            (setf (aref mat i j) (funcall read)))))
    mat))

(defun read-char-matrix (height width)
  (let ((mat (make-array `(,height ,width) :element-type 'character)))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref mat i j) (read-char)))
      (read-char)) ; skip #\Newline
    mat))

(defun read-conses (n)
  (collect n
    (readlet (x y)
      (cons x y))))

(defun read-lists (n)
  (collect n (readlist)))

(defun read-edges (n-edges &key directed)
  (loop repeat n-edges
        nconc
        (readlet (u v)
          (if directed
              (list (cons (1- u) (1- v)))
              (list (cons (1- u) (1- v))
                    (cons (1- v) (1- u)))))))

(defun read-weighted-edges (n-edges &key directed)
  (nlet rec ((i 0) (edges nil) (ws nil))
    (if (= i n-edges)
        (values (nreverse edges)
                (nreverse ws))
        (readlet (u v w)
          (rec (1+ i)
               (if directed
                   (acons (1- u) (1- v) edges)
                   (acons (1- u) (1- v)
                          (acons (1- v) (1- u)
                                 edges)))
               (if directed
                   (cons w ws)
                   (list* w w ws)))))))

(defun read-graph (n-vertices n-edges &key directed)
  (let ((graph (make-list-array n-vertices)))
    (loop repeat n-edges do
      (readlet (u v)
        (push (1- v) (aref graph (1- u)))
        (unless directed
          (push (1- u) (aref graph (1- v))))))
    graph))

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":count ~A :test ~A"
            (hash-table-count object)
            (hash-table-test object))
    (loop for k being the hash-key of object
          do (format stream "~%[~A] ~A" k (gethash k object)))))

(defsubst println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format*
          (if (typep obj 'double-float) 'double-float *read-default-float-format*)))
    (prog1 (princ obj stream) (terpri stream))))

(defsubst printlns (lst)
  (format t "~{~A~%~}" lst))

(defmacro bulk-stdout (&body body)
  `(write-string
    (with-output-to-string (*standard-output* nil :element-type 'base-char)
      ,@body)))

(defun join-print (sequence &key (count (length sequence)) (key #'identity) (test (constantly t)) (sep " "))
  (let ((c 0))
    (map nil (lambda (item)
               (when (and (<= (incf c) count)
                          (funcall test item))
                 (princ (funcall key item))
                 (when (< c count)
                   (princ sep))))
         sequence)
    (fresh-line)))

;;; Functionals

(defmacro curry* (function &rest arguments)
  (assert (= 1 (count '% arguments :test 'eq)))
  (let ((g% (gensym)))
  `(lambda (,g%)
     (funcall ,function ,@(subst g% '% arguments)))))

(defun curry (function &rest arguments)
  (lambda (&rest args)
    (mvcall function
            (values-list arguments)
            (values-list args))))

(defun rcurry (function &rest arguments)
  (lambda (&rest args)
    (mvcall function
            (values-list args)
            (values-list arguments))))

(defun apply-n (n f x)
  (nlet rec ((n n) (x x))
    (if (zerop n)
        x
        (rec (1- n)
             (funcall f x)))))

(eval-always
  (defun tree-find (item tree &key (test #'eql) (key #'identity))
    (labels ((rec (tree)
               (cond ((funcall test item (funcall key tree))
                      (return-from tree-find tree))
                     ((consp tree)
                      (rec (car tree))
                      (rec (cdr tree))))))
      (rec tree))))

(defmacro -> (x &rest forms)
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (tree-find '% form)
                               `(funcall (lambda (%) ,form) ,x)
                               `(,(first form) ,x ,@(rest form)))
                           `(,form ,x))))
        `(-> ,threaded ,@(rest forms)))
      x))

(defmacro ->> (x &rest forms)
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (tree-find '% form)
                               `(funcall (lambda (%) ,form) ,x)
                               `(,(first form) ,@(rest form) ,x))
                           `(,form ,x))))
        `(->> ,threaded ,@(rest forms)))
      x))

(defsubst mapper (hash-table &optional default)
  (lambda (key)
    (gethash key hash-table default)))

(defsubst eqer (object)
  (lambda (x)
    (eq object x)))

(defsubst eqler (object)
  (lambda (x)
    (eql object x)))

(defsubst equaler (object)
  (lambda (x)
    (equal object x)))

(defsubst quantizer (predicate)
  (lambda (x)
    (if (funcall predicate x) 1 0)))

(defsubst applier (fn)
  (lambda (args)
    (apply fn args)))

(defsubst cons-applier (fn)
  (dlambda ((a . b))
    (funcall fn a b)))

(defsubst arefer (array)
  (lambda (&rest subscripts)
    (apply #'aref array subscripts)))

(defsubst adder (&rest numbers)
  (lambda (x) (apply #'+ x numbers)))

(defsubst suber (&rest  numbers)
  (lambda (x) (apply #'- x numbers)))

(defsubst muler (&rest numbers)
  (lambda (x) (apply #'* x numbers)))

(defsubst diver (&rest numbers)
  (lambda (x) (apply #'/ x numbers)))

(defun foldl (function initial-value sequence)
  (reduce function sequence :initial-value initial-value))

(defun foldl1 (function sequence)
  (if (zerop (length sequence))
      (error "Empty sequence.")
      (reduce function sequence)))

(defun foldr (function initial-value sequence)
  (reduce function sequence :initial-value initial-value :from-end t))

(defun foldr1 (function sequence)
  (if (zerop (length sequence))
      (error "Empty sequence.")
      (reduce function sequence :from-end t)))

(defun mvfoldl (function list initial-value &rest initial-args)
  "Generalization of FOLDL.
(FUNCTION item acc arg1 arg2 ... argN) => new-acc, new-arg1, new-arg2,..., new-argN
LIST -- list of items
INITIAL-ARGS == (initial-arg1 initial-arg2 ... initial-argN)"
  (declare (optimize (speed 3) (safety 1)))
  (declare (function function))
  (if (null list)
      (apply #'values initial-value initial-args)
      (mvcall #'mvfoldl
              function
              (cdr list)
              (apply function (car list) initial-value initial-args))))

(defun mvfoldl1 (function list &rest initial-args)
  (if (null list)
      (error "Empty list.")
      (apply #'mvfoldl function (cdr list) (car list) initial-args)))

(defun mvfoldr (function list initial-value &rest initial-args)
  "Generalization of FOLDR.
(FUNCTION item acc arg1 arg2 ... argN) => new-acc, new-arg1, new-arg2,..., new-argN
LIST -- list of items
INITIAL-ARGS == (initial-arg1 initial-arg2 ... initial-argN)"
  (apply #'mvfoldl function (reverse list) initial-value initial-args))

(defun mvfoldr1 (function list &rest initial-args)
  (if (null list)
      (error "Empty list.")
      (let ((rev (reverse list)))
        (apply #'mvfoldl function (cdr rev) (car rev) initial-args))))

(defun scanl (function initial-value list)
  (labels ((rec (list acc)
             (if (null list)
                 (nreverse acc)
                 (rec (cdr list) (cons (funcall function (car acc) (car list))
                                       acc)))))
    (rec list (list initial-value))))

(defun scanl1 (function list)
  (if (null list)
      (error "Empty list.")
      (scanl function (car list) (cdr list))))

(defun scanr (function initial-value list)
  (labels ((rec (list acc)
             (if (null list)
                 acc
                 (rec (cdr list)
                      (cons (funcall function (car list) (car acc))
                            acc)))))
    (rec (nreverse list) (list initial-value))))

(defun scanr1 (function list)
  (if (null list)
      (error "Empty list.")
      (scanr function (car (last list)) (nbutlast list))))

(defun ensure-function (sym-or-func)
  (cond ((symbolp sym-or-func) `',sym-or-func)
        ((or (functionp sym-or-func)
             (and (consp sym-or-func) (eq (car sym-or-func) 'lambda)))
         sym-or-func)
        (t (error "~A is not symbol or function." sym-or-func))))

(defun disjoin (predicate &rest more-predicates)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((predicate (ensure-function predicate))
        (more-predicates (mapcar #'ensure-function more-predicates)))
    (lambda (&rest arguments)
      (or (apply predicate arguments)
          (some (lambda (p)
                  (declare (type function p))
                  (apply p arguments))
                more-predicates)))))

(defun conjoin (predicate &rest more-predicates)
  (if (null more-predicates)
      predicate
      (lambda (&rest arguments)
        (and (apply predicate arguments)
             (do ((tail (cdr more-predicates) (cdr tail))
                  (head (car more-predicates) (car tail)))
                 ((not tail)
                  (apply head arguments))
               (unless (apply head arguments)
                 (return nil)))))))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;;; Numbers

(defconstant +mod+ 1000000007)
(defconstant +inf+ most-positive-fixnum)
(defconstant +ninf+ most-negative-fixnum)

(defsubst md (x)
  (mod x +mod+))

;; from cp/mod-operations
(eval-always
  (defvar *modulus* 0))
(declaim ((unsigned-byte 31) *modulus*)
         #+sbcl (sb-ext:always-bound *modulus*))

(defmacro define-mod-operations
    (divisor &optional (package #+sbcl (sb-int:sane-package) #-sbcl *package*))
  (let ((mod* (intern "MOD*" package))
        (mod+ (intern "MOD+" package))
        (mod- (intern "MOD-" package))
        (incfmod (intern "INCFMOD" package))
        (decfmod (intern "DECFMOD" package))
        (mulfmod (intern "MULFMOD" package)))
    `(progn
       (defun ,mod* (&rest args)
         (cond ((cdr args) (reduce (lambda (x y) (mod (* x y) ,divisor)) args))
               (args (mod (car args) ,divisor))
               (t (mod 1 ,divisor))))
       (defun ,mod+ (&rest args)
         (cond ((cdr args) (reduce (lambda (x y) (mod (+ x y) ,divisor)) args))
               (args (mod (car args) ,divisor))
               (t 0)))
       (defun ,mod- (&rest args)
         (if (cdr args)
             (reduce (lambda (x y) (mod (- x y) ,divisor)) args)
             (mod (- (car args)) ,divisor)))

       #+sbcl
       (eval-always
         (locally (declare (sb-ext:muffle-conditions warning))
           (sb-c:define-source-transform ,mod* (&rest args)
             (case (length args)
               (0 `(mod 1 ,',divisor))
               (1 `(mod ,(car args) ,',divisor))
               (otherwise (reduce (lambda (x y) `(mod (* ,x ,y) ,',divisor)) args))))
           (sb-c:define-source-transform ,mod+ (&rest args)
             (case (length args)
               (0 0)
               (1 `(mod ,(car args) ,',divisor))
               (otherwise (reduce (lambda (x y) `(mod (+ ,x ,y) ,',divisor)) args))))
           (sb-c:define-source-transform ,mod- (&rest args)
             (case (length args)
               (0 (values nil t))
               (1 `(mod (- ,(car args)) ,',divisor))
               (otherwise (reduce (lambda (x y) `(mod (- ,x ,y) ,',divisor)) args))))))

       (define-modify-macro ,incfmod (delta)
         (lambda (x y) (mod (+ x y) ,divisor)))
       (define-modify-macro ,decfmod (delta)
         (lambda (x y) (mod (- x y) ,divisor)))
       (define-modify-macro ,mulfmod (multiplier)
         (lambda (x y) (mod (* x y) ,divisor))))))

(define-mod-operations +mod+)

(defsubst oddity= (x y)
  (= (mod x 2) (mod y 2)))

(defsubst ^2 (x)
  (expt x 2))

(defsubst 2^ (x)
  (ash 1 x))

(defsubst 2* (x)
  (if (integerp x)
      (ash x 1)
      (* 2 x)))

(defsubst dist (x y)
  (abs (- x y)))

(defsubst df (x)
  (coerce x 'double-float))

(eval-always
  (defun mappend (function list)
    (loop for item in list
          append (funcall function item))))

(defmacro sortf (predicate &rest places)
  (let* ((meths (mapcar (lambda (p)
                          (mvlist
                           (get-setf-expansion p)))
                        places))
         (temps (mappend #'third meths)))
    `(let* ,(mapcar #'list
                    (mapcan (lambda (m)
                              (append (first m)
                                      (third m)))
                            meths)
                    (mapcan (lambda (m)
                              (append (second m)
                                      (list (fifth m))))
                            meths))
       ,@(mapcon (lambda (rest)
                   (mapcar
                    (lambda (arg)
                      `(unless (,predicate ,(car rest) ,arg)
                         (rotatef ,(car rest) ,arg)))
                    (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))

(defun binary-search (left right predicate)
  "invariant: (funcall predicate left) => t, (funcall predicate right) => nil"
  (labels ((rec (left right)
             (let ((mid (ash (+ left right) -1)))
               (cond ((= (1+ left) right)
                      left)
                     ((funcall predicate mid)
                      (rec mid right))
                     (t
                      (rec left mid))))))
    (rec left right)))

;;; Hash tables

(defun alist->ht (alist &key (test #'eql))
  (let ((ht (make-hash-table :test test)))
    (mapc (dlambda ((k . v))
            (setf (gethash k ht) v))
          alist)
    ht))

(defun ht->alist (ht)
  (let (result)
    (maphash (lambda (k v)
               (push (cons k v) result))
             ht)
    result))

(defun ht (keys values &key (test #'eql))
  (let ((ht (make-hash-table :test test)))
    (mapc (lambda (k v)
            (setf (gethash k ht) v))
          keys values)
    ht))

(defun make-hashset (list &key (test #'eql))
  (let ((ht (make-hash-table :test test)))
    (dolist (item list ht)
      (setf (gethash item ht) t))))

(defun counter (sequence &key (test #'eql) (key #'identity))
  (let ((counter (make-hash-table :size (length sequence) :test test)))
    (map nil (lambda (elem)
               (let ((k (funcall key elem)))
                 (setf (gethash k counter)
                       (1+ (gethash k counter 0)))))
         sequence)
    counter))

(defun ht-keys (hash-table)
  (loop for k being the hash-key of hash-table
        collect k))

(defun ht-values (hash-table)
  (loop for v being the hash-value of hash-table
        collect v))

(defmacro do-hashkeys ((key hash-table &optional result) &body body)
  `(loop for ,key being the hash-key of ,hash-table
         do ,@body
         finally (return ,result)))

(defmacro do-hashvalues ((value hash-table &optional result) &body body)
  `(loop for ,value being the hash-value of ,hash-table
         do ,@body
         finally (return ,result)))

(defmacro accum-ht (alist value-init-form update &key (test ''eql))
  (with-gensyms (ht k v value found-p gupdate)
    `(let ((,ht (make-hash-table :test ,test))
           (,gupdate ,update))
       (loop for (,k . ,v) in ,alist do
         (mvbind (,value ,found-p) (gethash ,k ,ht)
           (if ,found-p
               (setf (gethash ,k ,ht)
                     (funcall ,gupdate ,value ,v))
               (setf (gethash ,k ,ht)
                     (funcall ,gupdate ,value-init-form ,v)))))
       ,ht)))

(defun vector-accum-ht (alist &key (test #'eql))
  (labels ((vpush (v obj)
             (vector-push-extend obj v)
             v))
    (accum-ht alist (make-adj-array) #'vpush :test test)))

(defun list-accum-ht (alist &key (test #'eql))
  (labels ((snoc (list obj)
             (cons obj list)))
    (let ((ht (accum-ht alist nil #'snoc :test test)))
      (do-hashkeys (k ht)
        (setf (gethash k ht)
              (nreverse (gethash k ht))))
      ht)))

(defun count-accum-ht (alist &key (test #'eql))
  (accum-ht alist 0 #'+ :test test))

;;; Arrays

(defun coerce-vector (object)
  (coerce object 'vector))

(defun pad (vector)
  (concatenate 'vector #(0) vector))

(defun slice (vec beg &optional (end (length vec)))
  (loop (multiple-value-bind (disp-to disp-index) (array-displacement vec)
          (if disp-to
              (setf vec disp-to
                    beg (+ beg disp-index)
                    end (when end (+ end disp-index)))
              (return))))
  (let ((size (max 0 (- (or end (length vec)) beg))))
    (apply #'make-array size :element-type (array-element-type vec)
           (unless (zerop size)
             (list :displaced-to vec :displaced-index-offset beg)))))

(defun make-bit-array (dimensions &rest initargs)
  (apply #'make-array dimensions :element-type 'bit initargs))

(defun make-list-array (dimensions)
  (make-array dimensions :element-type 'list
                         :initial-element nil))

(defun make-fixnum-array (dimensions &rest initargs)
  (apply #'make-array dimensions :element-type 'fixnum initargs))

(defun make-adj-array (&optional (length 0))
  (make-array length :fill-pointer 0 :adjustable t))

(defsubst row-major-index (i j n-cols)
  (+ (* i n-cols) j))

(eval-always
  (defun var-and-dimension-spec->loop (var-and-dimension-spec body)
    (dbind (var upper-bound &key downward) var-and-dimension-spec
      (if downward
          `(loop for ,var from (1- ,upper-bound) downto 0
                 do ,body)
          `(loop for ,var below ,upper-bound
                 do ,body)))))

(defmacro make-array-with-content (var-and-dimension-specs &body body)
  `(let ((self (make-array (list ,@(mapcar #'second var-and-dimension-specs)))))
     ,(reduce (lambda (var-and-dimension-spec acc)
                (var-and-dimension-spec->loop var-and-dimension-spec acc))
              var-and-dimension-specs
              :from-end t
              :initial-value `(setf (aref self ,@(mapcar #'first var-and-dimension-specs))
                                    ,(ensure-form body)))
     self))

(defun accum-array (alist dimensions &rest initargs)
  (let ((a (apply #'make-array dimensions initargs)))
    (mapc (dlambda ((subscripts . value))
            (setf (apply #'aref a subscripts)
                  value))
          alist)
    a))

(defun arr (subscripts-list values dimensions &rest initargs)
  (let ((a (apply #'make-array dimensions initargs)))
    (mapc (lambda (subscripts value)
            (setf (apply #'aref a (ensure-list subscripts))
                  value))
          subscripts-list values)
    a))

(defun matrix (h w subscripts-alist values &rest initargs)
  (let ((m (apply #'make-array `(,h ,w) initargs)))
    (mapc (dlambda ((i . j) value)
            (setf (aref m i j) value))
          subscripts-alist values)
    m))

(defun make-arrayset (natural-numbers &optional (max (reduce #'max natural-numbers)))
  (assert (<= max #.(expt 10 6)))
  (let ((a (make-bit-array (1+ max))))
    (mapc (lambda (i)
            (setf (aref a i) 1))
          natural-numbers)
    a))

;;; Lists/Sequences

(defun coerce-list (object)
  (coerce object 'list))

(eval-always
  (defun singletonp (list)
    (and (consp list) (null (cdr list)))))

(defsubst uncons (list)
  (assert (consp list))
  (values (car list) (cdr list)))

(defun length>= (list1 list2)
  (labels ((rec (l1 l2)
             (cond ((null l2) t)
                   ((null l1) nil)
                   (t
                    (rec (cdr l1) (cdr l2))))))
    (rec list1 list2)))

(defun length> (list1 list2)
  (labels ((rec (l1 l2)
             (cond ((null l1) nil)
                   ((null l2) t)
                   (t
                    (rec (cdr l1) (cdr l2))))))
    (rec list1 list2)))

(defun length<= (n list)
  (cond ((null list)
         (>= n 0))
        ((zerop n)
         nil)
        (t
         (length<= (1- n) (cdr list)))))

(defun all-same-p (list &key (test #'eql))
  (or (null list)
      (every (curry test (car list))
             (cdr list))))

(defun range (&rest args)
  (ecase (length args)
    (1 (let ((end (car args)))
         (loop for i below end
               collect i)))
    (2 (dbind (start end) args
         (loop for i from start below end
               collect i)))
    (3 (dbind (start end step) args
         (loop for i from start below end by step
               collect i)))))

(defun drange (&rest args)
  (nreverse (apply #'range args)))

(declaim (ftype (function (fixnum fixnum fixnum) fixnum) range-last))
(defun range-last (start end step)
  (assert (< start end))
  (+ start (* step (floor (- end start 1) step))))

(defun map-range (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (loop for i fixnum from start below end by step
        collect (funcall function i)))

(defun map-drange (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (when (< start end)
    (let ((last (range-last start end step)))
      (loop for i fixnum = last then (the fixnum (- i step))
            while (>= i start)
            collect (funcall function i)))))

(defun mapc-range (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (loop for i fixnum from start below end by step
        do (funcall function i)))

(defun mapc-drange (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (when (< start end)
    (let ((last (range-last start end step)))
      (loop for i fixnum = last then (the fixnum (- i step))
            while (>= i start)
            do (funcall function i)))))

(defun filter-range (predicate start end &optional (step 1))
  (declare (function predicate)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (loop for i fixnum from start below end by step
        when (funcall predicate i)
          collect i))

(defun filter-map-range (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (loop for i fixnum from start below end by step
        for item = (funcall function i)
        when item
          collect item))

(defun range-foldl (function initial-value start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (let ((acc initial-value))
    (loop for i fixnum from start below end by step
          do (setf acc (funcall function acc i)))
    acc))

(defun range-foldl1 (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (assert (< start end))
  (range-foldl function start (1+ start) end step))

(defun range-foldr (function initial-value start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (if (>= start end)
      (list initial-value)
      (let ((last (range-last start end step))
            (acc initial-value))
        (loop for i fixnum = last then (the fixnum (- i step))
              while (>= i start)
              do (setf acc (funcall function i acc)))
        acc)))

(defun range-foldr1 (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (assert (< start end))
  (let ((last (range-last start end step)))
    (range-foldr function last start last step)))

(defun range-scanl (function initial-value start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (let ((acc initial-value))
    (cons initial-value
          (loop for i fixnum from start below end by step
                do (setf acc (funcall function acc i))
                collect acc))))  

(defun range-scanl1 (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (assert (< start end))
  (range-scanl function start (1+ start) end step))

(defun range-scanr (function initial-value start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (if (>= start end)
      (list initial-value)
      (let ((last (range-last start end step))
            (acc initial-value))
        (nreverse
         (cons initial-value
               (loop for i fixnum = last then (the fixnum (- i step))
                     while (>= i start)
                     do (setf acc (funcall function i acc))
                     collect acc))))))

(defun range-scanr1 (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (assert (< start end))
  (let ((last (range-last start end step)))
    (range-scanr function last start last step)))

(defun find-if*-range (predicate start end &optional (step 1))
  (declare (function predicate)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (loop for i fixnum from start below end by step
        for value = (funcall predicate i)
        when value
          return (values value i)))

(defun find-if*-drange (predicate start end &optional (step 1))
  (declare (function predicate)
           (fixnum start end step)
           (optimize speed (safety 1)))
  (when (< start end)
    (let ((last (range-last start end step)))
      (loop for i fixnum = last then (the fixnum (- i step))
            while (>= i start)
            for value = (funcall predicate i)
            when value
              return (values value i)))))

(defun take (n list &key (step 1))
  (nlet rec ((n n) (list list) (acc nil))
    (if (or (zerop n) (null list))
        (nreverse acc)
        (rec (1- n)
             (nthcdr step list)
             (cons (car list) acc)))))

(defun take-while (predicate list)
  (nlet rec ((list list) (acc nil))
    (if (and list
             (funcall predicate (car list)))
        (rec (cdr list)
             (cons (car list) acc))
        (nreverse acc))))

(defsubst drop (n list)
  (nthcdr n list))

(defun drop-while (predicate list &key count)
  (if (or (and count (zerop count))
          (null list)
          (not (funcall predicate (car list))))
      list
      (drop-while predicate (cdr list)
                  :count (if count (1- count) nil))))

(defun drop-while-end (predicate list)
  (nreverse (drop-while predicate (reverse list))))
  
(defun split (item list &key (test #'eql) omit-nulls)
  (split-if (curry test item)
            list
            :omit-nulls omit-nulls))

(defun split-if (predicate list &key omit-nulls)
  (labels ((rec (rest &optional temp-acc)
             (cond ((null rest)
                    (if temp-acc
                        (list (nreverse temp-acc))
                        nil))
                   ((funcall predicate (car rest))
                    (cond ((eq rest list)
                           (rec (cdr rest) nil))
                          ((and omit-nulls (null temp-acc))
                           (rec (cdr rest) nil))
                          (t
                           (cons (nreverse temp-acc)
                                 (rec (cdr rest) nil)))))
                   (t
                    (rec (cdr rest)
                         (cons (car rest) temp-acc))))))
    (rec list)))

(defun split-at (n list)
  (labels ((rec (n list left)
             (if (or (zerop n) (null list))
                 (values (nreverse left) list)
                 (rec (1- n) (cdr list) (cons (car list) left)))))
    (rec n list nil)))

(defun span-if (predicate list)
  "(span-if p list) == 
(values (take-while p list) (drop-while p list))"
  (break-if (complement predicate) list))

(defun break-if (predicate list)
  "(break-if p list) == 
(values (take-while (not p) list) (drop-while (not p) list))"
  (nlet rec ((list list) (acc nil))
    (if (or (null list) (funcall predicate (car list)))
        (values (nreverse acc) list)
        (rec (cdr list)
             (cons (car list) acc)))))

(defun partition (predicate list)
  (labels ((rec (list left right)
             (cond ((null list)
                    (values left right))
                   ((funcall predicate (car list))
                    (rec (cdr list)
                         (cons (car list) left)
                         right))
                   (t
                    (rec (cdr list)
                         left
                         (cons (car list) right))))))
    (rec (nreverse list) nil nil)))

(defun strip-prefix (prefix list &key (test #'eql))
  (if (prefixp prefix list :test test)
      (values (drop (length prefix) list) t)
      (values list nil)))

(eval-always
  (defun last1 (sequence)
    (etypecase sequence
      (list (car (last sequence)))
      (vector (when (plusp (length sequence))
                (aref sequence (1- (length sequence))))))))

(defsubst repeat (n item)
  (make-list n :initial-element item))

(defun alist (keys data &optional alist)
  (nconc (nreverse (pairlis keys data))
         alist))

(defun zip (list &rest more-lists)
  (apply #'mapcar #'list list more-lists))

(defun zip-with (function list &rest more-lists)
  (apply #'mapcar function list more-lists))

(defun zip* (&rest lists)
  (when lists
    (apply #'zip-with* #'list lists)))

(defun zip-with* (fn &rest lists)
  (when lists
    (labels ((rec (lists acc)
               (if (every #'null lists)
                   (nreverse acc)
                   (rec (mapcar #'cdr lists)
                        (cons (apply fn (mapcar #'car lists))
                              acc)))))
      (rec lists nil))))

(defun zip-with-index (list &optional (index-base 0))
  (mapcar (let ((i (1- index-base)))
            (lambda (item)
              (cons item (incf i))))
          list))

(defun unzip (alist)
  (mvfoldr (dlambda ((k . v) keys values)
             (values (cons k keys)
                     (cons v values)))
           alist nil nil))

(defun filter (predicate sequence &key from-end (start 0) end count key)
  (remove-if-not predicate sequence
                 :from-end from-end
                 :start start
                 :end end
                 :count count
                 :key key))

;; (defun filter-map (function list)
;;   (nreverse
;;    (foldl (lambda (acc item)
;;             (aif (funcall function item)
;;                  (cons it acc)
;;                  acc))
;;           nil
;;           list)))

;; more general
(defun filter-map (f list &rest more-lists)
  (labels ((rec (list more-lists acc)
             (if (and (consp list) (every #'consp more-lists))
                 (rec (cdr list)
                      (mapcar #'cdr more-lists)
                      (let ((value (apply f (car list) (mapcar #'car more-lists))))
                        (if value
                            (cons value acc)
                            acc)))
                 (nreverse acc))))
    (rec list more-lists nil)))

(defun position-filter (predicate sequence)
  (let ((pos -1))
    (remove-if-not (lambda (item)
                     (declare (ignore item))
                     (funcall predicate (incf pos)))
                   sequence)))

(defun map-adjacents (function list &optional (k 2))
  (nlet rec ((list list) (acc nil))
    (if (null (nthcdr (1- k) list))
        (nreverse acc)
        (rec (cdr list)
             (cons (apply function (take k list))
                   acc)))))

(defun intersperse (sep list)
  (when list
    (if (singletonp list)
        list
        (nreverse (foldl (lambda (acc item)
                           (list* item sep acc))
                         (list (car list))
                         (cdr list))))))         

(defun intercalate (sep list-of-lists)
  (flatten (intersperse sep list-of-lists)))

(defun flatten (tree)
  (let (acc)
    (labels ((rec (tree)
               (when tree
                 (if (consp tree)
                     (progn
                       (rec (car tree))
                       (rec (cdr tree)))
                     (push tree acc)))))
      (rec tree)
      (nreverse acc))))

(defun multiset (sequence test)
  (when (or (consp sequence) (>= (length sequence) 1))
    (nreverse
     (reduce (lambda (acc next)
               (dbind (curr . count) (car acc)
                 (if (funcall test curr next)
                     (cons (cons curr (1+ count)) (cdr acc))
                     (cons (cons next 1) acc))))
             (subseq sequence 1)
             :initial-value (list (cons (elt sequence 0) 1))))))

(defun delete-dups (list &key (order #'<) (test #'=))
  (mapcar #'car
          (multiset (sort list order)
                    test)))

(defun group (n list)
  (labels ((rec (src &optional acc)
             (let ((rest (nthcdr n src)))
               (if (consp rest)
                   (rec rest (cons (subseq src 0 n) acc))
                   (nreverse (cons src acc))))))
    (when list
      (rec list))))

(defun find-if* (predicate list &key from-end (start 0) end)
  (let ((list (subseq list start end)))
    (nlet rec ((list (if from-end (reverse list) list)))
      (if (null list)
          (values nil nil)
          (aif (funcall predicate (car list))
               (values it (car list))
               (rec (cdr list)))))))

(defun best (function list)
  (assert (consp list))
  (mvfoldl (lambda (item argmax max)
             (let ((value (funcall function item)))
               (if (> value max)
                   (values item value)
                   (values argmax max))))
           (cdr list)
           (car list)
           (funcall function (car list))))

(defun best-k (k list test &key key)
  (take k (sort (copy-list list) test :key key)))

(defun nbest-k (k list test &key key)
  (take k (sort list test :key key)))

(defun iterate (n x successor)
  (nlet rec ((n n) (x x) (acc nil))
    (if (zerop n)
        (nreverse acc)
        (rec (1- n)
             (funcall successor x)
             (cons x acc)))))

(defun prefixes (list)
  (mapcar #'reverse (suffixes (reverse list))))

(defun suffixes (list)
  (maplist #'identity list))

(defun prefixp (prefix sequence &key (test #'eql) (start 0) end)
  (let ((i (mismatch prefix sequence
                     :test test :start2 start :end2 end)))
    (or (not i)
        (= i (length prefix)))))

(defun suffixp (suffix sequence &key (test #'eql) (start 0) end)
  (let ((i (mismatch suffix sequence
                     :from-end t :test test :start2 start :end2 end)))
    (or (not i)
        (zerop i))))

(defun infixp (infix list &key (test #'eql) (start 0) end)
  (some (curry* #'prefixp infix % :test test :start start :end end)
        (suffixes list)))

(defun subsequencep (sub list &key (test #'eql) (start 0) end)
  (let ((list (subseq list start end)))
    (nlet rec ((sub sub) (list list))
      (cond ((null sub) t)
            ((null list) nil)
            ((funcall test (car sub) (car list))
             (rec (cdr sub) (cdr list)))
            (t
             (rec sub (cdr list)))))))

(defun splice-replace (new old list &key (test #'eql) count)
  (let ((old-len (length old))
        (new-rev (reverse new)))
    (labels ((rec (list count acc)
               (cond ((null list)
                      (nreverse acc))
                     ((zerop count)
                      (append (nreverse acc)
                              list))
                     (t
                      (let ((i (search old list :test test)))
                        (if i
                            (mvbind (left right) (split-at i list)
                              (rec (nthcdr old-len right)
                                   (1- count)
                                   (append new-rev
                                           (nreverse left)
                                           acc)))
                            (append (nreverse acc)
                                    list)))))))
      (rec list
           (or count (length list))
           nil))))

(defun non-empty-subsequences (list)
  (if (null list)
      nil
      (let ((item (car list)))
        (cons (list item)
              (foldr (lambda (sub acc)
                       (list* sub (cons item sub) acc))
                     nil
                     (non-empty-subsequences (cdr list)))))))
              
(defun subsequences (list)
  (cons nil (non-empty-subsequences list)))

;;; Graphs

(defun make-graph (n-vertices edges)
  (let ((graph (make-list-array n-vertices)))
    (mapc (dlambda ((u . v))
            (push v (aref graph u)))
          edges)
    graph))

(defun adjacent-cells (h w i j)
  (loop for di in '(-1 1 0 0)
        for dj in '(0 0 -1 1)
        for k = (+ i di)
        for l = (+ j dj)
        when (and (>= k 0) (< k h)
                  (>= l 0) (< l w))
          collect (cons k l)))

(defun graph-count-if (predicate graph start
                       &optional (seen (make-bit-array (length graph))))
  (labels ((dfs (vs acc)
             (if (null vs)
                 acc
                 (let ((u (car vs))
                       (vs (cdr vs)))
                   (dfs (append (filter (lambda (v)
                                          (when (zerop (aref seen v))
                                            (setf (aref seen v) 1)))
                                        (aref graph u))
                                vs)
                        (+ acc
                           (funcall (quantizer predicate) u)))))))
    (setf (aref seen start) 1)
    (dfs (list start) 0)))

;;; Accumulations

(eval-always
  (defun loop-for-clause (var &rest args)
    (ecase (length args)
      (1 `(loop for ,var in ,(car args)))
      (2 `(loop for ,var from ,(first args) below ,(second args)))
      (3 `(loop for ,var from ,(first args) below ,(second args) by ,(third args))))))

(defmacro define-accumulations ()
  `(progn
     ,@(loop for acc in '(sum maximize minimize thereis always)
             collect
             `(defmacro ,acc ((var &rest args) &body body)
                `(,@(apply #'loop-for-clause var args)
                  ,',acc ,(ensure-form body)))
             collect
             `(defmacro ,(mksym "~A*" acc) (var-and-args-specs &body body)
                (when (null var-and-args-specs)
                  (error "VAR-AND-ARGS-SPECS must not be null."))
                (labels ((rec (specs)
                           (if (null specs)
                               (ensure-form body)
                               `(,',acc ,(car specs)
                                        ,(rec (cdr specs))))))
                  (rec var-and-args-specs))))))

(define-accumulations)

(defmacro counting ((var &rest args) &body body)
  `(,@(apply #'loop-for-clause var args)
    count ,(ensure-form body)))

(defmacro counting* (var-and-args-specs &body body)
  (if (null var-and-args-specs)
      0
      (labels ((rec (specs)
                 (if (singletonp specs)
                     `(counting ,(car specs)
                        ,(ensure-form body))
                     `(sum ,(car specs)
                        ,(rec (cdr specs))))))
        (rec var-and-args-specs))))

(defmacro nconcing ((var &rest args) &body body)
  `(,@(apply #'loop-for-clause var args)
    nconc ,(ensure-form body)))

(defmacro nconcing* (var-and-args-specs &body body)
  (if (null var-and-args-specs)
      nil
      (labels ((rec (specs)
                 (if (singletonp specs)
                     `(nconcing ,(car specs)
                        ,(ensure-form body))
                     `(nconcing ,(car specs)
                        ,(rec (cdr specs))))))
        (rec var-and-args-specs))))

(defmacro collecting ((var &rest args) &body body)
  `(,@(apply #'loop-for-clause var args)
    collect ,(ensure-form body)))

(defmacro collecting* (var-and-args-specs &body body)
  (if (null var-and-args-specs)
      nil
      (labels ((rec (specs)
                 (if (singletonp specs)
                     `(collecting ,(car specs)
                        ,(ensure-form body))
                     `(nconcing ,(car specs)
                        ,(rec (cdr specs))))))
        (rec var-and-args-specs))))

(defmacro lcomp (var-and-args-specs condition &body body)
  (if (null var-and-args-specs)
      nil
      (labels ((rec (specs)
                 (if (singletonp specs)
                     `(,@(apply #'loop-for-clause (car specs))
                       for it = ,condition
                       when it
                       collect ,(ensure-form body))
                     `(,@(apply #'loop-for-clause (car specs))
                       nconc ,(rec (cdr specs))))))
        (rec var-and-args-specs))))

(defmacro define-array-accumulations ()
  `(progn
     ,@(loop for acc in '(sum collect count nconc append some every)
             for wrd in '(sum collect count nconc append thereis always)
             collect
             `(defun ,(mksym "ARRAY-~A" acc) (array &optional (key 'identity))
                (loop for i below (array-total-size array)
                      ,wrd (funcall key (row-major-aref array i)))))))

(define-array-accumulations)

;;; More utilities

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

(defsubst random-choice (list)
  (nth (random (length list)) list))

(defmacro with-timelimit ((seconds) &body body)
  (let ((gstart (gensym "START-TIME"))
        (gend (gensym "END-TIME")))
    `(let* ((,gstart (get-internal-real-time))
            (,gend (+ ,gstart
                      (* ,seconds
                         internal-time-units-per-second))))
       (labels ((time-up-p ()
                  (>= (get-internal-real-time) ,gend)))
         ,@body))))

(defun print-matrix (matrix &key key (sep " ") (stream t))
  (dotimes (i (array-dimension matrix 0))
    (dotimes (j (array-dimension matrix 1))
      (when (plusp j) (princ sep))
      (princ (ensure-key key (aref matrix i j))
             stream))
    (fresh-line stream))
  (fresh-line stream))

(defun shuffle! (vector)
  #@(vector vector)
  (loop for i fixnum from (length vector) downto 2
        do (rotatef (aref vector (random i))
                    (aref vector (1- i)))))

(defsubst non-nils (&rest items)
  (delete nil items))

(defsubst monotonicp (x y z)
  (declare (fixnum x y z)
           (optimize speed (safety 1)))
  (or (< x y z) (> x y z)))

(define-modify-macro minf (&rest more-numbers) min)

(define-modify-macro maxf (&rest more-numbers) max)

;;; Core

(defvar *point-id* -1)
(defvar *n*)
(defvar *m*)
(defvar *center*)
(defvar *timelimit*)
(defvar *randomness*)
(defvar *initial-cands*)
(defvar *threshold-ratio*)
(defvar *k-threshold* +inf+)
(defvar *r-min*)
(defvar *r-max*)
(defvar *c-min*)
(defvar *c-max*)
(defvar *k-threshold-ratio*)
(defvar *start-time*)

(eval-always
  (defconstant +dirs+          '(n ne e se s sw w nw))
  (defconstant +opposite-dirs+ '(s sw w nw n ne e se))
  (defconstant +dir-alist+
    (mapcar #'cons +dirs+ +opposite-dirs+))

  (defun opposite (dir)
    (cdr (assoc dir +dir-alist+)))

  (defstruct (point (:constructor point
                        (row col &key (id (incf *point-id*))
                                   deletable)))
    (id         -1  :type fixnum)
    (row        0   :type uint8)
    (col        0   :type uint8)
    (n-adj      nil :type (or null point))
    (ne-adj     nil :type (or null point))
    (e-adj      nil :type (or null point))
    (se-adj     nil :type (or null point))
    (s-adj      nil :type (or null point))
    (sw-adj     nil :type (or null point))
    (w-adj      nil :type (or null point))
    (nw-adj     nil :type (or null point))
    (n-connect  nil :type (or null point))
    (ne-connect nil :type (or null point))
    (e-connect  nil :type (or null point))
    (se-connect nil :type (or null point))
    (s-connect  nil :type (or null point))
    (sw-connect nil :type (or null point))
    (w-connect  nil :type (or null point))
    (nw-connect nil :type (or null point))
    (deletable  nil :type boolean))
  ) ; eval-always

(defun point-repr (point)
  (if (null point)
      ""
      (with-output-to-string (out)
        (print-unreadable-object (point out)
          (format out ":ID ~D :R ~D :C ~D"
                  (point-id point)
                  (point-row point)
                  (point-col point))))))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t)
    (apply #'format stream
           ":ID ~D :R ~A :C ~A :N ~A :NE ~A :E ~A :SE ~A :S ~A :SW ~A :W ~A :NW ~A"
           (point-id object) (point-row object) (point-col object)
           (mapcar (lambda (dir)
                     (point-repr (funcall (mksym "POINT-~A-CONNECT" dir) object)))
                   +dirs+))))
            
(deftype cell ()
  '(or null point))

(eval-always
  (defstruct (state (:constructor state
                        (grid points)))
    (grid   #() :type (simple-array cell))
    (points #() :type (vector point))))

(defmethod print-object ((object state) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":GRID~%")
    (print-matrix (state-grid object)
                  :key (quantizer #'point-p)
                  :stream stream)))

(defsubst make-grid (dimensions)
  (make-array dimensions :element-type 'cell :initial-element nil))

;;; Predicates

(defsubst filledp (grid r c)
  #@((simple-array cell) grid)
  (aref grid r c))

(defsubst blankp (grid r c)
  #@((simple-array cell) grid)
  (null (aref grid r c)))

(defsubst grid-in-bounds-p (r c)
  #@(fixnum r c *n*)
  (and (< -1 r *n*)
       (< -1 c *n*)))

(defsubst deletablep (point)
  #@(point point)
  (deletable point))

;;; Points

(defsubst ldiag-row-start (row col)
  (let ((const (+ row col)))
    (max *r-min* (- const *c-max*))))

(defsubst ldiag-row-end (row col)
  (let ((const (+ row col)))
    (1+ (min *r-max* (- const *c-min*)))))

(defsubst rdiag-row-start (row col)
  (let ((const (- row col)))
    (max *r-min* (+ const *c-min*))))

(defsubst rdiag-row-end (row col)
  (let ((const (- row col)))
    (1+ (min *r-max* (+ const *c-max*)))))

(defun points (grid)
  #@((simple-array cell) grid)
  (lcomp ((r 0 *n*) (c 0 *n*))
      (filledp grid r c)
    it))

(defsubst adjacent-n-point (grid row col)
  #@((simple-array cell) grid)
  (find-if*-range (curry* #'filledp grid % col)
                  (1+ row) (1+ *r-max*)))

(defsubst adjacent-ne-point (grid row col)
  #@((simple-array cell) grid)
  (let ((const (- row col)))
    (flet ((filledp (r)
             (filledp grid r (- r const))))
      (find-if*-range #'filledp
                      (1+ row) (rdiag-row-end row col)))))

(defsubst adjacent-e-point (grid row col)
  #@((simple-array cell) grid)
  (find-if*-range (curry* #'filledp grid row %)
                  (1+ col) (1+ *c-max*)))

(defsubst adjacent-se-point (grid row col)
  #@((simple-array cell) grid)
  (let ((const (+ row col)))
    (flet ((filledp (r)
             (filledp grid r (- const r))))
      (find-if*-drange #'filledp
                       (ldiag-row-start row col) row))))

(defsubst adjacent-s-point (grid row col)
  #@((simple-array cell) grid)
  (find-if*-drange (curry* #'filledp grid % col)
                   *r-min* row))

(defsubst adjacent-sw-point (grid row col)
  #@((simple-array cell) grid)
  (let ((const (- row col)))
    (flet ((filledp (r)
             (filledp grid r (- r const))))
      (find-if*-drange #'filledp
                       (rdiag-row-start row col) row))))

(defsubst adjacent-w-point (grid row col)
  #@((simple-array cell) grid)
  (find-if*-drange (curry* #'filledp grid row %)
                   *c-min* col))

(defsubst adjacent-nw-point (grid row col)
  #@((simple-array cell) grid)
  (let ((const (+ row col)))
    (flet ((filledp (r)
             (filledp grid r (- const r))))
      (find-if*-range #'filledp
                      (1+ row) (ldiag-row-end row col)))))

(defun dir (row col r c)
  #@(fixnum row col r c)
  (labels ((err ()
             (error "Row-col pairs must not be identical.")))
    (cond ((= row r)
           (cond ((< col c) 'e)
                 ((< c col) 'w)
                 (t (err))))
          ((= col c)
           (cond ((< row r) 'n)
                 ((< r row) 's)
                 (t (err))))
          ((= (+ row col) (+ r c))
           (cond ((< row r) 'nw)
                 ((< r row) 'se)
                 (t (err))))
          ((= (- row col) (- r c))
           (cond ((< row r) 'ne)
                 ((< r row) 'sw)
                 (t (err))))
          (t (error "Row-col pairs are not adjacent.")))))

;; (defun dir (row col r c)
;;   #@(fixnum row col r c)
;;   (let ((dr (signum (- r row)))
;;         (dc (signum (- c col))))
;;     #@(fixnum dr dc)
;;     (switch (dr :test '=)
;;       (-1 (switch (dc :test '=)
;;             (-1 'sw)
;;             (0  's)
;;             (1  'se)))
;;       (0  (switch (dc :test '=)
;;             (-1 'w)
;;             (0  (error "Row-col pairs must not be identical."))
;;             (1  'e)))
;;       (1  (switch (dc :test '=)
;;             (-1 'nw)
;;             (0  'n)
;;             (1  'ne))))))

(eval-always
  (defsubst adj-accessor (dir)
    (ecase dir
      (n  'point-n-adj)
      (ne 'point-ne-adj)
      (e  'point-e-adj)
      (se 'point-se-adj)
      (s  'point-s-adj)
      (sw 'point-sw-adj)
      (w  'point-w-adj)
      (nw 'point-nw-adj))))

(eval-always
 (defsubst connect-accessor (dir)
   (ecase dir
     (n  'point-n-connect)
     (ne 'point-ne-connect)
     (e  'point-e-connect)
     (se 'point-se-connect)
     (s  'point-s-connect)
     (sw 'point-sw-connect)
     (w  'point-w-connect)
     (nw 'point-nw-connect))))

(defmacro %all-adjacent-points-aux (point)
  (labels ((p (dir)
             `(,(adj-accessor dir) ,point)))
    `(non-nils ,@(mapcar #'p +dirs+))))

(defun all-adjacent-points (point)
  #@(point point)
  (%all-adjacent-points-aux point))

(defmacro %connectedp-aux (p1 p2)
  (sb-ext::once-only ((p1 p1) (p2 p2))
    (labels ((aux (dir)
               `(eq ,p1 (,(connect-accessor dir) ,p2))))
      `(or
        ,@(mapcar #'aux +dirs+)))))

(defsubst connectedp (p1 p2)
  (%connectedp-aux p1 p2))

(defsubst connectedp2 (p1 p2 dir)
  #@(point p1 p2)
  (eq (funcall (connect-accessor dir) p1)
      p2))

(defun betweenp (p1 p2 r c)
  #@(point p1 p2)
  (with-accessors ((r1 point-row) (c1 point-col)) p1
    (with-accessors ((r2 point-row) (c2 point-col)) p2
      #@(fixnum r c r1 c1 r2 c2)
      (and (not (and (= r1 r2 r) (= c1 c2 c)))
           (and (or (= r1 r2 r) (monotonicp r1 r2 r))
                (or (= c1 c2 c) (monotonicp c1 c2 c)))))))

(defun connectablep (point r c)
  (let* ((dir (dir (point-row point)
                   (point-col point)
                   r c))
         (adj (funcall (adj-accessor dir)
                       point)))
    (or (null adj)
        (not (or (connectedp2 point adj dir)
                 (betweenp point adj r c))))))

;;; Ops

(defun print-op (op)
  (dbind (p1 p2 p3 p4) op
    (with-accessors ((r1 point-row) (c1 point-col)) p1
      (with-accessors ((r2 point-row) (c2 point-col)) p2
        (with-accessors ((r3 point-row) (c3 point-col)) p3
          (with-accessors ((r4 point-row) (c4 point-col)) p4
            (format t "~D ~D ~D ~D ~D ~D ~D ~D"
                    r1 c1 r2 c2 r3 c3 r4 c4)))))))

(defun make-op-if-valid (grid r1 c1 p2 p3 p4)
  (when (and (grid-in-bounds-p r1 c1)
             (blankp grid r1 c1)
             (not (connectedp p2 p3))
             (not (connectedp p3 p4))
             (connectablep p2 r1 c1)
             (connectablep p4 r1 c1))
    (list (point r1 c1 :deletable t) p2 p3 p4)))
  
(defun make-axis-aligned-op-if-valid (grid point row-point col-point)
  #@(point point row-point col-point)
  (with-accessors ((c2 point-col)) row-point
    (with-accessors ((r4 point-row)) col-point
      (let ((r1 r4)
            (c1 c2))
        (make-op-if-valid grid r1 c1 row-point point col-point)))))

(defun make-diagonal-op-if-valid (grid point ldiag-point rdiag-point)
  #@(point point ldiag-point rdiag-point)
  (with-accessors ((r3 point-row) (c3 point-col)) point
    (with-accessors ((r2 point-row) (c2 point-col)) ldiag-point
      (with-accessors ((r4 point-row) (c4 point-col)) rdiag-point
        (let ((r1 (- r4 (- r3 r2)))
              (c1 (+ c4 (- c2 c3))))
          (make-op-if-valid grid r1 c1 ldiag-point point rdiag-point))))))

(defun valid-axis-aligned-ops (grid point)
  #@(point point)
  (with-accessors ((n point-n-adj) (e point-e-adj)
                   (s point-s-adj) (w point-w-adj))
      point
    (lcomp ((rp (non-nils e w))
            (cp (non-nils n s)))
        (make-axis-aligned-op-if-valid grid point rp cp)
      it)))

(defun valid-diagonal-ops (grid point)
  #@(point point)
  (with-accessors ((ne point-ne-adj) (se point-se-adj)
                   (sw point-sw-adj) (nw point-nw-adj))
      point
    (lcomp ((ldp (non-nils se nw))
            (rdp (non-nils ne sw)))
        (make-diagonal-op-if-valid grid point ldp rdp)
      it)))

(defun valid-ops (grid point)
  (nconc (valid-axis-aligned-ops grid point)
         (valid-diagonal-ops grid point)))

(defun random-valid-op (state &optional (finder #'valid-ops) (r *randomness*))
  #@(state state)
  (labels ((ret (acc)
             (when acc
               (best #'d acc))))
    (with-accessors ((grid state-grid)
                     (points state-points))
        state
      (shuffle! points)
      (let ((len (length points)))
        (nlet rec ((i 0) (count 0) (acc nil))
          #@(fixnum len i count r)
          (if (or (>= i len)
                  (>= count r))
              (ret acc)
              (mvbind (ops j) (find-if*-range (lambda (pos)
                                                (funcall finder grid (aref points pos)))
                                              i len)
                #@((or null fixnum) j)
                (if (null j)
                    (ret acc)
                    (rec (1+ j)
                         (+ count (length ops))
                         (nconc ops acc))))))))))

;; (defsubst random-valid-axis-aligned-op (state &optional (r *randomness*))
;;   (random-valid-op state #'valid-axis-aligned-ops r))

;; (defsubst random-valid-diagonal-op (state &optional (r *randomness*))
;;   (random-valid-op state #'valid-diagonal-ops r))

;; (defun random-valid-op2 (state)
;;   #@(state state)
;;   (with-accessors ((grid state-grid)
;;                    (points state-points))
;;       state
;;     (shuffle! points)
;;     (find-if*-range (lambda (pos)
;;                       (valid-ops grid (aref points pos)))
;;                     0 (length points))))

(defun best-valid-op (grid point)
  (aand (valid-ops grid point)
        (best #'d it)))

;;; State update

(defsubst dir-from (p1 p2)
  #@(point p1 p2)
  (with-accessors ((row point-row) (col point-col)) p1
    (with-accessors ((r point-row) (c point-col)) p2
      (dir row col r c))))

(defsubst dir-connect! (p1 p2 dir)
  (funcall (fdefinition `(setf ,(connect-accessor dir)))
           p2 p1)
  (funcall (fdefinition `(setf ,(connect-accessor (opposite dir))))
           p1 p2))

(define-compiler-macro dir-connect! (&whole whole p1 p2 dir)
  (if (and (consp dir)
           (eq (first dir) 'quote)
           (symbolp (second dir)))
      (let ((dirsym (mksym (symbol-name (second dir)))))
        (sb-ext::once-only ((p1 p1) (p2 p2))
          `(setf (,(connect-accessor dirsym) ,p1) ,p2
                 (,(connect-accessor (opposite dirsym)) ,p2) ,p1)))
      whole))

(defsubst dir-disconnect! (p1 p2 dir)
  (funcall (fdefinition `(setf ,(connect-accessor dir)))
           nil p1)
  (funcall (fdefinition `(setf ,(connect-accessor (opposite dir))))
           nil p2))

(defsubst connect! (p1 p2)
  (dir-connect! p1 p2 (dir-from p1 p2)))

(defsubst disconnect! (p1 p2)
  (dir-disconnect! p1 p2 (dir-from p1 p2)))

(defun connect-rect! (op)
  (dbind (p1 p2 p3 p4) op
    (connect! p1 p2)
    (connect! p2 p3)
    (connect! p3 p4)
    (connect! p4 p1)))

(defmacro %update-adjacent-points-slots!-aux ()
  (labels ((aux (dir)
             `(awhen (,(mksym "ADJACENT-~A-POINT" dir) grid r c)
                (let ((old (,(mksym "POINT-~A-ADJ" (opposite dir)) it)))
                  (setf (,(mksym "POINT-~A-ADJ" dir) point) it
                        (,(mksym "POINT-~A-ADJ" (opposite dir)) it) point)
                  (when (and old (connectedp it old))
                    (dir-connect! point it ',dir)
                    (dir-connect! point old ',(opposite dir)))))))
    `(progn
       ,@(mapcar #'aux +dirs+))))

(defun update-adjacent-points-slots! (grid point)
  #@(point point)
  (with-accessors ((r point-row) (c point-col)) point
    (%update-adjacent-points-slots!-aux)))

(defun operate! (state op)
  #@(state state)
  (let* ((p1 (first op))
         (r1 (point-row p1))
         (c1 (point-col p1)))
    (with-accessors ((grid state-grid)
                     (points state-points))
        state
      (update-adjacent-points-slots! grid p1)
      (connect-rect! op)
      (setf (aref grid r1 c1) p1)
      (vector-push-extend p1 points))
    (update-bounds! r1 c1))
  state)  

(defun delete-connects! (grid start-point goal-point)
  (let ((adj-accessor (adj-accessor (dir-from start-point goal-point))))
    (nlet rec ((p start-point))
      (unless (eq p goal-point)
        (let ((adj (funcall adj-accessor p)))
          (assert (point-p adj))
          (disconnect! p adj)
          (rec adj))))))

(defun undo-operate! (state op)
  #@(state state)
  (dbind (p1 p2 p3 p4) op
    (with-accessors ((id1 point-id)
                     (r1 point-row)
                     (c1 point-col))
        p1
      (with-accessors ((grid state-grid)
                       (points state-points))
          state
        (delete-connects! grid p1 p2)
        (delete-connects! grid p2 p3)
        (delete-connects! grid p3 p4)
        (delete-connects! grid p4 p1)
        (setf (aref grid r1 c1) nil)
        (mapc (curry #'update-adjacent-points-slots! grid)
              (all-adjacent-points p1))
        (setf points (delete p1 points :test #'eq))
        (let ((deleted-points (mapcan (curry #'undo-operate! state)
                                      (filter (conjoin #'deletablep
                                                       (compose #>id1 #'point-id))
                                              (cdr op)))))
          (values state
                  (cons p1 deleted-points)))))))

(defun all-points-greedy-operate! (state)
  (with-accessors ((grid state-grid)
                   (points state-points))
      state
    (let ((m (length points)))
      (nlet rec ((state state) (i 0) (ops nil))
        (if (= i m)
            (values state ops)
            (let ((op (best-valid-op grid (aref points i))))
              (if (null op)
                  (rec state (1+ i) ops)
                  (rec (operate! state op)
                       (1+ i)
                       (cons op ops)))))))))

;;; Main

(defun init-state (xys)
  (let* ((ds (list *n* *n*))
         (grid (make-grid ds))
         (points (make-adj-array *m*)))
    #@((simple-array cell) grid)
    (mapc (dlambda ((x . y))
            (setf (aref grid x y) (point x y)))
          xys)
    (mapc (rcurry #'vector-push points)
          (points grid))
    (map nil (curry #'update-adjacent-points-slots! grid)
         points)
    (state grid points)))

(defsubst d-from-center (r c)
  (+ (^2 (- r *center*))
     (^2 (- c *center*))))

(defun d (op)
  (let* ((p1 (first op))
         (r1 (point-row p1))
         (c1 (point-col p1)))
    (d-from-center r1 c1)))

(defun score (ops)
  (reduce #'+ ops :initial-value 0 :key #'d))

(defun generate-cand (xys best-score best-k)
  (declare (ignore best-score best-k))
  (nlet rec ((state (init-state xys)) (score 0) (ops nil))
    (mvbind (op d) (random-valid-op state)
      (if (null op)
          (values score
                  (length ops)
                  (nreverse ops)
                  state)
          (rec (operate! state op)
               (+ score d)
               (cons op ops))))))

(defun erapsed-seconds ()
  (/ (- (get-internal-real-time)
        *start-time*)
     internal-time-units-per-second))

(defun score-threshold-ratio ()
  (* 1/2 (+ 1 (/ (erapsed-seconds) *timelimit*))))

(defun abortp (score k best-score best-k k-threshold)
  (and (>= k k-threshold)
       (< (* score best-k)
          (* best-score k (score-threshold-ratio)))))

(let ((count 0)
      (abort-count 0))
  (defun generate-cand-kernighan-lin (xys best-score best-k)
    (labels ((ret (score k ops abortedp)
               ;; (incf count)
               ;; (when abortedp (incf abort-count))
               ;; (when (zerop (mod count 100))
               ;;   (dbg count abort-count))
               (values score k (nreverse ops))))
      (let ((k-thrs (* *k-threshold-ratio* best-k)))
        (nlet rec ((state (init-state xys))
                   (score 0)
                   (k 0)
                   (ops nil))
          (shuffle! (state-points state))
          (mvbind (s* ops*) (all-points-greedy-operate! state)
            (if (null ops*)
                (ret score k ops nil)
                (let ((score* (+ score (reduce #'+ ops* :key #'d)))
                      (k* (+ k (length ops*))))
                  (if (abortp score* k* best-score best-k k-thrs)
                      (ret score* k* nil t)
                      (rec s* score* k*
                           (nconc ops* ops)))))))))))                   

(defun generate-cand-test-undo-operate! (xys best-score best-k)
  (mvbind (score k ops state) (generate-cand xys best-score best-k)
    (let* ((op (car ops))
           (deleted (nth-value 1 (undo-operate! state op)))
           (ops* (delete-if (lambda (op)
                              (member (car op) deleted :test #'eq))
                            ops)))
      ;; (dbg 'delete (car op))
      ;; (dbg 'deleted deleted)
      (values (score ops*)
              (length ops*)
              ops*))))
    
(defun solve (xys cand-generator)
  (let ((count 0))
    (mvbind (score k ops) (funcall cand-generator xys 0 +inf+)
      (with-timelimit (*timelimit*)
        (nlet rec ((score score)
                   (k k)
                   (ops ops))
          (incf count)
          (if (time-up-p)
              (progn
                (dbg count)
                (values k ops))
              (mvbind (score* k* ops*) (funcall cand-generator xys score k)
                (if (> score* score)
                    (rec score* k* ops*)
                    (rec score k ops)))))))))
   
(defun update-bounds! (r c)
  (minf *r-min* r)
  (maxf *r-max* r)
  (minf *c-min* c)
  (maxf *c-max* c))

(defun set-vars! (n m xys randomness k-threshold-ratio)
  (setf *n* n
        *m* m
        *center* (ash n -1)
        *timelimit* 4.5
        *randomness* randomness
        *r-min* n
        *r-max* 0
        *c-min* n
        *c-max* 0
        *k-threshold-ratio* k-threshold-ratio
        *start-time* (get-internal-real-time))
  (mapc (cons-applier #'update-bounds!) xys))

(defun main (&optional (stream *standard-input*)
               (randomness 4)
               (k-threshold-ratio 1/2))
  (let ((*standard-input* stream))
    (readlet (n m)
      (let ((xys (read-conses m)))
        (set-vars! n m xys randomness k-threshold-ratio)
        (mvbind (k ops) (solve xys #'generate-cand-test-undo-operate!)
          (bulk-stdout
            (println k)
            (mapc (lambda (op)
                    (print-op op) (fresh-line))
                  ops)))))))

#-swank (main)

;;; Benchmark

;; (require :uiop)

;; (defun testcase-filename (test-dir test-id)
;;   (format nil "~A/sample-~4,'0D.in" test-dir test-id))

;; (defun testcase-score (filename randomness k-threshold-ratio)
;;   (with-open-stream (in (-> (uiop:launch-program
;;                              (list "cat" filename)
;;                              :output :stream)
;;                             uiop:process-info-output))
;;     (let ((*standard-input* in))
;;       (readlet (n m)
;;         (let ((xys (read-conses n)))
;;           (set-vars! n m xys randomness k-threshold-ratio)
;;           (mvbind (k ops) (solve xys #'generate-cand-kernighan-lin)
;;             (reduce #'+ ops :key #'d)))))))

;; (defun total-score (test-dir randomness k-threshold-ratio)
;;   (reduce #'+ (range 10)
;;           :key (lambda (id)
;;                  (testcase-score (testcase-filename test-dir id)
;;                                  randomness
;;                                  k-threshold-ratio))))

;; (defconstant +rs+ '(1))
;; (defconstant +ths+ '(1/3 1/2 2/3))

;; (defun benchmark ()
;;   (let ((dir (sb-ext:posix-getenv "dir")))
;;     (println dir)
;;     (dolist (r +rs+)
;;       (dolist (th +ths+)
;;         (dbg 'randomness r 'k-threshold-ratio th)
;;         (dbg 'total-score (total-score dir r th))
;;         (terpri)))))

;; (trace testcase-score)
;; (benchmark)
