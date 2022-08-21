(in-package :cl-user)

(unless (member :child-sbcl *features*)
  (quit
   :recklessly-p t
   :unix-status
   (process-exit-code
    (run-program *runtime-pathname*
                 `("--control-stack-size" "256MB"
                   "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                   "--eval" "(push :child-sbcl *features*)"
                   "--script" ,(namestring *load-pathname*))
                 :output t :error t :input t))))

(macrolet ((def (b)
             `(progn (deftype ,(intern (format nil "UINT~A" b)) () '(unsigned-byte ,b))
                     (deftype ,(intern (format nil "INT~A" b)) () '(signed-byte ,b))))
           (define-int-types (&rest bits) `(progn ,@(mapcar (lambda (b) `(def ,b)) bits))))
  (define-int-types 2 4 7 8 15 16 31 32 62 63 64))

(defconstant +mod+ 998244353)

(defmacro dbg (&rest forms)
  (declare (ignorable forms))
  (if (= (length forms) 1)
      `(format t "~A => ~A~%" ',(car forms) ,(car forms))
      `(format t "~A => ~A~%" ',forms `(,,@forms))))

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":count ~A :test ~A"
            (hash-table-count object)
            (hash-table-test object))
    (loop for k being the hash-key of object
          do (format stream "~%[~A] ~A" k (gethash k object)))))

(declaim (inline println printlns))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format*
          (if (typep obj 'double-float) 'double-float *read-default-float-format*)))
    (prog1 (princ obj stream) (terpri stream))))

(defun printlns (lst)
  (format t "~{~A~%~}" lst))

;; BEGIN_INSERTED_CONTENTS
(defpackage :cp/read-fixnum
  (:use :cl)
  (:export #:read-fixnum))
(in-package :cp/read-fixnum)

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

(defpackage :cp/mod-power
  (:use :cl)
  (:export #:mod-power))
(in-package :cp/mod-power)

(declaim (inline mod-power))
(defun mod-power (base power modulus)
  "Returns BASE^POWER mod MODULUS. Note: 0^0 = 1.

BASE := integer
POWER, MODULUS := non-negative fixnum"
  (declare ((integer 0 #.most-positive-fixnum) modulus power)
           (integer base))
  (let ((base (mod base modulus))
        (res (mod 1 modulus)))
    (declare ((integer 0 #.most-positive-fixnum) base res))
    (loop while (> power 0)
          when (oddp power)
          do (setq res (mod (* res base) modulus))
          do (setq base (mod (* base base) modulus)
                   power (ash power -1)))
    res))

;; BEGIN_USE_PACKAGE
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/read-fixnum :cl-user))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/mod-power :cl-user))

;;
;; Utility
;;

(in-package :cl-user)

(defmacro readlet (vars &body body)
  `(let ,(mapcar (lambda (v)
                   `(,v (read-fixnum)))
          vars)
     ,@body))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
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
           (multiple-value-bind (,val ,found-p)
               (gethash ,k ,table)
             (if ,found-p ,val
                 (setf (gethash ,k ,table)
                       (destructuring-bind ,lambda-list ,args
                         ,@body)))))))))

(defmacro with-memo ((name lambda-list &body definition) &body body)
  (with-gensyms (table args val found-p)
    `(let ((,table (make-hash-table :test 'equal)))
       (labels ((,name (&rest ,args)
                  (multiple-value-bind (,val ,found-p)
                      (gethash ,args ,table)
                    (if ,found-p ,val
                        (setf (gethash ,args ,table)
                              (destructuring-bind ,lambda-list ,args
                                ,@definition))))))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
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
  (let ((gargs (gensym "ARGS")))
    `(lambda (&rest ,gargs)
       (destructuring-bind ,lambda-list ,gargs
         ,@body))))

(defmacro collect (n expr)
  `(loop repeat ,n
         collect ,expr))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro mod-acc (fn initial-value divisor (var &rest args) &body body)
  (ecase (length args)
    (1 (let ((gd (gensym "DIVISOR"))
             (gacc (gensym "ACC")))
         `(let ((,gd ,divisor))
            (reduce (lambda (,gacc ,var)
                      (mod (,fn ,gacc (progn ,@body)) ,gd))
                    ,(car args)
                    :initial-value ,initial-value))))
    (2 (let ((gd (gensym "DIVISOR"))
             (gans (gensym "ANSWER")))
         `(let ((,gd ,divisor)
                (,gans ,initial-value))
            (loop for ,var from ,(first args) below ,(second args)
                  do (setf ,gans
                           (mod (,fn ,gans
                                     (progn ,@body))
                                ,gd)))
            ,gans)))))

(defun range (&rest args)
  (ecase (length args)
    (1 (let ((end (car args)))
         (loop for i below end
               collect i)))
    (2 (destructuring-bind (start end) args
         (loop for i from start below end
               collect i)))
    (3 (destructuring-bind (start end step) args
         (loop for i from start below end by step
               collect i)))))

;;;
;;; Body
;;;
                  
(defmemo (fact) (n)
  (if (<= n 1)
      0
      (* n (fact (1- n)))))

(defun md (x)
  (mod x +mod+))

(defun solve-linear-eqs (a b c d p q)
  "Solve 
ax + by = p
cx + dy = q"
  (let ((det (- (* a d) (* b c))))
    (if (= det 0)
        (values nil nil)
        (values (/ (- (* d p) (* b q))
                   det)
                (/ (- (* a q) (* c p))
                   det)))))

(defun solve (n m a b c d e f xys)
  (labels ((n-paths-to-xy (x y k)
             (multiple-value-bind (p q)
                 (solve-linear-eqs (- a e)
                                   (- c e)
                                   (- b f)
                                   (- d f)
                                   (- x (* e k))
                                   (- y (* f k)))
               (when (and (integerp p)
                          (integerp q)
                          (>= p 0)
                          (>= q 0)
                          (<= (+ p q) k))
                 (list p q (- k p q))))))
    (let ((base (mod-power 3 m +mod+))
          (ks (range 1 (1+ n))))
      (nlet rec ((xys xys) (acc base))
        (if (null xys)
            acc
            (destructuring-bind (x . y) (car xys)
              (rec (cdr xys)
                   (md (- acc
                          (mod-acc + 0 +mod+ (k ks)
                            (aif (n-paths-to-xy x y k)
                                 (destructuring-bind (p q r) it
                                   (* (/ (fact k)
                                         (fact p)
                                         (fact q)
                                         (fact r))
                                      (mod-power 3 (- n k) +mod+)))
                                 0)))))))))))
                
(defun main ()
  (readlet (n m a b c d e f)
    (let ((xys (collect m
                 (readlet (x y)
                   (cons x y)))))
      (println (solve n m a b c d e f xys)))))

#-swank (main)
