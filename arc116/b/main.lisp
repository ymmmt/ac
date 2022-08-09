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
(defpackage :cp/mod-inverse
  (:use :cl)
  #+sbcl (:import-from #:sb-c #:defoptimizer #:lvar-type #:integer-type-numeric-bounds
                       #:derive-type #:flushable #:foldable)
  #+sbcl (:import-from :sb-kernel #:specifier-type)
  (:export #:mod-inverse))
(in-package :cp/mod-inverse)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown %mod-inverse ((integer 0) (integer 1)) (integer 0)
      (flushable foldable)
    :overwrite-fndb-silently t)
  (sb-c:defknown mod-inverse (integer (integer 1)) (integer 0)
      (flushable foldable)
    :overwrite-fndb-silently t)
  (defun derive-mod (modulus)
    (let ((high (nth-value 1 (integer-type-numeric-bounds (lvar-type modulus)))))
      (specifier-type (if (integerp high)
                          `(integer 0 (,high))
                          `(integer 0)))))
  (defoptimizer (%mod-inverse derive-type) ((integer modulus))
    (declare (ignore integer))
    (derive-mod modulus))
  (defoptimizer (mod-inverse derive-type) ((integer modulus))
    (declare (ignore integer))
    (derive-mod modulus)))

(defun %mod-inverse (integer modulus)
  (declare (optimize (speed 3) (safety 0))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (macrolet ((frob (stype)
               `(let ((a integer)
                      (b modulus)
                      (u 1)
                      (v 0))
                  (declare (,stype a b u v))
                  (loop until (zerop b)
                        for quot = (floor a b)
                        do (decf a (the ,stype (* quot b)))
                           (rotatef a b)
                           (decf u (the ,stype (* quot v)))
                           (rotatef u v))
                  (if (< u 0)
                      (+ u modulus)
                      u))))
    (typecase modulus
      ((unsigned-byte 31) (frob (signed-byte 32)))
      ((unsigned-byte 62) (frob (signed-byte 63)))
      (otherwise (frob integer)))))

(declaim (inline mod-inverse))
(defun mod-inverse (integer modulus)
  "Solves ax = 1 mod m. Signals DIVISION-BY-ZERO when INTEGER and MODULUS are
not coprime."
  (let* ((integer (mod integer modulus))
         (result (%mod-inverse integer modulus)))
    (unless (or (= 1 (mod (* integer result) modulus)) (= 1 modulus))
      (error 'division-by-zero
             :operands (list integer modulus)
             :operation 'mod-inverse))
    result))

;; BEGIN_USE_PACKAGE
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/read-fixnum :cl-user))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/mod-power :cl-user))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/mod-inverse :cl-user))

;;
;; Utility
;;

(in-package :cl-user)

(defmacro readlet (vars &body body)
  `(let ,(mapcar (lambda (v)
                   `(,v (read-fixnum)))
          vars)
     ,@body))

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

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

;; depends on cp/mod-inverse
(defmacro define-mod-operations ()
  `(progn
     ,@(mapcar (lambda (operator)
                 `(defun ,(intern (format nil "MOD~A" (string operator)))
                      (x y &optional (mod +mod+))
                    (let ((a (numerator x))
                          (b (denominator x))
                          (c (numerator y))
                          (d (denominator y)))
                      (mod (,operator (* a (mod-inverse b +mod+))
                                      (* c (mod-inverse d +mod+)))
                           mod))))
               '(+ -))))

(define-mod-operations)

(defun scanl (function initial-value list)
  (labels ((rec (list acc)
             (if (null list)
                 (nreverse acc)
                 (rec (cdr list) (cons (funcall function (car acc) (car list))
                                       acc)))))
    (rec list (list initial-value))))

(defun zip (&rest lists)
  (when lists
    (apply #'zip-with #'list lists)))

(defun zip-with (fn &rest lists)
  (when lists
    (labels ((rec (lists acc)
               (if (some #'null lists)
                   (nreverse acc)
                   (rec (mapcar #'cdr lists)
                        (cons (apply fn (mapcar #'car lists))
                              acc)))))
      (rec lists nil))))

(defun iterate (n x successor)
  (nlet rec ((n n) (x x) (acc nil))
    (if (zerop n)
        (nreverse acc)
        (rec (1- n)
             (funcall successor x)
             (cons x acc)))))

(defmacro -> (x &rest forms)
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (member '% form)
                               `(funcall (lambda (%) ,form) ,x)
                               `(,(first form) ,x ,@(rest form)))
                           `(,form ,x))))
        `(-> ,threaded ,@(rest forms)))
      x))

(defun md (x)
  (mod x +mod+))

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

;;;
;;; Body
;;;

(defun solve (n as)
  (let* ((as (sort as #'<))
         (cumsums (-> (zip-with #'*
                                (iterate n 2 (lambda (x) (md (ash x 1))))
                                as)
                      (scanl #'mod+ 0 %)
                      (coerce 'vector))))
    (mod+ (mod-acc + 0 +mod+ (a as)
            (expt a 2))
          (nlet rec ((as as) (i 1) (acc 0))
            (if (= i n)
                acc
                (rec (cdr as)
                     (1+ i)
                     (mod+ acc
                           (/ (* (car as)
                                 (mod- (aref cumsums n)
                                       (aref cumsums i)))
                              (mod-power 2 (1+ i) +mod+)))))))))

(defun main ()
  (readlet (n)
    (let ((as (readlist)))
      (println (solve n as)))))

#-swank (main)
