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

;;;
;;; Body
;;;

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

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

(defun counter (sequence &key (test 'eql) (key 'identity))
  (let ((counter (make-hash-table :size (length sequence) :test test)))
    (map nil (lambda (elem)
               (let ((k (funcall key elem)))
                 (setf (gethash k counter)
                       (1+ (gethash k counter 0)))))
         sequence)
    counter))

(defun ht-values (hash-table)
  (loop for v being the hash-value of hash-table
        collect v))

(defun mapper (hash-table &optional default)
  (lambda (key)
    (gethash key hash-table default)))

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

(defun solve (n ds)
  (declare (ignore n))
  (let ((max (apply #'max ds))
        (counts (counter ds)))
    (if (and (zerop (car ds))
             (= 1 (gethash 0 counts))
             (every (mapper counts)
                    (range (1+ max))))
        (mod-acc * 1 +mod+ (level (range 1 (1+ max)))
          (mod-power (gethash (1- level) counts)
                     (gethash level counts)
                     +mod+))
        0)))

(defun main ()
  (readlet (n)
    (let ((ds (readlist)))
      (println (solve n ds)))))

#-swank (main)