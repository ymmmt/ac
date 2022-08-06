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

(defconstant +mod+ 1000000007)

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

;; BEGIN_USE_PACKAGE
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/read-fixnum :cl-user))

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

(defmacro define-accumulations ()
  `(progn
     ,@(loop for acc in '(sum maximize minimize thereis always)
             for fn in '(+ max min or and)
             collect
             `(defmacro ,acc ((var &rest args) &body body)
                (ecase (length args)
                  (1 `(reduce ',',fn ,(car args)
                              :key (lambda (,var) ,@body)))
                  (2 `(loop for ,var from ,(first args) below ,(second args)
                            ,',acc (progn ,@body)))))
             collect
             `(defmacro ,(intern (format nil "~A*" acc)) (var-and-args-specs &body body)
                (labels ((rec (specs)
                           (if (null specs)
                               `(progn ,@body)
                               `(,',acc ,(car specs)
                                        ,(rec (cdr specs))))))
                  (rec var-and-args-specs))))))

(define-accumulations)

(defun scanl (function initial-value list)
  (labels ((rec (list acc)
             (if (null list)
                 (nreverse acc)
                 (rec (cdr list) (cons (funcall function (car acc) (car list))
                                       acc)))))
    (rec list (list initial-value))))

(defun scanl1 (function list)
  (if (null list)
      (error "scanl1: empty list")
      (scanl function (car list) (cdr list))))

(defun scanr (function initial-value list)
  (labels ((rec (list acc)
             (if (null list)
                 acc
                 (rec (cdr list)
                      (cons (funcall function (car list) (car acc))
                            acc)))))
    (rec (reverse list) (list initial-value))))

(defun scanr1 (function list)
  (if (null list)
      (error "scanr1: empty list")
      (scanr function (car (last list)) (nbutlast list))))

(defun coerce-vector (object)
  (coerce object 'vector))

(defun pad (vector)
  (concatenate 'vector #(0) vector))

(defun solve (n l r a)
  (let* ((sum (reduce #'+ a))
         (l-a (mapcar (lambda (ai) (- l ai)) a))
         (l-a-cumsum (scanl1 #'+ l-a))
         (l-a-cum-min (pad (coerce-vector (scanl1 #'min l-a-cumsum))))
         (r-a (mapcar (lambda (ai) (- r ai)) a))
         (r-a-cumsum-from-end (scanr1 #'+ r-a))
         (r-a-cum-min-from-end (pad (coerce-vector (scanr #'min 0 r-a-cumsum-from-end)))))
    (minimize (x 0 (1+ n))
      (+ sum
         (aref l-a-cum-min x)
         (aref r-a-cum-min-from-end (1+ x))))))
           
(defun main ()
  (readlet (n l r)
    (let ((a (readlist)))
      (println (solve n l r a)))))

#-swank (main)
