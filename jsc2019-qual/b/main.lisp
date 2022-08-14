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
;; https://github.com/ruricolist/serapeum/blob/master/definitions.lisp#L129
(defmacro defsubst (name params &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params
       ,@body)))

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

;; depends on range
(defun ranks (sequence &key (test '<) (rank-base 0))
  (let ((map (make-hash-table)))
    (map nil (lambda (item rank)
                 (unless (gethash item map)
                   (setf (gethash item map) rank)))
         (sort (copy-seq sequence) test)
         (range rank-base
                (+ rank-base (length sequence))))
    map))

(defun split-at (n list)
  (labels ((rec (n list left)
             (if (or (zerop n) (null list))
                 (values (nreverse left) list)
                 (rec (1- n) (cdr list) (cons (car list) left)))))
    (rec n list nil)))

(defun mapper (hash-table &optional default)
  (lambda (key)
    (gethash key hash-table default)))

(defun md (x)
  (mod x +mod+))

;;;
;;; Body
;;;

;; depends on split-at
;; https://kira000.hatenadiary.jp/entry/2019/02/23/053917
(defun inversion-number (list > &optional (n (length list)))
  (if (<= n 1)
      (values 0 list)
      (let ((l (ceiling n 2))
            (r (floor n 2)))
        (multiple-value-bind (left split) (right-at l list)
          (multiple-value-bind (acc1 m1) (inversion-number left > l)
            (multiple-value-bind (acc2 m2) (inversion-number right > r)
              (nlet rec ((m1 m1) (l1 l)
                         (m2 m2) (l2 r)
                         (merged nil)
                         (acc (+ acc1 acc2)))
                (cond ((or (null m1) (null m2))
                       (values acc
                               (append (nreverse merged) m1 m2)))
                      ((funcall > (car m1) (car m2))
                       ;; (every (curry% > % (car m2)) m1) == t
                       (rec m1 l1
                            (cdr m2) (1- l2)
                            (cons (car m2) merged)
                            (+ acc l1)))
                      (t
                       ;; (funcall > (car m2) (car m1))
                       (rec (cdr m1) (1- l1)
                            m2 l2
                            (cons (car m1) merged)
                            acc))))))))))

;; https://img.atcoder.jp/jsc2019-qual/editorial.pdf
(defun solve (n k a)
  (declare (ignore n))
  (md (+ (* k (inversion-number a #'>))
         (* (/ (* (1- k) k) 2)
            (reduce #'+ a
                    :key (mapper (ranks a)))))))

(defun main ()
  (readlet (n k)
    (let ((a (readlist)))
      (println (solve n k a)))))

#-swank (main)