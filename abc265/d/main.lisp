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

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

;; depends on with-gensyms
(defmacro label-subarray-sum ((name vector &optional (plus-fn '+) (minus-fn '-)) &body body)
  (with-gensyms (vec n cum-sum)
    `(let* ((,vec ,vector)
            (,n (length ,vec))
            (,cum-sum (make-array ,n)))
       (dotimes (i ,n)
         (setf (svref ,cum-sum i)
               (if (zerop i)
                   (aref ,vec i)
                   (,plus-fn (aref ,vec i) (svref ,cum-sum (1- i))))))
       (labels ((,(intern (format nil "~A-SUBARRAY-SUM" name)) (start end)
                  (cond ((> start end) 0)
                        ((zerop start)
                         (svref ,cum-sum end))
                        (t
                         (,minus-fn (svref ,cum-sum end)
                                    (svref ,cum-sum (1- start)))))))
         ,@body))))

(defun binary-search (left right predicate)
  "invariant: (funcall predicate left) => t, (funcall predicate right) => nil"
  (labels ((rec (left right)
             (let ((mid (floor (+ left right) 2)))
               (cond ((= (1+ left) right)
                      left)
                     ((funcall predicate mid)
                      (rec mid right))
                     (t
                      (rec left mid))))))
    (rec left right)))

;;;
;;; Body
;;;

(defun find-xyzw (subarray-sum-fn i n p q r)
  (labels ((bsearch (i sum)
             (binary-search i (1+ n)
                            (lambda (j)
                              (<= (funcall subarray-sum-fn i (1- j))
                                  sum))))
           (subsum= (start end sum)
             (= (funcall subarray-sum-fn start (1- end))
                sum)))
    (let ((x i))
      (let ((y (bsearch x p)))
        (when (and (< x y n)
                   (subsum= x y p))
          (let ((z (bsearch y q)))
            (when (and (< y z n)
                       (subsum= y z q))
              (let ((w (bsearch z r)))
                (and (< z w)
                     (subsum= z w r))))))))))

(defun solve (n p q r as)
  (label-subarray-sum (as (coerce as 'vector))
    (loop for i from 0 to (- n 3)
          thereis (find-xyzw #'as-subarray-sum i n p q r))))

(defun main ()
  (readlet (n p q r)
    (let ((as (readlist)))
      (println (if (solve n p q r as)
                   "Yes"
                   "No")))))

#-swank (main)
