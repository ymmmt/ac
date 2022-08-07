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

(defmacro dlambda (lambda-list &body body)
  (let ((gargs (gensym "ARGS")))
    `(lambda (&rest ,gargs)
       (destructuring-bind ,lambda-list ,gargs
         ,@body))))

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

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

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

;; depends on zip
(defun zip-with-index (list &optional (index-base 0))
  (zip list
       (range index-base
              (+ index-base (length list)))))

(defun split-at (n list)
  (labels ((rec (n list left)
             (if (or (zerop n) (null list))
                 (values (nreverse left) list)
                 (rec (1- n) (cdr list) (cons (car list) left)))))
    (rec n list nil)))

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

(defun mvfoldl (function list initial-value &rest initial-args)
  "Generalization of FOLDL.
(FUNCTION item acc arg1 arg2 ... argN) => new-acc, new-arg1, new-arg2,..., new-argN
LIST -- list of items
INITIAL-ARGS == (initial-arg1 initial-arg2 ... initial-argN)"
  (declare (optimize (speed 3) (debug 1)))
  (declare (function function))
  (if (null list)
      (apply #'values initial-value initial-args)
      (multiple-value-call #'mvfoldl
        function
        (cdr list)
        (apply function (car list) initial-value initial-args))))

(defconstant inf most-positive-fixnum)

;; (< i k ), (<= k j)、(< ai aj)なるi, jを見つけられれば、
;; まずaiを(1- k)番目(上位k個の末尾)に持ってきて、
;; 次にajをそこへ持っていくという
;; (+ (- (1- k) i) (- j (1- k))) = (- j i)
;; 回の操作で求める解が得られる。
;; (<= k j)なる各(aj, j)に対して
;; (< i k)であって(< ai aj)を満たすようなiの
;; 最大値があればそれを見つけ、
;; (- j i)を最小化する。
;; そのようなiを効率的に求めるために累積minをとっておく。
(defun solve (n k a)
  (declare (ignore n))
  (multiple-value-bind (xs ys)
      (split-at k a)
    (let ((xs* (-> (zip-with-index xs)
                   (sort #'< :key #'car)
                   (scanl1 (dlambda ((_ acc) (x i))
                             (list x (max acc i)))
                           %)
                   (coerce 'vector))))
      (mvfoldl (lambda (y min j) 
                 (let ((i (binary-search
                           -1 k
                           (lambda (l)
                             (< (first (svref xs* l)) y)))))
                   (values (if (= i -1)
                               min
                               (min min (- j (second (svref xs* i)))))
                           (1+ j))))
               ys inf k))))

(defun main ()
  (readlet (n k)
    (let ((a (readlist)))
      (let ((ans (solve n k a)))
        (println (if (< ans inf)
                     ans
                     -1))))))

#-swank (main)
