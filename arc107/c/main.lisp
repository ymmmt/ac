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

(defpackage :cp/mod-factorial
  (:use :cl)
  (:export #:mod-factorial))
(in-package :cp/mod-factorial)

(declaim (inline mod-factorial))
(defun mod-factorial (n modulus)
  "Returns N! mod MODULUS."
  (declare ((integer 0 #.most-positive-fixnum) n modulus))
  (labels ((recur (n result)
             (declare ((integer 0 #.most-positive-fixnum) n result))
             (if (zerop n)
                 result
                 (recur (- n 1) (mod (* result n) modulus)))))
    (recur n 1)))

(defpackage :cp/disjoint-set
  (:use :cl)
  (:export #:disjoint-set #:make-disjoint-set #:ds-data
           #:ds-root #:ds-unite! #:ds-connected-p #:ds-size #:ds-clear)
  (:documentation "Provides disjoint set implementation with union by size and
path compression."))
(in-package :cp/disjoint-set)

(defstruct (disjoint-set
            (:constructor make-disjoint-set
                (size &aux (data (make-array size :element-type 'fixnum :initial-element -1))))
            (:conc-name ds-)
            (:predicate nil)
            (:copier nil))
  (data nil :type (simple-array fixnum (*))))

(declaim (inline ds-root))
(defun ds-root (disjoint-set x)
  "Returns the root of X."
  (declare ((mod #.array-dimension-limit) x))
  (let ((data (ds-data disjoint-set)))
    (labels ((recur (x)
               (if (< (aref data x) 0)
                   x
                   (setf (aref data x)
                         (recur (aref data x))))))
      (recur x))))

(declaim (inline ds-unite!))
(defun ds-unite! (disjoint-set x1 x2)
  "Destructively unites X1 and X2 and returns true iff X1 and X2 become
connected for the first time."
  (let ((root1 (ds-root disjoint-set x1))
        (root2 (ds-root disjoint-set x2)))
    (unless (= root1 root2)
      (let ((data (ds-data disjoint-set)))
        ;; NOTE: If you want X1 to always be root, just delete this form. (Time
        ;; complexity becomes worse, however.)
        (when (> (aref data root1) (aref data root2))
          (rotatef root1 root2))
        (incf (aref data root1) (aref data root2))
        (setf (aref data root2) root1)))))

(declaim (inline ds-connected-p))
(defun ds-connected-p (disjoint-set x1 x2)
  "Returns true iff X1 and X2 have the same root."
  (= (ds-root disjoint-set x1) (ds-root disjoint-set x2)))

(declaim (inline ds-size))
(defun ds-size (disjoint-set x)
  "Returns the size of the connected component to which X belongs."
  (- (aref (ds-data disjoint-set)
           (ds-root disjoint-set x))))

(declaim (inline ds-clear))
(defun ds-clear (disjoint-set)
  "Deletes all connections."
  (fill (ds-data disjoint-set) -1)
  disjoint-set)

;; BEGIN_USE_PACKAGE
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/read-fixnum :cl-user))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/mod-factorial :cl-user))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/disjoint-set :cl-user))

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

(defun read-matrix (height width &key (read #'read-fixnum) (element-type 'fixnum))
  (let ((mat (make-array `(,height ,width) :element-type element-type)))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref mat i j) (funcall read))))
    mat))

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

(defmacro curry* (function &rest arguments)
  (assert (= 1 (count '% arguments :test 'eq)))
  (let ((g% (gensym)))
  `(lambda (,g%)
     (funcall ,function ,@(subst g% '% arguments)))))

(defun curry (function &rest arguments)
  (lambda (&rest args)
    (multiple-value-call function
      (values-list arguments)
      (values-list args))))

(defun counter (sequence &key (test 'eql) (key 'identity))
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

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun filter-map (function list)
  (nlet rec ((list list) (acc nil))
    (if (null list)
        (nreverse acc)
        (rec (cdr list)
             (aif (funcall function (car list))
                  (cons it acc)
                  acc)))))

(defun filter (predicate sequence &key from-end (start 0) end count key)
  (remove-if-not predicate sequence
                 :from-end from-end
                 :start start
                 :end end
                 :count count
                 :key key))

(defun combinations (list k &optional (n (length list)))
  "returns power set of LIST when K = -1"
  (cond ((or (< n k) (zerop n) (zerop k))
         (list nil))
        ((= n k)
         (list list))
        (t
         (let ((x (car list)))
           (append (mapcar (lambda (comb) (cons x comb))
                           (combinations (cdr list) (1- k) (1- n)))
                   (combinations (cdr list) k (1- n)))))))

;; paip ch9
(defun memo (fn &key (key #'first) (test #'eql) name)
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

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

(defun md (x)
  (mod x +mod+))

;;;
;;; Body
;;;

(defun row (n a i)
  (mapcar (curry* #'aref a i %)
          (range n)))

(defun col (n a j)
  (mapcar (curry* #'aref a % j)
          (range n)))

(defun row-swaps (n a k)
  (filter (dlambda ((i j))
            (every (lambda (x y)
                     (<= (+ x y) k))
                   (row n a i)
                   (row n a j)))
          (delete nil
                  (combinations (range n) 2))))

(defun col-swaps (n a k)
  (filter (dlambda ((i j))
            (every (lambda (x y)
                     (<= (+ x y) k))
                   (col n a i)
                   (col n a j)))
          (delete nil
                  (combinations (range n) 2))))

(memoize 'row :key #'third)
(memoize 'col :key #'third)

;; depends on cp/disjoint-set
(defun ds-count (ds)
  (length (ds-data ds)))

;; depends on range, cp/disjoint-set, ht-keys, counter
(defun ds-roots-size>=2 (ds)
  (let ((n (length (ds-data ds))))
    (ht-keys
     (counter (filter-map (lambda (i)
                            (when (>= (ds-size ds i) 2)
                              (ds-root ds i)))
                          (range n))))))

(defun solve (n k a)
  (if (= n 1)
      1
      (let ((rs (make-disjoint-set n))
            (cs (make-disjoint-set n)))
        (mapc (dlambda ((i j))
                (ds-unite! rs i j))
              (row-swaps n a k))
        (mapc (dlambda ((i j))
                (ds-unite! cs i j))
              (col-swaps n a k))
        (let ((r-roots (ds-roots-size>=2 rs))
              (c-roots (ds-roots-size>=2 cs)))
          (mod-acc * 1 +mod+ (size (nconc (mapcar (curry #'ds-size rs) r-roots)
                                          (mapcar (curry #'ds-size cs) c-roots)))
            (mod-factorial size +mod+))))))

(defun main ()
  (readlet (n k)
    (let ((a (read-matrix n n)))
      (println (solve n k a)))))

#-swank (main)
