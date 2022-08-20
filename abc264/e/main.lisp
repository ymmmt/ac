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

(defmacro collect (n expr)
  `(loop repeat ,n
         collect ,expr))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun var-and-dimension-spec->loop (var-and-dimension-spec body)
    (destructuring-bind (var upper-bound &key downward) var-and-dimension-spec
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
                                    (progn ,@body)))
     self))

(defun make-list-array (dimensions)
  (make-array dimensions :element-type 'list
                         :initial-element nil))

(defun make-bit-array (dimensions &rest args)
  (apply #'make-array dimensions :element-type 'bit args))

(defun connected-components (graph)
  (let* ((n (length graph))
         (ccs (make-array n :element-type 'fixnum
                            :initial-element -1))
         (index -1))
    (labels ((dfs (u)
               (dolist (v (aref graph u))
                 (when (= (aref ccs v) -1)
                   (setf (aref ccs v) index)
                   (dfs v)))))
      (dotimes (u n)
        (when (= (aref ccs u) -1)
          (incf index)
          (setf (aref ccs u) index)
          (dfs u)))
      (values ccs (1+ index)))))

(defmacro bulk-stdout (&body body)
  `(write-string
    (with-output-to-string (*standard-output* nil :element-type 'base-char)
      ,@body)))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mappend (function list)
    (loop for item in list
          append (funcall function item))))

(defmacro sortf (predicate &rest places)
  (let* ((meths (mapcar (lambda (p)
                          (multiple-value-list
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

(defmacro curry* (function &rest arguments)
  (assert (= 1 (count '% arguments :test 'eq)))
  (let ((g% (gensym)))
  `(lambda (,g%)
     (funcall ,function ,@(subst g% '% arguments)))))

;;;
;;; Body
;;;

(defun solve (n m e uvs q xs)
  (declare (ignore m q))
  (let ((ds (make-disjoint-set (1+ n)))
        (on-edge (make-bit-array e :initial-element 1)))
    (dolist (x xs)
      (setf (aref on-edge x) 0))
    (dotimes (i e)
      (when (plusp (aref on-edge i))
        (destructuring-bind (u . v) (aref uvs i)
          (ds-unite! ds u v))))
    (let ((on (count-if (curry* #'ds-connected-p ds % n)
                        (range n))))
      (nlet rec ((xs (nreverse (cdr xs)))
                 (on on)
                 (acc (list on)))
        (if (null xs)
            acc
            (destructuring-bind (u . v) (aref uvs (car xs))
              (sortf < u v)
              (cond ((= u v n)
                     (rec (cdr xs) on (cons on acc)))
                    ((and (< u n) (= v n))
                     (if (ds-connected-p ds u n)
                         (rec (cdr xs) on (cons on acc))
                         (let ((size (ds-size ds u)))
                           (ds-unite! ds u n)
                           (rec (cdr xs) (+ on size) (cons (+ on size) acc)))))
                    ((ds-connected-p ds u v)
                     (rec (cdr xs) on (cons on acc)))
                    ((and (ds-connected-p ds u n)
                          (ds-connected-p ds v n))
                     (ds-unite! ds u v)
                     (rec (cdr xs) on (cons on acc)))
                    ((ds-connected-p ds u n)
                     (let ((size (ds-size ds v)))
                       (ds-unite! ds u v)
                       (rec (cdr xs) (+ on size) (cons (+ on size) acc))))
                    ((ds-connected-p ds v n)
                     (let ((size (ds-size ds u)))
                       (ds-unite! ds u v)
                       (rec (cdr xs) (+ on size) (cons (+ on size) acc))))
                    (t
                     (ds-unite! ds u v)
                     (rec (cdr xs) on (cons on acc))))))))))

(defun main ()
  (readlet (n m e)
    (let ((uvs (make-array-with-content ((i e))
                 (readlet (u v)
                   (cons (min n (1- u))
                         (min n (1- v)))))))
      (readlet (q)
        (let ((xs (collect q (1- (read-fixnum)))))
          (bulk-stdout
            (printlns (solve n m e uvs q xs))))))))

#-swank (main)
