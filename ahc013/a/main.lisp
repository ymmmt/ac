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

(defun join-print (sequence &key (count (length sequence)) (key #'identity) (test (constantly t)))
  (let ((c 0))
    (map nil (lambda (item)
               (when (and (<= (incf c) count)
                          (funcall test item))
                 (princ (funcall key item))
                 (when (< c count)
                   (princ " "))))
         sequence)
    (fresh-line)))

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

(defun curry (function &rest arguments)
  (lambda (&rest args)
    (multiple-value-call function
      (values-list arguments)
      (values-list args))))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))

(defun map-adjacents (function list)
  (nlet rec ((list list) (acc nil))
    (if (null (cdr list))
        (nreverse acc)
        (rec (cdr list)
             (cons (funcall function (first list) (second list))
                   acc)))))

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

(defmacro ->> (x &rest forms)
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (member '% form)
                               `(funcall (lambda (%) ,form) ,x)
                               `(,(first form) ,@(rest form) ,x))
                           `(,form ,x))))
        `(->> ,threaded ,@(rest forms)))
      x))

;;;
;;; Body
;;;

(defvar *indices*)
(defvar *n*)

(defun blankp (state i j)
  (zerop (aref state i j)))

(defun computerp (state i j)
  (plusp (aref state i j)))

(defun connect (i j k l)
  (list i j k l))

(defun row-connects (state i)
  (->> (filter-map (lambda (j)
                     (when (computerp state i j)
                       (cons (aref state i j) j)))
                   *indices*)
       (map-adjacents (dlambda ((cj1 . j1) (cj2 . j2))
                        (when (= cj1 cj2)
                          (connect i j1 i j2))))
       (delete nil)))

(defun col-connects (state j)
  (->> (filter-map (lambda (i)
                     (when (computerp state i j)
                       (cons (aref state i j) i)))
                   *indices*)
       (map-adjacents (dlambda ((ci1 . i1) (ci2 . i2))
                        (when (= ci1 ci2)
                          (connect i1 j i2 j))))
       (delete nil)))

(defun delete-crosses (row-connects col-connects)
  (nconc (delete-if (dlambda ((i j1 _ j2))
                      (some (dlambda ((k1 l k2 _))
                              (and (<= k1 i k2)
                                   (<= j1 l j2)))
                            col-connects))
                    row-connects)
         col-connects))
            
(defun connects (state)
  (delete-crosses (mapcan (curry #'row-connects state)
                          *indices*)
                  (mapcan (curry #'col-connects state)
                          *indices*)))

(defun solve (n k state)
  (declare (ignore n k))
  (let ((connects (connects state)))
  (values 0
          nil
          (length connects)
          connects)))
          
(defun setup-vars (n)
  (setf *indices* (range n)
        *n* n))

(defun read-state (n)
  (let ((state (make-array `(,n ,n) :element-type 'uint4)))
    (dotimes (i n)
      (let ((line (read-line)))
        (dotimes (j n)
          (awhen (digit-char-p (char line j))
            (setf (aref state i j) it)))))
    state))

(defun main ()
  (readlet (n k)
      (setup-vars n)
      (multiple-value-bind (x moves y connects)
          (solve n k (read-state n))
        (println x)
        (dolist (m moves)
          (join-print m))
        (println y)
        (dolist (c connects)
          (join-print c)))))

#-swank (main)
