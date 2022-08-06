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

(defun mapper (hash-table &optional default)
  (lambda (key)
    (gethash key hash-table default)))

;; depends on ranks, mapper
(defun coordinate-compress (list &key (test '<) (index-base 0))
  (mapcar (mapper (ranks list :test test :rank-base index-base))
          list))
            
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

(defconstant inf most-positive-fixnum)

;; 次のようなi、j(ただしi<=(1- k)<=j、ai<aj)を見つけられれば、
;; まずaiを(1- k)番目(上位k個の末尾)に持ってきて、
;; 次にajをそこへ持っていくという
;; (+ (- (1- k) i) (- j (1- k))) = (- j i)
;; 回の操作で求める解が得られる。
;; (<= k j)なる各(aj, j)に対して
;; (< i k)であって(< ai aj)を満たすようなiの
;; 最大値(leftmost-pos)があればそれを見つけ、
;; (- j leftmost-pos)を最小化する。
(defun solve (n k a)
  (declare (ignore n))
  (let ((a (coordinate-compress a)))
    (multiple-value-bind (xs ys) (split-at k a)
      (let ((rightmost-pos (make-hash-table))
            (i -1))
        (dolist (x xs)
          (setf (gethash x rightmost-pos)
                (incf i)))
        (let* ((xs (sort xs #'<))
               (xs* (-> (mapcar (mapper rightmost-pos)
                                xs)
                        (scanl1 #'max %)
                        (zip xs %)
                        (coerce 'vector)))
               (ys* (zip-with-index ys k)))
          (reduce (lambda (acc _)
                    (destructuring-bind (y j) _
                      (let ((i (binary-search
                                -1 k
                                (lambda (i)
                                  (destructuring-bind (x i) (svref xs* i)
                                    (< x y))))))
                        (if (= i -1)
                            acc
                            (let ((leftmost-pos (second (svref xs* i))))
                              (min acc (- j leftmost-pos)))))))
                  ys*
                  :initial-value inf))))))      

(defun main ()
  (readlet (n k)
    (let ((a (readlist)))
      (let ((ans (solve n k a)))
        (println (if (< ans inf)
                     ans
                     -1))))))
                   
#-swank (main)
