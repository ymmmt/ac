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

(defpackage :cp/bisect
  (:use :cl)
  (:export #:bisect-left #:bisect-right))
(in-package :cp/bisect)

(declaim (inline bisect-left))
(defun bisect-left (target value &key (start 0) end (order #'<) (key #'identity))
  "TARGET := vector | function (taking an integer argument)
ORDER := strict order

Analogue of lower_bound() of C++ or bisect_left() of Python: Returns the least
index (or input) i such that TARGET[i] >= VALUE, where '>=' is the complement of
ORDER. In other words, this function returns the leftmost index at which VALUE
can be inserted with keeping the order. Therefore, TARGET must be monotonically
non-decreasing with respect to ORDER.

- This function returns END if VALUE exceeds TARGET[END-1]. 
- The range [START, END) is half-open.
- END must be explicitly specified if TARGET is function.
- KEY is applied to each element of TARGET before comparison."
  (declare (integer start)
           ((or null integer) end))
  (macrolet
      ((frob (accessor &optional declaration)
         `(labels
              ((%bisect-left (ng ok)
                 ;; TARGET[OK] >= VALUE always holds (assuming
                 ;; TARGET[END] = +infinity)
                 ,@(when declaration (list declaration))
                 (if (<= (- ok ng) 1)
                     ok
                     (let ((mid (ash (+ ng ok) -1)))
                       (if (funcall order (funcall key (,accessor target mid)) value)
                           (%bisect-left mid ok)
                           (%bisect-left ng mid))))))
            (assert (<= start end))
            (%bisect-left (- start 1) end))))
    (etypecase target
      (vector
       (let ((end (or end (length target))))
         (frob aref (declare ((integer -1 (#.most-positive-fixnum)) ng ok)))))
      (function
       (assert end () "Requires END argument if TARGET is a function.")
       (frob funcall)))))

(declaim (inline bisect-right))
(defun bisect-right (target value &key (start 0) end (order #'<) (key #'identity))
  "TARGET := vector | function (taking an integer argument)
ORDER := strict order

Analogue of upper_bound() of C++ or bisect_right() of Python: Returns the least
index (or input) i such that TARGET[i] > VALUE. In other words, this function
returns the rightmost index at which VALUE can be inserted with keeping the
order. Therefore, TARGET must be monotonically non-decreasing with respect to
ORDER.

- This function returns END if VALUE >= TARGET[END-1].
- The range [START, END) is half-open.
- END must be explicitly specified if TARGET is function.
- KEY is applied to each element of TARGET before comparison."
  (declare (integer start)
           ((or null integer) end))
  (macrolet
      ((frob (accessor &optional declaration)
         `(labels
              ((%bisect-right (ng ok)
                 ;; TARGET[OK] > VALUE always holds (assuming
                 ;; TARGET[END] = +infinity)
                 ,@(when declaration (list declaration))
                 (if (<= (- ok ng) 1)
                     ok
                     (let ((mid (ash (+ ng ok) -1)))
                       (if (funcall order value (funcall key (,accessor target mid)))
                           (%bisect-right ng mid)
                           (%bisect-right mid ok))))))
            (assert (<= start end))
            (%bisect-right (- start 1) end))))
    (etypecase target
      (vector
       (let ((end (or end (length target))))
         (frob aref (declare ((integer -1 (#.array-dimension-limit)) ng ok)))))
      (function
       (assert end () "Requires END argument if TARGET is a function.")
       (frob funcall)))))

;; BEGIN_USE_PACKAGE
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/read-fixnum :cl-user))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cp/bisect :cl-user))

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

(defun filter (predicate sequence &key from-end (start 0) end count key)
  (remove-if-not predicate sequence
                 :from-end from-end
                 :start start
                 :end end
                 :count count
                 :key key))

(defmacro curry (fn &rest args)
  (with-gensyms (%)
    `(lambda (,%)
       (,fn ,@args ,%))))

;;;
;;; Body
;;;

(defsubst cut-count (l ai)
  (max 0 (1- (ceiling ai l))))

(defsubst cut-count-sum (a l)
  (assert (plusp l))
  (reduce #'+ a
          :key (curry cut-count l)))

(defun solve (n k a)
  (declare (ignore n))
  (let ((max (reduce #'max a)))
    (bisect-left (curry cut-count-sum a)
                 k :start 1 :end (1+ max)
                 :order #'>)))

(defun main ()
  (readlet (n k)
    (let ((a (readlist)))
      (println (solve n k a)))))

#-swank (main)
