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

(defun readvector (&rest args)
  (values (read-from-string
           (concatenate 'string "#("
                        (apply #'read-line args)
                        ")"))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym (string ',s))))
          syms)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tree-find-if (predicate tree &key (key #'identity))
    (labels ((rec (tree)
               (cond ((funcall predicate (funcall key tree))
                      (return-from tree-find-if tree))
                     ((consp tree)
                      (rec (car tree))
                      (rec (cdr tree))))))
      (rec tree)))

  (defun apply-form-predicate (operator)
    (lambda (tree)
      (and (consp tree)
           (eq (car tree) operator))))

  (defun labels-definitions->memoized-definitions (fn key table definitions)
    (let ((fn-definition (tree-find-if (apply-form-predicate fn)
                                       definitions)))
      (assert fn-definition)
      (let ((fn (first fn-definition))
            (lambda-list (second fn-definition))
            (body (nthcdr 2 fn-definition)))
        (subst-if (with-gensyms (k val found-p)
                    `(,fn (&rest args)
                          (let ((,k (funcall ,key args)))
                            (multiple-value-bind (,val ,found-p)
                                (gethash ,k ,table)
                              (if ,found-p ,val
                                  (setf (gethash ,k ,table)
                                        (destructuring-bind ,lambda-list args
                                          ,@body)))))))
                  (apply-form-predicate fn)
                  definitions))))
  ) ; eval-when

(defmacro with-memoized ((fn &key (key ''first) (test ''eql)) &body (labels-form))
  "Overrides definitions of FN in labels form to memoized definition.
Asserts the direct child form is labels form."
  (assert (eq 'labels (first labels-form)))
  (with-gensyms (table)
    `(let ((,table (make-hash-table :test ,test)))
       (labels ,(labels-definitions->memoized-definitions
                 fn key table (second labels-form))
         ,@(nthcdr 2 labels-form)))))

;;;
;;; Body
;;;

(defun solve (n a)
  (with-memoized (rec :key 'identity :test 'equal)
    (labels ((cost (i)
               (svref a (mod i n)))
             (rec (i last)
               (if (> i last)
                   0
                   (min (+ (cost (1- i))
                           (rec (+ i 1) last))
                        (+ (cost i)
                           (rec (+ i 2) last))))))
      (min (rec 0 (1- n))
           (+ (cost (1- n))
              (rec 1 (- n 2)))))))

(defun main ()
  (readlet (n)
    (let ((a (readvector)))
      (println (solve n a)))))

#-swank (main)
