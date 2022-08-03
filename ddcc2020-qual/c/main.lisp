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

(defun read-char-matrix (height width)
  (let ((mat (make-array `(,height ,width) :element-type 'character)))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref mat i j) (read-char)))
      (read-char)) ; skip #\Newline
    mat))

(defun print-matrix (matrix &optional (sep " "))
  (dotimes (i (array-dimension matrix 0))
    (dotimes (j (array-dimension matrix 1))
      (when (plusp j) (princ sep))
      (princ (aref matrix i j)))
    (fresh-line))
  (fresh-line))

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

(defun berryp (char)
  (char= #\# char))

(defun solve (h w k s)
  (declare (ignore k))
  (let ((res (make-array `(,h ,w) :element-type 'fixnum))
        (num 0))
    (dotimes (i h)
      (dotimes (j w)
        (when (berryp (aref s i j))
          (setf (aref res i j) (incf num)))))
    (dotimes (i h)
      (loop for j from 1 below w do
        (when (zerop (aref res i j))
          (setf (aref res i j)
                (aref res i (1- j)))))
      (loop for j from (- w 2) downto 0 do
        (when (zerop (aref res i j))
          (setf (aref res i j)
                (aref res i (1+ j))))))
    (dotimes (j w)
      (loop for i from 1 below h do
        (when (zerop (aref res i j))
          (setf (aref res i j)
                (aref res (1- i) j))))
      (loop for i from (- h 2) downto 0 do
        (when (zerop (aref res i j))
          (setf (aref res i j)
                (aref res (1+ i) j)))))
    res))

(defun main ()
  (readlet (h w k)
    (let ((s (read-char-matrix h w)))
      (print-matrix (solve h w k s)))))

#-swank (main)
