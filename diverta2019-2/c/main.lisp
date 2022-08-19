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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\^
   (lambda (stream char num)
     (declare (ignore char))
     (unless num (setf num 1))
     `(lambda ,(loop for i from 1 to num
                     collect (intern (format nil "%~d" i)))
        ,(read stream t nil t)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\<
   (lambda (stream char num)
     (declare (ignore char num))
     (cond ((char= #\= (peek-char t stream))
            (read-char stream)
            `(lambda (x) (<= x ,(read stream t nil t))))
           (t
            `(lambda (x) (< x ,(read stream t nil t))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\>
   (lambda (stream char num)
     (declare (ignore char num))
     (cond ((char= #\= (peek-char t stream))
            (read-char stream)
            `(lambda (x) (>= x ,(read stream t nil t))))
           (t
            `(lambda (x) (> x ,(read stream t nil t))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\%
   (lambda (stream char num)
     (declare (ignore char num))
     `(lambda (x) (mod x ,(read stream t nil t))))))

(defun last1 (sequence)
  (etypecase sequence
    (list (car (last sequence)))
    (vector (when (plusp (length sequence))
              (aref sequence (1- (length sequence)))))))

(defun filter (predicate sequence &key from-end (start 0) end count key)
  (remove-if-not predicate sequence
                 :from-end from-end
                 :start start
                 :end end
                 :count count
                 :key key))

(defmacro bulk-stdout (&body body)
  `(write-string
    (with-output-to-string (*standard-output* nil :element-type 'base-char)
      ,@body)))

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

;;;
;;; Body
;;;

(defun delete1 (item list &key (test #'eql))
  (when list
    (if (funcall test item (car list))
        (cdr list)
        (cons (car list)
              (delete1 item (cdr list) :test test)))))

;; 問題は、∑  (* si ai)の最大値を求める問題に帰着できる。
;; ただしsiは+1or-1で、すべてのsiが+1またはすべてのsiが-1になる
;; 以外の全組み合わせが可能。
;; -1を掛ける項が最低1つ必要なので、最小要素minを-1掛ける要素に
;; 固定する。minを取り除いたaを改めてaとすると、
;; 答えは(+ (- min) (sum (mapcar #'abs a)))と分かる。
;; aを0以上の要素と0未満の要素に分ける。
;; |a+|=kとする。(k=0ならこの工程はスキップ。)
;; 0以上の要素に関してはすべて+1を掛けたい。そのために、そのうちの
;; ひとつ(akとする)だけとっておいて、操作で(- min a1 a2 a3 ... ak-1)という
;; 値をつくってから、最後に(+ a1 a2 ... ak (- min))という値をつくる。
;; 次に0未満の項は、すべて-1を掛けたいから、順番にひたすら引いていけばいい。
(defun solve (n a)
  (declare (ignore n))
  (let* ((min (reduce #'min a))
         (max (reduce #'max a))
         (a (delete1 min (delete1 max a)))
         (a+ (filter #>=0 a))
         (a- (filter #<0 a)))
    (labels ((rec (m ops terms)
               (if (null terms)
                   (values m
                           ops)
                   (rec (- m (car terms))
                        (cons (list m (car terms))
                              ops)
                        (cdr terms)))))
      (if (null a+)
          (rec (- max min)
               (list (list max min))
               a-)
          (multiple-value-bind (m ops)
              (rec (- min max)
                   (list (list min max))
                   (butlast a+))
            (let* ((ak (last1 a+))
                   (m* (- ak m))
                   (ops* (cons (list ak m)
                               ops)))
              (rec m*
                   ops*
                   a-)))))))

(defun main ()
  (readlet (n)
    (let ((a (readlist)))
      (multiple-value-bind (m ops) (solve n a)
        (println m)
        (bulk-stdout
          (dolist (op (nreverse ops))
            (join-print op)))))))

#-swank (main)
