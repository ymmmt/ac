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

(defun random-choice (list)
  (nth (random (length list)) list))

(defun judge (probability)
  (< (random 1.0) probability))

(defun singletonp (list)
  (and (consp list) (null (cdr list))))

(defun nshuffle (vector)
  (loop for i from (length vector) downto 2
        do (rotatef (aref vector (random i))
                    (aref vector (1- i))))
  vector)

(defun coerce-vector (object)
  (coerce object 'vector))

(defun coerce-list (object)
  (coerce object 'list))

;; from serapeum
;; https://github.com/ruricolist/serapeum/blob/master/LICENSE.txt
;; https://github.com/ruricolist/serapeum/blob/master/definitions.lisp#L129
(defmacro defsubst (name params &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params
       ,@body)))

;;;
;;; Body
;;;

(defvar *indices*)
(defvar *n*)

(defstruct (state (:conc-name nil)
                  (:constructor make-state
                      (grid coms)))
  (grid nil :type (or null (simple-array int8 (* *))))
  (coms nil :type (or null (simple-array com (*))))
  (conns nil :type list))

(defstruct (com (:conc-name nil)
                (:constructor make-com
                    (com-row com-col)))
  (com-row 0 :type uint8)
  (com-col 0 :type uint8)
  (u-conn nil :type (or null com))
  (d-conn nil :type (or null com))
  (l-conn nil :type (or null com))
  (r-conn nil :type (or null com)))

(defsubst cablep (grid i j)
  (= (aref grid i j) -1))

(defsubst blankp (grid i j)
  (zerop (aref grid i j)))

(defsubst comp (grid i j)
  (when (plusp (aref grid i j))
    (aref grid i j)))

(defsubst conn (i j k l)
  (list i j k l))

(defun random-conn (grid)
  (let ((conns (if (judge 0.5)
                   (row-conns grid (random *n*))
                   (col-conns grid (random *n*)))))
    (if conns
        (random-choice conns)
        (random-conn grid))))
  
(defun row-conns (grid i)
  (->> (filter-map (lambda (j)
                     (when (comp grid i j)
                       (cons (aref grid i j) j)))
                   *indices*)
       (map-adjacents (dlambda ((c1 . j1) (c2 . j2))
                        (when (= c1 c2)
                          (conn i j1 i j2))))
       (delete nil)))

(defun col-conns (grid j)
  (->> (filter-map (lambda (i)
                     (when (comp grid i j)
                       (cons (aref grid i j) i)))
                   *indices*)
       (map-adjacents (dlambda ((c1 . i1) (c2 . i2))
                        (when (= c1 c2)
                          (conn i1 j i2 j))))
       (delete nil)))

;; (defun delete-crosses (row-conns col-conns)
;;   (nconc (delete-if (dlambda ((i j1 _ j2))
;;                       (some (dlambda ((k1 l k2 _))
;;                               (and (<= k1 i k2)
;;                                    (<= j1 l j2)))
;;                             col-conns))
;;                     row-conns)
;;          col-conns))
            
(defun conns (grid)
  (nconc (mapcan (curry #'row-conns grid)
                 *indices*)
         (mapcan (curry #'col-conns grid)
                 *indices*)))

(defun try-connect (grid conn)
  (destructuring-bind (i j k l) conn
    (cond ((= i k)
           (when (try-row-connect grid i j l)
             conn))
          ((= j l)
           (when (try-col-connect grid j i k)
             conn))
          (t
           (error "invalid conn ~A" conn)))))

(defun try-row-connect (grid row j1 j2)
  (if (= (1+ j1) j2)
      grid
      (when (loop for j from (1+ j1) below j2
                  always (blankp grid row j))
        (let ((com-type (aref grid row j1)))
          (loop for j from (1+ j1) below j2 do
            (setf (aref grid row j) com-type))
          grid))))

(defun try-col-connect (grid col i1 i2)
  (if (= (1+ i1) i2)
      grid
      (when (loop for i from (1+ i1) below i2
                  always (blankp grid i col))
        (let ((com-type (aref grid i1 col)))
          (loop for i from (1+ i1) below i2 do
            (setf (aref grid i col) com-type))
          grid))))

(defun random-connect (grid)
  (let ((conns (-> grid conns coerce-vector nshuffle coerce-list)))
    (filter-map (lambda (conn)
                  (try-connect grid conn))
                conns)))

(defun solve (n k grid)
  (declare (ignore n k))
  (let ((conns (random-connect grid)))
    (values 0
            nil
            (length conns)
            conns)))
          
(defun setup-vars (n)
  (setf *indices* (range n)
        *n* n))

(defun read-grid (n)
  (let ((grid (make-array `(,n ,n) :element-type 'int8)))
    (dotimes (i n)
      (let ((line (read-line)))
        (dotimes (j n)
          (awhen (digit-char-p (char line j))
            (setf (aref grid i j) it)))))
    grid))

(defun main ()
  (readlet (n k)
    (setup-vars n)
    (multiple-value-bind (x moves y conns)
        (solve n k (read-grid n))
      (println x)
      (dolist (m moves)
        (join-print m))
      (println y)
      (dolist (c conns)
        (join-print c)))))

#-swank (main)
