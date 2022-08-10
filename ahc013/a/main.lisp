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

(defmacro curry* (function &rest arguments)
  (assert (= 1 (count '% arguments :test 'eq)))
  (let ((g% (gensym)))
  `(lambda (,g%)
     (funcall ,function ,@(subst g% '% arguments)))))

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

(defun filter (predicate sequence &key from-end (start 0) end count key)
  (remove-if-not predicate sequence
                 :from-end from-end
                 :start start
                 :end end
                 :count count
                 :key key))

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

(defun random-a-b (a b)
  "return random integer r that satisfies A <= r < B."
  (+ (random (- b a)) a))

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

(defun dist (x y)
  (abs (- x y)))

(defun take (n list &key (step 1))
  (if (or (zerop n) (null list))
      nil
      (cons (car list)
            (take (1- n) (nthcdr step list) :step step))))

;;;
;;; Body
;;;

(defvar *indices*)
(defvar *n*)
(defvar *ops-count-limit*)
(defvar *moves-count-limit*)

(defconstant +cable+ -1)
(defconstant +blank+ 0)

(defsubst cablep (grid i j)
  (= (aref grid i j) +cable+))

(defsubst blankp (grid i j)
  (= (aref grid i j) +blank+))

(defsubst comp (grid i j)
  (when (plusp (aref grid i j))
    (aref grid i j)))

;;; Random moves

(defun make-move (i j k l)
  (list i j k l))

(defun make-row-moves (row j1 j2)
  (if (= j1 j2)
      (values nil 0)
      (values (map-adjacents (lambda (l1 l2)
                               (make-move row l1 row l2))
                             (if (< j1 j2)
                                 (range j1 (1+ j2))
                                 (nreverse (range j2 (1+ j1)))))
              (dist j1 j2))))
    
(defun make-col-moves (col i1 i2)
  (if (= i1 i2)
      (values nil 0)
      (values (map-adjacents (lambda (k1 k2)
                               (make-move k1 col k2 col))
                             (if (< i1 i2)
                                 (range i1 (1+ i2))
                                 (nreverse (range i2 (1+ i1)))))
              (dist i1 i2))))

(defun row-coms (grid row)
  (filter (curry* #'comp grid row %)
          *indices*))

(defun col-coms (grid col)
  (filter (curry* #'comp grid % col)
          *indices*))

(defun random-row-reposition (grid row)
  (let ((js (row-coms grid row)))
    (when js
      (labels ((repos (j start end)
                 (let ((j* (random-a-b start end)))
                   (rotatef (aref grid row j)
                            (aref grid row j*))
                   j*)))
        (nlet rec ((js (nconc js (list *n*)))
                   (start 0)
                   (moves-list nil)
                   (count 0))
          (if (singletonp js)
              (values (apply #'nconc (nreverse moves-list))
                      count)
              (let ((j* (repos (first js) start (second js))))
                (multiple-value-bind (ms c)
                    (make-row-moves row (car js) j*)
                  (rec (cdr js)
                       (1+ j*)
                       (cons ms moves-list)
                       (+ count c))))))))))

(defun random-col-reposition (grid col)
  (let ((is (col-coms grid col)))
    (when is
      (labels ((repos (i start end)
                 (let ((i* (random-a-b start end)))
                   (rotatef (aref grid i col)
                            (aref grid i* col))
                   i*)))
        (nlet rec ((is (nconc is (list *n*)))
                   (start 0)
                   (moves-list nil)
                   (count 0))
          (if (singletonp is)
              (values (apply #'nconc (nreverse moves-list))
                      count)
              (let ((i* (repos (first is) start (second is))))
                (multiple-value-bind (ms c)
                    (make-col-moves col (car is) i*)
                  (rec (cdr is)
                       (1+ i*)
                       (cons ms moves-list)
                       (+ count c))))))))))
    
(defun random-reposition (grid)
  (if (judge 0.5)
      (random-row-reposition grid (random *n*))
      (random-col-reposition grid (random *n*))))

;;; Random connections

(defsubst make-conn (i j k l)
  (list i j k l))
  
(defun row-conns (grid row)
  (->> (filter-map (lambda (j)
                     (when (comp grid row j)
                       (cons (aref grid row j) j)))
                   *indices*)
       (map-adjacents (dlambda ((c1 . j1) (c2 . j2))
                        (when (= c1 c2)
                          (make-conn row j1 row j2))))
       (delete nil)))

(defun col-conns (grid col)
  (->> (filter-map (lambda (i)
                     (when (comp grid i col)
                       (cons (aref grid i col) i)))
                   *indices*)
       (map-adjacents (dlambda ((c1 . i1) (c2 . i2))
                        (when (= c1 c2)
                          (make-conn i1 col i2 col))))
       (delete nil)))

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
        (loop for j from (1+ j1) below j2 do
          (setf (aref grid row j) +cable+))
        grid)))

(defun try-col-connect (grid col i1 i2)
  (if (= (1+ i1) i2)
      grid
      (when (loop for i from (1+ i1) below i2
                  always (blankp grid i col))
        (loop for i from (1+ i1) below i2 do
          (setf (aref grid i col) +cable+))
        grid)))

(defun random-connect (grid)
  (let ((conns (-> (conns grid) coerce-vector nshuffle coerce-list)))
    (filter-map (curry #'try-connect grid)
                conns)))

;;; Main

(defun solve (n k grid)
  (declare (ignore n k))
  (multiple-value-bind (moves count)
      (nlet rec ((moves-list nil) (count 0))
            (if (>= count *moves-count-limit*)
                (values (apply #'nconc (nreverse moves-list))
                        count)
                (multiple-value-bind (ms c)
                    (random-reposition grid)
                  (rec (cons ms moves-list)
                       (+ count c)))))
    (let ((conns (take (- *ops-count-limit* count)
                       (random-connect grid))))
      (values count
              moves
              (length conns)
              conns))))

(defun setup-vars (n k)
  (setf *indices* (range n)
        *n* n
        *ops-count-limit* (* k 100)
        *moves-count-limit* (/ (* k 100) 2)))

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
    (setup-vars n k)
    (multiple-value-bind (x moves y conns)
        (solve n k (read-grid n))
      (println x)
      (dolist (m moves)
        (join-print m))
      (println y)
      (dolist (c conns)
        (join-print c)))))

#-swank (main)
