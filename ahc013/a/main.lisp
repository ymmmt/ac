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

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(let ((it ,(car args)))
              (when it
                (aand ,@(cdr args)))))))

;; depends on take
(defun map-adjacents (function list &optional (k 2))
  (nlet rec ((list list) (acc nil))
    (if (null (nthcdr (1- k) list))
        (nreverse acc)
        (rec (cdr list)
             (cons (apply function (take k list))
                   acc)))))

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

(defun nshuffle (vector)
  (loop for i from (length vector) downto 2
        do (rotatef (aref vector (random i))
                    (aref vector (1- i))))
  vector)

(defsubst coerce-vector (object)
  (coerce object 'vector))

(defsubst coerce-list (object)
  (coerce object 'list))

(defsubst dist (x y)
  (abs (- x y)))

(defun take (n list &key (step 1))
  (nlet rec ((n n) (list list) (acc nil))
    (if (or (zerop n) (null list))
        (nreverse acc)
        (rec (1- n)
             (nthcdr step list)
             (cons (car list) acc)))))

(defsubst row-major-index (i j n-cols)
  (+ (* i n-cols) j))

(defun counter (sequence &key (test 'eql) (key 'identity))
  (let ((counter (make-hash-table :size (length sequence) :test test)))
    (map nil (lambda (elem)
               (let ((k (funcall key elem)))
                 (setf (gethash k counter)
                       (1+ (gethash k counter 0)))))
         sequence)
    counter))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defmacro domatrix ((element i j matrix) &body body)
  (let ((gmat (gensym)))
    `(let ((,gmat ,matrix))
       (symbol-macrolet ((,element (aref ,gmat ,i ,j))) 
         (dotimes (,i (array-dimension ,gmat 0))
           (dotimes (,j (array-dimension ,gmat 1))
             ,@body))))))

(defmacro collect (n expr)
  `(loop repeat ,n
         collect ,expr))

(defun ht-keys (hash-table)
  (loop for k being the hash-key of hash-table
        collect k))

(defun mvfoldl (function list initial-value &rest initial-args)
  "Generalization of FOLDL.
(FUNCTION item acc arg1 arg2 ... argN) => new-acc, new-arg1, new-arg2,..., new-argN
LIST -- list of items
INITIAL-ARGS == (initial-arg1 initial-arg2 ... initial-argN)"
  (declare (optimize (speed 3) (debug 1)))
  (declare (function function))
  (if (null list)
      (apply #'values initial-value initial-args)
      (multiple-value-call #'mvfoldl
        function
        (cdr list)
        (apply function (car list) initial-value initial-args))))

;; depends on mvfoldl
(defun best (function list)
  (assert (consp list))
  (mvfoldl (lambda (item argmax max)
             (let ((value (funcall function item)))
               (if (> value max)
                   (values item value)
                   (values argmax max))))
           (cdr list)
           (car list)
           (funcall function (car list))))

(defmacro with-timelimit ((seconds) &body body)
  (let ((gstart (gensym "START-TIME"))
        (gend (gensym "END-TIME")))
    `(let* ((,gstart (get-internal-real-time))
            (,gend (+ ,gstart
                      (* ,seconds
                         internal-time-units-per-second))))
       (labels ((time-up-p ()
                  (>= (get-internal-real-time) ,gend)))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mksym (control-string &rest format-arguments)
    (intern (apply #'format nil control-string format-arguments))))

(defun applier (fn)
  (lambda (args)
    (apply fn args)))

;;;
;;; Body
;;;

(defvar *indices*)
(defvar *indices^2*)
(defvar *n*)
(defvar *coms-count*)
(defvar *ops-count-limit*)
(defvar *moves-count-limit*)
(defvar *ds*)
(defvar *initial-temperature*)
(defvar *connection-search-duration*)

(defconstant +epsilon+ 1)
(defconstant +cable+ -1)
(defconstant +blank+ 0)

(defsubst make-ds ()
  (ds-clear *ds*))
   
;;; Predicates

(defsubst invalid-subscripts-p (i j)
  (or (< i 0) (>= i *n*)
      (< j 0) (>= j *n*)))
      
(defsubst cablep (grid i j)
  (minusp (aref grid i j)))

(defsubst blankp (grid i j)
  (= (aref grid i j) +blank+))

(defsubst comp (grid i j)
  (when (plusp (aref grid i j))
    (aref grid i j)))

(defsubst type-com-p (com-type grid i j)
  (= com-type (aref grid i j)))

;;; Coms

(defsubst row-coms (grid row)
  (filter (curry* #'comp grid row %)
          *indices*))

(defsubst col-coms (grid col)
  (filter (curry* #'comp grid % col)
          *indices*))

(defun coms (grid)
  (mapcan (lambda (row)
            (mapcar (curry #'cons row)
                    (row-coms grid row)))
          *indices*))

;;; Moves

(defsubst make-move (r1 c1 r2 c2)
  (list r1 c1 r2 c2))

(defun make-row-moves (row c1 c2)
  (if (= c1 c2)
      (values nil 0)
      (values (map-adjacents (lambda (j1 j2)
                               (make-move row j1 row j2))
                             (if (< c1 c2)
                                 (range c1 (1+ c2))
                                 (nreverse (range c2 (1+ c1)))))
              (dist c1 c2))))

(defun make-col-moves (col r1 r2)
  (if (= r1 r2)
      (values nil 0)
      (values (map-adjacents (lambda (i1 i2)
                               (make-move i1 col i2 col))
                             (if (< r1 r2)
                                 (range r1 (1+ r2))
                                 (nreverse (range r2 (1+ r1)))))
              (dist r1 r2))))

(defun make-moves (r1 c1 r2 c2)
  (cond ((= r1 r2)
         (make-row-moves r1 c1 c2))
        ((= c1 c2)
         (make-col-moves c1 r1 r2))
        (t
         (error "make-moves: invalid args"))))

(defun reposition! (grid r1 c1 r2 c2)
  (assert (or (= r1 r2) (= c1 c2)))
  (rotatef (aref grid r1 c1)
           (aref grid r2 c2)))

;;; Connections

(defsubst make-conn (r1 c1 r2 c2)
  (list r1 c1 r2 c2))

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

(defun try-row-connect! (grid row c1 c2)
  (if (= (1+ c1) c2)
      grid
      (when (loop for j from (1+ c1) below c2
                  always (blankp grid row j))
        (loop for j from (1+ c1) below c2 do
          (setf (aref grid row j) +cable+))
        grid)))

(defun try-col-connect! (grid col r1 r2)
  (if (= (1+ r1) r2)
      grid
      (when (loop for i from (1+ r1) below r2
                  always (blankp grid i col))
        (loop for i from (1+ r1) below r2 do
          (setf (aref grid i col) +cable+))
        grid)))

(defun try-connect! (grid r1 c1 r2 c2)
  (cond ((= r1 r2)
         (when (try-row-connect! grid r1 c1 c2)
           (make-conn r1 c1 r2 c2)))
        ((= c1 c2)
         (when (try-col-connect! grid c1 r1 r2)
           (make-conn r1 c1 r2 c2)))
        (t
         (error "try-connect!: invalid conn"))))

(defun random-connect! (grid)
  (let ((conns (-> (conns grid) coerce-vector nshuffle coerce-list)))
    (filter-map (applier (curry #'try-connect! grid))
                conns)))

(defun search-best-conns (grid moves-count timelimit)
  (with-timelimit (timelimit)
    (nlet rec ((cands nil))
      (if (time-up-p)
          (best #'conns-cost cands)
          (rec (cons (take (- *ops-count-limit* moves-count)
                           (random-connect! (copy grid)))
                     cands))))))

;;; Cost

(defsubst cpower (cluster-size)
  (/ (* cluster-size (1- cluster-size))
     2))

(defun conns-cost (conns)
  (let ((ds (make-ds)))
    (mapc (dlambda ((i j k l))
            (ds-unite! ds
                       (row-major-index i j *n*)
                       (row-major-index k l *n*)))
          conns)
    (let ((roots (counter (filter-map (lambda (i)
                                        (when (>= (ds-size ds i) 2)
                                          (ds-root ds i)))
                                      *indices^2*))))
      (reduce #'+ (ht-keys roots)
              :key (compose #'cpower (curry #'ds-size ds))))))

;;; Main

(defmacro define-com-searches ()
  `(progn
     ,@(mapcar (lambda (dir di dj)
                 `(defun ,(mksym "~AWARD-COM-SEARCH" dir) (com-type grid row col)
                    (nlet rec ((i (+ row ,di))
                               (j (+ col ,dj)))
                      (cond ((invalid-subscripts-p i j) nil)
                            ((cablep grid i j) nil)
                            ((type-com-p com-type grid i j)
                             (values i j))
                            ((comp grid i j) nil)
                            (t
                             (rec (+ i ,di) (+ j ,dj)))))))
               '(up down left right)
               '(-1 1 0 0)
               '(0 0 -1 1))))

(define-com-searches)

(defmacro define-connection-searches ()
  `(progn
     ,@(mapcar (lambda (dir rowcol subdir1 subdir2 di dj)
                 `(defun ,(mksym "~AWARD-~A-CONNECTION-SEARCH" rowcol dir)
                      (grid row col)
                    (aand (comp grid row col)
                          (let ((com-type it))
                            (nlet rec ((i (+ row ,di))
                                       (j (+ col ,dj)))
                              (cond ((invalid-subscripts-p i j) nil)
                                    ((comp grid i j) nil)
                                    ((,(mksym "~AWARD-COM-SEARCH" subdir1) com-type grid i j)
                                     (cons i j))
                                    ((,(mksym "~AWARD-COM-SEARCH" subdir2) com-type grid i j)
                                     (cons i j))
                                    (t
                                     (rec (+ i ,di) (+ j ,dj)))))))))
               '(row row col col)
               '(up down left right)
               '(left left up up)
               '(right right down down)
               '(-1 1 0 0)
               '(0 0 -1 1))))

(define-connection-searches)

(defun connection-search (grid row col)
  (or (upward-row-connection-search grid row col)
      (downward-row-connection-search grid row col)
      (leftward-col-connection-search grid row col)
      (rightward-col-connection-search grid row col)))

(defsubst inc (x)
  (mod (1+ x) *coms-count*))

(defun random-moves! (grid)
  (let ((coms (-> (coms grid) coerce-vector nshuffle)))
    (nlet rec ((x 0)
               (moves-count 0)
               (moves-list nil))
      (cond ((>= moves-count *moves-count-limit*)
             (values moves-count
                     (apply #'nconc (nreverse moves-list))))
            (t
             (destructuring-bind (i . j) (aref coms x)
               (aif (connection-search grid i j)
                    (destructuring-bind (i* . j*) it
                      (reposition! grid i j i* j*)
                      (multiple-value-bind (ms c) (make-moves i j i* j*)
                        (rec (inc x)
                             (+ moves-count c)
                             (cons ms moves-list))))
                    (rec (inc x)
                         moves-count
                         moves-list))))))))

(defun solve (grid)
  (multiple-value-bind (moves-count moves)
      (random-moves! grid)
    (let ((conns (search-best-conns grid
                                    moves-count
                                    *connection-search-duration*)))
      (values moves-count
              moves
              (length conns)
              conns))))

(defun initialize-vars (n k)
  (setf *indices* (range n)
        *indices^2* (range (* n n))
        *n* n
        *coms-count* (* 100 k)
        *ops-count-limit* (* 100 k)
        *moves-count-limit* (* 40 k)
        *ds* (make-disjoint-set (* *n* *n*))
        *connection-search-duration* 1.7))

(defun read-grid (n)
  (let ((grid (make-array `(,n ,n) :element-type 'int8)))
    (dotimes (i n)
      (let ((line (read-line)))
        (dotimes (j n)
          (awhen (digit-char-p (char line j))
            (setf (aref grid i j) it)))))
    grid))

(defun copy (grid)
  (let ((copy (make-array `(,*n* ,*n*) :element-type 'int8)))
    (domatrix (gij i j grid)
      (setf (aref copy i j)
            (the int8 gij)))
    copy))
  
(defun main ()
  (readlet (n k)
    (initialize-vars n k)
    (multiple-value-bind (x moves y conns)
        (solve (read-grid n))
      (println x)
      (dolist (m moves)
        (join-print m))
      (println y)
      (dolist (c conns)
        (join-print c)))))

#-swank (main)
