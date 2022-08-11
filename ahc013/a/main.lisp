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

(defun scanr (function initial-value list)
  (labels ((rec (list acc)
             (if (null list)
                 acc
                 (rec (cdr list)
                      (cons (funcall function (car list) (car acc))
                            acc)))))
    (rec (nreverse list) (list initial-value))))

(defun row-major-index->subscripts (dimensions row-major-index)
  (let ((a (coerce (scanr #'* 1 (copy-list dimensions)) 'vector)))
    (loop for i from 1 below (length a)
          collect (floor (mod row-major-index (svref a (1- i)))
                         (svref a i)))))

(defun row-major-index (i j n-cols)
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

(defun apply-n (n f x)
  (if (zerop n)
      x
      (funcall f (apply-n (1- n) f x))))

(defun applier (fn)
  (lambda (args)
    (apply fn args)))

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

;;;
;;; Body
;;;

(defvar *indices*)
(defvar *indices^2*)
(defvar *n*)
(defvar *ops-count-limit*)
(defvar *random-reposition-moves-count*)
(defvar *moves-count-limit*)
(defvar *ds*)
(defvar *best-conns-tries-count*)
(defvar *search-width*)
(defvar *beam-search-width*)

(defconstant +cable+ -1)
(defconstant +blank+ 0)

(defsubst make-ds ()
  (ds-clear *ds*))
   
;;; Predicates

(defsubst cablep (grid i j)
  (= (aref grid i j) +cable+))

(defsubst blankp (grid i j)
  (= (aref grid i j) +blank+))

(defsubst comp (grid i j)
  (when (plusp (aref grid i j))
    (aref grid i j)))

;;; Random moves

(defsubst make-move (i j k l)
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

(defsubst row-coms (grid row)
  (filter (curry* #'comp grid row %)
          *indices*))

(defsubst col-coms (grid col)
  (filter (curry* #'comp grid % col)
          *indices*))

(defun random-row-reposition (grid row)
  (let ((js (row-coms grid row)))
    (if (null js)
        (values nil 0)
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
    (if (null is)
        (values nil 0)
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
  (let ((copy (copy grid))
        (conns (-> (conns grid) coerce-vector nshuffle coerce-list)))
    (filter-map (curry #'try-connect copy)
                conns)))

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
    (let ((roots (counter (mapcar (curry #'ds-root ds)
                                  *indices^2*))))
      (reduce #'+ (ht-keys roots)
              :key (compose #'cpower (curry #'ds-size ds))))))

(defun search-best-conns (grid moves-count)
  (let* ((copy (copy grid))
         (list-of-conns (collect *best-conns-tries-count*
                          (take (- *ops-count-limit* moves-count)
                                (random-connect copy)))))
    (best #'conns-cost list-of-conns)))

;;; Main

(defun random-repositions (grid)
  (nlet rec ((moves-list nil) (count 0))
    (if (>= count *random-reposition-moves-count*)
        (values (apply #'nconc (nreverse moves-list))
                count)
        (multiple-value-bind (ms c)
            (random-reposition grid)
          (rec (cons ms moves-list)
               (+ count c))))))

(defstruct (state (:constructor state
                      (cost grid moves-list moves-count conns)))
  (cost 0 :type fixnum)
  (grid nil :type (or null (simple-array int8 (* *))))
  (moves-list nil :type list)
  (moves-count 0 :type uint16)
  (conns nil :type list))

(defun search-and-select-best (state)
  (with-slots (grid moves-list moves-count) state
    (let ((states (collect *search-width*
                    (let ((copy (copy grid)))
                      (multiple-value-bind (ms c)
                          (random-repositions grid)
                        (multiple-value-bind (conns cost)
                            (search-best-conns grid (+ moves-count c))
                          (state cost
                                 grid
                                 (cons ms moves-list)
                                 (+ moves-count c)
                                 conns)))))))
      (best #'state-cost states))))
    
(defun initialize-states (grid)
  (collect *beam-search-width*
    (state 0
           (copy grid)
           nil
           0
           nil)))

(defun beam-search (states)
  (apply-n (floor *moves-count-limit*
                  *random-reposition-moves-count*)
           (curry #'mapcar #'search-and-select-best)
           states))

(defun formatize (state)
  (with-slots (moves-list moves-count conns) state
    (values moves-count
            (apply #'nconc (nreverse moves-list))
            (length conns)
            conns)))

(defun solve (n k grid)
  (declare (ignore n k))
  (-> (initialize-states grid)
      beam-search
      (sort #'> :key #'state-cost)
      car
      formatize))

(defun setup-vars (n k)
  (setf *indices* (range n)
        *indices^2* (range (* n n))
        *n* n
        *ops-count-limit* (* k 100)
        *random-reposition-moves-count* k
        *moves-count-limit* (/ (* k 100) 10)
        *ds* (make-disjoint-set (* *n* *n*))
        *best-conns-tries-count* 20
        *search-width* 5
        *beam-search-width* 10))

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
