(defmacro min-integer (var test &optional (start 0))
  `(loop for ,var from ,start
         when ,test
           return (return ,var)))

(defun palindrome-p (string)
  (equal string (reverse string)))

(defun last1 (sequence)
  (etypecase sequence
    (list (car (last sequence)))
    (vector (when (plusp (length sequence))
              (aref sequence (1- (length sequence)))))))

(defun fold-back (x lower upper)
  "XをLOWERとUPPERの内側に折り返す"
  (cond ((<= lower x upper)
         x)
        ((< x lower)
         (fold-back (- (* 2 lower) x) lower upper))
        (t ; (< upper x)
         (fold-back (- (* 2 upper) x) lower upper))))

;; depends on cp/disjoint-set
(defun mst-kruskal (n-vertices edges weight)
  "EDGES -- alist whose each entry represents edge: (from-index . to-index)"
  (let ((vs (make-disjoint-set n-vertices))
        (es (mapcar (lambda (edge)
                      (cons edge
                            (destructuring-bind (u . v) edge
                              (funcall weight u v))))
                    edges))
        (mst (make-hash-table :test 'equal))
        (cost-sum 0))
    (loop for (edge . cost) in (sort es #'< :key #'cdr) do
      (destructuring-bind (u . v) edge
        (unless (ds-connected-p vs u v)
          (ds-unite! vs u v)
          (setf (gethash edge mst) t)
          (incf cost-sum cost))))
    (values mst cost-sum)))

;; depends on singletonp
(defun set-product (&rest sets)
  (cond ((null sets) nil)
        ((singletonp sets)
         (mapcar #'list (car sets)))
        (t
         (let ((product (apply #'set-product (cdr sets))))
           (mapcan (lambda (e)
                     (mapcar (lambda (p)
                               (cons e p))
                             product))
                   (car sets))))))

;; depends on mappend
(defun interleave (list &rest lists)
  (apply #'mappend #'list list lists))

(defun dist (x y)
  (abs (- x y)))

;; depends on prime-factors, scanl, repeat, singletonp
(defun divisors (prime-factors)
  (assert (consp prime-factors))
  (if (singletonp prime-factors)
      (destructuring-bind (p . d) (car prime-factors)
        (scanl #'* 1 (repeat d p)))
      (mapcan (lambda (divisor)
                (mapcar (lambda (expo)
                          (* expo divisor))
                        (destructuring-bind (p . d) (car prime-factors)
                          (scanl #'* 1 (repeat d p)))))
              (divisors (cdr prime-factors)))))

(defun choices (k list &optional (n (length list)))
  (assert (<= 0 k n))
  (cond ((< 1 k n)
         (let ((item (car list)))
           (append (mapcar (lambda (c) (cons item c))
                           (choices (1- k) (cdr list) (1- n)))
                   (choices k (cdr list) (1- n)))))
        ((= k 1)
         (mapcar #'list list))
        ((= k n)
         (list list))
        (t ; (zerop k)
         nil)))        

@nlet
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
@nlet end

(defun covering-intervals-counts (start end ranges)
  (let ((result (make-array (1+ end) :element-type 'fixnum)))
    (loop for (l . r) in ranges do
      (assert (<= start l r end))
      (incf (aref result l))
      (decf (aref result r)))
    (loop for i from 1 below (length result) do
      (incf (aref result i)
            (aref result (1- i))))
    result))

(defun pad (vector)
  (concatenate 'vector #(0) vector))

(defun md (x)
  (mod x +mod+))

(defun connected-components (graph)
  (let* ((n (length graph))
         (ccs (make-array n :element-type 'fixnum
                            :initial-element -1))
         (index -1))
    (labels ((dfs (u)
               (dolist (v (aref graph u))
                 (when (= (aref ccs v) -1)
                   (setf (aref ccs v) index)
                   (dfs v)))))
      (dotimes (u n)
        (when (= (aref ccs u) -1)
          (incf index)
          (setf (aref ccs u) index)
          (dfs u)))
      (values ccs (1+ index)))))

;; depends on make-list-array
(defun read-graph (n-vertices n-edges &key directed)
  (let ((graph (make-list-array n-vertices)))
    (loop repeat n-edges do
      (readlet (u v)
        (push (1- v) (aref graph (1- u)))
        (unless directed
          (push (1- u) (aref graph (1- v))))))
    graph))

(defun group (n list)
  (labels ((rec (src &optional acc)
             (let ((rest (nthcdr n src)))
               (if (consp rest)
                   (rec rest (cons (subseq src 0 n) acc))
                   (nreverse (cons src acc))))))
    (when list
      (rec list))))

@switch
;; depends on with-gensyms
(defun extract-function-name (spec)
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(defun generate-switch-body (whole object clauses test key &optional default)
  (with-gensyms (value)
    (setf test (extract-function-name test))
    (setf key (extract-function-name key))
    (when (and (consp default)
               (member (first default) '(error cerror)))
      (setf default `(,@default "No keys match in SWITCH. Testing against ~S with ~S."
                      ,value ',test)))
    `(let ((,value (,key ,object)))
      (cond ,@(mapcar (lambda (clause)
                        (if (member (first clause) '(t otherwise))
                            (progn
                              (when default
                                (error "Multiple default clauses or illegal use of a default clause in ~S."
                                       whole))
                              (setf default `(progn ,@(rest clause)))
                              '(()))
                            (destructuring-bind (key-form &body forms) clause
                              `((,test ,value ,key-form)
                                ,@forms))))
                      clauses)
            (t ,default)))))

(defmacro switch (&whole whole (object &key (test 'eql) (key 'identity))
                         &body clauses)
  (generate-switch-body whole object clauses test key))

(defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  (generate-switch-body whole object clauses test key '(error)))

(defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  (generate-switch-body whole object clauses test key '(cerror "Return NIL from CSWITCH.")))
@switch end

(defun swap (list i j)
  (rotatef (nth i list)
           (nth j list))
  list)

@lazy
(defstruct promise
  thunk value forced-p)

(defmacro delay (exp)
  `(make-promise :thunk (lambda () ,exp)))

(defun force (object)
  (if (promise-p object)
      (cond ((promise-forced-p object)
             (promise-value object))
            (t
             (setf (promise-forced-p object) t)
             (setf (promise-value object) 
                   (funcall (promise-thunk object)))))
      object))

(defun lcar (infseq)
  (car infseq))

(defun lcdr (infseq)
  (force (cdr infseq)))

(defmacro lcons (x y)
  `(cons ,x (delay ,y)))

(defun lmap (function &rest infseqs)
  (unless (some #'null infseqs)
    (lcons (apply function (mapcar #'lcar infseqs))
           (apply #'lmap function (mapcar #'lcdr infseqs)))))

(defun lfilter (predicate infseq)
  (if (null infseq)
      nil
      (let ((obj (lcar infseq)))
        (if (funcall predicate obj)
            (lcons obj (lfilter predicate (lcdr infseq)))
            (lfilter predicate (lcdr infseq))))))

(defun ltake (n infseq)
  (labels ((rec (n infseq acc)
             (if (zerop n)
                 (nreverse acc)
                 (rec (1- n) (lcdr infseq) (cons (lcar infseq) acc)))))
    (rec n infseq nil)))

(defun iterate (x successor)
  (let (xs)
    (setf xs (lcons x (lmap successor xs)))))

(defun ints ()
  (iterate 1 #'1+))

(defun primes ()
  (labels ((sieve (p nums)
             (lfilter (lambda (n) (plusp (mod n p))) nums))
           (iter (nums)
             (lcons (lcar nums)
                    (iter (sieve (lcar nums) (lcdr nums))))))
    (iter (lcdr (ints)))))
@lazy end

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

(defun ^2 (x)
  (expt x 2))

;; depends on ^2
(defun distance^2 (point line)
  (destructuring-bind (x . y) point
    (destructuring-bind ((a . b) . (c . d)) line
      (assert (or (/= a c) (/= b d)))
      (let ((l (- d b))
            (m (- a c))
            (n (- (* b c) (* a d))))
        (/ (^2 (+ (* l x) (* m y) n))
           (+ (^2 l) (^2 m)))))))

(defun monotonic-subarrays (vector &key (test #'<))
  (let ((n (length vector)))
    (labels ((rec (i start acc)
               (cond ((= i n)
                      (nreverse (if (= start (1- n))
                                    acc
                                    (acons start (1- n) acc))))
                     ((not start)
                      (rec (1+ i) i acc))
                     ((and start (funcall test
                                          (aref vector (1- i))
                                          (aref vector i)))
                      (rec (1+ i) start acc))
                     (t
                      (rec (1+ i) i
                           (if (= start (1- i))
                               acc
                               (acons start (1- i) acc)))))))
      (rec 0 nil nil))))

(defun applier (fn)
  (lambda (args)
    (apply fn args)))

(defun repeat (n item)
  (make-list n :initial-element item))

(defun length>= (list1 list2)
  (labels ((rec (l1 l2)
             (cond ((null l2) t)
                   ((null l1) nil)
                   (t
                    (rec (cdr l1) (cdr l2))))))
    (rec list1 list2)))

(defun length> (list1 list2)
  (labels ((rec (l1 l2)
             (cond ((null l1) nil)
                   ((null l2) t)
                   (t
                    (rec (cdr l1) (cdr l2))))))
    (rec list1 list2)))

;; depends on length>=
(defun prefixp% (list prefix &key (test 'eql))
  (if (and (consp list) (consp prefix))
      (and (length>= list prefix)
           (loop for x in list
                 for y in prefix
                 always (funcall test x y)))
      (error "LIST and PREFIX must be list whose length >= 1")))

(defun prefixp (sequence prefix &key (test 'eql))
  (let ((i (mismatch prefix sequence :test test)))
    (or (not i)
        (= i (length prefix)))))

(defun suffixp (sequence suffix &key (test 'eql))
  (let ((i (mismatch suffix sequence
                     :from-end t :test test)))
    (or (not i)
        (zerop i))))

;; depends on split-at
(defun splice-replace (new old list &key (test 'eql) count)
  (declare (optimize (speed 3) (debug 1)))
  (let ((old-len (length old))
        (new-rev (reverse new)))
    (labels ((rec (list count acc)
               (cond ((null list)
                      (nreverse acc))
                     ((zerop count)
                      (append (nreverse acc)
                              list))
                     (t
                      (let ((i (search old list :test test)))
                        (if i
                            (multiple-value-bind (left right) (split-at i list)
                              (rec (nthcdr old-len right)
                                   (1- count)
                                   (append new-rev
                                           (nreverse left)
                                           acc)))
                            (append (nreverse acc)
                                    list)))))))
      (rec list
           (or count (length list))
           nil))))

(defun curry (function &rest arguments)
  (lambda (&rest args)
    (multiple-value-call function
      (values-list arguments)
      (values-list args))))

;; need to rewrite using once-only
;; (defmacro curry (function &rest arguments)
;;   (let ((gargs (gensym)))
;;   `(lambda (&rest ,gargs)
;;      (apply ,function
;;             ,@arguments
;;             ,gargs))))

;; depends on take
(defun curry-k (function k &rest arguments)
  (if (< k (length arguments))
      (let ((left (take k arguments))
            (right (nthcdr k arguments)))
        (lambda (kth)
          (multiple-value-call function
            (values-list left)
            kth
            (values-list right))))
      (lambda (kth)
        (multiple-value-call function
          (values-list arguments)
          kth))))

@currystar
(defmacro curry* (function &rest arguments)
  (assert (= 1 (count '% arguments :test 'eq)))
  (let ((g% (gensym)))
  `(lambda (,g%)
     (funcall ,function ,@(subst g% '% arguments)))))
@currystar end

;; depends on combinations
(defun power-set (list)
  (combinations list -1))

(defun combinations (list k &optional (n (length list)))
  "returns power set of LIST when K = -1"
  (cond ((or (< n k) (zerop n) (zerop k))
         (list nil))
        ((= n k)
         (list list))
        (t
         (let ((x (car list)))
           (append (mapcar (lambda (comb) (cons x comb))
                           (combinations (cdr list) (1- k) (1- n)))
                   (combinations (cdr list) k (1- n)))))))

;; 重複を許す
;; depends on repeat
(defun multi-combinations (list k &optional (n (length list)))
  (cond ((or (zerop n) (zerop k))
         (list nil))
        ((= n 1)
         (list (repeat k (car list))))
        (t
         (let ((x (car list)))
           (append (mapcar (lambda (comb) (cons x comb))
                           (multi-combinations list (1- k) n))
                   (multi-combinations (cdr list) k (1- n)))))))

(defun insert (list index value)
  (labels ((rec (list index)
             (if (zerop index)
                 (cons value list)
                 (cons (car list)
                       (rec (cdr list) (1- index))))))
    (rec list index)))

@dayutil
(defvar *days* '(0
                 31 29 31 30 31 30
                 31 31 30 31 30 31))
(defvar *cum-days* (scanl1 #'+ 0 *days*))

(defun tomorrow (month day day-of-week)
  (if (= (nth month *days*) day)
      (values (1+ month) 1 (mod (1+ day-of-week) 7))
      (values month (1+ day) (mod (1+ day-of-week) 7))))

(defun days->day (days)
  (let ((m (position-if (lambda (x) (<= days x))
                        *cum-days*)))
    (values m (- days (nth (1- m) *cum-days*)))))

(defun day->days (m d)
  (+ (nth (1- m) *cum-days*)
     d))

(defun day> (day1 day2)
  (destructuring-bind (m1 . d1) day1
    (destructuring-bind (m2 . d2) day2
      (or (> m1 m2)
          (and (= m1 m2)
               (> d1 d2))))))
@dayutil end

(defun alist->ht (alist &key (test 'eql))
  (let ((ht (make-hash-table :test test)))
    (loop for (k . v) in alist do
      (setf (gethash k ht) v))
    ht))

(defun make-hashset (list &key (test 'eql))
  (let ((ht (make-hash-table :test test)))
    (dolist (item list ht)
      (setf (gethash item ht) t))))

(defun repeat% (n string)
  (if (zerop n)
      ""
      (concatenate 'string
                   string
                   (repeat (1- n) string))))

(defun empty-sequence (sequence)
  (etypecase sequence
    (list nil)
    (string "")
    (vector #())))

;; depends on empty-sequence
(defun repeat-sequence (n sequence)
  (let ((type (etypecase sequence
                (list 'list)
                (string 'string)
                (vector 'vector))))
    (labels ((rec (n)
               (if (zerop n)
                   (empty-sequence sequence)
                   (concatenate type
                                sequence
                                (rec (1- n))))))
      (rec n))))

(defun hand> (hand1 hand2)
  (macrolet ((hands= (h1 h2)
               `(and (char= hand1 ,h1)
                     (char= hand2 ,h2))))
    (or (hands= #\S #\P)
        (hands= #\P #\R)
        (hands= #\R #\S))))

(defun span (predicate list)
  (labels ((rec (list left right)
             (cond ((null list)
                    (values left right))
                   ((funcall predicate (car list))
                    (rec (cdr list)
                         (cons (car list) left)
                         right))
                   (t
                    (rec (cdr list)
                         left
                         (cons (car list) right))))))
    (rec (nreverse list) nil nil)))

(defun matrix-bfs% (i j on-visit)
  (let ((q (make-queue))
        (seen (make-bit-array `(,h ,w))))
    (enqueue (list i j 0) q)
    (setf (aref seen i j) 1)
    (loop until (queue-empty-p q) do
      (destructuring-bind (i j d) (dequeue q)
        (when (blackp i j)
          (return-from bfs d))
        (dotimes (dir 4)
          (let ((k (+ i (nth dir *di*)))
                (l (+ j (nth dir *dj*))))
            (when (and (<= 0 k (1- h))
                       (<= 0 l (1- w))
                       (zerop (aref seen k l)))
              (enqueue (list k l (1+ d)) q))))))))

(defun judge (probability)
  (< (random 1.0) probability))

(defun random-a-b (a b)
  "return random integer r that satisfies A < r < B."
  (+ (random (- b a 1)) a 1))

(defun coerce-vector (object)
  (coerce object 'vector))

(defun split-at (n list)
  (labels ((rec (n list left)
             (if (or (zerop n) (null list))
                 (values (nreverse left) list)
                 (rec (1- n) (cdr list) (cons (car list) left)))))
    (rec n list nil)))

;; depends on split-if
(defun split (item list &key (test 'eql) omit-nulls)
  (split-if (lambda (item2)
              (funcall test item item2))
            list
            :omit-nulls omit-nulls))

(defun split-if (predicate list &key omit-nulls)
  (labels ((rec (rest &optional temp-acc)
             (cond ((null rest)
                    (if temp-acc
                        (list (nreverse temp-acc))
                        nil))
                   ((funcall predicate (car rest))
                    (cond ((eq rest list)
                           (rec (cdr rest) nil))
                          ((and omit-nulls (null temp-acc))
                           (rec (cdr rest) nil))
                          (t
                           (cons (nreverse temp-acc)
                                 (rec (cdr rest) nil)))))
                   (t
                    (rec (cdr rest)
                         (cons (car rest) temp-acc))))))
    (rec list)))

(defun row-major-index->subscripts (dimensions row-major-index)
  (let ((a (coerce (scanr #'* 1 (copy-list dimensions)) 'vector)))
    (loop for i from 1 below (length a)
          collect (floor (mod row-major-index (svref a (1- i)))
                         (svref a i)))))

(defun mappend (function list &rest lists)
  (loop for l in (apply #'mapcar function list lists)
        append l))

@sortf
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mappend (function list)
    (loop for item in list
          append (funcall function item))))

(defmacro sortf (predicate &rest places)
  (let* ((meths (mapcar (lambda (p)
                          (multiple-value-list
                           (get-setf-expansion p)))
                        places))
         (temps (mappend #'third meths)))
    `(let* ,(mapcar #'list
                    (mapcan (lambda (m)
                              (append (first m)
                                      (third m)))
                            meths)
                    (mapcan (lambda (m)
                              (append (second m)
                                      (list (fifth m))))
                            meths))
       ,@(mapcon (lambda (rest)
                   (mapcar
                    (lambda (arg)
                      `(unless (,predicate ,(car rest) ,arg)
                         (rotatef ,(car rest) ,arg)))
                    (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))
@sortf end

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

;; (define-compiler-macro range (&rest args)
;;   (let ((gi (gensym)))
;;     (ecase (length args)
;;       (1 (let ((end (car args)))
;;            `(loop for ,gi below ,end
;;                   collect ,gi)))
;;       (2 (destructuring-bind (start end) args
;;            `(loop for ,gi from ,start below ,end
;;                   collect ,gi)))
;;       (3 (destructuring-bind (start end step) args
;;            `(loop for ,gi from ,start below ,end by ,step
;;                   collect ,gi))))))

@simpler-macros
(defmacro mvbind (vars value-form &body body)
  `(multiple-value-bind ,vars ,value-form
     ,@body))

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression
     ,@body))

(defmacro mvcall (function arg &rest arguments)
  `(multiple-value-call ,funcrion ,arg
     ,@arguments))
@simpler-macros end

(defun memq (item list)
  (member item list :test 'eq))

@dlet
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun singletonp (list)
    (and (consp list) (null (cdr list)))))

(defmacro dlet (destructuring-bindings &body body)
  (assert (consp destructuring-bindings))
  (labels ((rec (bindings)
             (let ((b (car bindings)))
               (if (singletonp bindings)
                   `(destructuring-bind ,(first b) ,(second b)
                      ,@body)
                   `(destructuring-bind ,(first b) ,(second b)
                      ,(rec (cdr bindings)))))))
    (rec destructuring-bindings)))
@dlet end

(defmacro mulf (place factor)
  `(setf ,place
         (* ,place ,factor)))

(defmacro divf (place divisor)
  `(setf ,place
         (/ ,place divisor)))

@comb
(defmacro mulf (place factor)
  `(setf ,place
         (* ,place ,factor)))

(defmacro divf (place divisor)
  `(setf ,place
         (/ ,place ,divisor)))

(defun comb (n k)
  (cond ((and (> n k) (plusp k))
         (let ((k (min k (- n k)))
               (numer 1)
               (denom 1))
           (loop for p from n downto (1+ (- n k))
                 for r from k downto 1
                 do
                    (mulf numer p)
                    (mulf denom r)
                    (let ((d (gcd numer denom)))
                      (divf numer d)
                      (divf denom d)))
           numer))
        ((and (> n k) (<= k 0))
         0)
        ((= n k) 1)
        ((< n k) 0)))
@comb end

@zip
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
@zip end

@zipstar
(defun zip* (&rest lists)
  (when lists
    (apply #'zip-with* #'list lists)))

(defun zip-with* (fn &rest lists)
  (when lists
    (labels ((rec (lists acc)
               (if (every #'null lists)
                   (nreverse acc)
                   (rec (mapcar #'cdr lists)
                        (cons (apply fn (mapcar #'car lists))
                              acc)))))
      (rec lists nil))))
@zipstar end

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
(defun mvscanl (function list initial-value &rest initial-args)
  (let ((result (multiple-value-list
                 (apply #'mvfoldl
                        (lambda (item mvscanl-acc acc &rest args)
                          (let ((list (multiple-value-list (apply function item acc args))))
                            (apply #'values
                                   (cons (car list)
                                         mvscanl-acc)
                                   list)))
                        list
                        (list initial-value)
                        initial-value
                        initial-args))))
    (apply #'values
           (nreverse (car result))
           (cdr result))))

(defun foldl (function initial-value sequence)
  (reduce function sequence :initial-value initial-value))

(defun foldl1 (function sequence)
  (if (zerop (length sequence))
      (error "foldl1: empty sequence")
      (reduce function sequence)))

(defun foldr (function initial-value sequence)
  (reduce function sequence :initial-value initial-value :from-end t))

(defun foldr1 (function sequence)
  (if (zerop (length sequence))
      (error "foldr1: empty sequence")
      (reduce function sequence :from-end t)))

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

(defun scanr (function initial-value list)
  (labels ((rec (list acc)
             (if (null list)
                 acc
                 (rec (cdr list)
                      (cons (funcall function (car list) (car acc))
                            acc)))))
    (rec (nreverse list) (list initial-value))))

(defun scanr1 (function list)
  (if (null list)
      (error "scanr1: empty list")
      (scanr function (car (last list)) (nbutlast list))))

(defun replace-nth (list n item)
  (cond ((null list) ())
        ((zerop n)
         (cons item (cdr list)))
        (t
         (cons (car list) (replace-nth (cdr list) (1- n) item)))))

(defun print-matrix (matrix &optional (sep " "))
  (dotimes (i (array-dimension matrix 0))
    (dotimes (j (array-dimension matrix 1))
      (when (plusp j) (princ sep))
      (princ (aref matrix i j)))
    (fresh-line))
  (fresh-line))

(defun char-matrix->num-matrix (char-matrix &rest init-args)
  (let* ((h (array-dimension char-matrix 0))
         (w (array-dimension char-matrix 1))
         (mat (apply #'make-array
                  (list (array-dimension char-matrix 0)
                        (array-dimension char-matrix 1))
                  init-args)))
    (dotimes (i h)
      (dotimes (j w)
        (setf (aref mat i j)
              (digit-char-p (aref char-matrix i j)))))
    mat))

(defun position-if-all (predicate sequence &key (start 0) end key)
  (let ((pos (position-if predicate sequence
                          :start start
                          :end end
                          :key key)))
    (when pos
      (cons pos (position-if-all predicate sequence
                                 :start (1+ pos)
                                 :end end
                                 :key key)))))

(defun n-digits (x &optional (base 2))
  (assert (>= x 0))
  (if (= base 2)
      (integer-length x)
      (ceiling (log (1+ x) base))))

(defun num->digits (num &optional (base 10))
  (nreverse 
   (loop for x = num then (floor x base)
         while (plusp x)
         collect (mod x base))))

(defun digits->num (digits &optional (base 10))
  (reduce (lambda (acc d) (+ (* acc base) d))
          digits))

(defun row-major-index (i j n-rows)
  (+ (* i n-rows) j))

(defun manhattan (u v w z)
  (+ (abs (- u w))
      (abs (- v z))))

(defmacro delete-when (clauses sequence)
  (labels ((rec (clauses)
             (if (null clauses)
                 sequence
                 (destructuring-bind (test item) (car clauses)
                   `(if ,test
                        (delete ,item ,(rec (cdr clauses)))
                        ,(rec (cdr clauses)))))))
    (rec clauses)))

(defun equiv (p q)
  (or (and p q) (and (not p) (not q))))

@make-array-with-content
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
@make-array-with-content end

(defun position-filter (predicate sequence)
  (let ((pos -1))
    (remove-if-not (lambda (item)
                     (declare (ignore item))
                     (funcall predicate (incf pos)))
                   sequence)))

(defun most-frequent (counter)
  (assert (hash-table-p counter))
  (let ((max-count 0)
        max-keys)
    (loop for k being the hash-key of counter
          for count = (gethash k counter)
          do
             (cond ((> count max-count)
                    (setf max-count count
                          max-keys (list k)))
                   ((= count max-count)
                    (push k max-keys))
                   (t nil)))
    (values (nreverse max-keys) max-count)))

(defmacro counting ((var &rest args) &body body)
  (ecase (length args)
    (1 `(reduce (lambda (acc ,var) (+ acc (if (progn ,@body) 1 0)))
                ,(car args) :initial-value 0))
    (2 `(loop for ,var from ,(first args) below ,(second args)
              count (progn ,@body)))))

@countingstar
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun singletonp (list)
    (and (consp list) (null (cdr list)))))

(defmacro counting* (var-and-args-specs &body body)
  (if (null var-and-args-specs)
      0
      (labels ((rec (specs)
                 (if (singletonp specs)
                     `(counting ,(car specs)
                                (progn ,@body))
                     `(sum ,(car specs)
                           ,(rec (cdr specs))))))
        (rec var-and-args-specs))))
@countingstar end

@collectstar
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun singletonp (list)
    (and (consp list) (null (cdr list)))))

(defmacro collect* (var-and-args-specs &body body)
  (if (null var-and-args-specs)
      nil
      (labels ((rec (specs)
                 (if (singletonp specs)
                     (destructuring-bind (var . args) (car specs)
                       (ecase (length args)
                         (1 `(map 'list (lambda (,var) (progn ,@body)) ,(car args)))
                         (2 `(loop for ,var from ,(first args) below ,(second args)
                                   collect (progn ,@body)))))
                     (destructuring-bind (var . args) (car specs)
                       (ecase (length args)
                         (1 `(loop for ,var in ,(car args)
                                   nconc ,(rec (cdr specs))))
                         (2 `(loop for ,var from ,(first args) below ,(second args)
                                   nconc ,(rec (cdr specs)))))))))
        (rec var-and-args-specs))))
@collectstar end

@define-accumulations
(defmacro define-accumulations ()
  `(progn
     ,@(loop for acc in '(sum maximize minimize thereis always)
             for fn in '(+ max min or and)
             collect
             `(defmacro ,acc ((var &rest args) &body body)
                (ecase (length args)
                  (1 `(reduce ',',fn ,(car args)
                              :key (lambda (,var) ,@body)))
                  (2 `(loop for ,var from ,(first args) below ,(second args)
                            ,',acc (progn ,@body)))))
             collect
             `(defmacro ,(intern (format nil "~A*" acc)) (var-and-args-specs &body body)
                (labels ((rec (specs)
                           (if (null specs)
                               `(progn ,@body)
                               `(,',acc ,(car specs)
                                        ,(rec (cdr specs))))))
                  (rec var-and-args-specs))))))

(define-accumulations)
@define-accumulations end

(defun filter (predicate sequence &key from-end (start 0) end count key)
  (remove-if-not predicate sequence
                 :from-end from-end
                 :start start
                 :end end
                 :count count
                 :key key))

(defun singletonp (list)
  (and (consp list) (null (cdr list))))

@define-array-accumulations
(defmacro define-array-accumulations ()
  `(progn
     ,@(loop for acc in '(sum collect count nconc append some every)
             for wrd in '(sum collect count nconc append thereis always)
             collect
             `(defun ,(intern (format nil "ARRAY-~A" acc)) (array &optional key)
                (loop for i below (array-total-size array)
                      ,wrd (funcall key (row-major-aref array i)))))))

(define-array-accumulations)
@define-array-accumulations end

;; (let ((a #2a((1 2) (3 4))))
;;   (print (array-sum a))
;;   (print (array-collect a))
;;   (print (array-collect a (lambda (x) (expt 2 x)))))

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun apply-n (n f x)
  (if (zerop n)
      x
      (funcall f (apply-n (1- n) f x))))

@heap
(defstruct (heap (:constructor make-heap
                     (value left right)))
  value left right)

(declaim (inline empty-heap-p))
(defun empty-heap-p (heap)
  (null heap))

(defun min-value (heap)
  (heap-value heap))

(defun delete-min (heap)
  (merge-heaps (heap-left heap) (heap-right heap)))

(defun insert (heap value)
  (if (empty-heap-p heap)
      (make-heap value nil nil)
      (merge-heaps heap (make-heap value nil nil))))

(defun merge-heaps (h1 h2)
  (cond ((empty-heap-p h1) h2)
        ((empty-heap-p h2) h1)
        ((<= (heap-value h1) (heap-value h2))
         (join h1 h2))
        (t
         (join h2 h1))))

(defun join (h1 h2)
  (make-heap (heap-value h1)
             (heap-right h1)
             (merge-heaps (heap-left h1) h2)))
@heap end

@lrec
(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))

(defmacro alrec (rec &optional base)
  (let ((gfn (gensym)))
    `(lrec (lambda (it ,gfn)
             (symbol-macrolet ((rec (funcall ,gfn)))
               ,rec))
           ,base)))

(defmacro on-cdrs (rec base &optional lists)
  `(funcall (alrec ,rec (lambda () ,base)) ,@lists))
@lrec end

@readmacro
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
@readmacro end

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

@filter
(setf (symbol-function 'filter)
      #'remove-if-not)
@filter end

(defun ht-keys (hash-table)
  (loop for k being the hash-key of hash-table
        collect k))

(defun ht-values (hash-table)
  (loop for v being the hash-value of hash-table
        collect v))

(defmacro do-hashkeys ((key hash-table &optional result) &body body)
  `(loop for ,key being the hash-key of ,hash-table
         do ,@body
         finally (return ,result)))

(defmacro do-hashvalues ((value hash-table &optional result) &body body)
  `(loop for ,value being the hash-value of ,hash-table
         do ,@body
         finally (return ,result)))

@rectangle-sum
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rectangle->summing-form (array-sym rectangle)
    (let ((gs (loop repeat (length rectangle) collect (gensym))))
      (labels ((rec (i rectangle)
                 (cond ((null rectangle)
                        `(aref ,array-sym ,@gs))
                       ((consp (car rectangle))
                        (destructuring-bind (l r) (car rectangle)
                          `(loop for ,(nth i gs) from ,l below ,r
                                 sum ,(rec (1+ i) (cdr rectangle)))))
                       (t
                        `(let ((,(nth i gs) ,(car rectangle)))
                           ,(rec (1+ i) (cdr rectangle)))))))
        (rec 0 rectangle)))))

(defmacro rectangle-sum (array &rest rectangle)
  (rectangle->summing-form array rectangle))
@rectangle-sum end

(define-modify-macro nreversef () nreverse)

@do-dp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun nested-loops (var-and-range-pairs &rest body)
    (assert (every (lambda (pair)
                     (and (consp pair)
                          (= 3 (length pair))))
                   var-and-range-pairs))
    (if (null var-and-range-pairs)
        `(progn ,@body)
        (destructuring-bind (var start final) (car var-and-range-pairs)
          `(loop for ,var from ,start below ,final do
                 ,(apply #'nested-loops (cdr var-and-range-pairs) body))))))

(defmacro do-dp (var-and-range-pairs init-action action final-action)
  (let ((gdp (gensym)))
    `(let ((,gdp (make-array (list ,@(mapcar (lambda (pair)
                                               `(1+ ,(third pair)))
                                             var-and-range-pairs)))))
       (macrolet ((dp (&rest subscripts) 
                    `(aref ,',gdp ,@subscripts)))
         ,init-action
         ,(apply #'nested-loops var-and-range-pairs (list action))
         ,final-action))))
@do-dp end

(defmacro do-reads (count vars &body body)
  (let ((_ (gensym)))
    `(dotimes (,_ ,count)
       (let ,(mapcar (lambda (v)
                       `(,v (read-fixnum)))
              vars)
         ,@body))))

;; (defun generate-vector (vector indexer combinator &optional (default #'identity))
;;   (let ((result (make-array (length vector))))
;;     (dotimes (i (length vector) result)
;;       (let ((j (funcall indexer i)))
;;         (setf (aref result i)
;;               (if (< -1 j (length vector))
;;                   (funcall combinator
;;                            (aref result j)
;;                            (aref vector i))
;;                   (funcall default (aref vector i))))))))

(defun n-one-bits% (x)
  (loop for i below (integer-length x)
        count (logbitp i x)))

(defun n-one-bits (x)
  (logcount x))

(defun bits (x &optional n-bits)
  (let* ((n-bits (or n-bits (integer-length x)))
         (bits (make-array n-bits :element-type 'bit
                                  :initial-element 0)))
    (loop for y = x then (ash y -1)
          for i below n-bits
          while (plusp y)
          when (oddp y)
            do (setf (aref bits i) 1))
    bits))

(defun upcase-char-index (char)
  (assert (<= 65 (char-code char) 90))
  (- (char-code char) 65))

(defun downcase-char-index (char)
  (assert (<= 97 (char-code char) 122))
  (- (char-code char) 97))

(defmacro mod-acc (fn divisor (var &rest args) &body body)
  (ecase (length args)
    (1 (let ((gd (gensym "DIVISOR"))
             (gacc (gensym "ACC")))
         `(let ((,gd ,divisor))
            (reduce (lambda (,gacc ,var)
                      (mod (,fn ,gacc (progn ,@body)) ,gd))
                    ,(car args)))))
    (2 (let ((gd (gensym "DIVISOR"))
             (gans (gensym "ANSWER")))
         `(let ((,gd ,divisor)
                (,gans 0))
            (loop for ,var from ,(first args) below ,(second args)
                  do (setf ,gans
                           (mod (,fn ,gans
                                     (progn ,@body))
                                ,gd)))
            ,gans)))))

(defmacro for ((var &rest args) &body body)
  (ecase (length args)
    (1 `(map nil (lambda (,var) ,@body)
             ,(car args)))
    (2 `(loop for ,var from ,(first args) below ,(second args)
              do (progn ,@body)))))

(defun skip (n list)
  (cond ((zeorp n)
         list)
        ((null list)
         nil)
        (t (skip (1- n) (cdr list)))))

(defun skip-while (predicate list &key count)
  (if (or (and count (zerop count))
          (null list)
          (not (funcall predicate (car list))))
      list
      (skip-while predicate (cdr list)
                  :count (if count (1- count) nil))))

(defmacro nlet (name letargs &body body)
  `(labels ((,name ,(mapcar (lambda (a) (if (consp a) (first a) a))
                     letargs)
              ,@body))
     (,name ,@(mapcar (lambda (a) (if (consp a) (second a) nil))
                      letargs))))

(defun sorted-union (l1 l2 &key (test #'<))
  "l1 and l2 must be non decreasing."
  (labels ((deldup (list)
             (print list)
             (delete-duplicates list
                                :test (lambda (x y)
                                        (not (funcall test x y)))))
           (rec (l1 l2 acc)
             (cond ((and (null l1) (null l2))
                    (nreverse acc))
                   ((null l1)
                    (nconc (nreverse acc) (deldup l2)))
                   ((null l2)
                    (nconc (nreverse acc) (deldup l1)))
                   ((funcall test (car l1) (car l2))
                    (rec (member-if (lambda (x) (funcall test (car l1) x))
                                    (cdr l1))
                         l2
                         (cons (car l1) acc)))
                   ((funcall test (car l2) (car l1))
                    (rec l1
                         (member-if (lambda (x) (funcall test (car l2) x))
                                    (cdr l2))
                         (cons (car l2) acc)))
                   (t
                    (rec (member-if (lambda (x) (funcall test (car l1) x))
                                    (cdr l1))
                         (member-if (lambda (x) (funcall test (car l1) x))
                                    (cdr l2))
                         (cons (car l1) acc))))))
    (rec l1 l2 nil)))

;; (defmacro nreversef (place)
;;   `(setf ,place
;;  (nreverse ,place)))

(defun shakutori% (n predicate)
  (loop with r = 0
        with l = 0
        for i below n
        do (loop until (funcall predicate l r)
                 do (incf r))
        when (= r n)
          return nil
        sum (- n r)))

(defun shakutori (lower upper predicate)
  (loop for l from lower to upper
        for r = lower then (max l r)
        do
           (loop until (or (> r upper)
                           (funcall predicate l r))
                 do (incf r))
        if (<= r upper)
          collect (cons l r) into result
        else
          return result))

(defun n-subarray (length)
  (/ (* length (1+ length)) 2))

(defun make-list-array (dimensions)
  (make-array dimensions :element-type 'list
                         :initial-element nil))

(defun 01-bfs (n-vertices start indexer one-p successor &key initial-queue initial-d)
  "(funcall one-p curr next) == (if 1 t nil)
(funcall indexer vertex) => index of the vertex"
  (let ((q (or initial-queue (make-queue)))
        (d (or initial-d
               (make-array n-vertices :element-type 'fixnum
                                      :initial-element most-positive-fixnum))))
    (when start
      (enqueue start q)
      (setf (aref d (funcall indexer start)) 0))
    (loop until (queue-empty-p q)
          for curr = (dequeue q)
          for i = (funcall indexer curr)
          do
             (dolist (next (funcall successor curr))
               (let ((j (funcall indexer next))
                     (one-p (funcall one-p curr next)))
                 (let ((dist (if one-p
                                 (1+ (aref d i))
                                 (aref d i))))
                   (when (< dist (aref d j))
                     (setf (aref d j) dist)
                     (if one-p
                         (enqueue next q)
                         (enqueue-front next q)))))))
    d))

(defun make-uint-array (dimensions sup &key set-inf)
  (let ((length (integer-length sup)))
    (let ((bits (find-if (lambda (b) (>= b length)) '(2 4 7 8 15 16 31 32 62 63 64))))
      (if bits
          (let ((inf (ash 1 (1- bits))))
            (values (make-array dimensions :element-type (intern (format nil "UINT~A" bits))
                                           :initial-element (if set-inf inf 0))
                    inf))
          (values (make-array dimensions)
                  nil)))))

(defun make-bit-array (dimensions)
  (make-array dimensions :element-type 'bit :initial-element 0))

(defmacro decfs (&rest places)
  `(progn
     ,@(mapcar (lambda (p) `(decf ,p))
               places)))

(defmacro sortf (place predicate &key key)
  `(setf ,place
         (sort ,place ,predicate :key ,key)))

(define-modify-macro nconcf (&rest lists) nconc)

(define-modify-macro appendf (&rest lists) append)

;; (defmacro sum (&rest numbers)
;;   (if (zerop (length numbers))
;;       0
;;       (let ((gn (gensym)))
;; `(+ ,@(mapcar (lambda (n)
;; `(let ((,gn ,n))
;;    (if ,gn
;;        ,gn
;;        0)))
;;       numbers)))))

(defun bitset-members (bitset)
  (loop for 2^k = 1 then (ash 2^k 1)
        for i from 0
        while (<= 2^k bitset)
        when (plusp (logand bitset 2^k))
          collect i))

(defun invert-kth-bit (k x)
  (logxor (ash 1 k) x))

@profile
;; http://www.sbcl.org/manual/#Profiling
(require :sb-sprof)
(defmacro static-profile (&body body)
  `(progn
     (let* ((n 10000)
            (sb-sprof:*sample-interval* (/ 1 n)))
       (sb-sprof:with-profiling (:max-samples n
                                 :report :flat
                                 :loop nil)
         ,@body))))

(static-profile (main))
@profile end

(set-dispatch-macro-character #\# #\i
                              #'(lambda (stream subchar numarg)
                                  (declare (ignore subchar numarg))
                                  `(declare ,@(mapcar #'(lambda (a)
                                                          `(type integer ,a))
                                                      (read stream t nil t)))))

(defun df (x)
  (coerce x 'double-float))

(defmacro readlet (vars &body body)
  `(let ,(mapcar (lambda (v)
                   `(,v (read-fixnum)))
          vars)
     ,@body))

(defmacro bulk-stdout (&body body)
  `(write-string
    (with-output-to-string (*standard-output* nil :element-type 'base-char)
      ,@body)))

(defun read-char-matrix (height width)
  (let ((mat (make-array `(,height ,width) :element-type 'character)))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref mat i j) (read-char)))
      (read-char)) ; skip #\Newline
    mat))

(defun read-matrix (height width &optional (read #'read-fixnum) (element-type 'fixnum))
  (let ((mat (make-array `(,height ,width) :element-type element-type)))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref mat i j) (funcall read))))
    mat))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
          syms)
     ,@body))

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

@join-print
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym ,(string s))))
          syms)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun true (&rest args)
    (apply (constantly t) args)))

(defmacro join-print (sequence &key (key '#'identity) (test '#'true))
  (with-gensyms (seq printed item)
    `(let* ((,seq ,sequence)
            ,printed
            (index 0))
       (map nil (lambda (,item)
                  (when (funcall ,test ,item)
                    (when ,printed
                      (princ " "))
                    (princ (funcall ,key ,item))
                    (setf ,printed t))
                  (incf index))
            ,seq)
       (fresh-line))))
@join-print end

(defun join (lst)
  (if (null lst)
      ""
      (let ((s (princ-to-string lst)))
        (subseq s 1 (1- (length s))))))

(defun join (vec)
  (if (zerop (length vec))
      ""
      (let ((s (princ-to-string vec)))
        (subseq s 2 (1- (length s))))))

(defun print-hash (ht)
  (loop for key being the hash-key of ht do
    (format t "~A: ~A~%" key (gethash key ht))))

#+sbcl
(defconstant inf
  sb-ext:double-float-positive-infinity)

#+sbcl
(defconstant ninf
  sb-ext:double-float-negative-infinity)

(defmacro benchmark (n exp)
  `(time (prog1 'end (loop repeat ,n do ,exp))))

;; (defmacro readlet (readfn vars &body body)
;;   `(let ,(mapcar #'(lambda (v)
;;      `(,v (funcall ,readfn)))
;;   vars)
;;      ,@body))

(defmacro readlet-strint ((str int) &body body)
  (with-gensyms (line i)
    `(let* ((,line (read-line))
            (,i (position #\  ,line))
            (,str (subseq ,line 0 ,i))
            (,int (parse-integer (subseq ,line (1+ ,i)))))
       ,@body)))

@for
;; start: inclusive
;; stop: exclusive
(defmacro %for ((var start stop) (&key (succ #'1+) (dir :ascending)) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (funcall ,succ ,var))
          (,gstop ,stop))
         ((,(ecase dir
              (:ascending '>=)
              (:descending '<=))
           ,var ,gstop))
       ,@body)))

(defmacro for ((var start stop) &body body)
  `(%for (,var ,start ,stop) nil ,@body))

(defmacro dfor ((var start stop) &body body)
  `(%for (,var ,start ,stop) (:succ #'1- :direction :descending) ,@body))
@for end

;; (defmacro for ((var start stop) &body body)
;;   (let ((gstop (gensym)))
;;     `(do ((,var ,start (1+ ,var))
;;   (,gstop ,stop))
;;  ((> ,var ,gstop))
;;        ,@body)))

;; (defmacro dfor ((var start stop) &body body)
;;   (let ((gstop (gensym)))
;;     `(do ((,var ,start (1- ,var))
;;   (,gstop ,stop))
;;  ((< ,var ,gstop))
;;        ,@body)))

(defmacro dovec ((var vector &optional result) &body body)
  `(loop for ,var across ,vector
         do
         ,@body
         finally (return ,result)))

(defmacro doht ((key value ht) &body body)
  (let ((ght (gensym)))
    `(let ((,ght ,ht))
       (loop for ,key being the hash-key of ,ght
               using (hash-value ,value)
             do ,@body))))

(defmacro domatrix ((element i j matrix) &body body)
  (let ((gmat (gensym)))
    `(let ((,gmat ,matrix))
       (symbol-macrolet ((,element (aref ,gmat ,i ,j))) 
         (dotimes (,i (array-dimension ,gmat 0))
           (dotimes (,j (array-dimension ,gmat 1))
             ,@body))))))

;; (defun true (x)
;;   (declare (ignore x))
;;   t)

;; (defmacro loop-acc (acc-type loop-for-clause &key (test #'true) (key #'identity))
;;   (let ((var (second loop-for-clause)))
;;   `(loop ,@loop-for-clause
;;  when (funcall ,test ,var)
;;    ,acc-type (funcall ,key ,var))))

;; (defmacro sum (loop-for-clause &key (test #'true) (key #'identity))
;;   `(loop-acc sum ,loop-for-clause :test ,test :key ,key))

;; (defmacro collect (loop-for-clause &key (test #'true) (key #'identity))
;;   `(loop-acc collect ,loop-for-clause :test ,test :key ,key))

@loopacc
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun true (x)
    (declare (ignore x))
    t)
  (defun ensure-function (sym-or-func)
    (cond ((symbolp sym-or-func) `',sym-or-func)
          ((or (functionp sym-or-func)
               (and (consp sym-or-func) (eq (car sym-or-func) 'lambda)))
           sym-or-func)
          (t (error "~A is not symbol or function" sym-or-func)))))

(defmacro loop-acc (acc-type loop-for-clause &key (test 'true) (key 'identity))
  (let ((var (second loop-for-clause)))
    `(loop ,@loop-for-clause
           when (funcall ,test ,var)
             ,acc-type (funcall ,key ,var))))

(defmacro sum (loop-for-clause &key (test 'true) (key 'identity))
  `(loop-acc sum ,loop-for-clause :test ,(ensure-function test) :key ,(ensure-function key)))

(defmacro collect (loop-for-clause &key (test 'true) (key 'identity))
  `(loop-acc collect ,loop-for-clause :test ,(ensure-function test) :key ,(ensure-function key)))

;; (ensure-function 'sqrt)

;; (sum (for i from 1 to 10) :key (lambda (x) (* 2 x)))
;; (sum (for i from 1 to 10) :key 'sqrt)

;; (collect (for i from 1 to 10))
;; (collect (for i from 1 to 10 by 2))
;; (collect (for cdr on '(1 2 3 4 5)))
@loopacc end

(defmacro collect (n expr)
  `(loop repeat ,n
         collect ,expr))

@f
(defmacro f (expr) `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))
@f end

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun most (lst &optional (fn #'identity))
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))

(defun least (lst &optional (fn #'identity))
  (most lst (compose #'- fn)))

(defun best (lst fn)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (when (funcall fn obj wins)
            (setf wins obj)))
        wins)))

@map
(defun map0-n (n fn)
  (mapa-b 0 n fn))

(defun map1-n (n fn)
  (mapa-b 1 n fn))

(defun mapa-b (a b fn &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))
@map end

(defun map-> (start fn test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

(defun readvector (&rest args)
  (values (read-from-string
           (concatenate 'string "#("
                        (apply #'read-line args)
                        ")"))))

@readvlet
(defun read-vectors (n-vectors size &rest args)
  (let ((vs (loop repeat n-vectors
                  collect (apply #'make-array size args))))
    (dotimes (i size)
      (dolist (v vs)
        (setf (aref v i) (read-fixnum))))
    (values-list vs)))

(defmacro readvlet ((vectors size &rest args) &body body)
  `(multiple-value-bind ,vectors
       (read-vectors ,(length vectors) ,size ,@args)
     ,@body))
@readvlet end

;; (defun %join (lst)
;;   (if (null lst)
;;       ""
;;       (let ((s (princ-to-string lst)))
;;         (subseq s 1 (1- (length s))))))

;; from cl-str
;; faster than %join
(defun join (separator strings)
  (let ((sep (string separator)))
    (with-output-to-string (out)
      (loop for (s . rest) on strings
            do (write-string s out)
            unless (null rest)
              do (write-string sep out)))))

;; (benchmark 1 (join " " (loop repeat 1000 collect (write-to-string (random 1000)))))
;; (benchmark 1 (%join (loop repeat 1000 collect (write-to-string (random 1000)))))

(defmacro key-incf (place delta key)
  `(setf ,place
         (,key (+ ,place ,delta))))

(defmacro key-decf (place delta key)
  `(setf ,place
         (,key (- ,place ,delta))))

(define-modify-macro minf (&rest more-numbers) min)

(define-modify-macro maxf (&rest more-numbers) max)

(defun palindrome-p (s)
  (let ((n (length s)))
    (loop for i from 0 below n
          always (char= (char s i) (char s (- n i 1))))))

(defun digit-ref (x index &key (base 10) (from-right nil))
  (let ((digits (ceiling (log x base))))
    (if (<= 0 index (1- digits))
        (mod (floor x (expt base
                            (if from-right
                                index
                                (- digits index 1))))
             base)
        (error "Invalid digit index ~A for ~A" index x))))

(defun println (obj)
  (format t "~A~%" obj))

(defun printlns (lst)
  (format t "~{~A~%~}" lst))

(defun random1-n (n)
  (1+ (random n)))

(defun randoma-b (a b)
  (+ a (random (1+ (- b a)))))

(defun head (n lst)
  (if (zerop n)
      nil
      (cons (car lst)
            (head (1- n) (cdr lst)))))

(defun take (n list &key (step 1))
  (if (or (zerop n) (null list))
      nil
      (cons (car list)
            (take (1- n) (nthcdr step list) :step step))))

(defun merge-lists (list1 list2 &key count (order #'<))
  (labels ((rec (acc list1 list2 &optional count)
             (cond ((and count (zerop count))
                    (nreverse acc))
                   ((null list1)
                    (nconc (nreverse acc)
                           (if count
                               (take count list2)
                               list2)))
                   ((null list2)
                    (nconc (nreverse acc)
                           (if count
                               (take count list1)
                               list2)))
                   ((funcall order (car list1) (car list2))
                    (rec (cons (car list1) acc)
                         (cdr list1)
                         list2
                         (if count (1- count) nil)))
                   (t
                    (rec (cons (car list2) acc)
                         list1
                         (cdr list2)
                         (if count (1- count) nil))))))
    (rec nil list1 list2 (when count count))))     

(defun biggest-p (n &rest more-numbers)
  (every (lambda (x) (>= n x))
         more-numbers))

(defun adjacent-pairs (lst)
  (loop for cdr on lst
        when (cdr cdr)
          collect (cons (first cdr) (second cdr))))

(defun substrs (s &optional acc)
  (let ((n (length s)))
    (if (= n 1)
        (nconc (list s) acc)
        (substrs (subseq s 1)
                 (nconc
                  (loop for j from 1 to n
                        collect (subseq s 0 j))
                  (list s)
                  acc)))))

(defun substrs-nodup (s)
  (delete-duplicates
   s :test #'string=))

(defun lst-perms% (lst)
  (labels ((rec (lst &optional (acc (list nil)))
             (if (null lst)
                 acc
                 (rec (cdr lst)
                      (loop for l in acc
                            append (all-insertions (car lst) l))))))
    (when lst
      (rec lst))))

(defun all-insertions (item lst)
  (let ((n (length lst)))
    (labels ((rec (i acc)
               (if (> i n)
                   acc
                   (rec (1+ i)
                        (cons (nconc (head i lst)
                                     (list item)
                                     (nthcdr i lst))
                              acc)))))
      (rec 0 nil))))

(defun partial-nreverse (vec start &optional end)
  (when (and vec (not end))
    (setf end (length vec)))
  (setf end (min end (length vec)))
  (loop for i from start below (+ start (floor (- end start) 2))
        do (rotatef (svref vec i)
                    (svref vec (- (+ start end) i 1))))
  vec)

(defun next-permutation (svec &optional (test #'<))
  (let* ((n (length svec))
         (pivot-idx
           (loop for i from (1- n) downto 1
                 when (funcall test (svref svec (1- i)) (svref svec i))
                   do (return (1- i))
                 finally
                    (return-from next-permutation nil))) 
         (pivot (svref svec pivot-idx))
         (succ-idx
           (do ((i (1- n) (1- i)))
               ((funcall test
                         pivot
                         (svref svec i))
                i))))
    (rotatef (svref svec pivot-idx)
             (svref svec succ-idx))
    (concatenate 'simple-vector
                 (subseq svec 0 (1+ pivot-idx))
                 (nreverse (subseq svec (1+ pivot-idx))))))

;; without vector copy
(defun next-permutation (svec &optional (test #'<))
  (let* ((n (length svec))
         (pivot-idx
           (loop for i from (1- n) downto 1
                 when (funcall test (svref svec (1- i)) (svref svec i))
                   do (return (1- i))
                 finally
                    (return-from next-permutation nil))) 
         (pivot (svref svec pivot-idx))
         (succ-idx
           (do ((i (1- n) (1- i)))
               ((funcall test
                         pivot
                         (svref svec i))
                i))))
    (rotatef (svref svec pivot-idx)
             (svref svec succ-idx))
    (partial-nreverse svec (1+ pivot-idx))))

(defmacro doperms ((var initial-permutation &optional result) &body body)
  `(loop for ,var = ,initial-permutation then (next-permutation ,var)
         while ,var
         do
         ,@body
         finally
            (return ,result)))

(defun cartesian-product-elements (n set)
  (when (>= n 1)
    (labels ((rec (n acc)
               (if (= n 1)
                   acc
                   (rec (1- n)
                        (mapcan (lambda (s)
                                  (mapcar (lambda (e)
                                            (cons e s))
                                          set))
                                acc)))))
      (rec n (mapcar #'list set)))))

(defun all-subsets (set)
  (when (consp set)
    (labels ((rec (rest acc)
               (if (null rest)
                   acc
                   (rec (cdr rest)
                        (mapcan (lambda (subset)
                                  (list (cons (car rest) subset)
                                        subset))
                                acc)))))
      (rec set (list nil)))))

(defun counter (sequence &key (test 'eql) (key 'identity))
  (let ((counter (make-hash-table :size (length sequence) :test test)))
    (map nil (lambda (elem)
               (let ((k (funcall key elem)))
                 (setf (gethash k counter)
                       (1+ (gethash k counter 0)))))
         sequence)
    counter))

(defun sorted-insert (obj lst &key (test #'<) (key #'identity))
  (if (null lst)
      (list obj)
      (if (funcall test (funcall key obj) (funcall key (car lst)))
          (cons obj lst)
          (cons (car lst) (sorted-insert obj (cdr lst) :test test :key key)))))

(defmacro sorted-push (obj lst &rest args)
  `(setf ,lst
         (sorted-insert ,obj ,lst ,@args)))

(defun make-adj-array (&optional (length 0))
  (make-array length :fill-pointer 0 :adjustable t))

(defun sorted-push% (obj adj-array &key (test #'<) (key #'identity))
  (if (zerop (fill-pointer adj-array))
      (progn
        (vector-push-extend obj adj-array)
        adj-array)
      (labels ((insert-position ()
                 (labels ((rec (left right)
                            (if (= (1+ left) right)
                                (if (funcall test (funcall key (aref adj-array left))
                                             (funcall key obj))
                                    right
                                    left)
                                (let ((mid (floor (+ left right) 2)))
                                  (cond ((funcall test
                                                  (funcall key (aref adj-array mid))
                                                  (funcall key obj))
                                         (rec mid right))
                                        ((funcall test
                                                  (funcall key obj)
                                                  (funcall key (aref adj-array mid)))
                                         (rec left mid))
                                        (t mid))))))
                   (rec 0 (fill-pointer adj-array))))
               (insert (i)
                 (let ((n (fill-pointer adj-array)))
                   (assert (<= 0 i n))
                   (if (= i n)
                       (vector-push-extend obj adj-array)
                       (progn       
                         (vector-push-extend (aref adj-array (1- n))
                                             adj-array)
                         (setf (subseq adj-array (1+ i) (1+ n)) (subseq adj-array i n))
                         (setf (aref adj-array i) obj))))))
        (insert (insert-position))
        adj-array)))

(defun all-same-chars (str)
  (if (string= str "")
      t
      (let ((fst (char str 0)))
        (every #'(lambda (c)
                   (char= c fst))
               (coerce str 'list)))))

(defun argmax (list &key (key #'identity))
  (when list
    (labels ((rec (i list idx elem max)
               (if (null list)
                   (values idx elem max)
                   (let ((val (funcall key (car list))))
                     (if (> val max)
                         (rec (1+ i) (cdr list) i (car list) val)
                         (rec (1+ i) (cdr list) idx elem max))))))
      (rec 1 (cdr list) 0 (car list) (funcall key (car list))))))

(defun argmin (list &key (key #'identity))
  (when list
    (multiple-value-bind (idx elem max)
        (argmax list :key (lambda (elem) (- (funcall key elem))))
      (values idx elem (- max)))))

(defmacro min-integer (var predicate &key (start 1) (step 1))
  `(loop for ,var from ,start by ,step
         when ,predicate
           return ,var))

;; generic
(defun mindex (seq key-fn)
  (let ((val (funcall key-fn (elt seq 0)))
        (idx 0))
    (loop for i from 1 below (length seq) do
      (let ((v (funcall key-fn (elt seq i))))
        (when (< v val)
          (setf val v
                idx i))))
    (values idx val)))

(defun mindex (lst key-fn)
  (let ((m (funcall key-fn (car lst)))
        (ret 0))
    (loop for i from 1 below (length lst) do
      (let ((v (funcall key-fn (nth i lst))))
        (when (< v m)
          (setf m v
                ret i))))
    ret))

(defun search-all (sub-sequence1 main-sequence2
                   &key (test #'eql) test-not)
  (let ((n (length sub-sequence1)))
    (labels ((rec (pos count founds)
               (let ((found (search sub-sequence1 main-sequence2
                                    :test test :test-not test-not
                                    :start2 pos)))
                 (if found
                     (rec (+ found n)
                          (1+ count)
                          (cons found founds))
                     (values (nreverse founds) count)))))
      (rec 0 0 nil))))

(defun position-all (item sequence &key (start 0) end key test)
  (unless end
    (setf end (length sequence)))
  (labels ((rec (start acc)
             (let ((i (position item sequence
                                :start start :end end :key key :test test)))
               (if i
                   (rec (1+ i) (cons i acc))
                   (nreverse acc)))))
    (rec start nil)))

(defun position-all (item list &key (start 0) end key test)
  (setf list (subseq list start end))
  (labels ((rec (offset rest acc)
             (let ((i (position item rest :key key :test test)))
               (if i
                   (rec (+ offset i 1)
                        (nthcdr (1+ i) rest)
                        (cons (+ i offset) acc))
                   (nreverse acc)))))
    (rec start list nil)))

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

;; (defun next-element (vector key)
;;   (let* ((n (length vector))
;;          (idx (binary-search
;;                -1
;;                n
;;                (lambda (i) (cond ((= i -1) t)
;;                                  ((= i n) nil)
;;                                  (t (<= (svref vector i) key)))))))
;;     (dbg idx)
;;     (cond ((= idx -1) (values (svref vector 0) 0))
;;           ((= idx (1- n)) (values nil nil))
;;           (t (values (svref vector (1+ idx))
;;                      (1+ idx))))))

(defun previous-element (vector key)
  (let ((n (length vector)))
    (when (plusp n)
      (labels ((rec (left right)
                 (let ((mid (floor (the fixnum (+ left right)) 2))) ;; invariant: (=/ mid -1)
                   (cond ((= (1+ left) right) left)
                         ((< (aref vector mid) key)
                          (rec mid right))
                         (t
                          (rec left mid))))))
        (let ((idx (rec -1 n)))
          (if (= idx -1)
              nil
              (values (aref vector idx) idx)))))))

(defun next-element (vector key)
  (let ((n (length vector)))
    (when (plusp n)
      (labels ((rec (left right)
                 (let ((mid (floor (the fixnum (+ left right)) 2))) ;; invariant: (=/ mid -1)
                   (cond ((= (1+ left) right) right)
                         ((<= (aref vector mid) key)
                          (rec mid right))
                         (t
                          (rec left mid))))))
        (let ((idx (rec -1 n)))
          (if (= idx n)
              nil
              (values (aref vector idx) idx)))))))

(defun n-elements-of-interval (sorted-vector left right)
  (assert (<= left right))
  (let ((left-idx (nth-value 1 (next-element sorted-vector (1- left))))
        (right-idx (nth-value 1 (previous-element sorted-vector (1+ right)))))
    (if (and left-idx right-idx)
        (1+ (- right-idx left-idx))
        0)))

@nthorderstatistic
(defun partition (lst &optional (test #'<))
  (let ((pivot (car (last lst))))
    (labels ((rec (rest left right)
               (cond ((null rest)
                      (values left right))
                     ((funcall test (car rest) pivot)
                      (rec (cdr rest)
                           (cons (car rest) left)
                           right))
                     (t
                      (rec (cdr rest)
                           left
                           (cons (car rest) right))))))
      (multiple-value-bind (left right)
          (rec (butlast lst) nil nil)
        (values left pivot right)))))

(defun randomized-partition (lst &optional (test #'<))
  (let ((len (length lst)))
    (if (<= len 1)
        (partition lst test)
        (let ((cp (copy-list lst)))
          (rotatef (nth (1- len) cp)
                   (nth (random len) cp))
          (partition cp test)))))

(defun randomized-select (n lst &optional (test #'<))
  (if (null lst)
      nil
      (multiple-value-bind (left pivot right)
          (randomized-partition lst test)
        (let ((p-idx (length left)))
          (cond ((= p-idx n) pivot)
                ((< n p-idx)
                 (randomized-select n left test))
                (t
                 (randomized-select (- n p-idx 1)
                                    right test)))))))

(defun nth-order-statistic (n lst &optional (test #'<))
  (randomized-select (1- n) lst test))
@nthorderstatistic end

(defun prime-p (n)
  (when (>= n 2)
    (loop for k from 2 to (sqrt n)
          always (plusp (mod n k)))))

(defun make-smallest-prime-factors-vector (maxnum)
  (let ((vec (make-array (1+ maxnum) :initial-contents
                         (loop for i from 0 to maxnum
                               collect i))))
    (loop for k from 2 to maxnum
          when (= (svref vec k) k) do
            (loop for l from (* k k) to maxnum by k
                  when (= (svref vec l) l) do
                    (setf (svref vec l) k)))
    vec))

(defun factors (n smallest-prime-factors-vector)
  (assert (<= n (1- (length smallest-prime-factors-vector))))
  (when (>= n 2)
    (loop for k = n then (/ k smallest-factor)
          for smallest-factor = (svref smallest-prime-factors-vector k)
          while (> k 1)
          collect smallest-factor)))

(defun primep (n smallest-prime-factors-vector)
  (assert (<= n (1- (length smallest-prime-factors-vector))))
  (= n (svref smallest-prime-factors-vector n)))

(defun prime-factors (n)
  (when (>= n 2)
    (let ((factors (loop for k from 2 to (isqrt n)
                         when (zerop (rem n k))
                           collect
                           (do ((power 0 (1+ power)))
                               ((plusp (rem n k))
                                (cons k power))
                             (setf n (/ n k))))))
      (if (= n 1)
          factors
          (cons (cons n 1) factors)))))

;; (defun multiset (list test)
;;   "all 'same' elements of LIST must be side by side."
;;   (labels ((rec (rest curr count acc)
;;              (cond ((null rest)
;;                     (nreverse (cons (cons curr count) acc)))
;;                    ((funcall test curr (car rest))
;;                     (rec (cdr rest) curr (1+ count) acc))
;;                    (t
;;                     (rec (cdr rest) (car rest) 1
;;                          (cons (cons curr count) acc))))))
;;     (when list
;;       (rec (cdr list) (car list) 1 nil))))

;; generic ver.
(defun multiset (sequence test)
  (when (or (consp sequence) (>= (length sequence) 1))
    (nreverse
     (reduce (lambda (acc next)
               (destructuring-bind (curr . count) (car acc)
                 (if (funcall test curr next)
                     (cons (cons curr (1+ count)) (cdr acc))
                     (cons (cons next 1) acc))))
             (subseq sequence 1)
             :initial-value (list (cons (elt sequence 0) 1))))))

(defun all-subsets-of-multiset (multiset)
  (when (consp multiset)
    (labels ((rec (rest acc)
               (if (null rest)
                   acc
                   (rec (cdr rest)
                        (destructuring-bind (item . count) (car rest)
                          (nconc (loop for c from 1 to count
                                       nconc (mapcar (lambda (subset)
                                                       (cons (cons item c) subset))
                                                     acc))
                                 acc))))))
      (rec (cdr multiset)
           (destructuring-bind (item . count) (car multiset)
             (cons nil
                   (loop for c from 1 to count
                         collect (list (cons item c)))))))))

(defun divisors (n smallest-prime-factors-vector)
  (if (= n 1)
      (list 1)
      (let ((factors-ms (multiset (factors n smallest-prime-factors-vector)
                                  #'=)))
        (loop for ms in (all-subsets-of-multiset factors-ms)
              collect (if (null ms)
                          1
                          (reduce #'* ms :key (lambda (element)
                                                (expt (car element) (cdr element)))))))))

(defun nth-power-p (number n)
  (assert (and (integerp number) (integerp n) (>= n 1)))
  (let ((factor (nth-root number n)))
    (values (numberp factor) factor)))

(defun nth-root (number n)
  (assert (and (integerp number) (integerp n)))
  (cond ((= n 1) number)
        ((and (minusp number) (evenp n))
         nil)
        ((and (= number -1) (oddp n))
         -1)
        ((= number 0) 0)
        ((= number 1) 1)
        (t
         (loop for k from 2 to (isqrt (abs number))
               when (= (abs number) (expt k n))
                 return (if (minusp number) (- k) k)))))

(defun expt-mod (base power mod)
  (labels ((md (x)
             (mod x mod)))
    (let ((ans 1))
      (loop for p = power then (ash p -1)
            for b = base then (md (* b b))
            while (plusp p)
            when (= 1 (logand p 1))
              do (setf ans (md (* ans b))))
      ans)))

(defun choose-mod (n r mod)
  (labels ((md (x)
             (mod x mod))
           (product-inverse-mod (x)
             (expt-mod x (- mod 2) mod))) ;; Felrmat's little theorem
    (if (or (minusp r) (< n r))
        0
        (let ((ans 1))
          (loop for x from n downto (1+ (- n r))
                do (setf ans (md (* ans x))))
          (loop for y from r downto 1
                do (setf ans (md (* ans (product-inverse-mod y)))))
          ans))))

;; 重複組み合わせ
(defun multi-choose-mod (n r mod)
  (choose-mod (+ n r -1) r mod))

;; (defun powmod (base power mod)
;;   (labels ((rec (p &optional (acc 1))
;;              (if (= p 0)
;;                  acc
;;                  (rec (1- p)
;;                       (rem (* acc base) mod)))))
;;     (rec power)))

;; (defun sum-of-arithmetic-sequence (n a1 an)
;;   (* n (/ (+ a1 an) 2)))

(defun sum-of-arithmetic-sequence (n d a1)
  (* n (+ a1 (* (/ (1- n) 2) d))))

(defun max-subarray-sum% (a &optional (key #'identity))
  (labels ((a (i)
             (funcall key (aref a i))))
    (let* ((ans (a 0))
           (sum 0))
      (dotimes (i (length a) ans)
        (setf sum (max (a i) (+ sum (a i))))
        (maxf ans sum)))))

(defun min-subarray-sum (a)
  (- (max-subarray-sum% a #'-)))

;; depends on cp/deque
;; https://stackoverflow.com/questions/32517315/maximal-subarray-with-length-constraint
(defun max-subarray-sum (a &key (max-length (length a)))
  (assert (and (< 0 max-length) (<= max-length (length a))))
  (let ((cum-sum (cumlative-sum-array a))
        (deq (make-deque (length a)))
        (ans (aref a 0)))
    (loop for q from 0 to (length a) do
      (when (and (not (deque-empty-p deq))
                 (> (- q (deque-peek-front deq)) max-length))
        (deque-pop-front deq))
      (loop while (and (not (deque-empty-p deq))
                       (> (svref cum-sum (deque-peek-back deq))
                          (svref cum-sum q)))
            do (deque-pop-back deq))
      (deque-push-back q deq)
      (let ((front (deque-peek-front deq)))
        (when (/= q front)
          (maxf ans
                (- (svref cum-sum q) (svref cum-sum front))))))
    ans))

(defun cumlative-sum-array (a)
  "(length cum-sum) == (1+ (length a)),
(svref cum-sum 0) => 0,
(svref cum-sum k) == (loop for i from 0 below k sum (aref a i))
where (<= 0 k (length a))"
  (let ((cum-sum (make-array (1+ (length a))
                             :element-type 'fixnum
                             :initial-element 0)))
    (loop for i from 1 to (length a) do
      (setf (aref cum-sum i)
            (+ (aref a (1- i)) (aref cum-sum (1- i)))))
    cum-sum))

;; depends on with-gensyms
(defmacro label-subarray-sum ((name vector &optional (plus-fn '+) (minus-fn '-)) &body body)
  (with-gensyms (vec n cum-sum)
    `(let* ((,vec ,vector)
            (,n (length ,vec))
            (,cum-sum (make-array ,n)))
       (dotimes (i ,n)
         (setf (svref ,cum-sum i)
               (if (zerop i)
                   (aref ,vec i)
                   (,plus-fn (aref ,vec i) (svref ,cum-sum (1- i))))))
       (labels ((,(intern (format nil "~A-SUBARRAY-SUM" name)) (start end)
                  (cond ((> start end) 0)
                        ((zerop start)
                         (svref ,cum-sum end))
                        (t
                         (,minus-fn (svref ,cum-sum end)
                                    (svref ,cum-sum (1- start)))))))
         ,@body))))

;; for list
;; return hash-table (more general)
(defun coordinate-compress (items)
  (let ((s (sort (copy-list items) #'<))
        (idx 0)
        (ht (make-hash-table)))
    (loop for x in s
          unless (gethash x ht) do
            (setf (gethash x ht) idx)
            (incf idx)
          finally (return ht))))

;; for array
(defun coordinate-compress (arr)
  (let ((s (sort (copy-seq arr) #'<))
        (idx 0)
        (ht (make-hash-table :test #'eq)))
    (loop for x across s
          unless (gethash x ht) do
            (setf (gethash x ht) idx)
            (incf idx))
    (make-array (length arr)
                :initial-contents
                (loop for x across arr
                      collect
                      (gethash x ht)))))

@unionset
(defstruct (%set-element (:conc-name nil))
  parent rank value)

(defmethod print-object ((object %set-element) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (rank value) object
      (format stream ":rank ~A :value ~A" rank value))))

(defun make-set (x)
  (setf (parent x) x)
  (setf (rank x) 0))

(defun make-set-element (&key parent rank value)
  (let ((e (make-%set-element :parent parent
                              :rank rank
                              :value value)))
    (make-set e)
    e))

(defun same-set-p (x y)
  (eq (find-set x)
      (find-set y)))

(defun union-set (x y)
  (link (find-set x) (find-set y)))

(defun link (x y)
  (if (> (rank x) (rank y))
      (setf (parent y) x)
      (progn
        (setf (parent x) y)
        (when (= (rank x) (rank y))
          (incf (rank y))))))

(defun find-set (x)
  (unless (eq x (parent x))
    (setf (parent x) (find-set (parent x))))
  (parent x))
@unionset

@multisetiterate
;; depend-on: cp/multiset
(defun %iterate (mset key exclusive-search inclusive-search &key exclusive count)
  (when (or (not count) (plusp count))
    (let ((start-key (if exclusive
                         (funcall exclusive-search mset key)
                         (funcall inclusive-search mset key))))
      (when start-key
        (let ((key-count (mset-count mset start-key)))
          (cons (cons start-key (if count (min key-count count) key-count))
                (lambda ()
                  (%iterate mset
                            start-key
                            exclusive-search
                            inclusive-search
                            :exclusive t
                            :count (when count (- count key-count))))))))))

(defun iterate-ascending (mset key &key exclusive count)
  (%iterate mset
            key
            #'mset-bisect-right
            #'mset-bisect-left
            :exclusive exclusive
            :count count))

(defun iterate-descending (mset key &key exclusive count)
  (%iterate mset
            key
            #'mset-bisect-left-1
            #'mset-bisect-right-1
            :exclusive exclusive
            :count count))

(defun iterator-nth (n iterator)
  (when (and (plusp n) iterator)
    (let ((key (caar iterator))
          (count (cdar iterator)))
      (if (<= n count)
          key
          (iterator-nth (- n count) (funcall (cdr iterator)))))))
@multisetiterate end

(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

(defun split-string (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
          :then (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
          :while end))

@matrixproduct
(defun matrix->vector (matrix)
  (destructuring-bind (m n) (array-dimensions matrix)
    (assert (or (= m 1) (= n 1)))
    (let ((vector (make-array (* m n)
                              :element-type (array-element-type matrix))))
      (dotimes (i (* m n))
        (setf (svref vector i)
              (row-major-aref matrix i)))
      vector)))

(defun vector->matrix (vector &optional (direction :column))
  (let ((matrix
          (make-array (ecase direction
                        (:row `(1 ,(length vector)))
                        (:column `(,(length vector) 1)))
                      :element-type (array-element-type vector))))
    (dotimes (i (length vector))
      (setf (row-major-aref matrix i)
            (svref vector i)))
    matrix))

(defun ensure-matrix (P &optional (direction :column))
  (cond ((typep P '(array * (* *)))
         P)
        ((vectorp P)
         (vector->matrix P direction))
        (t
         (error "can't convert ~A into matrix" P))))

(defun matrix-product (A B)
  (assert (= (array-dimension A 1)
             (array-dimension B 0)))
  (let* ((l (array-dimension A 0))
         (m (array-dimension A 1))
         (n (array-dimension B 1))
         (result (make-array `(,l ,n))))
    (dotimes (i l)
      (dotimes (j n)
        (setf (aref result i j)
              (loop for k below m
                    sum (* (aref A i k) (aref B k j))))))
    result))
@matrixproduct end

@matrixinverse
(defun matrix-swap-rows (matrix n i k)
  (dotimes (m n)
    (rotatef (aref matrix i m)
             (aref matrix k m))))

(defun matrix-swap-cols (matrix n j k)
  (dotimes (m n)
    (rotatef (aref matrix m j)
             (aref matrix m k))))

(defun matrix-process (a n k pivot)
  (dotimes (i n)
    (setf (aref a k i)
          (/ (aref a k i) (- pivot))))
  (setf (aref a k k) pivot)
  (dotimes (i n)
    (if (not (= i k))
        (do ((temp (aref a k i))
             (j    0 (1+ j)))
            ((>= j n))
          (if (not (= j k))
              (setf (aref a j i)
                    (+ (* temp (aref a j k))
                        (aref a j i)))))))  
  (dotimes (j n)
    (setf (aref a j k)
          (/ (aref a j k) pivot)))  
  (setf (aref a k k) (/ 1.0 pivot))
  nil)

(defun matrix-subr (matrix n k)
  (if (>= k n)
      1.0
      (let* ((i k)
             (j k)
             (max (abs (aref matrix i j))))
        (do ((i0 k (1+ i0)))
            ((>= i0 n))
          (do ((j0 k (1+ j0)))
              ((>= j0 n))
            (if (> (abs (aref matrix i0 j0)) max)
                (setq i i0 j j0 max (abs (aref matrix i0 j0))))))
        (let ((pivot (aref matrix i j))
              (d     0.0))
          (if (= pivot 0.0)
              0.0
              (progn
                (matrix-swap-rows matrix n i k)
                (matrix-swap-cols matrix n j k)
                (matrix-process matrix n k pivot)
                (setq d (* pivot (matrix-subr matrix n (1+ k))))
                (matrix-swap-rows matrix n j k)
                (matrix-swap-cols matrix n i k)
                d))))))

(defun matrix-inverse (matrix)
  (matrix-subr matrix (array-dimension matrix 0) 0))
@matrixinverse end

;; (defun memoize (fn)
;;   (let ((cache (make-hash-table :test #'equal)))
;;     #'(lambda (&rest args)
;;         (multiple-value-bind (val win) (gethash args cache)
;;           (if win
;;               val
;;               (setf (gethash args cache)
;;                     (apply fn args)))))))

@memo
;; paip ch9
(defun memo (fn &key (key #'first) (test #'eql) name)
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))
@memo end

;; (defun dfs (adjacency-list root)
;;   (labels ((explore (start connected)
;;              (loop for dest in (svref adjacency-list start)
;;                    unless (svref connected dest)
;;                      do (setf (svref connected dest) t)
;;                      and collect dest)))
;;     (let ((connected (make-array (length adjacency-list)
;;                                  :initial-element nil
;;                                  :element-type 'boolean))
;;           (queue (list root))
;;           curr)
;;       (while queue
;;                                         ;(format t "curr=~A, connected=~A, queue=~A~%" curr connected queue)
;;              (setf curr (pop queue))
;;              (setf (svref connected curr) t)
;;              (setf queue
;;                    (nconc queue
;;                           (explore curr
;;                                    connected))))
;;       connected)))

@bfs
(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defconstant inf most-positive-fixnum)

(defun make-queue () (cons nil nil))

(defun empty-queue-p (q)
  (not (car q)))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defstruct (vertex (:print-object
                    (lambda (v out)
                      (format out
                              "#<VERTEX ~A :parent ~A :children ~A :leaf-p ~A :d ~A :value ~A :paths ~A :color ~A :adjs ~A>"
                              (vertex-index v)
                              (when (vertex-parent v) (vertex-index (vertex-parent v)))
                              (when (vertex-children v)
                                (mapcar #'vertex-index (vertex-children v)))
                              (vertex-leaf-p v)
                              (vertex-d v)
                              (vertex-value v)
                              (vertex-paths v)
                              (vertex-color v)
                              (mapcar #'vertex-index (vertex-adj v))))))
  index parent children leaf-p (d inf) value (paths 0) (color :white) adj)

(defun make-vertices (n-vertices)
  (make-array
   n-vertices
   :initial-contents
   (loop for i below n-vertices
         collect (make-vertex :index i))))

(defun make-undirected-link (u v)
  (push u (vertex-adj v))
  (push v (vertex-adj u)))

(defun initialize-vertices (vertices)
  (loop for v across vertices do
    (setf (vertex-d v) inf)
    (setf (vertex-color v) :white)
    (setf (vertex-parent v) nil)))

;; basic bfs
(defun bfs (vertices start-idx &key on-visit on-exit)
  (assert (<= 0 start-idx (1- (length vertices))))
  (initialize-vertices vertices)
  (let ((q (make-queue))
        (start (aref vertices start-idx)))
    (setf (vertex-d start) 0)
    (setf (vertex-color start) :gray)
    (enqueue start q)
    (until (empty-queue-p q)
           (let ((u (dequeue q))
                 (leaf-p t))
             (when on-visit
               (funcall on-visit u))
             (dolist (v (vertex-adj u))
               (when (eq (vertex-color v) :white)
                 (setf (vertex-d v) (1+ (vertex-d u)))
                 (setf (vertex-color v) :gray)
                 (setf (vertex-parent v) u)
                 (push v (vertex-children u))
                 (setf leaf-p nil)
                 (enqueue v q)))
             (setf (vertex-color u) :black)
             (setf (vertex-leaf-p u) leaf-p)
             (when on-exit
               (funcall on-exit u))))
    vertices))
@bfs end

;; computes also number of shortest paths from start
(defun bfs (vertices start)
  (assert (find start vertices :test #'eq))
                                        ;  (format t "vertices=~A~%" vertices)
  (let ((q (make-queue)))
    (setf (vertex-d start) 0)
    (setf (vertex-paths start) 1)
    (setf (vertex-color start) :gray)
    (enqueue start q)
    (until (empty-queue-p q)
                                        ;      (format t "queue=~A~%" (car q))
           (let ((u (dequeue q)))
             (dolist (v (vertex-adj u))
               (case (vertex-color v)
                 (:white
                  (setf (vertex-d v) (1+ (vertex-d u)))
                  (setf (vertex-paths v) (vertex-paths u))
                  (setf (vertex-color v) :gray)
                  (setf (vertex-parent v) u)
                  (enqueue v q))
                 (:gray
                  (when (= (vertex-d v) (1+ (vertex-d u)))
                    (incf (vertex-paths v) (vertex-paths u))))))
             (setf (vertex-color u) :black)))
    vertices))

;; depends on cp/queue
;; depends on make-adj-array
(defun bfs (graph start)
  (let ((q (make-queue))
        (seen (make-bit-array (length graph))))
    (setf (aref seen start) 1)
    (enqueue start q)
    (loop until (queue-empty-p q)
          do
             (let ((u (dequeue q)))
               (dolist (v (aref graph u))
                 (when (zerop (aref seen v))
                   (setf (aref seen v) 1)
                   (enqueue v q)))))
    seen))

(defun paint-graph-in-two-colors (adjacency-list start)
  (let ((verts (bfs adjacency-list 0)))
    (loop for v across verts do
      (setf (vertex-color v)
            (if (evenp (vertex-value v))
                0
                1)))
    verts))

@dfs
(defstruct (vertex (:conc-name nil)
                   (:print-object
                    (lambda (v out)
                      (format out
                              "#<VERTEX ~A adjacents: ~A>"
                              (index v)
                              (mapcar #'index (adjacents v))))))
  adjacents index (color :white))

(defun make-undirected-link (u v)
  (push u (adjacents v))
  (push v (adjacents u)))

(defun has-cycle-p (start)
  (labels ((visit (v prev)
             (setf (color v) :gray)
             (dolist (u (adjacents v))
               (cond ((eq (color u) :white)
                      (visit u v))
                     ((and (eq (color u) :gray)
                           (not (eq u prev)))
                      (return-from has-cycle-p t))))
             (setf (color v) :black)))
    (visit start nil)
    nil))
@dfs end

@avltree
(defconstant inf most-positive-fixnum)

(defconstant ninf most-negative-fixnum)

(defclass avl-tree ()
  ((key
    :initarg :node-key
    :reader node-key)
   (value
    :initarg :node-value
    :reader node-value)
   (left
    :initarg :left-child
    :reader left-child)
   (right
    :initarg :right-child
    :reader right-child)))

(defun make-node (key value left right)
  (make-instance
   'avl-tree :node-key key :node-value value
   :left-child left :right-child right))

(defmethod tree-height ((tree null))
  0)

(defmethod tree-height ((tree avl-tree))
  (1+ (max (tree-height (left-child tree))
           (tree-height (right-child tree)))))

(defun balance-factor (node)
  (ecase (- (tree-height (right-child node))
             (tree-height (left-child node)))
    (-2 :imbalanced-left)
    (-1 :left-heavy)
    ( 0 :balanced)
    (+1 :right-heavy)
    (+2 :imbalanced-right)))

(defmethod rotate-left ((node avl-tree))
  (with-slots (key value left right) node
    (avl-node
     (node-key right)
     (node-value right)
     (avl-node key value
               left (left-child right))
     (right-child right))))

(defmethod rotate-right ((node avl-tree))
  (with-slots (key value left right) node
    (avl-node
     (node-key left)
     (node-value left)
     (left-child left)
     (avl-node key value
               (right-child left) right))))

(defun avl-node (key value &optional left right)
  (let ((node (make-node key value left right)))
    (ecase (balance-factor node)
      ((:left-heavy :balanced :right-heavy)
       node)
      (:imbalanced-left
       (ecase (balance-factor left)
         (:left-heavy
          (rotate-right node))
         (:right-heavy
          (avl-node key value
                    (rotate-left left) right))))
      (:imbalanced-right
       (ecase (balance-factor right)
         (:left-heavy
          (avl-node key value
                    left (rotate-left right)))
         (:right-heavy
          (rotate-left node)))))))

(defmethod lessp ((a number) &rest rest)
  (apply #'< a rest))

(defmethod insert (key value (tree null))
  (avl-node key value nil nil))

(defmethod insert (key value (tree avl-tree))
  (avl-node
   (node-key tree)
   (node-value tree)
   (if (lessp key (node-key tree))
       (insert key value
               (left-child tree))
       (left-child tree))
   (if (lessp key (node-key tree))
       (right-child tree)
       (insert key value
               (right-child tree)))))

(defmethod lookup (key (tree null))
  nil)

(defmethod lookup (key (tree avl-tree))
  (with-slots ((node-key key) value left right) tree
    (cond ((lessp key node-key) (lookup key left))
          ((lessp node-key key) (lookup key right))
          (t (cons value
                   (append (lookup key left)
                           (lookup key right)))))))

(defmethod minimum ((tree null))
  nil)

(defmethod minimum ((tree avl-tree))
  (if (left-child tree)
      (minimum (left-child tree))
      tree))

(defmethod maximum ((tree null))
  nil)

(defmethod maximum ((tree avl-tree))
  (if (right-child tree)
      (maximum (right-child tree))
      tree))
@avltree end

@dijkstra
;; dijkstra
;; :depends-on fib-heap
;; https://github.com/kraison/graph-utils/blob/master/fib-heap.lisp

(defconstant inf most-positive-fixnum)

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defstruct (vertex (:print-object
                    (lambda (v out)
                      (format out
                              "#<VERTEX ~A :parent ~A :d ~A :paths ~A :color ~A :adjs ~A>"
                              (vertex-index v)
                              (when (vertex-parent v) (vertex-index (vertex-parent v)))
                              (vertex-d v)
                              (vertex-paths v)
                              (vertex-color v)
                              (mapcar #'vertex-index (vertex-adj v))))))
  index parent (d inf) (paths 0) (color :white) adj)

(defun make-vertices (n-vertices)
  (make-array
   n-vertices
   :initial-contents
   (loop for i below n-vertices
         collect (make-vertex :index i))))

(defun make-undirected-link (u v)
  (push u (vertex-adj v))
  (push v (vertex-adj u)))

(defun initialize-single-source (vertices start-idx)
  (assert (<= 0 start-idx (1- (length vertices))))
  (loop for v across vertices do
    (setf (vertex-d v) inf)
    (setf (vertex-parent v) nil))
  (setf (vertex-d (aref vertices start-idx)) 0))

(defun dijkstra (vertices start-idx weight)
  (initialize-single-source vertices start-idx)
  (let ((heap (make-instance 'fib-heap)))
    (labels ((relax (u v)
               (let ((value (+ (vertex-d u) (funcall weight u v))))
                 (when (> (vertex-d v) value)
                   (setf (vertex-d v) value)
                   (decrease-key heap v value)
                   (setf (vertex-parent v) u)))))
      (loop for v across vertices do
        (insert heap (vertex-d v) v))
      (until (empty-p heap)
             (let ((u (extract-min heap)))
               (dolist (v (vertex-adj u))
                 (relax u v)))))))

(defun bellman-ford (vertices start-idx weight)
  (initialize-single-source vertices start-idx)
  (let ((edges (loop for u in vertices
                     nconc (loop for v in (vertex-adj u)
                                 collect (cons u v)))))
    (labels ((relax (u v)
               (let ((value (+ (vertex-d u) (funcall weight u v))))
                 (when (> (vertex-d v) value)
                   (setf (vertex-d v) value)
                   (setf (vertex-parent v) u)))))
      (dotimes (_ (1- (length vertices)))
        (loop for (u . v) in edges do
          (relax u v)))
      (loop for (u . v) in edges
            always (<= (vertex-d v) (+ (vertex-d u) (funcall weight u v)))))))

(defun johnson-without-reweighting (vertices weight)
  (let* ((n (length vertices))
         (distance (make-array `(,n ,n))))
    (dotimes (i n)
      (dijkstra vertices i weight)
      (loop for v across vertices
            for j below n do
              (setf (aref distance i j) (vertex-d v))))
    distance))      
@dijkstra end
