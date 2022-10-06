(defmacro define (name closure)
  (assert (symbolp name))
  (sb-ext::once-only ((closure closure))
    `(progn
       (defun ,name (&rest args)
         (apply ,closure args))
       (setf (fdefinition ',name)
             ,closure))))

;; https://stackoverflow.com/questions/15465138/find-functions-arity-in-common-lisp
;; https://sourceforge.net/projects/clocc/
(defun arglist (fn)
  "Return the signature of the function."
  #+allegro (excl:arglist fn)
  #+clisp (sys::arglist fn)
  #+(or cmu scl)
  (let ((f (coerce fn 'function)))
    (typecase f
      (STANDARD-GENERIC-FUNCTION (pcl:generic-function-lambda-list f))
      (EVAL:INTERPRETED-FUNCTION (eval:interpreted-function-arglist f))
      (FUNCTION (values (read-from-string (kernel:%function-arglist f))))))
  #+cormanlisp (ccl:function-lambda-list
                (typecase fn (symbol (fdefinition fn)) (t fn)))
  #+gcl (let ((fn (etypecase fn
                    (symbol fn)
                    (function (si:compiled-function-name fn)))))
          (get fn 'si:debug))
  #+lispworks (lw:function-lambda-list fn)
  #+lucid (lcl:arglist fn)
  #+sbcl (sb-introspect:function-lambda-list fn)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl scl)
  (error 'not-implemented :proc (list 'arglist fn)))

(defmacro portably-without-package-locks (&body body)
  `(#+sbcl sb-ext:without-package-locks
    #+allegro excl::without-package-locks
    #+cmu ext:without-package-locks
    #+lispworks let
    #+lispworks
    ((lw:*handle-warn-on-redefinition* :warn)
                                        ; (dspec:*redefinition-action* :warn)
     (hcl:*packages-for-warn-on-redefinition* nil))
    #+clisp ext:without-package-lock #+clisp ()
    #+ccl let
    #+ccl ((ccl:*warn-if-redefine-kernel* nil))
    #-(or allegro lispworks sbcl clisp cmu ccl)
    progn
    ,@body))

;; depends on rank
(defun coord-compress (list &key (test #'<) (rank-base 0))
  (mapcar (mapper (rank list :test test :rank-base rank-base))
          list))

(defun vector-mapcan (function vector)
  (nconcing (i 0 (length vector))
    (funcall function (aref vector i))))

(defun map-subseq (function sequence start end)
  (let ((copy (copy-seq sequence)))
    (setf (subseq copy start end)
          (funcall function (subseq copy start end)))
  copy))

(defsubst non-nils (list)
  (delete nil list))

(defun lines (string)
  (->> (split #\Newline (coerce-list string)
              :test #'char=
              :omit-nulls t)
       (mapcar (rcurry #'coerce 'string))))

(defun average (list &key key)
  (/ (reduce #'+ list :key key)
     (length list)))

(defsubst random-choice (list)
  (nth (random (length list)) list))

(defmacro on (function key)
  (sb-ext::once-only ((function function) (key key))
    (with-gensyms (args)
      `(lambda (&rest ,args)
         (apply ,function (mapcar ,key ,args))))))

(defmacro on (function &rest keys)
  (sb-ext::once-only ((function function))
    (with-gensyms (x)
      (let ((gkeys (loop repeat (length keys)
                         collect (gensym "KEY"))))
        `(let ,(mapcar (lambda (gk k)
                         `(,gk ,k))
                gkeys keys)
           (lambda (,x)
             (funcall ,function
                      ,@(mapcar (lambda (k)
                                  `(funcall ,k ,x))
                                gkeys))))))))

(defun choose2 (n)
  (if (< n 2)
      0
      (/ (* n (1- n)) 2)))

(defun median (list)
  (assert (consp list))
  (let* ((n (length list))
         (n/2 (floor n 2))
         (sorted (sort list #'<)))
    (if (evenp n)
        (dbind (x y)
            (take 2 (drop (1- n/2) sorted))
          (/ (+ x y) 2))
        (nth n/2 sorted))))

(defmacro once-only (syms &body body)
  (assert (every #'symbolp syms))
  (let ((gs (loop repeat (length syms)
                  collect (gensym "ARG"))))
    `(let ,(mapcar #'list gs syms)
       `(let (,,@(mapcar (lambda (g sym)
                           ``(,,g ,,sym))
                         gs syms))
          ,(let ,(mapcar #'list syms gs)
             ,@body)))))

(defun numberings (sequence &key (test #'eql))
  (let ((c (make-hash-table :test test)))
    (etypecase sequence
      (list (mapcar (lambda (item)
                      (prog1 (gethash item c 0)
                        (incf (gethash item c 0))))
                    sequence))
      (vector (map 'vector (lambda (item)
                             (prog1 (gethash item c 0)
                               (incf (gethash item c 0))))
                   sequence)))))

@mod-choose
(defmemo (mod-fact) (n)
  (if (<= n 1)
      1
      (mod* (mod n +mod+)
            (mod-fact (1- n)))))

(mapc-range #'mod-fact 1 (expt 10 5))

;; depends on mod-inverse
(defun mod-choose (n k)
  (if (or (< n k) (< n 0) (< k 0))
      0
      (mod* (mod-fact n)
            (mod-inverse (mod-fact k) +mod+)
            (mod-inverse (mod-fact (- n k)) +mod+))))
@mod-choose end

(defmacro avalues (&rest values)
  (when values
    `(apply #'values
            ,(foldr (lambda (v acc)
                      (with-gensyms (value)
                        `(let ((,value ,v))
                           (cons ,value
                                 (let ((it ,value))
                                   ,acc)))))
                    `(list ,(last1 values))
                    (butlast values)))))

(defun read-binary (&rest args)
  (values (read-from-string
           (concatenate 'string "#b"
                        (apply #'read-line args)))))

(defun read-bit-vector (&rest args)
  (values (read-from-string
           (concatenate 'string "#*"
                        (apply #'read-line args)))))

;; https://atcoder.jp/contests/arc146/editorial/4634
(defun minimum-superset (x min)
  "Returns Y >= X s.t. (logand X Y) == X."
  #@(fixnum x min)
  (assert (and (integerp x)
               (plusp x)))
  (if (>= x min)
      x
      (range-foldr (lambda (i acc)
                     (+ (2* acc)
                        (if (logbitp i x)
                            1
                            (if (< (1- (ash (1+ (2* acc)) i))
                                   min)
                                1
                                0))))
                   0
                   0 (integer-length min))))

(defun cons-order% (car-key cdr-key &key (order #'<))
  (dlambda ((a . b) (c . d))
    (let ((ka (funcall car-key a))
          (kc (funcall car-key c)))
      (or (funcall order ka kc)
          (and (not (funcall order kc ka))
               (funcall order (funcall cdr-key b) (funcall cdr-key d)))))))

(defun cons-order (car-order cdr-order)
  (dlambda ((a . b) (c . d))
    (or (funcall car-order a c)
        (and (not (funcall car-order c a))
             (funcall cdr-order b d)))))

(defun cons< (cons1 cons2 &key (test #'<))
  (dbind (a . b) cons1
    (dbind (c . d) cons2
      (or (funcall test a c)
          (and (not (funcall test c a))
               (funcall test b d))))))

(defun list< (list1 list2 &key (test #'<))
  (nlet rec ((l1 list1) (l2 list2))
    (cond ((null l2) nil)
          ((null l1) t)
          ((funcall test (car l1) (car l2))
           t)
          ((funcall test (car l2) (car l1))
           nil)
          (t
           (rec (cdr l1) (cdr l2))))))

;; @avltree
;; (defstruct (avltree (:conc-name nil)
;;                     (:constructor avltree
;;                         (key left right)))
;;   key left right)

;; (defun %rotate-left (avltree)
;;   (if (null avltree)
;;       nil
;;       (with-accessors ((k key) (l left) (r right)) avltree
;;         (with-accessors ((lk key) (ll left) (lr right)) l
;;           (avltree lk ll (avltree lr r))))))

;; (defun %rotate-right (avltree)
;;   (if (null avltree)
;;       nil
;;       (with-accessors ((k key) (l left) (r right)) avltree
;;         (with-accessors ((rk key) (rl left) (rr right)) r
;;           (avltree rk (avltree k l rl) rr)))))

;; (defun %double-rotate-right-left (avltree)
;;   (with-accessors ((k key) (l left) (r right)) avltree
;;     (with-accessors ((rk key) (rl left) (rr right)) r
;;       (with-accessors ((rlk key) (rll left) (rlr right)) rl
;;         (avltree rlk
;;                  (avltree k l rll)
;;                  (avltree rk rlr rr))))))

;; (defun %double-rotate-left-right (avltree)
;;   (with-accessors ((k key) (l left) (r right)) avltree
;;     (with-accessors ((lk key) (ll left) (lr right)) l
;;       (with-accessors ((lrk key) (lrl left) (lrr right)) lr
;;         (avltree lrk
;;                  (avltree lk ll lrl)
;;                  (avltree k lrr r))))))

;; (defun %height (avltree)
;;   (if (null avltree)
;;       0
;;       (with-accessors ((l left) (r right)) avltree
;;         (1+ (max (height l) (height r))))))

;; (defun avltree-insert (avltree key &key (order #'<))
;;   (if (null avltree)
;;       (avltree key nil nil)
;;       (with-accessors ((k key) (l left) (r right)) avltree
;;         (if (funcall order key k)
;;             (let ((l* (avltree-insert key l)))
;;               (with-accessors ((lk* key)) l*
;;                 (if (= (- (%height l*) (%height r)) 2)
;;                     (if (funcall order key lk*)
;;                         (%rotate-left (avltree k l* r))
;;                         (%double-rotate-left-right (avltree k l* r)))
;;                     (avltree k l* r))))
;;             (let ((r* (avltree-insert key r)))
;;               (with-accessors ((rk* key)) r*
;;                 (if (= (- (%height r*) (%height l)) 2)
;;                     (if (funcall order rk* key)
;;                         (%rotate-right (avltree k l r*))
;;                         (%double-rotate-right-left (avltree k l r*)))
;;                     (avltree k l r*))))))))

;; (defun avltree-find (avltree key &key (order #'<))
;;   (if (null avltree)
;;       nil
;;       (with-accessors ((k key) (l left) (r right)) avltree
;;         (cond ((funcall order key k)
;;                (avltree-find l key :order order))
;;               ((funcall order k key)
;;                (avltree-find r key :order order))
;;               (t avltree)))))
;; @avltree

(defun triangle-area (p q r)
  (dbind (px . py) p
    (dbind (qx . qy) q
      (dbind (rx . ry) r
        (let ((a (- qx px))
              (b (- qy py))
              (c (- rx px))
              (d (- ry py)))
          (/ (abs (- (* a d) (* b c)))
             2))))))

(defun dff (graph)
  "Returns list of connected compoents by doing dfs."
  (let* ((n (length graph))
         (seen (make-bit-array n)))
    (labels ((dfs (start)
               (setf (aref seen start) 1)
               (nlet rec ((stack (list start))
                          (vs nil)
                          (es nil))
                 (if (null stack)
                     (list vs es)
                     (let ((u (car stack)))
                       (rec (nconc (filter (lambda (v)
                                             (when (zerop (aref seen v))
                                               (setf (aref seen v) 1)))
                                           (aref graph u))
                                   (cdr stack))
                            (cons u vs)
                            (nconc (mapcar (curry #'cons u) (aref graph u))
                                   es)))))))
      (lcomp ((v 0 n))
          (zerop (aref seen v))
        (dfs v)))))

(defun n-cycles (n-vertices n-edges n-connected-components)
  "Returns number of cycles of undirected and simple graph."
  (+ (/ n-edges 2) (- n-vertices) n-connected-components))

(defun count-cycles (graph start &optional (seen (make-bit-array (length graph))))
  "GRAPH must be simple: no self loops and no duplicate edges."
  (nlet rec ((vs `((,start . -1)))
             (c 0))
    (if (null vs)
        c
        (dbind (u . parent) (car vs)
          (if (plusp (aref seen u))
              (rec (cdr vs) (1+ c))
              (progn
                (setf (aref seen u) 1)
                (rec (nconc (filter-map (lambda (v)
                                          (when (and (/= v parent)
                                                     (zerop (aref seen v)))
                                            (cons v u)))
                                        (aref graph u))
                            (cdr vs))
                     c)))))))

(defun intersections (list &rest more-lists)
  (reduce #'intersection
          (cons list more-lists)))

;; depends on dijkstra
(defun johnson-without-reweighting (graph weight)
  (let* ((n (length graph))
         (dus (make-array-with-content ((u n))
                (dijkstra graph u weight))))
    (make-array-with-content ((u n) (v n))
      (aref (aref dus u) v))))

(defun floyd-warshall (n-vertices weight)
  (with-memos ((d (i j k)
                 (if (zerop k)
                     (funcall weight i j)
                     (min (d i j (1- k))
                          (+ (d i k (1- k))
                             (d k j (1- k)))))))
    (rcurry #'d (1- (n-vertices)))))

;; depends on cp/explicit-treap
(defun treap-count (treap)
  (let ((c 0))
    (treap-map (lambda (k v)
                 (declare (ignore k v))
                 (incf c))
               treap)
    c))

;; depends on cp/bisect
(defun longest-monotonic-subsequence (sequence &key key (test #'<))
  (let ((length->last-min (make-adj-array)))
    (map nil (lambda (x)
               (let ((k (ensure-key key x)))
                 (if (zerop (length length->last-min))
                     (vector-push-extend k length->last-min)
                     (let ((len (bisect-left (arefer length->last-min)
                                             k
                                             :end (length length->last-min)
                                             :order test)))
                       (if (>= len (length length->last-min))
                           (vector-push-extend k length->last-min)
                           (setf (aref length->last-min len) k))))))
         sequence)
    (values (length length->last-min)
            length->last-min)))

;; ;; depends on %insert
;; (defun perms (n)
;;   (if (= n 1)
;;       (list (list 1))
;;       (mapcan (lambda (p)
;;                 (map-drange (lambda (pos)
;;                               (%insert p pos n))
;;                             0 n))
;;               (perms (1- n)))))

;; depends on %insert
(defun perms (n)
  (if (zerop n)
      (list nil)
      (mapcan (lambda (p)
                (map-drange (lambda (pos)
                              (%insert p pos (1- n)))
                            0 n))
              (perms (1- n)))))

;; depends on %insert
(defun permutations (list &optional (n (length list)))
  (if (null list)
      (list nil)
      (mapcan (lambda (p)
                (map-drange (lambda (pos)
                              (%insert p pos (car list)))
                            0 n))
              (permutations (cdr list) (1- n)))))

(defun remove-uniques (list &key (test #'eql))
  (filter (compose #>1 (mapper (counter list :test test)))
          list))

(defun longest-common-subsequence (seq1 seq2 &key (test #'eql))
  (let ((n (length seq1))
        (m (length seq2)))
    (with-memos ((dp (i j)
                   (if (funcall test (elt seq1 i) (elt seq2 j))
                       (if (or (zerop i) (zerop j))
                           1
                           (1+ (dp (1- i) (1- j))))
                       (cond ((and (zerop i) (zerop j))
                              0)
                             ((zerop i)
                              (dp i (1- j)))
                             ((zerop j)
                              (dp (1- i) j))
                             (t
                              (max (dp i (1- j)) (dp (1- i) j)))))))
      (values (nlet rec ((i (1- n)) (j (1- m)) (acc nil))
                (if (funcall test (elt seq1 i) (elt seq2 j))
                    (if (or (zerop i) (zerop j))
                        (cons (elt seq1 i) acc)
                        (rec (1- i) (1- j) (cons (elt seq1 i) acc)))
                    (cond ((and (zerop i) (zerop j))
                           (cons (elt seq1 i) acc))
                          ((zerop i)
                           (rec i (1- j) acc))
                          ((zerop j)
                           (rec (1- i) j acc))
                          (t
                           (if (= (dp i j) (dp i (1- j)))
                               (rec i (1- j) acc)
                               (rec (1- i) j acc))))))
              (dp (1- n) (1- m))))))

(defmemo (mod-fib) (n)
  (if (<= n 1)
      1
      (nlet rec ((a 1) (b 1) (c 2))
        (if (= c n)
            (md (+ a b))
            (rec (md (+ a b))
                 a
                 (1+ c))))))

(defun crossp (p q a b)
  "p == (px . py), q == (qx . qy),
   a == (ax . ay), b == (bx . by)
Returns t if line PQ intersects line AB."
  (and (dividep p q a b)
       (dividep a b p q)))

(defun separatep (p q a b)
  "p == (px . py), q == (qx . qy),
   a == (ax . ay), b == (bx . by)
Returns t if line PQ separates point A and point B into
different sign domains."
  (dbind (px . py) p
    (dbind (qx . qy) q
      (labels ((sign (point)
                 (dbind (x . y) point
                   (signum (- (* (- py qy) (- x px))
                              (* (- px qx) (- y py)))))))
        (= (* (sign a) (sign b))
           -1)))))

(defmemo (fact) (n)
  (if (<= n 1)
      1
      (* n (fact (1- n)))))

(defun min-dists (graph start)
  (let* ((n (length graph))
         (d (make-fixnum-array n :initial-element -1))
         (p (make-fixnum-array n :initial-element -1)))
    (labels ((dfs (u dist parent)
               (setf (aref d u) dist)
               (setf (aref p u) parent)
               (dolist (v (aref graph u))
                 (when (= (aref d v) -1)
                   (dfs v (1+ dist) u)))))
      (dfs start 0 -1)
      (values d p))))

(defmemo (subtree-size :key (curry #'take 2) :test #'equal) (graph root &optional parent)
  (1+ (sum (v (aref graph root))
        (if (eql parent v)
            0
            (subtree-size graph v root)))))

(defun choose (n k)
  "Note that (choose 0 0) == 1."
  (if (or (< n k) (< n 0) (< k 0))
      0
      (/ (fact n)
         (fact k)
         (fact (- n k)))))

;; depends on fib-heap
;; https://github.com/kraison/graph-utils/blob/master/fib-heap.lisp
(defun dijkstra (graph start weight)
  (let* ((n (length graph))
         (d (make-array-with-content ((v n))
              (if (= v start) 0 +inf+)))
         (p (make-array n))
         (heap (make-instance 'fib-heap)))
    (mapc-range (lambda (v)
                  (insert heap (svref d v) v))
                0 n)
    (labels ((relax (u v)
               (let ((e (+ (svref d u) (funcall weight u v))))
                 (when (> (svref d v) e)
                   (setf (svref d v) e)
                   (setf (svref p v) u)
                   (decrease-key heap v e)))))
      (nlet rec ()
        (if (empty-p heap)
            (values d p)
            (let ((u (extract-min heap)))
              (mapc (curry #'relax u) (aref graph u))
              (rec)))))))

(defun row-insert (m w from to)
  "Inserts FROM row to TO row of matrix M."
  (let ((row (make-array-with-content ((j w))
               (aref m from j))))
    (cond ((< from to)
           (loop for i from from to (1- to) do
             (dotimes (j w)
               (setf (aref m i j)
                     (aref m (1+ i) j)))))
          ((> from to)
           (loop for i from from downto (1+ to) do
             (dotimes (j w)
               (setf (aref m i j)
                     (aref m (1- i) j)))))
          (t
           (return-from row-insert m)))
    (dotimes (j w m)
      (setf (aref m to j)
            (aref row j)))))

@stack
(defstruct (stack (:constructor make-stack
                      (size &aux (data (make-fixnum-array size)))))
  (data nil :type (or null (simple-array fixnum (*))))
  (pointer 0 :type fixnum)
  (size 0 :type fixnum)
  (count 0 :type fixnum))

(defun stack-full-p (stack)
  (= (stack-size stack)
     (stack-count stack)))

(defun stack-empty-p (stack)
  (zerop (stack-count stack)))

(defun spush (item stack)
  (when (stack-full-p stack)
    (error "spush: full stack"))
  (with-slots (data pointer count) stack
    (setf (aref data pointer) item)
    (incf pointer)
    (incf count)))

(defun spop (stack)
  (when (stack-empty-p stack)
    (error "spop: empty stack"))
  (with-slots (data pointer count) stack
    (prog1 (aref data (1- pointer))
      (decf pointer)
      (decf count))))

(defun shead (stack)
  (assert (not (stack-empty-p stack)))
  (with-slots (data pointer) stack
    (aref data (1- pointer))))
@stack end

@bitset
(defsubst bs-universal (size)
  (1- (ash 1 size)))

(defsubst bs-empty-p (bitset)
  (zerop bitset))

(defsubst bs-member-p (i bitset)
  (logbitp i bitset))

(defun bitset-members (bitset)
  (loop for 2^k = 1 then (ash 2^k 1)
        for i from 0
        while (<= 2^k bitset)
        when (plusp (logand bitset 2^k))
          collect i))

(defsubst bs-add (i bitset)
  (+ bitset
     (logxor bitset (ash 1 i))))

(defsubst bs-remove (i bitset)
  (- bitset
     (logand bitset (ash 1 i))))
@bitset end

@mem-usage
(defun mem-usage ()
  "Memory used (in bytes) by various memory spaces in a Lisp image. Returns a plist."
  #+sbcl (list :dynamic (sb-vm::dynamic-usage)
               :static (sb-kernel::static-space-usage)
               :immobile (sb-kernel::immobile-space-usage))
  #+cmu (list :dynamic (lisp::dynamic-usage)
              :static (lisp::static-space-usage))
  #+ccl (multiple-value-bind (usedbytes static-used staticlib-used frozen-space-size)
            (ccl::%usedbytes)
          (list :dynamic usedbytes
                :static (+ static-used staticlib-used frozen-space-size)))
  #+clisp (multiple-value-bind (used room static)
              (sys::%room)
            (declare (ignore room))
            (list :dynamic used
                  :static static)))

(defun print-usage (usage stream)
  "Prints memory usage plist returned by MEM-USAGE in a human-readable format."
  (format stream "~&~{~{~50<~:(~A~) space usage is:~;~:D bytes.~%~>~}~}"
          `((:dynamic ,(getf usage :dynamic 0))
            (:static ,(getf usage :static 0))
            (:immobile ,(getf usage :immobile 0))))
  usage)

(defun mem-difference (x y)
  "Difference between two mem-usage records"
  (loop
     :for metric in '(:dynamic :static :immobile)
     :for x-value = (getf x metric)
     :when x-value
     :nconc (list metric (- x-value (getf y metric)))))

;; Copied from TRIVIAL-GARBAGE
(defun run-gc (&key full verbose)
  "Initiates a garbage collection. @code{full} forces the collection
   of all generations, when applicable. When @code{verbose} is
   @em{true}, diagnostic information about the collection is printed
   if possible."
  (declare (ignorable verbose full))
  #+(or cmu scl) (ext:gc :verbose verbose :full full)
  #+sbcl (sb-ext:gc :full full)
  #+allegro (excl:gc (not (null full)))
  #+(or abcl clisp) (ext:gc)
  #+ecl (si:gc t)
  #+openmcl (ccl:gc)
  #+corman (ccl:gc (if full 3 0))
  #+lispworks (hcl:gc-generation (if full t 0)))

(defmacro mem-used ((&optional place (stream '*standard-output*)) &body forms)
  "Prints memory allocated by running FORMS. Memory usage is recorded
before and after FORMS are run, and the difference in usage is printed
to STREAM.

Full GC is run before and after running forms to eliminate as many
temporary allocations as possible. Make sure to save a reference to
the objects whose memory usage you want to measure. (If the object you
want to measure is the last one returned, and you use this form in the
REPL, then * and / will refer to these objects, so you don't need to
handle this explicitly.

Does not work well for small allocations. In that case, predefine a
vector of appropriate size, make repeated allocations and insert the
newly allocated objects in that vector.

Returns the values returned by the last form.

If PLACE is non-nil, nothing is printed and instead the raw
values (see MEM-USAGE) are saved in PLACE."
  (let ((start-usage (gensym "START-USAGE-"))
        (diff (gensym "DIFF-")))
    `(progn
       (run-gc :full t)
       (let ((,start-usage (mem-usage)))
         (multiple-value-prog1
             (progn ,@forms)
           (run-gc :full t)
           (let ((,diff (mem-difference (mem-usage) ,start-usage)))
             ,(if place
                  `(setf ,place ,diff)
                  `(print-usage ,diff ,stream))))))))

(mem-used () (main))
@mem-usage end

(defun natural-number-sol (a b c)
  "Returns X, Y that satisfy aX + bY = c, X >= 0, Y >= 0, X, Y <- N"
  (awhen (position-if (lambda (ax)
                        (zerop (mod (- c ax) b)))
                      (iterate (1+ (floor c a)) 0 (adder a)))
    (values it (/ (- c (* a it)) b))))

(defun natural-number-sol (a b c)
  "Returns X, Y that satisfy aX + bY = c, X >= 0, Y >= 0, X, Y <- N"
  (dotimes (x (1+ (floor c a)))
    (mvbind (y r) (floor (- c (* a x)) b)
      (when (zerop r)
        (return-from natural-number-sol
          (values x y))))))

(defun detect-loop (initial-value max-value successor)
  (let ((seen (make-fixnum-array (1+ max-value) :initial-element -1)))
    (setf (aref seen initial-value) 0)
    (nlet rec ((i 1)
               (prev-value initial-value)
               (value (funcall successor initial-value))
               (path (list initial-value)))
      (let ((j (aref seen value)))
        (if (>= j 0)
            (values (- i j)          ; loop length
                    j                ; loop start index
                    (1- i)           ; last index before loop restarts
                    value            ; loop start value
                    prev-value       ; last value before loop restarts
                    (->> (nreverse path)
                         (drop j)
                         (take (- i j))) ; values of loop
                    )
            (progn
              (setf (aref seen value) i)
              (rec (1+ i)
                   value
                   (funcall successor value)
                   (cons value path))))))))

;; more general
(defun detect-loop (initial-value successor &key (test #'eql))
  (let ((seen (make-hash-table :test test)))
    (setf (gethash initial-value seen) 0)
    (nlet rec ((i 1)
               (prev-value initial-value)
               (value (funcall successor initial-value)))
      (aif (gethash value seen)
           (values (- i it)          ; loop length
                   it                ; loop start index
                   (1- i)            ; last index before loop restarts
                   value             ; loop start value
                   prev-value        ; last value before loop restarts
                   )
           (progn
             (setf (gethash value seen) i)
             (rec (1+ i)
                  value
                  (funcall successor value)))))))

(defun shakutori (lower upper initial-value threshold &key (op #'+) (inverse #'-) (key #'identity) (test #'<=))
  (nlet rec ((left lower)
             (right lower)
             (value (funcall op
                             initial-value
                             (funcall key lower)))
             (acc nil))
    (cond ((or (> left upper) (> right upper))
           (nreverse acc))
          ((> left right)
           (rec left
                (1+ right)
                (funcall op
                         value
                         (funcall key right))
                acc))
          ;; (<= left right upper)
          ((funcall test value threshold)
           (if (= right upper)
               (nreverse acc)
               (rec left
                    (1+ right)
                    (funcall op
                             value
                             (funcall key (1+ right)))
                    acc)))
          ;; (> value threshold)
          (t 
           (rec (1+ left)
                right
                (funcall inverse
                         value
                         (funcall key left))
                (if (and (< left right)
                         (<= (funcall inverse
                                      value
                                      (funcall key right))
                             threshold))
                    (acons left (1- right) acc)
                    acc))))))

(defun solve-linear-eqs (a b c d p q)
  "Solve 
ax + by = p
cx + dy = q"
  (let ((det (- (* a d) (* b c))))
    (if (= det 0)
        (values nil nil)
        (values (/ (- (* d p) (* b q))
                   det)
                (/ (- (* a q) (* c p))
                   det)))))

(defun tree-find (item tree &key (test #'eql) (key #'identity))
  (labels ((rec (tree)
             (cond ((funcall test item (funcall key tree))
                    (return-from tree-find tree))
                   ((consp tree)
                    (rec (car tree))
                    (rec (cdr tree))))))
    (rec tree)))

(defun tree-find-if (predicate tree &key (key #'identity))
  (labels ((rec (tree)
             (cond ((funcall predicate (funcall key tree))
                    (return-from tree-find-if tree))
                   ((consp tree)
                    (rec (car tree))
                    (rec (cdr tree))))))
    (rec tree)))

(defun circular-list (list)
  (setf (cdr (last list)) list)
  list)

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

(defmacro timed-loop (seconds &body body)
  (let ((gstart (gensym "START-TIME"))
        (gend (gensym "END-TIME")))
    `(let* ((,gstart (get-internal-real-time))
            (,gend (+ ,gstart
                       (* ,seconds
                          internal-time-units-per-second))))
     (loop while (< (get-internal-real-time) ,gend)
           do ,@body))))

;; https://kira000.hatenadiary.jp/entry/2019/02/23/053917
(defun inversion-number (list > &optional (n (length list)))
  (if (<= n 1)
      (values 0 list)
      (let ((l (ceiling n 2))
            (r (floor n 2)))
        (multiple-value-bind (left right) (split-at l list)
          (multiple-value-bind (acc1 m1) (inversion-number left > l)
            (multiple-value-bind (acc2 m2) (inversion-number right > r)
              (nlet rec ((m1 m1) (l1 l)
                         (m2 m2) (l2 r)
                         (merged nil)
                         (acc (+ acc1 acc2)))
                (cond ((or (null m1) (null m2))
                       (values acc
                               (append (nreverse merged) m1 m2)))
                      ((funcall > (car m1) (car m2))
                       ;; (every (curry* > % (car m2)) m1) == t
                       (rec m1 l1
                            (cdr m2) (1- l2)
                            (cons (car m2) merged)
                            (+ acc l1)))
                      (t
                       ;; (funcall > (car m2) (car m1))
                       (rec (cdr m1) (1- l1)
                            m2 l2
                            (cons (car m1) merged)
                            acc))))))))))

(defun join-with (item list-of-lists)
  (when list-of-lists
    (reduce (lambda (l1 l2) (append l1 (list item) l2))
            list-of-lists)))

;; depends on split
(defun words (string &key (separator #\Space))
  (mapcar (lambda (cs) (coerce cs 'string))
          (split separator (coerce string 'list)
                 :test #'char=
                 :omit-nulls t)))

;; depends on join-with
(defun unwords (strings)
  (coerce (join-with #\Space
                     (mapcar (lambda (s) (coerce s 'list))
                             strings))
          'string))

;; depends on apply-n
(defun zoro (x n-digits &optional (base 10))
  (assert (plusp n-digits))
  (apply-n (1- n-digits)
           (lambda (y)
             (+ (* y base) x))
           x))

(defun mapcars (function &rest lists)
  (nlet rec ((list (car lists))
             (lists (cdr lists))
             (acc nil))
    (cond (list
           (rec (cdr list)
                lists
                (cons (funcall function (car list))
                      acc)))
          (lists
           (rec (car lists)
                (cdr lists)
                acc))
          (t
           (nreverse acc)))))

(defun vector-filter (predicate vector)
  (let ((n (length vector)))
    (nlet rec ((i 0) (acc nil))
      (if (= i n)
          (nreverse acc)
          (rec (1+ i)
               (aif (funcall predicate (svref vector i))
                    (cons it acc)
                    acc))))))

@queue
(defun make-queue ()
  (list () ()))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun checkf (queue)
  (if (null (first queue))
      (list (reverse (second queue))
            ())
      queue))

(defun snoc (queue object)
  (destructuring-bind (f r) queue
    (checkf (list f (cons object r)))))

(defun head (queue)
  (if (null (first queue))
      (error "empty queue")
      (car (first queue))))

(defun tail (queue)
  (if (null (first queue))
      (error "empty queue")
      (destructuring-bind (f r) queue
        (checkf (list (cdr f) r)))))

(defun heads (n queue)
  (nlet rec ((n n) (queue queue) (acc nil))
    (if (or (zerop n) (queue-empty-p queue))
        (values (nreverse acc) queue)
        (rec (1- n)
             (tail queue)
             (cons (head queue) acc)))))

(defun nthtail (n queue)
  (nth-value 1 (heads n queue)))

(defun pop-all (queue)
  (append (first queue)
          (reverse (second queue))))
@queue end

@define-mod-operations
;; depends on cp/mod-inverse
(defmacro define-mod-operations ()
  `(progn
     ,@(mapcar (lambda (operator)
                 `(defun ,(intern (format nil "MOD~A" (string operator)))
                      (x y &optional (mod +mod+))
                    (let ((a (numerator x))
                          (b (denominator x))
                          (c (numerator y))
                          (d (denominator y)))
                      (mod (,operator (* a (mod-inverse b +mod+))
                                      (* c (mod-inverse d +mod+)))
                           mod))))
               '(+ -))))

(define-mod-operations)
@define-mod-operations end

;; depends on cp/synced-sort
(defun synced-sort (list order &rest lists)
  (let ((copies (mapcar (lambda (list)
                          (coerce list 'vector))
                        (cons list lists))))
    (apply #'synced-sort!
           (car copies)
           order
           (cdr copies))
    (values-list (mapcar (lambda (vec)
                           (coerce vec 'list))
                         copies))))

;; depends on rank, mapper
(defun coordinate-compress (list &key (test '<) (index-base 0))
  (mapcar (mapper (rank list :test test :rank-base index-base))
          list))

(defun rank (sequence &key (test #'<) (rank-base 0))
  (let ((map (make-hash-table)))
    (map nil (lambda (item rank)
               (unless (gethash item map)
                 (setf (gethash item map) rank)))
         (sort (copy-seq sequence) test)
         (range rank-base
                (+ rank-base (length sequence))))
    map))

;; depends on take
(defun best-k (k list predicate)
  (take k (sort (copy-list list) predicate)))

;; depends on cp/disjoint-set
(defun ds-roots (disjoint-set)
  (let ((n (length (ds-data disjoint-set))))
    (delete-dups (mapcar (curry #'ds-root disjoint-set)
                         (range n))
                 #'< #'=)))

;; depends on cp/disjoint-set
(defun alist->ds (alist ds-count)
  (let ((ds (make-disjoint-set ds-count)))
    (mapc (dlambda ((u . v))
            (ds-unite! ds u v))
          alist)
    ds))

;; depends on cp/disjoint-set
(defun ds-count (disjoint-set)
  (length (ds-data disjoint-set)))

;; depends on cp/disjoint-set
(defun ds-roots-size>=2 (disjoint-set)
  (let ((n (length (ds-data disjoint-set))))
    (ht-keys
     (counter (filter-map (lambda (i)
                            (when (>= (ds-size disjoint-set i) 2)
                              (ds-root disjoint-set i)))
                          (range n))))))

(defun remove-head (item list &key (test 'eql))
  (cond ((null list) nil)
        ((funcall item (car list))
         (remove-head item (cdr list) :test test))
        (t
         list)))

;; depends on remove-head
(defun remove-tail (item list &key (test 'eql))
  (nreverse (remove-head item (reverse list)
                         :test test)))

(defun all-lower-case-chars ()
  (loop for code from #.(char-code #\a) to #.(char-code #\z)
        collect (code-char code)))

(defun all-upper-case-chars ()
  (loop for code from #.(char-code #\A) to #.(char-code #\Z)
        collect (code-char code)))

(defmacro min-integer (var test &optional (start 0))
  `(loop for ,var from ,start
         when ,test
           return (return ,var)))

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

(defmacro curry (fn &rest args)
  (with-gensyms (%)
    `(lambda (,%)
       (,fn ,@args ,%))))

;; need to rewrite using once-only
;; (defmacro curry (function &rest arguments)
;;   (let ((gargs (gensym)))
;;   `(lambda (&rest ,gargs)
;;      (apply ,function
;;             ,@arguments
;;             ,gargs))))

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

(defun %insert (list index value)
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
  "return random integer r that satisfies A <= r < B."
  (+ (random (- b a)) a))

;; depends on scanr
(defun row-major-index->subscripts (dimensions row-major-index)
  (let ((a (coerce (scanr #'* 1 (copy-list dimensions)) 'vector)))
    (loop for i from 1 below (length a)
          collect (floor (mod row-major-index (svref a (1- i)))
                         (svref a i)))))

;; (define-compiler-macro row-major-index->subscripts (&whole form dimensions row-major-index)
;;   (if (and (eq (car dimensions) 'quote)
;;            (plusp (length (second dimensions)))
;;            (every #'integerp (second dimensions))
;;            (integerp row-major-index))
;;       `(,@(row-major-index->subscripts (second dimensions) row-major-index))
;;       form))

(defun memq (item list)
  (member item list :test 'eq))

(defun replace-nth (list n item)
  (cond ((null list) ())
        ((zerop n)
         (cons item (cdr list)))
        (t
         (cons (car list) (replace-nth (cdr list) (1- n) item)))))

(defun print-matrix (matrix &key (sep " ") (stream t))
  (dotimes (i (array-dimension matrix 0))
    (dotimes (j (array-dimension matrix 1))
      (when (plusp j) (princ sep))
      (princ (aref matrix i j) stream))
    (fresh-line stream))
  (fresh-line stream))

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
      (if (zerop x)
          1
          (nlet rec ((x x) (acc 0))
            (if (zerop x)
                acc
                (rec (floor x base)
                     (1+ acc)))))))

(defun num->digits (num &optional (base 10))
  (if (zerop num)
      (list 0)
      (nreverse 
       (loop for x = num then (floor x base)
             while (plusp x)
             collect (mod x base)))))

(defun digits->num (digits &optional (base 10))
  (reduce (lambda (acc d) (+ (* acc base) d))
          digits
          :initial-value 0))

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

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

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

(defun lower-case-char-index (char)
  (assert (<= #.(char-code #\a) (char-code char) #.(char-code #\z)))
  (- (char-code char) #.(char-code #\a)))

(defun upper-case-char-index (char)
  (assert (<= #.(char-code #\A) (char-code char) #.(char-code #\Z)))
  (- (char-code char) #.(char-code #\A)))

(defmacro for ((var &rest args) &body body)
  (ecase (length args)
    (1 `(map nil (lambda (,var) ,@body)
             ,(car args)))
    (2 `(loop for ,var from ,(first args) below ,(second args)
              do (progn ,@body)))))

;; (defun skip (n list)
;;   (cond ((zeorp n)
;;          list)
;;         ((null list)
;;          nil)
;;         (t (skip (1- n) (cdr list)))))

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

(defun n-subarray (length)
  (/ (* length (1+ length)) 2))

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

;; (defun invert-kth-bit% (k x)
;;   (logxor (ash 1 k) x))

(defun invert-kth-bit (k x)
  (let ((b (if (logbitp k x) 0 1)))
    (dpb b (byte k 1) x)))

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

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

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

(defun ensure-function (sym-or-func)
  (cond ((symbolp sym-or-func) `',sym-or-func)
        ((or (functionp sym-or-func)
             (and (consp sym-or-func) (eq (car sym-or-func) 'lambda)))
         sym-or-func)
        (t (error "~A is not symbol or function" sym-or-func))))

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

;; (defun palindrome-p (s)
;;   (let ((n (length s)))
;;     (loop for i from 0 below (floor n 2)
;;           always (char= (char s i) (char s (- n i 1))))))

(defun palindrome-p (string)
  (equal string (reverse string)))

(defun almost-palindrome-p (string)
  (let* ((k (floor (length string) 2))
         (s (coerce string 'list))
         (left (take k s))
         (right (take k (reverse s)))
         (bs (mapcar #'char/= left right))
         (i (position t bs)))
    (when (= (count t bs) 1)
      i)))

(defun digit-ref (x index &key (base 10) (from-right nil))
  (let ((digits (ceiling (log x base))))
    (if (<= 0 index (1- digits))
        (mod (floor x (expt base
                            (if from-right
                                index
                                (- digits index 1))))
             base)
        (error "Invalid digit index ~A for ~A" index x))))

(defun random1-n (n)
  (1+ (random n)))

(defun randoma-b (a b)
  (+ a (random (1+ (- b a)))))

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

(defun sorted-insert (obj lst &key (test #'<) (key #'identity))
  (if (null lst)
      (list obj)
      (if (funcall test (funcall key obj) (funcall key (car lst)))
          (cons obj lst)
          (cons (car lst) (sorted-insert obj (cdr lst) :test test :key key)))))

(defmacro sorted-push (obj lst &rest args)
  `(setf ,lst
         (sorted-insert ,obj ,lst ,@args)))

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

(defun argmax (function list)
  (if (null list)
      (error "Empty list.")
      (labels ((rec (i list idx elem max)
                 (if (null list)
                     (values idx elem max)
                     (let ((val (funcall function (car list))))
                       (if (> val max)
                           (rec (1+ i) (cdr list) i (car list) val)
                           (rec (1+ i) (cdr list) idx elem max))))))
        (rec 1 (cdr list) 0 (car list) (funcall function (car list))))))

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

;; (defun divisors (n smallest-prime-factors-vector)
;;   (if (= n 1)
;;       (list 1)
;;       (let ((factors-ms (multiset (factors n smallest-prime-factors-vector)
;;                                   #'=)))
;;         (loop for ms in (all-subsets-of-multiset factors-ms)
;;               collect (if (null ms)
;;                           1
;;                           (reduce #'* ms :key (lambda (element)
;;                                                 (expt (car element) (cdr element)))))))))

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

;; depends on expt-mod
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
(defun coordinate-compress% (items)
  (let ((s (sort (copy-list items) #'<))
        (idx 0)
        (ht (make-hash-table)))
    (loop for x in s
          unless (gethash x ht) do
            (setf (gethash x ht) idx)
            (incf idx)
          finally (return ht))))

;; for array
(defun coordinate-compress% (arr)
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
@unionset end

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

;; depends on @queue
(defun bfs (graph start)
  (let* ((n (length graph))
         (d (make-array n :initial-element -1))
         (p (make-array n :initial-element -1)))
    (setf (aref d start) 0)
    (nlet rec ((vs (snoc (make-queue) start)))
      (if (queue-empty-p vs)
          (values d p)
          (let ((u (head vs)))
            (rec (foldl #'snoc (tail vs)
                        (filter (lambda (v)
                                  (when (= (aref d v) -1)
                                    (setf (aref d v) (1+ (aref d u))
                                          (aref p v) u)))
                                (aref graph u)))))))))

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
