(in-package :cl-user)

#-swank
(unless (member :child-sbcl *features*)
  (quit
   :recklessly-p t
   :unix-status
   (process-exit-code
    (run-program *runtime-pathname*
                 `("--control-stack-size" "256MB"
;;                   "--dynamic-space-size" "64MB"
                   "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                   "--eval" "(push :child-sbcl *features*)"
                   "--script" ,(namestring *load-pathname*))
                 :output t :error t :input t))))

;;; BEGIN_INSERTED_CONTENTS

;;;; MINHEAP is by Stephan Frank <defclass@googlemail.com>, 2007-2012.
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;;  Modified by Keivn Raison to easier lookup of nodes in heap.

(defpackage :fib-heap (:use :cl)
            (:export
             #:fib-heap
             #:clear-heap
             #:empty-p
             #:insert
             #:peek-min
             #:extract-min
             #:extract-node
             #:heap-size
             #:decrease-key
             #:meld
             #:lookup-node
             ))

(in-package :fib-heap)

;;;; Fibonacci heap based on CLRS, chapter 21

                                        ;(declaim (inline link consolidate splice-lists cut cascading-cut))

(defstruct (node (:constructor %make-node (key data)))
  (key 0 :type fixnum)
  (data nil)
  (parent nil :type (or null node))
  (child nil :type (or null node))
  (left nil :type (or null node))
  (right nil :type (or null node))
  (degree 0 :type (integer 0 #.(ceiling (log most-positive-fixnum
                                             (/ (1+ (sqrt 5)) 2)))))
  (mark nil :type boolean))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~4I~:_key: ~A data: ~A degree: ~A mark: ~A~:_"
            (node-key obj) (node-data obj) (node-degree obj) (node-mark obj))))

                                        ;(declaim (inline %make-node make-node))

(defun make-node (key data)
  "Return a new heap node with KEY and DATA as key/data items and set
the cycle list accordingly."
  (let ((node (%make-node key data)))
    (setf (node-right node) node
          (node-left node) node)
    node))

(defun splice-lists (list-a list-b)
  "Splice two circular lists together"
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (cond
    ((null list-a) list-b)
    ((null list-b) list-a)
    (t (let ((a-pred (node-left list-a))
             (b-tail (node-left list-b)))
         (setf (node-left list-a) b-tail
               (node-right b-tail) list-a
               (node-left list-b) a-pred
               (node-right a-pred) list-b)
         list-a))))


(defclass fib-heap ()
  ((min :accessor min-node
        :type (or null node)
        :initform nil)
   (node-table :accessor node-table
               :initform (make-hash-table))
   (nodes :accessor heap-size
          :type (integer 0 #.most-positive-fixnum)
          :initform 0)))

(defmethod print-object ((obj fib-heap) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~4I~:_size: ~A~:_" (heap-size obj))))

(defun clear-heap (heap)
  (setf (min-node heap) nil
        (heap-size heap) 0)
  (clrhash (node-table heap))
  heap)

(defun cut (heap x y)
  "Remove X from the child list of Y."
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type node x y))
  (let ((min (min-node heap)))
    (setf (node-right (node-left x)) (node-right x)
          (node-left (node-right x)) (node-left x))
    (if (zerop (the fixnum (decf (node-degree y))))
        (setf (node-child y) nil)
        (when (eq (node-child y) x)
          (setf (node-child y) (node-right x))))
    (setf (node-right x) min
          (node-left x) (node-left min)
          (node-left min) x
          (node-right (node-left x)) x
          (node-parent x) nil
          (node-mark x) nil)))


(defun cascading-cut (heap cnode)
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type node cnode))
  (loop for node = cnode then parent
     for parent = (node-parent node)
     while parent do (if (node-mark node)
                         (cut heap node parent)
                         (progn
                           (setf (node-mark node) t)
                           (loop-finish)))))

(defun link (y x)
  "Make node Y a child of node X."
  (declare (optimize (speed 3) (space 0) (debug 0)))
                                        ;(assert (not (eq x y)))
  (setf (node-right (node-left y)) (node-right y)
        (node-left (node-right y)) (node-left y)
        (node-left y) y
        (node-right y) y
        (node-child x) (splice-lists (node-child x) y)
        (node-parent y) x
        (node-mark y) nil)
  (incf (node-degree x)))

(defun consolidate (heap)
  (declare (optimize (speed 3) (space 0) (debug 0)))
  ;; The array size constant ist $log_{phi}(m-p-f)$ where
  ;; most-positive-fixnum represents the largest number of elements we
  ;; are able to hold. The resulting constant is the largest degree of
  ;; any root list and thus our max. necessary array size which we
  ;; avoid to recalculate every time we call consolidate.
  (let ((A (make-array #.(ceiling (log most-positive-fixnum (/ (1+ (sqrt 5)) 2)))
                       :initial-element nil
                       :element-type '(or null node)))
        (w (min-node heap)))
    (declare (dynamic-extent A))
    ;; For each root list node search for others of the same degree
    (loop for next-w = (node-right w)
       for x = w
       for d fixnum = (node-degree x)
       with start = w
       with max-d fixnum = 0 ; use max-d to minimise the array scan length
       do (do ((y (aref A d) (aref A d)))
              ((null y))
            (when (< (node-key y) (node-key x))
              (rotatef x y))
            (when (eq y start)
              (setf start (node-right start)))
            (when (eq y next-w)
              (setf next-w (node-right next-w)))
            (link y x)
            (setf (aref A d) nil)
            (incf d))
         (setf (aref A d) x
               max-d (max max-d d))
       until (eq (setf w next-w) start)
       finally ;; find minimum key and its corresponding node again
         (loop for node across A
            for i to max-d
            with min = start
            while (<= i max-d)
            do (when (and node
                          (< (node-key node)
                             (node-key min)))
                 (setf min node))
            finally (setf (min-node heap) min)))))

(defun empty-p (heap)
  "Return NIL if HEAP is empty, otherwise the minnimal node."
  (zerop (heap-size heap)))

(defun insert (heap key data)
    "Insert a new node with KEY and associated DATA item into the HEAP
root-list. No consolidation is done at this time."
    (let ((node (make-node key data))
          (min (min-node heap)))
      (setf (gethash data (node-table heap)) node)
      (incf (heap-size heap))
      (cond
        (min
         (splice-lists min node)
         (if (< key (node-key min))
             (setf (min-node heap) node)
             node))
        (t
         (setf (min-node heap) node)))))

(defun peek-min (heap)
  (let ((min (min-node heap)))
    (when min
      (values (node-data min)
              (node-key min)))))

(defun extract-min (heap)
  (let ((min (min-node heap)))
    (when min
      (when (node-child min)
        (setf (node-parent (node-child min)) nil)
        (loop for x = (node-right (node-child min)) then (node-right x)
           until (eq x (node-child min))
           do (setf (node-parent x) nil))
        (setf (min-node heap)
              (splice-lists (min-node heap) (node-child min))))
      (setf (node-right (node-left min)) (node-right min)
            (node-left (node-right min)) (node-left min))
      (cond
        ((eq min (node-right min))
         (setf (min-node heap) nil))
        (t
         (setf (min-node heap) (node-right min))
         (consolidate heap)))
      (setf (node-parent min) nil
            (node-left min) nil
            (node-right min) nil)
      (decf (heap-size heap))
      (remhash (node-data min) (node-table heap))
      (values (node-data min)
              (node-key min)))))

(defun decrease-key (heap data key)
  (let ((node (gethash data (node-table heap))))
    (when (< (node-key node) key)
      (error "Cannot decrease key: new key greater than current key."))
    (let ((y (node-parent node)))
      (setf (node-key node) key)
      (when (and y (< key (node-key y)))
        (cut heap node y)
        (cascading-cut heap y))
      (if (< key (node-key (min-node heap)))
          (setf (min-node heap) node)
          node))))

(defun extract-node (heap node)
  (let ((key (node-key node))
        (value (node-data node)))
    (decrease-key heap value most-negative-fixnum)
    (extract-min heap)
    (values value key)))

(defun lookup-node (heap data)
  (let ((node (gethash data (node-table heap))))
    (when node
      (node-key node))))

(defun meld (heap-a heap-b)
    "Melds HEAP-A and HEAP-B into a new heap and returns it. HEAP-A and
HEAP-B will be empty after this operation but may be used further."
    (let ((heap (make-instance 'fib-heap)))
      (setf (min-node heap) (splice-lists (min-node heap-a)
                                          (min-node heap-b)))
      (when (and (min-node heap-a)
                 (min-node heap-b)
                 (< (node-key (min-node heap-b))
                    (node-key (min-node heap-a))))
        (setf (min-node heap) (min-node heap-b)))
      (setf (heap-size heap) (+ (heap-size heap-a) (heap-size heap-b))
            (min-node heap-a) nil
            (min-node heap-b) nil
            (heap-size heap-a) 0
            (heap-size heap-b) 0)
      heap))
;;; BEGIN_USE_PACKAGE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :fib-heap :cl-user))

;;; Utilities

(in-package :cl-user)

(macrolet ((def (b)
             `(progn (deftype ,(intern (format nil "UINT~A" b)) () '(unsigned-byte ,b))
                     (deftype ,(intern (format nil "INT~A" b)) () '(signed-byte ,b))))
           (define-int-types (&rest bits) `(progn ,@(mapcar (lambda (b) `(def ,b)) bits))))
  (define-int-types 2 4 7 8 15 16 31 32 62 63 64))

(defmacro dbg (&rest forms)
  (declare (ignorable forms))
  (if (= (length forms) 1)
      `(format t "~A => ~A~%" ',(car forms) ,(car forms))
      `(format t "~A => ~A~%" ',forms `(,,@forms))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mksym (control-string &rest format-arguments)
    (intern (apply #'format nil control-string format-arguments))))

(defmacro mvbind (vars value-form &body body)
  `(multiple-value-bind ,vars ,value-form ,@body))

(defmacro mvcall (function arg &rest arguments)
  `(multiple-value-call ,function ,arg ,@arguments))

(defmacro mvlist (value-form)
  `(multiple-value-list ,value-form))

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression ,@body))

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
           (mvbind (,val ,found-p)
               (gethash ,k ,table)
             (if ,found-p ,val
                 (setf (gethash ,k ,table)
                       (dbind ,lambda-list ,args
                         ,@body)))))))))

(defmacro with-memo ((name lambda-list &body definition) &body body)
  (with-gensyms (table args val found-p)
    `(let ((,table (make-hash-table :test 'equal)))
       (labels ((,name (&rest ,args)
                  (mvbind (,val ,found-p)
                      (gethash ,args ,table)
                    (if ,found-p ,val
                        (setf (gethash ,args ,table)
                              (dbind ,lambda-list ,args
                                ,@definition))))))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-form (body)
    (cond ((null body)
           nil)
          ((= (length body) 1)
           (car body))
          (t
           `(progn ,@body))))

  (defun definition->memoized-definition (definition table)
    (with-gensyms (args val found-p)
      (dbind (name lambda-list &body body) definition
        `(,name (&rest ,args)
                (mvbind (,val ,found-p)
                    (gethash ,args ,table)
                  (if ,found-p ,val
                      (setf (gethash ,args ,table)
                            (dbind ,lambda-list ,args
                              ,@body))))))))

  ) ; eval-when

(defmacro with-memos (definitions &body body)
  (if (null definitions)
      (ensure-form body)
      (let* ((n (length definitions))
             (tables (loop repeat n collect (gensym "TABLE"))))
        `(let ,(mapcar (lambda (table)
                         `(,table (make-hash-table :test 'equal)))
                tables)
           (labels ,(mapcar (lambda (definition table)
                              (definition->memoized-definition definition table))
                            definitions tables)
             ,@body)))))

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
       (dbind ,lambda-list ,gargs
         ,@body))))

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

(defun until (predicate function value &rest more-values)
  (declare (optimize speed (safety 1)))
  (declare (function predicate function))
  (if (funcall predicate value)
      value
      (mvcall #'until
              predicate
              function
              (apply function value more-values))))

(defun while (predicate function value &rest more-values)
  (declare (optimize speed (safety 1)))
  (declare (function predicate function))
  (if (funcall predicate value)
      (mvcall #'while
              predicate
              function
              (apply function value more-values))
      value))

(eval-when (:compile-toplevel :load-toplevel :execute)
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
  ) ; eval-when

(defmacro switch (&whole whole (object &key (test 'eql) (key 'identity))
                         &body clauses)
  (generate-switch-body whole object clauses test key))

(defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  (generate-switch-body whole object clauses test key '(error)))

(defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  (generate-switch-body whole object clauses test key '(cerror "Return NIL from CSWITCH.")))

;;; Read macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\^
   (lambda (stream char num)
     (declare (ignore char))
     (unless num (setf num 1))
     `(lambda ,(loop for i from 1 to num
                     collect (mksym "%~d" i))
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

;;; IO

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

(defmacro collect (n expr)
  `(loop repeat ,n
         collect ,expr))

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

(defun read-matrix (height width &key (read #'read-fixnum) (element-type 'fixnum))
  (let ((mat (make-array `(,height ,width) :element-type element-type)))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref mat i j) (funcall read))))
    mat))

(defun read-char-matrix (height width)
  (let ((mat (make-array `(,height ,width) :element-type 'character)))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref mat i j) (read-char)))
      (read-char)) ; skip #\Newline
    mat))

(defun read-edges (n-edges)
  (collect n-edges
    (readlet (u v)
      (cons (1- u) (1- v)))))

(defun read-weighted-edges (n-edges)
  (nlet rec ((i 0) (edges nil) (ws nil))
    (if (= i n-edges)
        (values (nreverse edges)
                (nreverse ws))
        (readlet (u v w)
          (rec (1+ i)
               (acons (1- u) (1- v) edges)
               (cons w ws))))))

(defun read-graph (n-vertices n-edges &key directed)
  (let ((graph (make-list-array n-vertices)))
    (loop repeat n-edges do
      (readlet (u v)
        (push (1- v) (aref graph (1- u)))
        (unless directed
          (push (1- u) (aref graph (1- v))))))
    graph))

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":count ~A :test ~A"
            (hash-table-count object)
            (hash-table-test object))
    (loop for k being the hash-key of object
          do (format stream "~%[~A] ~A" k (gethash k object)))))

(defsubst println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format*
          (if (typep obj 'double-float) 'double-float *read-default-float-format*)))
    (prog1 (princ obj stream) (terpri stream))))

(defsubst printlns (lst)
  (format t "~{~A~%~}" lst))

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

;;; Functionals

(defmacro curry* (function &rest arguments)
  (assert (= 1 (count '% arguments :test 'eq)))
  (let ((g% (gensym)))
  `(lambda (,g%)
     (funcall ,function ,@(subst g% '% arguments)))))

(defun curry (function &rest arguments)
  (lambda (&rest args)
    (mvcall function
            (values-list arguments)
            (values-list args))))

(defun rcurry (function &rest arguments)
  (lambda (&rest args)
    (mvcall function
            (values-list args)
            (values-list arguments))))

(defun apply-n (n f x)
  (nlet rec ((n n) (x x))
    (if (zerop n)
        x
        (rec (1- n)
             (funcall f x)))))

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

(defsubst mapper (hash-table &optional default)
  (lambda (key)
    (gethash key hash-table default)))

(defsubst eqer (object)
  (lambda (x)
    (eq object x)))

(defsubst eqler (object)
  (lambda (x)
    (eql object x)))

(defsubst equaler (object)
  (lambda (x)
    (equal object x)))

(defsubst quantizer (predicate)
  (lambda (x)
    (if (funcall predicate x) 1 0)))

(defsubst applier (fn)
  (lambda (args)
    (apply fn args)))

(defsubst arefer (array)
  (lambda (&rest subscripts)
    (apply #'aref array subscripts)))

(defsubst adder (&rest numbers)
  (lambda (x) (apply #'+ x numbers)))

(defsubst suber (&rest  numbers)
  (lambda (x) (apply #'- x numbers)))

(defsubst muler (&rest numbers)
  (lambda (x) (apply #'* x numbers)))

(defsubst diver (&rest numbers)
  (lambda (x) (apply #'/ x numbers)))

(defun foldl (function initial-value sequence)
  (reduce function sequence :initial-value initial-value))

(defun foldl1 (function sequence)
  (if (zerop (length sequence))
      (error "Empty sequence.")
      (reduce function sequence)))

(defun foldr (function initial-value sequence)
  (reduce function sequence :initial-value initial-value :from-end t))

(defun foldr1 (function sequence)
  (if (zerop (length sequence))
      (error "Empty sequence.")
      (reduce function sequence :from-end t)))

(defun mvfoldl (function list initial-value &rest initial-args)
  "Generalization of FOLDL.
(FUNCTION item acc arg1 arg2 ... argN) => new-acc, new-arg1, new-arg2,..., new-argN
LIST -- list of items
INITIAL-ARGS == (initial-arg1 initial-arg2 ... initial-argN)"
  (declare (optimize (speed 3) (safety 1)))
  (declare (function function))
  (if (null list)
      (apply #'values initial-value initial-args)
      (mvcall #'mvfoldl
              function
              (cdr list)
              (apply function (car list) initial-value initial-args))))

(defun mvfoldl1 (function list &rest initial-args)
  (if (null list)
      (error "Empty list.")
      (apply #'mvfoldl function (cdr list) (car list) initial-args)))

(defun mvfoldr (function list initial-value &rest initial-args)
  "Generalization of FOLDR.
(FUNCTION item acc arg1 arg2 ... argN) => new-acc, new-arg1, new-arg2,..., new-argN
LIST -- list of items
INITIAL-ARGS == (initial-arg1 initial-arg2 ... initial-argN)"
  (apply #'mvfoldl function (reverse list) initial-value initial-args))

(defun mvfoldr1 (function list &rest initial-args)
  (if (null list)
      (error "Empty list.")
      (let ((rev (reverse list)))
        (apply #'mvfoldl function (cdr rev) (car rev) initial-args))))

(defun scanl (function initial-value list)
  (labels ((rec (list acc)
             (if (null list)
                 (nreverse acc)
                 (rec (cdr list) (cons (funcall function (car acc) (car list))
                                       acc)))))
    (rec list (list initial-value))))

(defun scanl1 (function list)
  (if (null list)
      (error "Empty list.")
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
      (error "Empty list.")
      (scanr function (car (last list)) (nbutlast list))))

(defun ensure-function (sym-or-func)
  (cond ((symbolp sym-or-func) `',sym-or-func)
        ((or (functionp sym-or-func)
             (and (consp sym-or-func) (eq (car sym-or-func) 'lambda)))
         sym-or-func)
        (t (error "~A is not symbol or function." sym-or-func))))

(defun disjoin (predicate &rest more-predicates)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((predicate (ensure-function predicate))
        (more-predicates (mapcar #'ensure-function more-predicates)))
    (lambda (&rest arguments)
      (or (apply predicate arguments)
          (some (lambda (p)
                  (declare (type function p))
                  (apply p arguments))
                more-predicates)))))

(defun conjoin (predicate &rest more-predicates)
  (if (null more-predicates)
      predicate
      (lambda (&rest arguments)
        (and (apply predicate arguments)
             (do ((tail (cdr more-predicates) (cdr tail))
                  (head (car more-predicates) (car tail)))
                 ((not tail)
                  (apply head arguments))
               (unless (apply head arguments)
                 (return nil)))))))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;;; Numbers

(defconstant +mod+ 1000000007)
(defconstant +inf+ most-positive-fixnum)
(defconstant +ninf+ most-negative-fixnum)

(defsubst md (x)
  (mod x +mod+))

(defsubst ^2 (x)
  (expt x 2))

(defsubst 2^ (x)
  (ash 1 x))

(defsubst 2* (x)
  (ash x 1))

(defsubst dist (x y)
  (abs (- x y)))

(defsubst df (x)
  (coerce x 'double-float))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mappend (function list)
    (loop for item in list
          append (funcall function item))))

(defmacro sortf (predicate &rest places)
  (let* ((meths (mapcar (lambda (p)
                          (mvlist
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

;;; Hash tables

(defun alist->ht (alist &key (test #'eql))
  (let ((ht (make-hash-table :test test)))
    (mapc (dlambda ((k . v))
            (setf (gethash k ht) v))
          alist)
    ht))

(defun ht->alist (ht)
  (let (result)
    (maphash (lambda (k v)
               (push (cons k v) result))
             ht)
    result))

(defun ht (keys values &key (test #'eql))
  (let ((ht (make-hash-table :test test)))
    (mapc (lambda (k v)
            (setf (gethash k ht) v))
          keys values)
    ht))

(defun make-hashset (list &key (test 'eql))
  (let ((ht (make-hash-table :test test)))
    (dolist (item list ht)
      (setf (gethash item ht) t))))

(defun counter (sequence &key (test 'eql) (key 'identity))
  (let ((counter (make-hash-table :size (length sequence) :test test)))
    (map nil (lambda (elem)
               (let ((k (funcall key elem)))
                 (setf (gethash k counter)
                       (1+ (gethash k counter 0)))))
         sequence)
    counter))

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

(defun vector-accum-ht (alist &key (test #'eql))
  (let ((ht (make-hash-table :test test)))
    (loop for (k . v) in alist do
      (unless (gethash k ht)
        (setf (gethash k ht) (make-adj-array)))
      (vector-push-extend v (gethash k ht)))
    ht))

(defun list-accum-ht (alist &key (test 'eql))
  (let ((ht (make-hash-table :test test)))
    (loop for (k . v) in alist do
      (push v (gethash k ht)))
    ht))

;;; Arrays

(defun coerce-vector (object)
  (coerce object 'vector))

(defun pad (vector)
  (concatenate 'vector #(0) vector))

(defun slice (vec beg &optional (end (length vec)))
  (loop (multiple-value-bind (disp-to disp-index) (array-displacement vec)
          (if disp-to
              (setf vec disp-to
                    beg (+ beg disp-index)
                    end (when end (+ end disp-index)))
              (return))))
  (let ((size (max 0 (- (or end (length vec)) beg))))
    (apply #'make-array size :element-type (array-element-type vec)
           (unless (zerop size)
             (list :displaced-to vec :displaced-index-offset beg)))))

(defun make-bit-array (dimensions &rest initargs)
  (apply #'make-array dimensions :element-type 'bit initargs))

(defun make-list-array (dimensions)
  (make-array dimensions :element-type 'list
                         :initial-element nil))

(defun make-fixnum-array (dimensions &rest initargs)
  (apply #'make-array dimensions :element-type 'fixnum initargs))

(defun make-adj-array (&optional (length 0))
  (make-array length :fill-pointer 0 :adjustable t))

(defsubst row-major-index (i j n-cols)
  (+ (* i n-cols) j))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun var-and-dimension-spec->loop (var-and-dimension-spec body)
    (dbind (var upper-bound &key downward) var-and-dimension-spec
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

(defun accum-array (alist dimensions &rest initargs)
  (let ((a (apply #'make-array dimensions initargs)))
    (mapc (dlambda ((subscripts . value))
            (setf (apply #'aref a subscripts)
                  value))
          alist)
    a))

;;; Lists/Sequences

(defun coerce-list (object)
  (coerce object 'list))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun singletonp (list)
    (and (consp list) (null (cdr list)))))

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

(defun length<= (n list)
  (cond ((null list)
         (>= n 0))
        ((zerop n)
         nil)
        (t
         (length<= (1- n) (cdr list)))))

(defun all-same-p (list &key (test 'eql))
  (or (null list)
      (every (curry test (car list))
             (cdr list))))

(defun range (&rest args)
  (ecase (length args)
    (1 (let ((end (car args)))
         (loop for i below end
               collect i)))
    (2 (dbind (start end) args
         (loop for i from start below end
               collect i)))
    (3 (dbind (start end step) args
         (loop for i from start below end by step
               collect i)))))

(defun drange (&rest args)
  (nreverse (apply #'range args)))

(declaim (ftype (function (fixnum fixnum fixnum) fixnum) range-last))
(defun range-last (start end step)
  (assert (< start end))
  (+ start (* step (floor (- end start 1) step))))

(defun map-range (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 0)))
  (loop for i fixnum from start below end by step
        collect (funcall function i)))

(defun map-drange (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 0)))
  (let ((last (range-last start end step)))
    (loop for i fixnum = last then (the fixnum (- i step))
          while (>= i start)
          collect (funcall function i))))

(defun mapc-range (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 0)))
  (loop for i fixnum from start below end by step
        do (funcall function i)))

(defun mapc-drange (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 0)))
  (let ((last (range-last start end step)))
    (loop for i fixnum = last then (the fixnum (- i step))
          while (>= i start)
          do (funcall function i))))

(defun filter-range (predicate start end &optional (step 1))
  (declare (function predicate)
           (fixnum start end step)
           (optimize speed (safety 0)))
  (loop for i fixnum from start below end by step
        when (funcall predicate i)
          collect i))

(defun filter-map-range (function start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 0)))
  (loop for i fixnum from start below end by step
        for item = (funcall function i)
        when item
          collect item))

(defun range-foldl (function initial-value start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 0)))
  (let ((acc initial-value))
    (loop for i fixnum from start below end by step
          do (setf acc (funcall function acc i)))
    acc))

(defun range-foldl1 (function start end &optional (step 1))
  (assert (< start end))
  (range-foldl function start (1+ start) end step))

(defun range-foldr (function initial-value start end &optional (step 1))
  (declare (function function)
           (fixnum start end step)
           (optimize speed (safety 0)))
  (let ((last (range-last start end step))
        (acc initial-value))
    (loop for i fixnum = last then (the fixnum (- i step))
          while (>= i start)
          do (setf acc (funcall function i acc)))
    acc))

(defun range-foldr1 (function start end &optional (step 1))
  (assert (< start end))
  (range-foldr function start (1+ start) end step))

(defun take (n list &key (step 1))
  (nlet rec ((n n) (list list) (acc nil))
    (if (or (zerop n) (null list))
        (nreverse acc)
        (rec (1- n)
             (nthcdr step list)
             (cons (car list) acc)))))

(defun take-while (predicate list)
  (nlet rec ((list list) (acc nil))
    (if (and list
             (funcall predicate (car list)))
        (rec (cdr list)
             (cons (car list) acc))
        (nreverse acc))))

(defsubst drop (n list)
  (nthcdr n list))

(defun drop-while (predicate list &key count)
  (if (or (and count (zerop count))
          (null list)
          (not (funcall predicate (car list))))
      list
      (drop-while predicate (cdr list)
                  :count (if count (1- count) nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun last1 (sequence)
    (etypecase sequence
      (list (car (last sequence)))
      (vector (when (plusp (length sequence))
                (aref sequence (1- (length sequence))))))))

(defsubst repeat (n item)
  (make-list n :initial-element item))

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

(defun zip-with-index (list &optional (index-base 0))
  (zip list
       (range index-base
              (+ index-base (length list)))))

(defun filter (predicate sequence &key from-end (start 0) end count key)
  (remove-if-not predicate sequence
                 :from-end from-end
                 :start start
                 :end end
                 :count count
                 :key key))

(defun filter-map (function list)
  (nreverse
   (foldl (lambda (acc item)
            (aif (funcall function item)
                 (cons it acc)
                 acc))
          nil
          list)))

(defun find-if* (predicate list &key from-end (start 0) end)
  (let ((list (subseq list start end)))
    (nlet rec ((list (if from-end (reverse list) list)))
      (if (null list)
          (values nil nil)
          (aif (funcall predicate (car list))
               (values (car list) it)
               (rec (cdr list)))))))

;; ;; more general
;; (defun filter-map (f list &rest more-lists)
;;   (labels ((rec (list more-lists acc)
;;              (if (and (consp list) (every #'consp more-lists))
;;                  (rec (cdr list)
;;                       (mapcar #'cdr more-lists)
;;                       (let ((value (apply f (car list) (mapcar #'car more-lists))))
;;                         (if value
;;                             (cons value acc)
;;                             acc)))
;;                  (nreverse acc))))
;;     (rec list more-lists nil)))

(defun position-filter (predicate sequence)
  (let ((pos -1))
    (remove-if-not (lambda (item)
                     (declare (ignore item))
                     (funcall predicate (incf pos)))
                   sequence)))

(defun map-adjacents (function list &optional (k 2))
  (nlet rec ((list list) (acc nil))
    (if (null (nthcdr (1- k) list))
        (nreverse acc)
        (rec (cdr list)
             (cons (apply function (take k list))
                   acc)))))

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

(defun split-at (n list)
  (labels ((rec (n list left)
             (if (or (zerop n) (null list))
                 (values (nreverse left) list)
                 (rec (1- n) (cdr list) (cons (car list) left)))))
    (rec n list nil)))

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

(defun iterate (n x successor)
  (nlet rec ((n n) (x x) (acc nil))
    (if (zerop n)
        (nreverse acc)
        (rec (1- n)
             (funcall successor x)
             (cons x acc)))))

(defun multiset (sequence test)
  (when (or (consp sequence) (>= (length sequence) 1))
    (nreverse
     (reduce (lambda (acc next)
               (dbind (curr . count) (car acc)
                 (if (funcall test curr next)
                     (cons (cons curr (1+ count)) (cdr acc))
                     (cons (cons next 1) acc))))
             (subseq sequence 1)
             :initial-value (list (cons (elt sequence 0) 1))))))

(defun delete-dups (list sort-predicate test)
  (mapcar #'car
          (multiset (sort list sort-predicate)
                    test)))

(defun group (n list)
  (labels ((rec (src &optional acc)
             (let ((rest (nthcdr n src)))
               (if (consp rest)
                   (rec rest (cons (subseq src 0 n) acc))
                   (nreverse (cons src acc))))))
    (when list
      (rec list))))

(defun prefixp (sequence prefix &key (test 'eql) (start 0) end)
  (let ((i (mismatch prefix sequence
                     :test test :start2 start :end2 end)))
    (or (not i)
        (= i (length prefix)))))

(defun suffixp (sequence suffix &key (test 'eql) (start 0) end)
  (let ((i (mismatch suffix sequence
                     :from-end t :test test :start2 start :end2 end)))
    (or (not i)
        (zerop i))))

(defun splice-replace (new old list &key (test 'eql) count)
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
                            (mvbind (left right) (split-at i list)
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

;;; Graphs

(defun make-graph (n-vertices edges &key directed)
  (let ((graph (make-list-array n-vertices)))
    (mapc (dlambda ((u . v))
            (push v (aref graph u))
            (unless directed
              (push u (aref graph v))))
          edges)
    graph))

(defun adjacent-cells (h w i j)
  (loop for di in '(-1 1 0 0)
        for dj in '(0 0 -1 1)
        for k = (+ i di)
        for l = (+ j dj)
        when (and (>= k 0) (< k h)
                  (>= l 0) (< l w))
          collect (cons k l)))

(defun graph-count-if (predicate graph start
                       &optional (seen (make-bit-array (length graph))))
  (labels ((dfs (vs acc)
             (if (null vs)
                 acc
                 (let ((u (car vs))
                       (vs (cdr vs)))
                   (dfs (append (filter (lambda (v)
                                          (when (zerop (aref seen v))
                                            (setf (aref seen v) 1)))
                                        (aref graph u))
                                vs)
                        (+ acc
                           (funcall (quantizer predicate) u)))))))
    (setf (aref seen start) 1)
    (dfs (list start) 0)))

;;; Accumulations

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun loop-for-clause (var &rest args)
    (ecase (length args)
      (1 `(loop for ,var in ,(car args)))
      (2 `(loop for ,var from ,(first args) below ,(second args)))
      (3 `(loop for ,var from ,(first args) below ,(second args) by ,(third args))))))

(defmacro define-accumulations ()
  `(progn
     ,@(loop for acc in '(sum maximize minimize thereis always)
             collect
             `(defmacro ,acc ((var &rest args) &body body)
                `(,@(apply #'loop-for-clause var args)
                  ,',acc ,(ensure-form body)))
             collect
             `(defmacro ,(mksym "~A*" acc) (var-and-args-specs &body body)
                (when (null var-and-args-specs)
                  (error "VAR-AND-ARGS-SPECS must not be null."))
                (labels ((rec (specs)
                           (if (null specs)
                               (ensure-form body)
                               `(,',acc ,(car specs)
                                        ,(rec (cdr specs))))))
                  (rec var-and-args-specs))))))

(define-accumulations)

(defmacro counting ((var &rest args) &body body)
  `(,@(apply #'loop-for-clause var args)
    count ,(ensure-form body)))

(defmacro counting* (var-and-args-specs &body body)
  (if (null var-and-args-specs)
      0
      (labels ((rec (specs)
                 (if (singletonp specs)
                     `(counting ,(car specs)
                        ,(ensure-form body))
                     `(sum ,(car specs)
                        ,(rec (cdr specs))))))
        (rec var-and-args-specs))))

(defmacro nconcing ((var &rest args) &body body)
  `(,@(apply #'loop-for-clause var args)
    nconc ,(ensure-form body)))

(defmacro collecting ((var &rest args) &body body)
  `(,@(apply #'loop-for-clause var args)
    collect ,(ensure-form body)))

(defmacro collecting* (var-and-args-specs &body body)
  (if (null var-and-args-specs)
      nil
      (labels ((rec (specs)
                 (if (singletonp specs)
                     `(collecting ,(car specs)
                        ,(ensure-form body))
                     `(nconcing ,(car specs)
                        ,(rec (cdr specs))))))
        (rec var-and-args-specs))))

(defmacro lcomp (var-and-args-specs condition &body body)
  (if (null var-and-args-specs)
      nil
      (labels ((rec (specs)
                 (if (singletonp specs)
                     `(,@(apply #'loop-for-clause (car specs))
                       when ,condition
                       collect ,(ensure-form body))
                     `(,@(apply #'loop-for-clause (car specs))
                       nconc ,(rec (cdr specs))))))
        (rec var-and-args-specs))))

(defmacro define-array-accumulations ()
  `(progn
     ,@(loop for acc in '(sum collect count nconc append some every)
             for wrd in '(sum collect count nconc append thereis always)
             collect
             `(defun ,(mksym "ARRAY-~A" acc) (array &optional (key 'identity))
                (loop for i below (array-total-size array)
                      ,wrd (funcall key (row-major-aref array i)))))))

(define-array-accumulations)

;;; Body

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

(defun solve (n m abs cs)
  (let* ((g (make-graph n abs))
         (w (ht abs cs :test 'equal))
         (edge->index (ht abs (range 1 (1+ m))
                          :test 'equal))
         (p (nth-value 1 (dijkstra g 0 (lambda (u v)
                                         (sortf < u v)
                                         (gethash (cons u v) w))))))
    (map-range (lambda (v)
                 (let ((parent (svref p v)))
                   (sortf < v parent)
                   (gethash (cons v parent) edge->index)))
               1 n)))

(defun main ()
  (readlet (n m)
    (mvbind (abs cs) (read-weighted-edges m)
      (join-print (solve n m abs cs)))))
    
#-swank (main)
