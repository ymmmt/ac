(defpackage :cp/minimal-explicit-treap
  (:use :cl)
  (:export #:treap #:treap-p #:treap-key
           #:treap-split #:treap-insert #:treap-merge #:treap-delete
           #:%treap-ensure-key #:treap-unite #:treap-map #:do-treap
           #:make-treap #:treap-update #:treap-ref #:treap-pop
           #:treap-first #:treap-last #:treap-find
           #:treap-bisect-left #:treap-bisect-right #:treap-bisect-left-1 #:treap-bisect-right-1)
  (:documentation "Provides treap with explicit key, which virtully works like
std::map or java.util.TreeMap.

NOTE: please see cp/multiset if what you want is a multiset."))
(in-package :cp/minimal-explicit-treap)

(declaim (inline op))
(defun op (x y)
  "Is the operator comprising a monoid."
  (min x y))

(defconstant +op-identity+ most-positive-fixnum
  "identity element w.r.t. OP")

(defstruct (treap (:constructor %make-treap
                      (key priority value &key left right))
                  (:copier nil)
                  (:conc-name %treap-))
  (key 0 :type fixnum)
  value
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null treap))
  (right nil :type (or null treap)))

(declaim (inline treap-key))
(defun treap-key (treap)
  "Returns the key of (nullable) TREAP."
  (if treap
      (values (%treap-key treap) (%treap-value treap))
      (values nil nil)))

(declaim (ftype (function * (values (or null treap) (or null treap) &optional))
                treap-split))
(defun treap-split (treap key &key (order #'<))
  "Destructively splits TREAP with reference to KEY and returns two treaps,
the smaller sub-treap (< KEY) and the larger one (>= KEY)."
  (declare (optimize (speed 3))
           (function order)
           ((or null treap) treap))
  (if (null treap)
      (values nil nil)
      (progn
        (if (funcall order (%treap-key treap) key)
            (multiple-value-bind (left right)
                (treap-split (%treap-right treap) key :order order)
              (setf (%treap-right treap) left)
              (values treap right))
            (multiple-value-bind (left right)
                (treap-split (%treap-left treap) key :order order)
              (setf (%treap-left treap) right)
              (values left treap))))))

(declaim (inline treap-insert))
(defun treap-insert (treap key value &key (order #'<))
  "Destructively inserts KEY into TREAP and returns the resultant treap. You
cannot rely on the side effect. Use the returned value.

The behavior is undefined when duplicate keys are inserted."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (node treap)
             (declare (treap node))
             (unless treap (return-from recur node))
             (if (> (%treap-priority node) (%treap-priority treap))
                 (progn
                   (setf (values (%treap-left node) (%treap-right node))
                         (treap-split treap (%treap-key node) :order order))
                   node)
                 (progn
                   (if (funcall order (%treap-key node) (%treap-key treap))
                       (setf (%treap-left treap)
                             (recur node (%treap-left treap)))
                       (setf (%treap-right treap)
                             (recur node (%treap-right treap))))
                   treap))))
    (recur (%make-treap key (random (+ 1 most-positive-fixnum)) value) treap)))

(defun treap-merge (left right)
  "Destructively concatenates two treaps. Assumes that all keys of LEFT are
smaller (or larger, depending on the order) than those of RIGHT.

Note that this `merge' is different from CL:MERGE and rather close to
CL:CONCATENATE."
  (declare (optimize (speed 3))
           ((or null treap) left right))
  (cond ((null left) right)
        ((null right) left)
        (t (if (> (%treap-priority left) (%treap-priority right))
               (progn
                 (setf (%treap-right left)
                       (treap-merge (%treap-right left) right))
                 left)
               (progn
                 (setf (%treap-left right)
                       (treap-merge left (%treap-left right)))
                 right)))))

(defun treap-delete (treap key &key (order #'<))
  "Destructively deletes KEY in TREAP and returns the resultant treap. Returns
unmodified TREAP If KEY doesn't exist. You cannot rely on the side effect. Use
the returned value.

\(Note that this function deletes only a node even if duplicate keys are
contained.)"
  (declare (optimize (speed 3))
           ((or null treap) treap)
           (function order))
  (when treap
    (cond ((funcall order key (%treap-key treap))
           (setf (%treap-left treap)
                 (treap-delete (%treap-left treap) key :order order))
           treap)
          ((funcall order (%treap-key treap) key)
           (setf (%treap-right treap)
                 (treap-delete (%treap-right treap) key :order order))
           treap)
          (t
           (treap-merge (%treap-left treap) (%treap-right treap))))))

(declaim (inline treap-unite))
(defun treap-unite (treap1 treap2 &key (order #'<))
  "Merges two treaps with keeping the order."
  (labels
      ((recur (l r)
         (cond ((null l) r)
               ((null r) l)
               (t (when (< (%treap-priority l) (%treap-priority r))
                    (rotatef l r))
                  (multiple-value-bind (lchild rchild)
                      (treap-split r (%treap-key l) :order order)
                    (setf (%treap-left l) (recur (%treap-left l) lchild)
                          (%treap-right l) (recur (%treap-right l) rchild))
                    l)))))
    (recur treap1 treap2)))

(declaim (inline treap-map))
(defun treap-map (function treap)
  "Successively applies FUNCTION to TREAP[0], ..., TREAP[SIZE-1]. FUNCTION must
take two arguments: KEY and VALUE."
  (labels ((recur (treap)
             (when treap
               (force-down treap)
               (recur (%treap-left treap))
               (funcall function (%treap-key treap) (%treap-value treap))
               (recur (%treap-right treap)))))
    (recur treap)))

(defmethod print-object ((object treap) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (treap-map (lambda (key value)
                   (if init
                       (setf init nil)
                       (write-char #\  stream))
                   (format stream "<~A . ~A>" key value))
                 object))))

(defmacro do-treap ((key-var value-var treap &optional result) &body body)
  "Successively binds the keys and the values of TREAP to KEY-VAR and VALUE-VAR
in ascending order, and executes BODY."
  `(block nil
     (treap-map (lambda (,key-var ,value-var) ,@body) ,treap)
     ,result))

;; This function takes O(nlog(n)) time. It is just for debugging.
(defun treap (order &rest key-and-values)
  "Takes cons cells in the form of (<key> . <value>)."
  (loop with res = nil
        for (key . value) in key-and-values
        do (setf res (treap-insert res key value :order order))
        finally (return res)))

;; Reference: https://cp-algorithms.com/data_structures/treap.html
(declaim (inline make-treap))
(defun make-treap (size key-function &optional value-function)
  "Makes a treap in O(n) time using each key returned by (KEY-FUNCTION
<index>). Note that this function doesn't check if the keys are really sorted
w.r.t. your intended order. The values are filled by VALUE-FUNCTION in the same
way. If it is not given, the identity element is used."
  (declare ((integer 0 #.most-positive-fixnum) size)
           (function key-function)
           ((or null function) value-function))
  (labels ((heapify (top)
             (when top
               (let ((prioritized-node top))
                 (when (and (%treap-left top)
                            (> (%treap-priority (%treap-left top))
                               (%treap-priority prioritized-node)))
                   (setq prioritized-node (%treap-left top)))
                 (when (and (%treap-right top)
                            (> (%treap-priority (%treap-right top))
                               (%treap-priority prioritized-node)))
                   (setq prioritized-node (%treap-right top)))
                 (unless (eql prioritized-node top)
                   (rotatef (%treap-priority prioritized-node)
                            (%treap-priority top))
                   (heapify prioritized-node)))))
           (build (l r)
             (declare ((integer 0 #.most-positive-fixnum) l r))
             (if (= l r)
                 nil
                 (let* ((mid (ash (+ l r) -1))
                        (node (%make-treap (funcall key-function mid)
                                           (random (+ 1 most-positive-fixnum))
                                           (when value-function
                                             (funcall value-function mid)))))
                   (setf (%treap-left node) (build l mid))
                   (setf (%treap-right node) (build (+ mid 1) r))
                   (heapify node)
                   node))))
    (build 0 size)))

(declaim (inline %treap-ensure-key))
(defun %treap-ensure-key (treap key value &key (order #'<) if-exists)
  "IF-EXISTS := nil | function

Ensures that TREAP contains KEY and assigns VALUE to it if IF-EXISTS is
false. If IF-EXISTS is function and TREAP already contains KEY,
%TREAP-ENSURE-KEY updates the value by the function instead of overwriting it
with VALUE. You cannot rely on the side effect. Use the returned value.

NOTE: Except for efficiency reasons, it is better to use TREAP-REF."
  (declare (function order)
           ((or null treap) treap))
  (labels ((find-and-update (treap)
             ;; Updates the value slot and returns T if KEY exists
             (unless treap (return-from find-and-update nil))
             (cond ((funcall order key (%treap-key treap))
                    (when (find-and-update (%treap-left treap))
                      t))
                   ((funcall order (%treap-key treap) key)
                    (when (find-and-update (%treap-right treap))
                      t))
                   (t (setf (%treap-value treap)
                            (if if-exists
                                (funcall if-exists (%treap-value treap))
                                value))
                      t))))
    (if (find-and-update treap)
        treap
        (treap-insert treap key value :order order))))

(declaim (inline treap-ref))
(defun treap-ref (treap key &key (order #'<) default)
  "Returns the value that is assigned to KEY if it exists. Otherwise returns
DEFAULT."
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (if treap
                 (prog1 (cond ((funcall order key (%treap-key treap))
                               (recur (%treap-left treap)))
                              ((funcall order (%treap-key treap) key)
                               (recur (%treap-right treap)))
                              (t (%treap-value treap))))
                 default)))
    (recur treap)))

(define-setf-expander treap-ref (treap key &key (order '#'<) default &environment env)
  (multiple-value-bind (tmps vals stores store-form access-form)
      (get-setf-expansion treap env)
    (when (cdr stores)
      (error "SETF TREAP-REF too hairy."))
    (let ((key-tmp (gensym "KEY"))
          (order-tmp (gensym "ORDER"))
          (default-tmp (gensym "DEFAULT"))
          (store (gensym "NEW")))
      (values (list* key-tmp order-tmp default-tmp tmps)
              (list* key order default vals)
              (list store)
              `(let ((,(car stores)
                       (%treap-ensure-key ,access-form ,key-tmp ,store :order ,order-tmp)))
                 ;; KLUDGE: I add this line because SBCL claims that defalt-tmp
                 ;; is never used in the process of macro expansion.
                 ,default-tmp 
                 ,store-form
                 ,store)
              `(treap-ref ,access-form ,key-tmp
                          :order ,order-tmp
                          :default ,default-tmp)))))

(defmacro treap-pop (treap key &optional (order '#'<) &environment env)
  "Deletes a KEY from TREAP."
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion treap env)
    `(let* (,@(mapcar #'list temps vals)
            (,(car stores) (treap-delete ,getter ,key :order ,order))
            ,@(cdr stores))
       ,setter)))

(defun treap-first (treap)
  "Returns the leftmost key of TREAP."
  (declare (optimize (speed 3))
           (treap treap))
  (if (%treap-left treap)
      (treap-first (%treap-left treap))
      (%treap-key treap)))

(defun treap-last (treap)
  "Returns the rightmost key of TREAP."
  (declare (optimize (speed 3))
           (treap treap))
  (if (%treap-right treap)
      (treap-last (%treap-right treap))
      (%treap-key treap)))

;;;
;;; Binary search by key
;;;

(defun treap-find (treap key &key (order #'<))
  "Finds the key that satisfies (AND (NOT (FUNCALL ORDER KEY (%TREAP-KEY
<sub-treap>))) (NOT (FUNCALL ORDER (%TREAP-KEY <sub-treap>) KEY))) and returns
KEY if it exists, otherwise returns NIL."
  (declare (optimize (speed 3))
           (function order)
           ((or null treap) treap))
  (cond ((null treap) nil)
        ((funcall order key (%treap-key treap))
         (treap-find (%treap-left treap) key :order order))
        ((funcall order (%treap-key treap) key)
         (treap-find (%treap-right treap) key :order order))
        (t key)))

;; Cheat sheet
;; next key: treap-bisect-right
;; previous key: treap-bisect-left-1

(declaim (inline treap-bisect-left))
(defun treap-bisect-left (treap key &key (order #'<))
  "Returns the smallest key equal to or larger than KEY. Returns NIL if KEY is
larger than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order (%treap-key treap) key)
                    (recur (%treap-right treap)))
                   (t (or (recur (%treap-left treap))
                          treap)))))
    (treap-key (recur treap))))

(declaim (inline treap-bisect-right))
(defun treap-bisect-right (treap key &key (order #'<))
  "Returns the smallest key larger than KEY. Returns NIL if KEY is equal to or
larger than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order key (%treap-key treap))
                    (or (recur (%treap-left treap))
                        treap))
                   (t (recur (%treap-right treap))))))
    (treap-key (recur treap))))

(declaim (inline treap-bisect-left-1))
(defun treap-bisect-left-1 (treap key &key (order #'<))
  "Returns the largest key smaller than KEY. Returns NIL if KEY is equal to or
smaller than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order (%treap-key treap) key)
                    (or (recur (%treap-right treap))
                        treap))
                   (t (recur (%treap-left treap))))))
    (treap-key (recur treap))))

(declaim (inline treap-bisect-right-1))
(defun treap-bisect-right-1 (treap key &key (order #'<))
  "Returns the largest key equal to or smaller than KEY. Returns NIL if KEY is
smaller than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order key (%treap-key treap))
                    (recur (%treap-left treap)))
                   (t (or (recur (%treap-right treap))
                          treap)))))
    (treap-key (recur treap))))
