(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro mvbind (vars value-form &body body)
  `(multiple-value-bind ,vars ,value-form ,@body))

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression ,@body))

(eval-always
  (defun |#@-aux| (typespec)
    (dbind (type-specifier . args) typespec
      `(type ,type-specifier ,@args)))

  (set-dispatch-macro-character
   #\# #\@
   (lambda (stream char num)
     (declare (ignore char num))
     (let ((typespecs (read stream t nil t)))
       `(declare ,(|#@-aux| typespecs)
                 (optimize speed (safety 1))))))
  ) ;eval-always

(defmacro readlet (vars &body body)
  `(let ,(mapcar (lambda (v)
                   `(,v (read-fixnum)))
          vars)
     ,@body))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym ,(string s))))
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

(eval-always
  (set-dispatch-macro-character
   #\# #\>
   (lambda (stream char num)
     (declare (ignore char num))
     (with-gensyms (x)
       (cond ((char= #\= (peek-char t stream))
              (read-char stream)
              `(lambda (,x) (>= ,x ,(read stream t nil t))))
             (t
              `(lambda (,x) (> ,x ,(read stream t nil t)))))))))

(eval-always
  (make-dispatch-macro-character #\$)

  (set-dispatch-macro-character
   #\$ #\(
   (lambda (stream char num)
     (declare (ignore char num))
     (let ((list (read-delimited-list #\) stream t)))
       `(curry #',(car list) ,@(cdr list))))))

(eval-always
  (make-dispatch-macro-character #\@)

  (set-dispatch-macro-character
   #\@ #\(
   (lambda (stream char num)
     (declare (ignore char num))
     (let ((list (read-delimited-list #\) stream t)))
       `(compose ,@list)))))

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
      #@((integer 0 #.most-positive-fixnum) result)
      (loop
        (let* ((byte (%read-byte)))
          (if (<= 48 byte 57)
              (setq result (+ (- byte 48)
                              (* 10 (the (integer 0 #.(floor most-positive-fixnum 10))
                                         result))))
              (return (if minus (- result) result))))))))

(defsubst println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format*
          (if (typep obj 'double-float) 'double-float *read-default-float-format*)))
    (prog1 (princ obj stream) (terpri stream))))

(deftype u64 () '(unsigned-byte 64))

(defun foldl (function initial-value sequence)
  (reduce function sequence :initial-value initial-value))

(defsubst 2^ (x)
  (ash 1 x))

(defun floor-2p (number 2-power)
  #@(u64 number)
  (let ((mask (1- (the fixnum (2^ 2-power)))))
    #@(fixnum 2-power mask)
    (values (ash number (- 2-power))
            (logand number mask))))

(defsubst nim+ (&rest integers)
  (apply #'logxor integers))

;; https://hitonanode.github.io/cplib-cpp/number/nimber.hpp
(defun nim*2-rec (a b rec)
  #@(u64 a b)
  #@(function rec)
  (labels ((rec (bit)
             #@(fixnum bit)
             (cond ((plusp (ash a (- bit)))
                    (mvbind (a0 a1) (floor-2p a bit)
                      (mvbind (b0 b1) (floor-2p b bit)
                        (let ((p00 (funcall rec a0 b0))
                              (p01 (funcall rec a0 b1))
                              (p10 (funcall rec a1 b0))
                              (p11 (funcall rec a1 b1)))
                          #@(fixnum p00 p01 p10 p11)
                          (nim+ p11
                                (ash (nim+ p00 p01 p10) bit)
                                (funcall rec p00 (2^ (1- bit))))))))
                   ((plusp (ash b (- bit)))
                    (mvbind (b0 b1) (floor-2p b bit)
                      #@(fixnum b0)
                      (nim+ (ash (funcall rec a b0) bit)
                            (funcall rec a b1))))
                   (t
                    (rec (ash bit -1))))))
    (rec 32)))

(defsubst smallp (a)
  (< a 256))

(defmemo (nim*2-small :key #'identity :test #'equal) (a b)
  #@(fixnum a b)
  (when (> a b)
    (rotatef a b))
  ;; (<= a b)
  (cond ((= a 0) 0)
        ((= a 1) b)
        (t (nim*2-rec a b #'nim*2-small))))
         
(defun nim*2 (a b)
  #@(u64 a b)
  (when (> a b)
    (rotatef a b))
  ;; (<= a b)
  (cond ((= a 0) 0)
        ((= a 1) b)
        ((smallp b) (nim*2-small a b))
        (t (nim*2-rec a b #'nim*2))))

(defun main ()
  (readlet (n)
    (dotimes (_ n)
      (let ((a (read))
            (b (read)))
        (println (nim*2 a b))))))

(main)
