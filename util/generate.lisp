(ql:quickload '(:alexandria :str))

(defmacro with-output-to-file ((stream-name file-name) &body body)
  `(alexandria:with-output-to-file (,stream-name ,file-name
						 :if-does-not-exist :create
						 :if-exists :supersede)
     ,@body))

(defun module-start-p (chunk)
  (char= #\@ (char chunk 0)))

(defun module-name (module-start-chunk)
  (let ((newline (position #\Newline module-start-chunk :test #'char=)))
    (subseq module-start-chunk 1 newline)))

(defun module-end-p (chunk)
  (let* ((last-line-start (position #\Newline (str:trim chunk)
				    :from-end t
				    :test #'char=))
	 (last-@ (and last-line-start
		      (position #\@ chunk
				:from-end t
				:start last-line-start
				:test #'char=))))
    (and last-@
	 (char= (char chunk (1- last-@)) #\Newline))))

(defun form-name (chunk)
  (let ((lines (str:split #\Newline chunk)))
    (dolist (line lines)
      (when (str:starts-with? "(" line)
	(return-from form-name (second (str:split #\Space line)))))))

(defun curr-dir-file (file)
  (concatenate 'string
	       "/home/yy/documents/atcoder/util/"
	       file))

(defun module-forms-to-file (module-name chunks)
;  (format t "generate module ~A~%" module-name)
  (with-output-to-file (out (curr-dir-file (concatenate 'string "@" module-name)))
    (dolist (chunk chunks)
      (dolist (line (str:lines chunk :omit-nulls t))
	(unless (str:starts-with? "@" line)
	  (princ line out)
	  (fresh-line out)))
      (terpri out))))

(defun form-to-file (form-name chunk)
;  (format t "generate ~A~%" form-name)
  (with-output-to-file (out (curr-dir-file form-name))
    (princ chunk out)
    (fresh-line out)
    (terpri out)))

(defun main (filepath)
  (let ((chunks (str:split (make-string 2 :initial-element #\Newline) ; split by blank line
			   (alexandria:read-file-into-string filepath)
			   :omit-nulls t)))
    (labels ((rec (chunks)
	       (when chunks
		 (if (module-start-p (car chunks))
		     (let ((module-end (position-if #'module-end-p
						    chunks))
			   (module-name (module-name (car chunks))))
		       (unless module-end
			 (error "module ~A has no end." module-name))
		       (module-forms-to-file module-name (subseq chunks 0 (1+ module-end)))
		       (rec (nthcdr (1+ module-end) chunks)))
		     (let ((form-name (form-name (car chunks))))
		       (when form-name
			 (form-to-file form-name (car chunks)))
		       (rec (cdr chunks)))))))
      (rec chunks))))

(main "/home/yy/documents/atcoder/util/atcoder.lisp")
