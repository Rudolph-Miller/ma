(defun f-open (f-name)
  (with-open-file (f f-name :direction :input)
	(with-open-file (out "ejdic.csv" :direction :output :if-exists :supersede)
	(setf *standard-output* out)
	(loop
	  for line = (read-line f nil)
	  while line
	  do (txt->csv (splie #\Tab line))))))

(defun splie (key str)
  (let ((result nil))
	(loop
	  for pos = (position key str)
	  do (setf result (cons (subseq str 0 pos) result))
	  while pos
	  do (setf str (subseq str (1+ pos))))
	(nreverse result)))

(defun txt->csv (txt)
  (format t "~a,*,*,*,英単語,一般,*,*,*,*,~a~%" (car txt) (cadr txt)))

(f-open (car (directory "*.txt")))
