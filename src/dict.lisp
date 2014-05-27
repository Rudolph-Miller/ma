(defpackage dict
  (:use :common-lisp)
  (:export get-longest
		   set-dict
		   from-file
		   longest-search
		   print-result
		   sort-hash
		   *dict*))
(in-package dict)

(defvar *dict*
  (make-hash-table :test #'equal))

(defmacro ncons (lst1 lst2)
  `(setf ,lst2 (cons ,lst1 ,lst2)))

(defun split (key line)
  (let ((result nil))
	(declare (list result))
	(loop
	  for pos = (position key line)
	  do (ncons (the string (subseq line 0 pos)) result)
	  while pos
	  do (setf line (subseq line (1+ pos))))
	(nreverse result)))


(defun set-dict (file)
  (with-open-file (input file :direction :input)
	(loop
	  for line = (read-line input nil)
	  while line
	  do (let ((lst (split #\, line)))
		   (setf (gethash (car lst) *dict*) lst)))))

(defun from-file (file)
  (mapc #'set-dict
		(directory (concatenate 'string file "/*.csv"))))

;(from-file "utf-8")

(defun print-hash (hash)
  (maphash #'(lambda (key val) (format t "~a: ~a~%" key val)) hash))

(defun sort-hash (hash)
  (let ((lst nil))
	(maphash #'(lambda (key val) (ncons key lst))
			 hash)
	(sort lst #'< :key #'length)))

(defun get-longest (str)
  (declare (optimize speed))
  (handler-case 
	(let ((result nil))
	  (loop 
		for i from 15 downto 1
		for chs = (handler-case 
					(if (< (length str) i)
					  str
					  (subseq str 0 i))
					(sb-kernel:bounding-indices-bad-error (c) nil))
		while chs
		for long = (the list (gethash chs *dict*))
		do (setf result long)
		until long)
	  result)
	(sb-int:invalid-array-index-error (c) nil)))


(defun longest-search (str)
  (declare (string str))
  (let ((result nil))
	(loop
	  until (zerop (length str) )
	  for key = (let ((l (get-longest str)))
				  (if l
					l
					(list (subseq str 0 1) "*" "*" "*" "unknown" "unknown")))
	  while key
	  do (ncons key result)
	  do (setf str (subseq str (length (car key)))))
	(nreverse result)))


(defun get-part (lst)
  (nth 4 lst))


(defun print-result (result)
  (mapc #'(lambda (lst)
			(declare (list lst))
			(if (equal (get-part lst) "名詞")
			  (format t "~a: ~a~%" (car lst) (get-part lst))))
		result))

(defun input-html (input)
  (let ((result ""))
	(with-open-file (f input :direction :input)
	  (loop
		for line = (read-line f nil)
		while line
		do (setf result (concatenate 'string result line)))
	  result)))

;(mapcar #'compile '(split ncons get-longest sort-hash print-result get-part longest-search))

;(let ((*standard-output* (open "test" :direction :output :if-exists :supersede)))
;  (print-result (longest-search (input-html "~/lab/test/test1.html"))))

;(sb-ext:save-lisp-and-die
;  "dict-sample"
;  :toplevel 
;  :executable t)

;(print-result (longest-search "あーやだなぁ。明<a> </>日もおきなきゃいけないのかな。"))

;(defun set-lst (lst)
;  (mapcar 
;	#'(lambda (item)
;		(format t "~a,*,*,*,記号,一般,*,*,*,*,*~%" item))
;	lst))
;
;(defvar *lst*
;  (append (loop
;	for i from 0 to 64
;	collect (code-char i))
;  (loop for i from 123 to 126
;		collect (code-char i))))
;
;
;
;(with-open-file
;  (output "symbols.csv" :direction :output :if-exists :supersede)
;  (let ((*standard-output* output))
;  (set-lst *lst*)))
;

