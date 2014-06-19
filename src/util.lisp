(in-package :n-gram)

(defmacro profiler (fn-list)
  (format t "~a is...~%" (car fn-list))
  `(print ,(disassemble (car fn-list)))
  `(time 
	 (loop
	   for i from 1 to 1000000
	   do ,fn-list)))

;;;str-> list
(defun split (key str)
  (declare (character key)
		   (string str)
		   (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let ((result nil))
	(declare (list result))
	(loop
	  for pos = (position key str)
	  do (setf result (the list (cons (the string (subseq str 0 pos)) result)))
	  while pos
	  do (setf str (the string (subseq str (1+ pos)))))
	(the list (nreverse result))))

;;;slice str to n strings
(defun n-slice (n str)
  (declare (fixnum n)
		   (string str))
  (if (not (< (the fixnum (length str)) n))
	(the string (subseq str 0 n))
	nil))

;;;cdr of str
(defun cdr-str (str)
  (declare (string str))
  (the string (subseq str 1)))

;;;hash->list(key . val) sorted by its value (cdr lst)
(defun sort-hash (hash &key (reverse-p nil))
  (let ((lst nil))
	(declare (hash-table hash)
			 (boolean reverse-p)
			 (list lst))
	(maphash #'(lambda (key val)
				 (setf lst (the list (cons (the list (cons key val)) lst)))) hash)
	(if (null reverse-p)
	(the list (sort lst #'> :key #'cdr))
	(the list (sort lst #'< :key #'cdr)))))

;;;hash->hash sorted by its value
(defun sorted-hash (hash &key (reverse-p nil))
  (let ((result (make-hash-table :test #'equal)))
  (declare (hash-table hash)
		   (boolean reverse-p)
		   (hash-table result))
  (mapc #'(lambda (item)
			(setf (gethash (car item) result) (cdr item)))
		(the list (sort-hash hash :reverse-p reverse-p)))
  (the hash-table result)))

;;;hash -> *standard-output* "~key: ~value~%"
(defun print-sorted-hash (hash)
  (let ((lst (sort-hash hash)))
	(declare (list lst))
	(mapc #'(lambda (item)
			  (format t "~a: ~a~%" (car item) (cdr item)))
		  lst)))

;;;f-name->concatenated string
(defun input (input)
  (let ((result ""))
	(declare (string result))
	(with-open-file (input input :direction :input)
	  (loop
		for line = (read-line input nil)
		while line
		do (setf result (concatenate 'string result line))))
	(the string result)))

;;;input str -> removed str (by char-code)
(defun remove-not-jp (str)
  (declare (string str))
  (the string (remove-if #'(lambda (chr) (< (char-code chr) 1000)) str)))

;;;remove comment from html
(defun remove-comment (html)
  (declare (string html))
  (let ((input-stream (make-string-input-stream html))
		(result nil)
		(chr1) (chr2) (chr3) (chr4) (flag) (change-p))
	(declare (stream input-stream)
			 (list result)
			 (boolean flag change-p))
	(loop
	  for chr = (read-char input-stream nil)
	  while chr
	  do (setf chr1 chr2 chr2 chr3 chr3 chr4 chr4 chr)
	  do (multiple-value-bind (f change-p) (comment-p chr1 chr2 chr3 chr4 flag change-p)
		   (setf flag f)
		   (if change-p (setf result (remove-3element result)))
		   (if (not flag) (push chr result))))
	(the string (concatenate 'string (nreverse result)))))

(defun comment-p (chr1 chr2 chr3 chr4 flag change-p)
  (declare (boolean flag change-p))
  (cond ((and (eql chr1 #\<) (eql chr2 #\!) (eql chr3 #\-) (eql chr4 #\-))
		 (values t t))
		((and (eql chr1 #\-) (eql chr2 #\-) (eql chr3 #\>))
		 (values nil nil))
		(t
		  (values flag nil))))

(defun remove-3element (lst)
  (cdddr lst))

(defun input-jp (input)
  (let ((result nil))
	(declare (list result))
	(with-open-file (input input :direction :input)
	  (loop 
		for chr = (read-char input nil)
		while chr
		when (> (char-code chr) 1000) do (push chr result)))
	  (coerce (nreverse result) 'string)))

;;;hash->list
(defun hash->list (hash)
  (let ((lst nil))
	(declare (hash-table hash)
			 (list lst))
	(maphash #'(lambda (key val)
				 (setf lst (cons (cons key val) lst)))
			 hash)
	(the list lst)))

;;;list->hash
(defun list->hash (lst)
  (let ((hash (make-hash-table :test #'equal)))
	(declare (hash-table hash)
			 (list lst))
	(mapc #'(lambda (item)
			  (setf (gethash (car item) hash) (cdr item)))
		  lst)
	(the hash-table hash)))

;;;requires 2 hashes
;;;returns intersection part of 2 hashes
;;;return type is hash-table
(defun intersection-of-hash (hash1 hash2)
  (let ((lst1 (hash->list hash1))
		(lst2 (hash->list hash2))
		(result (make-hash-table :test #'equal)))
	(declare (hash-table hash1 hash2 result)
			 (list lst1 lst2))
	(maphash
	  #'(lambda (key val)
		  (let ((score (gethash key hash2)))
			(if score
			  (setf (gethash key result) (+ score val)))))
	  hash1)
	(the hash-table result)))

;;;restrict numbers of hash
;;;without :items t cut-off hash-table by (> poit value)
;;;with :items t cut-off hash-tables by how many items in hash-table (indexed by point)
(defun cut-off (point hash &key (items nil))
  (let ((result (make-hash-table :test #'equal)))
	(declare (hash-table result))
	(if (null items)
	  (maphash #'(lambda (key val) (if (< point val)
									 (setf (gethash key result) val)))
			   hash)
	  (let ((items (sort-hash hash)))
		(loop
		  for i from 0 to point
		  for item = (pop items)
		  while item
		  do (setf (gethash (car item) result) (cdr item)))))
	result))

;;;take n items from lst
;;;required by gen-combi
(defun take (lst n)
  (declare (list lst)
		   (fixnum n))
  (if (or (<= n 0) (null lst))
	nil
	(the list (cons (car lst) (the list (take (cdr lst) (1- n)))))))

;;;remove n items from lst
;;;required by gen-combi
(defun drop (lst n)
  (declare (list lst)
		   (fixnum n))
  (if (or (null lst) (<= n 0))
	lst
	(the list (drop (cdr lst) (1- n)))))

;;;lst to n group
;;;required by gen-combi
(defun group (lst n)
  (declare (list lst)
		   (fixnum n))
  (if (null lst)
	nil
	(the list (cons (the list (take lst n)) (the list (group (drop lst n) n))))))

;;;required by gen-combi
(defun combi (lst)
  (let ((n (length lst))
		(result nil))
  (declare (list lst)
		   (integer n)
		   (list result))
	(the list
		 (loop
		   for i from n downto 1
		   collect (group lst i)))))

;;;generate combination of list
(defun gen-combi (lst)
  (let ((result nil))
	(declare (list lst result))
	(loop
	  for i from 1 to (1- (length lst))
	  do (setf result 
			   (cons 
				 (mapcar
				   #'(lambda (item1) 
					   (mapcar 
						 #'(lambda (item2) (append item2 item1))
						 (the list (combi (take lst i)))))
				   (the list (combi (drop lst i))))
				 result)))
	(the list result)))

;;;required by get-str-combi
(defun reduce-list (lst)
  (declare (list lst))
  (the list (mapcar #'(lambda (lst)
			  (mapcar #'(lambda (item)
						  (if (<= 1 (length item))
							(apply #'concatenate 'string item)
							item)) lst))
		  (mapcar #'append (mapcar #'append  (apply #'append (apply #'append lst)))))))

;;;strings -> list
;;;its not tail recursion (can be tail recursion)
(defun str->list (str)
  (declare (string str))
  (if (not (equal str ""))
	(the list (cons (subseq str 0 1) (str->list (subseq str 1))))
	nil))

(defun str->char-list (str &optional result)
  (if (not (zerop (length str)))
	(str->char-list (subseq str 1) (cons (char str 0) result))
	(nreverse result)))

;;;strings -> combination of strings
(defun get-str-combi (str)
  (declare (string str))
  (the list (remove-duplicates (apply #'append (the list (reduce-list (the list (gen-combi (the list (str->list str))))))) :test #'equal)))

;;;hash -> sum of score 
(defun sum-score (hash)
  (let ((result 0))
	(declare (hash-table hash))
	(maphash #'(lambda (key val) (setf result (+ result val))) hash)
	result))

;;;character -> type of it
(defun char-type (chr) ;like "a"
  (let ((code (char-code chr)))
	(declare (character chr)
			 (fixnum code))
	(cond 
	  ((and (< 12353 code) (< code 12447))
	   (the symbol 'Hiragana))
	  ((and (< 12449 code) (< code 12539))
	   (the symbol 'Katakana))
	  (t
		(the symbol 'other)))))

;;;generate unique id by universal time
(defun gen-unique-id ()
  (the string (write-to-string (the integer (get-universal-time)))))

;;;make value half
(defun half-value (hash)
  (let ((result (make-hash-table :test #'equal)))
	(declare (hash-table hash result))
	(maphash #'(lambda (key val) (setf (gethash key result) (/ val 2)))
			 hash)
	(the hash-table result)))

;;;remove intersecton of hash from hash1
;;;target hash -> hash
(defun remove-intersection (hash1 hash2)
  (let ((result (make-hash-table :test #'equal)))
	(declare (hash-table hash1 hash2 result))
	(maphash #'(lambda (key val) (if (not (gethash key hash2))
								   (setf (gethash key result) val)))
			 hash1)
	(the hash-table result)))

;;;merge 2 hashes
;;;hash1 hash2 -> hash
;;;if conflicted in keys -> take hash2 value
(defun merge-hash (hash1 hash2)
  (let ((result (make-hash-table :test #'equal)))
	(declare (hash-table hash1 hash2 result))
	(mapc #'(lambda (hash) (maphash #'(lambda (key val) (setf (gethash key result) val)) hash))
		  (list hash1 hash2))
	(the hash-table result)))

;;;hash n -> (hash-table (key . (/ val n)) ..)
(defun divided-hash (hash n)
  (declare (hash-table hash)
		   (single-float n))
  (let ((result (make-hash-table :test #'equal)))
	(declare (hash-table result))
	(maphash #'(lambda (key val)
				 (setf (gethash key result) (/ val n)))
			 hash)
	result))

;;;read-line from file-name and set variable
(defun set-dict (file-name variable)
  (with-open-file (f file-name :direction :input)
	(loop
	  for line = (read-line f nil)
	  while line
	  for lst = (split #\, line)
	  do (setf (gethash (car lst) variable) (cadr lst)))))

