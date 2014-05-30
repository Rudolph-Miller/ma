(in-package n-gram)

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
  (if (not (< (the fixnum (length str)) n))
	(the string (subseq str 0 n))
	nil))

;;;cdr of str
(defun cdr-str (str)
  (subseq str 1))

;;;hash->list(key . val) sorted by its value (cdr lst)
(defun sort-hash (hash &key (reverse-p nil))
  (let ((lst nil))
	(maphash #'(lambda (key val) (setf lst (cons (cons key val) lst))) hash)
	(if (null reverse-p)
	(sort lst #'> :key #'cdr)
	(sort lst #'< :key #'cdr))))

;;;hash->hash sorted by its value
(defun sorted-hash (hash &key (reverse-p nil))
  (let ((result (make-hash-table :test #'equal)))
	(mapc #'(lambda (item)
			  (setf (gethash (car item) result) (cdr item)))
		  (sort-hash hash :reverse-p reverse-p))
	result))

;;;hash -> *standard-output* "~key: ~value~%"
(defun print-sorted-hash (hash)
  (let ((lst (sort-hash hash)))
	(mapc #'(lambda (item)
			  (format t "~a: ~a~%" (car item) (cdr item)))
		  lst)))

;;;f-name->concatenated string
(defun input (input)
  (let ((result ""))
	(with-open-file (input input :direction :input)
	  (loop
		for line = (read-line input nil)
		while line
		do (setf result (concatenate 'string result line))))
	result))

;;;input str -> removed str (by char-code)
(defun remove-not-jp (str)
  (remove-if #'(lambda (chr) (< (char-code chr) 1000)) str))

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
	(declare (list lst))
	(maphash #'(lambda (key val)
				 (setf lst (cons (cons key val) lst)))
			 hash)
	lst))

;;;list->hash
(defun list->hash (lst)
  (let ((hash (make-hash-table :test #'equal)))
	(mapc #'(lambda (item)
			  (setf (gethash (car item) hash) (cdr item)))
		  lst)
	hash))

;;;requires 2 hashes
;;;returns intersection part of 2 hashes
;;;return type is hash-table
(defun intersection-of-hash (hash1 hash2)
  (let ((lst1 (hash->list hash1))
		(lst2 (hash->list hash2))
		(result (make-hash-table :test #'equal)))
	(maphash
	  #'(lambda (key val)
		  (let ((score (gethash key hash2)))
			(if score
			  (setf (gethash key result) (+ score val)))))
	  hash1)
	result))

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
  (if (or (<= n 0) (null lst))
	nil
	(cons (car lst) (take (cdr lst) (1- n)))))

;;;remove n items from lst
;;;required by gen-combi
(defun drop (lst n)
  (if (or (null lst) (<= n 0))
	lst
	(drop (cdr lst) (1- n))))

;;;lst to n group
;;;required by gen-combi
(defun group (lst n)
  (if (null lst)
	nil
	(cons (take lst n) (group (drop lst n) n))))

;;;required by gen-combi
(defun combi (lst)
  (let ((n (length lst))
		(result nil))
	(loop
	  for i from n downto 1
	  collect (group lst i))))

;;;generate combination of list
(defun gen-combi (lst)
  (let ((result nil))
	(loop
	  for i from 1 to (1- (length lst))
	  do (setf result 
			   (cons 
				 (mapcar
				   #'(lambda (item1) 
					   (mapcar 
						 #'(lambda (item2) (append item2 item1))
						 (combi (take lst i))))
				   (combi (drop lst i))) 
				 result)))
	result))

;;;required by get-str-combi
(defun reduce-list (lst)
  (mapcar #'(lambda (lst)
			  (mapcar #'(lambda (item)
						  (if (<= 1 (length item))
							(apply #'concatenate 'string item)
							item)) lst))
		  (mapcar #'append (mapcar #'append  (apply #'append (apply #'append lst))))))

;;;strings -> list
;;;its not tail recursion (can be tail recursion)
(defun str->list (str)
  (if (not (equal str ""))
	(cons (subseq str 0 1) (str->list (subseq str 1)))
	nil))

(defun str->char-list (str &optional result)
  (if (not (zerop (length str)))
	(str->char-list (subseq str 1) (cons (char str 0) result))
	(nreverse result)))

;;;strings -> combination of strings
(defun get-str-combi (str)
  (remove-duplicates (apply #'append (reduce-list (gen-combi (str->list str)))) :test #'equal))

;;;hash -> sum of score 
(defun sum-score (hash)
  (let ((result 0))
	(maphash #'(lambda (key val) (setf result (+ result val))) hash)
	result))

;;;character -> type of it
(defun char-type (chr) ;like "a"
  (let ((code (char-code chr)))
	(cond 
	  ((and (< 12353 code) (< code 12447))
	   'Hiragana)
	  ((and (< 12449 code) (< code 12539))
	   'Katakana)
	  (t
		'other))))

;;;generate unique id by universal time
(defun gen-unique-id ()
	(write-to-string (get-universal-time)))

;;;make value half
(defun half-value (hash)
  (let ((result (make-hash-table :test #'equal)))
  (maphash #'(lambda (key val) (setf (gethash key result) (/ val 2)))
		   hash)
  result))

;;;remove intersecton of hash from hash1
;;;target hash -> hash
(defun remove-intersection (hash1 hash2)
  (let ((result (make-hash-table :test #'equal)))
  (maphash #'(lambda (key val) (if (not (gethash key hash2))
								 (setf (gethash key result) val)))
		   hash1)
  result))

;;;merge 2 hashes
;;;hash1 hash2 -> hash
;;;if conflicted in keys -> take hash2 value
(defun merge-hash (hash1 hash2)
  (let ((result (make-hash-table :test #'equal)))
	(mapc #'(lambda (hash) (maphash #'(lambda (key val) (setf (gethash key result) val)) hash))
		  (list hash1 hash2))
	result))
