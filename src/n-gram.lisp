(defpackage n-gram
  (:use  :common-lisp)
  (:export :n-gram
		   :scoring-html
		   :compare-html))

(in-package n-gram)

(defvar *dict* (make-hash-table :test #'equal)) ;dictionary memory 

;;;tags list
(defvar *tags* '(tech life-hack))

(defvar *group-dir* "../group")
;;;str-> list
(defun split (key str)
  (let ((result nil))
	(declare (list result))
	(loop
	  for pos = (position key str)
	  do (setf result (cons (the string (subseq str 0 pos)) result))
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

;;;destructive
;;;just count word
(defun count-word (word hash)
  (let ((score (gethash word hash)))
	(if (not (gate word)) ;;restriction of word
	  (if score 
		(setf (gethash word hash) (+ 1 score))
		(setf (gethash word hash) 1)))))

;; or you can set it just list and use find instead of gethash

;;;destructive
;;;set *dict* from input file
(defun set-dict (input)
  (with-open-file (i input :direction :input)
	(loop
	  for line = (read-line i nil)
	  while line
	  do (let ((lst (split #\, line)))
		   (setf (gethash (car lst) *dict*) 1)))))

;;;set files to *dict*
(defun set-gate-dict ()
  (set-dict "../utf-8/Verb.csv")
  (set-dict "../utf-8/Postp.csv")
  (set-dict "../utf-8/Postp-col.csv")
  (set-dict "../utf-8/Symbol.csv")
  (set-dict "../utf-8/zenkaku-symbol.csv"))

(set-gate-dict)

;;;restriction of word
(defun gate (word)
  (or (find #\、 word) (find #\。 word) ;、。をはじく
	  (find #\年 word) (find #\月 word)
	  (find #\日 word)
	  (gethash word *dict*)));*dict*に登録されている単語をはじく

;;;main function
;;;n-gram serch 
(defun n-gram (n str hash)
  (loop
	for word = (n-slice n str)
	while word
	do (count-word word hash)
	do (setf str (cdr-str str))))

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

;;;do n-gram from n to m
;;;returns hash-table
(defun n-to-m-gram (n m str &optional (hash nil))
  (if (null hash)
	(setf hash (make-hash-table :test #'equal)))
  (loop
	for i from n to m
	do (n-gram i str hash))
  hash)

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

;;;hash(key, val) -> hash (key, probability of val)
(defun to-prob (hash-t)
  (let ((hash hash-t))
	(let ((vals nil))
	  (declare (list vals))
	  (maphash #'(lambda (key val) (push val vals)) hash)
	  (let ((total (reduce #'+ vals)))
		(declare (fixnum total))
		(maphash #'(lambda (key val) (setf (gethash key hash) (/ val total))) hash)
		hash))))

;;;restrict numbers of hash
;;;without :items t cut-off hash-table by (> poit value)
;;;with :items t cut-off hash-tables by how many items in hash-table (indexed by point)
(defun cut-off (point hash-t &key (items nil))
  (let ((hash hash-t))
	(if (null items)
	  (maphash #'(lambda (key val) (if (> point val)
									 (remhash key hash)))
			   hash)
	  (let ((items (sort-hash hash)))
		(loop
		  for i from 1
		  for item = (pop items)
		  while item
		  do (if (< point i)
			   (remhash (car item) hash)))))
	hash))

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

;;;strings -> combination of strings
(defun get-str-combi (str)
  (remove-duplicates (apply #'append (reduce-list (gen-combi (str->list str)))) :test #'equal))

;;;scoring function
;;;word (let n (length word))->scoring word by 1 to (1- n) gram
;;;requires word hash -> score(integer)
(defun scoring (word hash &key (length nil))
  (let ((sum 0)
		(len (or length 1))
		(val (gethash word hash)))
	(loop
	  for item in (the list (get-str-combi word))
	  for score = (gethash item hash)
	  do (if score
		   (setf sum (+ sum score))))
	(/ (* (gethash word hash) sum) (/ length (length word)))))

;;;scoring hash-table
;;;hash -> hash
(defun scoring-hash (hash-t &key (length nil))
  (let ((result (make-hash-table :test #'equal))
		(hash hash-t))
	(maphash #'(lambda (key val)
				 (let ((score (scoring key hash :length length)))
				   (if (not (eql 0 score))
					 (setf (gethash key result) score))))
			 (cut-off 100 (brush-up hash) :items t))
	result))

;;;input html -> output scoring hash-table
(defun scoring-html (html)
  (let* ((str (input html))
		 (len (length str)))
  (scoring-hash (n-to-m-gram 1 12 (remove-not-jp str))
				:length len)))

(defun scoring-str (str)
  (let ((len (length str)))
	(scoring-hash (n-to-m-gram 1 12 (remove-not-jp str)) :length len)))

;;;hash -> sum of score 
(defun sum-score (hash)
  (let ((result 0))
	(maphash #'(lambda (key val) (setf result (+ result val))) hash)
	result))



;;;brushing up key words
;;;remove key which includes 2 types
;;;like 形態素解析を -> 形態素解析
;;;Hiragana 12353-12447
;;;Katakana 12449-12539
(defun brush-up (hash)
  (let ((keys nil))
	(maphash #'(lambda (key val)
				 (let ((typ (char-type (char key 0)))
					   (ct nil))
				 (loop
				   for i in (str->list key)
				   do (let ((ct (char-type (char i 0))))
						(if (not
							  (or (eql typ ct)
								  (and (eql ct 'Katakana) (eql ct 'other))
								  (and (eql ct 'other) (eql ct 'Katakana))))
						(remhash key hash))
						(setf typ ct)))))
			 hash)
	hash))

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
	   
;;;loop-p
;;;like 
;;;save file from url to unique id by wget
;;;then load it by txt
(defun wget (url)
  (let ((file-name (concatenate 'string (namestring (car (directory "../tmp"))) (gen-unique-id)))
		(result ""))
	(declare (string file-name result))
	(labels ((sub (cha)
				  (with-open-file (f file-name :direction :input :external-format cha)
					(loop
					  for line = (read-line f nil)
					  while line
					  do (setf result (concatenate 'string result line))))))
	  (sb-ext:run-program "/sw/bin/wget" (list url "-O" file-name) :output nil)
	  (handler-case
		(sub :utf-8)
		(sb-int:stream-decoding-error (c) 
									  (handler-case
										(sub :cp932)
										(sb-int:stream-decoding-error (c)
																	  (sub :euc-jp))))))
	result))

;;;generate unique id by universal time
(defun gen-unique-id ()
	(write-to-string (get-universal-time)))

(defun scoring-url (url)
  (scoring-str (wget url)))

;;;save hash -> file
;;;format ~key,~val~%
(defun save-file (hash file)
  (with-open-file (f file :direction :output :if-exists :supersede)
	(maphash #'(lambda (key val) (format f "~a,~a~%" key val)) hash)))

;;;read saved file -> hash
(defun load-file (file)
  (let ((result (make-hash-table :test #'equal)))
	(with-open-file (f file :direction :input)
	  (loop
		for line = (read-line f nil)

		while line
		for lst = (split #\, line)
		do (setf (gethash (car lst) result) (read-from-string (cadr lst)))))
	result))


;;;tag list -> (list (tag . hash-table)..)
(defun gen-tag-hash (tags)
  (mapcar #'(lambda (key) (cons key (make-hash-table :test #'equal))) tags))

;;;tag -> get hash-table of this tag from tag-hash-list
(defun get-tag-hash (tag lst)
  (cdr (assoc tag lst)))

;;;tags list and group file -> (list (tag . hash)..)
(defun load-tag-hash (tags file)
  (let ((tag-list (gen-tag-hash tags)))
	(mapcar #'(lambda (tag)
	(with-open-file (f (car (directory (concatenate 'string file "/" (string tag) "/key-words.csv"))) :direction :input)
	  (loop
		for line = (read-line f nil)
		while line
		for lst = (split #\, line)
		do (setf (gethash (car lst) (get-tag-hash tag tag-list)) (read-from-string (cadr lst))))))
			tags)
	tag-list))

;;;hash tags file -> (list (tag . score)..)
(defun compare (hash &optional (tags *tags*) (file *group-dir*))
  (let ((tag-hash-list (load-tag-hash tags file)))
	(mapcar #'(lambda (tag) (cons tag (sum-score (intersection-of-hash hash (get-tag-hash tag tag-hash-list))))) tags)))

;;;hash -> sorted list
(defun get-tags (hash &optional (tags *tags*) (file *group-dir*))
  (let ((tag-scores (compare hash tags file)))
	(sort tag-scores #'> :key #'cdr)))

;;;url -> sorted tag list
(defun url->tags (url)
  (get-tags (scoring-url url)))


;(print-sorted-hash (scoring-url "http://somewrite.jp/media/somewrite/3347"))

(defun exe ()
  (print-sorted-hash (scoring-url (cadr sb-ext:*posix-argv*))))

(sb-ext:save-lisp-and-die "sample.exe"
  :toplevel #'exe
  :executable t)
