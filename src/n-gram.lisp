(defpackage n-gram
  (:use  :common-lisp)
  (:export main))

(load "util")
(load "vars")
(in-package n-gram)

;;;destructive
;;;just count word
;; or you can set it just list and use find instead of gethash
(defun count-word (word hash)
  (let ((score (gethash word hash)))
	(if (not (gate word)) ;;restriction of word
	  (if score 
		(setf (gethash word hash) (+ 1 score))
		(setf (gethash word hash) 1)))))


;;;restriction of word
(defun gate (word)
  (or (find #\、 word) (find #\。 word) ;、。をはじく
	  (find #\年 word) (find #\月 word)
	  (find #\日 word)))
	  ;(gethash word *dict*)));*dict*に登録されている単語をはじく
	  ;;not used -> if you want to use it,
	  ;;you should load unuser.lisp and do set-dict

;;;main function
;;;n-gram serch 
(defun n-gram (n str hash)
  (loop
	for word = (n-slice n str)
	while word
	do (count-word word hash)
	do (setf str (cdr-str str))))

;;;do n-gram from n to m
;;;returns hash-table
(defun n-to-m-gram (n m str &optional (hash nil))
  (if (null hash)
	(setf hash (make-hash-table :test #'equal)))
  (loop
	for i from n to m
	do (n-gram i str hash))
  hash)

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
						(if (or
							  (and (eql ct 'Hiragana) (not (eql ct typ)))
							  (and (eql typ 'Hiragana) (not (eql ct typ))))
						  (remhash key hash))
						(setf typ ct)))))
			 hash)
	hash))

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

;;;scoring from url
(defun scoring-url (url)
  (scoring-str (wget url)))

;;;save hash -> file
;;;format ~key,~val~%
(defun save-file (hash file)
  (with-open-file (f file
					 :direction :output
					 :if-exists :supersede
					 :if-does-not-exist :create)
	(maphash #'(lambda (key val) (format f "~a,~a~%" key val)) hash)))

;;;read saved file -> hash
(defun load-file (file)
  (let ((result (make-hash-table :test #'equal)))
	(with-open-file (f file :direction :input :if-does-not-exist :create)
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
	(with-open-file (f (load-tag-key-file tag file) :direction :input)
	  (loop
		for line = (read-line f nil)
		while line
		for lst = (split #\, line)
		do (setf (gethash (car lst) (get-tag-hash tag tag-list)) (read-from-string (cadr lst))))))
			tags)
	tag-list))

;;;tag -> tag/key-words.csv
(defun load-tag-key-file (tag &optional (file *group-dir*))
  (car (directory (concatenate 'string file "/" (string tag) "/key-words.csv"))))

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

;;;update key-list
(defun update-key-list (hash file)
  (let* ((key-lst (let ((h (load-file file)))
					(if (not (null h))
					  h
					  (make-hash-tabl :test #'equal))));hash
		 (intersect (half-value (intersection-of-hash hash key-lst)))
		 ;;intersection-of-hash returns hash table having sum of values
		 (dif1 (remove-intersection hash key-lst))
		 (dif2 (remove-intersection key-lst hash))
		 (merg (merge-hash (merge-hash intersect dif1) dif2)))
	(print-sorted-hash hash)
	(print intersect)
	(print dif1)
	(print dif2)
	(print merg)
	(save-file (cut-save-hash merg)
			   file)))

;;;cut-off saveing hash
(defun cut-save-hash (hash)
  (cut-off 1000 hash :items t))

;;;url -> update key file
(defun update-from-url (url file)
  (update-key-list (scoring-url url) file))

(defun main (url &key (tag nil))
  (let* ((score (scoring-url url))
		 (tag-list (get-tags score))
		 (tag (if (null tag) (car (car tag-list)) tag)))
	(update-key-list score (load-tag-key-file tag))
	(let ((result nil))
	(loop
	  for n from 1 to 3
	  for tag in tag-list
	  do (setf result (cons tag result)))
	(nreverse result))))
