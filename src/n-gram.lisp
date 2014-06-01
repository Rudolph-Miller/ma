
(defpackage n-gram
  (:use  :common-lisp)
  (:export main
		   learn))

(load "../../html-parser/html.lisp")
(load "vars")
(load "util")
(in-package n-gram)

(use-package :html)
(declaim (inline get-str-combi)
		 (inline slice)
		 (inline scoring-url))

;;;destructive
;;;just count word
;; or you can set it just list and use find instead of gethash
(defun count-word (word hash)
  (declare (string word)
		   (hash-table hash)
		   (optimize (speed 3) (safety 0) (debug 0) (space 0)))
	(if (not (gate word)) ;;restriction of word
	  (let ((score (gethash word hash)))
		(declare ((or single-float boolean) score))
	  (if score 
		(incf (gethash word hash) 1.0)
		(setf (gethash word hash) (the single-float 1.0))))))


;;;restriction of word
(defun gate (word)
  (declare (string word))
  (or (find #\、 word) (find #\。 word) ;、。をはじく
	  (find #\年 word) (find #\月 word)
	  (find #\日 word)
	  (gethash word *blacklist*))) ;; set-dict *blacklist* is done in main or lern
	  ;(gethash word *dict*)));*dict*に登録されている単語をはじく
	  ;;not used -> if you want to use it,
	  ;;you should load unuser.lisp and do set-dict

;;;destructive, though wrapping by n-to-m-gram
;;;main function
;;;n-gram serch 
(defun n-gram (n str hash)
  (declare (fixnum n)
		   (string str)
		   (hash-table hash)
		   (optimize (speed 3) (safety 0)))
  (loop
	for word = (n-slice n str)
	while word
	do (count-word word hash)
	do (setf str (cdr-str str))))

;;;do n-gram from n to m
;;;returns hash-table
(defun n-to-m-gram (n m str &optional (hash (make-hash-table :test #'equal)))
  (declare (fixnum n m)
		   (string str)
		   (hash-table hash))
  (loop
	for i from n to m
	do (n-gram i str hash))
  (the hash-table hash))

;;;scoring function
;;;word (let n (length word))->scoring word by 1 to (1- n) gram
;;;requires word hash -> score(integer)
(defun scoring (word hash &key (length nil))
  (declare (string word)
		   (hash-table hash))
  (let ((sum 0.0)
		(len (if length
			   (/ length 1.0)
			   1.0))
		(val (gethash word hash)))
	(declare (single-float sum len)
			 (number val))
	(loop
	  for item in (the list (get-str-combi word))
	  for score = (gethash item hash)
		   when score do (setf sum (the float (+ sum score))))
	(/ (* (gethash word hash) sum) (/ length (length word)))))

;;;scoring hash-table
;;;hash -> hash
(defun scoring-hash (hash-t &key (length nil))
  (let ((result (make-hash-table :test #'equal))
		(hash hash-t))
	(declare (hash-table hash-t hash result))
	(maphash #'(lambda (key val)
				 (let ((score (scoring key hash :length length)))
				   (if (not (eql 0.0 score))
					 (setf (gethash key result) score))))
			 (brush-up hash))
	result))

;;;input html -> output scoring hash-table
(defun scoring-html (html)
  (let* ((str (input html))
		 (len (length str)))
	(declare (string str)
			 (fixnum len))
  (the hash-table
	   (scoring-hash (n-to-m-gram 1 12 (remove-not-jp str))
				:length len))))

;;;strnigs -> hash-table
(defun scoring-str (str)
  (let ((len (length str)))
	(declare (string str))
	(the hash-table (scoring-hash (n-to-m-gram 1 12 str) :length len))))

;;;brushing up key words
;;;remove key which includes 2 types
;;;like 形態素解析を -> 形態素解析
;;;Hiragana 12353-12447
;;;Katakana 12449-12539
(defun brush-up (hash)
  (declare (hash-table hash)
		   (optimize (speed 3)))
  (let ((keys nil)
		(cnt 0)
		(result (make-hash-table :test #'equal)))
	(declare (list keys)
			 (fixnum cnt)
			 (hash-table result))
	(maphash #'(lambda (key val)
				 (if (< cnt 100)
				 (let ((ct (char-type (char key 0)))
					   (typ)
					   (lst (str->char-list key)))
				   (declare (symbol typ ct)
							(list lst))
				 (loop
				   for i in lst
				   do (setf typ ct)
				   do (setf ct (char-type i))
				   when (or (eql ct typ)
								(not (or (eq ct 'Hiragana) (eq typ 'Hiragana))))
				   when (eql i (car (last lst))) do (setf (gethash key result) val))
				 (incf cnt))))
			 (sorted-hash hash))
	result))

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

;;;wget and remove no JP in one function
(defun wget-and-remove  (url)
  (let ((file-name (concatenate 'string (namestring (car (directory "../tmp"))) (gen-unique-id)))
		(result nil))
	(declare (string file-name)
			 (list result))
	(labels ((sub (charset)
				  (with-open-file (f file-name :direction :input :external-format charset)
					(loop
					  for chr = (read-char f nil)
					  while chr
					  when (> (char-code chr) 1000) do (push chr result)))))
	  (sb-ext:run-program "/sw/bin/wget" (list url "-O" file-name) :output nil)
	  (handler-case
		(sub :utf-8)
		(sb-int:stream-decoding-error (c) 
									  (handler-case
										(sub :cp932)
										(sb-int:stream-decoding-error (c)
																	  (sub :euc-jp))))))
	(coerce (nreverse result) 'string)))


;;;scoring from url
(defun scoring-url (url)
  (scoring-str (wget-and-remove url)))

;;;save hash -> file
;;;format ~key,~val~%
(defun save-file (hash file)
  (declare (hash-table hash))
  (with-open-file (f file
					 :direction :output
					 :if-exists :supersede
					 :if-does-not-exist :create)
	(maphash #'(lambda (key val) (format f "~a,~a~%" key val)) hash)))

;;;read saved file -> hash
(defun load-file (file)
  (let ((result (make-hash-table :test #'equal)))
	(declare (hash-table result))
	(with-open-file (f file :direction :input :if-does-not-exist :create)
	  (loop
		for line = (read-line f nil)
		while line
		for lst = (split #\, line)
		do (setf (gethash (car lst) result) (float (read-from-string (cadr lst))))))
	(the hash-table result)))


;;;tag list -> (list (tag . hash-table)..)
(defun gen-tag-hash (tags)
  (declare (list tags))
  (the list (mapcar #'(lambda (key) (cons key (make-hash-table :test #'equal))) tags)))

;;;tag -> get hash-table of this tag from tag-hash-list
(defun get-tag-hash (tag lst)
  (declare (symbol tag)
		   (list lst))
  (the hash-table (cdr (assoc tag lst))))

;;;tags list and group file -> (list (tag . hash)..)
(defun load-tag-hash (tags file)
  (let ((tag-list (gen-tag-hash tags)))
	(declare (list tags tag-list))
	(mapcar #'(lambda (tag)
	(with-open-file (f (load-tag-key-file tag file) :direction :input)
	  (loop
		for line = (read-line f nil)
		while line
		for lst = (split #\, line)
		do (setf (gethash (car lst) (the hash-table (get-tag-hash tag tag-list))) (read-from-string (cadr lst))))))
			tags)
	(the list tag-list)))

;;;get blacklist from tag-list
;;;destructive
(defun get-blacklist (tag-list n)
  (let ((black-dict (make-hash-table :test #'equal)))
	(mapc #'(lambda (tag)
			  (maphash #'(lambda (key val)
						   (let ((cnt (gethash key black-dict)))
							 (if cnt
							   (if (>= cnt (1- n))
								 (setf (gethash key *blacklist*) 1)
							   (setf (gethash key black-dict) (1+ cnt)))
							   (setf (gethash key black-dict) 1))))
					   (cdr tag)))
			  tag-list)))

;;;tag -> tag/key-words.csv
(defun load-tag-key-file (tag &optional (file *group-dir*))
  (car (directory (concatenate 'string file "/" (string-downcase (string tag)) "/key-words.csv"))))

;;;hash tags file -> (list (tag . score)..)
(defun compare (hash &optional (tags *tags*) (file *group-dir*))
  (let ((tag-hash-list (load-tag-hash tags file)))
	(declare (hash-table hash)
			 (list tag-hash-list))
	(get-blacklist tag-hash-list 5)
	(the list (mapcar #'(lambda (tag) (cons tag (sum-score (the hash-table (intersection-of-hash (remove-blacklist hash) (get-tag-hash tag tag-hash-list)))))) tags))))

;;;remove word in *blacklist*
(defun remove-blacklist (hash)
  (let ((result (make-hash-table :test #'equal)))
	(maphash #'(lambda (key val) (if (not (gethash key *blacklist*))
								   (setf (gethash key result) val)))
			 hash)
	result))

;;;hash -> sorted list
(defun get-tags (hash &optional (tags *tags*) (file *group-dir*))
  (let ((tag-scores (compare hash tags file)))
	(declare (hash-table hash)
			 (list tag-scores))
	(the list (sort tag-scores #'> :key #'cdr))))

;;;url -> sorted tag list
(defun url->tags (url)
  (declare (string url))
  (the list (get-tags (scoring-url url))))

;;;update key-list
(defun update-key-list (hash file)
  (let* ((key-lst (let ((h (load-file file)))
					(if (not (null h))
					  h
					  (make-hash-table :test #'equal))));hash
		 (intersect (half-value (intersection-of-hash hash key-lst)))
		 ;;intersection-of-hash returns hash table having sum of values
		 (dif1 (remove-intersection hash key-lst))
		 (dif2 (remove-intersection key-lst hash))
		 (merg (merge-hash (merge-hash intersect dif1) dif2)))
	(declare (hash-table hash key-lst intersect dif1 dif2 merg))
	(save-file (cut-save-hash merg)
			   file)))

;;;cut-off saveing hash
(defun cut-save-hash (hash)
  (declare (hash-table hash))
  (the hash-table (cut-off 1000 hash :items t)))

;;;url -> learn key-words.csv and format top 3 tags
(defun main (url)
  (set-dict *bladklist-dir* *blacklist*)
  (handler-case 
	(let* ((score (scoring-url url))
		   (tag-list (get-tags score))
		   (result nil))
	  (declare (string url)
			   (hash-table score)
			   (list tag-list result))
	  (print tag-list)
	  (save-file (cut-save-hash *blacklist*) *bladklist-dir*)
	  (loop
		for n from 1.0 to 3.0
		for tag in (mapcar #'car tag-list)
		do (update-key-list (divided-hash score n) (load-tag-key-file tag))
		do (format t "~%~a: ~a~%" (round n) tag)
		do (setf result (cons tag result)))
	  (the list (nreverse result)))
	(type-error (c) (print "Invalid URL"))))

;;;url tag -> learn tag/key-words.csv
(defun learn (url tag)
  (set-dict *bladklist-dir* *blacklist*)
  (let* ((score (scoring-url url))
		 (tag-list (get-tags score)))
	(declare (string url)
			 (hash-table score)
			 (list tag-list))
	(update-key-list score (load-tag-key-file tag))
	tag-list))

;;;chroler for learning
;;;ready file which having name of tag
;;;and url~%...
;;;then read-line and save it to tag/key-words
(defun chroler (file)
  (let ((tag (intern (string-upcase (car (last (split #\/ file)))) "KEYWORD"))
		(urls))
	(declare (list urls))
	(with-open-file (input file)
	  (loop 
		for line = (read-line input nil)
		while line
		do (push line urls)))
	(mapcar #'(lambda (url) (format t "~a:~%~a~%" url (learn url tag)))
			urls)))

