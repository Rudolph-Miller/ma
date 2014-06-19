(defpackage :url
	(:use :common-lisp)
	(:export :key-url-list
					 :sort-sites
					 :get-value
					 :search-engine))

(in-package :url)

(load "longest")
(load "util-for-url")

;;;search in google, yahoo, or livedoor
(defun search-engine (q &key (livedoor nil) (yahoo nil))
	(let ((s (make-string-output-stream)))
		(sb-ext:run-program 
			"/usr/bin/python"
			(cond
				(yahoo  (list "test.py" q "yahoo"))
				(livedoor (list "test.py" q "livedoor"))
				(t (list "test.py" q)))
			:output s)
		(get-output-stream-string s)))

;;;pickup "..." form strings
(defun pick-double-quotation (str)
	(let ((result nil))
		(declare (list result))
		(loop
			for beg = (position #\" str)
			while beg
			for end = (position #\" str :start (1+ beg))
			while end
			do (setf result (cons (subseq str (1+ beg) end) result))
			do (setf str (subseq str (1+ end))))
		(nreverse result)))

;;;http or not
(defun http-p (str)
	(let ((len (length str)))
		(if (> len 4)
			(string= str "http" :start1 0 :end1 4))))

;;;split 
(defun split (key str)
	(let ((result nil))
		(loop
			for pos = (position key str)
			do (setf result (cons (subseq str 0 pos) result))
			while pos
			do (setq str (subseq str (1+ pos))))
		(nreverse result)))

;;;csv->list
(defun input-csv (csv)
	(let ((lst (split #\, csv)))
		lst))

(defun include (key str)
	(let ((flag nil)
				(len1 (length str))
				(len2 (length key)))
		(loop
			for i from 0 to (- len1 len2)
			do (if (string= key str :start2 i :end2 (+ i len2))
					 (setq flag t))
			until flag)
		flag))

(defun pick-sites (result)
	(remove-if #'null
						 (mapcar 
							 #'(lambda (item)
									 (if (and (http-p item) 
														(not
															(some 
																	 #'(lambda (key)
																			 (include key item))
																	 '("livedoor" "linecorp" "adsense" "youtube" "dailymotion"))))
										 item))
							 (remove-duplicates 
								 (pick-double-quotation result) :test #'equal))))

(defun wget (url uid)
	(let ((file-name (concatenate 'string (namestring (car (directory "../tmp"))) (concatenate 'string (gen-unique-id) "-" uid)))
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
	(the string (write-to-string (the integer (get-universal-time)))))

(defun in-thread (url-list)
	(let ((threads)
				(hash (make-hash-table :test #'equal)))
		(loop
			for url in url-list
			for i from 1
			do (push 
					 (sb-thread:make-thread
						 #'(lambda ()
								 (setf (gethash url hash) (make-hash-table :test #'equal))
								 (let ((result (gethash url hash))
											 (str (remove-not-jp (remove-comment
																						 (wget url (write-to-string i))))))
									 (declare (hash-table result)
														(string str))
									 (setf (gethash :length result) (length str))
									 (mapc #'(lambda (key)
														 (let ((score (gethash (car key) result)))
															 (if score
																 (incf (gethash (car key) result))
																 (setf (gethash (car key) result) 1))))
												 (longest-search str)))))
					 threads)
			when (eql (mod i 5) 0) do
			(loop
				for thread in threads
				do (sb-thread:join-thread thread)))
		hash))


(defun not-in-thread (url-list)
	(let ((hash (make-hash-table :test #'equal)))
		(loop
			for url in url-list
			for i from 1
			do (setf (gethash url hash) (make-hash-table :test #'equal))
			do (let ((result (gethash url hash))
							 (str (remove-not-jp (remove-comment
																		 (wget url (write-to-string i))))))
					 (declare (hash-table result)
										(string str))
					 (setf (gethash :length result) (length str))
					 (list-n-to-m-gram 
						 1 3
						 (gen-word-list str)
						 result)))
		;(mapc #'(lambda (key)
		; 				 (if (noun-p key)
		; 	 (let ((score (gethash (car key) result)))
		; 		 (if score
		; 			 (incf (gethash (car key) result))
		; 			 (setf (gethash (car key) result) 1)))))
		;(longest-search str))))
		hash))

(defun gen-word-list (str)
	(remove-hiragana
		(mapcar #'car 
					(longest-search str))))

(defun remove-hiragana (lst)
	(labels ((iter (str)
								 (if (zerop (length str))
									 t
									 (if (and (<= 12353 (char-code (character (subseq str 0 1)))) (<= (char-code (character (subseq str 0 1))) 12447 ))
										 (iter (subseq str 1))
										 nil))))
;;;Hiragana 12353-12447
	(remove-if 
		#'iter
		lst)))

(defun list-n-gram (n lst hash)
	(declare (fixnum n)
					 (list lst)
					 (hash-table hash)
					 (optimize (speed 3) (safety 0)))
	(loop
		for word = (slice-list n lst)
		while word
		do (setf lst (cdr lst))
		do (count-word word hash)
		while (>= (length lst) n))
	hash)

(defun slice-list (n lst)
	(declare (fixnum n)
					 (list lst)
					 (optimize (speed 3) (safety 0)))
	(labels ((sub (n lst acc)
								(if (<= n 0)
									(nreverse acc)
									(sub (1- n) (cdr lst) (cons (car lst) acc)))))
		(sub n lst nil)))


;;;destructive
;;;just count word
;; or you can set it just list and use find instead of gethash
(defun count-word (word hash)
	(declare 
					 (hash-table hash)
					 (optimize (speed 3) (safety 0) (debug 0) (space 0)))
	(if (gate word)
		(let ((score (gethash word hash)))
			(declare ((or single-float boolean) score))
			(if score 
				(incf (gethash word hash) 1.0)
				(setf (gethash word hash) (the single-float 1.0))))))

(defun gate (word)
  (or (find #\、 word) (find #\。 word) ;、。をはじく
	  (find #\年 word) (find #\月 word)
	  (find #\日 word)))

(defun list-n-to-m-gram (n m  lst &optional
													 (hash (make-hash-table :test #'equal)))
	(declare (fixnum n m)
					 (hash-table hash))
	(loop
		for i from n to m
		do (list-n-gram i lst hash))
	(the hash-table hash))
(defun get-part (key)
	(nth 4 key))

(defun noun-p (key)
	(equal (get-part key) "名詞"))

(defun get-value (url-list)
	(let ((hash (not-in-thread url-list))
				(result (make-hash-table :test #'equal)))
		(print-hash hash)
		(maphash 
			#'(lambda (key val)
					(maphash
						#'(lambda (k v)
								(let ((score (gethash k result)))
									(if score
										(incf (gethash k result) v)
										(setf (gethash k result) v))))
						val))
			hash)
		result))

(defun print-hash (hash)
	(maphash 
		#'(lambda (key val)
				(format t "~a: ~a~%" key val))
		hash))

(defun sort-hash (n hash)
	(let ((lst nil)
				(cnt 0)) 
		(maphash 
			#'(lambda (key val)
					(if (not (eql key :length))
						(push (cons key val) lst)))
			hash)
		(brush-up n (sort lst #'> :key #'cdr))))

(defun brush-up (n lst)
	(let ((result nil))
		(loop
			for i from 1 to n
			do (push (pop lst) result))
		(nreverse result)))


(defun get-result (q &key (livedoor nil) (yahoo nil) (result nil))
	(let* ((sea (if (null result)
								(search-engine q :livedoor livedoor :yahoo yahoo)
								q))
				 (dq (pick-double-quotation sea))
				 (lst (coerce sea 'list))
				 (result nil) (flag nil) (acc nil))
		(if (or (find "との一致はありません。" dq :test #'equal)
						(find "に一致するウェブページは見つかりませんでした。" dq :test #'equal))
			0
			(progn
				(loop for chr in lst
							while chr
							do (if (eql #\件 chr)
									 (progn (setq result acc)
													(setq flag nil)))
							do (if flag
									 (push chr acc))
							do (if (eql #\約 chr)
									 (setq flag 't acc nil)))
				(if (null result) (setq result (list #\0)))
				(read-from-string 
					(coerce (remove-not-number (nreverse result)) 'string))))))
;;;numbers -> 48-57
(defun remove-not-number (lst)
	(remove-if
		#'(lambda (chr)
				(not
					(and (<= 48 (char-code chr)) (<= (char-code chr) 57))))
		lst))

(defun key-url-list (q)
	(let ((lst 
				(pick-sites
				(search-engine
					q
					:livedoor t))))
	(remove-if #'null
	(mapcar #'(lambda (lst)
							(if (< 1 (length (apply 'concatenate 'string (car lst))))
							(let ((result (search-engine (apply #'concatenate 'string (car lst)) :livedoor t)))
								(if (not (zerop (get-result result :result t)))
									(list (apply #'concatenate 'string (car lst))
												(sort-sites (pick-sites result)))))))
	(sort-hash
		30
			(get-value
				lst))))))

(defun sort-sites (lst)
	(let ((wiki)
				(result nil))
		(mapc 
			#'(lambda (url)
					(if (include "wikipedia" url)
						(setq wiki url)
						(push url result)))
			lst)
		(if wiki
		(cons wiki (nreverse result))
		(nreverse result))))

(defun aiueo-jun (lst)
	(let ((result
					(mapcar 
						#'(lambda (item)
								(cons item (car (last (car (longest-search item))))))
						lst)))
		(mapcar  #'car (sort result #'string< :key #'cdr))))

