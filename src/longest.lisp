(in-package :url)

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
	(declare (dynamic-extent split)
					 (optimize (speed 3) (safety 0) (debug 0)))
	(with-open-file (input file :direction :input)
		(loop
			for line = (read-line input nil)
			while line
			do (let ((lst (split #\, line)))
					 (setf (gethash (car lst) *dict*) lst)))))

;(defun from-file (file)
;	(let ((threads))
;		(loop for f in (directory (concatenate 'string file "/*.csv"))
;					for i from 1
;					do (push 
;							 (sb-thread:make-thread #'(lambda () (set-dict f)))
;							 threads)
;					when (eql (mod i 3) 0) do 
;					(progn (loop for thread in threads
;											 do (sb-thread:join-thread thread))
;								 (setq threads nil)))))

(defun from-file (file)
	(mapc #'set-dict
				(directory (concatenate 'string file "/*.csv"))))


(defun get-longest (str)
	(declare (optimize (speed 3) (safety 0) (debug 0)))
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
		(declare (list result))
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
