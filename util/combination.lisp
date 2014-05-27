
(defun take (lst n)
  (if (or (<= n 0) (null lst))
	nil
	(cons (car lst) (take (cdr lst) (1- n)))))

(defun drop (lst n)
  (if (or (null lst) (<= n 0))
	lst
	(drop (cdr lst) (1- n))))

(defun group (lst n)
  (if (null lst)
	nil
	(cons (take lst n) (group (drop lst n) n))))

(defun combi (lst)
  (let ((n (length lst))
		(result nil))
	(loop
	  for i from n downto 1
	  collect (group lst i))))

(defun gen-combi (lst)
  (let ((result nil))
  (loop
	for i from 1 to (1- (length lst))
	do (setf result (cons (mapcar #'(lambda (item1) (mapcar #'(lambda (item2) (append item2 item1))
											  (combi (take lst i))))
					(combi (drop lst i))) result)))
  result))


(defun reduce-list (lst)
  (mapcar #'(lambda (lst)
			  (mapcar #'(lambda (item)
						  (if (<= 1 (length item))
							(apply #'concatenate 'string item)
							item)) lst))
		  (mapcar #'append (mapcar #'append  (apply #'append (apply #'append lst))))))

(defun str->list (str)
  (if (not (equal str ""))
	(cons (subseq str 0 1) (str->list (subseq str 1)))
	nil))

(defun get-str-combi (str)
  (remove-duplicates (reduce-list (gen-combi (str->list str))) :test #'equal))

(print (get-str-combi "明日もはれるかな"))
