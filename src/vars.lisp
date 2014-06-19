(in-package :n-gram)

(defvar *group-dir* "../group")
(defvar *blacklist* (make-hash-table :test #'equal))
(defvar *bladklist-dir* (car (directory (concatenate 'string *group-dir* "/.blacklist"))))

;;;tags list
(defvar *tags* 
  (mapcar #'(lambda (tag)
			  (intern (string-upcase (subseq (namestring tag) 31 (1- (length (namestring tag)))))))
		  (remove-if #'(lambda (dir)
						 (let ((str (namestring dir)))
						   (if (not (equal (char str (1- (length str))) #\/)) t)))
						   (directory (concatenate 'string *group-dir* "/*")))))
