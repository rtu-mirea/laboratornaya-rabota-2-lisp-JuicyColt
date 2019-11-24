;; task 3.1
(defun encode (lista)
	(if (eql lista nil) 
		nil
		(if (= (length (first-alike lista)) 1 )
		    (cons (car lista) (encode (except-first-alike lista)))
		    (cons (list (length (first-alike lista)) (car lista)) (encode (except-first-alike lista))))
	)
)

(defun first-alike (lista)
	(cond ((eql lista nil) nil)
		((eql (cdr lista) nil) lista)
		((equal (car lista) (cadr lista))
			(cons (car lista) (first-alike (cdr lista))))
		(t (list (car lista)))
	)
)

(defun except-first-alike (lista)
	(cond ((eql lista nil) nil)
		((eql (cdr lista) nil) nil)
		((equal (car lista) (cadr lista))
			(except-first-alike (cdr lista)))
		(t (cdr lista))
	)
)

(encode (list 1 1 1 3 111 111 111 55 55 6 7 2 2 2))

;;task 3.2
(defun decompress(lst)
  (loop for element in lst
    if (integerp element) collect element
    if (listp element) nconc (make-list (car element) 
	:initial-element (cadr element))))

(decompress (list (list 3 1) 3 (list 3 111) (list 2 55) 6 7 (list 3 2)))
