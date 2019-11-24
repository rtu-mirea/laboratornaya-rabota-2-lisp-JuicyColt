  ;; task 1.1
(defun insert-at (item index list)
  (concatenate 'list
    (subseq list 0 (- index 1))
    (list item)
    (nthcdr (- index 1) list)))

(insert-at 5 2 (list 6 4 3))

;; task 1.2
(defun remove-at (listq pos)
	(if (eql pos 1)
		(cdr listq)
		(cons (car listq) (remove-at (cdr listq) (1- pos)))
	)
)

(remove-at (list 6 4 3) 2)

;; task 1.3
(defun pos (element list)
  (loop for x in list
     counting x into pos
     when (equal element x)
     return pos
     end
     finally (return NIL)))

(pos 7 (list 1 2 3 7 5))
