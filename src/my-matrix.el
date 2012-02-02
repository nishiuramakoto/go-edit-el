
;; See elisp manual
;(require 'cl)
(eval-when-compile (require 'cl))

(defstruct (my-matrix 
	    (:constructor nil)
	    (:constructor make-my-matrix (column row init
						 &optional &aux (data (make-vector (* column row) init)))))
; public
  column row
; private
  data 
  )

;; These accessors are macros so as to make it setf-able by default 
(defmacro mref (m column row)
  `(aref (my-matrix-data ,m) (+ (1- ,column)  (* (my-matrix-column ,m)  (1- ,row) ))))

(defmacro mset (m column row newelt)
  `(let ((x ,newelt))
     (aset (my-matrix-data ,m) (+ (1- ,column)  (* (my-matrix-column ,m)  (1- ,row) )) x)
     x))
  

(defun mfill (m elt)
  (loop for j from 0 to (1- (length (my-matrix-data m)))
	do  (aset (my-matrix-data m) j elt)))
  

(defun my-matrix-print (m &optional field-length)
  (loop for j from 1 to (my-matrix-row m)
	with format-string = (format "%%%ds " (or field-length 3))
	do
	(insert "\n")
	(loop for i from 1 to (my-matrix-column m)
	      do (insert (format format-string (mref m i j))))))

;; matrix arithmetic,determinant,eigenvalues,etc...



(provide 'my-matrix)
