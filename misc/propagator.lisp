;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                            PROPAGATOR
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(in-package "CONSTRAINT-MAINTENANCE")

(defun SQUARE-ARRAY (arr row-dim col-dim dims zero-el adder multiplier)
  (let* ((sum nil)
	 (arr-out (make-array dims
			      :adjustable t
			      :element-type 'integer)))
    (dotimes (i row-dim)
      (dotimes (j col-dim)
	(setf sum zero-el)
	(setf (aref arr-out i j)
	      (dotimes (k col-dim sum)
		(setf sum
		      (funcall adder
			       sum
			       (funcall multiplier
					(aref arr i k)
					(aref arr k j))))))))
    arr-out))

(defun EQUAL-ARRAYS-P (arr1 arr2 row-dim col-dim equal-p)	;Assumes dimensions are equal
  (let* ((ret-val t))
    (dotimes (i row-dim)
      (dotimes (j col-dim)
	(setq ret-val (and ret-val
			   (funcall equal-p
				    (aref arr1 i j)
				    (aref arr2 i j))))))
    ret-val))

(defun PROPAGATE-AUX (new-arr old-arr row-dim col-dim dims
		      zero-el adder multiplier equal-p alg)
  (if (equal-arrays-p new-arr old-arr row-dim col-dim equal-p)
      (make-network :algebra alg
		    :arr     new-arr)
      (propagate-aux (square-array new-arr row-dim col-dim dims zero-el adder multiplier)
		     new-arr row-dim col-dim dims zero-el adder multiplier equal-p alg)))

(defun PROPAGATE (net)
  (let* ((arr        (get-network-arr net  ))
	 (row-dim    (array-dimension arr 0))
	 (col-dim    (array-dimension arr 1))
	 (dims       (list row-dim col-dim))
	 (alg        (get-network-algebra    net))
	 (zero-el    (get-algebra-zero       alg))
	 (multiplier (get-algebra-multiplier alg))
	 (adder      (get-algebra-adder      alg))
	 (equal-p    (get-algebra-equal-p    alg)))
    (propagate-aux (square-array arr row-dim col-dim dims zero-el adder multiplier)
		   arr
		   row-dim col-dim dims zero-el adder multiplier equal-p alg)))

