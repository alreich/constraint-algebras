;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                           UTILITIES
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

;(in-package "CONSTRAINT-MAINTENANCE")

;;;---------------------------------------------------------------------------

;;; Remember FORTRAN?  Well, here's a flash from the past...

;;; NOTE: The following construct could be replaced with a loop of the
;;;       form (loop for VAR from START to END do BODY).  In
;;;       uncompiled form, the loop is slightly faster (approx 6%),
;;;       but it uses much more space (approx 250%).  In compiled form
;;;       the two are almost identical.  For now I'll choose DOLOOP's
;;;       large savings in consing over LOOP's small savings in speed.

(defmacro DOLOOP (var start end &body body)
  "Basically does the same thing as Fortran's do loop.  This form is
slower than a LOOP form--approx 6%--but does much less consing space
--- loop is 250% higher."
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,end))
     ,@body))

;;;---------------------------------------------------------------------------

;;; A utility for turning an array into a list:

(defun LIST-ROW (arr row col-dim)
  "An auxillary function used by LIST-ARRAY.  It works on a single row."
  (let ((row-list nil))
    (dotimes (c col-dim (reverse row-list))
      (push (aref arr row c) row-list))))


(defun LIST-ARRAY (arr)
  "Turns an array into a list of lists.  Each list in the list corresponds
to a row in the array.  The output of this is suitable as the INITIAL-CONTENTS
of make-array."
  (let* ((row-dim (array-dimension arr 0))
	 (col-dim (array-dimension arr 1))
	 (net-list nil))
    (dotimes (row row-dim (reverse net-list))
      (push (list-row arr row col-dim) net-list))))

;;;---------------------------------------------------------------------------

(defun MAP-ALIST (function alist)
  "Map over an alist and replace each datum with (function datum).
Returns a new alist with the keys in the orginal key order."
  (let ((new-alist nil))
    (dolist (item alist (reverse new-alist))
      (setf new-alist
	    (acons (first item)
		   (funcall function (cdr item))
		   new-alist)))))


(defun MAPPEND (fn list)
  "Append the results of calling fn on each element of list.
Like mapcon, but uses append instead of nconc.  This comes from
Norvig's book."
  (apply #'append (mapcar fn list)))


(defun MKLIST (x)
  "If x is a list return it, otherwise return the list of x.
This comes from Norvig's book."
  (if (listp x) x (list x)))

(defun FLATTEN (exp)                    ; 
  "Get rid of embedded lists to one level only.  This comes from
Norvig's book"
  (mappend #'mklist exp))

;;;---------------------------------------------------------------------------
;;;                           END OF FILE
;;;---------------------------------------------------------------------------
