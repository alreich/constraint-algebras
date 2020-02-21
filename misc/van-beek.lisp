(defmacro doloop (var start end &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,end))
     ,@body))

(defun related-paths (i j n)
  (let ((rp nil))
    (doloop l 1 n
      (let ((l-1 (1- l)))
	(doloop k 1 l-1
	  (if (and (< k l) (/= k i) (/= k j) (/= l i) (/= l j) (/= i j))
	      (push (list k i j l) rp)))))
    (doloop l 1 n
      (doloop k 1 n
	(if (and (/= k i) (/= k j) (/= k l) (/= l i) (/= l j) (/= i j))
	    (progn
	      (push (list k l i j) rp)
	      (push (list i j l k) rp)))))
    rp))

(defun list-equal-p (l1 l2)
  (let ((result t)
	(n (length l1)))
    (dotimes (i n)
      (setq result (and result (= (nth i l1) (nth i l2)))))
    result))

(related-paths 1 2 4)
;((1 2 4 3) (3 4 1 2) (1 2 3 4) (4 3 1 2) (3 1 2 4))
(related-paths 1 3 4)
;((1 3 4 2) (2 4 1 3) (1 3 2 4) (4 2 1 3) (2 1 3 4))
(related-paths 1 4 4)
;((1 4 3 2) (2 3 1 4) (1 4 2 3) (3 2 1 4) (2 1 4 3))
(related-paths 2 3 4)
;((2 3 4 1) (1 4 2 3) (2 3 1 4) (4 1 2 3) (1 2 3 4))
(related-paths 2 4 4)
;((2 4 3 1) (1 3 2 4) (2 4 1 3) (3 1 2 4) (1 2 4 3))
(related-paths 3 4 4)
;((3 4 2 1) (1 2 3 4) (3 4 1 2) (2 1 3 4) (1 3 4 2))


(setq indices (union
		(union
		  (union
		    (union
		      (union
			(related-paths 1 2 4)
			(related-paths 1 3 4) :test #'list-equal-p)
		      (related-paths 1 4 4) :test #'list-equal-p)
		    (related-paths 2 3 4) :test #'list-equal-p)
		  (related-paths 2 4 4) :test #'list-equal-p)
		(related-paths 3 4 4) :test #'list-equal-p))

;((3 4 2 1) (2 4 3 1) (4 1 2 3) (2 3 4 1) (2 1 4 3) (3 2 1 4)
; (1 4 2 3) (2 3 1 4) (1 4 3 2) (2 1 3 4) (4 2 1 3) (1 3 2 4)
; (2 4 1 3) (1 3 4 2) (1 2 4 3) (3 4 1 2) (1 2 3 4) (4 3 1 2)
; (3 1 2 4))


(sort
  (sort 
    (sort 
      (sort
	indices
	#'< :key #'fourth)
      #'< :key #'third)
    #'< :key #'second)
  #'< :key #'first)

;((1 2 3 4) (1 2 4 3) (1 3 2 4) (1 3 4 2) (1 4 2 3) (1 4 3 2)
; (2 1 3 4) (2 1 4 3) (2 3 1 4) (2 3 4 1) (2 4 1 3) (2 4 3 1)
; (3 1 2 4) (3 2 1 4) (3 4 1 2) (3 4 2 1) (4 1 2 3) (4 2 1 3)
; (4 3 1 2))
