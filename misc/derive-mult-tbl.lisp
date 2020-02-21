;;; -*- Package: CONSTRAINT-MAINTENANCE; Mode: LISP; Base: 10; Syntax: Common-Lisp -*-

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                 MULTIPLICATION TABLE DERIVATION
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(in-package "CONSTRAINT-MAINTENANCE")

;;;---------------------------------------------------------------------------
;;; Old Mult Derivation Code (STILL IN USE)
;;;---------------------------------------------------------------------------

;;; Multiplication Derivation Experimental Code

(defvar *period* nil)
(defvar *point* nil)
(defvar *perORpt* nil)
(defvar *any* nil)
(defvar	*shell* nil)
(defvar	*shell3* nil)
(defvar *shell3* nil)

#|
(setf *period* #N((:= :<)
                  (:> :=)))
	   
(setf *point* #N((:= :=)
                 (:= :=)))
	   
(setf *perORpt* #N((:=  :<=)
                   (:>= := )))

;;; For use with the SCALAR algebra   
(setf *any* #N((:? :?)
               (:? :?)))
	   
(setf *any* #N((:RB<=>~ :RB<=>~)
	       (:RB<=>~ :RB<=>~)))


(setf *any* #N((:LB<=>~ :LB<=>~)
	       (:LB<=>~ :LB<=>~)))


;;; For use with the SCALAR algebra   
(setf *shell* #N((:? :? :? :? :? :?)
                 (:? :? :? :? :? :?)
                 (:? :? :? :? :? :?)
                 (:? :? :? :? :? :?)
                 (:? :? :? :? :? :?)
                 (:? :? :? :? :? :?)))
	   
(setf *shell* #N((:RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~)
		 (:RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~)
		 (:RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~)
		 (:RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~)
		 (:RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~)
		 (:RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~ :RB<=>~)))

(setf *shell* #N((:LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~)
		 (:LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~)
		 (:LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~)
		 (:LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~)
		 (:LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~)
		 (:LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~ :LB<=>~)))

(defvar	*shell3* #N((:? :? :? :? :? :? :? :?)
                    (:? :? :? :? :? :? :? :?)
                    (:? :? :? :? :? :? :? :?)
                    (:? :? :? :? :? :? :? :?)
                    (:? :? :? :? :? :? :? :?)
                    (:? :? :? :? :? :? :? :?)
                    (:? :? :? :? :? :? :? :?)
                    (:? :? :? :? :? :? :? :?)))
|#
;;;---------------------------------------------------------------------------

(defun extract-partition (net row-part col-part)
  (let* ((arr (get-network-arr net))
	 (r0  (* (1- row-part) 2))
	 (c0  (* (1- col-part) 2))
	 (r1  (1+ r0))
	 (c1  (1+ c0)))
    (list (aref arr r0 c0)
	  (aref arr r0 c1)
	  (aref arr r1 c0)
	  (aref arr r1 c1))))

(defun insert-partition (part net row-part col-part)
  (let* ((arr (get-network-arr net))
	 (r0  (* (1- row-part) 2))
	 (c0  (* (1- col-part) 2))
	 (r1  (1+ r0))
	 (c1  (1+ c0)))
    (setf (aref arr r0 c0) (first  part))
    (setf (aref arr r0 c1) (second part))
    (setf (aref arr r1 c0) (third  part))
    (setf (aref arr r1 c1) (fourth part))
    net))

(defun make-prod-net (net1 net2 sh)
  (insert-partition (extract-partition net1  1 1) sh 1 1)
  (insert-partition (extract-partition net1  1 2) sh 1 2)
  (insert-partition (extract-partition net1  2 1) sh 2 1)
  (insert-partition (extract-partition net1  2 2) sh 2 2)
  (insert-partition (extract-partition net2  1 2) sh 2 3)
  (insert-partition (extract-partition net2  2 1) sh 3 2)
  (insert-partition (extract-partition net2  2 2) sh 3 3)
  (insert-partition (extract-partition *any* 1 1) sh 1 3)
  (insert-partition (extract-partition *any* 1 1) sh 3 1)
  sh)

(defun make-prod-net3 (net1 net2 net3 sh2 sh3)
  (let ((net12 (mult-nets net1 net2 sh2))
	(net23 (mult-nets net2 net3 sh2)))
    (insert-partition (extract-partition net1  1 1) sh3 1 1)
    (insert-partition (extract-partition net1  1 2) sh3 1 2)
    (insert-partition (extract-partition net1  2 1) sh3 2 1)
    (insert-partition (extract-partition net1  2 2) sh3 2 2)
    (insert-partition (extract-partition net2  1 2) sh3 2 3)
    (insert-partition (extract-partition net2  2 1) sh3 3 2)
    (insert-partition (extract-partition net2  2 2) sh3 3 3)
    (insert-partition (extract-partition net3  1 2) sh3 3 4)
    (insert-partition (extract-partition net3  2 1) sh3 4 3)
    (insert-partition (extract-partition net3  2 2) sh3 4 4)
    (insert-partition (extract-partition net12 1 3) sh3 1 3)
    (insert-partition (extract-partition net12 3 1) sh3 3 1)
    (insert-partition (extract-partition net23 1 3) sh3 2 4)
    (insert-partition (extract-partition net23 3 1) sh3 4 2)
    (insert-partition (extract-partition *any* 1 1) sh3 1 4)
    (insert-partition (extract-partition *any* 1 1) sh3 4 1))
  sh3)

(defun mult-nets (net1 net2 prod)
  (propagate (make-prod-net net1 net2 prod)))

(defun mult-nets3 (net1 net2 net3 prod2 prod3)
  (propagate (make-prod-net3 net1 net2 net3 prod2 prod3)))

(defun subequivalent-p (rel1 rel2)
  (if (<= rel1 rel2)
      (logtest rel1 rel2)))

(defun subequivalent-partition-p (part1 part2)
  (not (member nil (mapcar #'subequivalent-p part1 part2))))

(defun multiply-relations (rel1 rel2 element-list)
  (let* ((net1 (cdr (assoc rel1 element-list)))
	 (net2 (cdr (assoc rel2 element-list)))
	 (prod (mult-nets net1 net2 *shell*))
	 (prod-part (extract-partition prod 1 3))
	 (product nil))
    (dolist (element element-list (sort product #'string<))
      (let ((rel (car element))
	    (net (cdr element)))
	(if (subequivalent-partition-p (extract-partition net 1 2) prod-part)
	    (push rel product))))))

(defun multiply-relations3 (rel1 rel2 rel3 element-list)
  (let* ((net1 (cdr (assoc rel1 element-list)))
	 (net2 (cdr (assoc rel2 element-list)))
	 (net3 (cdr (assoc rel3 element-list)))
	 (prod (mult-nets3 net1 net2 net3 *shell* *shell3*))
	 (prod-part (extract-partition prod 1 4))
	 (product nil))
    (dolist (element element-list (sort product #'string<))
      (let ((rel (car element))
	    (net (cdr element)))
	(if (subequivalent-partition-p (extract-partition net 1 2) prod-part)
	    (push rel product))))))

;;;---------------------------------------------------------------------------

;;;; SOME TESTS:
;
;(multiply-relations :faf :saf *endpt-pt-nets-WITHOUT-equals*)
;; ==> (:FAF :SAF :SWF)
;
;(setq faf (cdr (assoc :faf *endpt-pt-nets-WITHOUT-equals*)))
;;#N((:= :< :? :?)
;;   (:> := :> :>)
;;   (:? :< := :<)
;;   (:? :< :> :=))
;
;(setq saf (cdr (assoc :saf *endpt-pt-nets-WITHOUT-equals*)))
;;#N((:= :< :> :>)
;;   (:> := :> :>)
;;   (:< :< := :<)
;;   (:< :< :> :=))
;
;(mult-nets faf saf *shell*)
;#N((:= :< :? :? :? :?)
;   (:> := :> :> :> :>)
;   (:? :< := :< :> :>)
;   (:? :< :> := :> :>)
;   (:? :< :< :< := :<)
;   (:? :< :< :< :> :=))

;;;---------------------------------------------------------------------------

;(defun print-mult-table (stream element-list)
;  (let ((mult-table (create-mult-table element-list)))
;    (print-column-labels stream mult-table)
;    (dolist (element mult-table)
;      (print-table-row stream mult-table))
;    (finish-printing-table stream)))
;
;(defun print-column-labels (stream element-list)
;  (format stream "~%")
;  (format stream "      ")
;  (dolist (element element-list)
;    (format stream "~

(defun CREATE-MULT-TABLE (element-list)
  (let* ((mult-table nil)
	 (col-labels nil)
	 (row        nil)
	 (len-alg    (length element-list)))
    (dolist (i element-list)
      (push (car i) col-labels))
    (push
     (setq col-labels (sort col-labels #'string<))
     ;;(sort col-labels #'string<)
     ;;(setq col-labels (reverse col-labels))
     mult-table)
    (dolist (i1 col-labels)
      (setq row nil)
      (push i1 row)
      (dolist (i2 col-labels)
	(let* ((prod (multiply-relations i1 i2 element-list))
	       (len  (length prod)))
	  (push (cond
                 ((= len len-alg) :?)
                 ((= len 1) (car prod))
                 (t prod))
		row)))
      (push (reverse row) mult-table))
    (reverse mult-table)))

;;;---------------------------------------------------------------------------
;;; New Mult Derivation Code (INCOMPLETE --- NOT IN USE)
;;;---------------------------------------------------------------------------

;;(defun extract-partition (net row-part col-part)
;;  (let* ((arr (get-network-arr net))
;;	 (r0  (* (1- row-part) 2))
;;	 (c0  (* (1- col-part) 2))
;;	 (r1  (1+ r0))
;;	 (c1  (1+ c0)))
;;    (list (aref arr r0 c0)
;;	  (aref arr r0 c1)
;;	  (aref arr r1 c0)
;;	  (aref arr r1 c1))))
;
;(defun PARTITION-DIMS (arr-row-dim arr-col-dim
;		       part-row-beg part-row-end part-col-beg part-col-end)
;  (if (not (or (<= 0 part-row-beg part-row-end arr-row-dim)
;	       (<= 0 part-col-beg part-col-end arr-col-dim)))
;      (error "Inconsistent array dimensions for extracting partition."))
;  (list (1+ (- part-row-end part-row-beg))
;	(1+ (- part-col-end part-col-beg))))
;
;(defun EXTRACT-PARTITION (array row-begin row-end col-begin col-end)
;  (let* ((row-dim  (array-dimension array 0))
;	 (col-dim  (array-dimension array 1))
;	 (new-dims (partition-dims row-dim col-dim
;				   row-begin row-end col-begin col-end))
;	 (new-arr  (make-array new-dims
;			       :adjustable t
;			       :element-type 'integer)))
;;;;<NEED TO FINISH THIS>
;    ))
;
;;(defun insert-partition (part net row-part col-part)
;;  (let* ((arr (get-network-arr net))
;;	 (r0  (* (1- row-part) 2))
;;	 (c0  (* (1- col-part) 2))
;;	 (r1  (1+ r0))
;;	 (c1  (1+ c0)))
;;    (setf (aref arr r0 c0) (first  part))
;;    (setf (aref arr r0 c1) (second part))
;;    (setf (aref arr r1 c0) (third  part))
;;    (setf (aref arr r1 c1) (fourth part))
;;    net))

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;; End of File
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
