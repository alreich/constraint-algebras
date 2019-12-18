;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;; Period Derivations
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

;(in-package "CONSTRAINT-MAINTENANCE")

#|
;(defvar *snet* #N((:= :< :? :?) (:> := :? :?) (:? :? := :<) (:? :? :> :=)))
(defvar *snet* #N((:= :< :? :?)
		  (:> := :? :?)
		  (:? :? := :<)
		  (:? :? :> :=)))
	   
(defvar *s=net* #N((:=  :<= :?  :? )
		   (:>= :=  :?  :? )
		   (:?  :?  :=  :<=)
		   (:?  :?  :>= := )))


;(defvar *rbnet* #N((:rb= :rb< :rb<=>~ :rb<=>~) (:rb> :rb= :rb<=>~ :rb<=>~) (:rb<=>~ :rb<=>~ :rb= :rb<) (:rb<=>~ :rb<=>~ :rb> :rb=)))

(defvar *rbnet* #N((:rb=    :rb<    :rb<=>~ :rb<=>~)
		   (:rb>    :rb=    :rb<=>~ :rb<=>~)
		   (:rb<=>~ :rb<=>~ :rb=    :rb<   )
		   (:rb<=>~ :rb<=>~ :rb>    :rb=   )))

(defvar *rb=net* #N((:rb=    :rb<=   :rb<=>~ :rb<=>~)
		    (:rb=>   :rb=    :rb<=>~ :rb<=>~)
		    (:rb<=>~ :rb<=>~ :rb=    :rb<=  )
		    (:rb<=>~ :rb<=>~ :rb=>   :rb=   )))

(defvar *lbnet* #N((:lb=    :lb<    :lb<=>~ :lb<=>~)
                   (:lb>    :lb=    :lb<=>~ :lb<=>~)
                   (:lb<=>~ :lb<=>~ :lb=    :lb<   )
                   (:lb<=>~ :lb<=>~ :lb>    :lb=   )))

(defvar *lb=net* #N((:lb=    :lb<=   :lb<=>~ :lb<=>~)
		    (:lb=>   :lb=    :lb<=>~ :lb<=>~)
		    (:lb<=>~ :lb<=>~ :lb=    :lb<=  )
		    (:lb<=>~ :lb<=>~ :lb=>   :lb=   )))

(defvar *panet* #N((:pa= :pa< :pa? :pa?)
		   (:pa> :pa= :pa? :pa?)
		   (:pa? :pa? :pa= :pa<)
		   (:pa? :pa? :pa> :pa=)))
|#

(defun make-workspace-net (x1y1 x1y2 x2y1 x2y2 net)
  (let* ((arr (get-network-arr      net))
	 (alg (get-network-algebra  net))
	 (inv (get-algebra-inverter alg)))
    (progn
      (setf (aref arr 0 2) x1y1)
      (setf (aref arr 2 0) (funcall inv x1y1))
      (setf (aref arr 0 3) x1y2)
      (setf (aref arr 3 0) (funcall inv x1y2))
      (setf (aref arr 1 2) x2y1)
      (setf (aref arr 2 1) (funcall inv x2y1))
      (setf (aref arr 1 3) x2y2)
      (setf (aref arr 3 1) (funcall inv x2y2))
      net)))


;;; The form: (ENUMERATE-SINGLE-RELATION-NETS '(:< := :>) *SNET*) will produce
;;; 13 networks that are in one-to-one equivalence with the 13 interval
;;; relations in Allen's Theory of Time.

(defun ENUMERATE-SINGLE-RELATION-NETS (rels net)
  (let* ((temp-net nil)
	 (net-list nil)
	 (alg      (get-network-algebra net))
	 (enc      (get-algebra-encoder alg)))
    (dolist (r1 rels)
      (dolist (r2 rels)
	(dolist (r3 rels)
	  (dolist (r4 rels)
	    (if (consistentp
		  (setf temp-net
			(propagate
			  (make-workspace-net
			    (funcall enc r1) (funcall enc r2)
			    (funcall enc r3) (funcall enc r4) net))))
		(push temp-net net-list))))))
    (remove-duplicates net-list :test #'equal-nets-p)))


;;; The form: (ENUMERATE-MULTIPLE-RELATION-NETS '(:< := :>) :? *SNET*) will produce
;;; 12 networks that are in one-to-one equivalence with the 12 interval
;;; relations in Matuzsek, et.al.'s Theory of Time.

(defun ENUMERATE-MULTIPLE-RELATION-NETS (rels unknown-element net)
  (let* ((temp-net nil)
	 (net-list nil)
	 (alg      (get-network-algebra net))
	 (enc      (get-algebra-encoder alg))
	 (unk      (funcall enc unknown-element)))
    (dolist (r rels)
      (if (consistentp (setf temp-net
			     (propagate
			       (make-workspace-net
				 (funcall enc r) unk unk unk net))))
	  (push temp-net net-list))
      (if (consistentp (setf temp-net
			     (propagate
			       (make-workspace-net
				 unk (funcall enc r) unk unk net))))
	  (push temp-net net-list))
      (if (consistentp (setf temp-net
			     (propagate
			       (make-workspace-net
				 unk unk (funcall enc r) unk net))))
	  (push temp-net net-list))
      (if (consistentp (setf temp-net
			     (propagate
			       (make-workspace-net
				 unk unk unk (funcall enc r) net))))
	  (push temp-net net-list)))
    (remove-duplicates net-list :test #'equal-nets-p)))


;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;; End of File
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
