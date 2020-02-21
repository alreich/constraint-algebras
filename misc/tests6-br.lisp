(in-package "CONSTRAINT-MAINTENANCE")

;;;----------------------------------------------------------------------

(defun SETUP-TEST-RBNET (intervals points constraints)
  (let ((rbnet (initialize-constraint-network 'right-branching-interval-&-point
					      :obj-eq-test #'string=)))
    (register-constrained-objects intervals :interval rbnet)
    (register-constrained-objects points    :point    rbnet)
    (assert-constraints constraints rbnet :propagate-p nil)
    (propagate-constraints rbnet)
    rbnet))


(defun SETUP-RBNET (&key intervals points constraints (propagate? t))
  (let ((rbnet (initialize-constraint-network 'right-branching-interval-&-point)))
    (if intervals
	(register-constrained-objects intervals :interval rbnet))
    (if points
	(register-constrained-objects points :point rbnet))
    (if constraints
	(assert-constraints constraints rbnet :propagate-p propagate?))
    rbnet))

;;;----------------------------------------------------------------------

;;;                                  +------------[C]
;;;                                  |    i5
;;;                     +------------o------------[B]
;;;                     |    i3     p2    i4
;;;         |-----------o-------------------------[A]
;;;              i1    p1           i2
;;;
;;;
;;;            i1   i2   i3   i4   i5   p1   p2
;;;          +----------------------------------
;;;       i1 | e    m    m    ?    ?    pfi  ?
;;;       i2 | mi   e    rs   ?    ?    psi  ?
;;;       i3 | mi   rs   e    m    m    psi  pfi
;;;       i4 | ?    ?    mi   e    rs   ?    psi
;;;       i5 | ?    ?    mi   rs   e    ?    psi
;;;       p1 | pf   ps   ps   ?    ?    pe   ?
;;;       p2 | ?    ?    pf   ps   ps   ?    pe


(defparameter *INTERVAL-OBJECTS* '("i1" "i2" "i3" "i4" "i5"))
(defparameter    *POINT-OBJECTS* '("p1" "p2"))


(defparameter *CONSTRAINTS*

  (list '(:=<M   "i1" "i2")
	'(:=<M   "i1" "i3")
	'(:=<PFI "i1" "p1")

	'(:=<MI  "i2" "i1")
	'(:=<RS  "i2" "i3")
	'(:=<PSI "i2" "p1")

	'(:=<MI  "i3" "i1")
	'(:=<RS  "i3" "i2")
	'(:=<M   "i3" "i4")
	'(:=<M   "i3" "i5")
	'(:=<PSI "i3" "p1")
>	'(:=<PFI "i3" "p2")

	'(:=<MI  "i4" "i3")
	'(:=<E   "i4" "i4")
	'(:=<RS  "i4" "i5")
	'(:=<PSI "i4" "p2")

	'(:=<MI  "i5" "i3")
	'(:=<RS  "i5" "i4")
	'(:=<E   "i5" "i5")
	'(:=<PSI "i5" "p2")

	'(:=<PF "p1" "i1")
	'(:=<PS "p1" "i2")
	'(:=<PS "p1" "i3")
	'(:=<PE "p1" "p1")

	'(:=<PF "p2" "i3")
	'(:=<PS "p2" "i4")
	'(:=<PS "p2" "i5")
	'(:=<PE "p2" "p2")

	))


(setf test-rbnet (setup-test-rbnet *INTERVAL-OBJECTS*
				   *POINT-OBJECTS*
				   *CONSTRAINTS*))
(pprint (get-constraints test-rbnet))


;;; (describe test-rbnet)
;;;
;;; #<Constraint-Network #X10BEE2B>
;;;     is an instance of the class CONSTRAINT-NETWORK:
;;;  The following slots have allocation :INSTANCE:
;;;  CONSTRAINT-MATRIX
;;;     #N((:=<E  :=<M   :=<M  :=<B  :=<B  :=<PFI :=<B  )
;;;        (:=<MI :=<E   :=<RS :=<RB :=<RB :=<PSI :=<RB )
;;;        (:=<MI :=<RS  :=<E  :=<M  :=<M  :=<PSI :=<PFI)
;;;        (:=<BI :=<RBI :=<MI :=<E  :=<RS :=<BI  :=<PSI)
;;;        (:=<BI :=<RBI :=<MI :=<RS :=<E  :=<BI  :=<PSI)
;;;        (:=<PF :=<PS  :=<PS :=<B  :=<B  :=<PE  :=<B  )
;;;        (:=<BI :=<RBI :=<PF :=<PS :=<PS :=<BI  :=<PE))
;;;  CONSTRAINED-OBJECTS    ("i1" "i2" "i3" "i4" "i5" "p1" "p2")
;;;  ALG-NAME               RIGHT-BRANCHING-INTERVAL-&-POINT
;;;  OBJ-EQ-TEST            #<Compiled-Function STRING= 5FE41F>
;;;  OBJ-ID-KEY             #<Compiled-Function IDENTITY 5F435F>


;;;----------------------------------------------------------------------


(defparameter *INTERVAL-OBJECTS-2* '("i1" "i2" "i3"))
(defparameter    *POINT-OBJECTS-2* '("p1"))


(defparameter *CONSTRAINTS-2*
  (list '(:=<M   "i1" "i2")
	'(:=<M   "i1" "i3")
	'(:=<PFI "i1" "p1")
	'(:=<RS  "i2" "i3")
	'(:=<PSI "i2" "p1")
	'(:=<PSI "i3" "p1")
	))


(setf test-rbnet-2 (setup-test-rbnet *INTERVAL-OBJECTS-2*
				     *POINT-OBJECTS-2*
				     *CONSTRAINTS-2*))


(pprint (get-constraints test-rbnet-2))


;;; (describe test-rbnet-2)
;;; 
;;; #<Constraint-Network #XFEED2B>
;;;     is an instance of the class CONSTRAINT-NETWORK:
;;;  The following slots have allocation :INSTANCE:
;;;  CONSTRAINT-MATRIX
;;;      #N((:=<E  :=<M  :=<M  :=<PFI)
;;;	    (:=<MI :=<E  :=<RS :=<PSI)
;;;	    (:=<MI :=<RS :=<E  :=<PSI)
;;;	    (:=<PF :=<PS :=<PS :=<PE ))
;;;  CONSTRAINED-OBJECTS    ("i1" "i2" "i3" "p1")
;;;  ALG-NAME               RIGHT-BRANCHING-INTERVAL-&-POINT
;;;  OBJ-EQ-TEST            #<Compiled-Function STRING= 5FE41F>
;;;  OBJ-ID-KEY             #<Compiled-Function IDENTITY 5F435F>

;;;----------------------------------------------------------------------