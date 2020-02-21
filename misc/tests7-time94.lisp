(in-package "CONSTRAINT-MAINTENANCE")


(defun SETUP-TEST-RBNET (&key intervals points constraints (propagate? t))
  (let ((rbnet (initialize-constraint-network 'right-branching-interval-&-point)))
    (if intervals
	(register-constrained-objects intervals :interval rbnet))
    (if points
	(register-constrained-objects points :point rbnet))
    (if constraints
	(assert-constraints constraints rbnet :propagate-p propagate?))
    rbnet))

;;;---------------------------------------------------------------------------

(defparameter *INTERVALS* '(:A :B1 :B2))

(defparameter *CONSTRAINTS*
  (list '(:=<M  :A  :B1)
	'(:=<M  :A  :B2)
	))

(setf RBNET (setup-test-rbnet :intervals   *INTERVALS*
				:constraints *CONSTRAINTS*))

(pprint (get-constraints RBNET))

;;; ((:=<M :A :B1)
;;;  (:=<M :A :B2)
;;;  ((:=<E :=<RS :=<S :=<SI) :B1 :B2))

(assert-constraint '(:=<r~ :B1 :B2) rbnet)
;;; #<Constraint-Network #XFD34C3>
;;; NIL
;;; "Constraint is immediately inconsistent with an existing constraint."

;;;---------------------------------------------------------------------------

(defparameter *INTERVALS* '(:B1 :B2))

(defparameter *POINTS* '(:P1 :P2))

(defparameter *CONSTRAINTS*
  (list '(:=<PS  :P1  :B1)
	'(:=<PS  :P2  :B2)
	))

(setf RBNET (setup-test-rbnet :intervals   *INTERVALS*
			      :points      *POINTS*
			      :constraints *CONSTRAINTS*))

(pprint (get-constraints RBNET))
;;; (
;;;  ((:=<B :=<BI :=<D :=<DI :=<E :=<F :=<FI :=<M :=<MI :=<O :=<OI :=<RB :=<RBI :=<RO
;;; 	:=<ROI :=<RS :=<R~ :=<S :=<SI)	; 19 relations
;;;   :B1 :B2)
;;;  (:=<PSI :B1 :P1)
;;;  ((:=<B :=<BI :=<DI :=<PFI :=<PSI :=<RB :=<R~) :B1 :P2)
;;;  ((:=<B :=<BI :=<DI :=<PFI :=<PSI :=<RB :=<R~) :B2 :P1)
;;;  (:=<PSI :B2 :P2)
;;;  ((:=<B :=<BI :=<PE :=<R~) :P1 :P2)
;;;  )


(assert-constraint '(:=<r~ :P1 :P2) rbnet)
;;; #<Constraint-Network #XFFC35B>
;;; T
;;; "Constraint has been successfully added and propagated in the network."

(get-constraints RBNET)
;;; (
;;;  (:=<R~ :B1 :B2)
;;;  (:=<PSI :B1 :P1)
;;;  (:=<R~ :B1 :P2)
;;;  (:=<R~ :B2 :P1)
;;;  (:=<PSI :B2 :P2)
;;;  (:=<R~ :P1 :P2)
;;;  )

;;;---------------------------------------------------------------------------

(defparameter *INTERVALS* '(:B1 :B2))

(defparameter *POINTS* '(:P1 :P2))

(defparameter *CONSTRAINTS*
  (list '(:=<PS  :P1  :B1)
	'(:=<PS  :P2  :B2)
	))

(setf RBNET (setup-test-rbnet :intervals   *INTERVALS*
			      :points      *POINTS*
			      :constraints *CONSTRAINTS*))

(assert-constraint '(:=<PE :P1 :P2) rbnet)

(get-constraints RBNET)
;;; (
;;;  ((:=<E :=<RS :=<S :=<SI) :B1 :B2)
;;;  (:=<PSI :B1 :P1)
;;;  (:=<PSI :B1 :P2)
;;;  (:=<PSI :B2 :P1)
;;;  (:=<PSI :B2 :P2)
;;;  (:=<PE :P1 :P2)
;;;  )
