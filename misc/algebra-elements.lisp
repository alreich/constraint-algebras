;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

(in-package "CONSTRAINT-MAINTENANCE")

;;;----------------------------------------------------------------------
;;; (ENUMERATE-SINGLE-RELATION-NETS '(:< := :>) *SNET*)

(defparameter
  *LINEAR-INTERVALS*                    ; Allen's 13 relations
  '(
    #N((:= :< :> :>) (:> := :> :>) (:< :< := :<) (:< :< :> :=))
    #N((:= :< :> :=) (:> := :> :>) (:< :< := :<) (:= :< :> :=))
    #N((:= :< :> :<) (:> := :> :>) (:< :< := :<) (:> :< :> :=))
    #N((:= :< :> :<) (:> := :> :=) (:< :< := :<) (:> := :> :=))
    #N((:= :< :> :<) (:> := :> :<) (:< :< := :<) (:> :> :> :=))
    #N((:= :< := :<) (:> := :> :>) (:= :< := :<) (:> :< :> :=))
    #N((:= :< := :<) (:> := :> :=) (:= :< := :<) (:> := :> :=))
    #N((:= :< := :<) (:> := :> :<) (:= :< := :<) (:> :> :> :=))
    #N((:= :< :< :<) (:> := :> :>) (:> :< := :<) (:> :< :> :=))
    #N((:= :< :< :<) (:> := :> :=) (:> :< := :<) (:> := :> :=))
    #N((:= :< :< :<) (:> := :> :<) (:> :< := :<) (:> :> :> :=))
    #N((:= :< :< :<) (:> := := :<) (:> := := :<) (:> :> :> :=))
    #N((:= :< :< :<) (:> := :< :<) (:> :> := :<) (:> :> :> :=))
    ))
;;;----------------------------------------------------------------------
;;; (ENUMERATE-SINGLE-RELATION-NETS '(:< := :>) *S=NET*)

(defparameter 
  *LINEAR-INTERVALS-&-POINTS*           ; 18 relations
  '(
    #N((:= :<= :> :>) (:>= := :> :>) (:< :< := :<=) (:< :< :>= :=))
    #N((:= :<  :> :=) (:>  := :> :>) (:< :< := :< ) (:= :< :>  :=))
    #N((:= :=  :> :=) (:=  := :> :=) (:< :< := :< ) (:= := :>  :=))
    #N((:= :<  :> :<) (:>  := :> :>) (:< :< := :< ) (:> :< :>  :=))
    #N((:= :<  :> :<) (:>  := :> :=) (:< :< := :< ) (:> := :>  :=))
    #N((:= :<= :> :<) (:>= := :> :<) (:< :< := :< ) (:> :> :>  :=))
    #N((:= :<  := :=) (:>  := :> :>) (:= :< := := ) (:= :< :=  :=))
    #N((:= :=  := :=) (:=  := := :=) (:= := := := ) (:= := :=  :=))
    #N((:= :<  := :<) (:>  := :> :>) (:= :< := :< ) (:> :< :>  :=))
    #N((:= :<  := :<) (:>  := :> :=) (:= :< := :< ) (:> := :>  :=))
    #N((:= :<  := :<) (:>  := :> :<) (:= :< := :< ) (:> :> :>  :=))
    #N((:= :=  := :<) (:=  := := :<) (:= := := :< ) (:> :> :>  :=))
    #N((:= :<  :< :<) (:>  := :> :>) (:> :< := :<=) (:> :< :>= :=))
    #N((:= :<  :< :<) (:>  := :> :=) (:> :< := :< ) (:> := :>  :=))
    #N((:= :<  :< :<) (:>  := :> :<) (:> :< := :< ) (:> :> :>  :=))
    #N((:= :<  :< :<) (:>  := := :=) (:> := := := ) (:> := :=  :=))
    #N((:= :<  :< :<) (:>  := := :<) (:> := := :< ) (:> :> :>  :=))
    #N((:= :<= :< :<) (:>= := :< :<) (:> :> := :<=) (:> :> :>= :=))
    ))
;;;----------------------------------------------------------------------
;;; (ENUMERATE-SINGLE-RELATION-NETS '(:RB< :RB= :RB> :RB~) *RBNET*)

(defparameter 
  *RIGHT-BRANCHING-INTERVALS*           ; 19 relations
  '(
    #N((:RB= :RB< :RB~ :RB~) (:RB> :RB= :RB~ :RB~) (:RB~ :RB~ :RB= :RB<) (:RB~ :RB~ :RB> :RB=))
    #N((:RB= :RB< :RB> :RB~) (:RB> :RB= :RB> :RB~) (:RB< :RB< :RB= :RB<) (:RB~ :RB~ :RB> :RB=))
    #N((:RB= :RB< :RB> :RB>) (:RB> :RB= :RB> :RB>) (:RB< :RB< :RB= :RB<) (:RB< :RB< :RB> :RB=))
    #N((:RB= :RB< :RB> :RB=) (:RB> :RB= :RB> :RB>) (:RB< :RB< :RB= :RB<) (:RB= :RB< :RB> :RB=))
    #N((:RB= :RB< :RB> :RB<) (:RB> :RB= :RB> :RB~) (:RB< :RB< :RB= :RB<) (:RB> :RB~ :RB> :RB=))
    #N((:RB= :RB< :RB> :RB<) (:RB> :RB= :RB> :RB>) (:RB< :RB< :RB= :RB<) (:RB> :RB< :RB> :RB=))
    #N((:RB= :RB< :RB> :RB<) (:RB> :RB= :RB> :RB=) (:RB< :RB< :RB= :RB<) (:RB> :RB= :RB> :RB=))
    #N((:RB= :RB< :RB> :RB<) (:RB> :RB= :RB> :RB<) (:RB< :RB< :RB= :RB<) (:RB> :RB> :RB> :RB=))
    #N((:RB= :RB< :RB= :RB<) (:RB> :RB= :RB> :RB~) (:RB= :RB< :RB= :RB<) (:RB> :RB~ :RB> :RB=))
    #N((:RB= :RB< :RB= :RB<) (:RB> :RB= :RB> :RB>) (:RB= :RB< :RB= :RB<) (:RB> :RB< :RB> :RB=))
    #N((:RB= :RB< :RB= :RB<) (:RB> :RB= :RB> :RB=) (:RB= :RB< :RB= :RB<) (:RB> :RB= :RB> :RB=))
    #N((:RB= :RB< :RB= :RB<) (:RB> :RB= :RB> :RB<) (:RB= :RB< :RB= :RB<) (:RB> :RB> :RB> :RB=))
    #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB~ :RB~) (:RB> :RB~ :RB= :RB<) (:RB> :RB~ :RB> :RB=))
    #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB> :RB~) (:RB> :RB< :RB= :RB<) (:RB> :RB~ :RB> :RB=))
    #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB> :RB>) (:RB> :RB< :RB= :RB<) (:RB> :RB< :RB> :RB=))
    #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB> :RB=) (:RB> :RB< :RB= :RB<) (:RB> :RB= :RB> :RB=))
    #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB> :RB<) (:RB> :RB< :RB= :RB<) (:RB> :RB> :RB> :RB=))
    #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB= :RB<) (:RB> :RB= :RB= :RB<) (:RB> :RB> :RB> :RB=))
    #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB< :RB<) (:RB> :RB> :RB= :RB<) (:RB> :RB> :RB> :RB=))
    ))

;;; Note what happens if we leave the relation :RB~ out of the 'enumerate'
;;; computation.  We get the 13 interval relations of Allen's.  This is a
;;; way of checking the Right-Branching-Point algebra.

;;; (ENUMERATE-SINGLE-RELATION-NETS '(:RB< :RB= :RB>) *RBNET*)
#|
(#N((:RB= :RB< :RB> :RB>) (:RB> :RB= :RB> :RB>) (:RB< :RB< :RB= :RB<) (:RB< :RB< :RB> :RB=))
 #N((:RB= :RB< :RB> :RB=) (:RB> :RB= :RB> :RB>) (:RB< :RB< :RB= :RB<) (:RB= :RB< :RB> :RB=))
 #N((:RB= :RB< :RB> :RB<) (:RB> :RB= :RB> :RB>) (:RB< :RB< :RB= :RB<) (:RB> :RB< :RB> :RB=))
 #N((:RB= :RB< :RB> :RB<) (:RB> :RB= :RB> :RB=) (:RB< :RB< :RB= :RB<) (:RB> :RB= :RB> :RB=))
 #N((:RB= :RB< :RB> :RB<) (:RB> :RB= :RB> :RB<) (:RB< :RB< :RB= :RB<) (:RB> :RB> :RB> :RB=))
 #N((:RB= :RB< :RB= :RB<) (:RB> :RB= :RB> :RB>) (:RB= :RB< :RB= :RB<) (:RB> :RB< :RB> :RB=))
 #N((:RB= :RB< :RB= :RB<) (:RB> :RB= :RB> :RB=) (:RB= :RB< :RB= :RB<) (:RB> :RB= :RB> :RB=))
 #N((:RB= :RB< :RB= :RB<) (:RB> :RB= :RB> :RB<) (:RB= :RB< :RB= :RB<) (:RB> :RB> :RB> :RB=))
 #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB> :RB>) (:RB> :RB< :RB= :RB<) (:RB> :RB< :RB> :RB=))
 #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB> :RB=) (:RB> :RB< :RB= :RB<) (:RB> :RB= :RB> :RB=))
 #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB> :RB<) (:RB> :RB< :RB= :RB<) (:RB> :RB> :RB> :RB=))
 #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB= :RB<) (:RB> :RB= :RB= :RB<) (:RB> :RB> :RB> :RB=))
 #N((:RB= :RB< :RB< :RB<) (:RB> :RB= :RB< :RB<) (:RB> :RB> :RB= :RB<) (:RB> :RB> :RB> :RB=))
 )
|#

;;;----------------------------------------------------------------------
;;; (ENUMERATE-SINGLE-RELATION-NETS '(:RB< :RB= :RB> :RB~) *RB=NET*)

(defparameter
  *RIGHT-BRANCHING-INTERVALS-&-POINTS*           ; 24 relations
  '(
    #N((:RB= :RB<= :RB~ :RB~) (:RB=> :RB= :RB~ :RB~) (:RB~ :RB~ :RB= :RB<=) (:RB~ :RB~ :RB=> :RB=))
    #N((:RB= :RB<= :RB> :RB~) (:RB=> :RB= :RB> :RB~) (:RB< :RB< :RB= :RB< ) (:RB~ :RB~ :RB>  :RB=))
    #N((:RB= :RB<= :RB> :RB>) (:RB=> :RB= :RB> :RB>) (:RB< :RB< :RB= :RB<=) (:RB< :RB< :RB=> :RB=))
    #N((:RB= :RB<  :RB> :RB=) (:RB>  :RB= :RB> :RB>) (:RB< :RB< :RB= :RB< ) (:RB= :RB< :RB>  :RB=))
    #N((:RB= :RB=  :RB> :RB=) (:RB=  :RB= :RB> :RB=) (:RB< :RB< :RB= :RB< ) (:RB= :RB= :RB>  :RB=))
    #N((:RB= :RB<  :RB> :RB<) (:RB>  :RB= :RB> :RB~) (:RB< :RB< :RB= :RB< ) (:RB> :RB~ :RB>  :RB=))
    #N((:RB= :RB<  :RB> :RB<) (:RB>  :RB= :RB> :RB>) (:RB< :RB< :RB= :RB< ) (:RB> :RB< :RB>  :RB=))
    #N((:RB= :RB<  :RB> :RB<) (:RB>  :RB= :RB> :RB=) (:RB< :RB< :RB= :RB< ) (:RB> :RB= :RB>  :RB=))
    #N((:RB= :RB<= :RB> :RB<) (:RB=> :RB= :RB> :RB<) (:RB< :RB< :RB= :RB< ) (:RB> :RB> :RB>  :RB=))
    #N((:RB= :RB<  :RB= :RB=) (:RB>  :RB= :RB> :RB>) (:RB= :RB< :RB= :RB= ) (:RB= :RB< :RB=  :RB=))
    #N((:RB= :RB=  :RB= :RB=) (:RB=  :RB= :RB= :RB=) (:RB= :RB= :RB= :RB= ) (:RB= :RB= :RB=  :RB=))
    #N((:RB= :RB<  :RB= :RB<) (:RB>  :RB= :RB> :RB~) (:RB= :RB< :RB= :RB< ) (:RB> :RB~ :RB>  :RB=))
    #N((:RB= :RB<  :RB= :RB<) (:RB>  :RB= :RB> :RB>) (:RB= :RB< :RB= :RB< ) (:RB> :RB< :RB>  :RB=))
    #N((:RB= :RB<  :RB= :RB<) (:RB>  :RB= :RB> :RB=) (:RB= :RB< :RB= :RB< ) (:RB> :RB= :RB>  :RB=))
    #N((:RB= :RB<  :RB= :RB<) (:RB>  :RB= :RB> :RB<) (:RB= :RB< :RB= :RB< ) (:RB> :RB> :RB>  :RB=))
    #N((:RB= :RB=  :RB= :RB<) (:RB=  :RB= :RB= :RB<) (:RB= :RB= :RB= :RB< ) (:RB> :RB> :RB>  :RB=))
    #N((:RB= :RB<  :RB< :RB<) (:RB>  :RB= :RB~ :RB~) (:RB> :RB~ :RB= :RB<=) (:RB> :RB~ :RB=> :RB=))
    #N((:RB= :RB<  :RB< :RB<) (:RB>  :RB= :RB> :RB~) (:RB> :RB< :RB= :RB< ) (:RB> :RB~ :RB>  :RB=))
    #N((:RB= :RB<  :RB< :RB<) (:RB>  :RB= :RB> :RB>) (:RB> :RB< :RB= :RB<=) (:RB> :RB< :RB=> :RB=))
    #N((:RB= :RB<  :RB< :RB<) (:RB>  :RB= :RB> :RB=) (:RB> :RB< :RB= :RB< ) (:RB> :RB= :RB>  :RB=))
    #N((:RB= :RB<  :RB< :RB<) (:RB>  :RB= :RB> :RB<) (:RB> :RB< :RB= :RB< ) (:RB> :RB> :RB>  :RB=))
    #N((:RB= :RB<  :RB< :RB<) (:RB>  :RB= :RB= :RB=) (:RB> :RB= :RB= :RB= ) (:RB> :RB= :RB=  :RB=))
    #N((:RB= :RB<  :RB< :RB<) (:RB>  :RB= :RB= :RB<) (:RB> :RB= :RB= :RB< ) (:RB> :RB> :RB>  :RB=))
    #N((:RB= :RB<= :RB< :RB<) (:RB=> :RB= :RB< :RB<) (:RB> :RB> :RB= :RB<=) (:RB> :RB> :RB=> :RB=))
    ))

;;;----------------------------------------------------------------------
;;; (ENUMERATE-SINGLE-RELATION-NETS '(:LB< :LB= :LB> :LB~) *LBNET*)

(defparameter
  *LEFT-BRANCHING-INTERVALS*           ; 19 relations
  '(
    #N((:LB= :LB< :LB~ :LB~) (:LB> :LB= :LB~ :LB~) (:LB~ :LB~ :LB= :LB<) (:LB~ :LB~ :LB> :LB=))
    #N((:LB= :LB< :LB~ :LB~) (:LB> :LB= :LB> :LB>) (:LB~ :LB< :LB= :LB<) (:LB~ :LB< :LB> :LB=))
    #N((:LB= :LB< :LB~ :LB<) (:LB> :LB= :LB~ :LB<) (:LB~ :LB~ :LB= :LB<) (:LB> :LB> :LB> :LB=))
    #N((:LB= :LB< :LB~ :LB<) (:LB> :LB= :LB> :LB>) (:LB~ :LB< :LB= :LB<) (:LB> :LB< :LB> :LB=))
    #N((:LB= :LB< :LB~ :LB<) (:LB> :LB= :LB> :LB=) (:LB~ :LB< :LB= :LB<) (:LB> :LB= :LB> :LB=))
    #N((:LB= :LB< :LB~ :LB<) (:LB> :LB= :LB> :LB<) (:LB~ :LB< :LB= :LB<) (:LB> :LB> :LB> :LB=))
    #N((:LB= :LB< :LB> :LB>) (:LB> :LB= :LB> :LB>) (:LB< :LB< :LB= :LB<) (:LB< :LB< :LB> :LB=))
    #N((:LB= :LB< :LB> :LB=) (:LB> :LB= :LB> :LB>) (:LB< :LB< :LB= :LB<) (:LB= :LB< :LB> :LB=))
    #N((:LB= :LB< :LB> :LB<) (:LB> :LB= :LB> :LB>) (:LB< :LB< :LB= :LB<) (:LB> :LB< :LB> :LB=))
    #N((:LB= :LB< :LB> :LB<) (:LB> :LB= :LB> :LB=) (:LB< :LB< :LB= :LB<) (:LB> :LB= :LB> :LB=))
    #N((:LB= :LB< :LB> :LB<) (:LB> :LB= :LB> :LB<) (:LB< :LB< :LB= :LB<) (:LB> :LB> :LB> :LB=))
    #N((:LB= :LB< :LB= :LB<) (:LB> :LB= :LB> :LB>) (:LB= :LB< :LB= :LB<) (:LB> :LB< :LB> :LB=))
    #N((:LB= :LB< :LB= :LB<) (:LB> :LB= :LB> :LB=) (:LB= :LB< :LB= :LB<) (:LB> :LB= :LB> :LB=))
    #N((:LB= :LB< :LB= :LB<) (:LB> :LB= :LB> :LB<) (:LB= :LB< :LB= :LB<) (:LB> :LB> :LB> :LB=))
    #N((:LB= :LB< :LB< :LB<) (:LB> :LB= :LB> :LB>) (:LB> :LB< :LB= :LB<) (:LB> :LB< :LB> :LB=))
    #N((:LB= :LB< :LB< :LB<) (:LB> :LB= :LB> :LB=) (:LB> :LB< :LB= :LB<) (:LB> :LB= :LB> :LB=))
    #N((:LB= :LB< :LB< :LB<) (:LB> :LB= :LB> :LB<) (:LB> :LB< :LB= :LB<) (:LB> :LB> :LB> :LB=))
    #N((:LB= :LB< :LB< :LB<) (:LB> :LB= :LB= :LB<) (:LB> :LB= :LB= :LB<) (:LB> :LB> :LB> :LB=))
    #N((:LB= :LB< :LB< :LB<) (:LB> :LB= :LB< :LB<) (:LB> :LB> :LB= :LB<) (:LB> :LB> :LB> :LB=))
    ))

;;;----------------------------------------------------------------------
;;; (ENUMERATE-SINGLE-RELATION-NETS '(:LB< :LB= :LB> :LB~) *LB=NET*)

(defparameter
  *LEFT-BRANCHING-INTERVALS-&-POINTS*           ; 24 relations
  '(
    #N((:LB= :LB<= :LB~ :LB~) (:LB=> :LB= :LB~ :LB~) (:LB~ :LB~ :LB= :LB<=) (:LB~ :LB~ :LB=> :LB=))
    #N((:LB= :LB<  :LB~ :LB~) (:LB>  :LB= :LB> :LB>) (:LB~ :LB< :LB= :LB<=) (:LB~ :LB< :LB=> :LB=))
    #N((:LB= :LB<= :LB~ :LB<) (:LB=> :LB= :LB~ :LB<) (:LB~ :LB~ :LB= :LB< ) (:LB> :LB> :LB>  :LB=))
    #N((:LB= :LB<  :LB~ :LB<) (:LB>  :LB= :LB> :LB>) (:LB~ :LB< :LB= :LB< ) (:LB> :LB< :LB>  :LB=))
    #N((:LB= :LB<  :LB~ :LB<) (:LB>  :LB= :LB> :LB=) (:LB~ :LB< :LB= :LB< ) (:LB> :LB= :LB>  :LB=))
    #N((:LB= :LB<  :LB~ :LB<) (:LB>  :LB= :LB> :LB<) (:LB~ :LB< :LB= :LB< ) (:LB> :LB> :LB>  :LB=))
    #N((:LB= :LB<= :LB> :LB>) (:LB=> :LB= :LB> :LB>) (:LB< :LB< :LB= :LB<=) (:LB< :LB< :LB=> :LB=))
    #N((:LB= :LB<  :LB> :LB=) (:LB>  :LB= :LB> :LB>) (:LB< :LB< :LB= :LB< ) (:LB= :LB< :LB>  :LB=))
    #N((:LB= :LB=  :LB> :LB=) (:LB=  :LB= :LB> :LB=) (:LB< :LB< :LB= :LB< ) (:LB= :LB= :LB>  :LB=))
    #N((:LB= :LB<  :LB> :LB<) (:LB>  :LB= :LB> :LB>) (:LB< :LB< :LB= :LB< ) (:LB> :LB< :LB>  :LB=))
    #N((:LB= :LB<  :LB> :LB<) (:LB>  :LB= :LB> :LB=) (:LB< :LB< :LB= :LB< ) (:LB> :LB= :LB>  :LB=))
    #N((:LB= :LB<= :LB> :LB<) (:LB=> :LB= :LB> :LB<) (:LB< :LB< :LB= :LB< ) (:LB> :LB> :LB>  :LB=))
    #N((:LB= :LB<  :LB= :LB=) (:LB>  :LB= :LB> :LB>) (:LB= :LB< :LB= :LB= ) (:LB= :LB< :LB=  :LB=))
    #N((:LB= :LB=  :LB= :LB=) (:LB=  :LB= :LB= :LB=) (:LB= :LB= :LB= :LB= ) (:LB= :LB= :LB=  :LB=))
    #N((:LB= :LB<  :LB= :LB<) (:LB>  :LB= :LB> :LB>) (:LB= :LB< :LB= :LB< ) (:LB> :LB< :LB>  :LB=))
    #N((:LB= :LB<  :LB= :LB<) (:LB>  :LB= :LB> :LB=) (:LB= :LB< :LB= :LB< ) (:LB> :LB= :LB>  :LB=))
    #N((:LB= :LB<  :LB= :LB<) (:LB>  :LB= :LB> :LB<) (:LB= :LB< :LB= :LB< ) (:LB> :LB> :LB>  :LB=))
    #N((:LB= :LB=  :LB= :LB<) (:LB=  :LB= :LB= :LB<) (:LB= :LB= :LB= :LB< ) (:LB> :LB> :LB>  :LB=))
    #N((:LB= :LB<  :LB< :LB<) (:LB>  :LB= :LB> :LB>) (:LB> :LB< :LB= :LB<=) (:LB> :LB< :LB=> :LB=))
    #N((:LB= :LB<  :LB< :LB<) (:LB>  :LB= :LB> :LB=) (:LB> :LB< :LB= :LB< ) (:LB> :LB= :LB>  :LB=))
    #N((:LB= :LB<  :LB< :LB<) (:LB>  :LB= :LB> :LB<) (:LB> :LB< :LB= :LB< ) (:LB> :LB> :LB>  :LB=))
    #N((:LB= :LB<  :LB< :LB<) (:LB>  :LB= :LB= :LB=) (:LB> :LB= :LB= :LB= ) (:LB> :LB= :LB=  :LB=))
    #N((:LB= :LB<  :LB< :LB<) (:LB>  :LB= :LB= :LB<) (:LB> :LB= :LB= :LB< ) (:LB> :LB> :LB>  :LB=))
    #N((:LB= :LB<= :LB< :LB<) (:LB=> :LB= :LB< :LB<) (:LB> :LB> :LB= :LB<=) (:LB> :LB> :LB=> :LB=))
    ))

;;;----------------------------------------------------------------------
;;;                         END OF FILE
;;;----------------------------------------------------------------------
