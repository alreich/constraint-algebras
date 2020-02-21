;;; -*- Package: CONSTRAINT-MAINTENANCE; Mode: LISP; Base: 10; Syntax: Common-Lisp -*-

(in-package "CONSTRAINT-MAINTENANCE")

;;;===========================================================================
;;; This file contains the scalar network representation of period relations
;;; that correspond to several algebras: Allen's, Matuzsek et.al's, and my
;;; branching time algebra.
;;;
;;; Each of the sets of representations are listed as "with equals" and
;;; "without equals", which corresponds to "includes" and "doesn't include"
;;; degenerate periods, respectively.
;;;===========================================================================

;;;---------------------------------------------------------------------------
;;; SCALAR NETWORKS THAT ARE BASED ON ALLEN'S PERIOD RELATIONS:
;;;---------------------------------------------------------------------------

;;; Created by using the function ENUMERATE-SINGLE-PERIOD-RELATIONS.

;;; These point networks correspond to Allen's 13 interval relations
;;; when one assumes that degenerate intervals are NOT allowed.  That is,
;;; these networks are exactily isomorphic to Allen's relations.
;;;
;;; LENGTH = 13

(defvar *allen-pt-nets-WITHOUT-equals*
  
  '((:A  . #N((:= :< :> :>)		; After
              (:> := :> :>)
              (:< :< := :<)
              (:< :< :> :=)))
    
    (:MI . #N((:= :< :> :=)		; Met-By
              (:> := :> :>)
              (:< :< := :<)
              (:= :< :> :=)))
    
    (:OI . #N((:= :< :> :<)		; Overlapped-By
              (:> := :> :>)
              (:< :< := :<)
              (:> :< :> :=)))
    
    (:F  . #N((:= :< :> :<)		; Finishes
              (:> := :> :=)
              (:< :< := :<)
              (:> := :> :=)))
    
    (:D  . #N((:= :< :> :<)		; During
              (:> := :> :<)
              (:< :< := :<)
              (:> :> :> :=)))
    
    (:SI . #N((:= :< := :<)		; Started-By
              (:> := :> :>)
              (:= :< := :<)
              (:> :< :> :=)))
    
    (:E  . #N((:= :< := :<)		; Equals
              (:> := :> :=)
              (:= :< := :<)
              (:> := :> :=)))
    
    (:S  . #N((:= :< := :<)		; Starts
              (:> := :> :<)
              (:= :< := :<)
              (:> :> :> :=)))
    
    (:DI . #N((:= :< :< :<)		; Contains
              (:> := :> :>)
              (:> :< := :<)
              (:> :< :> :=)))
    
    (:FI . #N((:= :< :< :<)		; Finished-By
              (:> := :> :=)
              (:> :< := :<)
              (:> := :> :=)))
    
    (:O  . #N((:= :< :< :<)		; Overlaps
              (:> := :> :<)
              (:> :< := :<)
              (:> :> :> :=)))
    
    (:M  . #N((:= :< :< :<)		; Meets
              (:> := := :<)
              (:> := := :<)
              (:> :> :> :=)))
    
    (:B  . #N((:= :< :< :<)		; Before
              (:> := :< :<)
              (:> :> := :<)
              (:> :> :> :=)))))

;;;---------------------------------------------------------------------------

;;; These point networks correspond to Allen-like interval relations
;;; when one assumes that degenerate intervals ARE allowed.
;;;
;;; LENGTH = 18 

(setq *allen-pt-nets-WITH-equals*
      
      '((:BI  . #N((:= :<= :> :>)	; After
		   (:>= := :> :>)
		   (:< :< := :<=)
		   (:< :< :>= :=)))
        
	(:MI  . #N((:= :< :> :=)	; Met-By
		   (:> := :> :>)
		   (:< :< := :<)
		   (:= :< :> :=)))
        
	(:PF  . #N((:= := :> :=)	;       X  INSTANT-FINISHES
		   (:= := :> :=)	;       |   inverse is:
		   (:< :< := :<)	; |-----|   FINISHED-BY-INSTANT
		   (:= := :> :=)))	;    Y
        
	(:OI  . #N((:= :< :> :<)	; Overlapped-By
		   (:> := :> :>)
		   (:< :< := :<)
		   (:> :< :> :=)))
        
	(:F   . #N((:= :< :> :<)	; Finishes
		   (:> := :> :=)
		   (:< :< := :<)
		   (:> := :> :=)))
        
	(:D   . #N((:= :<= :> :<)	; During
		   (:>= := :> :<)
		   (:< :<  := :<)
		   (:> :>  :> :=)))
        
	(:PSI . #N((:= :< := :=)	;    X     STARTED-BY-INSTANT
		   (:> := :> :>)	; |-----|   inverse is:
		   (:= :< := :=)	; |         INSTANT-STARTS
		   (:= :< := :=)))	; Y
        
	(:PE  . #N((:= := := :=)	;   X      EQUALS-INSTANT
		   (:= := := :=)	;   |       inverse is:
		   (:= := := :=)	;   |       EQUALS-INSTANT
		   (:= := := :=)))	;   Y
        
	(:SI  . #N((:= :< := :<)	; Started-By
		   (:> := :> :>)
		   (:= :< := :<)
		   (:> :< :> :=)))
        
	(:E   . #N((:= :< := :<)	; Equals
		   (:> := :> :=)
		   (:= :< := :<)
		   (:> := :> :=)))
        
	(:S   . #N((:= :< := :<)	; Starts
		   (:> := :> :<)
		   (:= :< := :<)
		   (:> :> :> :=)))
        
	(:PS  . #N((:= := := :<)	; X        INSTANT-STARTS
		   (:= := := :<)	; |         inverse is:
		   (:= := := :<)	; |-----|   STARTED-BY-INSTANT
		   (:> :> :> :=)))	;    Y
        
	(:DI  . #N((:= :< :< :<)	; Contains
		   (:> := :> :>)
		   (:> :< := :<=)
		   (:> :< :>= :=)))
        
	(:FI  . #N((:= :< :< :<)	; Finished-By
		   (:> := :> :=)
		   (:> :< := :<)
		   (:> := :> :=)))
        
	(:O   . #N((:= :< :< :<)	; Overlaps
		   (:> := :> :<)
		   (:> :< := :<)
		   (:> :> :> :=)))
        
	(:PFI  . #N((:= :< :< :<)	;    X     FINISHED-BY-INSTANT
		    (:> := := :=)	; |-----|   inverse is:
		    (:> := := :=)	;       |   INSTANT-FINISHES
		    (:> := := :=)))	;       Y
        
	(:M   . #N((:= :< :< :<)	; Meets
		   (:> := := :<)
		   (:> := := :<)
		   (:> :> :> :=)))
        
	(:B   . #N((:= :<= :< :<)	; Before
		   (:>= := :< :<)
		   (:> :> := :<=)
		   (:> :> :>= :=)))))

;;;---------------------------------------------------------------------------
;;; SCALAR NETWORKS THAT ARE BASED ON MATUZSEK, ET. AL.'S PERIOD RELATIONS:
;;;---------------------------------------------------------------------------

;;; Created by using the function ENUMERATE-END-PT-PERIOD-RELATIONS.

;;; These point networks correspond to Matuszek et.al.'s 12 interval
;;; relations when one assumes that degenerate intervals are NOT allowed.
;;;
;;; LENGTH = 12

(defvar *endpt-pt-nets-WITHOUT-equals*
  
  '((:FAF . #N((:= :< :? :?)	; faf
               (:> := :> :>)
               (:? :< := :<)
               (:? :< :> :=)))
    
    (:FAS . #N((:= :< :? :?)	; fas
               (:> := :> :?)
               (:? :< := :<)
               (:? :? :> :=)))
    
    (:SAF . #N((:= :< :> :>)	; saf (same as "After")
               (:> := :> :>)
               (:< :< := :<)
               (:< :< :> :=)))
    
    (:SAS . #N((:= :< :> :?)	; sas
               (:> := :> :?)
               (:< :< := :<)
               (:? :? :> :=)))
    
    (:FWF . #N((:= :< :? :<)	; fwf
               (:> := :> :=)
               (:? :< := :<)
               (:> := :> :=)))
    
    (:FWS . #N((:= :< :< :<)	; fws
               (:> := := :<)
               (:> := := :<)
               (:> :> :> :=)))
    
    (:SWF . #N((:= :< :> :=)	; swf
               (:> := :> :>)
               (:< :< := :<)
               (:= :< :> :=)))
    
    (:SWS . #N((:= :< := :<)	; sws
               (:> := :> :?)
               (:= :< := :<)
               (:> :? :> :=)))
    
    (:FBF . #N((:= :< :? :<)	; fbf
               (:> := :? :<)
               (:? :? := :<)
               (:> :> :> :=)))
    
    (:FBS . #N((:= :< :< :<)	; fbs (same as "Before")
               (:> := :< :<)
               (:> :> := :<)
               (:> :> :> :=)))
    
    (:SBF . #N((:= :< :? :<)	; sbf
               (:> := :? :?)
               (:? :? := :<)
               (:> :? :> :=)))
    
    (:SBS . #N((:= :< :< :<)	; sbs
               (:> := :? :?)
               (:> :? := :<)
               (:> :? :> :=)))))
      
;;;---------------------------------------------------------------------------

;;; These point networks correspond to Matuszek et.al.'s 12 interval
;;; relations when one assumes that degenerate intervals ARE allowed.
;;;
;;; LENGTH = 12

      (defvar *endpt-pt-nets-WITH-equals*

	'(#N((:= :<= :? :?)		; faf
	     (:>= := :> :>)
	     (:? :< := :<=)
	     (:? :< :>= :=))
	  
	  #N((:= :<= :? :?)		; fas
	     (:>= := :> :?)
	     (:? :< := :<=)
	     (:? :? :>= :=))
	  
	  #N((:= :<= :> :>)		; saf (same as "After")
	     (:>= := :> :>)
	     (:< :< := :<=)
	     (:< :< :>= :=))
	  
	  #N((:= :<= :> :?)		; sas
	     (:>= := :> :?)
	     (:< :< := :<=)
	     (:? :? :>= :=))
	  
	  #N((:= :<= :? :<=)		; fwf=
	     (:>= := :>= :=)
	     (:? :<= := :<=)
	     (:>= := :>= :=))
	  
	  #N((:= :<= :<= :<=)		; fws=
	     (:>= := := :<=)
	     (:>= := := :<=)
	     (:>= :>= :>= :=))
	  
	  #N((:= :<= :>= :=)		; swf=
	     (:>= := :>= :>=)
	     (:<= :<= := :<=)
	     (:= :<= :>= :=))
	  
	  #N((:= :<= := :<=)		; sws=
	     (:>= := :>= :?)
	     (:= :<= := :<=)
	     (:>= :? :>= :=))
	  
	  #N((:= :<= :? :<)		; fbf
	     (:>= := :? :<)
	     (:? :? := :<=)
	     (:> :> :>= :=))
	  
	  #N((:= :<= :< :<)		; fbs (same as "Before")
	     (:>= := :< :<)
	     (:> :> := :<=)
	     (:> :> :>= :=))
	  
	  #N((:= :<= :? :<)		; sbf
	     (:>= := :? :?)
	     (:? :? := :<=)
	     (:> :? :>= :=))
	  
	  #N((:= :<= :< :<)		; sbs
	     (:>= := :? :?)
	     (:> :? := :<=)
	     (:> :? :>= :=))))
      
;;;---------------------------------------------------------------------------
;;; SCALAR NETWORKS THAT CORRESPOND TO BRANCHING TIME PERIOD RELATIONS:
;;;---------------------------------------------------------------------------

;;; Created by using the function ENUMERATE-SINGLE-BT-PERIOD-RELATIONS.
;;;
;;;  BW  --> "Branch With"
;;;  UBW --> "Until Branch With"
;;;
;;;  length = 19

      (defvar *bt-pt-nets-SINGLE-WITHOUT-equals*

	'((:IB  . #N((:BT= :BT< :BT~ :BT~) ; R5 (Incomparable)
		     (:BT> :BT= :BT~ :BT~)
		     (:BT~ :BT~ :BT= :BT<)
		     (:BT~ :BT~ :BT> :BT=)))
	  
	  (:AB  . #N((:BT= :BT< :BT> :BT~) ; After-BW (R1)
		     (:BT> :BT= :BT> :BT~) ;  (inverse is
		     (:BT< :BT< :BT= :BT<) ;   Before-BW)
		     (:BT~ :BT~ :BT> :BT=)))
	  
	  (:A   . #N((:BT= :BT< :BT> :BT>) ; After
		     (:BT> :BT= :BT> :BT>)
		     (:BT< :BT< :BT= :BT<)
		     (:BT< :BT< :BT> :BT=)))
	  
	  (:MI  . #N((:BT= :BT< :BT> :BT=) ; Met-By
		     (:BT> :BT= :BT> :BT>)
		     (:BT< :BT< :BT= :BT<)
		     (:BT= :BT< :BT> :BT=)))
	  
	  (:OIB . #N((:BT= :BT< :BT> :BT<) ; Overlapped-By-UBW (R3)
		     (:BT> :BT= :BT> :BT~) ;  (inverse is
		     (:BT< :BT< :BT= :BT<) ;   Overlaps-UBW)
		     (:BT> :BT~ :BT> :BT=)))
	  
	  (:OI  . #N((:BT= :BT< :BT> :BT<) ; Overlapped-By
		     (:BT> :BT= :BT> :BT>)
		     (:BT< :BT< :BT= :BT<)
		     (:BT> :BT< :BT> :BT=)))
	  
	  (:F   . #N((:BT= :BT< :BT> :BT<) ; Finishes
		     (:BT> :BT= :BT> :BT=)
		     (:BT< :BT< :BT= :BT<)
		     (:BT> :BT= :BT> :BT=)))
	  
	  (:D   . #N((:BT= :BT< :BT> :BT<) ; During
		     (:BT> :BT= :BT> :BT<)
		     (:BT< :BT< :BT= :BT<)
		     (:BT> :BT> :BT> :BT=)))
	  
	  (:SIB . #N((:BT= :BT< :BT= :BT<) ; Started-By-UBW (R6)
		     (:BT> :BT= :BT> :BT~) ;  (inverse is
		     (:BT= :BT< :BT= :BT<) ;   Started-By-UBW)
		     (:BT> :BT~ :BT> :BT=)))
	  
	  (:SI  . #N((:BT= :BT< :BT= :BT<) ; Started-By
		     (:BT> :BT= :BT> :BT>)
		     (:BT= :BT< :BT= :BT<)
		     (:BT> :BT< :BT> :BT=)))
	  
	  (:E   . #N((:BT= :BT< :BT= :BT<) ; Equals
		     (:BT> :BT= :BT> :BT=)
		     (:BT= :BT< :BT= :BT<)
		     (:BT> :BT= :BT> :BT=)))
	  
	  (:S   . #N((:BT= :BT< :BT= :BT<) ; Starts
		     (:BT> :BT= :BT> :BT<)
		     (:BT= :BT< :BT= :BT<)
		     (:BT> :BT> :BT> :BT=)))
	  
	  (:BB  . #N((:BT= :BT< :BT< :BT<) ; Before-BW (R2)
		     (:BT> :BT= :BT~ :BT~) ;  (inverse is
		     (:BT> :BT~ :BT= :BT<) ;   After-BW)
		     (:BT> :BT~ :BT> :BT=)))
	  
	  (:OB  . #N((:BT= :BT< :BT< :BT<) ; Overlaps-UBW (R4)
		     (:BT> :BT= :BT> :BT~) ;  (inverse is
		     (:BT> :BT< :BT= :BT<) ;   Overlapped-By-UBW)
		     (:BT> :BT~ :BT> :BT=)))
	  
	  (:DI  . #N((:BT= :BT< :BT< :BT<) ; Contains
		     (:BT> :BT= :BT> :BT>)
		     (:BT> :BT< :BT= :BT<)
		     (:BT> :BT< :BT> :BT=)))
	  
	  (:FI  . #N((:BT= :BT< :BT< :BT<) ; Finished-By
		     (:BT> :BT= :BT> :BT=)
		     (:BT> :BT< :BT= :BT<)
		     (:BT> :BT= :BT> :BT=)))
	  
	  (:O   . #N((:BT= :BT< :BT< :BT<) ; Overlaps
		     (:BT> :BT= :BT> :BT<)
		     (:BT> :BT< :BT= :BT<)
		     (:BT> :BT> :BT> :BT=)))
	  
	  (:M   . #N((:BT= :BT< :BT< :BT<) ; Meets
		     (:BT> :BT= :BT= :BT<)
		     (:BT> :BT= :BT= :BT<)
		     (:BT> :BT> :BT> :BT=)))
	  
	  (:B   . #N((:BT= :BT< :BT< :BT<) ; Before
		     (:BT> :BT= :BT< :BT<)
		     (:BT> :BT> :BT= :BT<)
		     (:BT> :BT> :BT> :BT=)))))
      
;;;---------------------------------------------------------------------------

;;;  length = 24

      (defvar *bt-pt-nets-SINGLE-WITH-equals*

	'(#N((:BT=  :BT<= :BT~  :BT~)	; Incomparable
	     (:BT=> :BT=  :BT~  :BT~)
	     (:BT~  :BT~  :BT=  :BT<=)
	     (:BT~  :BT~  :BT=> :BT=))
	  
	  #N((:BT=  :BT<= :BT> :BT~)	; After-BW
	     (:BT=> :BT=  :BT> :BT~)
	     (:BT<  :BT<  :BT= :BT<)
	     (:BT~  :BT~  :BT> :BT=))
	  
	  #N((:BT=  :BT<= :BT>  :BT>)	; After
	     (:BT=> :BT=  :BT>  :BT>)
	     (:BT<  :BT<  :BT=  :BT<=)
	     (:BT<  :BT<  :BT=> :BT=))
	  
	  #N((:BT= :BT< :BT> :BT=)	; Met-By
	     (:BT> :BT= :BT> :BT>)
	     (:BT< :BT< :BT= :BT<)
	     (:BT= :BT< :BT> :BT=))
	  
	  #N((:BT= :BT= :BT> :BT=)	; Instant-Finishes
	     (:BT= :BT= :BT> :BT=)
	     (:BT< :BT< :BT= :BT<)
	     (:BT= :BT= :BT> :BT=))
	  
	  #N((:BT= :BT< :BT> :BT<)	; Overlapped-By-UBW
	     (:BT> :BT= :BT> :BT~)
	     (:BT< :BT< :BT= :BT<)
	     (:BT> :BT~ :BT> :BT=))
	  
	  #N((:BT= :BT< :BT> :BT<)	; Overlapped-By
	     (:BT> :BT= :BT> :BT>)
	     (:BT< :BT< :BT= :BT<)
	     (:BT> :BT< :BT> :BT=))
	  
	  #N((:BT= :BT< :BT> :BT<)	; Finishes
	     (:BT> :BT= :BT> :BT=)
	     (:BT< :BT< :BT= :BT<)
	     (:BT> :BT= :BT> :BT=))
	  
	  #N((:BT=  :BT<= :BT> :BT<)	; During
	     (:BT=> :BT=  :BT> :BT<)
	     (:BT<  :BT<  :BT= :BT<)
	     (:BT>  :BT>  :BT> :BT=))
	  
	  #N((:BT= :BT< :BT= :BT=)	; Started-By-Instant
	     (:BT> :BT= :BT> :BT>)
	     (:BT= :BT< :BT= :BT=)
	     (:BT= :BT< :BT= :BT=))
	  
	  #N((:BT= :BT= :BT= :BT=)	; Equals-Instant
	     (:BT= :BT= :BT= :BT=)
	     (:BT= :BT= :BT= :BT=)
	     (:BT= :BT= :BT= :BT=))
	  
	  #N((:BT= :BT< :BT= :BT<)	; Starts-UBW
	     (:BT> :BT= :BT> :BT~)
	     (:BT= :BT< :BT= :BT<)
	     (:BT> :BT~ :BT> :BT=))
	  
	  #N((:BT= :BT< :BT= :BT<)	; Started-By
	     (:BT> :BT= :BT> :BT>)
	     (:BT= :BT< :BT= :BT<)
	     (:BT> :BT< :BT> :BT=))
	  
	  #N((:BT= :BT< :BT= :BT<)	; Equals
	     (:BT> :BT= :BT> :BT=)
	     (:BT= :BT< :BT= :BT<)
	     (:BT> :BT= :BT> :BT=))
	  
	  #N((:BT= :BT< :BT= :BT<)	; Starts
	     (:BT> :BT= :BT> :BT<)
	     (:BT= :BT< :BT= :BT<)
	     (:BT> :BT> :BT> :BT=))
	  
	  #N((:BT= :BT= :BT= :BT<)	; Instant-Starts
	     (:BT= :BT= :BT= :BT<)
	     (:BT= :BT= :BT= :BT<)
	     (:BT> :BT> :BT> :BT=))
	  
	  #N((:BT= :BT< :BT<  :BT<)	; Before-BW
	     (:BT> :BT= :BT~  :BT~)
	     (:BT> :BT~ :BT=  :BT<=)
	     (:BT> :BT~ :BT=> :BT=))
	  
	  #N((:BT= :BT< :BT< :BT<)	; Overlaps-UBW
	     (:BT> :BT= :BT> :BT~)
	     (:BT> :BT< :BT= :BT<)
	     (:BT> :BT~ :BT> :BT=))
	  
	  #N((:BT= :BT< :BT<  :BT<)	; Contains
	     (:BT> :BT= :BT>  :BT>)
	     (:BT> :BT< :BT=  :BT<=)
	     (:BT> :BT< :BT=> :BT=))
	  
	  #N((:BT= :BT< :BT< :BT<)	; Finished-By
	     (:BT> :BT= :BT> :BT=)
	     (:BT> :BT< :BT= :BT<)
	     (:BT> :BT= :BT> :BT=))
	  
	  #N((:BT= :BT< :BT< :BT<)	; Overlaps
	     (:BT> :BT= :BT> :BT<)
	     (:BT> :BT< :BT= :BT<)
	     (:BT> :BT> :BT> :BT=))
	  
	  #N((:BT= :BT< :BT< :BT<)	; Finished-By-Instant
	     (:BT> :BT= :BT= :BT=)
	     (:BT> :BT= :BT= :BT=)
	     (:BT> :BT= :BT= :BT=))
	  
	  #N((:BT= :BT< :BT< :BT<)	; Meets
	     (:BT> :BT= :BT= :BT<)
	     (:BT> :BT= :BT= :BT<)
	     (:BT> :BT> :BT> :BT=))
	  
	  #N((:BT=  :BT<= :BT<  :BT<)	; Before
	     (:BT=> :BT=  :BT<  :BT<)
	     (:BT>  :BT>  :BT=  :BT<=)
	     (:BT>  :BT>  :BT=> :BT=))))
      
;;;---------------------------------------------------------------------------

;;; ENUMERATE-END-PT-BT-POINT-RELATIONS:
;;;
;;;  length = 16

      (defvar *bt-pt-nets-MULTIPLE-WITHOUT-equals*

	'(#N((:BT=    :BT<  :BT<=>~ :BT<~)
	     (:BT>    :BT=  :BT>~   :BT~)
	     (:BT<=>~ :BT<~ :BT=    :BT<)
	     (:BT>~   :BT~  :BT>    :BT=))
 
	  #N((:BT=  :BT< :BT<~ :BT<~)
	     (:BT>  :BT= :BT~  :BT~)
	     (:BT>~ :BT~ :BT=  :BT<)
	     (:BT>~ :BT~ :BT>  :BT=))
 
	  #N((:BT=  :BT< :BT>~ :BT~)
	     (:BT>  :BT= :BT>~ :BT~)
	     (:BT<~ :BT<~ :BT= :BT<)
	     (:BT~  :BT~ :BT> :BT=))
 
	  #N((:BT= :BT< :BT~ :BT~)
	     (:BT> :BT= :BT~ :BT~)
	     (:BT~ :BT~ :BT= :BT<)
	     (:BT~ :BT~ :BT> :BT=))
 
	  #N((:BT=   :BT< :BT<=> :BT<=>)
	     (:BT>   :BT= :BT>   :BT>)
	     (:BT<=> :BT< :BT=   :BT<)
	     (:BT<=> :BT< :BT>   :BT=))
 
	  #N((:BT=    :BT<    :BT<=> :BT<=>~)
	     (:BT>    :BT=    :BT>   :BT<=>~)
	     (:BT<=>  :BT<    :BT=   :BT<)
	     (:BT<=>~ :BT<=>~ :BT>   :BT=))
 
	  #N((:BT= :BT< :BT> :BT>)
	     (:BT> :BT= :BT> :BT>)
	     (:BT< :BT< :BT= :BT<)
	     (:BT< :BT< :BT> :BT=))
 
	  #N((:BT=    :BT<    :BT> :BT<=>~)
	     (:BT>    :BT=    :BT> :BT<=>~)
	     (:BT<    :BT<    :BT= :BT<)
	     (:BT<=>~ :BT<=>~ :BT> :BT=))
 
	  #N((:BT=   :BT< :BT<=> :BT<)
	     (:BT>   :BT= :BT>   :BT=)
	     (:BT<=> :BT< :BT=   :BT<)
	     (:BT>   :BT= :BT>   :BT=))
 
	  #N((:BT= :BT< :BT< :BT<)
	     (:BT> :BT= :BT= :BT<)
	     (:BT> :BT= :BT= :BT<)
	     (:BT> :BT> :BT> :BT=))
 
	  #N((:BT= :BT< :BT> :BT=)
	     (:BT> :BT= :BT> :BT>)
	     (:BT< :BT< :BT= :BT<)
	     (:BT= :BT< :BT> :BT=))
 
	  #N((:BT= :BT<    :BT= :BT<)
	     (:BT> :BT=    :BT> :BT<=>~)
	     (:BT= :BT<    :BT= :BT<)
	     (:BT> :BT<=>~ :BT> :BT=))
 
	  #N((:BT=   :BT<   :BT<=> :BT<)
	     (:BT>   :BT=   :BT<=> :BT<)
	     (:BT<=> :BT<=> :BT=   :BT<)
	     (:BT>   :BT>   :BT>   :BT=))
 
	  #N((:BT= :BT< :BT< :BT<)
	     (:BT> :BT= :BT< :BT<)
	     (:BT> :BT> :BT= :BT<)
	     (:BT> :BT> :BT> :BT=))
 
	  #N((:BT=   :BT<    :BT<=>  :BT<)
	     (:BT>   :BT=    :BT<=>~ :BT<=>~)
	     (:BT<=> :BT<=>~ :BT=    :BT<)
	     (:BT>   :BT<=>~ :BT>    :BT=))
 
	  #N((:BT= :BT<    :BT<    :BT<)
	     (:BT> :BT=    :BT<=>~ :BT<=>~)
	     (:BT> :BT<=>~ :BT=    :BT<)
	     (:BT> :BT<=>~ :BT>    :BT=))))
      
;;;---------------------------------------------------------------------------

;;; ENUMERATE-END-PT-BT-POINT-RELATIONS:
;;;
;;;  length = 16

      (defvar *bt-pt-nets-MULTIPLE-WITH-equals*

	'(#N((:BT=    :BT<= :BT<=>~ :BT<~)
	     (:BT=>   :BT=  :BT>~   :BT~)
	     (:BT<=>~ :BT<~ :BT=    :BT<=)
	     (:BT>~   :BT~  :BT=>   :BT=))
	  
	  #N((:BT=  :BT<= :BT<~ :BT<~)
	     (:BT=> :BT=  :BT~  :BT~)
	     (:BT>~ :BT~  :BT=  :BT<=)
	     (:BT>~ :BT~  :BT=> :BT=))
	  
	  #N((:BT=  :BT<= :BT>~ :BT~)
	     (:BT=> :BT=  :BT>~ :BT~)
	     (:BT<~ :BT<~ :BT=  :BT<=)
	     (:BT~  :BT~  :BT=> :BT=))
	  
	  #N((:BT=  :BT<= :BT~  :BT~)
	     (:BT=> :BT=  :BT~  :BT~)
	     (:BT~  :BT~  :BT=  :BT<=)
	     (:BT~  :BT~  :BT=> :BT=))
	  
	  #N((:BT=   :BT<= :BT<=> :BT<=>)
	     (:BT=>  :BT=  :BT>   :BT>)
	     (:BT<=> :BT<  :BT=   :BT<=)
	     (:BT<=> :BT<  :BT=>  :BT=))
	  
	  #N((:BT=    :BT<=   :BT<=> :BT<=>~)
	     (:BT=>   :BT=    :BT>   :BT<=>~)
	     (:BT<=>  :BT<    :BT=   :BT<=)
	     (:BT<=>~ :BT<=>~ :BT=>  :BT=))
	  
	  #N((:BT=  :BT<= :BT>  :BT>)
	     (:BT=> :BT=  :BT>  :BT>)
	     (:BT<  :BT<  :BT=  :BT<=)
	     (:BT<  :BT<  :BT=> :BT=))
	  
	  #N((:BT=    :BT<=   :BT>  :BT<=>~)
	     (:BT=>   :BT=    :BT>  :BT<=>~)
	     (:BT<    :BT<    :BT=  :BT<=)
	     (:BT<=>~ :BT<=>~ :BT=> :BT=))
	  
	  #N((:BT=   :BT<= :BT<=> :BT<=)
	     (:BT=>  :BT=  :BT=>  :BT=)
	     (:BT<=> :BT<= :BT=   :BT<=)
	     (:BT=>  :BT=  :BT=>  :BT=))
	  
	  #N((:BT=  :BT<= :BT<= :BT<=)
		(:BT=> :BT=  :BT=  :BT<=)
		(:BT=> :BT=  :BT=  :BT<=)
		(:BT=> :BT=> :BT=> :BT=))
	  
	     #N((:BT=  :BT<= :BT=> :BT=)
		(:BT=> :BT=  :BT=> :BT=>)
		(:BT<= :BT<= :BT=  :BT<=)
		(:BT=  :BT<= :BT=> :BT=))
	  
	     #N((:BT=  :BT<=   :BT=  :BT<=)
		(:BT=> :BT=    :BT=> :BT<=>~)
		(:BT=  :BT<=   :BT=  :BT<=)
		(:BT=> :BT<=>~ :BT=> :BT=))
	  
	     #N((:BT=   :BT<=  :BT<=> :BT<)
		(:BT=>  :BT=   :BT<=> :BT<)
		(:BT<=> :BT<=> :BT=   :BT<=)
		(:BT>   :BT>   :BT=> :BT=))
	  
	     #N((:BT=  :BT<= :BT<  :BT<)
		(:BT=> :BT=  :BT<  :BT<)
		(:BT>  :BT>  :BT=  :BT<=)
		(:BT>  :BT>  :BT=> :BT=))
	  
	     #N((:BT=   :BT<=   :BT<=>  :BT<)
		(:BT=>  :BT=    :BT<=>~ :BT<=>~)
		(:BT<=> :BT<=>~ :BT=    :BT<=)
		(:BT>   :BT<=>~ :BT=>   :BT=))
	  
	     #N((:BT=  :BT<=   :BT<    :BT<)
		(:BT=> :BT=    :BT<=>~ :BT<=>~)
		(:BT>  :BT<=>~ :BT=    :BT<=)
		(:BT>  :BT<=>~ :BT=>   :BT=))))

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------






