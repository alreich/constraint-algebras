;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

(in-package "CONSTRAINT-MAINTENANCE")

;;;===========================================================================
;;; This file contains the scalar network representation of period relations
;;; that correspond to several algebras: Allen's, Matuzsek et.al's, and my
;;; branching time algebras
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
;;; these networks are exactly isomorphic to Allen's relations.
;;;
;;; LENGTH = 13
;;; 
;;; Intervals


(defvar *allen-pt-nets-WITHOUT-equals* nil)

(setf *allen-pt-nets-WITHOUT-equals*
      
      '((:BI . #N((:= :< :> :>)         ; After
                  (:> := :> :>)
                  (:< :< := :<)
                  (:< :< :> :=)))
        
        (:MI . #N((:= :< :> :=)         ; Met-By
                  (:> := :> :>)
                  (:< :< := :<)
                  (:= :< :> :=)))
        
        (:OI . #N((:= :< :> :<)         ; Overlapped-By
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
;;;
;;; Intervals and Points

(defvar *allen-pt-nets-WITH-equals* nil)

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

#|
(defvar *allen-pt-nets-WITH-equals*

	'(#N((:=  :<= :>  :>)		; After (:a)
	     (:>= :=  :>  :>)
	     (:<  :<  :=  :<=)
	     (:<  :<  :>= :=))
 
	  #N((:= :< :> :=)		; Met-By (:mi)
	     (:> := :> :>)
	     (:< :< := :<)
	     (:= :< :> :=))
 
	  #N((:= := :> :=)		;       X  INSTANT-FINISHES
	     (:= := :> :=)		;       |  (inverse is:
	     (:< :< := :<)		; |-----|   FINISHED-BY-INSTANT)
	     (:= := :> :=))		;    Y
 
	  #N((:= :< :> :<)		; Overlapped-By (:oi)
	     (:> := :> :>)
	     (:< :< := :<)
	     (:> :< :> :=))
 
	  #N((:= :< :> :<)		; Finishes (:f)
	     (:> := :> :=)
	     (:< :< := :<)
	     (:> := :> :=))
 
	  #N((:=  :<= :> :<)		; During (:d)
	     (:>= :=  :> :<)
	     (:<  :<  := :<)
	     (:>  :>  :> :=))
 
	  #N((:= :< := :=)		;    X     STARTED-BY-INSTANT
	     (:> := :> :>)		; |-----|  (inverse is:
	     (:= :< := :=)		; |         INSTANT-STARTS)
	     (:= :< := :=))		; Y
 
	  #N((:= := := :=)		;   X      EQUALS-INSTANT
	     (:= := := :=)		;   |      (inverse is:
	     (:= := := :=)		;   |       EQUALS-INSTANT)
	     (:= := := :=))		;   Y
 
	  #N((:= :< := :<)		; Started-By
	     (:> := :> :>)
	     (:= :< := :<)
	     (:> :< :> :=))
 
	  #N((:= :< := :<)		; Equals
	     (:> := :> :=)
	     (:= :< := :<)
	     (:> := :> :=))
 
	  #N((:= :< := :<)		; Starts
	     (:> := :> :<)
	     (:= :< := :<)
	     (:> :> :> :=))
 
	  #N((:= := := :<)		; X        INSTANT-STARTS
	     (:= := := :<)		; |        (inverse is:
	     (:= := := :<)		; |-----|   STARTED-BY-INSTANT)
	     (:> :> :> :=))		;    Y
 
	  #N((:= :< :<  :<)		; Contains
	     (:> := :>  :>)
	     (:> :< :=  :<=)
	     (:> :< :>= :=))
 
	  #N((:= :< :< :<)		; Finished-By
	     (:> := :> :=)
	     (:> :< := :<)
	     (:> := :> :=))
 
	  #N((:= :< :< :<)		; Overlaps
	     (:> := :> :<)
	     (:> :< := :<)
	     (:> :> :> :=))
 
	  #N((:= :< :< :<)		;    X     FINISHED-BY-INSTANT
	     (:> := := :=)		; |-----|  (inverse is:
	     (:> := := :=)		;       |   INSTANT-FINISHES)
	     (:> := := :=))		;       Y
 
	  #N((:= :< :< :<)		; Meets
	     (:> := := :<)
	     (:> := := :<)
	     (:> :> :> :=))
 
	  #N((:=  :<= :<  :<)		; Before
	     (:>= :=  :<  :<)
	     (:>  :>  :=  :<=)
	     (:>  :>  :>= :=))))

|#

;;;---------------------------------------------------------------------------
;;; SCALAR NETWORKS THAT ARE BASED ON MATUZSEK, ET. AL.'S PERIOD RELATIONS:
;;;---------------------------------------------------------------------------

;;; Created by using the function ENUMERATE-END-PT-PERIOD-RELATIONS.

;;; These point networks correspond to Matuszek et.al.'s 12 interval
;;; relations when one assumes that degenerate intervals are NOT allowed.
;;;
;;; LENGTH = 12
;;;
;;; Matuzsek Intervals

(defvar *endpt-pt-nets-WITHOUT-equals* nil)

(setf *endpt-pt-nets-WITHOUT-equals*

	'((:FAF . #N((:= :< :? :?)		; faf
		     (:> := :> :>)
		     (:? :< := :<)
		     (:? :< :> :=)))
 
	  (:FAS . #N((:= :< :? :?)		; fas
		     (:> := :> :?)
		     (:? :< := :<)
		     (:? :? :> :=)))
 
	  (:SAF . #N((:= :< :> :>)		; saf (same as "After")
		     (:> := :> :>)
		     (:< :< := :<)
		     (:< :< :> :=)))
 
	  (:SAS . #N((:= :< :> :?)		; sas
		     (:> := :> :?)
		     (:< :< := :<)
		     (:? :? :> :=)))
 
	  (:FWF . #N((:= :< :? :<)		; fwf
		     (:> := :> :=)
		     (:? :< := :<)
		     (:> := :> :=)))
 
	  (:FWS . #N((:= :< :< :<)		; fws
		     (:> := := :<)
		     (:> := := :<)
		     (:> :> :> :=)))
 
	  (:SWF . #N((:= :< :> :=)		; swf
		     (:> := :> :>)
		     (:< :< := :<)
		     (:= :< :> :=)))
 
	  (:SWS . #N((:= :< := :<)		; sws
		     (:> := :> :?)
		     (:= :< := :<)
		     (:> :? :> :=)))
 
	  (:FBF . #N((:= :< :? :<)		; fbf
		     (:> := :? :<)
		     (:? :? := :<)
		     (:> :> :> :=)))
 
	  (:FBS . #N((:= :< :< :<)		; fbs (same as "Before")
		     (:> := :< :<)
		     (:> :> := :<)
		     (:> :> :> :=)))
 
	  (:SBF . #N((:= :< :? :<)		; sbf
		     (:> := :? :?)
		     (:? :? := :<)
		     (:> :? :> :=)))
 
	  (:SBS . #N((:= :< :< :<)		; sbs
		     (:> := :? :?)
		     (:> :? := :<)
		     (:> :? :> :=)))))

;;;---------------------------------------------------------------------------

;;; These point networks correspond to Matuszek et.al.'s 12 interval
;;; relations when one assumes that degenerate intervals ARE allowed.
;;;
;;; LENGTH = 12
;;;
;;; Matuzsek Intervals and Points

(defvar *endpt-pt-nets-WITH-equals* nil)

(setf *endpt-pt-nets-WITH-equals*

	'(#N((:= :<= :? :?)			; faf
	     (:>= := :> :>)
	     (:? :< := :<=)
	     (:? :< :>= :=))
	  
	  #N((:= :<= :? :?)			; fas
	     (:>= := :> :?)
	     (:? :< := :<=)
	     (:? :? :>= :=))
	  
	  #N((:= :<= :> :>)			; saf (same as "After")
	     (:>= := :> :>)
	     (:< :< := :<=)
	     (:< :< :>= :=))
	  
	  #N((:= :<= :> :?)			; sas
	     (:>= := :> :?)
	     (:< :< := :<=)
	     (:? :? :>= :=))
	  
	  #N((:= :<= :? :<=)			; fwf=
	     (:>= := :>= :=)
	     (:? :<= := :<=)
	     (:>= := :>= :=))
	  
	  #N((:= :<= :<= :<=)			; fws=
	     (:>= := := :<=)
	     (:>= := := :<=)
	     (:>= :>= :>= :=))
	  
	  #N((:= :<= :>= :=)			; swf=
	     (:>= := :>= :>=)
	     (:<= :<= := :<=)
	     (:= :<= :>= :=))
	  
	  #N((:= :<= := :<=)			; sws=
	     (:>= := :>= :?)
	     (:= :<= := :<=)
	     (:>= :? :>= :=))
	  
	  #N((:= :<= :? :<)			; fbf
	     (:>= := :? :<)
	     (:? :? := :<=)
	     (:> :> :>= :=))
	  
	  #N((:= :<= :< :<)			; fbs (same as "Before")
	     (:>= := :< :<)
	     (:> :> := :<=)
	     (:> :> :>= :=))
	  
	  #N((:= :<= :? :<)			; sbf
	     (:>= := :? :?)
	     (:? :? := :<=)
	     (:> :? :>= :=))
	  
	  #N((:= :<= :< :<)			; sbs
	     (:>= := :? :?)
	     (:> :? := :<=)
	     (:> :? :>= :=))))

;;;---------------------------------------------------------------------------
;;; SCALAR NETWORKS THAT CORRESPOND TO BRANCHING TIME PERIOD RELATIONS:
;;;---------------------------------------------------------------------------

;;; Created by using the function ENUMERATE-SINGLE-RB-PERIOD-RELATIONS.
;;;
;;;   RB  --> "Right Branching"
;;;  RBW  --> "Right Branch With"
;;;  URBW --> "Until Right Branch With"
;;;
;;;  length = 19
;;;
;;; Right Branching Intervals

(defvar *rb-pt-nets-SINGLE-WITHOUT-equals* nil)

(setf *rb-pt-nets-SINGLE-WITHOUT-equals*

	'((:IB  . #N((:RB= :RB< :RB~ :RB~)	; R5 (Incomparable)
		     (:RB> :RB= :RB~ :RB~)
		     (:RB~ :RB~ :RB= :RB<)
		     (:RB~ :RB~ :RB> :RB=)))
	  
	  (:AB  . #N((:RB= :RB< :RB> :RB~)	; After-RBW (R1)
		     (:RB> :RB= :RB> :RB~)	;  (inverse is
		     (:RB< :RB< :RB= :RB<)	;   Before-RBW)
		     (:RB~ :RB~ :RB> :RB=)))
	  
	  (:A   . #N((:RB= :RB< :RB> :RB>)	; After
		     (:RB> :RB= :RB> :RB>)
		     (:RB< :RB< :RB= :RB<)
		     (:RB< :RB< :RB> :RB=)))
	  
	  (:MI  . #N((:RB= :RB< :RB> :RB=)	; Met-By
		     (:RB> :RB= :RB> :RB>)
		     (:RB< :RB< :RB= :RB<)
		     (:RB= :RB< :RB> :RB=)))
	  
	  (:OIB . #N((:RB= :RB< :RB> :RB<)	; Overlapped-By-URBW (R3)
		     (:RB> :RB= :RB> :RB~)	;  (inverse is
		     (:RB< :RB< :RB= :RB<)	;   Overlaps-URBW)
		     (:RB> :RB~ :RB> :RB=)))
	  
	  (:OI  . #N((:RB= :RB< :RB> :RB<)	; Overlapped-By
		     (:RB> :RB= :RB> :RB>)
		     (:RB< :RB< :RB= :RB<)
		     (:RB> :RB< :RB> :RB=)))
	  
	  (:F   . #N((:RB= :RB< :RB> :RB<)	; Finishes
		     (:RB> :RB= :RB> :RB=)
		     (:RB< :RB< :RB= :RB<)
		     (:RB> :RB= :RB> :RB=)))
	  
	  (:D   . #N((:RB= :RB< :RB> :RB<)	; During
		     (:RB> :RB= :RB> :RB<)
		     (:RB< :RB< :RB= :RB<)
		     (:RB> :RB> :RB> :RB=)))
	  
	  (:SIB . #N((:RB= :RB< :RB= :RB<)	; Started-By-URBW (R6)
		     (:RB> :RB= :RB> :RB~)	;  (inverse is
		     (:RB= :RB< :RB= :RB<)	;   Started-By-URBW)
		     (:RB> :RB~ :RB> :RB=)))
	  
	  (:SI  . #N((:RB= :RB< :RB= :RB<)	; Started-By
		     (:RB> :RB= :RB> :RB>)
		     (:RB= :RB< :RB= :RB<)
		     (:RB> :RB< :RB> :RB=)))
	  
	  (:E   . #N((:RB= :RB< :RB= :RB<)	; Equals
		     (:RB> :RB= :RB> :RB=)
		     (:RB= :RB< :RB= :RB<)
		     (:RB> :RB= :RB> :RB=)))
	  
	  (:S   . #N((:RB= :RB< :RB= :RB<)	; Starts
		     (:RB> :RB= :RB> :RB<)
		     (:RB= :RB< :RB= :RB<)
		     (:RB> :RB> :RB> :RB=)))
	  
	  (:BB  . #N((:RB= :RB< :RB< :RB<)	; Before-RBW (R2)
		     (:RB> :RB= :RB~ :RB~)	;  (inverse is
		     (:RB> :RB~ :RB= :RB<)	;   After-RBW)
		     (:RB> :RB~ :RB> :RB=)))
	  
	  (:OB  . #N((:RB= :RB< :RB< :RB<)	; Overlaps-URBW (R4)
		     (:RB> :RB= :RB> :RB~)	;  (inverse is
		     (:RB> :RB< :RB= :RB<)	;   Overlapped-By-URBW)
		     (:RB> :RB~ :RB> :RB=)))
	  
	  (:DI  . #N((:RB= :RB< :RB< :RB<)	; Contains
		     (:RB> :RB= :RB> :RB>)
		     (:RB> :RB< :RB= :RB<)
		     (:RB> :RB< :RB> :RB=)))
	  
	  (:FI  . #N((:RB= :RB< :RB< :RB<)	; Finished-By
		     (:RB> :RB= :RB> :RB=)
		     (:RB> :RB< :RB= :RB<)
		     (:RB> :RB= :RB> :RB=)))
	  
	  (:O   . #N((:RB= :RB< :RB< :RB<)	; Overlaps
		     (:RB> :RB= :RB> :RB<)
		     (:RB> :RB< :RB= :RB<)
		     (:RB> :RB> :RB> :RB=)))
	  
	  (:M   . #N((:RB= :RB< :RB< :RB<)	; Meets
		     (:RB> :RB= :RB= :RB<)
		     (:RB> :RB= :RB= :RB<)
		     (:RB> :RB> :RB> :RB=)))
	  
	  (:B   . #N((:RB= :RB< :RB< :RB<)	; Before
		     (:RB> :RB= :RB< :RB<)
		     (:RB> :RB> :RB= :RB<)
		     (:RB> :RB> :RB> :RB=)))))

;;;---------------------------------------------------------------------------

;;;  length = 24
;;;
;;; Right Branching Intervals and Points

(defvar *rb-pt-nets-SINGLE-WITH-equals* nil)

(setf *rb-pt-nets-SINGLE-WITH-equals*
      
      '((:+R~  . #N((:RB=  :RB<= :RB~  :RB~) ; Incomparable (R5)
		    (:RB=> :RB=  :RB~  :RB~)
		    (:RB~  :RB~  :RB=  :RB<=)
		    (:RB~  :RB~  :RB=> :RB=)))
        
	(:+RBI . #N((:RB=  :RB<= :RB> :RB~) ; Branch-After (R1)
		    (:RB=> :RB=  :RB> :RB~) ;   inverse is
		    (:RB<  :RB<  :RB= :RB<) ;   Branch-Before
		    (:RB~  :RB~  :RB> :RB=)))
        
	(:+BI  . #N((:RB=  :RB<= :RB>  :RB>) ; After
		    (:RB=> :RB=  :RB>  :RB>)
		    (:RB<  :RB<  :RB=  :RB<=)
		    (:RB<  :RB<  :RB=> :RB=)))
        
	(:+MI  . #N((:RB= :RB< :RB> :RB=) ; Met-By
		    (:RB> :RB= :RB> :RB>)
		    (:RB< :RB< :RB= :RB<)
		    (:RB= :RB< :RB> :RB=)))
        
	(:+PF  . #N((:RB= :RB= :RB> :RB=) ;       X  Point-Finishes
		    (:RB= :RB= :RB> :RB=) ;       |   inverse is
		    (:RB< :RB< :RB= :RB<) ; |-----|   Point-Finished-By
		    (:RB= :RB= :RB> :RB=))) ;  Y
        
	(:+ROI . #N((:RB= :RB< :RB> :RB<) ; Branch-Overlapped-By (R3)
		    (:RB> :RB= :RB> :RB~) ;   inverse is
		    (:RB< :RB< :RB= :RB<) ;   Branch-Overlaps
		    (:RB> :RB~ :RB> :RB=)))
        
	(:+OI  . #N((:RB= :RB< :RB> :RB<) ; Overlapped-By
		    (:RB> :RB= :RB> :RB>)
		    (:RB< :RB< :RB= :RB<)
		    (:RB> :RB< :RB> :RB=)))
        
	(:+F   . #N((:RB= :RB< :RB> :RB<) ; Finishes
		    (:RB> :RB= :RB> :RB=)
		    (:RB< :RB< :RB= :RB<)
		    (:RB> :RB= :RB> :RB=)))
        
	(:+D   . #N((:RB=  :RB<= :RB> :RB<) ; During
		    (:RB=> :RB=  :RB> :RB<)
		    (:RB<  :RB<  :RB= :RB<)
		    (:RB>  :RB>  :RB> :RB=)))
        
	(:+PSI . #N((:RB= :RB< :RB= :RB=) ;       X     Point-Started-By
		    (:RB> :RB= :RB> :RB>) ;    |-----|   inverse is
		    (:RB= :RB< :RB= :RB=) ;    |         Point-Starts
		    (:RB= :RB< :RB= :RB=))) ;  Y
        
	(:+PE  . #N((:RB= :RB= :RB= :RB=) ;    X  Point-Equals
		    (:RB= :RB= :RB= :RB=) ;    |    inverse is
		    (:RB= :RB= :RB= :RB=) ;    |    Point-Equals
		    (:RB= :RB= :RB= :RB=))) ;  Y
        
	(:+RS  . #N((:RB= :RB< :RB= :RB<) ; Branch-Starts (R6)
		    (:RB> :RB= :RB> :RB~) ;   inverse is
		    (:RB= :RB< :RB= :RB<) ;   Branch-Starts
		    (:RB> :RB~ :RB> :RB=)))
        
	(:+SI  . #N((:RB= :RB< :RB= :RB<) ; Started-By
		    (:RB> :RB= :RB> :RB>)
		    (:RB= :RB< :RB= :RB<)
		    (:RB> :RB< :RB> :RB=)))
        
	(:+E   . #N((:RB= :RB< :RB= :RB<) ; Equals
		    (:RB> :RB= :RB> :RB=)
		    (:RB= :RB< :RB= :RB<)
		    (:RB> :RB= :RB> :RB=)))
        
	(:+S   . #N((:RB= :RB< :RB= :RB<) ; Starts
		    (:RB> :RB= :RB> :RB<)
		    (:RB= :RB< :RB= :RB<)
		    (:RB> :RB> :RB> :RB=)))
        
	(:+PS  . #N((:RB= :RB= :RB= :RB<) ;   X        Point-Starts
		    (:RB= :RB= :RB= :RB<) ;   |         inverse is
		    (:RB= :RB= :RB= :RB<) ;   |-----|   Point-Started-By
		    (:RB> :RB> :RB> :RB=))) ;  Y
        
	(:+RB  . #N((:RB= :RB< :RB<  :RB<) ; Branch-Before (R2)
		    (:RB> :RB= :RB~  :RB~) ;   inverse is
		    (:RB> :RB~ :RB=  :RB<=) ;   Branch-After
		    (:RB> :RB~ :RB=> :RB=)))
        
	(:+RO  . #N((:RB= :RB< :RB< :RB<) ; Branch-Overlaps (R4)
		    (:RB> :RB= :RB> :RB~) ;   inverse is
		    (:RB> :RB< :RB= :RB<) ;   Branch-Overlapped-By
		    (:RB> :RB~ :RB> :RB=)))
        
	(:+DI  . #N((:RB= :RB< :RB<  :RB<) ; Contains
		    (:RB> :RB= :RB>  :RB>)
		    (:RB> :RB< :RB=  :RB<=)
		    (:RB> :RB< :RB=> :RB=)))
        
	(:+FI  . #N((:RB= :RB< :RB< :RB<) ; Finished-By
		    (:RB> :RB= :RB> :RB=)
		    (:RB> :RB< :RB= :RB<)
		    (:RB> :RB= :RB> :RB=)))
        
	(:+O   . #N((:RB= :RB< :RB< :RB<) ; Overlaps
		    (:RB> :RB= :RB> :RB<)
		    (:RB> :RB< :RB= :RB<)
		    (:RB> :RB> :RB> :RB=)))
        
	(:+PFI . #N((:RB= :RB< :RB< :RB<) ;    X     Point-Finished-By
		    (:RB> :RB= :RB= :RB=) ; |-----|   inverse is
		    (:RB> :RB= :RB= :RB=) ;       |   Point-Finishes
		    (:RB> :RB= :RB= :RB=))) ;     Y
        
	(:+M   . #N((:RB= :RB< :RB< :RB<) ; Meets
		    (:RB> :RB= :RB= :RB<)
		    (:RB> :RB= :RB= :RB<)
		    (:RB> :RB> :RB> :RB=)))
        
	(:+B   . #N((:RB=  :RB<= :RB<  :RB<) ; Before
		    (:RB=> :RB=  :RB<  :RB<)
		    (:RB>  :RB>  :RB=  :RB<=)
		    (:RB>  :RB>  :RB=> :RB=)))))

;;;---------------------------------------------------------------------------

;;; ENUMERATE-END-PT-RB-POINT-RELATIONS:
;;;
;;;  length = 16
;;;
;;; Right Branching Matuzsek Intervals

(defvar *rb-pt-nets-MULTIPLE-WITHOUT-equals* nil)

(setf *rb-pt-nets-MULTIPLE-WITHOUT-equals*

	'(#N((:RB=    :RB<  :RB<=>~ :RB<~)
	     (:RB>    :RB=  :RB>~   :RB~)
	     (:RB<=>~ :RB<~ :RB=    :RB<)
	     (:RB>~   :RB~  :RB>    :RB=))
 
	  #N((:RB=  :RB< :RB<~ :RB<~)
	     (:RB>  :RB= :RB~  :RB~)
	     (:RB>~ :RB~ :RB=  :RB<)
	     (:RB>~ :RB~ :RB>  :RB=))
 
	  #N((:RB=  :RB< :RB>~ :RB~)
	     (:RB>  :RB= :RB>~ :RB~)
	     (:RB<~ :RB<~ :RB= :RB<)
	     (:RB~  :RB~ :RB> :RB=))
 
	  #N((:RB= :RB< :RB~ :RB~)
	     (:RB> :RB= :RB~ :RB~)
	     (:RB~ :RB~ :RB= :RB<)
	     (:RB~ :RB~ :RB> :RB=))
 
	  #N((:RB=   :RB< :RB<=> :RB<=>)
	     (:RB>   :RB= :RB>   :RB>)
	     (:RB<=> :RB< :RB=   :RB<)
	     (:RB<=> :RB< :RB>   :RB=))
 
	  #N((:RB=    :RB<    :RB<=> :RB<=>~)
	     (:RB>    :RB=    :RB>   :RB<=>~)
	     (:RB<=>  :RB<    :RB=   :RB<)
	     (:RB<=>~ :RB<=>~ :RB>   :RB=))
 
	  #N((:RB= :RB< :RB> :RB>)
	     (:RB> :RB= :RB> :RB>)
	     (:RB< :RB< :RB= :RB<)
	     (:RB< :RB< :RB> :RB=))
 
	  #N((:RB=    :RB<    :RB> :RB<=>~)
	     (:RB>    :RB=    :RB> :RB<=>~)
	     (:RB<    :RB<    :RB= :RB<)
	     (:RB<=>~ :RB<=>~ :RB> :RB=))
 
	  #N((:RB=   :RB< :RB<=> :RB<)
	     (:RB>   :RB= :RB>   :RB=)
	     (:RB<=> :RB< :RB=   :RB<)
	     (:RB>   :RB= :RB>   :RB=))
 
	  #N((:RB= :RB< :RB< :RB<)
	     (:RB> :RB= :RB= :RB<)
	     (:RB> :RB= :RB= :RB<)
	     (:RB> :RB> :RB> :RB=))
 
	  #N((:RB= :RB< :RB> :RB=)
	     (:RB> :RB= :RB> :RB>)
	     (:RB< :RB< :RB= :RB<)
	     (:RB= :RB< :RB> :RB=))
 
	  #N((:RB= :RB<    :RB= :RB<)
	     (:RB> :RB=    :RB> :RB<=>~)
	     (:RB= :RB<    :RB= :RB<)
	     (:RB> :RB<=>~ :RB> :RB=))
 
	  #N((:RB=   :RB<   :RB<=> :RB<)
	     (:RB>   :RB=   :RB<=> :RB<)
	     (:RB<=> :RB<=> :RB=   :RB<)
	     (:RB>   :RB>   :RB>   :RB=))
 
	  #N((:RB= :RB< :RB< :RB<)
	     (:RB> :RB= :RB< :RB<)
	     (:RB> :RB> :RB= :RB<)
	     (:RB> :RB> :RB> :RB=))
 
	  #N((:RB=   :RB<    :RB<=>  :RB<)
	     (:RB>   :RB=    :RB<=>~ :RB<=>~)
	     (:RB<=> :RB<=>~ :RB=    :RB<)
	     (:RB>   :RB<=>~ :RB>    :RB=))
 
	  #N((:RB= :RB<    :RB<    :RB<)
	     (:RB> :RB=    :RB<=>~ :RB<=>~)
	     (:RB> :RB<=>~ :RB=    :RB<)
	     (:RB> :RB<=>~ :RB>    :RB=))))

;;;---------------------------------------------------------------------------

;;; ENUMERATE-END-PT-RB-POINT-RELATIONS:
;;;
;;;  length = 16
;;;
;;; Right Branching Matuzsek Intervals & Points

(defvar *rb-pt-nets-MULTIPLE-WITH-equals* nil)

(setf *rb-pt-nets-MULTIPLE-WITH-equals*

	'(#N((:RB=    :RB<= :RB<=>~ :RB<~)
	     (:RB=>   :RB=  :RB>~   :RB~)
	     (:RB<=>~ :RB<~ :RB=    :RB<=)
	     (:RB>~   :RB~  :RB=>   :RB=))
	  
	  #N((:RB=  :RB<= :RB<~ :RB<~)
	     (:RB=> :RB=  :RB~  :RB~)
	     (:RB>~ :RB~  :RB=  :RB<=)
	     (:RB>~ :RB~  :RB=> :RB=))
	  
	  #N((:RB=  :RB<= :RB>~ :RB~)
	     (:RB=> :RB=  :RB>~ :RB~)
	     (:RB<~ :RB<~ :RB=  :RB<=)
	     (:RB~  :RB~  :RB=> :RB=))
	  
	  #N((:RB=  :RB<= :RB~  :RB~)
	     (:RB=> :RB=  :RB~  :RB~)
	     (:RB~  :RB~  :RB=  :RB<=)
	     (:RB~  :RB~  :RB=> :RB=))
	  
	  #N((:RB=   :RB<= :RB<=> :RB<=>)
	     (:RB=>  :RB=  :RB>   :RB>)
	     (:RB<=> :RB<  :RB=   :RB<=)
	     (:RB<=> :RB<  :RB=>  :RB=))
	  
	  #N((:RB=    :RB<=   :RB<=> :RB<=>~)
	     (:RB=>   :RB=    :RB>   :RB<=>~)
	     (:RB<=>  :RB<    :RB=   :RB<=)
	     (:RB<=>~ :RB<=>~ :RB=>  :RB=))
	  
	  #N((:RB=  :RB<= :RB>  :RB>)
	     (:RB=> :RB=  :RB>  :RB>)
	     (:RB<  :RB<  :RB=  :RB<=)
	     (:RB<  :RB<  :RB=> :RB=))
	  
	  #N((:RB=    :RB<=   :RB>  :RB<=>~)
	     (:RB=>   :RB=    :RB>  :RB<=>~)
	     (:RB<    :RB<    :RB=  :RB<=)
	     (:RB<=>~ :RB<=>~ :RB=> :RB=))
	  
	  #N((:RB=   :RB<= :RB<=> :RB<=)
	     (:RB=>  :RB=  :RB=>  :RB=)
	     (:RB<=> :RB<= :RB=   :RB<=)
	     (:RB=>  :RB=  :RB=>  :RB=))
	  
	  #N((:RB=  :RB<= :RB<= :RB<=)
	     (:RB=> :RB=  :RB=  :RB<=)
	     (:RB=> :RB=  :RB=  :RB<=)
	     (:RB=> :RB=> :RB=> :RB=))
	  
	  #N((:RB=  :RB<= :RB=> :RB=)
	     (:RB=> :RB=  :RB=> :RB=>)
	     (:RB<= :RB<= :RB=  :RB<=)
	     (:RB=  :RB<= :RB=> :RB=))
	  
	  #N((:RB=  :RB<=   :RB=  :RB<=)
	     (:RB=> :RB=    :RB=> :RB<=>~)
	     (:RB=  :RB<=   :RB=  :RB<=)
	     (:RB=> :RB<=>~ :RB=> :RB=))
	  
	  #N((:RB=   :RB<=  :RB<=> :RB<)
	     (:RB=>  :RB=   :RB<=> :RB<)
	     (:RB<=> :RB<=> :RB=   :RB<=)
	     (:RB>   :RB>   :RB=> :RB=))
	  
	  #N((:RB=  :RB<= :RB<  :RB<)
	     (:RB=> :RB=  :RB<  :RB<)
	     (:RB>  :RB>  :RB=  :RB<=)
	     (:RB>  :RB>  :RB=> :RB=))
	  
	  #N((:RB=   :RB<=   :RB<=>  :RB<)
	     (:RB=>  :RB=    :RB<=>~ :RB<=>~)
	     (:RB<=> :RB<=>~ :RB=    :RB<=)
	     (:RB>   :RB<=>~ :RB=>   :RB=))
	  
	  #N((:RB=  :RB<=   :RB<    :RB<)
	     (:RB=> :RB=    :RB<=>~ :RB<=>~)
	     (:RB>  :RB<=>~ :RB=    :RB<=)
	     (:RB>  :RB<=>~ :RB=>   :RB=))))

;;;---------------------------------------------------------------------------

;;; (enumerate-single-relation-nets '(:LB< :LB= :LB> :LB~) *lb=net*)

;;;  length = 24
;;;
;;; Left Branching Intervals & Points
;;;
;;; LEGEND: X,Y ----- Intervals or Points
;;;         Xi,Yi --- Intervals only
;;;         Xp,Yp --- Points only

(defvar *lb-pt-nets-SINGLE-WITH-equals* nil)
  
(setf *lb-pt-nets-SINGLE-WITH-equals*
      
      '((:>=L~  . #N((:LB=  :LB<= :LB~  :LB~)     ; X <Incomparable> Y
                     (:LB=> :LB=  :LB~  :LB~)     ;  (It's its own inverse)
                     (:LB~  :LB~  :LB=  :LB<=)
                     (:LB~  :LB~  :LB=> :LB=)))
        
        (:>=LBI . #N((:LB= :LB< :LB~  :LB~)       ; Xi <Branch-After> Y
                     (:LB> :LB= :LB>  :LB>)
                     (:LB~ :LB< :LB=  :LB<=)
                     (:LB~ :LB< :LB=> :LB=)))
        
        (:>=LB  . #N((:LB=  :LB<= :LB~ :LB<)      ; X <Branch-Before> Yi
                     (:LB=> :LB=  :LB~ :LB<)
                     (:LB~  :LB~  :LB= :LB<)
                     (:LB>  :LB>  :LB> :LB=)))
        
        (:>=LOI . #N((:LB= :LB< :LB~ :LB<)        ; Xi <Branch-Overlapped-By> Yi
                     (:LB> :LB= :LB> :LB>)
                     (:LB~ :LB< :LB= :LB<)
                     (:LB> :LB< :LB> :LB=)))
        
        (:>=LF  . #N((:LB= :LB< :LB~ :LB<)        ; Xi <Branch-Finishes> Yi
                     (:LB> :LB= :LB> :LB=)        ;  (It's its own inverse)
                     (:LB~ :LB< :LB= :LB<)
                     (:LB> :LB= :LB> :LB=)))
        
        (:>=LO  . #N((:LB= :LB< :LB~ :LB<)        ; Xi <Branch-Overlaps> Yi
                     (:LB> :LB= :LB> :LB<)
                     (:LB~ :LB< :LB= :LB<)
                     (:LB> :LB> :LB> :LB=)))
        
        (:>=BI  . #N((:LB=  :LB<= :LB>  :LB>)     ; X <After> Y
                     (:LB=> :LB=  :LB>  :LB>)
                     (:LB<  :LB<  :LB=  :LB<=)
                     (:LB<  :LB<  :LB=> :LB=)))
        
        (:>=MI  . #N((:LB= :LB< :LB> :LB=)        ; Xi <Met-By> Yi
                     (:LB> :LB= :LB> :LB>)
                     (:LB< :LB< :LB= :LB<)
                     (:LB= :LB< :LB> :LB=)))
        
        (:>=PF  . #N((:LB= :LB= :LB> :LB=)        ; Xp <Point-FInishes> Yi
                     (:LB= :LB= :LB> :LB=)
                     (:LB< :LB< :LB= :LB<)
                     (:LB= :LB= :LB> :LB=)))
        
        (:>=OI  . #N((:LB= :LB< :LB> :LB<)        ; Xi <Overlapped-By> Yi
                     (:LB> :LB= :LB> :LB>)
                     (:LB< :LB< :LB= :LB<)
                     (:LB> :LB< :LB> :LB=)))
        
        (:>=F   . #N((:LB= :LB< :LB> :LB<)        ; Xi <Finishes> Yi
                     (:LB> :LB= :LB> :LB=)
                     (:LB< :LB< :LB= :LB<)
                     (:LB> :LB= :LB> :LB=)))
        
        (:>=D   . #N((:LB=  :LB<= :LB> :LB<)      ; X <During> Yi
                     (:LB=> :LB=  :LB> :LB<)
                     (:LB<  :LB<  :LB= :LB<)
                     (:LB>  :LB>  :LB> :LB=)))
        
        (:>=PSI . #N((:LB= :LB< :LB= :LB=)        ; Xi <Point-Started-By> Yp
                     (:LB> :LB= :LB> :LB>)
                     (:LB= :LB< :LB= :LB=)
                     (:LB= :LB< :LB= :LB=)))
        
        (:>=PE  . #N((:LB= :LB= :LB= :LB=)        ; Xp <Point-Equals> Yp
                     (:LB= :LB= :LB= :LB=)
                     (:LB= :LB= :LB= :LB=)
                     (:LB= :LB= :LB= :LB=)))
        
        (:>=SI  . #N((:LB= :LB< :LB= :LB<)        ; Xi <Started-By> Yi
                     (:LB> :LB= :LB> :LB>)
                     (:LB= :LB< :LB= :LB<)
                     (:LB> :LB< :LB> :LB=)))
        
        (:>=E   . #N((:LB= :LB< :LB= :LB<)        ; Xi <Equals> Yi
                     (:LB> :LB= :LB> :LB=)
                     (:LB= :LB< :LB= :LB<)
                     (:LB> :LB= :LB> :LB=)))
        
        (:>=S   . #N((:LB= :LB< :LB= :LB<)        ; Xi <Starts> Yi
                     (:LB> :LB= :LB> :LB<)
                     (:LB= :LB< :LB= :LB<)
                     (:LB> :LB> :LB> :LB=)))
        
        (:>=PS  . #N((:LB= :LB= :LB= :LB<)        ; Xp <Point-Starts> Yi
                     (:LB= :LB= :LB= :LB<)
                     (:LB= :LB= :LB= :LB<)
                     (:LB> :LB> :LB> :LB=)))
        
        (:>=DI  . #N((:LB= :LB< :LB<  :LB<)       ; Xi <Contains> Y
                     (:LB> :LB= :LB>  :LB>)
                     (:LB> :LB< :LB=  :LB<=)
                     (:LB> :LB< :LB=> :LB=)))
        
        (:>=FI  . #N((:LB= :LB< :LB< :LB<)        ; Xi <Finished-By> Yi
                     (:LB> :LB= :LB> :LB=)
                     (:LB> :LB< :LB= :LB<)
                     (:LB> :LB= :LB> :LB=)))
        
        (:>=O   . #N((:LB= :LB< :LB< :LB<)        ; Xi <Overlaps> Yi
                     (:LB> :LB= :LB> :LB<)
                     (:LB> :LB< :LB= :LB<)
                     (:LB> :LB> :LB> :LB=)))
        
        (:>=PFI . #N((:LB= :LB< :LB< :LB<)        ; Xi <Point-Finished-By> Yp
                     (:LB> :LB= :LB= :LB=)
                     (:LB> :LB= :LB= :LB=)
                     (:LB> :LB= :LB= :LB=)))
        
        (:>=M   . #N((:LB= :LB< :LB< :LB<)        ; Xi <Meets> Yi
                     (:LB> :LB= :LB= :LB<)
                     (:LB> :LB= :LB= :LB<)
                     (:LB> :LB> :LB> :LB=)))
        
        (:>=B   . #N((:LB=  :LB<= :LB<  :LB<)     ; X <Before> Y
                     (:LB=> :LB=  :LB<  :LB<)
                     (:LB>  :LB>  :LB=  :LB<=)
                     (:LB>  :LB>  :LB=> :LB=)))))

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
