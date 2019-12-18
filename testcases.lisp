;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                     TEST ALGEBRAS
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

;(in-package "CONSTRAINT-MAINTENANCE")

(setq
  scalar-test-algebra-1
  (define-algebra
    'SCALAR-RS
    :Documentation "This is a very simple, strict, scalar algebra."
    :Type :relation-system
    :Inverse-Relations
    '(      :>      :=      :<     )
    :Relation-List
    ;;      001     010     100
    ;;       1       2       4
    '(      :<      :=      :>     )
    ;;------------------------------------------
    :Multiplication-Table
    '((     :<      :<  (:< := :>) )		; :<
      (     :<      :=      :>     )		; :=
      ( (:< := :>)  :>      :>     ))))		; :>


(mult :< :< scalar-test-algebra-1)	; ==> :<
(mult :< := scalar-test-algebra-1)	; ==> :<
(mult :< :> scalar-test-algebra-1)	; ==> (:< := :>)
(mult := :< scalar-test-algebra-1)	; ==> :<
(mult := := scalar-test-algebra-1)	; ==> :=
(mult := :> scalar-test-algebra-1)	; ==> :>
(mult :> :< scalar-test-algebra-1)	; ==> (:< := :>)
(mult :> := scalar-test-algebra-1)	; ==> :>
(mult :> :> scalar-test-algebra-1)	; ==> :>

(mult '(:< :>) :> scalar-test-algebra-1)	; ==> (:< := :>)
(mult :> '(:= :>) scalar-test-algebra-1)	; ==> :>

(let* ((alg  (find-algebra 'SCALAR-RS))
       (enc  (get-algebra-encoder  alg))
       (dec  (get-algebra-decoder  alg))
       (inv  (get-algebra-inverter alg))
       (rels (get-algebra-rels     alg)))
  (pairlis rels
	   ;; Construct the list of inverses of RELS the hard way.
	   (mapcar dec
		   (mapcar inv
			   (mapcar enc rels)))))
; ==> ((:> . :<) (:= . :=) (:< . :>))

; (let* ((alg  (find-algebra 'SCALAR))
;        (enc  (get-algebra-encoder  alg))
;        (dec  (get-algebra-decoder  alg))
;        (inv  (get-algebra-inverter alg))
;        (rels (get-algebra-rels     alg)))
;   (pairlis rels
; 	   ;; Construct the list of inverses of RELS the hard way.
; 	   (mapcar dec
; 		   (mapcar inv
; 			   (mapcar enc rels)))))
; ==> ((:? . :?) (:>= . :<=) (:/= . :/=)
;      (:> . :<) (:<= . :>=) (:= . :=) (:< . :>))

;;;...........................................................................

(setq scalar-test-algebra-2 (find-algebra 'scalar))

(mult :< :< scalar-test-algebra-2)	; ==> :<
(mult :< := scalar-test-algebra-2)	; ==> :<
(mult :< :> scalar-test-algebra-2)	; ==> :?
(mult := :< scalar-test-algebra-2)	; ==> :<
(mult := := scalar-test-algebra-2)	; ==> :=
(mult := :> scalar-test-algebra-2)	; ==> :>
(mult :> :< scalar-test-algebra-2)	; ==> :?
(mult :> := scalar-test-algebra-2)	; ==> :>
(mult :> :> scalar-test-algebra-2)	; ==> :>

(mult :/= :> scalar-test-algebra-2)	; ==> :?
(mult :> :>= scalar-test-algebra-2)	; ==> :>

;;;...........................................................................

(setq
  scalar-test-algebra-3
  (define-algebra
    'SCALAR-RELATION-2 :Documentation "This tests the ability to input a coded mult table."
    :Type :relation-system
    :Relation-List
    ;;  1  2  4
    '( :< := :> )
    ;;------------------------------------------------
    :Multiplication-Table
    '(( 1  1  7 )				; :<
      ( 1  2  4 )				; :=
      ( 7  4  4 ))))				; :>

(mult :< :< scalar-test-algebra-3)	; ==> :<
(mult :< := scalar-test-algebra-3)	; ==> :<
(mult :< :> scalar-test-algebra-3)	; ==> (:< := :>)
(mult := :< scalar-test-algebra-3)	; ==> :<
(mult := := scalar-test-algebra-3)	; ==> :=
(mult := :> scalar-test-algebra-3)	; ==> :>
(mult :> :< scalar-test-algebra-3)	; ==> (:< := :>)
(mult :> := scalar-test-algebra-3)	; ==> :>
(mult :> :> scalar-test-algebra-3)	; ==> :>

(mult '(:< :>) :> scalar-test-algebra-3)	; ==> (:< := :>)
(mult :> '(:= :>) scalar-test-algebra-3)	; ==> :>

;;;...........................................................................

(setq period-test-algebra-1 (find-algebra 'period))

(mult :S  :O period-test-algebra-1)			;(:B :M :O)
(mult :S  :E period-test-algebra-1)			;(:S)
(mult :MI :O period-test-algebra-1)			;(:D :F :OI)
(mult :MI :E period-test-algebra-1)			;(:MI)
(mult '(:S :MI) '(:O :E) period-test-algebra-1)	;(:B :D :F :M :MI :O :OI :S)

(let* ((alg  (find-algebra 'PERIOD))
       (enc  (get-algebra-encoder  alg))
       (dec  (get-algebra-decoder  alg))
       (inv  (get-algebra-inverter alg))
       (rels (get-algebra-rels     alg)))
  (pairlis rels
	   ;; Construct the list of inverses of RELS the hard way.
	   (mapcar dec
		   (mapcar inv
			   (mapcar enc rels)))))
;((:SI . :S) (:S . :SI) (:OI . :O) (:O . :OI) (:MI . :M) (:M . :MI) (:FI . :F) (:F . :FI)
; (:DI . :D) (:D . :DI) (:A . :B) (:E . :E) (:B . :A))

;;;...........................................................................

(setq test-algebra-4 (find-algebra 'right-branching-scalar))

(mult :rb>  :rb<  test-algebra-4)	;:RB<=>~
(mult :rb>  :rb=  test-algebra-4)	;:RB>
(mult :rb>  :rb>  test-algebra-4)	;:RB>
(mult :rb<> :rb>  test-algebra-4)	;:RB<=>
(mult :rb>  :rb=> test-algebra-4)	;:RB>

;;;...........................................................................

(define-algebra
  'RIGHT-BRANCHING-SCALAR-RS
  :Documentation "Basic scalar, right branching time relation system."
  :Type :relation-system
  :Relation-List
  '(  :rb<   :rb=   :rb>   :rb~ )
  :Multiplication-Table
  ;;-----------------------------
  '((   1      1      7      9  )
    (   1      2      4      8  )
    (  15      4      4      8  )
    (   8      8     12     15  )))

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                     TEST NETWORK DEFINITIONS
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(setf test-period-net1 #N(((:E)     (:D :DI) (:F :FI) (:MI :SI))
			  ((:D :DI) (:E)     (:D :DI) (:OI)    )
			  ((:F :FI) (:D :DI) (:E)     (:MI :SI))
			  ((:M :S)  (:O)     (:M :S)  (:E)     )))

; ==> #N(( :E      (:D :DI) (:F :FI) (:MI :SI))
;        ((:D :DI)  :E      (:D :DI)  :OI     )
;        ((:F :FI) (:D :DI)  :E      (:MI :SI))
;        ((:M :S)   :O      (:M :S)   :E      ))



;;;...........................................................................

(nref test-period-net1 1 1)			; ==> :E
(nref test-period-net1 2 4)			; ==> :OI
(nref test-period-net1 3 2)			; ==> (:D :DI)
(nref test-period-net1 4 4)			; ==> :E

;;;...........................................................................

;;; A <overlaps> B
;;;
;;;    AAAA           A = [a1,a2]
;;;      BBBBBB       B = [b1,b2]

;;;                         a1   a2   b1   b2
(setf test-scalar-net1 #N(( :=   :<   :<   :< )
			  ( :>   :=   :>   :< )
			  ( :>   :<   :=   :< )
			  ( :>   :>   :>   := )))
; ==> #N((:= :< :< :<)
;        (:> := :> :<)
;        (:> :< := :<)
;        (:> :> :> :=))

;;;...........................................................................

(setq test-rb-net1 #N((:RB= :RB< :RB> :RB>)
		      (:RB> :RB= :RB> :RB>)
		      (:RB< :RB< :RB= :RB<)
		      (:RB< :RB< :RB> :RB=)))
; ==> #N((:RB= :RB< :RB> :RB>)
;        (:RB> :RB= :RB> :RB>)
;        (:RB< :RB< :RB= :RB<)
;        (:RB< :RB< :RB> :RB=))

;;; Duration algebra test

(setq *inf-dur* #I(#.most-negative-fixnum #.most-positive-fixnum))

(setq test-dur-net1 #N((#I( 1  1)    #I( 1   2 )  #.*INF-DUR*)
		       (#I(1/2 1)    #I( 1   1 )  #I( 2  3 ))
		       (#.*INF-DUR*  #I(1/3 1/2)  #I( 1  1 ))))

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                     TEST NETWORK PROPAGATOR
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(propagate #N(( :E      (:D :DI) (:F :FI) (:MI :SI))
	      ((:D :DI)  :E      (:D :DI)  :OI     )
	      ((:F :FI) (:D :DI)  :E      (:MI :SI))
	      ((:M :S)   :O      (:M :S)   :E      )))

; ==> #N(( :E      (:D :DI) (:F :FI) (:MI :SI))
;        ((:D :DI)  :E      (:D :DI)  :OI     )
;        ((:F :FI) (:D :DI)  :E      (:MI :SI))
;        ((:M :S)   :O      (:M :S)   :E      ))

;;;...........................................................................

(propagate test-dur-net1)

; ==> #N((      1     #I( 1   2 ) #I(2 6))
;        (#I(1/2  1)        1     #I(2 3))
;        (#I(1/6 1/2) #I(1/3 1/2)     1  ))

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                     TEST EQUAL-NETS-P AND SKEW-OF-P
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(skew-of-p #N(( :=   :<   :<   :< )
	      ( :>   :=   :>   :< )
	      ( :>   :<   :=   :< )
	      ( :>   :>   :>   := ))
	   #N(( :=   :<   :<   :< )
	      ( :>   :=   :>   :< )
	      ( :>   :<   :=   :< )
	      ( :>   :>   :>   := )))		; ==> T

(equal-nets-p test-period-net1 test-period-net1)	; ==> T
(equal-nets-p test-dur-net1 test-dur-net1)		; ==> T
(equal-nets-p test-rb-net1 test-rb-net1)		; ==> T
(equal-nets-p test-scalar-net1 test-scalar-net1)	; ==> T
(equal-nets-p test-dur-net1 test-period-net1)	; ==> NIL
(equal-nets-p test-rb-net1  test-period-net1)	; ==> NIL
(equal-nets-p #N(( :=   :<   :<   :< )
		 ( :>   :=   :>   :< )
		 ( :>   :<   :=   :< )
		 ( :>   :>   :>   := ))
	      #N(( :=   :<   :<   :> )
		 ( :>   :=   :>   :< )
		 ( :>   :<   :=   :< )
		 ( :>   :>   :>   := )))	; ==> NIL

(skew-of-p test-period-net1 test-period-net1)	; ==> T
;(skew-of-p test-dur-net1 test-dur-net1)		; ==> T
(skew-of-p test-rb-net1 test-rb-net1)		; ==> T
(skew-of-p test-scalar-net1 test-scalar-net1)	; ==> T
(skew-of-p test-dur-net1 test-period-net1)	; ==> NIL
(skew-of-p test-rb-net1  test-period-net1)	; ==> NIL
(skew-of-p #N(( :=   :<   :<   :< )
	      ( :>   :=   :>   :< )
	      ( :>   :<   :=   :< )
	      ( :>   :>   :>   := ))
	   #N(( :=   :<   :<   :> )
	      ( :>   :=   :>   :< )
	      ( :>   :<   :=   :< )
	      ( :>   :>   :>   := )))		; ==> NIL


(setq rel-fi (cdr (assoc :fi *allen-pt-nets-WITHOUT-equals*)))
;#N((:= :< :< :<)
;   (:> := :> :=)
;   (:> :< := :<)
;   (:> := :> :=))

(setq rel-f  (cdr (assoc :f  *allen-pt-nets-WITHOUT-equals*)))
;#N((:= :< :> :<)
;   (:> := :> :=)
;   (:< :< := :<)
;   (:> := :> :=))

(skew-of-p rel-fi rel-f)
(inverse-net-of-p rel-fi rel-f)

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                     AN EXPERIMENT IN 4-CONSISTENCY
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------


;(setf test-net #N(( :=   :<                     )
;		  ( :>   :=                     )
;		  (           :=   :<           )
;		  (           :>   :=           )
;		  (                     :=   :< )
;		  (                     :>   := )))