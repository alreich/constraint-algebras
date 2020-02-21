;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

(in-package "CONSTRAINT-MAINTENANCE")

(define-algebra
  'SCALAR-RS
  :Documentation "This is a simple strict scalar relation system."
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
    ( (:< := :>)  :>      :>     )))		; :>

;;;---------------------------------------------------------------------------

(define-algebra
  'VAN-BEEK-PA-SCALAR				; Same as SCALAR but w/o "/="
  :Type :constraint-algebra
  :Documentation "PA in van Beek's paper (p.1294 of #53).  Has no /= relation."
  :Inverse-Relations
  '( :PA>  :PA=  :PA>= :PA<  :PA<= :PA?)
  :Relation-List
  ;; 001   010   011   100   110   111
  ;;  1     2     3     4     6     7    <---------- 5 would correspond to /=
  '( :PA<  :PA=  :PA<= :PA>  :PA>= :PA?)
  ;;------------------------------------
  :Multiplication-Table
  '(( 1     1     1     7     7     7  )
    ( 1     2     3     4     6     7  )
    ( 1     3     3     7     7     7  )
    ( 7     4     7     4     4     7  )
    ( 7     6     7     4     6     7  )
    ( 7     7     7     7     7     7  )))

;;;---------------------------------------------------------------------------

;;; Same as in the file "basic-algebras.lisp", only there the
;;; relation symbol, A, has been changed to, BI.

#|
(define-algebra
  'PERIOD  :Documentation "Allen's Theory of Time."
  :Type :relation-system
  :Inverse-Relations
  '(   :A     :E     :B     :DI   :D      :FI   :F      :MI   :M      :OI   :O      :SI   :S )
  :Relation-List
  ;;    1      2      4      8     16     32     64    128    256    512   1024   2048   4096
  '(   :B     :E     :A     :D    :DI     :F    :FI     :M    :MI     :O    :OI     :S    :SI)
  ;;-----------------------------------------------------------------------------------------
  :Multiplication-Table
  '((   1      1   8191   2697      1   2697      1      1   2697      1   2697      1      1)	;   :B
    (   1      2      4      8     16     32     64    128    256    512   1024   2048   4096)	;   :E
    (8191      4      4   1324      4      4      4   1324      4   1324      4   1324      4)	;   :A
    (   1      8      4      8   8191      8   2697      1      4   2697   1324      8   1324)	;   :D
    ( 721     16   5396   7802     16   5136     16    592   5136    592   5136    592     16)	;   :DI
    (   1     32      4      8   5396     32     98    128      4   2568   1284      8   1284)	;   :F
    (   1     64   5396   2568     16     98     64    128   5136    512   5136    512     16)	;   :FI
    (   1    128   5396   2568      1   2568      1      1     98      1   2568    128    128)	;   :M
    ( 721    256      4   1064      4    256    256   6146      4   1064      4   1064      4)	;   :MI
    (   1    512   5396   2568    721   2568    641      1   5136    641   7802    512    592)	;   :O
    ( 721   1024      4   1064   5396   1024   5136    592      4   7802   1284   1064   1284)	;   :OI
    (   1   2048      4      8    721      8    641      1    256    641   1064   2048   6146)	;   :S
    ( 721   4096      4   1064     16   1024     16    592    256    592   1024   6146   4096))) ;   :SI
|#  

;;;---------------------------------------------------------------------------

#|
(define-algebra
  'RIGHT-BRANCHING-SCALAR-RS
  :Documentation "Al Reich's basic scalar, right branching time, relation system."
  :Type :relation-system
  :Relation-List
  '(  :RB<   :RB=   :RB>   :RB~ )
  :Multiplication-Table
  ;;-----------------------------
  '((   1      1      7      9  )
    (   1      2      4      8  )
    (  15      4      4      8  )
    (   8      8     12     15  )))

(setq INVERSE-RB-POINT-REL
      '((:rb< :rb>) (:rb= :rb=) (:rb<= :RB=>) (:rb> :rb<) (:RB<> :RB<>)
        (:RB=> :rb<=) (:RB<=> :RB<=>) (:RB~ :RB~) (:RB<~ :RB>~) (:RB=~ :RB=~)
        (:RB<=~ :RB=>~) (:RB>~ :RB<~) (:RB<>~ :RB<>~) (:RB=>~ :RB<=~)
        (:RB<=>~ :RB<=>~)))

(setq INVERSE-RB-POINT-CODED-REL
      '((1 4) (2 2) (3 6) (4 1) (5 5) (6 3) (7 7) (8 8) (9 12)
        (10 10) (11 14) (12 9) (13 13) (14 11) (15 15) (0 0)))
|#

;;;---------------------------------------------------------------------------

#|
(define-algebra
    'LEFT-BRANCHING-SCALAR-RS
    :Documentation "Al Reich's basic scalar, left branching time, relation system."
    :Type :relation-system
    :Inverse-Relations
    '(  :LB>   :LB=   :LB<   :LB~ )
    :Relation-List
    ;;  0001   0010   0100   1000
    '(  :LB<   :LB=   :LB>   :LB~ )
    :Multiplication-Table
    ;;-----------------------------
    '((   1      1     15      8  )	; :LB<
      (   1      2      4      8  )	; :LB=
      (   7      4      4     12  )	; :LB>
      (   9      8      8     15  )))	; :LB~


(setf RELMAP
      (pairlis
       '(:LB< :LB= :LB<= :LB> :LB<> :LB=> :LB<=> :LB~ :LB<~ :LB=~ :LB<=~ :LB>~ :LB<>~ :LB=>~ :LB<=>~)
       '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))

(setf invs (mapcar inv crels))
;;; ==> (4 2 6 1 5 3 7 8 12 10 14 9 13 11 15)

(setf invrels (mapcar #'(lambda (crel) (car (rassoc crel relmap))) invs))
;;; ==> (:LB> :LB= :LB=> :LB< :LB<> :LB<= :LB<=> :LB~ :LB>~ :LB=~ :LB=>~
;;;      :LB<~ :LB<>~ :LB<=~ :LB<=>~)

(pprint (setf tbl (relsys-to-conalg relmap lbrs)))
|#

;;;---------------------------------------------------------------------------

;;;---------------------------------------------------------------------------

(define-algebra
  'TEST-PERIOD  :Documentation "Automatically derived version of Allen's Theory of Time."
  :Type :relation-system
  :Inverse-Relations
  '(:+B :+M  :+O  :+FI :+DI :+S  :+E :+SI :+D  :+F  :+OI :+MI :+A)
  :Relation-List
  '(:+A :+MI :+OI :+F  :+D  :+SI :+E :+S  :+DI :+FI :+O  :+M  :+B)
  ;;-----------------------------------------------------------------------------------------
  :Multiplication-Table
  '((:+A :+A :+A :+A (:+A :+D :+F :+MI :+OI) :+A :+A (:+A :+D :+F :+MI :+OI) :+A :+A (:+A :+D :+F :+MI :+OI)
	 (:+A :+D :+F :+MI :+OI) (:+A :+MI :+OI :+F :+D :+SI :+E :+S :+DI :+FI :+O :+M :+B))
    (:+A :+A :+A :+MI (:+D :+F :+OI) :+A :+MI (:+D :+F :+OI) :+A :+MI (:+D :+F :+OI) (:+E :+S :+SI)
	 (:+B :+DI :+FI :+M :+O))
    (:+A :+A (:+A :+MI :+OI) :+OI (:+D :+F :+OI) (:+A :+MI :+OI) :+OI (:+D :+F :+OI) (:+A :+DI :+MI :+OI :+SI)
	 (:+DI :+OI :+SI) (:+DI :+D :+E :+F :+FI :+O :+OI :+S :+SI) (:+DI :+FI :+O) (:+B :+DI :+FI :+M :+O))
    (:+A :+A (:+A :+MI :+OI) :+F :+D (:+A :+MI :+OI) :+F :+D (:+A :+DI :+MI :+OI :+SI) (:+E :+F :+FI)
	 (:+D :+O :+S) :+M  :+B)
    (:+A :+A (:+A :+D :+F :+MI :+OI) :+D :+D (:+A :+D :+F :+MI :+OI) :+D :+D
	 (:+A :+MI :+OI :+F :+D :+SI :+E :+S :+DI :+FI :+O :+M :+B) (:+B :+D :+M :+O :+S) (:+B :+D :+M :+O :+S)
	 :+B :+B)
    (:+A :+MI :+OI :+OI (:+D :+F :+OI) :+SI :+SI (:+E :+S :+SI) :+DI :+DI (:+DI :+FI :+O) (:+DI :+FI :+O)
	 (:+B :+DI :+FI :+M :+O))
    (:+A :+MI :+OI :+F :+D :+SI :+E :+S :+DI :+FI :+O :+M :+B)
    (:+A :+MI (:+D :+F :+OI) :+D :+D (:+E :+S :+SI) :+S :+S (:+B :+DI :+FI :+M :+O) (:+B :+M :+O) (:+B :+M :+O)
	 :+B :+B)
    ((:+A :+DI :+MI :+OI :+SI) (:+DI :+OI :+SI) (:+DI :+OI :+SI) (:+DI :+OI :+SI)
     (:+DI :+D :+E :+F :+FI :+O :+OI :+S :+SI) :+DI :+DI (:+DI :+FI :+O) :+DI :+DI (:+DI :+FI :+O)
     (:+DI :+FI :+O) (:+B :+DI :+FI :+M :+O))
    ((:+A :+DI :+MI :+OI :+SI) (:+DI :+OI :+SI) (:+DI :+OI :+SI) (:+E :+F :+FI) (:+D :+O :+S) :+DI :+FI
     :+O :+DI :+FI :+O :+M :+B)
    ((:+A :+DI :+MI :+OI :+SI) (:+DI :+OI :+SI) (:+DI :+D :+E :+F :+FI :+O :+OI :+S :+SI) (:+D :+O :+S)
     (:+D :+O :+S) (:+DI :+FI :+O) :+O :+O (:+B :+DI :+FI :+M :+O) (:+B :+M :+O) (:+B :+M :+O) :+B :+B)
    ((:+A :+DI :+MI :+OI :+SI) (:+E :+F :+FI) (:+D :+O :+S) (:+D :+O :+S) (:+D :+O :+S) :+M :+M :+M :+B :+B :+B
     :+B :+B)
    ((:+A :+MI :+OI :+F :+D :+SI :+E :+S :+DI :+FI :+O :+M :+B) (:+B :+D :+M :+O :+S) (:+B :+D :+M :+O :+S)
     (:+B :+D :+M :+O :+S) (:+B :+D :+M :+O :+S) :+B :+B :+B :+B :+B :+B :+B :+B)))

;;;---------------------------------------------------------------------------

;;; Right Branching Period & Point Algebra Definition (Coded Version)
;;; Created 8/16/92.  Same as in the file "basic-algebras.lisp", only
;;; it has the newer relation names.

(define-algebra
    'RIGHT-BRANCHING-PERIOD-&-POINT
    :Documentation "This is Al Reich's right branching time, period and point algebra."
    :Type :RELATION-SYSTEM
    :Inverse-Relations
    '(:=<B :=<BB :=<A :=<AB :=<DI :=<D  :=<E :=<FI :=<F  :=<IB :=<MI :=<M  :=<OI :=<OIB :=<O
      :=<OB  :=<SI :=<S  :=<SIB :=<TE :=<TFI :=<TF  :=<TSI :=<TS)
    :Relation-List
    '(:=<A :=<AB :=<B :=<BB :=<D  :=<DI :=<E :=<F  :=<FI :=<IB :=<M  :=<MI :=<O  :=<OB  :=<OI
      :=<OIB :=<S  :=<SI :=<SIB :=<TE :=<TF  :=<TFI :=<TS  :=<TSI)
    ;;-----------------------------------------------------------------------------------------
    :Multiplication-Table
    '((1 2 16777215 512 1099923 1 1 1 1 512 1099923 1 1099923 2 1 2 1099923 1 2 1 1 1 1099923 1)
      (1 2 512 16777215 2 515 2 2 514 512 512 1 514 1100435 3 1099923 2 3 1099923 0 0 512 0 1)
      (16473589 4568086 4 4 4264980 4 4 4264980 4 524 4 4264980 4 4 4264980 4264980 4 4 4 4 4264980 4 4 4)
      (8841257 520698 8 8 303112 8 8 303112 8 2111276 8 303112 8 8 303112 303112 8 8 8 8 303112 8 8 8)
      (1 2 4 524 16 16473589 16 16 4264980 512 4 1 4264980 4568086 1067153 32786 16 1067153 32786 0 0 4 0 1)
      (8538145 303106 2110764 8 520688 32 32 147488 32 520 12576 147488 12576 8192 147488 303104 12576 32 8192 32 147488 32 12576 32)
      (1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 0 0 2097152 0 8388608)
      (1 2 4 520 16 8538145 128 128 448 512 1024 1 69648 303106 18433 32770 16 18433 32770 0 0 2097152 0 1)
      (8538145 303106 4 8 69648 32 256 448 256 520 1024 147488 4096 8192 147488 303104 4096 32 8192 0 0 2097152 0 32)
      (515 1100435 512 512 514 512 512 514 512 16777215 512 514 512 512 514 514 512 512 512 512 514 512 512 512)
      (8538145 303106 4 4 69648 4 1024 69648 4 520 4 448 4 4 69648 69648 1024 1024 1024 0 0 4 0 2097152)
      (1 2 2110764 512 49296 1 2048 2048 2048 512 458816 1 49296 2 1 2 49296 1 2 0 0 8388608 0 1)
      (8538145 303106 4 12 69648 2102564 4096 69648 5124 520 4 147488 5124 13316 217584 372752 4096 4384 12288 0 0 4 0 32)
      (8538145 303106 8 2110764 303104 8232 8192 303104 8200 520 8 147488 8200 12584 450592 520688 8192 8224 12576 0 0 8 0 32)
      (1 2 2110764 520 49296 8538145 16384 16384 147488 512 12576 1 520688 303106 18433 32770 49296 18433 32770 0 0 32 0 1)
      (1 2 8 2111276 32768 8841257 32768 32768 303112 512 8 1 303112 520698 51201 49298 32768 51201 49298 0 0 8 0 1)
      (1 2 4 12 16 2102564 65536 16 5124 512 4 2048 5124 13316 16528 32784 65536 196672 327680 0 0 4 0 8388608)
      (1 2 2110764 8 49296 32 131072 16384 32 512 12576 2048 12576 8192 16384 32768 458816 131072 262144 0 0 32 0 8388608)
      (1 2 8 2110764 32768 8232 262144 32768 8200 512 8 2048 8200 12584 49152 49296 262144 393216 458816 0 0 8 0 8388608)
      (1 2 4 0 16 0 0 0 0 512 0 0 0 0 0 0 0 0 0 524288 1048576 0 4194304 0)
      (1 2 4 512 16 1 1048576 1048576 1048576 512 4194304 1 16 2 1 2 16 1 2 0 0 524288 0 1)
      (8538145 303106 4 0 69648 0 0 0 0 520 0 0 0 0 0 0 0 0 0 2097152 448 0 1024 0)
      (1 2 4 4 16 4 4194304 16 4 512 4 1048576 4 4 16 16 4194304 4194304 4194304 0 0 4 0 524288)
      (1 2 2110764 0 49296 0 0 0 0 512 0 0 0 0 0 0 0 0 0 8388608 2048 0 458816 0)))

;;;---------------------------------------------------------------------------

;(define-algebra
;  'TEST-NETWORK-BASED-PERIOD
;  :Documentation "Allen's Theory of Time constructed from *allen-pt-nets-WITHOUT-equals*"
;  :Type :network-based-algebra
;  :Network-Relation-List
;  '((:A  . #N((:= :< :> :>)			; After
;	      (:> := :> :>)
;	      (:< :< := :<)
;	      (:< :< :> :=)))
; 
;    (:MI . #N((:= :< :> :=)			; Met-By
;	      (:> := :> :>)
;	      (:< :< := :<)
;	      (:= :< :> :=)))
; 
;    (:OI . #N((:= :< :> :<)			; Overlapped-By
;	      (:> := :> :>)
;	      (:< :< := :<)
;	      (:> :< :> :=)))
; 
;    (:F  . #N((:= :< :> :<)			; Finishes
;	      (:> := :> :=)
;	      (:< :< := :<)
;	      (:> := :> :=)))
; 
;    (:D  . #N((:= :< :> :<)			; During
;	      (:> := :> :<)
;	      (:< :< := :<)
;	      (:> :> :> :=)))
; 
;    (:SI . #N((:= :< := :<)			; Started-By
;	      (:> := :> :>)
;	      (:= :< := :<)
;	      (:> :< :> :=)))
; 
;    (:E  . #N((:= :< := :<)			; Equals
;	      (:> := :> :=)
;	      (:= :< := :<)
;	      (:> := :> :=)))
; 
;    (:S  . #N((:= :< := :<)			; Starts
;	      (:> := :> :<)
;	      (:= :< := :<)
;	      (:> :> :> :=)))
; 
;    (:DI . #N((:= :< :< :<)			; Contains
;	      (:> := :> :>)
;	      (:> :< := :<)
;	      (:> :< :> :=)))
; 
;    (:FI . #N((:= :< :< :<)			; Finished-By
;	      (:> := :> :=)
;	      (:> :< := :<)
;	      (:> := :> :=)))
; 
;    (:O  . #N((:= :< :< :<)			; Overlaps
;	      (:> := :> :<)
;	      (:> :< := :<)
;	      (:> :> :> :=)))
; 
;    (:M  . #N((:= :< :< :<)			; Meets
;	      (:> := := :<)
;	      (:> := := :<)
;	      (:> :> :> :=)))
; 
;    (:B  . #N((:= :< :< :<)			; Before
;	      (:> := :< :<)
;	      (:> :> := :<)
;	      (:> :> :> :=)))))

;;;---------------------------------------------------------------------------
#|
;;; Allen's period algebra with points integrated into it.
;;; This is the raw output of the form:
;;;    (setf mult-tbl (create-mult-table *allen-pt-nets-WITH-equals*))

((:B :BI :D :DI :E :F :FI :M :MI :O :OI :PE :PF :PFI :PS :PSI :S :SI)
 (:B :B :? (:B :D :M :O :PS :S) :B :B (:B :D :M :O :PS :S) :B :B (:B :D :M :O :PS :S) :B (:B :D :M :O :PS :S) :B (:B :D :M :O :PS :S) :B :B :B :B :B)
 (:BI :? :BI (:BI :D :F :MI :OI :PF) :BI :BI :BI :BI (:BI :D :F :MI :OI :PF) :BI (:BI :D :F :MI :OI :PF) :BI :BI :BI :BI (:BI :D :F :MI :OI :PF) :BI (:BI :D :F :MI :OI :PF) :BI)
 (:D :B :BI :D :? :D :D (:B :D :M :O :PS :S) :B :BI (:B :D :M :O :PS :S) (:BI :D :F :MI :OI :PF) NIL NIL :B NIL :BI :D (:BI :D :F :MI :OI :PF))
 (:DI (:B :DI :FI :M :O :PFI) (:BI :DI :MI :OI :PSI :SI) (:D :DI :E :F :FI :O :OI :S :SI) :DI :DI (:DI :OI :SI) :DI (:DI :FI :O) (:DI :OI :SI) (:DI :FI :O) (:DI :OI :SI) :DI (:DI :OI :SI) :DI (:DI :FI :O) :DI (:DI :FI :O) :DI)
 (:E :B :BI :D :DI :E :F :FI :M :MI :O :OI NIL NIL :PFI NIL :PSI :S :SI)
 (:F :B :BI :D (:BI :DI :MI :OI :PSI :SI) :F :F (:E :F :FI) :M :BI (:D :O :S) (:BI :MI :OI) NIL NIL :PFI NIL :BI :D (:BI :MI :OI))
 (:FI :B (:BI :DI :MI :OI :PSI :SI) (:D :O :S) :DI :FI (:E :F :FI) :FI :M (:DI :OI :SI) :O (:DI :OI :SI) NIL NIL :PFI NIL :DI :O :DI)
 (:M :B (:BI :DI :MI :OI :PSI :SI) (:D :O :S) :B :M (:D :O :S) :B :B (:E :F :FI) :B (:D :O :S) NIL NIL :B NIL :PFI :M :M)
 (:MI (:B :DI :FI :M :O :PFI) :BI (:D :F :OI) :BI :MI :MI :MI (:E :S :SI) :BI (:D :F :OI) :BI NIL NIL :PSI NIL :BI (:D :F :OI) :BI)
 (:O :B (:BI :DI :MI :OI :PSI :SI) (:D :O :S) (:B :DI :FI :M :O :PFI) :O (:D :O :S) (:B :M :O) :B (:DI :OI :SI) (:B :M :O) (:D :DI :E :F :FI :O :OI :S :SI) NIL NIL :B NIL :DI :O (:DI :FI :O))
 (:OI (:B :DI :FI :M :O :PFI) :BI (:D :F :OI) (:BI :DI :MI :OI :PSI :SI) :OI :OI (:DI :OI :SI) (:DI :FI :O) :BI (:D :DI :E :F :FI :O :OI :S :SI) (:BI :MI :OI) NIL NIL :DI NIL :BI (:D :F :OI) (:BI :MI :OI))
 (:PE :B :BI :D NIL NIL NIL NIL NIL NIL NIL NIL :PE :PF NIL :PS NIL NIL NIL)
 (:PF :B :BI :D :BI :PF :PF :PF :PS :BI :D :BI NIL NIL :PE NIL :BI :D :BI)
 (:PFI :B (:BI :DI :MI :OI :PSI :SI) (:D :O :S) NIL NIL NIL NIL NIL NIL NIL NIL :PFI (:E :F :FI) NIL :M NIL NIL NIL)
 (:PS :B :BI :D :B :PS :D :B :B :PF :B :D NIL NIL :B NIL :PE :PS :PS)
 (:PSI (:B :DI :FI :M :O :PFI) :BI (:D :F :OI) NIL NIL NIL NIL NIL NIL NIL NIL :PSI :MI NIL (:E :S :SI) NIL NIL NIL)
 (:S :B :BI :D (:B :DI :FI :M :O :PFI) :S :D (:B :M :O) :B :MI (:B :M :O) (:D :F :OI) NIL NIL :B NIL :PSI :S (:E :S :SI))
 (:SI (:B :DI :FI :M :O :PFI) :BI (:D :F :OI) :DI :SI :OI :DI (:DI :FI :O) :MI (:DI :FI :O) :OI NIL NIL :DI NIL :PSI (:E :S :SI) :SI))
|#

;;;---------------------------------------------------------------------------

#|
(define-algebra 'POINT-&-PERIOD
    :Documentation "Allen's period algebra with points integrated into it."
    :Type :relation-system
    :Inverse-Relations
    '(:=BI :=B  :=DI :=D  :=E :=FI :=F  :=MI :=M  :=OI :=O  :=PE :=PFI :=PF  :=PSI :=PS  :=SI :=S )
    :Relation-List
    '(:=B  :=BI :=D  :=DI :=E :=F  :=FI :=M  :=MI :=O  :=OI :=PE :=PF  :=PFI :=PS  :=PSI :=S  :=SI)
    ;;-----------------------------------------------------------------------------------------
    :Multiplication-Table
    '((:=B (:=B :=BI :=D :=DI :=E :=F :=FI :=M :=MI :=O :=OI :=PE :=PF :=PFI :=PS :=PSI :=S :=SI) (:=B :=D :=M :=O :=PS :=S) :=B :=B (:=B :=D :=M :=O :=PS :=S)
       :=B :=B (:=B :=D :=M :=O :=PS :=S) :=B (:=B :=D :=M :=O :=PS :=S) :=B (:=B :=D :=M :=O :=PS :=S) :=B :=B :=B :=B :=B)
      ((:=B :=BI :=D :=DI :=E :=F :=FI :=M :=MI :=O :=OI :=PE :=PF :=PFI :=PS :=PSI :=S :=SI) :=BI (:=BI :=D :=F :=MI :=OI :=PF) :=BI :=BI :=BI :=BI
       (:=BI :=D :=F :=MI :=OI :=PF) :=BI (:=BI :=D :=F :=MI :=OI :=PF) :=BI :=BI :=BI :=BI (:=BI :=D :=F :=MI :=OI :=PF) :=BI (:=BI :=D :=F :=MI :=OI :=PF) :=BI)
      (:=B :=BI :=D (:=B :=BI :=D :=DI :=E :=F :=FI :=M :=MI :=O :=OI :=PE :=PF :=PFI :=PS :=PSI :=S :=SI) :=D :=D (:=B :=D :=M :=O :=PS :=S) :=B :=BI
       (:=B :=D :=M :=O :=PS :=S) (:=BI :=D :=F :=MI :=OI :=PF) NIL NIL :=B NIL :=BI :=D (:=BI :=D :=F :=MI :=OI :=PF))
      ((:=B :=DI :=FI :=M :=O :=PFI) (:=BI :=DI :=MI :=OI :=PSI :=SI) (:=D :=DI :=E :=F :=FI :=O :=OI :=S :=SI) :=DI :=DI (:=DI :=OI :=SI) :=DI (:=DI :=FI :=O)
       (:=DI :=OI :=SI) (:=DI :=FI :=O) (:=DI :=OI :=SI) :=DI (:=DI :=OI :=SI) :=DI (:=DI :=FI :=O) :=DI (:=DI :=FI :=O) :=DI)
      (:=B :=BI :=D :=DI :=E :=F :=FI :=M :=MI :=O :=OI NIL NIL :=PFI NIL :=PSI :=S :=SI)
      (:=B :=BI :=D (:=BI :=DI :=MI :=OI :=PSI :=SI) :=F :=F (:=E :=F :=FI) :=M :=BI (:=D :=O :=S) (:=BI :=MI :=OI) NIL NIL :=PFI NIL :=BI :=D (:=BI :=MI :=OI))
      (:=B (:=BI :=DI :=MI :=OI :=PSI :=SI) (:=D :=O :=S) :=DI :=FI (:=E :=F :=FI) :=FI :=M (:=DI :=OI :=SI) :=O (:=DI :=OI :=SI) NIL NIL :=PFI NIL :=DI :=O :=DI)
      (:=B (:=BI :=DI :=MI :=OI :=PSI :=SI) (:=D :=O :=S) :=B :=M (:=D :=O :=S) :=B :=B (:=E :=F :=FI) :=B (:=D :=O :=S) NIL NIL :=B NIL :=PFI :=M :=M)
      ((:=B :=DI :=FI :=M :=O :=PFI) :=BI (:=D :=F :=OI) :=BI :=MI :=MI :=MI (:=E :=S :=SI) :=BI (:=D :=F :=OI) :=BI NIL NIL :=PSI NIL :=BI (:=D :=F :=OI) :=BI)
      (:=B (:=BI :=DI :=MI :=OI :=PSI :=SI) (:=D :=O :=S) (:=B :=DI :=FI :=M :=O :=PFI) :=O (:=D :=O :=S) (:=B :=M :=O) :=B (:=DI :=OI :=SI) (:=B :=M :=O)
       (:=D :=DI :=E :=F :=FI :=O :=OI :=S :=SI) NIL NIL :=B NIL :=DI :=O (:=DI :=FI :=O))
      ((:=B :=DI :=FI :=M :=O :=PFI) :=BI (:=D :=F :=OI) (:=BI :=DI :=MI :=OI :=PSI :=SI) :=OI :=OI (:=DI :=OI :=SI) (:=DI :=FI :=O) :=BI
       (:=D :=DI :=E :=F :=FI :=O :=OI :=S :=SI) (:=BI :=MI :=OI) NIL NIL :=DI NIL :=BI (:=D :=F :=OI) (:=BI :=MI :=OI))
      (:=B :=BI :=D NIL NIL NIL NIL NIL NIL NIL NIL :=PE :=PF NIL :=PS NIL NIL NIL)
      (:=B :=BI :=D :=BI :=PF :=PF :=PF :=PS :=BI :=D :=BI NIL NIL :=PE NIL :=BI :=D :=BI)
      (:=B (:=BI :=DI :=MI :=OI :=PSI :=SI) (:=D :=O :=S) NIL NIL NIL NIL NIL NIL NIL NIL :=PFI (:=E :=F :=FI) NIL :=M NIL NIL NIL)
      (:=B :=BI :=D :=B :=PS :=D :=B :=B :=PF :=B :=D NIL NIL :=B NIL :=PE :=PS :=PS)
      ((:=B :=DI :=FI :=M :=O :=PFI) :=BI (:=D :=F :=OI) NIL NIL NIL NIL NIL NIL NIL NIL :=PSI :=MI NIL (:=E :=S :=SI) NIL NIL NIL)
      (:=B :=BI :=D (:=B :=DI :=FI :=M :=O :=PFI) :=S :=D (:=B :=M :=O) :=B :=MI (:=B :=M :=O) (:=D :=F :=OI) NIL NIL :=B NIL :=PSI :=S (:=E :=S :=SI))
      ((:=B :=DI :=FI :=M :=O :=PFI) :=BI (:=D :=F :=OI) :=DI :=SI :=OI :=DI (:=DI :=FI :=O) :=MI (:=DI :=FI :=O) :=OI NIL NIL :=DI NIL :=PSI (:=E :=S :=SI) :=SI)))
|#

;;; Left Branching Period & Point Algebra Definition (Symbolic Version)
;;; Created 12/27/92

#|
(define-algebra
  'LEFT-BRANCHING-PERIOD-&-POINT
  :Documentation "This is Al Reich's left branching time, period and point algebra."
  :Type :RELATION-SYSTEM
  :Inverse-Relations
  '(:>=BI :>=B :>=DI :>=D :>=E :>=FI :>=F :>=LBI :>=LB :>=LF :>=LOI :>=LO :>=L~ :>=MI :>=M :>=OI :>=O :>=PE :>=PFI :>=PF :>=PSI :>=PS :>=SI :>=S)
  :Relation-List
  '(:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=LB :>=LBI :>=LF :>=LO :>=LOI :>=L~ :>=M :>=MI :>=O :>=OI :>=PE :>=PF :>=PFI :>=PS :>=PSI :>=S :>=SI)
  ;;-----------------------------------------------------------------------------------------
  :Multiplication-Table
  '((:>=B (:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=LB :>=LBI :>=LF :>=LO :>=LOI :>=L~ :>=M :>=MI :>=O :>=OI :>=PE :>=PF :>=PFI :>=PS :>=PSI :>=S :>=SI) (:>=B :>=D :>=LB :>=LO :>=M :>=O :>=PS :>=S) :>=B :>=B (:>=B :>=D :>=LB :>=LO :>=M :>=O :>=PS :>=S) :>=B :>=LB :>=L~ :>=LB :>=LB :>=LB :>=L~ :>=B (:>=B :>=D :>=LB :>=LO :>=M :>=O :>=PS :>=S) :>=B (:>=B :>=D :>=LB :>=LO :>=M :>=O :>=PS :>=S) :>=B (:>=B :>=D :>=LB :>=LO :>=M :>=O :>=PS :>=S) :>=B :>=B :>=B :>=B :>=B)
    ((:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=M :>=MI :>=O :>=OI :>=PE :>=PF :>=PFI :>=PS :>=PSI :>=S :>=SI) :>=BI (:>=BI :>=D :>=F :>=MI :>=OI :>=PF) :>=BI :>=BI :>=BI :>=BI (:>=BI :>=D :>=F :>=LB :>=LF :>=LO :>=LOI :>=MI :>=OI :>=PF) :>=BI :>=BI (:>=BI :>=D :>=F :>=MI :>=OI :>=PF) :>=BI (:>=BI :>=LBI :>=L~) (:>=BI :>=D :>=F :>=MI :>=OI :>=PF) :>=BI (:>=BI :>=D :>=F :>=MI :>=OI :>=PF) :>=BI :>=BI :>=BI :>=BI (:>=BI :>=D :>=F :>=MI :>=OI :>=PF) :>=BI (:>=BI :>=D :>=F :>=MI :>=OI :>=PF) :>=BI)
    (:>=B :>=BI :>=D (:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=M :>=MI :>=O :>=OI :>=PE :>=PF :>=PFI :>=PS :>=PSI :>=S :>=SI) :>=D :>=D (:>=B :>=D :>=M :>=O :>=PS :>=S) :>=LB (:>=BI :>=LBI :>=L~) (:>=D :>=LB :>=LO) (:>=D :>=LB :>=LO) (:>=BI :>=D :>=F :>=LB :>=LF :>=LO :>=LOI :>=MI :>=OI :>=PF) :>=L~ :>=B :>=BI (:>=B :>=D :>=M :>=O :>=PS :>=S) (:>=BI :>=D :>=F :>=MI :>=OI :>=PF) NIL NIL :>=B NIL :>=BI :>=D (:>=BI :>=D :>=F :>=MI :>=OI :>=PF))
    ((:>=B :>=DI :>=FI :>=M :>=O :>=PFI) (:>=BI :>=DI :>=LBI :>=LOI :>=MI :>=OI :>=PSI :>=SI) (:>=D :>=DI :>=E :>=F :>=FI :>=LF :>=LO :>=LOI :>=O :>=OI :>=S :>=SI) :>=DI :>=DI (:>=DI :>=LOI :>=OI :>=SI) :>=DI (:>=LB :>=LF :>=LO :>=LOI) :>=LBI :>=LOI (:>=LF :>=LO :>=LOI) :>=LOI (:>=LBI :>=L~) (:>=DI :>=FI :>=O) (:>=DI :>=LOI :>=OI :>=SI) (:>=DI :>=FI :>=O) (:>=DI :>=LOI :>=OI :>=SI) :>=DI (:>=DI :>=LOI :>=OI :>=SI) :>=DI (:>=DI :>=FI :>=O) :>=DI (:>=DI :>=FI :>=O) :>=DI)
    (:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=LB :>=LBI :>=LF :>=LO :>=LOI :>=L~ :>=M :>=MI :>=O :>=OI NIL NIL :>=PFI NIL :>=PSI :>=S :>=SI)
    (:>=B :>=BI :>=D (:>=BI :>=DI :>=MI :>=OI :>=PSI :>=SI) :>=F :>=F (:>=E :>=F :>=FI) :>=LB (:>=BI :>=LBI) (:>=F :>=LF) (:>=D :>=LO) (:>=BI :>=LOI :>=MI :>=OI) :>=L~ :>=M :>=BI (:>=D :>=O :>=S) (:>=BI :>=MI :>=OI) NIL NIL :>=PFI NIL :>=BI :>=D (:>=BI :>=MI :>=OI))
    (:>=B (:>=BI :>=DI :>=LBI :>=LOI :>=MI :>=OI :>=PSI :>=SI) (:>=D :>=LO :>=O :>=S) :>=DI :>=FI (:>=E :>=F :>=FI :>=LF) :>=FI :>=LB :>=LBI :>=LF :>=LO :>=LOI :>=L~ :>=M (:>=DI :>=LOI :>=OI :>=SI) :>=O (:>=DI :>=LOI :>=OI :>=SI) NIL NIL :>=PFI NIL :>=DI :>=O :>=DI)
    (:>=B :>=L~ :>=LB (:>=B :>=LB :>=L~) :>=LB :>=LB (:>=B :>=LB) :>=LB (:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=LB :>=LBI :>=LF :>=LO :>=LOI :>=L~ :>=M :>=MI :>=O :>=OI :>=PE :>=PF :>=PFI :>=PS :>=PSI :>=S :>=SI) (:>=B :>=D :>=LB :>=LO :>=M :>=O :>=PS :>=S) (:>=B :>=D :>=LB :>=LO :>=M :>=O :>=PS :>=S) (:>=B :>=D :>=LB :>=LO :>=L~ :>=M :>=O :>=PS :>=S) :>=L~ :>=B :>=L~ (:>=B :>=LB) (:>=LB :>=L~) NIL NIL :>=B NIL :>=L~ :>=LB (:>=LB :>=L~))
    ((:>=B :>=DI :>=FI :>=LBI :>=LF :>=LO :>=LOI :>=M :>=O :>=PFI) :>=LBI (:>=LBI :>=LF :>=LO :>=LOI) :>=LBI :>=LBI :>=LBI :>=LBI (:>=D :>=DI :>=E :>=F :>=FI :>=LB :>=LBI :>=LF :>=LO :>=LOI :>=O :>=OI :>=S :>=SI) :>=LBI :>=LBI (:>=LBI :>=LF :>=LO :>=LOI) :>=LBI (:>=BI :>=DI :>=LBI :>=LOI :>=L~ :>=MI :>=OI :>=PSI :>=SI) (:>=LBI :>=LF :>=LO :>=LOI) :>=LBI (:>=LBI :>=LF :>=LO :>=LOI) :>=LBI :>=LBI :>=LBI :>=LBI (:>=LBI :>=LF :>=LO :>=LOI) :>=LBI (:>=LBI :>=LF :>=LO :>=LOI) :>=LBI)
    (:>=B :>=LBI :>=LO (:>=DI :>=LBI :>=LOI) :>=LF :>=LF (:>=FI :>=LF) :>=LB (:>=BI :>=DI :>=LBI :>=LOI :>=MI :>=OI :>=PSI :>=SI) (:>=E :>=F :>=FI :>=LF) (:>=D :>=LO :>=O :>=S) (:>=DI :>=LBI :>=LOI :>=OI :>=SI) :>=L~ :>=M :>=LBI (:>=LO :>=O) (:>=LBI :>=LOI) NIL NIL :>=PFI NIL :>=LBI :>=LO (:>=LBI :>=LOI))
    (:>=B :>=LBI :>=LO (:>=B :>=DI :>=FI :>=LBI :>=LF :>=LO :>=LOI :>=M :>=O :>=PFI) :>=LO :>=LO (:>=B :>=LO :>=M :>=O) :>=LB (:>=BI :>=DI :>=LBI :>=LOI :>=L~ :>=MI :>=OI :>=PSI :>=SI) (:>=D :>=LB :>=LO :>=O :>=S) (:>=D :>=LB :>=LO :>=O :>=S) (:>=D :>=DI :>=E :>=F :>=FI :>=LB :>=LBI :>=LF :>=LO :>=LOI :>=O :>=OI :>=S :>=SI) :>=L~ :>=B :>=LBI (:>=B :>=LO :>=M :>=O) (:>=LBI :>=LF :>=LO :>=LOI) NIL NIL :>=B NIL :>=LBI :>=LO (:>=LBI :>=LF :>=LO :>=LOI))
    ((:>=B :>=DI :>=FI :>=M :>=O :>=PFI) :>=LBI (:>=LF :>=LO :>=LOI) (:>=DI :>=LBI :>=LOI) :>=LOI :>=LOI (:>=DI :>=LOI) (:>=LB :>=LF :>=LO :>=LOI) (:>=BI :>=DI :>=LBI :>=LOI :>=MI :>=OI :>=PSI :>=SI) (:>=DI :>=LOI :>=OI :>=SI) (:>=D :>=DI :>=E :>=F :>=FI :>=LF :>=LO :>=LOI :>=O :>=OI :>=S :>=SI) (:>=DI :>=LBI :>=LOI :>=OI :>=SI) (:>=LBI :>=L~) (:>=DI :>=FI :>=O) :>=LBI (:>=DI :>=FI :>=LF :>=LO :>=LOI :>=O) (:>=LBI :>=LOI) NIL NIL :>=DI NIL :>=LBI (:>=LF :>=LO :>=LOI) (:>=LBI :>=LOI))
    ((:>=B :>=LB :>=L~) :>=L~ (:>=LB :>=L~) :>=L~ :>=L~ :>=L~ :>=L~ (:>=B :>=D :>=LB :>=LO :>=L~ :>=M :>=O :>=PS :>=S) :>=L~ :>=L~ (:>=LB :>=L~) :>=L~ (:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=LB :>=LBI :>=LF :>=LO :>=LOI :>=L~ :>=M :>=MI :>=O :>=OI :>=PE :>=PF :>=PFI :>=PS :>=PSI :>=S :>=SI) (:>=LB :>=L~) :>=L~ (:>=LB :>=L~) :>=L~ :>=L~ :>=L~ :>=L~ (:>=LB :>=L~) :>=L~ (:>=LB :>=L~) :>=L~)
    (:>=B (:>=BI :>=DI :>=LBI :>=LOI :>=MI :>=OI :>=PSI :>=SI) (:>=D :>=LO :>=O :>=S) :>=B :>=M (:>=D :>=LO :>=O :>=S) :>=B :>=LB :>=L~ :>=LB :>=LB :>=LB :>=L~ :>=B (:>=E :>=F :>=FI :>=LF) :>=B (:>=D :>=LO :>=O :>=S) NIL NIL :>=B NIL :>=PFI :>=M :>=M)
    ((:>=B :>=DI :>=FI :>=M :>=O :>=PFI) :>=BI (:>=D :>=F :>=OI) :>=BI :>=MI :>=MI :>=MI (:>=LB :>=LF :>=LO :>=LOI) :>=BI :>=MI (:>=D :>=F :>=OI) :>=BI (:>=LBI :>=L~) (:>=E :>=S :>=SI) :>=BI (:>=D :>=F :>=OI) :>=BI NIL NIL :>=PSI NIL :>=BI (:>=D :>=F :>=OI) :>=BI)
    (:>=B (:>=BI :>=DI :>=LBI :>=LOI :>=MI :>=OI :>=PSI :>=SI) (:>=D :>=LO :>=O :>=S) (:>=B :>=DI :>=FI :>=M :>=O :>=PFI) :>=O (:>=D :>=LO :>=O :>=S) (:>=B :>=M :>=O) :>=LB (:>=LBI :>=L~) (:>=LB :>=LO) (:>=LB :>=LO) (:>=LB :>=LF :>=LO :>=LOI) :>=L~ :>=B (:>=DI :>=LOI :>=OI :>=SI) (:>=B :>=M :>=O) (:>=D :>=DI :>=E :>=F :>=FI :>=LF :>=LO :>=LOI :>=O :>=OI :>=S :>=SI) NIL NIL :>=B NIL :>=DI :>=O (:>=DI :>=FI :>=O))
    ((:>=B :>=DI :>=FI :>=M :>=O :>=PFI) :>=BI (:>=D :>=F :>=OI) (:>=BI :>=DI :>=MI :>=OI :>=PSI :>=SI) :>=OI :>=OI (:>=DI :>=OI :>=SI) (:>=LB :>=LF :>=LO :>=LOI) (:>=BI :>=LBI) (:>=LOI :>=OI) (:>=D :>=F :>=LF :>=LO :>=LOI :>=OI) (:>=BI :>=LOI :>=MI :>=OI) (:>=LBI :>=L~) (:>=DI :>=FI :>=O) :>=BI (:>=D :>=DI :>=E :>=F :>=FI :>=O :>=OI :>=S :>=SI) (:>=BI :>=MI :>=OI) NIL NIL :>=DI NIL :>=BI (:>=D :>=F :>=OI) (:>=BI :>=MI :>=OI))
    (:>=B :>=BI :>=D NIL NIL NIL NIL :>=LB NIL NIL NIL NIL :>=L~ NIL NIL NIL NIL :>=PE :>=PF NIL :>=PS NIL NIL NIL)
    (:>=B :>=BI :>=D :>=BI :>=PF :>=PF :>=PF :>=LB :>=BI :>=PF :>=D :>=BI :>=L~ :>=PS :>=BI :>=D :>=BI NIL NIL :>=PE NIL :>=BI :>=D :>=BI)
    (:>=B (:>=BI :>=DI :>=LBI :>=LOI :>=MI :>=OI :>=PSI :>=SI) (:>=D :>=LO :>=O :>=S) NIL NIL NIL NIL :>=LB NIL NIL NIL NIL :>=L~ NIL NIL NIL NIL :>=PFI (:>=E :>=F :>=FI :>=LF) NIL :>=M NIL NIL NIL)
    (:>=B :>=BI :>=D :>=B :>=PS :>=D :>=B :>=LB :>=L~ :>=LB :>=LB :>=LB :>=L~ :>=B :>=PF :>=B :>=D NIL NIL :>=B NIL :>=PE :>=PS :>=PS)
    ((:>=B :>=DI :>=FI :>=M :>=O :>=PFI) :>=BI (:>=D :>=F :>=OI) NIL NIL NIL NIL (:>=LB :>=LF :>=LO :>=LOI) NIL NIL NIL NIL (:>=LBI :>=L~) NIL NIL NIL NIL :>=PSI :>=MI NIL (:>=E :>=S :>=SI) NIL NIL NIL)
    (:>=B :>=BI :>=D (:>=B :>=DI :>=FI :>=M :>=O :>=PFI) :>=S :>=D (:>=B :>=M :>=O) :>=LB (:>=LBI :>=L~) (:>=LB :>=LO) (:>=LB :>=LO) (:>=LB :>=LF :>=LO :>=LOI) :>=L~ :>=B :>=MI (:>=B :>=M :>=O) (:>=D :>=F :>=OI) NIL NIL :>=B NIL :>=PSI :>=S (:>=E :>=S :>=SI))
    ((:>=B :>=DI :>=FI :>=M :>=O :>=PFI) :>=BI (:>=D :>=F :>=OI) :>=DI :>=SI :>=OI :>=DI (:>=LB :>=LF :>=LO :>=LOI) :>=LBI :>=LOI (:>=LF :>=LO :>=LOI) :>=LOI (:>=LBI :>=L~) (:>=DI :>=FI :>=O) :>=MI (:>=DI :>=FI :>=O) :>=OI NIL NIL :>=DI NIL :>=PSI (:>=E :>=S :>=SI) :>=SI)
    ))
|#

;;;---------------------------------------------------------------------------

(define-algebra
  'RIGHT-BRANCHING-INTERVAL-NO-POINTS
  :Documentation "Al Reich's right branching time, interval constraint algebra (points not included)."
  :Type :relation-system
  :Inverse-Relations
  '(:<A :<AB :<B :<BB :<C :<D :<E :<F :<FI :<IB :<M :<MI :<O :<OB :<OI :<OIB :<S :<SI :<SIB)
  :Relation-List
  '(:<A :<AB :<B :<BB :<C :<D :<E :<F :<FI :<IB :<M :<MI :<O :<OB :<OI :<OIB :<S :<SI :<SIB)
  ;;-----------------------------------------------------------------------------------------
  :Multiplication-Table
  '((1 2 524287 512 1 51363 1 1 1 512 51363 1 51363 2 1 2 51363 1 2)
    (1 2 512 524287 515 2 2 2 514 512 512 1 514 51875 3 51363 2 3 51363)
    (220661 373798 4 4 4 70692 4 70692 4 524 4 70692 4 4 70692 70692 4 4 4)
    (452633 520698 8 8 8 303112 8 303112 8 14108 8 303112 8 8 303112 303112 8 8 8)
    (149521 303106 13596 8 16 520688 16 147472 16 520 12560 147472 12560 8192 147472 303104 12560 16 8192)
    (1 2 4 524 220661 32 32 32 70692 512 4 1 70692 373798 18593 32802 32 18593 32802)
    (1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144)
    (1 2 4 520 149521 32 128 128 448 512 1024 1 69664 303106 18433 32770 32 18433 32770)
    (149521 303106 4 8 16 69664 256 448 256 520 1024 147472 4096 8192 147472 303104 4096 16 8192)
    (515 51875 512 512 512 514 512 514 512 524287 512 514 512 512 514 514 512 512 512)
    (149521 303106 4 4 4 69664 1024 69664 4 520 4 448 4 4 69664 69664 1024 1024 1024)
    (1 2 13596 512 1 49312 2048 2048 2048 512 458816 1 49312 2 1 2 49312 1 2)
    (149521 303106 4 12 5396 69664 4096 69664 5124 520 4 147472 5124 13316 217584 372768 4096 4368 12288)
    (149521 303106 8 13596 8216 303104 8192 303104 8200 520 8 147472 8200 12568 450576 520688 8192 8208 12560)
    (1 2 13596 520 149521 49312 16384 16384 147472 512 12560 1 520688 303106 18433 32770 49312 18433 32770)
    (1 2 8 14108 452633 32768 32768 32768 303112 512 8 1 303112 520698 51201 49314 32768 51201 49314)
    (1 2 4 12 5396 32 65536 32 5124 512 4 2048 5124 13316 16544 32800 65536 196672 327680)
    (1 2 13596 8 16 49312 131072 16384 16 512 12560 2048 12560 8192 16384 32768 458816 131072 262144)
    (1 2 8 13596 8216 32768 262144 32768 8200 512 8 2048 8200 12568 49152 49312 262144 393216 458816)))

;;;---------------------------------------------------------------------------

