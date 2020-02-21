(in-package "CONSTRAINT-MAINTENANCE")

;;; LEGEND:
;;;  i# -- Interval
;;;  p# -- Point
;;;  o  -- Branch point
;;; [.] -- COA
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


;;; The symbol "?" denotes all possible relations (i.e., there is no
;;; specific known relation or relations --- all are possible).
;;;
(setf ? (get-algebra-rels (find-algebra 'RIGHT-BRANCHING-PERIOD-&-POINT)))
;;;
;;; ==> (:=<BI :=<RBI :=<B :=<RB :=<D :=<DI :=<E :=<F :=<FI ...

;;; Setup the constraint network:
;;;
;;;             i1     i2     i3     i4     i5     p1      p2
;;;           ------------------------------------------------
(setf NET1 #N((:=<e   :=<m   :=<m   #.?    #.?    :=<pfi  #.?     ) ; i1
	      (:=<mi  :=<e   :=<rs  #.?    #.?    :=<psi  #.?     ) ; i2
	      (:=<mi  :=<rs  :=<e   :=<m   :=<m   :=<psi  :=<pfi  ) ; i3
	      (#.?    #.?    :=<mi  :=<e   :=<rs  #.?     :=<psi  ) ; i4
	      (#.?    #.?    :=<mi  :=<rs  :=<e   #.?     :=<psi  ) ; i5
	      (:=<pf  :=<ps  :=<ps  #.?    #.?    :=<pe   #.?     ) ; p1
	      (#.?    #.?    :=<pf  :=<ps  :=<ps  #.?     :=<pe ))) ; p2

(describe net1)
;;;
;;; ==> #N((:=<E :=<M :=<M (:=<B :=<BI :=<D :=<DI :=<E :=<F :=<FI ...
;;;     It has 2 slots, with the following values:
;;;      ALGEBRA:                     #<RIGHT-BRANCHING-PERIOD-&-POINT-ALGEBRA>
;;;      ARR:                         #<Array T (7 7) 10C9A53>

(list-array (get-network-arr net1))
;;;
;;; ==> ((      64     1024    1024 16777215 16777215 2097152  16777215)
;;;      (    2048       64  262144 16777215 16777215 8388608  16777215)
;;;      (    2048   262144      64     1024     1024 8388608   2097152)
;;;      (16777215 16777215    2048       64   262144 16777215  8388608)
;;;      (16777215 16777215    2048   262144       64 16777215  8388608)
;;;      ( 1048576  4194304 4194304 16777215 16777215   524288 16777215)
;;;      (16777215 16777215 1048576  4194304  4194304 16777215   524288))

(describe (get-network-algebra net1))
;;;
;;; ==> #<RIGHT-BRANCHING-PERIOD-&-POINT-ALGEBRA> is a structure of type ALGEBRA.
;;;     It has 17 slots, with the following values:
;;;      NAME:                        RIGHT-BRANCHING-PERIOD-&-POINT
;;;      DOCUMENTATION:               "This is Al Reich's right branching time,
;;;                                    period and point algebra."
;;;      TYPE:                        :RELATION-SYSTEM
;;;      ELEMENT-TYPE:                SYMBOL
;;;      RELS:                        (:=<BI :=<RBI :=<B :=<RB :=<D :=<DI :=<E
;;;                                    :=<F :=<FI :=<R~ ...)
;;;      REL-CODING:                  ((:=<BI . 1) (:=<RBI . 2) (:=<B . 4)
;;;                                    (:=<RB . 8) (:=<D . 16) (:=<DI . 32)
;;;                                    (:=<E . 64) (:=<F . 128) (:=<FI . 256)
;;;                                    (:=<R~ . 512) ...)
;;;      INV-RELS:                    (:=<B :=<RB :=<BI :=<RBI :=<DI :=<D :=<E
;;;                                    :=<FI :=<F :=<R~ ...)
;;;      EQUAL-P:                     #<Compiled-Function EQUAL A1FAFF>
;;;      LESS-THAN-P:                 #<Compiled-Function (:INTERNAL ...>
;;;      NUM-RELS:                    24
;;;      ZERO:                        16777215
;;;      ENCODER:                     #<Compiled-Function (:INTERNAL ...>
;;;      DECODER:                     #<Compiled-Function (:INTERNAL ...>
;;;      MULT-TABLE:                  #<Simple-Array T (24 24) A1FDFB>
;;;      MULTIPLIER:                  #<Compiled-Function (:INTERNAL ...>
;;;      ADDER:                       #<Compiled-Function (:INTERNAL ...>
;;;      INVERTER:                    #<Compiled-Function (:INTERNAL ...>


;;; Propagate the constraints in the network:
;;;
(setf net1-prop (propagate net1))
;;;
;;; ==> #N((:=<E  :=<M   :=<M  :=<B  :=<B  :=<PFI :=<B  )
;;;        (:=<MI :=<E   :=<RS :=<RB :=<RB :=<PSI :=<RB )
;;;        (:=<MI :=<RS  :=<E  :=<M  :=<M  :=<PSI :=<PFI)
;;;        (:=<BI :=<RBI :=<MI :=<E  :=<RS :=<BI  :=<PSI)
;;;        (:=<BI :=<RBI :=<MI :=<RS :=<E  :=<BI  :=<PSI)
;;;        (:=<PF :=<PS  :=<PS :=<B  :=<B  :=<PE  :=<B  )
;;;        (:=<BI :=<RBI :=<PF :=<PS :=<PS :=<BI  :=<PE ))

;;;**********************************************************************

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

(setf RBNET1 (create-constraint-network
              'right-branching-interval-&-point
              "i1"
              :obj-eq-test #'string=
              :object-type :interval))

(register-constrained-objects '("i2" "i3" "i4" "i5")
                              :interval rbnet1)

(register-constrained-objects '("p1" "p2")
                              :point rbnet1)

(defvar CONSTRAINTS

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
	'(:=<PFI "i3" "p2")

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

(assert-constraints constraints rbnet1 :propagate-p nil)

(time (propagate-constraints rbnet1))   ; HP Common Lisp (Lucid)
;;; Elapsed Real Time = 2.98 seconds
;;; Total Run Time    = 2.98 seconds
;;; User Run Time     = 2.98 seconds
;;; System Run Time   = 0.00 seconds
;;; Dynamic Bytes Consed   =         48
;;; Ephemeral Bytes Consed =    631,032
;;; There was 1 ephemeral GC
;;; #<Constraint-Network #X10ABDC3>

;;; UNDER MCL 2.0 WE GET:
;;; (time (propagate-constraints rbnet1))
;;; ;Compiler warnings :
;;; ;   Undeclared free variable RBNET1, in an anonymous lambda form.
;;; (PROPAGATE-CONSTRAINTS RBNET1) took 27367 milliseconds (27.367 seconds) to run.
;;; Of that, 693 milliseconds (0.693 seconds) were spent in The Cooperative Multitasking Experience.
;;;  1800 bytes of memory allocated.
;;; #<CONSTRAINT-NETWORK #x570C51>

(describe rbnet1)
;;; #<Constraint-Network #X10FEE93>
;;;     is an instance of the class CONSTRAINT-NETWORK:
;;;  The following slots have allocation :INSTANCE:
;;;  CONSTRAINT-MATRIX      #N((:=<E  :=<M   :=<M  :=<B  :=<B  :=<PFI :=<B  )
;;; 			       (:=<MI :=<E   :=<RS :=<RB :=<RB :=<PSI :=<RB )
;;; 			       (:=<MI :=<RS  :=<E  :=<M  :=<M  :=<PSI :=<PFI)
;;; 			       (:=<BI :=<RBI :=<MI :=<E  :=<RS :=<BI  :=<PSI)
;;; 			       (:=<BI :=<RBI :=<MI :=<RS :=<E  :=<BI  :=<PSI)
;;; 			       (:=<PF :=<PS  :=<PS :=<B  :=<B  :=<PE  :=<B  )
;;; 			       (:=<BI :=<RBI :=<PF :=<PS :=<PS :=<BI  :=<PE ))
;;;  CONSTRAINED-OBJECTS    ("i1" "i2" "i3" "i4" "i5" "p1" "p2")
;;;  ALG-NAME               RIGHT-BRANCHING-INTERVAL-&-POINT
;;;  OBJ-EQ-TEST            #<Compiled-Function EQUAL 5F8AFF>
;;;  OBJ-ID-KEY             #<Compiled-Function IDENTITY 5F435F>

;;;------------------------------------------------------------------------
#|

09/01/93.....See pp.31-33 in "Readings in Planning", by Allen, et al.

       BLOCKS: A, B, C
    INTERVALS: I, Ca, Cb, Cc, G, Oab, Obc
ABBREVIATIONS: In = (During Finishes Starts) = (D F S)

Clear(A,Ca)   In(I,Ca)       On(A,B,Oab)   In(G,Oab)
Clear(B,Cb)   In(I,Cb)       On(B,C,Obc)   In(G,Obc)
Clear(C,Cc)   In(I,Cc)

GOAL: Stack(A,B,E1,I1) s.t. In(G,eff2(E1))
      Stack(B,C,E2,I2) s.t. In(G,eff2(E2))

---------------

Stacking Axiom 0: (Temporal Structure)
     Stack(a,b,e,i) ==>
          Overlaps(pre1(e),i) &
          Finishes(con1(e),i) &
          Meets(pre1(e),con1(e)) &
          Meets(i,eff1(e)) &
          SameEnd(i,pre2(e)) &
          Meets(i,eff2(e)).

Stacking Axiom 1:
     Stack(a,b,e,i) ==>
          Clear(a,pre1(e)) &
          Holding(a,con1(e)) &
          Clear(a,eff1(e)) &
          Clear(b,pre2(e)) &
          On(a,b,eff2(e)).

---------------

Stack(A,B,E1,I1) ==>
     Overlaps(pre1(E1),I1) &
     Finishes(con1(E1),I1) &
     Meets(pre1(E1),con1(E1)) &
     Meets(I1,eff1(E1)) &
     SameEnd(I1,pre2(E1)) &
     Meets(I1,eff2(E1)).

Stack(A,B,E1,I1) ==>
     Clear(A,pre1(E1)) &
     Holding(A,con1(E1)) &
     Clear(A,eff1(E1)) &
     Clear(B,pre2(E1)) &
     On(A,B,eff2(E1)).

---------------

Stack(B,C,E2,I2) ==>
     Overlaps(pre1(E2),I2) &
     Finishes(con1(E2),I2) &
     Meets(pre1(E2),con1(E2)) &
     Meets(I2,eff1(E2)) &
     SameEnd(I2,pre2(E2)) &
     Meets(I2,eff2(E2)).

Stack(B,C,E2,I2) ==>
     Clear(B,pre1(E2)) &
     Holding(B,con1(E2)) &
     Clear(B,eff1(E2)) &
     Clear(C,pre2(E2)) &
     On(B,C,eff2(E2)).

===============

(setf net1 (create-constraint-network 'linear-interval "I1"
                                      :obj-eq-test #'string=))

(setf objects '(
		;;; "I1"   ; Already registered
		"con1(E1)"
		"eff1(E1)"
		"eff2(E1)"
		"pre1(E1)"
		"pre2(E1)"
		"I2"
		"con1(E2)"
		"eff1(E2)"
		"eff2(E2)"
		"pre1(E2)"
		"pre2(E2)"))

(register-constrained-objects objects :interval net1)

(setf STACK-A-B
      (list
       '(:O        "pre1(E1)"     "I1"    )
       '(:F        "con1(E1)"     "I1"    )
       '(:M        "pre1(E1)"   "con1(E1)")
       '(:M          "I1"       "eff1(E1)")
       '((:E :F :FI) "I1"       "pre2(E1)")
       '(:M          "I1"       "eff2(E1)")))

(setf STACK-B-C
      (list
       '(:O        "pre1(E2)"     "I2"    )
       '(:F        "con1(E2)"     "I2"    )
       '(:M        "pre1(E2)"   "con1(E2)")
       '(:M          "I2"       "eff1(E2)")
       '((:E :F :FI) "I2"       "pre2(E2)")
       '(:M          "I2"       "eff2(E2)")))

(assert-constraints (append stack-a-b stack-b-c) net1 :propagate-p nil)

(time (propagate-constraints net1))     ; Under HP Common Lisp (Lucid)
;;; Elapsed Real Time = 17.53 seconds
;;; Total Run Time    = 17.46 seconds
;;; User Run Time     = 17.46 seconds
;;; System Run Time   = 0.00 seconds
;;; Dynamic Bytes Consed   =        432
;;; Ephemeral Bytes Consed =  4,823,752
;;; There were 9 ephemeral GCs
;;; #<Constraint-Network #X10BA283>

;;; UNDER MCL 2.0
;;; (time (propagate-constraints net1))
;;; ;Compiler warnings :
;;; ;   Undeclared free variable NET1, in an anonymous lambda form.
;;; (PROPAGATE-CONSTRAINTS NET1) took 109606 milliseconds (109.606 seconds) to run.
;;; Of that, 2654 milliseconds (2.654 seconds) were spent in The Cooperative Multitasking Experience.
;;;  3312 bytes of memory allocated.
;;; #<CONSTRAINT-NETWORK #x516659>


;;; ABBREVIATIONS (used below)
;;; :ss  = SameStart      = (:E :S :SI)
;;; :sf  = SameFinish     = (:E :F :FI)
;;; :fb  = FinishesBefore = (:B :D :M :O :S)
;;; :fbi = FinishesAfter  = (:BI :DI :MI :OI :SI)

(describe net1)
;;; #<Constraint-Network #X10BA283>
;;;     is an instance of the class CONSTRAINT-NETWORK:
;;;  The following slots have allocation :INSTANCE:
;;;  CONSTRAINT-MATRIX
;;;    I1 con1(E1) eff1(E1) eff2(E1) pre1(E1) pre2(E1) |  I2 con1(E2) eff1(E2) eff2(E2) pre1(E2) pre2(E2)
;;;    ------------------------------------------------+------------------------------------------------
;;;#N((:E   :FI      :M        :M      :OI      :sf    |  :?   :?       :?       :?       :?       :? )
;;;   (:F   :E       :M        :M      :MI      :sf    |  :?   :?       :?       :?       :?       :? )
;;;   (:MI  :MI      :E        :ss     :BI      :MI    |  :?   :?       :?       :?       :?       :? )
;;;   (:MI  :MI      :ss       :E      :BI      :MI    |  :?   :?       :?       :?       :?       :? )
;;;   (:O   :M       :B        :B      :E       :fb    |  :?   :?       :?       :?       :?       :? )
;;;   (:sf  :sf      :M        :M      :fbi     :E     |  :?   :?       :?       :?       :?       :? )
;;;    ------------------------------------------------+------------------------------------------------
;;;   (:?   :?       :?        :?      :?       :?     |  :E   :FI      :M       :M       :OI      :sf)
;;;   (:?   :?       :?        :?      :?       :?     |  :F   :E       :M       :M       :MI      :sf)
;;;   (:?   :?       :?        :?      :?       :?     |  :MI  :MI      :E       :ss      :BI      :MI)
;;;   (:?   :?       :?        :?      :?       :?     |  :MI  :MI      :ss      :E       :BI      :MI)
;;;   (:?   :?       :?        :?      :?       :?     |  :O   :M       :B       :B       :E       :fb)
;;;   (:?   :?       :?        :?      :?       :?     |  :sf  :sf      :M       :M       :fbi     :E )
;;;   )
;;;  CONSTRAINED-OBJECTS    ("I1" "con1(E1)" "eff1(E1)" "eff2(E1)" "pre1(E1)" "pre2(E1)"
;;; 			 "I2" "con1(E2)" "eff1(E2)" "eff2(E2)" "pre1(E2)" "pre2(E2)")
;;;  ALG-NAME               LINEAR-INTERVAL
;;;  OBJ-EQ-TEST            #<Compiled-Function STRING= 5FE41F>
;;;  OBJ-ID-KEY             #<Compiled-Function IDENTITY 5F435F>

(setf domain-constraints-1
      (list
       ;; Clear(B) and On(A,B) cannot overlap
       '((:b :bi :m :mi) "eff2(E1)" "pre1(E2)")
       ;; Clear(C) and On(B,C) cannot overlap
       '((:b :bi :m :mi) "eff2(E2)" "pre2(E2)")
       ;; On(A,B) and Holding(B) cannot overlap
       '((:b :bi :m :mi) "eff2(E1)" "con1(E2)")
       ;; Clear(B) and Holding(B) cannot overlap
       '((:b :bi :m :mi) "pre1(E2)" "con1(E2)")
       ;; Holding(A) and Holding(B) cannot overlap
       '((:b :bi :m :mi) "con1(E1)" "con1(E2)")
       ))

(setf domain-constraints-2
      (list
       ;; Holding(A) and Clear(A) cannot overlap
       '((:b :bi :m :mi) "con1(E1)" "pre1(E1)")
       ;; Holding(A) and On(A,B) cannot overlap
       '((:b :bi :m :mi) "con1(E1)" "eff2(E1)")
       ;; Holding(B) and On(B,C) cannot overlap
       '((:b :bi :m :mi) "con1(E2)" "eff2(E2)")))

;;; UNDER MCL 2.0:
(assert-constraints domain-constraints-1 net1 :propagate-p nil)
;;; #<CONSTRAINT-NETWORK #x516659>
(time (propagate-constraints net1))
;;; ;Compiler warnings :
;;; ;   Undeclared free variable NET1, in an anonymous lambda form.
;;; (PROPAGATE-CONSTRAINTS NET1) took 172309 milliseconds (172.309 seconds) to run.
;;; Of that, 4128 milliseconds (4.128 seconds) were spent in The Cooperative
;;; Multitasking Experience.
;;;  3176 bytes of memory allocated.
;;; #<CONSTRAINT-NETWORK #x516659>

;;; UNDER HP COMMON LISP (LUCID):
(time (propagate-constraints net1))
;;; Elapsed Real Time = 31.94 seconds
;;; Total Run Time    = 31.90 seconds
;;; User Run Time     = 31.88 seconds
;;; System Run Time   = 0.02 seconds
;;; Dynamic Bytes Consed   =        624
;;; Ephemeral Bytes Consed =  6,861,392
;;; There were 13 ephemeral GCs
;;; #<Constraint-Network #X10F1B2B>

#|

;;; FROM MCL:

#N((:E :FI :M :M :OI (:E :F :FI) (:B :BI :D :DI :MI :O :OI :S :SI)
                       (:B :BI :DI :MI :OI :SI) (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI)
                       (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI) (:B :BI :DI :MI :OI :SI)
                       (:B :BI :D :DI :M :MI :O :OI :S :SI))
                      (:F :E :M :M :MI (:E :F :FI) (:B :BI :D :MI) (:B :BI :MI)
                       (:B :BI :D :E :F :MI :OI :S :SI) (:B :BI :D :E :F :MI :OI :S :SI)
                       (:B :BI) (:B :BI :D :M :MI :O :S))
                      (:MI :MI :E (:E :S :SI) :BI :MI (:B :BI :D :DI :F :FI :M :O :OI)
                       (:B :BI :DI :FI :M :O) (:B :BI :D :DI :F :FI :M :MI :O :OI)
                       (:B :BI :D :DI :F :FI :M :MI :O :OI) (:B :BI :DI :FI :M :O)
                       (:B :BI :D :DI :E :F :FI :M :O :OI :S :SI))
                      (:MI :MI (:E :S :SI) :E :BI :MI (:B :BI :D) (:B :BI)
                       (:B :BI :D :F :MI :OI) (:B :BI :D :F :MI :OI) (:B :BI :M)
                       (:B :BI :D :M :O :S))
                      (:O :M :B :B :E (:B :D :M :O :S)
                       (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI)
                       (:B :BI :DI :E :F :FI :MI :OI :SI)
                       (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI)
                       (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI) (:B :BI :DI :MI :OI :SI)
                       (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI))
                      ((:E :F :FI) (:E :F :FI) :M :M (:BI :DI :MI :OI :SI) :E
                       (:B :BI :D :DI :MI :O :OI :S :SI) (:B :BI :DI :MI :OI :SI)
                       (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI)
                       (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI) (:B :BI :DI :MI :OI :SI)
                       (:B :BI :D :DI :M :MI :O :OI :S :SI))
                      ((:B :BI :D :DI :M :O :OI :S :SI) (:B :BI :DI :M)
                       (:B :BI :D :DI :F :FI :MI :O :OI) (:B :BI :DI)
                       (:B :BI :D :DI :E :F :FI :M :O :OI :S :SI)
                       (:B :BI :D :DI :M :O :OI :S :SI) :E :FI :M :M :OI (:E :F :FI))
                      ((:B :BI :D :M :O :S) (:B :BI :M) (:B :BI :D :F :MI :OI) (:B :BI)
                       (:B :BI :D :E :F :FI :M :O :S) (:B :BI :D :M :O :S) :F :E :M :M :MI
                       (:E :F :FI))
                      ((:B :BI :D :DI :E :F :FI :M :O :OI :S :SI)
                       (:B :BI :DI :E :FI :M :O :S :SI) (:B :BI :D :DI :F :FI :M :MI :O :OI)
                       (:B :BI :DI :FI :M :O) (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI)
                       (:B :BI :D :DI :E :F :FI :M :O :OI :S :SI) :MI :MI :E (:E :S :SI) :BI
                       :MI)
                      ((:B :BI :D :DI :E :F :FI :M :O :OI :S :SI)
                       (:B :BI :DI :E :FI :M :O :S :SI) (:B :BI :D :DI :F :FI :M :MI :O :OI)
                       (:B :BI :DI :FI :M :O) (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI)
                       (:B :BI :D :DI :E :F :FI :M :O :OI :S :SI) :MI :MI (:E :S :SI) :E :BI
                       :MI)
                      ((:B :BI :D :M :O :S) (:B :BI) (:B :BI :D :F :MI :OI) (:B :BI :MI)
                       (:B :BI :D :M :O :S) (:B :BI :D :M :O :S) :O :M :B :B :E
                       (:B :D :M :O :S))
                      ((:B :BI :D :DI :M :MI :O :OI :S :SI) (:B :BI :DI :M :MI :OI :SI)
                       (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI) (:B :BI :DI :MI :OI :SI)
                       (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI)
                       (:B :BI :D :DI :M :MI :O :OI :S :SI) (:E :F :FI) (:E :F :FI) :M :M
                       (:BI :DI :MI :OI :SI) :E))

;;; FROM LUCID:

#N((:E :FI
     :M
     :M
     :OI
     (:E :F :FI)
     (:B :BI :D :DI :MI :O :OI :S :SI)
     (:B :BI :DI :MI :OI :SI)
     (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI)
     (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI)
     (:B :BI :DI :MI :OI :SI)
     (:B :BI :D :DI :M :MI :O :OI :S :SI))
  (:F :E
      :M
      :M
      :MI
      (:E :F :FI)
      (:B :BI :D :MI)
      (:B :BI :MI)
      (:B :BI :D :E :F :MI :OI :S :SI)
      (:B :BI :D :E :F :MI :OI :S :SI)
      (:B :BI)
      (:B :BI :D :M :MI :O :S))
  (:MI :MI
       :E
       (:E :S :SI)
       :BI
       :MI
       (:B :BI :D :DI :F :FI :M :O :OI)
       (:B :BI :DI :FI :M :O)
       (:B :BI :D :DI :F :FI :M :MI :O :OI)
       (:B :BI :D :DI :F :FI :M :MI :O :OI)
       (:B :BI :DI :FI :M :O)
       (:B :BI :D :DI :E :F :FI :M :O :OI :S :SI))
  (:MI :MI
       (:E :S :SI)
       :E
       :BI
       :MI
       (:B :BI :D)
       (:B :BI)
       (:B :BI :D :F :MI :OI)
       (:B :BI :D :F :MI :OI)
       (:B :BI :M)
       (:B :BI :D :M :O :S))
  (:O :M
      :B
      :B
      :E
      (:B :D :M :O :S)
      (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI)
      (:B :BI :DI :E :F :FI :MI :OI :SI)
      (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI)
      (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI)
      (:B :BI :DI :MI :OI :SI)
      (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI))
  ((:E :F :FI) (:E :F :FI)
               :M
               :M
               (:BI :DI :MI :OI :SI)
               :E
               (:B :BI :D :DI :MI :O :OI :S :SI)
               (:B :BI :DI :MI :OI :SI)
               (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI)
               (:B :BI :D :DI :E :F :FI :MI :O :OI :S :SI)
               (:B :BI :DI :MI :OI :SI)
               (:B :BI :D :DI :M :MI :O :OI :S :SI))
  ((:B :BI :D :DI :M :O :OI :S :SI) (:B :BI :DI :M)
                                    (:B :BI :D :DI :F :FI :MI :O :OI)
                                    (:B :BI :DI)
                                    (:B :BI :D :DI :E :F :FI :M :O :OI :S :SI)
                                    (:B :BI :D :DI :M :O :OI :S :SI)
                                    :E
                                    :FI
                                    :M
                                    :M
                                    :OI
                                    (:E :F :FI))
  ((:B :BI :D :M :O :S) (:B :BI :M)
                        (:B :BI :D :F :MI :OI)
                        (:B :BI)
                        (:B :BI :D :E :F :FI :M :O :S)
                        (:B :BI :D :M :O :S)
                        :F
                        :E
                        :M
                        :M
                        :MI
                        (:E :F :FI))
  ((:B :BI :D :DI :E :F :FI :M :O :OI :S :SI)
    (:B :BI :DI :E :FI :M :O :S :SI)
    (:B :BI :D :DI :F :FI :M :MI :O :OI)
    (:B :BI :DI :FI :M :O)
    (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI)
    (:B :BI :D :DI :E :F :FI :M :O :OI :S :SI)
    :MI
    :MI
    :E
    (:E :S :SI)
    :BI
    :MI)
  ((:B :BI :D :DI :E :F :FI :M :O :OI :S :SI)
    (:B :BI :DI :E :FI :M :O :S :SI)
    (:B :BI :D :DI :F :FI :M :MI :O :OI)
    (:B :BI :DI :FI :M :O)
    (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI)
    (:B :BI :D :DI :E :F :FI :M :O :OI :S :SI)
    :MI
    :MI
    (:E :S :SI)
    :E
    :BI
    :MI)
  ((:B :BI :D :M :O :S) (:B :BI)
                        (:B :BI :D :F :MI :OI)
                        (:B :BI :MI)
                        (:B :BI :D :M :O :S)
                        (:B :BI :D :M :O :S)
                        :O
                        :M
                        :B
                        :B
                        :E
                        (:B :D :M :O :S))
  ((:B :BI :D :DI :M :MI :O :OI :S :SI) (:B :BI :DI :M :MI :OI :SI)
                                        (:B :BI
                                            :D
                                            :DI
                                            :E
                                            :F
                                            :FI
                                            :MI
                                            :O
                                            :OI
                                            :S
                                            :SI)
                                        (:B :BI :DI :MI :OI :SI)
                                        (:B :BI
                                            :D
                                            :DI
                                            :E
                                            :F
                                            :FI
                                            :M
                                            :MI
                                            :O
                                            :OI
                                            :S
                                            :SI)
                                        (:B :BI :D :DI :M :MI :O :OI :S :SI)
                                        (:E :F :FI)
                                        (:E :F :FI)
                                        :M
                                        :M
                                        (:BI :DI :MI :OI :SI)
                                        :E))
|#

(assert-constraints domain-constraints-2 net1 :propagate-p nil)
(time (propagate-constraints net1))

;;; Putting domain-constraints-2 into the network does not affect the
;;; propagated tcn.

===============
