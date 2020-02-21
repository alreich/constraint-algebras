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
