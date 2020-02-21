;;;----------------------------------------------------------------------

#|
(setf net2 (setup-test-net "I" *OBJECTS-2*
			   (append *INITIAL*
				   *STACK-A-B*
				   *STACK-B-C*
				   *DOMAIN-CONSTRAINTS-1*)))

(pprint (get-constrained-objects net2))
("I" "G"
     "I1"
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
     "pre2(E2)")

(get-constraint-matrix net2)
(pprint '((:E (:B :BI :D :DI :E :F :FI ... 
|#

(setf *OUTPUT-1*
      '((:E (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI)
	 (:B :D :M :O :S)
	 :B
	 :B
	 :B
	 (:D :S)
	 :B
	 (:B :D :M :O :S)
	 (:B :M)
	 :B
	 :B
	 (:D :E :F :S)
	 (:D :S))
	((:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI) :E
	 (:D :S)
	 (:B :M)
	 :B
	 :B
	 (:D :F)
	 (:B :M)
	 (:D :E :F :S)
	 (:B :D :E :F :FI :M :O :S)
	 (:B :M)
	 (:B :M)
	 (:BI :D :F :MI :OI)
	 (:B :D :E :F :FI :M :O :S))
	((:BI :DI :MI :OI :SI) (:DI :SI)
	 :E
	 :FI
	 :M
	 :M
	 :OI
	 :FI
	 (:DI :OI :SI)			; <--- "I1" <rel> "I2"
	 (:DI :OI :SI)
	 (:DI :FI)
	 (:DI :FI :O)
	 (:BI :DI :MI :OI :SI)
	 (:DI :OI :SI))
	(:BI (:BI :MI)
	 :F
	 :E
	 :M
	 :M
	 :MI
	 (:E :F :FI)
	 (:BI :MI)
	 (:BI :MI)
	 (:BI :E :F :MI :OI :SI)
	 (:BI :D :E :F :MI :OI :S :SI)
	 :BI
	 (:BI :MI))
	(:BI :BI
	 :MI
	 :MI
	 :E
	 (:E :S :SI)
	 :BI
	 :MI
	 :BI
	 :BI
	 (:BI :MI)
	 (:BI :D :F :MI :OI)
	 :BI
	 :BI)
	(:BI :BI
	 :MI
	 :MI
	 (:E :S :SI)
	 :E
	 :BI
	 :MI
	 :BI
	 :BI
	 (:BI :MI)
	 (:BI :D :F :MI :OI)
	 :BI
	 :BI)
	((:DI :SI) (:DI :FI)
	 :O
	 :M
	 :B
	 :B
	 :E
	 (:B :M :O)
	 (:DI :E :F :FI :OI :SI)
	 (:DI :FI)
	 (:DI :FI :M :O)
	 (:DI :FI :M :O)
	 (:DI :OI :SI)
	 (:DI :E :F :FI :OI :SI))
	(:BI (:BI :MI)
	 :F
	 (:E :F :FI)
	 :M
	 :M
	 (:BI :MI :OI)
	 :E
	 (:BI :MI)
	 (:BI :MI)
	 (:BI :E :F :MI :OI :SI)
	 (:BI :D :E :F :MI :OI :S :SI)
	 :BI
	 (:BI :MI))
	((:BI :DI :MI :OI :SI) (:DI :E :FI :SI)
	 (:D :O :S)
	 (:B :M)
	 :B
	 :B
	 (:D :E :F :FI :O :S)
	 (:B :M)
	 :E
	 :FI
	 :M
	 :M
	 :OI
	 (:E :F :FI))
	((:BI :MI) (:BI :DI :E :F :FI :MI :OI :SI)
	 (:D :O :S)
	 (:B :M)
	 :B
	 :B
	 (:D :F)
	 (:B :M)
	 :F
	 :E
	 :M
	 :M
	 :MI
	 :F)
	(:BI (:BI :MI)
	 (:D :F)
	 (:B :E :FI :M :O :S)
	 (:B :M)
	 (:B :M)
	 (:D :F :MI :OI)
	 (:B :E :FI :M :O :S)
	 :MI
	 :MI
	 :E
	 (:E :S :SI)
	 :BI
	 :MI)
	(:BI (:BI :MI)
	 (:D :F :OI)
	 (:B :DI :E :FI :M :O :S :SI)
	 (:B :DI :FI :M :O)
	 (:B :DI :FI :M :O)
	 (:D :F :MI :OI)
	 (:B :DI :E :FI :M :O :S :SI)
	 :MI
	 :MI
	 (:E :S :SI)
	 :E
	 :BI
	 :MI)
	((:DI :E :FI :SI) (:B :DI :FI :M :O)
	 (:B :D :M :O :S)
	 :B
	 :B
	 :B
	 (:D :O :S)
	 :B
	 :O
	 :M
	 :B
	 :B
	 :E
	 (:D :O :S))
	((:DI :SI) (:BI :DI :E :F :FI :MI :OI :SI)
	 (:D :O :S)
	 (:B :M)
	 :B
	 :B
	 (:D :E :F :FI :O :S)
	 (:B :M)
	 (:E :F :FI)
	 :FI
	 :M
	 :M
	 (:DI :OI :SI)
	 :E)))

;;;----------------------------------------------------------------------
