(in-package "CONSTRAINT-MAINTENANCE")

(defvar *LINEAR-POINT-ABBREVS* nil)

(setf *LINEAR-POINT-ABBREVS*
      '((:before          . :< )
        (:equal           . := )
        (:before-or-equal . :<=)
        (:after           . :> )
        (:not-equal       . :/=)
        (:after-or-equal  . :>=)
        (:unknown         . :? )
        ))

(defvar *LINEAR-INTERVAL-ABBREVS* nil)

#|
(setf *LINEAR-INTERVAL-ABBREVS*
      '(
        (:before        . :B  )
        (:equal         . :E  )
        (:after         . :BI )
        (:during        . :D  )
        (:contains      . :DI )
        (:finishes      . :F  )
        (:finished-by   . :FI )
        (:meets         . :M  )
        (:met-by        . :MI )
        (:overlaps      . :O  )
        (:overlapped-by . :OI )
        (:starts        . :S  )
        (:started-by    . :SI )
        ;;
        ;; ADDITIONAL ABBREVIATIONS:
        ;;
        (:?        . (:B :E :BI :D :DI :F :FI :M :MI :O :OI :S :SI))
        (:in       . (:D :E :F :S))
        (:disjoint . (:B :BI :M :MI))
        ;;
        ;; END POINT RELATIONS OF MATUSZEK, ET AL.
        ;;
        (:sbs                      . (:B :DI :FI :M :O))
        (:starts-before-starts     . (:B :DI :FI :M :O))
        (:sbf                      . (:B :E :D :DI :F :FI :M :O :OI :S :SI))
        (:starts-before-finishes   . (:B :E :D :DI :F :FI :M :O :OI :S :SI))
        (:sws                      . (:E :S :SI))
        (:starts-when-starts       . (:E :S :SI))
        (:swf                      . :MI)
        (:starts-when-finishes     . :MI)
        (:sas                      . (:BI :D :F :MI :OI))
        (:starts-after-starts      . (:BI :D :F :MI :OI))
        (:saf                      . :BI)
        (:starts-after-finishes    . :BI)
        (:fbs                      . :B)
        (:finishes-before-starts   . :B)
        (:fbf                      . (:B :D :M :O :S))
        (:finishes-before-finishes . (:B :D :M :O :S))
        (:fws                      . :M)
        (:finishes-when-starts     . :M)
        (:fwf                      . (:E :F :FI))
        (:finishes-when-finishes   . (:E :F :FI))
        (:fas                      . (:E :BI :D :DI :F :FI :MI :O :OI :S :SI))
        (:finishes-after-starts    . (:E :BI :D :DI :F :FI :MI :O :OI :S :SI))
        (:faf                      . (:BI :DI :MI :OI :SI))
        (:finishes-after-finishes  . (:BI :DI :MI :OI :SI))
        ))
|#

(setf *LINEAR-INTERVAL-ABBREVS*
      '(
        (:before        :B  )
        (:equal         :E  )
        (:after         :BI )
        (:during        :D  )
        (:contains      :DI )
        (:finishes      :F  )
        (:finished-by   :FI )
        (:meets         :M  )
        (:met-by        :MI )
        (:overlaps      :O  )
        (:overlapped-by :OI )
        (:starts        :S  )
        (:started-by    :SI )
        ;;
        ;; ADDITIONAL ABBREVIATIONS:
        ;;
        (:?        (:B :E :BI :D :DI :F :FI :M :MI :O :OI :S :SI))
        (:in       (:D :E :F :S))
        (:disjoint (:B :BI :M :MI))
        ;;
        ;; END POINT RELATIONS OF MATUSZEK, ET AL.
        ;;
        (:sbs                      (:B :DI :FI :M :O))
        (:starts-before-starts     (:B :DI :FI :M :O))
        (:sbf                      (:B :E :D :DI :F :FI :M :O :OI :S :SI))
        (:starts-before-finishes   (:B :E :D :DI :F :FI :M :O :OI :S :SI))
        (:sws                      (:E :S :SI))
        (:starts-when-starts       (:E :S :SI))
        (:swf                      :MI)
        (:starts-when-finishes     :MI)
        (:sas                      (:BI :D :F :MI :OI))
        (:starts-after-starts      (:BI :D :F :MI :OI))
        (:saf                      :BI)
        (:starts-after-finishes    :BI)
        (:fbs                      :B)
        (:finishes-before-starts   :B)
        (:fbf                      (:B :D :M :O :S))
        (:finishes-before-finishes (:B :D :M :O :S))
        (:fws                      :M)
        (:finishes-when-starts     :M)
        (:fwf                      (:E :F :FI))
        (:finishes-when-finishes   (:E :F :FI))
        (:fas                      (:E :BI :D :DI :F :FI :MI :O :OI :S :SI))
        (:finishes-after-starts    (:E :BI :D :DI :F :FI :MI :O :OI :S :SI))
        (:faf                      (:BI :DI :MI :OI :SI))
        (:finishes-after-finishes  (:BI :DI :MI :OI :SI))
        ))

(setf *RIGHT-BRANCHING-INTERVAL-&-POINT-ABBREVS*
      '(
	(:before :=<B
	:=<BI
	:=<D
	:=<DI
	:=<E
	:=<F
	:=<FI
	:=<M
	:=<MI
	:=<O
	:=<OI
	:=<PE
	:=<PF
	:=<PFI
	:=<PS
	:=<PSI
	:=<RB
	:=<RBI
	:=<RO
	:=<ROI
	:=<RS
	:=<R~
	:=<S
	:=<SI
	))

(:? (:=<B :=<BI :=<D :=<DI :=<E :=<F :=<FI :=<M :=<MI :=<O :=<OI :=<PE :=<PF :=<PFI
	  :=<PS :=<PSI :=<RB :=<RBI :=<RO :=<ROI :=<RS :=<R~ :=<S :=<SI ))
(:in ...)
(:disjoint ...)