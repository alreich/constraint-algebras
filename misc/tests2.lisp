;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                   TESTCASES FOR TCN DEVELOPMENT
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(setf *TEST-NET-1*
      #N(( :E      (:D :DI) (:F :FI) (:MI :SI))
         ((:D :DI)  :E      (:D :DI)  :OI     )
         ((:F :FI) (:D :DI)  :E      (:MI :SI))
         ((:M :S)   :O      (:M :S)   :E      )))

(setf OBJS '(:OBJ1 :OBJ2 :OBJ3 :OBJ4))

(setf TCN1
      (make-instance 'constraint-network
        :constraint-matrix *test-net-1*
        :objects OBJS))

(setf ALG1 (get-network-algebra *TEST-NET-1*))

(setf CREL (cdr (assoc :D (get-algebra-rel-coding ALG1))))

(add-to-constraint-matrix :OBJ2 :OBJ3 OBJS CREL *TEST-NET-1*)

;;;---------------------

(setf ? (get-algebra-rels (find-algebra 'LINEAR-INTERVAL)))

(progn
  (setf *TEST-NET-2*
        ;;  :today :gardening :mowing :raking :dinner :boil :cook   :eat :dishes
        #N((  :E      #.?       #.?     #.?     #.?    #.?   #.?     #.?    #.?   ) ;today
           (  #.?     :E        :SI     :FI     #.?    #.?   #.?     #.?    #.?   ) ;gardening
           (  #.?     :S        :E      :B      #.?    #.?   #.?     #.?    #.?   ) ;mowing
           (  #.?     :F        :BI     :E      #.?    #.?   #.?     #.?    #.?   ) ;raking
           (  #.?     #.?       #.?     #.?     :E     :SI   #.?     :FI    :M    ) ;dinner
           (  #.?     #.?       #.?     #.?     :S     :E    :M      #.?    #.?   ) ;boil
           (  #.?     #.?       #.?     #.?     #.?    :MI   :E    (:B :M)  #.?   ) ;cook
           (  #.?     #.?       #.?     #.?     :F     #.? (:BI :MI) :E     #.?   ) ;eat
           (  #.?     #.?       #.?     #.?     :MI    #.?   #.?     #.?    :E    )));dishes
  'done)

;;; (propagate *TEST-NET-2*) ==> *TEST-NET-3*

(progn
  (setf *TEST-NET-3*
        ;;  :today :gardening :mowing :raking :dinner :boil :cook   :eat :dishes
        #N((  :E      #.?       #.?     #.?     #.?    #.?   #.?     #.?    #.?) ;today
           (  #.?     :E        :SI     :FI     #.?    #.?   #.?     #.?    #.?) ;gardening
           (  #.?     :S        :E      :B      #.?    #.?   #.?     #.?    #.?) ;mowing
           (  #.?     :F        :BI     :E      #.?    #.?   #.?     #.?    #.?) ;raking
           (  #.?     #.?       #.?     #.?     :E     :SI   :DI     :FI    :M ) ;dinner
           (  #.?     #.?       #.?     #.?     :S     :E    :M      :B     :B ) ;boil
           (  #.?     #.?       #.?     #.?     :D     :MI   :E    (:B :M)  :B ) ;cook
           (  #.?     #.?       #.?     #.?     :F     :BI (:BI :MI) :E     :M ) ;eat
           (  #.?     #.?       #.?     #.?     :MI    :BI   :BI     :MI    :E )));dishes
  'done)

;;;---------------------

;;; Add the following constraints to *TEST-NET-4*:
;;;    raking <equals> boil

(progn
  (setf *TEST-NET-4*
        ;;  :today :gardening :mowing :raking :dinner :boil :cook   :eat :dishes
        #N((  :E      #.?       #.?     #.?     #.?    #.?   #.?     #.?    #.?) ;today
           (  #.?     :E        :SI     :FI     #.?    #.?   #.?     #.?    #.?) ;gardening
           (  #.?     :S        :E      :B      #.?    #.?   #.?     #.?    #.?) ;mowing
           (  #.?     :F        :BI     :E      #.?    :E    #.?     #.?    #.?) ;raking
           (  #.?     #.?       #.?     #.?     :E     :SI   :DI     :FI    :M ) ;dinner
           (  #.?     #.?       #.?     :E      :S     :E    :M      :B     :B ) ;boil
           (  #.?     #.?       #.?     #.?     :D     :MI   :E    (:B :M)  :B ) ;cook
           (  #.?     #.?       #.?     #.?     :F     :BI (:BI :MI) :E     :M ) ;eat
           (  #.?     #.?       #.?     #.?     :MI    :BI   :BI     :MI    :E )));dishes
  'done)

;;; (propagate *TEST-NET-4*) ==> *TEST-NET-5*

(progn
  (setf *TEST-NET-5*
        ;;  :today :gardening :mowing :raking :dinner :boil :cook   :eat :dishes
        #N((  :E      #.?       #.?     #.?     #.?    #.?   #.?     #.?    #.?)
           (  #.?     :E        :SI     :FI     :O     :FI   :M      :B     :B)
           (  #.?     :S        :E      :B      :B     :B    :B      :B     :B)
           (  #.?     :F        :BI     :E      :S     :E    :M      :B     :B)
           (  #.?     :OI       :BI     :SI     :E     :SI   :DI     :FI    :M)
           (  #.?     :F        :BI     :E      :S     :E    :M      :B     :B)
           (  #.?     :MI       :BI     :MI     :D     :MI   :E    (:B :M)  :B)
           (  #.?     :BI       :BI     :BI     :F     :BI (:BI :MI) :E     :M)
           (  #.?     :BI       :BI     :BI     :MI    :BI   :BI     :MI    :E)
           )
        'done))
