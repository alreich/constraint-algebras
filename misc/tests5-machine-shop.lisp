(in-package "CONSTRAINT-MAINTENANCE")

;;;; EXAMPLE: Build Machine Shop with Access Road and Grounds
;;;;          (see p. 691 in "VNR Encyc. of Math.")

(defparameter *OBJECTS* '(
			  :Found	; Lay Foundation (F)
			  :Brick	; Brickwork (B)
			  :Roof		; Roof Construction (Rf)
			  :Interior	; Interior Work (I)
			  ;;----------------------------------------
			  :Road		; Build Access Road (Rd)
			  :Ground	; Layout Grounds (G)
			  ;;----------------------------------------
			  :Delivery	; Delivery Time for Machines (D)
			  :Mount	; Mount Machines (M)
			  :Other	; Other Equipment (O)
			  ))


(defparameter *CONSTRAINTS*
  (list
   '(:meets  :Found    :Brick   )
   '(:meets  :Brick    :Roof    )
   '(:meets  :Roof     :Interior)

   '(:meets  :Found    :Road    )
   '(:meets  :Road     :Ground  )

   '(:meets  :Delivery :Mount   )
   '(:meets  :Mount    :Other   )

   '(:before :Brick    :Mount   )
   '(:before :Road     :Mount   )
   '(:before :Mount    :Interior)

   ;; '(:starts-when-starts     :Found  :Delivery)
   ;; '(:finishes-when-finishes :Ground :Interior)
   ;; '(:finishes-when-finishes :Ground :Other   )
   ;; '(:finishes-when-finishes :Other  :Interior)
   ))

(defun SETUP-TEST-NET (objects constraints &key (propagate-p t))
  (let ((net (initialize-constraint-network 'linear-interval
					    :obj-eq-test #'string=)))
    (register-constrained-objects objects :interval net)
    (assert-constraints constraints net :propagate-p nil)
    (if propagate-p (propagate-constraints net))
    net))

#|

(setf *shop-net-no-prop* (setup-test-net *OBJECTS* *CONSTRAINTS* :propagate-p nil))
(setf *shop-net*         (setup-test-net *OBJECTS* *CONSTRAINTS*))
(pprint (get-constraints shop-net))

((:M :FOUND :BRICK) (:B :FOUND :ROOF) (:B :FOUND :INTERIOR) (:M :FOUND :ROAD)
 (:B :FOUND :GROUND) ((:B :D :M :O :S) :FOUND :DELIVERY) (:B :FOUND :MOUNT)
 (:B :FOUND :OTHER) (:M :BRICK :ROOF) (:B :BRICK :INTERIOR)
 ((:E :S :SI) :BRICK :ROAD) ((:B :DI :FI :M :O) :BRICK :GROUND)
 ((:B :D :M :O :S) :BRICK :DELIVERY) (:B :BRICK :MOUNT) (:B :BRICK :OTHER)
 (:M :ROOF :INTERIOR) ((:BI :MI :OI) :ROOF :ROAD)
 ((:BI :D :DI :E :F :FI :MI :O :OI :S :SI) :ROOF :GROUND)
 ((:DI :OI :SI) :ROOF :DELIVERY) (:DI :ROOF :MOUNT) ((:DI :FI :O) :ROOF :OTHER)
 (:BI :INTERIOR :ROAD) ((:BI :D :F :MI :OI) :INTERIOR :GROUND)
 (:BI :INTERIOR :DELIVERY) (:BI :INTERIOR :MOUNT)
 ((:BI :D :F :MI :OI) :INTERIOR :OTHER) (:M :ROAD :GROUND)
 ((:B :D :M :O :S) :ROAD :DELIVERY) (:B :ROAD :MOUNT) (:B :ROAD :OTHER)
 ((:B :D :DI :E :F :FI :M :O :OI :S :SI) :GROUND :DELIVERY)
 ((:B :DI :FI :M :O) :GROUND :MOUNT) ((:B :DI :FI :M :O) :GROUND :OTHER)
 (:M :DELIVERY :MOUNT) (:B :DELIVERY :OTHER) (:M :MOUNT :OTHER))

;;; In the matrix below, the lowercase relations coorespond to the relations
;;; asserted (i.e., *CONSTRAINTS*, above):

;;; :F   :B    :Rf        :I   :Rd          :G    :D           :M   :O
;;;------------------------------------------------------------------------------
#N((:E   :m    :B         :B   :m           :B    :FBF         :B   :B          ) ; F
   (:MI  :E    :m         :B   :SWS         :SBS  :FBF         :b   :B          ) ; B
   (:BI  :MI   :E         :m  (:BI :MI :OI) :FAS (:DI :OI :SI) :DI (:DI :FI :O) ) ; Rf
   (:BI  :BI   :MI        :E   :BI          :SAS  :BI          :BI  :SAS        ) ; I
   (:MI  :SWS (:B :M :O)  :B   :E           :m    :FBF         :b   :B          ) ; Rd
   (:BI  :SAS  :SBF       :SBS :MI          :E    :SBF         :SBS :SBS        ) ; G
   (:FAF :FAF (:D :O :S)  :B   :FAF         :FAS  :E           :m   :B          ) ; D
   (:BI  :BI   :D         :b   :BI          :SAS  :MI          :E   :m          ) ; M
   (:BI  :BI  (:D :F :OI) :SBS :BI          :SAS  :BI          :MI  :E          ) ; O
   )
|#


#|

FYI: Running the above at the Lisp prompt and looking at the internals of
     the TCN reveals the following:

> (cd "Lisp/CM")
> (load "load-file")
> (in-package "CM")
> (defparameter *OBJECTS* '(...))
> (defparameter *CONSTRAINTS* (list...))
> (setf net (initialize-constraint-network 'linear-interval :obj-eq-test #'string=))
> (register-constrained-objects *OBJECTS* :interval net)
> (pprint (list-array (get-network-arr (get-constraint-matrix net))))
((2 8191 8191 8191 8191 8191 8191 8191 8191)
 (8191 2 8191 8191 8191 8191 8191 8191 8191)
 (8191 8191 2 8191 8191 8191 8191 8191 8191)
 (8191 8191 8191 2 8191 8191 8191 8191 8191)
 (8191 8191 8191 8191 2 8191 8191 8191 8191)
 (8191 8191 8191 8191 8191 2 8191 8191 8191)
 (8191 8191 8191 8191 8191 8191 2 8191 8191)
 (8191 8191 8191 8191 8191 8191 8191 2 8191)
 (8191 8191 8191 8191 8191 8191 8191 8191 2))

> (assert-constraints *CONSTRAINTS* net :propagate-p nil)
> (pprint (list-array (get-network-arr (get-constraint-matrix net))))
((2 128 8191 8191 128 8191 8191 8191 8191)
 (256 2 128 8191 8191 8191 8191 1 8191)
 (8191 256 2 128 8191 8191 8191 8191 8191)
 (8191 8191 256 2 8191 8191 8191 4 8191)
 (256 8191 8191 8191 2 128 8191 1 8191)
 (8191 8191 8191 8191 256 2 8191 8191 8191)
 (8191 8191 8191 8191 8191 8191 2 128 8191)
 (8191 4 8191 1 4 8191 256 2 128)
 (8191 8191 8191 8191 8191 8191 8191 256 2))

> (propagate-constraints net)
> (pprint (list-array (get-network-arr (get-constraint-matrix net))))
((2 128 1 1 128 1 2697 1 1)
 (256 2 128 1 6146 721 2697 1 1)
 (4 256 2 128 1284 8062 5136 16 592)
 (4 4 256 2 4 1324 4 4 1324)
 (256 6146 641 1 2 128 2697 1 1)
 (4 1324 7931 721 256 2 7931 721 721)
 (5396 5396 2568 1 5396 8062 2 128 1)
 (4 4 8 1 4 1324 256 2 128)
 (4 4 1064 721 4 1324 4 256 2))

|#

