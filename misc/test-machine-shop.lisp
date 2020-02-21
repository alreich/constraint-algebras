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

#|
(defun SETUP-TEST-NET (first-obj rest-objs constraints)
  (let ((net (create-constraint-network 'linear-interval first-obj
					:obj-eq-test #'string=)))
    (register-constrained-objects rest-objs :interval net)
    (assert-constraints constraints net :propagate-p nil)
    (propagate-constraints net)
    net))
|#

(defun SETUP-TEST-NET (objects constraints)
  (let ((net (initialize-constraint-network 'linear-interval
					    :obj-eq-test #'string=)))
    (register-constrained-objects objects :interval net)
    (assert-constraints constraints net :propagate-p nil)
    (propagate-constraints net)
    net))

#|

(setf shop-net (setup-test-net *OBJECTS* *CONSTRAINTS*))
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
