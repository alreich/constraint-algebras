(in-package "CONSTRAINT-MAINTENANCE")


(defparameter *OBJECTS-1* '(		; See p. 32 of "RaP"
			    ;; "I1"	; Stack(A,B)
			    "con1(E1)"	; Holding(A)
			    "eff1(E1)"	; Clear(A) ... after holding
			    "eff2(E1)"	; On(A,B)
			    "pre1(E1)"	; Clear(A) ... before holding
			    "pre2(E1)"	; Clear(B) ... before stacking A on
			    "I2"	; Stack(B,C)
			    "con1(E2)"	; Holding(B)
			    "eff1(E2)"	; Clear(B) ... after holding
			    "eff2(E2)"	; On(B,C)
			    "pre1(E2)"	; Clear(B) ... before holding
			    "pre2(E2)")) ; Clear(C)


(defparameter *OBJECTS-2* '(		; See p. 32 of "RaP"
			    ;; "I"
			    "G"
			    "I1"	; Stack(A,B)
			    "con1(E1)"	; Holding(A)
			    "eff1(E1)"	; Clear(A) ... after holding
			    "eff2(E1)"	; On(A,B)
			    "pre1(E1)"	; Clear(A) ... before holding
			    "pre2(E1)"	; Clear(B) ... before stacking A on
			    "I2"	; Stack(B,C)
			    "con1(E2)"	; Holding(B)
			    "eff1(E2)"	; Clear(B) ... after holding
			    "eff2(E2)"	; On(B,C)
			    "pre1(E2)"	; Clear(B) ... before holding
			    "pre2(E2)")) ; Clear(C)

#|
(defparameter *INITIAL*
  (list
   '((:d :e :f :s) "I" "pre1(E1)")
   '((:d :e :f :s) "I" "pre1(E2)")
   '((:d :e :f :s) "I" "pre2(E2)")
   '((:d :e :f :s) "G" "I1")
   '((:d :e :f :s) "G" "I2")))


(defparameter *STACK-A-B*		; Stacking constraints: On(A,B)
  (list
   '(:O        "pre1(E1)"     "I1"    )
   '(:F        "con1(E1)"     "I1"    )
   '(:M        "pre1(E1)"   "con1(E1)")
   '(:M          "I1"       "eff1(E1)")
   '((:E :F :FI) "I1"       "pre2(E1)")
   '(:M          "I1"       "eff2(E1)")))


(defparameter *STACK-B-C*		; Stacking constraints: On(B,C)
  (list
   '(:O        "pre1(E2)"     "I2"    )
   '(:F        "con1(E2)"     "I2"    )
   '(:M        "pre1(E2)"   "con1(E2)")
   '(:M          "I2"       "eff1(E2)")
   '((:E :F :FI) "I2"       "pre2(E2)")
   '(:M          "I2"       "eff2(E2)")))
|#

(defparameter *INITIAL*
  (list
   '(:in "I" "pre1(E1)")
   '(:in "I" "pre1(E2)")
   '(:in "I" "pre2(E2)")
   '(:in "G" "I1")
   '(:in "G" "I2")))


(defparameter *STACK-A-B*		; Stacking constraints: On(A,B)
  (list
   '(:overlaps               "pre1(E1)" "I1"      )
   '(:finishes               "con1(E1)" "I1"      )
   '(:meets                  "pre1(E1)" "con1(E1)")
   '(:meets                  "I1"       "eff1(E1)")
   '(:finishes-when-finishes "I1"       "pre2(E1)")
   '(:meets                  "I1"       "eff2(E1)")))


(defparameter *STACK-B-C*		; Stacking constraints: On(B,C)
  (list
   '(:overlaps               "pre1(E2)" "I2"      )
   '(:finishes               "con1(E2)" "I2"      )
   '(:meets                  "pre1(E2)" "con1(E2)")
   '(:meets                  "I2"       "eff1(E2)")
   '(:finishes-when-finishes "I2"       "pre2(E2)")
   '(:meets                  "I2"       "eff2(E2)")))


;;; IMPORTANT NOTE: There are multiple "Clear" intervals for some of
;;; the Clear(.) states:
;;; 
;;; "eff1(E1)"	; Clear(A) ... after holding (case 1)
;;; "pre1(E1)"	; Clear(A) ... before holding (case 2)
;;; "pre2(E1)"	; Clear(B) ... before stacking A on (3rd case)
;;; "eff1(E2)"	; Clear(B) ... after holding (2nd case)
;;; "pre1(E2)"	; Clear(B) ... before holding (1st case)
;;; "pre2(E2)"	; Clear(C)


(defparameter *DOMAIN-CONSTRAINTS-1*	; See p. 32 of "RaP"
  (list
   ;; Clear(B) and On(A,B) cannot overlap (1st case)
   '((:b :bi :m :mi) "eff1(E2)" "eff2(E1)")
   ;; Clear(B) and On(A,B) cannot overlap (2nd case)
   '((:b :bi :m :mi) "pre1(E2)" "eff2(E1)")
   ;; On(A,B) and Holding(B) cannot overlap
   '((:b :bi :m :mi) "eff2(E1)" "con1(E2)")
   ;; Clear(B) and Holding(B) cannot overlap
   '((:b :bi :m :mi) "pre2(E1)" "con1(E2)")
   ;; Holding(A) and Holding(B) cannot overlap
   '((:b :bi :m :mi) "con1(E1)" "con1(E2)")
   ))


#|
(defparameter *DOMAIN-CONSTRAINTS-2*	; Additional constraints not in "RaP"
  (list
   ;; Clear(C) and On(B,C) cannot overlap
   '((:b :bi :m :mi) "eff2(E2)" "pre2(E2)")
   ;; Clear(B) and On(A,B) cannot overlap
   '((:b :bi :m :mi) "eff2(E1)" "pre1(E2)")
   ;; Clear(B) and On(A,B) cannot overlap (3rd case)
   '((:b :bi :m :mi) "eff2(E1)" "pre2(E1)")
   ;; Holding(A) and Clear(A) cannot overlap (both cases)
   '((:b :bi :m :mi) "con1(E1)" "eff1(E1)") ; case 1
   '((:b :bi :m :mi) "con1(E1)" "pre1(E1)") ; case 2
   ;; Holding(A) and On(A,B) cannot overlap
   '((:b :bi :m :mi) "con1(E1)" "eff2(E1)")
   ;; Holding(B) and On(B,C) cannot overlap
   '((:b :bi :m :mi) "con1(E2)" "eff2(E2)")
   ;; Other Clear relations cannot overlap
   '((:b :bi :m :mi) "pre2(E1)" "eff1(E2)") 
   '((:b :bi :m :mi) "pre2(E1)" "pre1(E2)")))

   ;; The Clear relations before and after Holding relations
   ;; cannot overlap or meet
   '((:b :bi) "eff1(E1)" "pre1(E1)")	; Clear(A)'s
   '((:b :bi) "eff1(E2)" "pre1(E2)")	; Clear(B)'s
|#


(defun SETUP-TEST-NET (first-obj rest-objs constraints)
  (let ((net (create-constraint-network 'linear-interval first-obj
					:obj-eq-test #'string=)))
    (register-constrained-objects rest-objs :interval net)
    (assert-constraints constraints net :propagate-p nil)
    (propagate-constraints net)
    net))


#|
(setf net1 (setup-test-net "I1" *OBJECTS-1*
			   (append *STACK-A-B*
				   *STACK-B-C*
				   *DOMAIN-CONSTRAINTS-1*)))

(setf net2 (setup-test-net "I" *OBJECTS-2*
			   (append *INITIAL*
				   *STACK-A-B*
				   *STACK-B-C*
				   *DOMAIN-CONSTRAINTS-1*)))
|#


;;; If only the stacking constraints are added (no domain
;;; constraints), then the following constraint matrix results:
;;; 
;;; ABBREVIATIONS (used below):
;;; 
;;; :ss  = SameStart      = (:E :S :SI)
;;; :sf  = SameFinish     = (:E :F :FI)
;;; :fb  = FinishesBefore = (:B :D :M :O :S)
;;; :fbi = FinishesAfter  = (:BI :DI :MI :OI :SI)
;;;
;;; CONSTRAINT-MATRIX:
;;;  S(A,B) H(A)     C(A)    O(A,B)    C(A)     C(B)
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
;;;   (:?   :?       :?        :?      :?       :?     |  :sf  :sf      :M       :M       :fbi     :E ))
;;; 
;;;  CONSTRAINED-OBJECTS    ("I1" "con1(E1)" "eff1(E1)" "eff2(E1)" "pre1(E1)" "pre2(E1)"
;;; 			     "I2" "con1(E2)" "eff1(E2)" "eff2(E2)" "pre1(E2)" "pre2(E2)")
;;;  ALG-NAME               LINEAR-INTERVAL
;;;  OBJ-EQ-TEST            #<Compiled-Function STRING= 5FE41F>
;;;  OBJ-ID-KEY             #<Compiled-Function IDENTITY 5F435F>
