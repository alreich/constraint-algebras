;(defun EQUAL-NETS-P (net1 net2)
;  (let* ((arr1     (get-network-arr net1  ))
;	 (row-dim1 (array-dimension arr1 0))
;	 (col-dim1 (array-dimension arr1 1))
;	 (arr2     (get-network-arr net2  ))
;	 (row-dim2 (array-dimension arr2 0))
;	 (col-dim2 (array-dimension arr2 1))
;	 (ret-val t))
;    (if (or (/= row-dim1 row-dim2)
;	    (/= col-dim1 col-dim2))
;	nil
;	(dotimes (i row-dim1)
;	  (dotimes (j col-dim1)
;	    (setq ret-val
;		  (and ret-val
;		       (equal (aref arr1 i j)
;			      (aref arr2 i j)))))))))

;(defun SKEW-OF-P (net1 net2)
;  (let* ((arr1     (get-network-arr net1  ))
;	 (row-dim1 (array-dimension arr1 0))
;	 (col-dim1 (array-dimension arr1 1))
;	 (arr2     (get-network-arr net2  ))
;	 (row-dim2 (array-dimension arr2 0))
;	 (col-dim2 (array-dimension arr2 1))
;	 (ret-val  t)
;	 (alg1     (get-network-algebra net1))
;	 (alg2     (get-network-algebra net2))
;	 (inv      (get-algebra-inverter alg1)))
;    (if (or (/= row-dim1 col-dim2)
;	    (/= row-dim2 col-dim1)
;	    (not (equal (get-algebra-name alg1)
;			(get-algebra-name alg2))))
;	nil
;	(dotimes (i row-dim1 ret-val)
;	  (dotimes (j col-dim1)
;	    (setq ret-val
;		  (and ret-val
;		       (= (aref arr1 i j)
;			  (funcall inv (aref arr2 j i))))))))))

;;; From tcn.lisp (3/6/93) (INCOMPLETE)
(defun OVERLAY-ARRAY (from-array to-array &optional start-index)
  "OVERLAY-ARRAY assumes that the two arrays are both square and
   that the dimension of the 'to' array is greater than or equal
   to the 'from' dimension.  'from-array' is copied into 'to-array'
   starting (top-left corner) at diagonal elemnent 'start-index' in
   to-array.  'start-index' must be an integer between 0 and the
   difference in dimensions, inclusive."
  (let* ((from-dim   (first (array-dimensions from-array)))
         (to-dim     (first (array-dimensions to-array))))
    (if (<= 0 start-index (- to-dim from-dim))
      (error "TCN (OVERLAY-ARRAY): Start index is out of range."))
    (dotimes (row 0 from-dim)
      (dotimes (col 0 from-dim)
        (setf (aref to-array
                    (+ row start-index)
                    (+ col start-index)
                    ...))))))

#| From 'network-support.lisp' (3/6/93)

(defun READ-NETWORK (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((contents (read stream))
	 (alg      (determine-network-type contents))
	 (nsize    (determine-network-size contents)))
    (make-network
      :algebra alg
      :arr (make-array
	     nsize
	     :adjustable t
	     :element-type 'integer
	     :initial-contents (encode-array-list
				 contents
				 (get-algebra-encoder alg))))))
|#

;;;----------------------------------------------------------------------

;;; Stuff from "tcn"

;;; (defun ADD-OBJECT (object constraint-network)
;;;   (setf (get-objects constraint-network)
;;;         (append (get-objects constraint-network) (list object))))

;;; (defun EXPAND-CONSTRAINT-MATRIX (con-mat elem-type &optional (incr 1))
;;;   (let* ((mat-array  (get-network-arr     con-mat))
;;;          (algebra    (get-network-algebra con-mat))
;;; 	 (equal-elem (get-equality-element algebra elem-type))
;;;          (dimensions (array-dimensions    mat-array))
;;;          (zero-elem  (get-algebra-zero    algebra))
;;; 	 (dimension  (first dimensions))
;;; 	 )
;;;     (adjust-array mat-array
;;;                   (mapcar #'(lambda (dim)
;;;                               (+ dim incr))
;;;                           dimensions)
;;;                   :element-type (array-element-type mat-array)
;;;                   :initial-element zero-elem)
;;;     (doloop index dimension (+ dimension incr -1)
;;; 	    (setf (aref mat-array index index) equal-elem))
;;;     con-mat))

#|      
(defun EXPAND-CONSTRAINT-MATRIX (con-mat &optional (incr 1))
  (let* ((mat-array  (get-network-arr     con-mat))
         (algebra    (get-network-algebra con-mat))
         (dimensions (array-dimensions    mat-array))
         (zero-elem  (get-algebra-zero    algebra))
	 (dimension  (1- (first dimensions)))
	 (equal-elem (aref mat-array 0 0)))
    (adjust-array mat-array
                  (mapcar #'(lambda (dim)
                              (+ dim incr))
                          dimensions)
                  :element-type (array-element-type mat-array)
                  :initial-element zero-elem)
    (doloop index dimension (+ dimension incr)
	    (setf (aref mat-array index index) equal-elem))
    con-mat))
|#

#|
(defun ADD-TO-CONSTRAINT-MATRIX (object1 object2 objects rel con-mat
					 &optional (propagate-p t))
  (let* ((pos1     (position object1 objects))
         (pos2     (position object2 objects))
         (alg      (get-network-algebra  con-mat))
         (adder    (get-algebra-adder    alg))
         (inverter (get-algebra-inverter alg))
         (crel     (if (eql rel :?)
		       (get-algebra-zero alg)
		       (funcall (get-algebra-encoder alg) rel)))
         (arr      (get-network-arr      con-mat))
         (crel12   (funcall adder crel (aref arr pos1 pos2))))
    (if (zerop crel12)
	(values
	 nil
	 "Constraint is immediately inconsistent with an existing constraint.")
	(progn
	  (setf (aref arr pos1 pos2) crel12)
	  (setf (aref arr pos2 pos1)
		(funcall inverter (aref arr pos1 pos2)))
	  (if propagate-p
	      (progn
		(setf con-mat (propagate con-mat))
		(if (consistentp con-mat)
		    (values
		     con-mat
		     "Constraint has been successfully added and propagated in the network.")
		    (values
		     nil
		     "Constraint causes network to propagate into an inconsistent state.")))
	      (values
	       con-mat
	       "Constraint has been added, but not propagated in the network."))
	  ))))
|#

(defun ADD-TO-CONSTRAINT-MATRIX (row col rel con-mat &optional (propagate-p t))
  (let* ((alg      (get-network-algebra  con-mat))
         (adder    (get-algebra-adder    alg))
         (inverter (get-algebra-inverter alg))
         (crel     (if (eql rel :?)
		       (get-algebra-zero alg)
		       (funcall (get-algebra-encoder alg) rel)))
         (arr      (get-network-arr      con-mat))
         (crel12   (funcall adder crel (aref arr row col))))
    (if (zerop crel12)
	(values
	 nil
	 "Constraint is immediately inconsistent with an existing constraint.")
	(progn
	  (setf (aref arr row col) crel12)
	  (setf (aref arr col row)
		(funcall inverter (aref arr row col)))
	  (if propagate-p
	      (progn
		(setf con-mat (propagate con-mat))
		(if (consistentp con-mat)
		    (values
		     con-mat
		     "Constraint has been successfully added and propagated in the network.")
		    (values
		     nil
		     "Constraint causes network to propagate into an inconsistent state.")))
	      (values
	       con-mat
	       "Constraint has been added, but not propagated in the network."))
	  ))))

(defun ADD-CONSTRAINT-TO-CONSTRAINT-NETWORK
    (constraint constraint-network
		&key (propagate-p t) (obj-eq-test #'eql) (obj-id-key #'identity))
  "Add a constraint (and possibly new objects) to a constraint network."
  (let* ((rel       (get-constraint-relation constraint))
         (object1   (get-constraint-object1  constraint))
         (object2   (get-constraint-object2  constraint))
         (con-mat-1 (get-constraint-matrix   constraint-network))
	 (con-mat-2 (copy-network con-mat-1))
         (objects   (get-constrained-objects constraint-network))
         (position1 (position object1 objects :test obj-eq-test :key obj-id-key))
         (position2 (position object2 objects :test obj-eq-test :key obj-id-key))
	 (new-pos   (array-dimension (get-network-arr con-mat-1) 0)))
    (cond
      ((and position1 position2)
       (format *debug-io* "~%;;; Both positions exist, ~S & ~S." position1 position2)
       (multiple-value-bind
	     (result explanation)
	   (add-to-constraint-matrix position1 position2 rel con-mat-2 propagate-p)
	 (values (if result
		     (update-constraint-network constraint-network con-mat-2 objects)
		     constraint-network)
		 explanation)))
      (position1
       (format *debug-io* "~%;;; Position1 = ~S, New-Pos = ~S." position1 new-pos)
       (let ((objs (append objects (list object2))))
	 (multiple-value-bind
	       (result explanation)
	     (add-to-constraint-matrix
	      position1 new-pos
	      rel (expand-constraint-matrix con-mat-2) propagate-p)
	   (values (if result
		       (update-constraint-network constraint-network con-mat-2 objs)
		       constraint-network)
		   explanation))))
      (position2
       (format *debug-io* "~%;;; New-Pos = ~S, Position2 = ~S." new-pos position2)
       (let ((objs (append objects (list object1))))
	 (multiple-value-bind
	       (result explanation)
	     (add-to-constraint-matrix
	      new-pos position2
	      rel (expand-constraint-matrix con-mat-2) propagate-p)
	   (values (if result
		       (update-constraint-network constraint-network con-mat-2 objs)
		       constraint-network)
		   explanation))))
      (t
       (format *debug-io* "~%;;; New-Pos1 = ~S, New-Pos2 = ~S." new-pos (1+ new-pos))
       (let ((objs (append objects (list object1 object2))))
	 (multiple-value-bind
	       (result explanation)
	     (add-to-constraint-matrix
	      new-pos (1+ new-pos)
	      rel (expand-constraint-matrix con-mat-2 2)
	      propagate-p)
	   (values (if result
		       (update-constraint-network constraint-network con-mat-2 objs)
		       constraint-network)
		   explanation)))))))

;;;----------------------------------------------------------------------

;;; 9/16/93

;;;--------------------

;;; from 'algebra-support.text'

;;; (defstruct (ALGEBRA
;;; 	     (:conc-name get-algebra-)
;;; 	     (:print-function print-algebra))
;;;   NAME						;symbol or string
;;;   DOCUMENTATION					;string
;;;   TYPE						;keyword
;;;   (ELEMENT-TYPE 'symbol)			;type specifier
;;;   RELS						;list of keywords
;;;   REL-CODING					;assoc list
;;;   INV-RELS					;assoc list
;;;   (EQUAL-P #'equal)				;closure
;;;   LESS-THAN-P					;closure
;;;   NUM-RELS					;integer
;;;   ZERO						;integer
;;;   (ENCODER #'identity)				;closure
;;;   (DECODER #'identity)				;closure
;;;   MULT-TABLE					;2-D array of integers
;;;   MULTIPLIER					;closure
;;;   ADDER						;closure
;;;   INVERTER					;closure
;;;   )

;(defun find-inv-from-net (net net-element-list)
;  ;; INPUT:
;  ;;   NET-i
;  ;;   net-element-list = ((REL-NAME-1 NET-1) ... (REL-NAME-n NET-n))
;  (find net net-element-list :test #'(lambda (n1 n2) (skew-of-p n1 n2)) :key #'cdr))

;(defun make-rel-lists (net-element-list)
;  ;; INPUT:
;  ;;   net-element-list = ((REL-NAME-1 NET-1) ... (REL-NAME-n NET-n))
;  ;; OUTPUT:
;  ;;   rel-list = (    REL-NAME-1 ... REL-NAME-n    )
;  ;;   inv-list = (INV-REL-NAME-1 ... INV-REL-NAME-n)
;  (let ((rel-list nil)
;	(inv-list nil))
;    (dolist (net-element net-element-list (values rel-list inv-list))
;      (push (first net-element)
;	    rel-list)
;      (push (find-inv-from-net
;	      (second net-element) net-element-list)
;	    inv-list))))

;(defun test-multiply-constraints (coded-rel-1 coded-rel-2 alg)
;  (let ((encoder    (get-algebra-encoder    alg))
;	(decoder    (get-algebra-decoder    alg))
;	(multiplier (get-algebra-multiplier alg)))
;    (funcall decoder (funcall multiplier
;			      (funcall encoder coded-rel-1)
;			      (funcall encoder coded-rel-2)))))

;;;--------------------

;;; from 'tnc.lisp' (1/13/94)

#|
(defun CREATE-CONSTRAINT-MATRIX (algebra-name object-type)
  (let* ((algebra (find-algebra algebra-name))
	 (eq-elem (if object-type
                    (get-equality-element algebra object-type)
                    (cdr (first (get-algebra-equalities algebra))))))
    (create-network algebra (list 1 1) (list (list eq-elem)))))
|#

#|
(defun CREATE-CONSTRAINT-NETWORK (algebra-name object1
					       &key
					       (obj-eq-test #'eql)
					       (obj-id-key  #'identity)
					       object-type)
  (make-instance
   'constraint-network
   :constraint-matrix   (create-constraint-matrix algebra-name object-type)
   :constrained-objects (list object1)
   :alg-name            algebra-name
   :obj-eq-test         obj-eq-test
   :obj-id-key          obj-id-key
   ))
|#

#|
(defun REGISTER-CONSTRAINED-OBJECT (object obj-type constraint-network)
  "Add an OBJECT of type, OBJ-TYPE, to a CONSTRAINT-NETWORK.  Effectively, this
   expands the constraint network's constraint matrix by one row and column.  The
   new diagonal element is the equality element for the OBJ-TYPE.  The other new
   matrix elements are the algebra's zero element.  The new object is added to
   the end of the constraint network's constrained-objects list, also.  Returns
   the constraint-network."
  (if (register-constrained-object-inputs-ok object obj-type constraint-network)
      (let* ((con-mat (get-constraint-matrix   constraint-network))
	     (objects (get-constrained-objects constraint-network)))
	(setf (get-constrained-objects constraint-network)
	      (append objects (list object)))
	(expand-network con-mat obj-type)
	constraint-network)
      constraint-network))
|#

;;;--------------------

;;; from 'tests5-machine-shop.lisp' (1/13/94)

#|
(defun SETUP-TEST-NET (first-obj rest-objs constraints)
  (let ((net (create-constraint-network 'linear-interval first-obj
					:obj-eq-test #'string=)))
    (register-constrained-objects rest-objs :interval net)
    (assert-constraints constraints net :propagate-p nil)
    (propagate-constraints net)
    net))
|#

#|
(defun CREATE-TC-MGR-DIALOG ()          ; The top-level dialog
  (let ((tcn-list
         (make-dialog-item
          'sequence-dialog-item
          #@(9 26) #@(282 124)
          "TCN List"
          'nil
          :cell-size #@(266 16)
          :selection-type :single
          :table-hscrollp t
          :table-vscrollp t
          :table-sequence 'nil
          :table-print-function #'(lambda (tcn stream)
                                    (format stream "~A" (network-name tcn)))
          )))
    (MAKE-INSTANCE 'TCM-DIALOG
      :WINDOW-TYPE
      :TOOL
      :WINDOW-TITLE
      "Constraint Network Manager"
      :VIEW-POSITION
      #@(337 130)
      :VIEW-SIZE
      #@(300 300)
      :CLOSE-BOX-P
      NIL
      :VIEW-FONT
      '("Chicago" 12 :SRCOR :PLAIN)
      :VIEW-SUBVIEWS
      (LIST TCN-LIST
            (MAKE-DIALOG-ITEM
             'STATIC-TEXT-DIALOG-ITEM
             #@(45 7)
             #@(216 16)
             "Select networks from this list"
             'NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(115 194)
             #@(62 16)
             "Copy"
             #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Copy network action."))
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(115 167)
             #@(61 16)
             "Create"
             #'(lambda (item)
                 ;; (format t "~%Create network action.")
                 (set-table-sequence
                  tcn-list
                  (tcns 
                   (create-tcn (tc-mgr-of-subviews-container item))))
                 )
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(115 223)
             #@(62 16)
             "Merge"
             #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Merge networks action."))
             :DIALOG-ITEM-ENABLED-P
             NIL
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(35 270)
             #@(62 16)
             "Edit"
             #'(lambda (item)
                 (declare (ignore item))
                 (assert-constraint-dialog (selected-item tcn-list))
                 )
             :VIEW-NICK-NAME
             'EDIT
             :DEFAULT-BUTTON
             T)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(212 167)
             #@(62 16)
             "Show"
             #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Show constraints action."))
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(212 194)
             #@(62 16)
             "Delete"
             #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Delete constraint action."))
             :DIALOG-ITEM-ENABLED-P
             NIL
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(118 270)
             #@(62 16)
             "Quit"
             #'(LAMBDA (ITEM)
                 (if 
                   (y-or-n-dialog
                    "OK to quit the Temporal Constraint Manager?"
                    :yes-text "Yes"
                    :no-text "No"
                    :cancel-text nil)
                   (window-close (view-container item))))
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(201 270)
             #@(62 16)
             "Help"
             #'(LAMBDA (ITEM)
                 ITEM
                 (MESSAGE-DIALOG
                  "HELP ON MANAGING CONSTRAINT NETWORKS:
This dialog lets you create and load temporal constraint networks. To assert constraints in a network click \"Edit\".  To see a network's contents click \"Show\"."
                  :OK-TEXT
                  "Quit Help"
                  :SIZE
                  9175375))
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(18 223)
             #@(62 16)
             "Save As"
             #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Merge networks action."))
             :VIEW-NICK-NAME
             'MERGE
             :DIALOG-ITEM-ENABLED-P
             NIL
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(19 167)
             #@(61 16)
             "Load"
             #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Create network action."))
             :VIEW-NICK-NAME
             'CREATE
             :DIALOG-ITEM-ENABLED-P
             NIL
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(18 194)
             #@(62 16)
             "Save"
             #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Copy network action."))
             :VIEW-NICK-NAME
             'COPY
             :DIALOG-ITEM-ENABLED-P
             NIL
             :DEFAULT-BUTTON
             NIL)
            (MAKE-DIALOG-ITEM
             'BUTTON-DIALOG-ITEM
             #@(212 223)
             #@(62 16)
             "Rename"
             #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Rename network action."))
             :VIEW-NICK-NAME
             'RENAME
             :DEFAULT-BUTTON
             NIL)))))
|#

