;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                   TEMPORAL CONSTRAINT NETWORKS (TCN)
;;;---------------------------------------------------------------------------

;;; Four examples, embedded in the code below, illustrate how this code is
;;; used.

;;;---------------------------------------------------------------------------

(in-package "CONSTRAINT-MAINTENANCE")

(defclass CONSTRAINT-NETWORK ()
  ((NETWORK-NAME
    :initarg :network-name
    :accessor network-name
    :initform nil
    :documentation "A keyword or string.")
   (CONSTRAINT-MATRIX
    :initarg :constraint-matrix
    :accessor get-constraint-matrix
    :initform (create-constraint-matrix 'linear-interval :interval)
    :documentation "A constraint-matrix structure")
   (CONSTRAINED-OBJECTS
    :initarg :constrained-objects
    :accessor get-constrained-objects
    :initform '(:A)			; A list of 'objects' being constrained.
    :documentation "A list of constrained-objects in this TCN.")
   (ALG-NAME
    :initarg :alg-name
    :accessor get-alg-name
    :initform 'LINEAR-INTERVAL		; A symbol
    :documentation "The name of constraint algebra this TCN is based on.")
   (OBJ-EQ-TEST
    :initarg :obj-eq-test
    :accessor get-obj-eq-test
    :initform #'eql			; Same as default test of POSITION fnc
    :documentation "Tests for equality of two objects based on OBJ-ID-KEY.")
   (OBJ-ID-KEY
    :initarg :obj-id-key
    :accessor get-obj-id-key
    :initform #'identity		; Same as default key of POSITION fnc
    :documentation "Retrieves attribute from object for testing equality.")
   ))

(defun INITIALIZE-CONSTRAINT-NETWORK (algebra-name
				      &key
				      (obj-eq-test #'eql)
				      (obj-id-key  #'identity))
  (make-instance 'constraint-network
    :constraint-matrix   nil
    :constrained-objects nil
    :alg-name            algebra-name
    :obj-eq-test         obj-eq-test
    :obj-id-key          obj-id-key
    ))

;;;***********
;;; EXAMPLE 1:
;;;***********
;;;   The following form returns a constraint-network object that is set
;;;   up to accept constraints for an Allen-style interval network whose
;;;   interval objects are keywords.
;;; (setf NET (initialize-constraint-network 'linear-interval))
;;; ==> #<Constraint-Network #XF9344B>

;;;----------------------------------------------------------------------

;;; The following functions allow one to enter long names for relations.
;;; Long names should be defined in a form such as the one following and
;;; should be included in the file defining the algebra
;;; (e.g., basic-algebras).
;;;
;;;      [Actually, this code should probably be relocated to the file
;;;       'algebra-support.lisp']
;;;
;;; (setf *LINEAR-INTERVAL-ABBREVS*
;;;      '(
;;;        (:before        . :B  )
;;;        (:equal         . :E  )
;;;        (:after         . :BI )
;;;        ( ...

(defun ABBREV-RELATION (relation rel-list rel-abbrev-list)
  "Take a long relation name and return the shortened, internal version."
  (if (member relation rel-list)
    relation
    (second (assoc relation rel-abbrev-list))))

(defun ABBREV-RELATIONS (relations rel-list rel-abbrev-list)
  (typecase relations
    (symbol (list (abbrev-relation relations rel-list rel-abbrev-list)))
    (list   (mappend #'(lambda (rel)
                         (abbrev-relations rel rel-list rel-abbrev-list))
                     relations))))

(defun ABBREVIATE-RELATIONS (relations rel-list rel-abbrev-list)
  (let ((rels (sort
               (remove-duplicates 
                (flatten (abbrev-relations relations rel-list rel-abbrev-list)))
               #'string<)))
    (if (= (length rels) 1)
      (first rels)
      rels)))

(defun UNABBREV-RELATION (relation rel-abbrev-list)
  (first (find relation
               rel-abbrev-list
               :test #'(lambda (item pair)
                         (equal item (second pair))))))

;;;----------------------------------------------------------------------

;;; CONSTRAINTS:
;;; 
;;; The following abstractions, for taking constraints apart, assume that
;;; constraints are lists of the following form:
;;; 
;;;     ( <RELATION>  <OBJECT-1>  <OBJECT-2>  ...  )

(defun GET-CONSTRAINT-RELATION (constraint)
  (first constraint))

(defun GET-CONSTRAINT-OBJECT (constraint object-position)
  (nth object-position constraint))	; Object positions start at 1

;;; Most constraints are binary (i.e., only two objects).  Hence the
;;; following functions.

(defun GET-CONSTRAINT-OBJECT1 (constraint)
  (second constraint))

(defun GET-CONSTRAINT-OBJECT2 (constraint)
  (third constraint))

(defun PRINT-CONSTRAINT (constraint &optional (stream t))
  (format stream "~% ~A <r> ~A : r=~S"
          (get-constraint-object1  constraint)
          (get-constraint-object2  constraint)
          (get-constraint-relation constraint)
          ))

;;;----------------------------------------------------------------------
;;; REGISTER CONSTRAINED OBJECT WITH A CONSTRAINT NETWORK
;;;----------------------------------------------------------------------

(defun REGISTER-CONSTRAINED-OBJECT-INPUTS-OK (object obj-type constraint-network)
  "Check the inputs used in a call to REGISTER-CONSTRAINED-OBJECT.  The object
   should not already be in the constraint network.  If it is this is a continuable
   error.  Also, the object type should be one of the types known by the constraint
   network.  If not, this is a non-continuable error."
  (let* ((equalities (get-algebra-equalities
		      (get-network-algebra
		       (get-constraint-matrix constraint-network))))
	 (objects    (get-constrained-objects constraint-network))
	 (type-ok    (assoc obj-type equalities)) ; Is obj-type in alg's equalities?
	 (objects-ok (not (member object objects))) ; Is object already in con net?
	 (inputs-ok  t))
    (if (not type-ok)
	(error "Object type, ~S, is not compatible with constraint network, ~S"
	       obj-type constraint-network))
    (when (not objects-ok)
      (cerror
       "Proceed, using existing object."
       "The object, ~S, already exists in constraint network, ~S"
       object constraint-network)
      (setf inputs-ok nil))
    inputs-ok))

(defun REGISTER-CONSTRAINED-OBJECT (object obj-type constraint-network)
  "Add an OBJECT of type, OBJ-TYPE, to a CONSTRAINT-NETWORK.  Effectively, this
   expands the constraint network's constraint matrix by one row and column.  The
   new diagonal element is the equality element for the OBJ-TYPE.  The other new
   matrix elements are the algebra's zero element.  The new object is added to
   the end of the constraint network's constrained-objects list, also.  Returns
   the constraint-network."
  (if (null (get-constraint-matrix constraint-network))
    ;; IF THE CONSTRAINT MATRIX HAS NOT BEEN CREATED YET, CREATE IT
    ;; AND THEN PUT THE OBJECT INTO ITS CONSTRAINED OBJECTS LIST.
    (progn
      (setf (get-constraint-matrix constraint-network)
            (create-constraint-matrix (get-alg-name constraint-network)
                                      obj-type))
      (setf (get-constrained-objects constraint-network)
            (list object))
      constraint-network)
    ;; OTHERWISE, CHECK THE INPUTS FOR VALIDITY AND THEN REGISTER THE OBJECT.
    (if (register-constrained-object-inputs-ok object obj-type constraint-network)
      (let* ((con-mat (get-constraint-matrix   constraint-network))
	     (objects (get-constrained-objects constraint-network)))
	(setf (get-constrained-objects constraint-network)
	      (append objects (list object)))
	(expand-network con-mat obj-type)
	constraint-network)
      constraint-network)))

(defun REGISTER-CONSTRAINED-OBJECTS (objects object-type constraint-network)
  "Register a list of constrained objects, all of the same object type, with
   a constraint network."
  (dolist (object objects constraint-network)
    (register-constrained-object object object-type constraint-network)))

;;;***********
;;; EXAMPLE 2:
;;;***********
;;;   The following form "tells" the constraint network, NET, that it
;;;   should make space for three interval objects.
;;; (register-constrained-objects '(:A :B :C) :interval NET)
;;; ==> #<Constraint-Network #XF9344B>

;;;----------------------------------------------------------------------
;;; ASSERT CONSTRAINTS INTO A CONSTRAINT NETWORK
;;;----------------------------------------------------------------------

;;; Except for the fact that the following function calls PROPAGATE,
;;; it could go in the file, 'network-support'.

(defun ASSERT-RELATION (row col relations net &optional (propagate-p t))
  "Assert RELATIONS, in the NETwork's array at ROW,COL and its
   inverse at COL,ROW.  Propagate if requested (the default).
   Returns the network and a string explaining how the operation
   went."
  (let* ((alg      (get-network-algebra net))
         (arr      (get-network-arr     net))
         (all-rels (get-algebra-rels alg))
         (abbrevs  (get-algebra-abbrevs alg))
         (rel      (if abbrevs
                     (abbreviate-relations relations all-rels abbrevs)
                     relations))
         (crel     (if (eql rel :?)	; Encode the relation
                     (get-algebra-zero alg)
                     (funcall (get-algebra-encoder alg) rel)))
         (crel12   (funcall		; Add crel to the element already there.
		    (get-algebra-adder alg) crel (aref arr row col))))
    (if (zerop crel12)			; REL intersected w/ existing elem is empty.
      (values
       nil
       "Constraint is immediately inconsistent with an existing constraint.")
      (progn
        (setf (aref arr row col) crel12)
        (setf (aref arr col row)
              (funcall (get-algebra-inverter alg)
                       (aref arr row col)))
        (if propagate-p
          (progn
            (setf net (propagate net))
            (if (consistentp net)
              (values
               net
               "Constraint has been successfully added and propagated in the network.")
              (values
               nil
               "Constraint causes network to propagate into an inconsistent state.")))
          (values
           net
           "Constraint has been added, but not propagated in the network."))
        ))))

;;; WARNING: The objects of a constraint must be registered with a constraint
;;; network before any constraints related to them can be asserted into the
;;; constraint network.

(defun ASSERT-CONSTRAINT (constraint constraint-network &key (propagate-p t))
  (let* (
	 ;; Take the constraint apart into its relation and objects.
	 (rel  (get-constraint-relation constraint))
         (obj1 (get-constraint-object1  constraint))
         (obj2 (get-constraint-object2  constraint))
	 ;;
	 ;; Get necessary items out of the constraint network
         (old  (get-constraint-matrix   constraint-network))
         (objs (get-constrained-objects constraint-network))
	 (test (get-obj-eq-test         constraint-network))
	 (key  (get-obj-id-key          constraint-network))
	 ;;
	 ;; Make a copy of the constraint network to work on.
         (new  (copy-network old))
	 ;;
	 ;; Find out where in the matrix the assertion needs to take place.
         (row  (position obj1 objs :test test :key key))
         (col  (position obj2 objs :test test :key key))
	 )
    (if (not (and row col))
      (values constraint-network nil
              "An object to be constrained is not registered.")
      (multiple-value-bind (result explanation)
	                   (assert-relation row col rel new propagate-p)
        (if result
          (progn
            (setf (get-constraint-matrix constraint-network) result)
            (values constraint-network t explanation))
          (values constraint-network nil explanation))))))

(defun ASSERT-CONSTRAINTS (constraints constraint-network &key (propagate-p t))
  (dolist (constraint constraints constraint-network)
    (assert-constraint constraint constraint-network :propagate-p propagate-p)))

;;;***********
;;; EXAMPLE 3:
;;;***********
;;;   The following form asserts two constraints into NET and propagates
;;;   the net.
;;; (assert-constraints (list '(:overlaps :A :B) '(:contains :B :C)) NET)
;;; ==> #<Constraint-Network #XF9344B>

(defun PROPAGATE-CONSTRAINTS (constraint-network)
  (setf (get-constraint-matrix constraint-network)
	(propagate (get-constraint-matrix constraint-network)))
  constraint-network)

;;;----------------------------------------------------------------------

(defmethod GET-CONSTRAINT ((net constraint-network) object1 object2)
  "Return the decoded constraint set that exists between OBJECT1 and
   OBJECT2 in NET.  Requesting the constraint for an
   object or objects that are not in NET is a
   continuable error."
  (let ((objects (get-constrained-objects net))
        (test    (get-obj-eq-test net))
        (key     (get-obj-id-key  net)))
    ;; Check to see if OBJECT1 and OBJECT2 are in the net.
    (assert (position object1 objects :key key :test test) (object1)
            "In the call to GET-CONSTRAINT~
             ~%the object, ~S, is not in~
             ~%the constraint network, ~S.~
             ~%If you choose to continue, enter a valid object."
            object1 net)
    (assert (position object2 objects :key key :test test) (object2)
            "In the call to GET-CONSTRAINT~
             ~%the object, ~S, is not in~
             ~%the constraint network, ~S.~
             ~%If you choose to continue, enter a valid object."
            object2 net)
    ;; OK, you passed the entrance requirement, now you can have your answer.
    (let* ((con-mat   (get-constraint-matrix net))
           (algebra   (get-network-algebra con-mat))
           (mat-array (get-network-arr     con-mat))
           (position1 (position object1 objects :key key :test test))
           (position2 (position object2 objects :key key :test test)))
      ;; Get the relation set (i.e., coded integer) in the constraint-matrix's
      ;; array that corresponds to the appropriate object positions (i.e., row
      ;; & column) and decode it.
      (funcall (get-algebra-decoder algebra)
               (aref mat-array position1 position2)))))

;;;***********
;;; EXAMPLE 4:
;;;***********
;;;   The following form returns the constraints between object :A and :C
;;; (get-constraint NET :A :C) ==> (:B :DI :FI :M :O)

(defmethod GET-CONSTRAINTS ((net constraint-network) &optional object1 object2)
  (cond
   ;; Get the constraint set between both objects:
   ((and object1 object2)
    (list (get-constraint net object1 object2) object1 object2))
   ;; Get the list constraint sets relative to OBJECT1:
   (object1
    (let ((constraints nil))
      (dolist (object (remove object1 (get-constrained-objects net)
                              :test (get-obj-eq-test net))
                      (reverse constraints))
        (unless (null object)
          (push (list (get-constraint net object1 object) object1 object)
                constraints)))))
   ;; Get the list of all constraint sets in the NET.
   (t
    (let ((constraints nil)
          (objects (get-constrained-objects net)))
      (dolist (obj1 objects (reverse constraints))
        (dolist (obj2 (rest (member obj1 objects)))
          (push (list (get-constraint net obj1 obj2) obj1 obj2)
                constraints)))))))

;;;----------------------------------------------------------------------
;;;                            END OF FILE
;;;----------------------------------------------------------------------
