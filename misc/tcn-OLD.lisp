;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                   TEMPORAL CONSTRAINT NETWORKS (TCN)
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(in-package "CONSTRAINT-MAINTENANCE")

;;;----------------------------------------------------------------------

(defclass CONSTRAINT-NETWORK ()
  ((constraint-matrix
    :initarg :constraint-matrix
    :accessor get-constraint-matrix
    :documentation "A constraint-matrix structure")
   (objects
    :initarg :objects
    :accessor get-objects
    :documentation "A list of objects related in this TCN.")
   (object-eq-test
    :initarg :object-eq-test
    :initform #'eq
    :reader get-object-eq-test
    :documentation "Predicate to test the equality of TCN objects.")))

(defun MAKE-CONSTRAINT-NETWORK (&key constraints object-eq-test)
  )

;;;----------------------------------------------------------------------

(defun GET-CONSTRAINT (constraint-network &optional object1 object2)
  "Return the decoded constraint set that exists between OBJECT1 and
   OBJECT2 in CONSTRAINT-NETWORK.  Requesting the constraint for an
   object or objects that are not in CONSTRAINT-NETWORK is a
   continuable error."
  (let ((objects (get-objects constraint-network)))
    ;; Check to see if OBJECT1 and OBJECT2 are in the CONSTRAINT-NETWORK.
    (assert (position object1 objects) (object1)
            "In the call to GET-CONSTRAINT~
             ~%the object, ~S, is not in~
             ~%the constraint network, ~S.~
             ~%If you choose to continue, enter a valid object."
            object1 constraint-network)
    (assert (position object2 objects) (object2)
            "In the call to GET-CONSTRAINT~
             ~%the object, ~S, is not in~
             ~%the constraint network, ~S.~
             ~%If you choose to continue, enter a valid object."
            object2 constraint-network)
    ;; OK, you passed the entrance requirement, now you can have your answer.
    (let* ((con-mat   (get-constraint-matrix constraint-network))
           (algebra   (get-network-algebra con-mat))
           (mat-array (get-network-arr     con-mat))
           (position1 (position object1 objects))
           (position2 (position object2 objects)))
      ;; Get the relation set (i.e., coded integer) in the constraint-matrix's
      ;; array that corresponds to the appropriate object positions (i.e., row
      ;; & column) and decode it.
      (funcall (get-algebra-decoder algebra)
               (aref mat-array position1 position2)))))

;;;----------------------------------------------------------------------

;;; CONSTRAINTS:
;;; 
;;; The following abstractions for taking constraints apart assume that
;;; constraints are lists of the following form:
;;; 
;;;     ( <RELATION>  <OBJECT-1>  <OBJECT-2>  ...  )

(defmacro GET-CONSTRAINT-RELATION (constraint)
  `(first ,constraint))

(defmacro GET-CONSTRAINT-OBJECT (constraint object-position)
  `(nth ,object-position ,constraint))   ; Object positions start at 1

;;; Most constraints are binary (i.e., only two objects)

(defmacro GET-CONSTRAINT-OBJECT1 (constraint)
  `(second ,constraint))

(defmacro GET-CONSTRAINT-OBJECT2 (constraint)
  `(third ,constraint))

;;;----------------------------------------------------------------------

(defun ADD-OBJECT (object constraint-network)
  (setf (get-objects constraint-network)
        (append (get-objects constraint-network) (list object))))

(defun EXPAND-CONSTRAINT-MATRIX (constraint-matrix
                                 &key (expansion-increment 1) (copy-p t))
  (let* ((con-mat    (if copy-p
                       (copy-network constraint-matrix)
                       constraint-matrix))
         (mat-array  (get-network-arr     con-mat))
         (algebra    (get-network-algebra con-mat))
         (dimensions (array-dimensions    mat-array))
         (zero-elem  (get-algebra-zero    algebra)))
    (adjust-array mat-array
                  (mapcar #'(lambda (dim)
                              (+ dim expansion-increment))
                          dimensions)
                  :element-type (array-element-type mat-array)
                  :initial-element zero-elem)
    con-mat))

#|
(defun ADD-TO-CONSTRAINT-MATRIX (relations constraint-matrix row col)
  "RELATIONS can be a relation or list of relations in the algebra of 
   constraint-matrix. It is intersected with the relation set already
   at the position dictated by ROW and COL.  Then the constraint-matrix
   is propagated.  If the new relation(s) make the constraint-matrix
   inconsistent, then NIL is returned."
  (let* ((mat-array  (get-network-arr     constraint-matrix))
         (algebra    (get-network-algebra constraint-matrix))
         (dimensions (array-dimensions    mat-array))
         (encoder    (get-algebra-encoder algebra)))
    (case
      )))

;;(ADD-TO-CONSTRAINT-MATRIX OBJECT1 OBJECT2 OBJECTS CREL CON-MAT)
;; Get con-mat indices corresponding to object1 & object2 (two sets)
;; 'add' crel to the element at appropriate indices in con-mat (both places)
;; If 'sum' is 'zero',
;;    then return (values con-mat nil)
;;    else
;;       new-con-mat <-- propagate con-mat
;;       If new-con-mat is inconsistent,
;;          then return (values con-mat nil)
;;          else return (values new-con-mat t)
(defun ADD-TO-CONSTRAINT-MATRIX (object1 object2 objects crel con-mat)
  (let* ((pos1     (position object1 objects))
         (pos2     (position object2 objects))
         (alg      (get-network-algebra  con-mat))
         (adder    (get-algebra-adder    alg))
         (inverter (get-algebra-inverter alg))
         (arr      (get-network-arr con-mat))
         (crel12   (funcall adder crel (aref arr pos1 pos2))))
    (if (zerop crel12)
      (values con-mat nil)
      (let* ((con-mat-cpy (copy-network    con-mat))
             (arr         (get-network-arr con-mat-cpy))
             (new-con-mat nil))
        (setf (aref arr pos1 pos2) crel12)
        (setf (aref arr pos2 pos1)
              (funcall inverter (aref arr pos1 pos2)))
        (setf new-con-mat (propagate new-con-mat))
        (if (consistentp new-con-mat)
          (values new-con-mat t)
          (values con-mat nil))))))

(ADD-TO-CONSTRAINT-NETWORK CONSTRAINT CONSTRAINT-NETWORK)
;; Get the constraint network's constraint-matrix
;; Get the constraint network's objects
;; crel <-- Get the constraint's relation and encode it
;; Get the constraint's object1
;; Get the constraint's object2
;; CASE:
;;  (1) both object1 and object2 are in objects
;;       con-mat <-- copy of constraint-matrix
;;       (ADD-TO-CONSTRAINT-MATRIX OBJECT1 OBJECT2 OBJECTS CREL CON-MAT)
;;  (2) object1 is in objects, object2 is not
;;       matrix <-- expand constraint-matrix by one size
;;       objs <-- add object2 to objects
;;       (ADD-TO-CONSTRAINT-MATRIX OBJECT1 OBJECT2 OBJS CREL CON-MAT)
;;  (3) object2 is in objects, object1 is not
;;       matrix <-- expand constraint-matrix by one size
;;       objs <-- add object1 to objects
;;       (ADD-TO-CONSTRAINT-MATRIX OBJECT1 OBJECT2 OBJS CREL CON-MAT)
;;  (4) neither object1 nor object2 are in objects
;;       matrix <-- expand constraint-matrix by two sizes
;;       objs <-- add object1 & object2 to objects
;;       (ADD-TO-CONSTRAINT-MATRIX OBJECT1 OBJECT2 OBJS CREL CON-MAT)


(defun ADD-TO-CONSTRAINT-NETWORK (constraint constraint-network)
  (let* ((relation  (first  constraint))
         (object1   (second constraint))
         (object2   (third  constraint))
         (constraint-matrix   (get-constraint-matrix constraint-network))
         (objects   (get-objects constraint-network))
         (position1 (position object1 objects))
         (position2 (position object2 objects)))

|#
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

