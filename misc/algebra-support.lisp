;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                          ALGEBRA SUPPORT
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(in-package "CONSTRAINT-MAINTENANCE")

;;; The exports below are handled by the DEFPACKAGE form.
;;;(export '(define-algebra find-algebra delete-algebra dump-algebra))

(defvar *ALGEBRAS* nil
  "List of defined algebras. This is the only global variable in
this system.")

(defstruct (ALGEBRA
            (:conc-name get-algebra-)
            (:print-function print-algebra))
  "An algebra structure contains all the ingredients necessary to
the mathematical definition and use of a 'relation system' or
'constraint algebra' --- see my TIME-94 paper for definitions of
these.  The algebre is encoded in the form of closures that are
contained in this structure: multiplier, adder, inverter.  A zero
element and equality elements are also stored here.  A constraint
network is stored in an encoded form using the encoder and decoder
stored here.  For example there are algebras that correspond to
Allen's thirteen-element interval algebra and to my 26-element
branching time algebra --- again see the TIME-94 paper."
  NAME					; symbol or string
  DOCUMENTATION				; string
  TYPE					; keyword
  (ELEMENT-TYPE 'symbol)		; type specifier
  RELS					; list of keywords
  REL-CODING				; assoc list of dotted pairs
  INV-RELS				; list
  (EQUAL-P #'equal)			; closure
  LESS-THAN-P				; closure
  NUM-RELS				; integer
  ZERO					; integer
  EQUALITIES				; assoc list of dotted pairs
  (ENCODER #'identity)			; closure
  (DECODER #'identity)			; closure
  MULT-TABLE				; 2-D array of integers
  MULTIPLIER				; closure
  ADDER					; closure
  INVERTER				; closure
  ABBREVS                               ; assoc list of (not dotted)
  )

(defun PRINT-ALGEBRA (alg stream depth)
  "Provides a simple print representation for an algebra."
  (declare (ignore depth))
  (format stream "#<~A-ALGEBRA>" (get-algebra-name alg)))

(defun FIND-ALGEBRA (name)
  "Given a keyword name, return an algebra in the list *algebras* if
it exists, otherwise return NIL."
  (find name *algebras* :test #'equal :key #'get-algebra-name))

(defun DELETE-ALGEBRA (algebra)
  "Delete an algebra from the list *algebras*."
  (setf *algebras* (remove (get-algebra-name algebra) *algebras*
			   :test #'equal
			   :key  #'get-algebra-name)))

(defun REGISTER-ALGEBRA (algebra)
  "Install algebra in list of existing algebras.  Removes previous versions."
  (delete-algebra algebra)
  (push algebra *algebras*))

(defun GET-EQUALITY-ELEMENT (algebra type)
  "Return the encoded equality element of the given type for the
   given algebra."
  (cdr (assoc type (get-algebra-equalities algebra))))

;;;---------------------------------------------------------------------------

;;; This function is only used by the user interface code.

(defun ALGEBRA-NAMES ()
  "Returns a list of all registered algebra names."
  (mapcar #'(lambda (alg)
              (get-algebra-name alg))
          *algebras*))

;;;---------------------------------------------------------------------------

;;; UTILITIES FOR CREATING ALGEBRAS (DEFINE-ALGEBRA)

(defun MAKE-TYPE-OF-ALGEBRA (rel-sys-p)
  "If rel-sys-p is T, then returns :RELATION-SYSTEM,
otherwise it returns :CONSTRAINT-ALGEBRA."
  (if rel-sys-p :relation-system :constraint-algebra))

(defun MAKE-CODED-RELS (len rel-sys-p)
  "Returns a list of numbers to be used as the coded forms
for relations.  LEN is the number of elements in the algebra
and REL-SYS-P is T or NIL.  For example, (MAKE-CODED-RELS 3 t)
==> (1 2 4), and (MAKE-CODED-RELS 7 nil) ==> (0 1 2 3 4 5 6 7).
NOTE: The relation system {<, =, >} for points is equivalent to
the constraint algebra {<, =, <=, >, /=, >=, ?}. This constraint
algebra is the LINEAR-POINT algebra in *algebras*."
  (let ((crels nil))
    (if rel-sys-p
      (dotimes (k len (reverse crels))
        (push (expt 2 k) crels))
      (dotimes (k (1+ len) (reverse crels))
        (push k crels)))))

(defun MAKE-ZERO-ELEMENT (coded-rels rel-sys-p)
  "Given a list of coded relations, this returns the zero
element.  For example, (MAKE-ZERO-ELEMENT '(1 2 4) t) ==>
7, and (MAKE-ZERO-ELEMENT '(0 1 2 3 4 5 6 7) nil) ==> 7."
  (if rel-sys-p
    (apply #'+   coded-rels)
    (apply #'max coded-rels)))

(defun MAKE-REL-ASSOC-LIST (rels crels rel-sys-p)
  "Pairs up relation symbols, RELS, with their corresponding
coded forms, CRELS.  Returns an assoc list.  The assoc list
is eventually placed in the slot, REL-CODING in an algebra."
  (if rel-sys-p
    (pairlis rels crels)
    (pairlis (push nil rels) crels)))

(defun MAKE-LESS-THAN-P (rel-norms rels len)
  "Returns a closure that represents an 'less than' test for two symbolic
relations.  REL-NORMS can be NIL or a list of dotted pairs where each pair
consists of an algebraic symbol and a number that represents its order in
a linear order of all the symbols.  If REL-NORMS is NIL then this function
justs uses the ordering of the symbols in the list of relations RELS.  For
example, the LINEAR-POINT algebra rel-norms slot is NIL, consequently
(funcall (get-algebra-less-than-p (find-algebra 'linear-point)) :>= :?)
==> T.  That is, by default the symbols toward the front of the symbol
list are the 'smaller' ones."
  (if rel-norms
    #'(lambda (rel1 rel2)
        (< (second (assoc rel1 rel-norms))
           (second (assoc rel2 rel-norms))))
    (let* ((crels  nil)
           (norms  (dotimes (k len (reverse crels))
                     (push k crels)))
           (rnorms (pairlis rels norms)))
      #'(lambda (rel1 rel2)
          (< (cdr (assoc rel1 rnorms))
             (cdr (assoc rel2 rnorms)))))))

(defun MAKE-ENCODER-FUNCTION (relcode rel-sys-p)
  "This creates a closure of one variable, that takes an keyword algebraic
symbol and returns its coded form, i.e., an integer.  RELCODE is an assoc
list of symbols and their codes."
  (if rel-sys-p
    #'(lambda (rel)
        (if (symbolp rel)
          (if rel
            (cdr (assoc rel relcode))
            0)
          (let ((result 0))
            (dolist (r rel result)
              (setf result
                    (logior result
                            (cdr (assoc r relcode))))))))
    #'(lambda (rel)
        (cdr (assoc rel relcode)))))

(defun MAKE-DECODER-FUNCTION (relcode num-rels rel-sys-p)
  "This creates a closure of one variable, that takes the coded form of
an algebraic element, i.e., an integer, and returns its keyword algebraic
symbol.  RELCODE is an assoc list of symbols and their codes.  NUM-RELS
is the number of relations.  For example, (setf dec (get-algebra-decoder
(find-algebra 'linear-point))) ==> #<COMPILED-LEXICAL-CLOSURE #x54AB26>,
and (funcall dec 5) ==> :/= ."
  (if rel-sys-p
    #'(lambda (coded-rel)
        (let ((rels nil))
          (dotimes (val num-rels (if (cdr rels)
                                   (sort rels #'string<)
                                   (car rels)))
            (let ((v (expt 2 val)))
              (if (logtest v coded-rel)
                (push (car (rassoc v relcode))
                      rels))))))
    #'(lambda (coded-rel)
        (car (rassoc coded-rel relcode)))))

(defun ENCODE-ARRAY-LIST (array-list encoding-function)
  "Used when reading in an algebra to encode the algebra's multiplication
table into coded form."
  (mapcar #'(lambda (array-row)
	      (mapcar encoding-function array-row))
	  array-list))

(defun DECODE-ARRAY-LIST (array-list decoding-function)
  "This function is currently not used."
  (mapcar #'(lambda (array-row)
	      (mapcar decoding-function array-row))
	  array-list))

(defun MAKE-MULTIPLIER-FUNCTION (num-rels mult-table rel-sys-p)
  "Returns a closure that takes two relations in coded form and
returns their product according to the algebra."
  (if rel-sys-p
    #'(lambda (coded-rel-1 coded-rel-2)
        (let ((result1  0))
          (dotimes (i num-rels result1)
            (if (logtest (expt 2 i) coded-rel-1)
              (setf result1
                    (logior
                     result1
                     (let ((result2 0))
                       (dotimes (j num-rels result2)
                         (if (logtest (expt 2 j) coded-rel-2)
                           (setf result2
                                 (logior
                                  result2
                                  (aref mult-table i j))))))))))))
    #'(lambda (coded-rel-1 coded-rel-2)
        (if (zerop (* coded-rel-1 coded-rel-2))
          0
          (aref mult-table (1- coded-rel-1) (1- coded-rel-2))))))

(defun MAKE-ADDER-FUNCTION ()
  "Returns a closure that takes two relations in coded form and
returns their sum, according to the algebra, which is basically,
the set intersection."
  #'(lambda (coded-rel-1 coded-rel-2)
      (logand coded-rel-1 coded-rel-2)))

(defun INV-MAPPING (coded-rels coded-inv-rels rel-sys-p)
  "Returns an assoc list that associates a coded relation with
its corresponding inverse coded relation."
  (if rel-sys-p
    (pairlis coded-rels coded-inv-rels)
    (pairlis coded-rels (cons 0 coded-inv-rels))))

(defun MAKE-INVERTER-FUNCTION (coded-rels coded-inv-rels num-rels rel-sys-p)
  "Returns a closure that takes a coded relation and returns its inverse
in coded form."
  (let ((mapping (inv-mapping coded-rels coded-inv-rels rel-sys-p)))
    (if rel-sys-p
      #'(lambda (coded-rel)
          (let ((result1 0))
            (dotimes (i num-rels result1)
              (if (logtest (expt 2 i) coded-rel)
                (setf result1
                      (logior
                       result1
                       (cdr (assoc (expt 2 i) mapping))))))))
      #'(lambda (coded-rel)
          (cdr (assoc coded-rel mapping))))))

(defun DEFINE-CUSTOM-ALGEBRA (name documentation type element-type zero-element
				   multiplication-function addition-function
				   equal-p less-than-p inverse-function)
  "Returns a custom algebra.  In a custom algebra all closures must be
input to this function.  It does not automatically compute anything.  For
example, the duration algebra that is based on interval arithmetic is
created in this way."
  (let ((algebra (make-algebra
		  :name          name
		  :documentation documentation
		  :type          type
		  :element-type  element-type
		  :zero          zero-element
		  :equal-p       equal-p
		  :less-than-p   less-than-p
		  :multiplier    multiplication-function
		  :adder         addition-function
		  :inverter      inverse-function)))
    (register-algebra algebra)
    algebra))

(defun DEFINE-DISCRETE-ALGEBRA
       (name documentation relation-system-p equalities relation-list
	     multiplication-table inverse-relations relation-norms
             abbreviations)
  "Creates an algebra from basic information about it.  For example,
Allen's temporal algebra and Reich's right branching algebra are created
using this function."
  (let* ((num-rels    (length relation-list))
	 (crels       (make-coded-rels num-rels relation-system-p))
	 (zero        (make-zero-element crels relation-system-p))
	 (relcode     (make-rel-assoc-list relation-list crels relation-system-p))
	 (less-than-p (make-less-than-p relation-norms relation-list num-rels))
	 (encoder     (make-encoder-function relcode relation-system-p))
	 (decoder     (make-decoder-function relcode num-rels relation-system-p))
	 (mult-tbl    (make-array
		       (list num-rels num-rels)
		       :initial-contents
		       (if (numberp (caar multiplication-table))
                         multiplication-table
                         (encode-array-list
                          multiplication-table
                          encoder))))
	 (muliplier   (make-multiplier-function num-rels mult-tbl relation-system-p))
	 (adder       (make-adder-function))
	 (inverter    (make-inverter-function crels (mapcar encoder inverse-relations)
					      num-rels relation-system-p))
	 (type        (make-type-of-algebra relation-system-p))
	 (eq-alist    (map-alist encoder equalities))
	 (algebra     (make-algebra
		       :name          name
		       :documentation documentation
		       :type          type
		       :rels          relation-list
		       :rel-coding    relcode
		       :inv-rels      inverse-relations
		       :less-than-p   less-than-p
		       :num-rels      num-rels
		       :zero          zero
		       :equalities    eq-alist
		       :encoder       encoder
		       :decoder       decoder
		       :mult-table    mult-tbl
		       :multiplier    muliplier
		       :adder         adder
		       :inverter      inverter
                       :abbrevs       abbreviations)))
    (register-algebra algebra)
    algebra))

(defun DEFINE-ALGEBRA (name
		       &KEY documentation type element-type inverse-relations
		       relation-list multiplication-table relation-norms
		       equal-p less-than-p equalities
		       zero-element multiplication-function addition-function
		       inverse-function abbreviations)
  "Creates either a relation system or a constraint algebra from basic
information."
  (case type
    (:relation-system    (define-discrete-algebra
                           name documentation t   equalities relation-list
                           multiplication-table inverse-relations relation-norms
                           abbreviations))
    (:constraint-algebra (define-discrete-algebra
                           name documentation nil equalities relation-list
                           multiplication-table inverse-relations relation-norms
                           abbreviations))
    (otherwise           (define-custom-algebra
                           name documentation type element-type zero-element
                           multiplication-function addition-function
                           equal-p less-than-p inverse-function))))

;;;----------------------------------------------------------------------

;;;(defun DUMP-ALGEBRA (algebra
;;;		     &optional
;;;		     (coded-form-p t) (relation-system-p t) (destination t))
;;; )

;;;---------------------------------------------------------------------------
;;;                       TESTCASE FUNCTIONS
;;;---------------------------------------------------------------------------

;;; A USEFUL FUNCTION FOR DEBUGGING PURPOSES.

(defun MULT (rel1 rel2 algebra &optional (decodep t))
  "A debugging aid that lets one multiply two relations.  For example,
(mult :d :fi (find-algebra 'linear-interval)) ==> (:B :D :M :O :S)."
  (let* ((alg        (if (symbolp algebra)
                       (find-algebra algebra)
                       algebra))
	 (decoder    (get-algebra-decoder    alg))
	 (encoder    (get-algebra-encoder    alg))
	 (multiplier (get-algebra-multiplier alg))
	 (r1         (if (numberp rel1)
                       rel1
                       (funcall encoder rel1)))
	 (r2         (if (numberp rel2)
                       rel2
                       (funcall encoder rel2))))
    (if decodep
      (funcall decoder (funcall multiplier r1 r2))
      (funcall multiplier r1 r2))))

;;; A function that helps output an algebra's mult table in a
;;; different form for inclusion in a DEFINE-ALGEBRA form.

(defun RELSYS-TO-CONALG (relmap algebra)
  "Given a mapping (rel <--> crel) for a constraint algebra and the
   corresponding relation system, this function outputs the coded
   multiplication table for the constraint algebra. NOTE: MAKE SURE
   THAT RELMAP IS IN REVERSE ORDER, I.E., THE CRELS SHOULD BE IN
   DECENDING ORDER."
  (let ((table nil)
        (row   nil))
    (dolist (row-elem relmap table)
      (setf row nil)
      (dolist (col-elem relmap)
        (push (mult (cdr row-elem) (cdr col-elem) algebra nil)
              row))
      (push row table))))

;;;---------------------------------------------------------------------------
;;;                           END OF FILE
;;;---------------------------------------------------------------------------
