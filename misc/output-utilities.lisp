;;; -*- Package: CONSTRAINT-MAINTENANCE; Mode: LISP; Base: 10; Syntax: Common-Lisp -*-

(in-package "CONSTRAINT-MAINTENANCE")

;;;-----------------------------------------------------------------

;;; This code isn't complete yet.

(defun DUMP-MULT-TABLE (algebra coded-form-p relation-system-p
				destination)
  "Prints out an algebra's multiplication table.  The flag, CODED-FORM-P,
controls whether the printout is in coded form.  The flag, RELATION-SYSTEM-P
is not used yet."
  (declare (ignore relation-system-p))
  (let ((rels       (get-algebra-rels algebra))
	(multiplier (get-algebra-multiplier algebra))
	(encoder    (get-algebra-encoder algebra))
	(decoder    (get-algebra-decoder algebra)))
    (format destination "~%'(")
    (dolist (row rels)
      (format destination "~%(")
      (dolist (col rels)
	(let ((item (funcall multiplier
			     (funcall encoder row)
			     (funcall encoder col))))
	  (format destination " ~S "
		  (if coded-form-p
                    item
                    (funcall decoder item)))))
      (format destination ")"))
    (format destination ")")))

;;;-----------------------------------------------------------------

;;; The function, SHOW-TABLE, writes out a constraint algebra table
;;; in "raw" form.

(defun SHOW-TABLE (algebra &optional (destination t))
  (let ((rels       (get-algebra-rels algebra))
	(multiplier (get-algebra-multiplier algebra))
	(encoder    (get-algebra-encoder algebra))
	(decoder    (get-algebra-decoder algebra)))
    (dolist (row rels)
      (dolist (col rels)
	(format destination "~% ~S * ~S = ~S"
		row
		col
		(funcall decoder
			 (funcall multiplier
				  (funcall encoder row)
				  (funcall encoder col))))))))

;;;-----------------------------------------------------------------

;;; These functions support writing out a constraint algebra table
;;; in a form that is readable by PROLOG.
#|
(defun WRITE-TERMS (algebra &optional (destination t))
  (let ((rels       (get-algebra-rels algebra))
	(multiplier (get-algebra-multiplier algebra))
	(encoder    (get-algebra-encoder algebra))
	(decoder    (get-algebra-decoder algebra)))
    (dolist (row rels)
      (dolist (col rels)
	(let ((prod (funcall decoder
			     (funcall multiplier
				      (funcall encoder row)
				      (funcall encoder col)))))
	  (format destination "~%transitivity( ~S, ~S, ~A)."
		  row
		  col
		  (if (listp prod)
		      (write-prolog-list prod nil)
		      (write-prolog-list (list prod) nil))))))))
|#

(defun WRITE-TERMS (algebra &optional (destination t))
  (let ((rels       (get-algebra-rels algebra))
	(multiplier (get-algebra-multiplier algebra))
	(encoder    (get-algebra-encoder algebra))
	(decoder    (get-algebra-decoder algebra)))
    (dolist (row rels)
      (dolist (col rels)
	(let ((prod (funcall decoder
			     (funcall multiplier
				      (funcall encoder row)
				      (funcall encoder col)))))
	  (format destination "~%transitivity( ~S, ~S, " row col)
       	  (if (listp prod)
	      (write-prolog-list prod destination)
	      (write-prolog-list (list prod) destination))
	  (format destination ")."))))))

(defun WRITE-PROLOG-LIST (list &optional (destination t))
  (case (length list)
    (0 (format destination "[]"))
    (1 (format destination "[~S]" (car list)))
    (t (progn
	 (format destination "[")
	 (dolist (item (butlast list))
	   (format destination "~S," item))
	 (format destination "~S]" (first (last list)))))))

;;;-----------------------------------------------------------------

(defvar *COUNTER* 0)

#|
(defun WRITE-TABLE (algebra &key relations)
  "Write out a constraint algebra's multiplication table in a
   form that abbreviates all lists of relations contained in the
   table.  The abbreviations are also written out."
  (let ((rels       (if relations
			relations
			(get-algebra-rels algebra)))
	(multiplier (get-algebra-multiplier algebra))
	(encoder    (get-algebra-encoder    algebra))
	(decoder    (get-algebra-decoder    algebra))
	(inverter   (get-algebra-inverter   algebra))
	(table      nil)
	(abbrevs    nil))
    ;; RESET COUNTER FOR ABBREVIATIONS
    (setf *COUNTER* 0)
    ;; SELECT A ROW ELEMENT FROM ALL RELATIONS
    (dolist (row-elem rels)
      ;; INITIALIZE "ROW" TO THE EMPTY LIST
      (let ((row nil))
	;; SELECT A COLUMN ELEMENT FROM ALL RELATIONS
	(dolist (col-elem rels)
	  (let*
	      ;; COMPUTE THE PRODUCT OF THE ROW AND COLUMN
	      ;; ELEMENTS (LEAVE IT IN CODED FORM).
	      ((encoded-prod (funcall multiplier
				      (funcall encoder row-elem)
				      (funcall encoder col-elem)))
	       (decoded-prod (funcall decoder encoded-prod)))
	    (if
	     ;; IF THE PRODUCT IS NIL OR A SYMBOL...
	     (or (null decoded-prod) (symbolp decoded-prod))
	     ;; THEN DON'T ABBREVIATE IT --- SIMPLY PUSH IT
	     ;; ONTO THE ROW PRODUCTS BEING COLLECTED;
	     (push decoded-prod row)
	     ;; OTHERWISE, THE PRODUCT IS A LIST, SO, IF IT HASN'T
	     ;; ALREADY BEEN ABBREVIATED, THEN ABBREVIATE IT.
	     (let
		 ;; FIRST, CHECK TO SEE IF AN ABBREVIATION ALREADY
		 ;; EXISTS.
		 ((abbrev (find-abbrev encoded-prod abbrevs)))
	       (if abbrev
		   ;; IF AN ABBREV ALREADY EXISTS, THEN
		   ;; PUSH IT ONTO THE ROW OF PRODUCTS
		   ;; BEING COLLECTED.
		   (push abbrev row)
		   ;; OTHERWISE A NEW ABBREV IS NEEDED. CREATE
		   ;; IT AND IT'S INVERSE, PUSH BOTH ONTO THE
		   ;; ABBREVIATIONS LIST, AND PUSH THE NEW
		   ;; ABBREV ONTO THE ROW.
		   (let ((new-abbrev       (make-abbrev "Z"))
			 (encoded-prod-inv (funcall inverter encoded-prod)))
		     (setf abbrevs 
			   (put-abbrev encoded-prod new-abbrev abbrevs))
		     ;; IF PROD IS IT'S OWN INVERSE, DON'T
		     ;; PUSH IT ONTO THE ABBREVIATIONS LIST.
		     (if (/= encoded-prod encoded-prod-inv)
			 (setf abbrevs
			       (put-abbrev encoded-prod-inv
					   (make-inv-abbrev new-abbrev "V")
					   abbrevs)))
		     (push new-abbrev row)))))))
	(push (reverse row) table)))
    (list
     (list :RELATIONS            rels)
     (list :ABBREVIATIONS        (decode-abbrevs (reverse abbrevs) decoder))
     (list :MULTIPLICATION-TABLE (reverse table)))))
|#

(defun FIND-ABBREV (encoded-product abbreviations)
  "Find the abbreviation for a list of relations."
  (rest (assoc encoded-product abbreviations :test #'=)))

(defun PUT-ABBREV (encoded-product abbrev abbreviations)
  "Assert a new abbreviation for a list of relations."
  (acons encoded-product abbrev abbreviations))

(defun MAKE-ABBREV (prefix-string)
  (intern (concatenate 'string
		       prefix-string
		       (format nil "~S" (incf *COUNTER*)))
	  'keyword))

(defun MAKE-INV-ABBREV (abbrev postfix)
  (intern (concatenate 'string
		       (string abbrev)
		       postfix)
	  'keyword))

(defun DECODE-ABBREVS (abbrevs decoder)
  (mapcar #'(lambda (abbrev)
	      (list (rest abbrev)
		    (funcall decoder (first abbrev))))
	  abbrevs))
		     
;;;---------------------------------------------------------------------------

#|
(defun WRITE-TABLE-2 (algebra &key relations)
  "Write out a constraint algebra's multiplication table in a
   form that abbreviates all lists of relations contained in the
   table.  The abbreviations are also written out.  This version
   reuses abbreviations from Allen's period algebra."
  (let ((rels       (if relations
			relations
			(get-algebra-rels algebra)))
	(multiplier (get-algebra-multiplier algebra))
	(encoder    (get-algebra-encoder    algebra))
	(decoder    (get-algebra-decoder    algebra))
	(inverter   (get-algebra-inverter   algebra))
	(table      nil)
	(abbrevs    (reverse '((220661 . :Z1)
			       ( 18577 . :Z2)
			       (  5412 . :Z2V)
			       ( 70676 . :Z3)
			       (149537 . :Z3V)
			       (217584 . :Z4)
			       (147488 . :Z5)
			       ( 69648 . :Z5V)
			       (  4384 . :Z6)
			       ( 16528 . :Z6V)
			       (   448 . :Z7)
			       ( 18433 . :Z8)
			       (  5124 . :Z8V)
			       (196672 . :Z9)))))
    ;; RESET COUNTER FOR ABBREVIATIONS
    (setf *COUNTER* 9)
    ;; SELECT A ROW ELEMENT FROM ALL RELATIONS
    (dolist (row-elem rels)
      ;; INITIALIZE "ROW" TO THE EMPTY LIST
      (let ((row nil))
	;; SELECT A COLUMN ELEMENT FROM ALL RELATIONS
	(dolist (col-elem rels)
	  (let*
	      ;; COMPUTE THE PRODUCT OF THE ROW AND COLUMN
	      ;; ELEMENTS (LEAVE IT IN CODED FORM).
	      ((encoded-prod (funcall multiplier
				      (funcall encoder row-elem)
				      (funcall encoder col-elem)))
	       (decoded-prod (funcall decoder encoded-prod)))
	    (if
	     ;; IF THE PRODUCT IS NIL OR A SYMBOL...
	     (or (null decoded-prod) (symbolp decoded-prod))
	     ;; THEN DON'T ABBREVIATE IT --- SIMPLY PUSH IT
	     ;; ONTO THE ROW PRODUCTS BEING COLLECTED;
	     (push decoded-prod row)
	     ;; OTHERWISE, THE PRODUCT IS A LIST, SO, IF IT HASN'T
	     ;; ALREADY BEEN ABBREVIATED, THEN ABBREVIATE IT.
	     (let
		 ;; FIRST, CHECK TO SEE IF AN ABBREVIATION ALREADY
		 ;; EXISTS.
		 ((abbrev (find-abbrev encoded-prod abbrevs)))
	       (if abbrev
		   ;; IF AN ABBREV ALREADY EXISTS, THEN
		   ;; PUSH IT ONTO THE ROW OF PRODUCTS
		   ;; BEING COLLECTED.
		   (push abbrev row)
		   ;; OTHERWISE A NEW ABBREV IS NEEDED. CREATE
		   ;; IT AND IT'S INVERSE, PUSH BOTH ONTO THE
		   ;; ABBREVIATIONS LIST, AND PUSH THE NEW
		   ;; ABBREV ONTO THE ROW.
		   (let ((new-abbrev       (make-abbrev "Z"))
			 (encoded-prod-inv (funcall inverter encoded-prod)))
		     (setf abbrevs 
			   (put-abbrev encoded-prod new-abbrev abbrevs))
		     ;; IF PROD IS IT'S OWN INVERSE, DON'T
		     ;; PUSH IT ONTO THE ABBREVIATIONS LIST.
		     (if (/= encoded-prod encoded-prod-inv)
			 (setf abbrevs
			       (put-abbrev encoded-prod-inv
					   (make-inv-abbrev new-abbrev "V")
					   abbrevs)))
		     (push new-abbrev row)))))))
	(push (reverse row) table)))
    (list
     (list :RELATIONS            rels)
     (list :ABBREVIATIONS        (decode-abbrevs (reverse abbrevs) decoder))
     (list :MULTIPLICATION-TABLE (reverse table)))))
|#

#|
(setf *REUSED-ABBREVS*
      '(( Z1 . (:=B :=BI :=D :=DI :=E :=F :=FI :=M :=MI :=O :=OI :=S :=SI))
	( Z2 . (:=B :=D :=M :=O :=S))
	(Z2V . (:=BI :=DI :=MI :=OI :=SI))
	( Z3 . (:=BI :=D :=F :=MI :=OI))
	(Z3V . (:=B :=DI :=FI :=M :=O))
	( Z4 . (:=D :=DI :=E :=F :=FI :=O :=OI :=S :=SI))
	( Z5 . (:=DI :=OI :=SI))
	(Z5V . (:=D :=O :=S))
	( Z6 . (:=DI :=FI :=O))
	(Z6V . (:=D :=F :=OI))
	( Z7 . (:=E :=F :=FI))
	( Z8 . (:=BI :=MI :=OI))
	(Z8V . (:=B :=M :=O))
	( Z9 . (:=E :=S :=SI))))
|#

(defvar *REUSED-ABBREVS-I*
  '(( Ri . (:B :BI :D :DI :E :F :FI :M :MI :O :OI :S :SI))
    ( Z1 . (:B :D :M :O :S))
    (Z1V . (:BI :DI :MI :OI :SI))
    ( Z2 . (:BI :D :F :MI :OI))
    (Z2V . (:B :DI :FI :M :O))
    ( Z3 . (:D :DI :E :F :FI :O :OI :S :SI))
    ( Z4 . (:DI :OI :SI))
    (Z4V . (:D :O :S))
    ( Z5 . (:DI :FI :O))
    (Z5V . (:D :F :OI))
    ( Z6 . (:E :F :FI))
    ( Z7 . (:BI :MI :OI))
    (Z7V . (:B :M :O))
    ( Z8 . (:E :S :SI))))

(defvar *REUSED-ABBREVS-IP*
  '(( Ri . (:=B :=BI :=D :=DI :=E :=F :=FI :=M :=MI :=O :=OI :=S :=SI))
    (Rip . (:=B :=BI :=D :=DI :=E :=F :=FI :=M :=MI :=O :=OI :=PE :=PF
		:=PFI :=PS :=PSI :=S :=SI))
    ( Z1 . (:=B :=D :=M :=O :=S))
    (Z1V . (:=BI :=DI :=MI :=OI :=SI))
    ( Z2 . (:=BI :=D :=F :=MI :=OI))
    (Z2V . (:=B :=DI :=FI :=M :=O))
    ( Z3 . (:=D :=DI :=E :=F :=FI :=O :=OI :=S :=SI))
    ( Z4 . (:=DI :=OI :=SI))
    (Z4V . (:=D :=O :=S))
    ( Z5 . (:=DI :=FI :=O))
    (Z5V . (:=D :=F :=OI))
    ( Z6 . (:=E :=F :=FI))
    ( Z7 . (:=BI :=MI :=OI))
    (Z7V . (:=B :=M :=O))
    ( Z8 . (:=E :=S :=SI))))

(defvar *REUSED-ABBREVS-RB*
  '(( Ri . (:=<B :=<BI :=<D :=<DI :=<E :=<F :=<FI :=<M :=<MI :=<O :=<OI :=<S :=<SI))
    (Rip . (:=<B :=<BI :=<D :=<DI :=<E :=<F :=<FI :=<M :=<MI :=<O :=<OI :=<S :=<SI
		 :=<PE :=<PF :=<PFI :=<PS :=<PSI))
    (Rrb . (:=<B :=<BI :=<D :=<DI :=<E :=<F :=<FI :=<M :=<MI :=<O :=<OI :=<S :=<SI
		 :=<PE :=<PF :=<PFI :=<PS :=<PSI
		 :=<RB :=<RBI :=<RO :=<ROI :=<RS :=<R~))
    ( Z1 . (:=<B :=<D :=<M :=<O :=<S))
    (Z1V . (:=<BI :=<DI :=<MI :=<OI :=<SI))
    ( Z2 . (:=<BI :=<D :=<F :=<MI :=<OI))
    (Z2V . (:=<B :=<DI :=<FI :=<M :=<O))
    ( Z3 . (:=<D :=<DI :=<E :=<F :=<FI :=<O :=<OI :=<S :=<SI))
    ( Z4 . (:=<DI :=<OI :=<SI))
    (Z4V . (:=<D :=<O :=<S))
    ( Z5 . (:=<DI :=<FI :=<O))
    (Z5V . (:=<D :=<F :=<OI))
    ( Z6 . (:=<E :=<F :=<FI))
    ( Z7 . (:=<BI :=<MI :=<OI))
    (Z7V . (:=<B :=<M :=<O))
    ( Z8 . (:=<E :=<S :=<SI))
    ( Z9 . (:=<B :=<D :=<M :=<O :=<PS :=<S))
    (Z9V . (:=<BI :=<DI :=<MI :=<OI :=<PSI :=<SI))
    (Z10 . (:=<BI :=<D :=<F :=<MI :=<OI :=<PF))
    (Z10V . (:=<B :=<DI :=<FI :=<M :=<O :=<PFI))))

(defvar *REUSED-ABBREVS-LB*
  '(( RI . (:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=M :>=MI :>=O :>=OI :>=S :>=SI))
    (RIP . (:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=M :>=MI :>=O :>=OI :>=S :>=SI
		 :>=PE :>=PF :>=PFI :>=PS :>=PSI))
    (RLB . (:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=M :>=MI :>=O :>=OI :>=S :>=SI
		 :>=PE :>=PF :>=PFI :>=PS :>=PSI
		 :>=LB :>=LBI :>=LF :>=LO :>=LOI :>=L~))
    ( Z1 . (:>=B :>=D :>=M :>=O :>=S))
    (Z1V . (:>=BI :>=DI :>=MI :>=OI :>=SI))
    ( Z2 . (:>=BI :>=D :>=F :>=MI :>=OI))
    (Z2V . (:>=B :>=DI :>=FI :>=M :>=O))
    ( Z3 . (:>=D :>=DI :>=E :>=F :>=FI :>=O :>=OI :>=S :>=SI))
    ( Z4 . (:>=DI :>=OI :>=SI))
    (Z4V . (:>=D :>=O :>=S))
    ( Z5 . (:>=DI :>=FI :>=O))
    (Z5V . (:>=D :>=F :>=OI))
    ( Z6 . (:>=E :>=F :>=FI))
    ( Z7 . (:>=BI :>=MI :>=OI))
    (Z7V . (:>=B :>=M :>=O))
    ( Z8 . (:>=E :>=S :>=SI))
    ( Z9 . (:>=B :>=D :>=M :>=O :>=PS :>=S))
    (Z9V . (:>=BI :>=DI :>=MI :>=OI :>=PSI :>=SI))
    (Z10 . (:>=BI :>=D :>=F :>=MI :>=OI :>=PF))
    (Z10V . (:>=B :>=DI :>=FI :>=M :>=O :>=PFI))))

(defun SETUP-REUSED-ABBREVS (reuse-abbrevs encoder)
  (reverse (mapcar #'(lambda (abbrev)
		       (cons (funcall encoder (cdr abbrev))
			     (car abbrev)))
		   reuse-abbrevs)))

(defun TABLE-INFO (algebra &key relations reuse-abbrevs init-counter)
  "Write out a constraint algebra's multiplication table in a
   form that abbreviates all lists of relations contained in the
   table.  The abbreviations are also written out.  This version
   reuses abbreviations from Allen's period algebra."
  (let* ((rels       (if relations
			 relations
			 (get-algebra-rels algebra)))
	 (multiplier (get-algebra-multiplier algebra))
	 (encoder    (get-algebra-encoder    algebra))
	 (decoder    (get-algebra-decoder    algebra))
	 (inverter   (get-algebra-inverter   algebra))
	 (table      nil)
	 (abbrevs    (if reuse-abbrevs
			 (setup-reused-abbrevs reuse-abbrevs encoder))))
    ;; RESET COUNTER FOR ABBREVIATIONS
    (setf *COUNTER* init-counter)
    ;; SELECT A ROW ELEMENT FROM ALL RELATIONS
    (dolist (row-elem rels)
      ;; INITIALIZE "ROW" TO THE EMPTY LIST
      (let ((row nil))
	;; SELECT A COLUMN ELEMENT FROM ALL RELATIONS
	(dolist (col-elem rels)
	  (let*
	      ;; COMPUTE THE PRODUCT OF THE ROW AND COLUMN
	      ;; ELEMENTS (LEAVE IT IN CODED FORM).
	      ((encoded-prod (funcall multiplier
				      (funcall encoder row-elem)
				      (funcall encoder col-elem)))
	       (decoded-prod (funcall decoder encoded-prod)))
	    (if
	     ;; IF THE PRODUCT IS NIL OR A SYMBOL...
	     (or (null decoded-prod) (symbolp decoded-prod))
	     ;; THEN DON'T ABBREVIATE IT --- SIMPLY PUSH IT
	     ;; ONTO THE ROW PRODUCTS BEING COLLECTED;
	     (push decoded-prod row)
	     ;; OTHERWISE, THE PRODUCT IS A LIST, SO, IF IT HASN'T
	     ;; ALREADY BEEN ABBREVIATED, THEN ABBREVIATE IT.
	     (let
		 ;; FIRST, CHECK TO SEE IF AN ABBREVIATION ALREADY
		 ;; EXISTS.
		 ((abbrev (find-abbrev encoded-prod abbrevs)))
	       (if abbrev
		   ;; IF AN ABBREV ALREADY EXISTS, THEN
		   ;; PUSH IT ONTO THE ROW OF PRODUCTS
		   ;; BEING COLLECTED.
		   (push abbrev row)
		   ;; OTHERWISE A NEW ABBREV IS NEEDED. CREATE
		   ;; IT AND IT'S INVERSE, PUSH BOTH ONTO THE
		   ;; ABBREVIATIONS LIST, AND PUSH THE NEW
		   ;; ABBREV ONTO THE ROW.
		   (let ((new-abbrev       (make-abbrev "Z"))
			 (encoded-prod-inv (funcall inverter encoded-prod)))
		     (setf abbrevs 
			   (put-abbrev encoded-prod new-abbrev abbrevs))
		     ;; IF PROD IS IT'S OWN INVERSE, DON'T
		     ;; PUSH IT ONTO THE ABBREVIATIONS LIST.
		     (if (/= encoded-prod encoded-prod-inv)
			 (setf abbrevs
			       (put-abbrev encoded-prod-inv
					   (make-inv-abbrev new-abbrev "V")
					   abbrevs)))
		     (push new-abbrev row)))))))
	(push (reverse row) table)))
    (list
     (list :RELATIONS            rels)
     (list :ABBREVIATIONS        (decode-abbrevs (reverse abbrevs) decoder))
     (list :MULTIPLICATION-TABLE (reverse table)))))

#|
(defun PRINT-OUT-TABLE (table-info)
  (let ((rels    (second (assoc :relations            table-info)))
	(abbrevs (second (assoc :abbreviations        table-info)))
	(table   (second (assoc :multiplication-table table-info))))
    ;; PRINT OUT RELATIONS
    (format t "~2% RELATIONS:")
    (format t "~%        R = ~A" rels)
    ;; PRINT OUT ABBREVIATIONS
    (format t "~2% ABBREVIATIONS:")
    (dolist (abbrev abbrevs)
      (format t "~%   ~6@A = ~A" (first abbrev) (second abbrev)))
    ;; PRINT OUT MULTIPLICATION TABLE:
    (format t "~2% MULTIPLICATION TABLE:")
    ;; PRINT COLUMN HEADERS
    (format t "~%         ")		; Start on a new line.
    (dolist (rel rels) (format t "~6@A" rel))
    (format t "~%       +-")
    (dotimes (i (length rels)) (format t "------"))
    ;; PRINT THE ROWS
    (dotimes (i (1- (length rels)))
      (let ((rowi (nth i table)))
	;; PRINT I_th ROW HEADER
 	(format t "~%~6@A | " (nth i rels))
	;; PRINT THE REST OF THE I_th ROW
	(dolist (entry rowi) (format t "~6@A" entry))))))
|#

(defun PRINT-OUT-TABLE (table-info)
  (let ((rels    (second (assoc :relations            table-info)))
	(abbrevs (second (assoc :abbreviations        table-info)))
	(table   (second (assoc :multiplication-table table-info))))
    ;; PRINT OUT RELATIONS
    (format t "~2% RELATIONS:")
    (format t "~%        R = ~A" rels)
    ;; PRINT OUT ABBREVIATIONS
    (format t "~2% ABBREVIATIONS:")
    (dolist (abbrev abbrevs)
      (format t "~%   ~6@A = ~A" (first abbrev) (second abbrev)))
    ;; PRINT OUT MULTIPLICATION TABLE:
    (format t "~2% MULTIPLICATION TABLE:")
    ;; PRINT COLUMN HEADERS
    (format t "~%         ")		; Start on a new line.
    (dolist (rel rels) (format t "~6@A" rel))
    (format t "~%       +-")
    (dotimes (i (length rels)) (format t "------"))
    ;; PRINT THE ROWS
    (dotimes (i (length rels))
      (let ((rowi (nth i table)))
	;; PRINT I_th ROW HEADER
 	(format t "~%~6@A | " (nth i rels))
	;; PRINT THE REST OF THE I_th ROW
	(dolist (entry rowi) (format t "~6@A" entry))))))

#|
;;; An example call to WRITE-ALGEBRA-TABLE¬:

(write-algebra-table (find-algebra 'LEFT-BRANCHING-PERIOD-&-POINT)
                     :relations '(:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=M :>=MI :>=O :>=OI :>=S :>=SI :>=PE :>=PF :>=PFI :>=PS :>=PSI :>=LB :>=LBI :>=LO :>=LOI :>=LF :>=L~)
                     :reuse-abbrevs *REUSED-ABBREVS-LB*
                     :init-counter 10)
|#

(defun WRITE-ALGEBRA-TABLE (algebra &key relations reuse-abbrevs init-counter)
  (print-out-table (table-info algebra
			       :relations relations
			       :reuse-abbrevs reuse-abbrevs
			       :init-counter init-counter)))

;;;----------------------------------------------------------------------

(defun SORT-ELEM-DEFS (elem-defs)
  "Rearrange the element definitions in ELEM-DEFS so that they are in the
   order of each elements symbol."
  (setf elem-defs (sort elem-defs #'string< :key #'car)))

(defun WRITE-ELEM-DEFS (elem-defs)
  "Write out a list of element definitions as given by lists such
   as *ALLEN-PT-NETS-WITHOUT-EQUALS*.  This just prints out the
   upper right 2x2 partition of each elements point net."
  (dolist (elem-def elem-defs)
    (if (listp elem-def)
      (let* ((elem-name  (car elem-def))
             (pt-network (cdr elem-def))
             (net-arr    (get-network-arr pt-network))
             (net-alg    (get-network-algebra pt-network))
             (decoder    (get-algebra-decoder net-alg)))
        (format t "~%~6@A   <-- ~6@A ~6@A ~6@A ~6@A"
                elem-name
                (funcall decoder (aref net-arr 0 2))
                (funcall decoder (aref net-arr 0 3))
                (funcall decoder (aref net-arr 1 2))
                (funcall decoder (aref net-arr 1 3))))
      (let* ((pt-network elem-def)
             (net-arr    (get-network-arr pt-network))
             (net-alg    (get-network-algebra pt-network))
             (decoder    (get-algebra-decoder net-alg)))
        (format t "~%~6@A   <-- ~6@A ~6@A ~6@A ~6@A"
                ""
                (funcall decoder (aref net-arr 0 2))
                (funcall decoder (aref net-arr 0 3))
                (funcall decoder (aref net-arr 1 2))
                (funcall decoder (aref net-arr 1 3)))))))

(defun WRITE-ELEMENT-NETS (elem-nets)
  "Write out a list of sub-matrices (networks) extracted from
   a list of element networks (e.g., *LINEAR-INTERVALS*)"
  (dolist (elem-net elem-nets)
    (if (listp elem-net)
      (let* ((elem-name  (car elem-net))
             (pt-network (cdr elem-net))
             (net-arr    (get-network-arr pt-network))
             (net-alg    (get-network-algebra pt-network))
             (decoder    (get-algebra-decoder net-alg)))
        (format t "~%(~S . #N((~S ~S) (~S ~S)))"
                elem-name
                (funcall decoder (aref net-arr 0 2))
                (funcall decoder (aref net-arr 0 3))
                (funcall decoder (aref net-arr 1 2))
                (funcall decoder (aref net-arr 1 3))))
      (let* ((pt-network elem-net)
             (net-arr    (get-network-arr pt-network))
             (net-alg    (get-network-algebra pt-network))
             (decoder    (get-algebra-decoder net-alg)))
        (format t "~%(~S . #N((~S ~S) (~S ~S)))"
                :???
                (funcall decoder (aref net-arr 0 2))
                (funcall decoder (aref net-arr 0 3))
                (funcall decoder (aref net-arr 1 2))
                (funcall decoder (aref net-arr 1 3)))))))

;;;----------------------------------------------------------------------

(defun FIND-MULT-ASYMMETRIES (algebra)
  (let* ((alg        (if (symbolp algebra)
                       (find-algebra algebra)
                       algebra))
	 (decoder    (get-algebra-decoder    alg))
	 (encoder    (get-algebra-encoder    alg))
	 (inverter   (get-algebra-inverter   alg))
	 (multiplier (get-algebra-multiplier alg))
         (relations  (get-algebra-rels       alg))
         (asymmetry  nil))
    (format t "~%ALGEBRA --- ~S." (get-algebra-name algebra))
    (format t "~% (~S)." (get-algebra-documentation algebra))
    (dolist (rel1 relations)
      (dolist (rel2 relations)
        (let* ((crel1    (funcall encoder rel1))
               (crel2    (funcall encoder rel2))
               (cinvrel1 (funcall inverter crel1))
               (cinvrel2 (funcall inverter crel2))
               (product1 (funcall multiplier crel1 crel2))
               (product2 (funcall inverter
                                  (funcall multiplier cinvrel2 cinvrel1))))
          ;;(format t "~%***DEBUG: ~S * ~S   = ~S"
          ;;        rel1 rel2 (funcall decoder product1))
          ;;(format t "~%         (~S * ~S)^ = ~S"
          ;;        (funcall decoder cinvrel2)
          ;;        (funcall decoder cinvrel1)
          ;;        (funcall decoder product2))
          (if (/= product1 product2)
            (progn
              (setf asymmetry t)
              (format t "~% Asymmetry:")
              (format t "~%    ~S * ~S = ~S"
                      rel1 rel2 (funcall decoder product1))
              (format t "~%    ~S * ~S = ~S"
                      (funcall decoder cinvrel2)
                      (funcall decoder cinvrel1)
                      (funcall decoder product2)))))))
    (if (not asymmetry) (format t "~% No asymmetries found."))
    (format t "~% DONE.")))

;;;-----------------------------------------------------------------

;;; These functions support writing out a transitivity table
;;; in ASCII form.

(defun WRITE-TRANSITIVITY-TABLE (algebra &optional (destination t))
  (let ((rels       (sort (get-algebra-rels algebra) #'string<))
	(multiplier (get-algebra-multiplier algebra))
	(encoder    (get-algebra-encoder algebra))
	(decoder    (get-algebra-decoder algebra)))
    (dolist (row rels)
      (dolist (col rels)
	(let ((prod (funcall decoder
			     (funcall multiplier
				      (funcall encoder row)
				      (funcall encoder col)))))
	  (format destination "~%~10,,,S     ~10,,,S     " row col)
       	  (if (listp prod)
	      (write-lisp-list prod destination)
	      (write-lisp-list (list prod) destination))
	  (format destination ")."))))))

(defun WRITE-LISP-LIST (list &optional (destination t))
  (case (length list)
    (0 (format destination "()"))
    (1 (format destination "(~S)" (car list)))
    (t (progn
	 (format destination "(")
	 (dolist (item (butlast list))
	   (format destination "~S," item))
	 (format destination "~S)" (first (last list)))))))

;;;-----------------------------------------------------------------

(sort (get-algebra-rels (find-algebra 'RIGHT-BRANCHING-INTERVAL-&-POINT)) #'string<)