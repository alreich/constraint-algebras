;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

(in-package "CONSTRAINT-MAINTENANCE")

(defvar *CM-FUNCTION-NAMES* nil)

(setf *CM-FUNCTION-NAMES*
      ;;
      ;; utilities.lisp
      ;;
      '(
	;; doloop (???)
	list-array
	list-row
	map-alist
	;;
	;; algebra-support.lisp
	;;
	get-algebra-name
	get-algebra-documentation
	get-algebra-type
	get-algebra-element-type
	get-algebra-rels
	get-algebra-rel-coding
	get-algebra-inv-rels
	get-algebra-equal-p
	get-algebra-less-than-p
	get-algebra-num-rels
	get-algebra-zero
	get-algebra-equalities
	get-algebra-encoder
	get-algebra-decoder
	get-algebra-mult-table
	get-algebra-multiplier
	get-algebra-adder
	get-algebra-inverter
	print-algebra
	find-algebra
	delete-algebra
	register-algebra
	get-equality-element
	make-type-of-algebra
	make-coded-rels
	make-zero-element
	make-rel-assoc-list
	make-less-than-p
	make-encoder-function
	make-decoder-function
	encode-array-list
	decode-array-list
	make-multiplier-function
	make-adder-function
	inv-mapping
	make-inverter-function
	define-custom-algebra
	define-discrete-algebra
	define-algebra
	dump-algebra
	dump-mult-table
	mult
	relsys-to-conalg
	;;
	;; network-support.lisp
	;;
	get-network-algebra
	get-network-arr
	nref
	element-of
	determine-network-type
	determine-network-size
	print-network
	create-network
	read-network
	copy-network
	equal-nets-p
	skew-of-p
	inverse-net-of-p
	consistentp
	expand-network
	;;
	;; propagator.lisp
	;;
	square-array
	equal-arrays-p
	propagate-aux
	propagate
	;;
	;; output-utilities.lisp
	;;
	dump-mult-table
	show-table
	write-terms
	write-terms
	write-prolog-list
	find-abbrev
	put-abbrev
	make-abbrev
	make-inv-abbrev
	decode-abbrevs
	write-table-2
	setup-reused-abbrevs
	table-info
	print-out-table
	print-out-table
	write-algebra-table
	sort-elem-defs
	write-elem-defs
	write-element-nets
	find-mult-asymmetries
	;;
	;; tcn.lisp
	;;
	create-constraint-matrix
	get-constraint-matrix
	get-constrained-objects
	get-alg-name
	get-obj-eq-test
	get-obj-id-key
	create-constraint-network
	create-constraint-network
	get-constraint-relation
	get-constraint-object
	get-constraint-object1
	get-constraint-object2
	register-constrained-object-inputs-ok
	register-constrained-object
	register-constrained-objects
	assert-relation
	assert-constraint
	assert-constraints
	propagate-constraints
	get-constraint
	))

#|

(SMS::With-Function-Call-Count
    (list
     assert-constraint
     assert-constraints
     assert-relation
     consistentp
     copy-network
     create-constraint-matrix
     create-constraint-network
     create-constraint-network
     create-network
     decode-abbrevs
     decode-array-list
     define-algebra
     define-custom-algebra
     define-discrete-algebra
     delete-algebra
     determine-network-size
     determine-network-type
     dump-mult-table
     dump-mult-table
     element-of
     encode-array-list
     equal-arrays-p
     equal-nets-p
     expand-network
     find-abbrev
     find-algebra
     find-mult-asymmetries
     get-alg-name
     get-algebra-adder
     get-algebra-decoder
     get-algebra-documentation
     get-algebra-element-type
     get-algebra-encoder
     get-algebra-equal-p
     get-algebra-equalities
     get-algebra-inv-rels
     get-algebra-inverter
     get-algebra-less-than-p
     get-algebra-mult-table
     get-algebra-multiplier
     get-algebra-name
     get-algebra-num-rels
     get-algebra-rel-coding
     get-algebra-rels
     get-algebra-type
     get-algebra-zero
     get-constrained-objects
     get-constraint
     get-constraint-matrix
     get-constraint-object
     get-constraint-object1
     get-constraint-object2
     get-constraint-relation
     get-equality-element
     get-network-algebra
     get-network-arr
     get-obj-eq-test
     get-obj-id-key
     inv-mapping
     inverse-net-of-p
     list-array
     list-row
     make-abbrev
     make-adder-function
     make-coded-rels
     make-decoder-function
     make-encoder-function
     make-inv-abbrev
     make-inverter-function
     make-less-than-p
     make-multiplier-function
     make-rel-assoc-list
     make-type-of-algebra
     make-zero-element
     map-alist
     mult
     nref
     print-algebra
     print-network
     print-out-table
     print-out-table
     propagate
     propagate-aux
     propagate-constraints
     put-abbrev
     read-network
     register-algebra
     register-constrained-object
     register-constrained-object-inputs-ok
     register-constrained-objects
     relsys-to-conalg
     setup-reused-abbrevs
     show-table
     skew-of-p
     sort-elem-defs
     square-array
     table-info
     write-algebra-table
     write-elem-defs
     write-element-nets
     write-prolog-list
     write-terms
     write-terms
     )
  (setf net2 (setup-test-net "I" *OBJECTS-2*
			     (append *INITIAL*
				     *STACK-A-B*
				     *STACK-B-C*
				     *DOMAIN-CONSTRAINTS-1*))))

(0 22 1 22 0 22 1 2 2 23
 0 0 0 0 0 0 0 0 0 0
 0 0 6 0 13 0 1 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 5 0 0 0 0 0 48 0 49
 0 22 22 22 13 0 0 22 22 0
 0 22 308 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 0 1 6 1 0 0 0 13 13
 1 0 0 0 0 0 6 0 0 0
 0 0 0 0)

(0 22 308 0 5 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 1 0 0 13 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 23 0 22 0 0 0 0
 13 6 6 6 1 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 1 49 48 0 22 22
 2 2 22 0 22 22 13 13 1 22
 22 1 1 0)

|#