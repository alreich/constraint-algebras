(in-package "CM")

(proclaim '(optimize (speed 3) (safety 1) (compilation-speed 0)))

(load "compile-file")

;;;----------------------------------------------------------------------

#+LUCID
(monitor
 ABBREV-RELATION			; tcn.lisp
 ABBREV-RELATIONS			; tcn.lisp
 ABBREVIATE-RELATIONS			; tcn.lisp
 ASSERT-CONSTRAINT			; tcn.lisp
 ASSERT-CONSTRAINTS			; tcn.lisp
 ASSERT-RELATION			; tcn.lisp
 CONSISTENTP				; network-support.lisp
 COPY-NETWORK				; network-support.lisp
 CREATE-CONSTRAINT-MATRIX		; network-support.lisp
 CREATE-NETWORK				; network-support.lisp
 DECODE-ARRAY-LIST			; algebra-support.lisp
 DEFINE-ALGEBRA				; algebra-support.lisp
 DEFINE-CUSTOM-ALGEBRA			; algebra-support.lisp
 DEFINE-DISCRETE-ALGEBRA		; algebra-support.lisp
 DELETE-ALGEBRA				; algebra-support.lisp
 DETERMINE-NETWORK-SIZE			; network-support.lisp
 DETERMINE-NETWORK-TYPE			; network-support.lisp
 ;; DUMP-ALGEBRA			; algebra-support.lisp
 DUMP-MULT-TABLE			; algebra-support.lisp
 ELEMENT-OF				; network-support.lisp
 ENCODE-ARRAY-LIST			; algebra-support.lisp
 EQUAL-ARRAYS-P				; propagator.lisp
 EQUAL-NETS-P				; network-support.lisp
 EXPAND-NETWORK				; network-support.lisp
 FIND-ALGEBRA				; algebra-support.lisp
 FLATTEN				; utilities.lisp
 GET-ALG-NAME				; tcn.lisp
 GET-ALGEBRA-ABBREVS			; algebra-support.lisp
 GET-ALGEBRA-ADDER			; algebra-support.lisp
 GET-ALGEBRA-DECODER			; algebra-support.lisp
 GET-ALGEBRA-DOCUMENTATION		; algebra-support.lisp
 GET-ALGEBRA-ELEMENT-TYPE		; algebra-support.lisp
 GET-ALGEBRA-ENCODER			; algebra-support.lisp
 GET-ALGEBRA-EQUAL-P			; algebra-support.lisp
 GET-ALGEBRA-EQUALITIES			; algebra-support.lisp
 GET-ALGEBRA-INV-RELS			; algebra-support.lisp
 GET-ALGEBRA-INVERTER			; algebra-support.lisp
 GET-ALGEBRA-LESS-THAN-P		; algebra-support.lisp
 GET-ALGEBRA-MULT-TABLE			; algebra-support.lisp
 GET-ALGEBRA-MULTIPLIER			; algebra-support.lisp
 GET-ALGEBRA-NAME			; algebra-support.lisp
 GET-ALGEBRA-NUM-RELS			; algebra-support.lisp
 GET-ALGEBRA-REL-CODING			; algebra-support.lisp
 GET-ALGEBRA-RELS			; algebra-support.lisp
 GET-ALGEBRA-TYPE			; algebra-support.lisp
 GET-ALGEBRA-ZERO			; algebra-support.lisp
 GET-CONSTRAINED-OBJECTS		; tcn.lisp
 GET-CONSTRAINT				; tcn.lisp
 GET-CONSTRAINT-MATRIX			; tcn.lisp
 GET-CONSTRAINT-OBJECT			; tcn.lisp
 GET-CONSTRAINT-OBJECT1			; tcn.lisp
 GET-CONSTRAINT-OBJECT2			; tcn.lisp
 GET-CONSTRAINT-RELATION		; tcn.lisp
 GET-CONSTRAINTS			; tcn.lisp
 GET-EQUALITY-ELEMENT			; algebra-support.lisp
 GET-NETWORK-ALGEBRA			; network-support.lisp
 GET-NETWORK-ARR			; network-support.lisp
 GET-OBJ-EQ-TEST			; tcn.lisp
 GET-OBJ-ID-KEY				; tcn.lisp
 INITIALIZE-CONSTRAINT-NETWORK		; tcn.lisp
 INV-MAPPING				; algebra-support.lisp
 INVERSE-NET-OF-P			; network-support.lisp
 LIST-ARRAY				; utilities.lisp
 LIST-ROW				; utilities.lisp
 MAKE-ADDER-FUNCTION			; algebra-support.lisp
 MAKE-CODED-RELS			; algebra-support.lisp
 MAKE-DECODER-FUNCTION			; algebra-support.lisp
 MAKE-ENCODER-FUNCTION			; algebra-support.lisp
 MAKE-INVERTER-FUNCTION			; algebra-support.lisp
 MAKE-LESS-THAN-P			; algebra-support.lisp
 MAKE-MULTIPLIER-FUNCTION		; algebra-support.lisp
 MAKE-REL-ASSOC-LIST			; algebra-support.lisp
 MAKE-TYPE-OF-ALGEBRA			; algebra-support.lisp
 MAKE-ZERO-ELEMENT			; algebra-support.lisp
 MAP-ALIST				; utilities.lisp
 MAPPEND				; utilities.lisp
 MKLIST					; utilities.lisp
 MULT					; algebra-support.lisp
 NREF					; network-support.lisp
 PRINT-ALGEBRA				; algebra-support.lisp
 PRINT-NETWORK				; network-support.lisp
 PROPAGATE				; propagator.lisp
 PROPAGATE-AUX				; propagator.lisp
 PROPAGATE-CONSTRAINTS			; tcn.lisp
 READ-NETWORK				; network-support.lisp
 REGISTER-ALGEBRA			; algebra-support.lisp
 REGISTER-CONSTRAINED-OBJECT		; tcn.lisp
 REGISTER-CONSTRAINED-OBJECT-INPUTS-OK	; tcn.lisp
 REGISTER-CONSTRAINED-OBJECTS		; tcn.lisp
 RELSYS-TO-CONALG			; algebra-support.lisp
 SKEW-OF-P				; network-support.lisp
 SQUARE-ARRAY				; propagator.lisp
 )

;;;----------------------------------------------------------------------

(start-monitoring)

(setf shop-net (setup-test-net *OBJECTS* *CONSTRAINTS*))

(stop-monitoring)

(with-open-file (stream "monitor.output" :direction :output)
  (print-monitors stream))

(with-open-file (stream "monitor.summary" :direction :output)
    (summarize-monitors :stream stream :number-of-calls t))

(reset-monitors)

;;;----------------------------------------------------------------------
;;;                        END OF FILE
;;;----------------------------------------------------------------------
