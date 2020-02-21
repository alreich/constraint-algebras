;;;; BACKTRACE LOGGING (see Lucid CL "Delivery Toolkit", page 2-4)

(in-package "CM")

(proclaim '(optimize (speed 2) (safety 1) (compilation-speed 0)))

(load "compile-file")

;;;----------------------------------------------------------------------

#|

(setf *print-length* 8)

(start-backtrace-logging "tests-RaP.log")

(time (setf net (setup-test-net *OBJECTS-2*
				(append *INITIAL*
					*STACK-A-B*
					*STACK-B-C*
					*DOMAIN-CONSTRAINTS-1*))))

Elapsed Real Time = 31.28 seconds
Total Run Time    = 31.18 seconds
User Run Time     = 31.14 seconds
System Run Time   = 0.04 seconds
Dynamic Bytes Consed   =          0
Ephemeral Bytes Consed =    127,984
#<Constraint-Network #XF879A3>

(stop-backtrace-logging)

(summarize-backtrace-logging "tests-RaP.log"
			     :root 'assert-constraint
			     )



 Call-tree information obtained from a backtrace log. 
 The numbers at each node represent the % of backtraces 
 that include this node.  There are 25 backtraces total.

   1: ASSERT-CONSTRAINT 100.000 
     2: ASSERT-RELATION 12.000 
       3: (:INTERNAL MAKE-ADDER-FUNCTION 0) 4.000 
         4: LOGAND 4.000 
       3: ABBREVIATE-RELATIONS 8.000 
         4: ABBREV-RELATIONS 8.000 
           5: ABBREV-RELATION 8.000 
             6: ASSOC 8.000 
     2: COPY-NETWORK 88.000 
       3: LIST-ARRAY 20.000 
         4: LIST-ROW 16.000 
           5: LUCID-RUNTIME-SUPPORT:2DIM-AREF-SUBR 4.000 
           5: LIST-ROW 12.000 
         4: LIST-REVERSE 4.000 
       3: CREATE-NETWORK 68.000 
         4: MAKE-ARRAY 68.000 
           5: LUCID::MAKE-DIMENSIONS-VECTOR 4.000 
             6: GETHASH 4.000 
               7: SYSTEM:SXHASH-EQUAL 4.000 
                 8: LUCID::SXHASH-LIST 4.000 
                   9: LUCID::SXHASH-LIST 4.000 
           5: LUCID::MAKE-ARRAY-INITIAL-CONTENTS-CHECK 20.000 
             6: LUCID::INITIAL-CONTENTS-MISMATCH-P 20.000 
               7: ELT 4.000 
                 8: NTHCDR 4.000 
               7: LUCID::INITIAL-CONTENTS-MISMATCH-P 16.000 
                 8: ELT 8.000 
                   9: NTHCDR 8.000 
           5: LUCID::INSTALL-INITIAL-CONTENTS 12.000 
             6: LUCID::INSTALL-AUX 12.000 
               7: LUCID::INSTALL-AUX 12.000 
                 8: LUCID-RUNTIME-SUPPORT:SET-SVREF 4.000 
           5: LUCID::ARRAY-IMPLEMENTATION-TYPE-NUMBER 32.000 
             6: LUCID::ARRAY-IMPLEMENTATION-TYPE-NUMBER 32.000 
               7: LUCID::ARRAY-RESORT-TO-SUBTYPEP 32.000 
                 8: LUCID::NORMALIZE-LIST-TYPE 4.000 
                   9: LUCID::LISP-SIGNAL-HANDLER 4.000 
                    10: (:INTERNAL LUCID::LISP-SIGNAL-HANDLER LUCID::COMMON-BODY) 4.000 
                 8: SUBTYPEP 28.000 
                   9: LUCID::NORMALIZE-TYPE 8.000 
                   9: LUCID::NORMALIZE-LIMIT-TYPE 4.000 


     Procedures by % exclusive time -- 25 samples taken

  1 - SUBTYPEP                          16.000 
  2 - NTHCDR                            12.000 
  3 - LIST-ROW                          12.000 
  4 - LUCID::NORMALIZE-TYPE             8.000 
  5 - LUCID::INSTALL-AUX                8.000 
  6 - ASSOC                             8.000 
  7 - LUCID::INITIAL-CONTENTS-MISMATCH-P   8.000 
  8 - LUCID-RUNTIME-SUPPORT:2DIM-AREF-SUBR    4.000 
  9 - LUCID::SXHASH-LIST                4.000 
 10 - LUCID::NORMALIZE-LIMIT-TYPE       4.000 
 11 - LUCID-RUNTIME-SUPPORT:SET-SVREF   4.000 
 12 - LOGAND                            4.000 
 13 - LIST-REVERSE                      4.000 
 14 - SYSTEM:SYSCALL                    4.000 

;;;----------------------------------------------------------------------
;;;                        END OF FILE
;;;----------------------------------------------------------------------
