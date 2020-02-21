;;;======================================================================
;;; Compile file for...Constraint Maintenance (CM) System
;;; Author:  Alfred J. Reich
;;;======================================================================


#+LUCID (in-package "USER")
#+CLTL2 (in-package "COMMON-LISP-USER")


;;; Sometimes we fly without a net...
;;; (proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Put this in features so that ALL files get compiled...
(pushnew :DERIVE-CONSTRAINT-ALGEBRA *features*)


;;;----------------------------------------------------------------------

#+UNIX
(defun COMPILE-&-LOAD-FILE (pathname)
  (compile-file pathname)
  (load pathname))


#+UNIX
(let ((cwd (pwd)))
  ;;
  ;; LOAD INTERVAL MATHEMATICS PACKAGE
  ;;
  (cd "/usrhp1/albm/reich/Lisp/IM")
  (load "package")
  ;;
  ;; COMPILE REQUIRED CMS FILES
  ;;;
  (cd "/usrhp1/albm/reich/Lisp/CM")
  (compile-&-load-file "package")
  (compile-&-load-file "utilities")
  (compile-&-load-file "algebra-support")
  (compile-&-load-file "network-support")
  (compile-&-load-file "propagator")
  (compile-&-load-file "output-utilities")
  #+DERIVE-CONSTRAINT-ALGEBRA
  (compile-&-load-file "derive-alg-elem")
  #+DERIVE-CONSTRAINT-ALGEBRA
  (compile-&-load-file "derive-mult-tbl")
  ;;
  ;; LOAD INTERVAL MATHEMATICS SYSTEM
  ;;
  (cd "/usrhp1/albm/reich/Lisp/IM")
  (load "std-int-arith")
  ;;
  ;; COMPILE (OPTIONAL) PREDEFINED ALGEBRAS
  ;;
  (cd "/usrhp1/albm/reich/Lisp/CM")
  (compile-&-load-file "basic-algebras")
  (compile-&-load-file "tcn")           ; TCN = Temporal Constraint Network
  #+TEST-CONSTRAINT-ALGEBRA
  (compile-&-load-file "testcases")
  (cd cwd))

;;;----------------------------------------------------------------------
;;; LOADING UNDER APPLE MACINTOSH COMMON LISP
;;;----------------------------------------------------------------------

#+MCL
(progn
  ;;;
  ;;; LOAD THE INTERVAL MATHEMATICS SYSTEM
  (load "ims:load-file")
  ;;;
  ;;; LOAD THE BASIC CONSTRAINT MANAGEMENT SYSTEM (CMS)
  (compile-&-load-file "cms:package")
  (compile-&-load-file "cms:utilities")
  (compile-&-load-file "cms:algebra-support")
  (compile-&-load-file "cms:network-support")
  (compile-&-load-file "cms:propagator")
  ;;;
  ;;; LOAD PREDEFINED ALGEBRAS
  (compile-&-load-file "cms:basic-algebras")
  ;;;
  ;;; LOAD DERIVATION CODE
  #+DERIVE-CONSTRAINT-ALGEBRA
  (compile-&-load-file "cms:derive-alg-elem")
  #+DERIVE-CONSTRAINT-ALGEBRA
  (compile-&-load-file "cms:derive-mult-tbl")
  (compile-&-load-file "cms:output-utilities")
  (compile-&-load-file "cms:tcn")
  (format t "~%;;; Constraint Management System loaded.")
  'done)

(pushnew :CONSTRAINT-MANAGEMENT *features*)

;;;----------------------------------------------------------------------
;;; END OF FILE
;;;----------------------------------------------------------------------
