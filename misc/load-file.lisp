;;;======================================================================
;;; Load file for...Constraint Maintenance (CM) System
;;; Author:  Alfred J. Reich
;;;======================================================================

#+LUCID (in-package "USER")
#+CLTL2 (in-package "COMMON-LISP-USER")

;;;----------------------------------------------------------------------
;;; LOADING UNDER UNIX (LUCID)
;;;----------------------------------------------------------------------

#+UNIX
(let ((cwd (pwd)))
  ;;
  ;; LOAD/COMPILE INTERVAL MATHEMATICS PACKAGE
  ;;
  (cd "/usrhp1/albm/reich/Lisp/IM")
  (load "package"
	:if-source-newer :compile :if-source-only :compile)
  ;;
  ;; LOAD/COMPILE REQUIRED CMS FILES
  ;;;
  (cd "/usrhp1/albm/reich/Lisp/CM")
  (load "package"
	:if-source-newer :compile :if-source-only :compile)
  (load "utilities"
	:if-source-newer :compile :if-source-only :compile)
  (load "algebra-support"
	:if-source-newer :compile :if-source-only :compile)
  (load "network-support"
	:if-source-newer :compile :if-source-only :compile)
  (load "propagator"
	:if-source-newer :compile :if-source-only :compile)
  #+DERIVE-CONSTRAINT-ALGEBRA
  (load "derive-alg-elem"
	:if-source-newer :compile :if-source-only :compile)
  #+DERIVE-CONSTRAINT-ALGEBRA
  (load "derive-mult-tbl"
	:if-source-newer :compile :if-source-only :compile)
  ;;
  ;; LOAD/COMPILE INTERVAL MATHEMATICS SYSTEM
  ;;
  (cd "/usrhp1/albm/reich/Lisp/IM")
  (load "std-int-arith"
	:if-source-newer :compile :if-source-only :compile)
  ;;
  ;; LOAD/COMPILE (OPTIONAL) PREDEFINED ALGEBRAS
  ;;
  (cd "/usrhp1/albm/reich/Lisp/CM")
  (load "basic-algebras"
	:if-source-newer :compile :if-source-only :compile)
  (load "tcn"				; TCN = Temporal Constraint Network
	:if-source-newer :compile :if-source-only :compile)
  #+TEST-CONSTRAINT-ALGEBRA
  (load "testcases"
	:if-source-newer :compile :if-source-only :compile)
  (cd cwd))

;;;----------------------------------------------------------------------
;;; LOADING UNDER APPLE MACINTOSH COMMON LISP (MCL)
;;; [Assumes that 'ims' and 'cms' are logical pathnames for the
;;;  interval-math and constraint-management systems.]
;;;----------------------------------------------------------------------

#+MCL
(progn
  ;;;
  ;;; LOAD THE INTERVAL MATHEMATICS SYSTEM
  (load "ims:load-file")
  ;;;
  ;;; LOAD THE BASIC CONSTRAINT MANAGEMENT SYSTEM (CMS)
  (load "cms:package")
  (load "cms:utilities")
  (load "cms:algebra-support")
  (load "cms:network-support")
  (load "cms:propagator")
  ;;;
  ;;; LOAD PREDEFINED ALGEBRAS
  (load "cms:basic-algebras")
  ;;;
  ;;; LOAD DERIVATION CODE
  #+DERIVE-CONSTRAINT-ALGEBRA
  (load "cms:derive-alg-elem")
  #+DERIVE-CONSTRAINT-ALGEBRA
  (load "cms:derive-mult-tbl")
  (load "cms:output-utilities")
  (load "cms:tcn")
  (load "cms:mac-interface")
  (format t "~%;;; Constraint Management System loaded.")
  'done)

(pushnew :CONSTRAINT-MANAGEMENT *features*)

;;;----------------------------------------------------------------------
;;; END OF FILE
;;;----------------------------------------------------------------------
