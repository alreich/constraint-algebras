;;;;**********************************************************************
;;;;
;;;;    File: package.lisp
;;;;    Author: Alfred J. Reich, Ph.D.
;;;;            reich@austin.lockheed.com
;;;;            (512) 386-4178
;;;;    Creation Date: 
;;;;    Revision List: 
;;;;    Description:  The package definition for the Constraint
;;;;      Maintenance System (CMS)
;;;;    Exported Symbols:
;;;;      DEFINE-ALGEBRA.........Defines a constraint algebra.
;;;;      FIND-ALGEBRA...........Looks up and returns a predefined constraint
;;;;                             algebra from its name.
;;;;      DELETE-ALGEBRA.........Deletes a predefined algebra from currently
;;;;                             defined constraint algebras.
;;;;      DUMP-ALGEBRA...........Outputs a printed representation of a
;;;;                             constraint algebra.
;;;;    Notes:  This file is conditionalized to load under Lucid Common
;;;;      Lisp and Macintosh Common Lisp.
;;;;
;;;;**********************************************************************

#+CLTL2 (in-package "COMMON-LISP-USER")
#+LUCID (in-package "USER")

(defpackage "CONSTRAINT-MAINTENANCE"
  (:use
   "INTERVAL-MATH"
   #+LUCID "LISP"			; Lucid Common Lisp
   #+LUCID "USER"			; Lucid Common Lisp
   #+LUCID "LUCID-COMMON-LISP"	; Lucid Common Lisp
   #+LUCID "CLOS"			; Lucid Common Lisp
   #+CLTL2 "COMMON-LISP-USER"		; Macintosh Common Lisp 2.0
   #+CLTL2 "COMMON-LISP"		; Macintosh Common Lisp 2.0
   #+MCL   "CCL"			; Macintosh Common Lisp 2.0
   )
  (:nicknames "CM" "CMS")
  (:export DEFINE-ALGEBRA FIND-ALGEBRA DELETE-ALGEBRA DUMP-ALGEBRA
           INITIALIZE-CONSTRAINT-NETWORK REGISTER-CONSTRAINED-OBJECT
           REGISTER-CONSTRAINED-OBJECTS ASSERT-CONSTRAINT ASSERT-CONSTRAINTS
           PROPAGATE-CONSTRAINTS GET-CONSTRAINTS)
  #+MCL (:import-from "INTERVAL-MATH" INT/ I<<= INT= INT& INT* MAKE-INTERVAL)
  )

;;;----------------------------------------------------------------------
;;;                         END OF FILE
;;;----------------------------------------------------------------------
