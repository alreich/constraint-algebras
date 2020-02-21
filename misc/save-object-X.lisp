(in-package "CM")

#|

      File: save-object.lisp. (version 3A)

 SAVE-OBJECT by K.V. Koitzsch: Effective Date 5-31-91.

Copyright (c) 1990, 1991 Advanced Decision Systems

The views, opinions and/or findings contained in this document are
those of the author, and should not be construed as official
Advanced Decision Systems position, policy, or decision, unless
designated by other documentation.

Permission is granted to any individual or instituion to use, copy,
modify and distribute this document, provided this complete copyright
and permission notice is maintained, intact, in all copies and 
supporting documentation. Advanced Decision Systems makes no representations
about the suitability of the software described herein for any purpose.
It is provided "as is" without express or implied warranty.

Suggestions, bugs, criticism and questions to kerry@ads.com.


      This file contains:
      ==== ==== ========

      PCL and CLOS version dependent access functions/methods to support
      CLASS-FASD-FORM, GENERIC-FUNCTION-FASD-FORM, and METHOD-FASD-FORM.

Tested on:
====== ==

      Machines: Sun-4, Symbolics 3670, Macintosh.

      Versions of Symbolics Lisp: Rel 7.2.

      Versions of PCL/CLOS: AAAI PCL, Victoria Day PCL, REV 4b PCL, Lucid 4.0 CLOS.

      Directions:
      ==========

      (1). Redefine the IN-PACKAGEs below to suit: they should USE
           CLOS or PCL, though.

      (2). Load the file, save-object.lisp.

      (3). Try some of these simple test cases:

(defclass TEST ()
     ((blog)
      (rog)
      (dog :initform 25)))

(DB::CLASS-FASD-FORM (FIND-CLASS 'TEST)) evaluates to:

(DEFCLASS TEST
          (STANDARD-OBJECT)
          ((BLOG :TYPE T :ALLOCATION :INSTANCE :INITFORM NIL :DOCUMENTATION "")
           (ROG :TYPE T :ALLOCATION :INSTANCE :INITFORM NIL :DOCUMENTATION "")
           (DOG :TYPE T :ALLOCATION :INSTANCE :INITFORM 25 :DOCUMENTATION ""))
          (:DOCUMENTATION ""))

(defclass TEST-1 ()
     ((blog :initarg :blog)
      (rog :initarg :rog
	   :accessor get-rog)
      (dog :initform 25))
(:default-initargs :blog 'yes))

(DB::CLASS-FASD-FORM (FIND-CLASS 'TEST-1))

evaluates to:

(DEFCLASS TEST-1
          (STANDARD-OBJECT)
          ((BLOG :INITARG :BLOG :TYPE T :ALLOCATION :INSTANCE :INITFORM NIL :DOCUMENTATION "")
           (ROG :INITARG :ROG :TYPE T :ACCESSOR GET-ROG :ALLOCATION :INSTANCE :INITFORM NIL
            :DOCUMENTATION "")
           (DOG :TYPE T :ALLOCATION :INSTANCE :INITFORM 25 :DOCUMENTATION ""))
          (:DEFAULT-INITARGS :BLOG (LIST 'QUOTE 'YES))
          (:DOCUMENTATION ""))

;;; TEST CLASSES.

(defclass foo ()
   ((bar :initform '()
         :initarg :bar
	 :allocation :class
         :reader bar
         :documentation "Keeps a record of all foo objects ever created.
See initialize-instance below.  Note the :class allocation... "
   ))
(:documentation "A test class named FOO."))

(defmethod INITIALIZE-INSTANCE :AFTER ((foo foo) &rest plist)
  (declare (ignore plist))
    (with-slots (bar) foo
     (push foo bar)))

 ;; Try:

(CLASS-FASD-FORM (find-class 'foo))

Notes:
======

 slot & class access methods for pcl and clos versions.....
 GENERIC-FUNCTION-P and CLASSP are NOT EXPORTED in AAAI PCL?

 CLASS-FASD-FORM, GENERIC-FUNCTION-FASD-FORM, METHOD-FASD-FORM are defined in save-object.lisp.

Doesnt seem to be any way to determine the contents of a readtable in Lucid?

methods defined:
------- -------
     methodp: predicate to test for method object.
     mboundp: predicate to test if a 'method' is bound.
   
     get-slot-name, %get-slot-name
     get-slot-type, %get-slot-type
     get-slot-allocation, %get-slot-allocation
     get-slot-initarg, %get-slot-initarg
     get-slot-reader, %get-slot-reader
     get-slot-writer, %get-slot-writer
     get-slot-accessor, %get-slot-accessor
     get-slot-documentation, %get-slot-documentation
     get-slot-initform, %get-slot-initform
  
     get-class-metaclass (NEW).
     builtin-class-p (NEW)

PCL/CLOS dependent functions/methods:
=====================================

get-slot-readers
get-slot-writers
get-slot-type
get-slot-name
get-slot-allocation
get-slot-initarg

Revision List:
======== =====

---- ADDED READTABLE-FASD-FORM kludge.

---- added predicates BUILTIN-CLASS-P for lucid 4.0 CLOS and for non-AAAI pcls.

---- added BUILTIN-CLASS-P for Genera 8.0 CLOS.

MACINTOSH ALLEGRO ENHANCEMENTS:
========= ======= =============
Modified for Allegro Common Lisp/Mac Common Lisp v2.0 by Ashok Khosla, CLARIS Inc. 4/22/91
Modification changes:
1.) package references using lisp:: have been removed. in-package uses common-lisp-user package instead.
2.) cons-p, and structure-p are replaced by built-in functions consp and structurep
3.) structure saving functions were rewritten for MACL 2.0
4.) Compiled functions are not saveable

Modification caveats are annotated (AMK) 
Suggestions, bugs, criticism and questions about modifications should be sent to:
Ashok_Khosla@claris.com

CLARIS makes no representations about the suitability of the software described herein for
 any purpose.It is provided "as is" without express or implied warranty.

   ---- Added provision for :METACLASS option in DEFCLASS for CLASS-FASD-FORM,
        defined GET-CLASS-METACLASS in slot-access.lisp.

   ---- defined FUNCTION-NAME for Lucid 4.0. in terms of
        SYS:PROCEDURE-REF. (SYS:PROCEDURE-SYMBOL).

   ---- redefined MAKE-KEYWORD per barmars suggestion.

   ---- revised for Mac/Allegro Common Lisp v2.0 (AMK)

   ---- Added GENERIC-FUNCTION-FASD-FORM, METHOD-FASD-FORM, CLASS-FASD-FORM (overwrites any
   existing class definition of the same name in memory).,
   SAFE-CLASS-FASD-FORM (uses FIND-CLASS and does not overwrite previous class definition). 

   ---- Added predicate function METHODP, and support functions for the new FASD forms.

   ---- Added trivial INSTANCE-P definition for rev-4b of PCL.

   ---- Defined FIND-GENERIC-FUNCTION to support GENERIC-FUNCTION-FASD-FORM: A function
        given the name of a supposed generic function, returns the function object if it exists,
        NIL otherwise.

  See description of slot/class access functions.

  --- added defvar for *global-unsavable-slots*, *seen*, *vars* got deleted somehow.

  --- Took suggestion of kanderso@BBN.COM and modified INSTANCE-FASD-FORM to use
       ALLOCATE-INSTANCE and FILL-INSTANCE rather than MAKE-INSTANCE.
       This should allow circular instance references within instance slots. 
       OR, alternatively, use the recursive file-reader READ-FILE instead of LOAD.
      This seems to do the right thing as far as circular refs within instances go.

====================================================================================================

SAVE-OBJECT is a recursive function which writes an ASCII representation of
a LISP object to a designated file. 

Objects which may be saved include:

      ---- symbols, keywords, numbers, characters, strings, and  pathnames.
      ---- vectors and multi-dimensional arrays.
      ---- objects produced by defstruct.
      ---- CLOS (PCL) instances.
      ---- hash-tables.
      ---- compiled functions (represented as (FUNCTION <function-name>). )
           (not supported [in MAC version] - AMK)

      ---- generic-functions, method objects, and class objects.
           (New since version 1)

      ---- conses.
      ---- lists.

      ---- user defined methods may be defined for arbitrary objects, such as images.
           (not supported [in MAC version] - AMK)


      ---- readtables (note: kludge for now, no MAC version....) can be got at on the Symbolics....


VERSION TESTS:
======= =====

This version has been tested on the Symbolics Lisp Machine as well as in
Lucid Common Lisp: it is similar to SYS::DUMP-FORMS-TO-FILE on the Symbolics,
except that the output file is an ASCII representation of the dumped object.
This file may then be compiled to produce a binary representation.

(AMK) - tested MACL version only on MACL 2.0 Beta - on MacIIfx under 6.05. CLARIS makes no 
representations about the suitability of the software described herein for any purpose.
It is provided "as is" without express or implied warranty.

HOW TO USE:
=== == ===
To save an object to a file, invoke the SAVE-OBJECT function:

(in-package 'DATABASE) ;;; or invoke from your favorite package.

;;; Define what version of PCL it is, if PCL.

;;; Simple test class definition. 

(defclass TEST ()
  ((a :initarg :a
      :accessor test-a
      :documentation "")
   (b :initarg :b
      :accessor test-b
      :documentation ""))
(:default-initargs :a 333 :b 444)
(:documentation "Try something like this simple class definition to test!"))

;;;; How to save an object:

    (SAVE-OBJECT (MAKE-INSTANCE 'TEST) "/yourmachine/yourdir/saved-object")

    For the Mac:

    (SAVE-OBJECT (MAKE-INSTANCE 'TEST) "yourdir:saved-object") (AMK)

;;; If you want it in binary format:

    (SAVE-OBJECT (MAKE-INSTANCE 'TEST) "/yourmachine/yourdir/saved-object"
         :compile T)

     On the Mac:

    (SAVE-OBJECT (MAKE-INSTANCE 'TEST) "yourdir:saved-object" (AMK)
         :compile T)

;;;( This produces saved-object.bin or saved-object.sbin, depending on machine type).

;;; See SAVE-OBJECT documentation string for further information on its use.
;;; (LOAD <saved-object>) whether binary or source, should be enough to restore the object.
;;; Circular references should be dealt with: try READ-FILE, defined at the bottom of this
;;; file, to debug and test buggus loads....

;;; Upon loading the saved object file, the result will be bound to db::*db-input*.


KNOWN BUGS:
===== =====

    --- Does not deal with the storage of class allocated slot contents
        yet.

    --- Best way to do the following example? NOTE: this should work in version 3a.

   (DEFCLASS TEST ()((A :INITARG :A)))

   (SETF X (MAKE-INSTANCE 'TEST))

   (SETF (SLOT-VALUE X 'A) X)

   (SAVE-OBJECT X "~/your-favorite-savefile")

   Circularities of this type SHOULD NOW WORK....

|#

;;; NOTE: Change the package def below if it does not suit you:
;;; make sure you USE-PACKAGE your favorite brand of CLOS or PCL, though.

;;; Make sure that :genera-release-8-0 is in *features*, when using rel8.0 Symbolics CLOS.

#+clos
(in-package 'DATABASE :nicknames '(DB) :use '(CLOS LISP))

#+pcl
(in-package 'DATABASE :nicknames '(DB) :use '(PCL LISP))

#+excl
(unless (find-package "database")
(defpackage "database" (:nicknames "db") (:use :Common-lisp-user))
)

#+excl
(in-package DATABASE)

#+excl
(use-package 'common-lisp-user)

(export '(*db-input*
	  *global-unsavable-slots*
	  save-object
	  pseudo-quote-reader
	  reset-symbol-counter
	  hash-table-size
	  hash-table-rehash-size
	  hash-table-rehash-threshold
	  hash-table-test
	  make-keyword
	  string-append
	  all-slots
	  copy-instance
	  all-slots-and-values
	  all-values
	  symbol-fasd-form
	  readtable-fasd-form
	  %allocate-instance
	  find-generic-function
	  generic-function-fasd-form
	  method-fasd-form
	  methodp
	  class-fasd-form
	  instance-p
	  instance-name
	  structure-p
	  get-slot-values
	  pushsym
	  list-array
	  coerce-2d-array
	  array-fasd-form
	  make-defstruct-body
	  structure-fasd-form
	  vector-fasd-form
	  compiled-function-fasd-form
	  get-fasd-form
	  fasd-form
	  has-fasd-form-p
	  define-fasd-form
	  instance-fasd-form
	  %make-array
	  describe-htab
	  cons-p
	  cons-fasd-form
	  array-list-aux
	  set-defstruct-slot
	  get-defstruct-value
	  search-symbols
	  read-file
	  makesyms
	  *save-object-system-date* 
	  write-global-header))

;;; GLOBALS...

(defvar *use-default-class-initargs* nil "Uses a keyword of the slots name
as the initarg if this is true e.g. (A :initform 20) ===> (A :initarg :a Initform 20) when this variable is set to true.")

(defvar *suppress-standard-object* t "Does not show standard-object in
the superclass list of the defclass form when this is T.")

(defvar *save-contents-of-class-allocated-classes* nil "When this is t
the values of the class-allocated slots become the initform for the
class on re-load.")

(defvar *global-unsavable-slots* nil)
(defvar *seen* nil)
(defvar *vars* nil)

(defvar *save-object-system-date* "MAY 91 Save Object Experimental: VERSION 3A.")

#+lucid
(setf lcl::*print-structure* T) ;;;; "Prints the #S form of a defstruct when t."

;;; When it is PCL, eval the following forms:

#+pcl
(eval-when (load compile eval)
  
  ;; Find what version of PCL it is and add to the *features* list....
  
  ;; if AAAI PCL....
  
  (if (search "AAAI" pcl::*pcl-system-date*)
    (progn (format t "This is AAAI PCL.~%")
	   (pushnew :AAAI *features*))
    
    (cond ((or (equal pcl::*pcl-system-date* "5/22/89  Victoria Day PCL")
	       (equal pcl::*pcl-system-date* "5/22/89  Victoria Day PCL (BBN)"))
           (format t "initializing victoria day clos feature.")
           (pushnew :vic-day *features* :test #'equal))
          ((search "REV 4" pcl::*pcl-system-date*)
           (format t "This is a rev 4 PCL...~%")
           (pushnew :rev-4 *features* :test #'equal))
          ((equal pcl::*pcl-system-date* "this is a commercial clos."))
          (T (format t "initializing may day clos feature.")
             (pushnew :may-day *features* :test #'equal)))))

#+pcl
(eval-when (load eval compile)

#+rev-4
(defun GET-CLASS-DOCUMENTATION (self)
  "gets the documentation string for a class based on pcl::standard-class."
  (first (getf (slot-value self 'pcl::plist) 'pcl::documentation)))

#-rev-4
(defun GET-CLASS-DOCUMENTATION (self)
  "gets the documentation string for a class based on pcl::standard-class,
   when the pcl::class-options method of storing documentation is used, insted of
   the pcl::plist, pcl::documentation-mixin method of storing it."
  (or (second (assoc :documentation (pcl::class-options self) :test #'equal))
		       "no documentation specified yet."))

#-aaai
(defun GET-CLASS-DEFAULT-INITARGS (class)
  ""
  (mapcan #'(lambda (l)(list (first l)(get-fasd-form (third l))))
          (pcl::class-direct-default-initargs class)))

#+aaai
(defun GET-CLASS-DEFAULT-INITARGS (class)
  ""
  (mapcan #'(lambda (l)(list (first l)(get-fasd-form (third l))))
          (pcl::class-default-initargs class)))

(defmethod METHODP ((name symbol))
  "A symbol is a method if it is fbound, and it has methods on its generic function."
  (and (fboundp name) (generic-function-methods (symbol-function name))))

(defmethod METHODP ((self pcl::standard-generic-function))
"A standard generic function (the return from symbol-function) IS a method,
IFF there are methods defined on it (i.e. didnt just use DEFGENERIC to define  it."
(methodp (pcl::generic-function-name self)))

(defmethod METHODP ((thing pcl::standard-method))
"A standard method object IS a method."
	T)

#-rev-4
(defun CLASS-SLOTS (class)
(pcl::class-local-slots class))

(defun %GET-SLOT-INITFORM (S)
"Method to create the iniform pair, if there is an initform value!"
(if  *save-contents-of-class-allocated-classes*
(when (and (equal (get-slot-allocation s) :CLASS)
	   (slot-boundp s 'pcl::initform))
(list :initform (get-fasd-form (funcall (get-slot-reader s) s))))
(when (slot-boundp s 'pcl::initform)
      (list :initform  (pcl::slotd-initform s)))))

#-rev-4
(defmethod GET-SLOT-INITFORM ((s pcl::standard-slot-description))
""
(when (slot-boundp s 'pcl::initform)
       (pcl::slotd-initform s)))

(setf (symbol-function 'generic-function-lambda-list) #'pcl::generic-function-pretty-arglist)

(setf (symbol-function 'generic-function-name) #'pcl::generic-function-name)

#-aaai
(setf (symbol-function 'method-specializers) #'pcl::method-specializers)

#+aaai
(setf (symbol-function 'method-specializers) #'pcl::method-qualifiers)

(setf (symbol-function 'method-generic-function) 
      #'pcl::method-generic-function)

(setf (symbol-function '%generic-function-p) #'pcl::generic-function-p)

(setf (symbol-function 'classp) #'pcl::classp)

(defun GENERIC-FUNCTION-DOCUMENTATION (fun)
(documentation fun 'function))

#-rev-4
(defun GET-SUPERCLASS-NAMES (class)
""
(mapcar #'pcl::class-name (pcl::class-local-supers class)))

#+rev-4
(defun GET-SUPERCLASS-NAMES (class)
""
 (mapcar #'pcl::class-name (pcl::class-direct-superclasses class)))

#-aaai
(defmethod GET-SLOT-READERS ((slot-object pcl::standard-slot-definition))
     (pcl::slotd-readers slot-object))

#-aaai
(defmethod GET-SLOT-WRITERS ((slot-object pcl::standard-slot-definition))
  (pcl::slotd-writers slot-object))

#-aaai
(defmethod GET-SLOT-TYPE ((s pcl::standard-slot-definition))
"Method to get the type from a standard slot.."
(pcl::slotd-type s))

#-aaai
(defmethod GET-SLOT-NAME ((s pcl::standard-slot-definition))
"Method to get the name from a standard slot."
(pcl::slotd-name s))

#+aaai
(defmethod GET-SLOT-READERS ((slot-object pcl::standard-slot-description))
     (pcl::slotd-readers slot-object))

#+aaai
(defmethod GET-SLOT-WRITERS ((slot-object pcl::standard-slot-description))
  (pcl::slotd-writers slot-object))

#+aaai
(defmethod GET-SLOT-TYPE ((s pcl::standard-slot-description))
"Method to get the type from a standard slot.."
(pcl::slotd-type s))

#+aaai
(defmethod GET-SLOT-NAME ((s pcl::standard-slot-description))
"Method to get the name from a standard slot."
(pcl::slotd-name s))

(defun GET-SLOT-ALLOCATION (S)
(let ((alloc (pcl::slotd-allocation s)))
(cond ((classp alloc) :CLASS)
      ((member alloc '(:INSTANCE :CLASS)) alloc) 
      (T :INSTANCE))))

#-rev-4
(defun GET-SLOT-DOCUMENTATION (S)
(or (pcl::slotd-documentation s) ""))

) ;;;; end of PCL eval-when....


;;;; Beginning of CLOS eval when.....

#+clos
(eval-when (load eval compile)

#+lucid
(defun GET-CLASS-DOCUMENTATION (C)
(or (documentation c) ""))

#+lucid
(defmethod GET-SLOT-DOCUMENTATION ((s clos::standard-slot-definition))
""
(or (clos::slotd-documentation s) ""))

#+lucid
(defun GET-SLOT-READERS (slot-object)
  (clos::slotd-readers slot-object))

#+lucid
(defun GET-SLOT-WRITERS (slot-object)
  (clos::slotd-writers slot-object))

#+lucid
(defun GET-SLOT-TYPE (S)
"Method to get the type from a standard slot.."
(clos::slotd-type s))

#+lucid
(defmethod GET-SLOT-ALLOCATION ((s clos::standard-slot-description))
"Method to get the type of allocation from a standard slot: oneof :CLASS or :INSTANCE."
(let ((alloc (clos::slotd-allocation s)))
(cond ((classp alloc) :CLASS)
      ((member alloc '(:INSTANCE :CLASS)) alloc) 
      (T :INSTANCE))))

#+(and lispm pcl)
(defun CLASS-SLOTS (x)
(pcl::class-slots x))

#+(and lispm pcl)
(defun GET-SLOT-INITARGS (slot)
(pcl::slotd-initargs slot))

#+rev-4
(defun GET-SLOT-DOCUMENTATION (slot)
(slot-value slot 'pcl::documentation))

#+genera-release-8-0
(defun GET-SLOT-ALLOCATION (S)
"Method to get the type of allocation from a standard slot: oneof :CLASS or :INSTANCE."
(let ((alloc (clos::slot-definition-allocation s)))
(cond ((classp alloc) :CLASS)
      ((member alloc '(:INSTANCE :CLASS)) alloc) 
      (T :INSTANCE))))

#+genera-release-8-0
(defun GET-SLOT-DOCUMENTATION (S)
"Default method for getting the slots documentation."
(or (clos::slot-definition-documentation s) ""))

#+lucid
(defun GET-CLASS-DEFAULT-INITARGS (class)
"Gets the default-initargs out of the class object."
(mapcan #'(lambda (l)(list (first l)(get-fasd-form (third l))))
(clos::class-direct-default-initargs class)))

#+lucid
(defun GET-SLOT-NAME (S)
"Method to get the name from a standard slot."
(clos::slotd-name s))

#+genera-release-8-0
(defun GET-SLOT-READERS (slot-object)
  (clos::slot-definition-readers slot-object))

#+genera-release-8-0
(defun GET-SLOT-WRITERS (slot-object)
  (clos::slot-definition-writers slot-object))

#+genera-release-8-0
(defun GET-SLOT-TYPE (S)
"Method to get the type from a standard slot.."
(clos::slot-definition-type s))

#+genera-release-8-0
(defun GET-SLOT-NAME (S)
"Method to get the name from a standard slot."
(clos::slot-definition-name s))

#+lucid
(defmethod GET-SLOT-INITFORM ((s clos::standard-slot-description))
""
(when (slot-boundp s 'clos::initform)
       (clos::slotd-initform s)))

#+genera-release-8-0
(defun get-SLOT-INITFORM (s)
""
  (clos::slot-definition-initform s))

#+LUCID
(defUN %GET-SLOT-INITFORM (S)
"Method to create the iniform pair, if there is an initform value!"
(if  *save-contents-of-class-allocated-classes*
(when (and (equal (get-slot-allocation s) :CLASS)
	   (slot-boundp s 'clos::initform))
(list :initform (get-fasd-form (funcall (get-slot-reader s) s))))
(when (slot-boundp s 'clos::initform)
      (list :initform  (clos::slotd-initform s)))))

) ;;;; end of CLOS eval when....

;;; VERSION DEPENDENT FUNCTIONS....

#+aaai
(defvar *builtin-class-names* (mapcar #'first pcl::*built-in-class-symbols*))

#+clos
(defun GET-SUPERCLASS-NAMES (class)
(mapcar #'clos::class-name (clos::class-direct-superclasses class)))

#+(and lucid clos)
(defun GET-SLOT-INITARGS (s)
(clos::slotd-initargs s))

#+(and lucid pcl)
(defun GET-SLOT-INITARGS (s)
(pcl::slotd-initargs s))

#+genera-release-8-0
(defun GET-SLOT-INITARGS (s)
(clos::slot-definition-initargs s))

#+(and lucid clos)
(defun GET-SLOT-INITARG (s)
(first (clos::slotd-initargs s)))

#+(and lucid pcl)
(defun GET-SLOT-INITARG (s)
(first (pcl::slotd-initargs s)))

#+genera-release-8-0
(defun GET-SLOT-INITARG (s)
(first (clos::slot-definition-initargs s)))

#+aaai
(defun BUILTIN-CLASS-P (X)
"Predicate which tells if an class-object is a builtin class or not."
(and (classp x)(member (class-name x) *builtin-class-names*)))

#+(and lucid clos)
(defun BUILTIN-CLASS-P (X)
  "Predicate to determine whether a class object is a builtin class. returns
   T if it is."
  (and (classp x)(member (class-name x)
  (mapcar #'first clos-system::built-in-classes) :test #'equal)))

#+(and pcl (not lucid))
(defun BUILTIN-CLASS-P (X)
  "Predicate to determine whether a class object is a builtin class. returns
   T if it is."
(and (classp x)(member (class-name x) *builtin-class-names*)))

#+pcl
(defun GET-CLASS-METACLASS (class-object)
"Given a class object, returns the metaclass name to help build
 CLASS-FASD-FORM: (NEW)."
(when (classp class-object)
(let ((meta (class-name (class-of (class-of class-object)))))
(if (not (equal meta 'pcl::standard-class)) ;;; the default...
(list (list :metaclass meta))))))

#+clos
(defun GET-CLASS-METACLASS (class-object)
"Given a class object, returns the metaclass name to help build
 CLASS-FASD-FORM:  (NEW)."
(when (classp class-object)
(let ((meta (class-name (class-of (class-of class-object)))))
(if (not (equal meta 'clos::standard-class)) ;;; the default...
(list (list :metaclass meta))))))

#+genera-release-8-0
(defun BUILTIN-CLASS-P (X)
(and (classp x)(equal (class-name (class-of x)) 'CLOS::BUILT-IN-CLASS)))

#+clos
(defun %GENERIC-FUNCTION-P (x)
"Predicate, returns t for generic functions. causes symbol conflict problem
 in genera 8.0."
(clos::generic-function-p x))

#+CLOS
(defun GENERIC-FUNCTION-DOCUMENTATION (f)
(clos::generic-function-documentation f))

#+CLOS
(defun CLASSP (X)
"Predicate to determine if x is a class."
(clos::classp x))

;;; Independent.

(defun GET-CLASS-SUPERCLASSES (class)
"Returns a list of the NAMES (symbol list) of the direct superclasses of the class object."
(let ((the-ones (get-superclass-names class)))
  (if *suppress-standard-object* (delete 'standard-object the-ones)
    the-ones)))

(defun GET-SLOT-READER (slot-object)
  (first (get-slot-readers slot-object)))

(defun GET-SLOT-WRITER (slot-object)
  (first (get-slot-writers slot-object)))

(defun ACCESSOR-EXISTS-P (S)
"Predicate: Returns T iff the slot has both a reader and a standard writer."
(let* ((readers (get-slot-readers s))
       (writers (get-slot-writers s))
       (accessors (some #'(lambda (writer)
			    (and (listp writer)
				 (equal (first writer) 'SETF)
				 (second writer)
				 (member (second writer) readers
					 :test #'equal)))
			writers)))
  accessors))

(defun GET-SLOT-ACCESSOR (s)
"Returns the first slot accessor alone."
(let ((val  (first (get-slot-readers s))))
(when (and val (accessor-exists-p s))
      val)))

(defun %GET-SLOT-NAME (S)
"Method to get the name from a standard slot."
(get-slot-name s))

(defun %GET-SLOT-ALLOCATION (S)
"Method to get the type of allocation from a standard slot: oneof :CLASS or :INSTANCE."
(let ((val  (get-slot-allocation s)))
(when val (list :allocation val))))

(defun %GET-SLOT-TYPE (S)
"Method to get the type from a standard slot.."
(list :type (get-slot-type s)))

(defun %GET-SLOT-INITARG (S)
"Method to get the first initarg found for the standard slot instance supplied."
(let ((val (or (first (get-slot-initargs s))
(if *use-default-class-initargs* (make-keyword (get-slot-name s))))))
(when val (list :initarg val))))

(defun %GET-SLOT-READER (slot)
"Method to determine whether to use an accessor or a reader. Does not splice
 into the fasd form if there is no reader defined."
(when (null (%get-slot-accessor slot))
  (let ((val  (GET-SLOT-reader slot)))
    (when val (list :reader val)))))

(defun %GET-SLOT-WRITER (slot)
"Method to determine whether to use an accessor or a writer. Does not splice
 into the fasd form if there is no writer defined."
(when (null (%get-slot-accessor slot))
  (let ((val (GET-SLOT-WRITER slot)))
      (when val (list :writer val)))))

(defun %GET-SLOT-DOCUMENTATION (S)
""
(list :documentation (or (GET-SLOT-DOCUMENTATION s) "")))

(defun %GET-SLOT-ACCESSOR (S)
""
(let ((val  (GET-SLOT-READER s)))
(when (and val (accessor-exists-p s))
      (list :accessor val))))

(defmethod METHODP ((thing null))
"NIL is not a method."
nil)

(defmethod METHODP ((thing t)) 
"Anything else is not a method."
  nil)

(defmethod MBOUNDP ((name symbol))
  "Predicate: returns t if this name is a method as opposed to a function/macro."
  (when (methodp name) T))

(defmethod MBOUNDP ((name null))
"vacuous case for NIL."
NIL)

(defmethod MBOUNDP ((name t))
  "Predicate: returns t if this name is a method as opposed to a function/macro."
  (when (methodp name) T))

(defun SLOT-DATA-AS-PLIST (slot)
"Generates the slot value pairs of the slot descriptor as a property list, 
 of course the name is stuck on the front."
(let ((name (get-slot-name slot))
      (initarg (get-slot-initarg slot))
      (accessor (get-slot-accessor slot))
      (initform (get-slot-initform slot))
      (type (get-slot-type slot))
      (documentation (get-slot-documentation slot))
      (allocation (get-slot-allocation slot)))
(if accessor
(list name :initarg initarg 
      :accessor accessor
      :initform initform
      :type type
      :documentation documentation
      :allocation allocation)
(list name :initarg initarg 
      :initform initform
      :type type
      :documentation documentation
      :allocation allocation))))

(defun CONSTRUCT-SLOT-SPEC (slot)
"The internal fasd-form constructor for slots."
(let ((name (%get-slot-name slot))
      (initarg-pair (%get-slot-initarg slot))
      (type-pair (%get-slot-type slot))
      (accessor-pair (%get-slot-accessor slot))
      (reader-pair (%get-slot-reader slot))
      (writer-pair (%get-slot-writer slot))
      (allocation-pair (%get-slot-allocation slot))
      (initform-pair (%get-slot-initform slot))
      (documentation-pair (%get-slot-documentation slot)))
`(,name ,@initarg-pair
        ,@type-pair
	,@accessor-pair
	,@reader-pair
	,@writer-pair
	,@allocation-pair
	,@initform-pair
	,@documentation-pair)))

(defun GENERATE-CLASS-SLOT-FORM (slotd)
"Default method for rev4b --- seems to be defective...
 This one gets called by CLASS-FASD-FORM."
(construct-slot-spec slotd))

(defun SORT-ALLOCATED-SLOTS (class-object)
(let ((slots (class-slots class-object)))
(values
(remove-if-not #'(lambda (slot)(equal (get-slot-allocation slot)
				      :CLASS))
	       slots)
(remove-if-not #'(lambda (slot)(equal (get-slot-allocation slot)
				      :INSTANCE))
	       slots))))

;;;; Only allow class save, method save, and generic function save when this
;;; file is loaded: control this with :class-save on the features list.

(pushnew :class-save *features*)

;;; To deal with machine-dependent hash-table parameters in pre-ANSI Lucid COMMON LISP...

#+lucid
(eval-when (load eval compile)

(setf (symbol-function 'hash-table-rehash-size) #'lcl::hash-table-rehash-size)

(setf (symbol-function 'hash-table-size) #'lcl::hash-table-size)

(setf (symbol-function 'hash-table-test) #'lcl::hash-table-test)

(setf (symbol-function 'hash-table-rehash-threshold) #'lcl::hash-table-rehash-threshold)

) ;;; end of lucid eval-when.

;;; CLOS/PCL independent class accessor methods.

#+class-save
(eval-when (load eval compile)

(defun DO-SPECIALIZER (spec)
"Map objects to class names."
(cond ((SYMBOLP SPEC) spec)
      ((CLASSP SPEC)`(FIND-CLASS ',(class-name spec)))
      (T SPEC)))

(defun DO-SPECIALIZERS (lst)
(loop for spec in lst collect (do-specializer spec)))

(defun FIND-GENERIC-FUNCTION (name)
"A function given the name of a supposed generic function,
 returns the function object if it exists, NIL otherwise."
(cond ((and (fboundp name)(%generic-function-p name))
       (symbol-function name))
      (T NIL)))

(defun GENERATE-CLASS-OPTIONS-FORM (class)
"Generates a fasd form for the default-initargs, metaclass,
 documentation components of a class object...."
(let ((default-initargs (get-class-default-initargs class))
      (metaclass (get-class-metaclass class)))
  (if default-initargs
      `((:default-initargs ,@default-initargs)
	,@metaclass
	(:documentation ,(or (get-class-documentation class) "")))
    `(,@metaclass
      (:documentation ,(or (get-class-documentation class) ""))))))

(defun GENERATE-CLASS-SLOT-FORMS (class)
"This generates fasd forms for all the slots in the class object."
(loop for slot in (class-slots class)
      collect (generate-class-slot-form slot)))

) ;;; end of class-save eval-when.

;;; Now, the Symbolics....

#+lispm
(defun HASH-TABLE-SIZE (x)
(scl:send x :size))

#+lispm
(defun HASH-TABLE-TEST (x)
(si:function-name (cli::test-function x)))

(defun PSEUDO-QUOTE-READER (stream subchar arg)
  "Reader to convert a function spec into a more parsable format."
  (declare (ignore subchar arg))
  (eval
   (list 'quote
	 (second (read-from-string 
		  (nsubstitute #\space #\#
       (concatenate 'string "(" 
	    (read-line stream t nil t) ")")
			       :test #'equal))))))

(defun MAKE-KEYWORD (x)
"Makes a keyword out of a symbol."
 (if (keywordp x) x (intern (symbol-name x) 'keyword)))

(defun NEWSYM (symbol)
  "Similar to GENSYM, but allows access to the gensym counter unlike pre-ANSI GENSYM."
  (if (null (get symbol 'namecounter))
      (setf (get symbol 'namecounter) 0))
  (read-from-string (concatenate 'string (string symbol)
  (format nil "~S" (incf (get symbol 'namecounter))))))

#-clos
(defun INSTANCE-NAME (instance)
  "returns the symbol naming the given class object."
  (pcl::class-name (pcl::class-of instance)))

#+clos
(defmethod INSTANCE-NAME ((instance T))
  "returns the symbol naming the given class object."
(clos::class-name (clos::class-of instance)))

#+aaai
(defmacro INSTANCE-P (X)
"Predicate to test if something is an instance: AAAI PCL."
  `(typep ,x 'pcl::iwmc-class))

#+rev-4
(defmacro INSTANCE-P (X)
"Predicate to test if something is an instance: AAAI PCL."
  `(and (lisp:typep ,x 'pcl::standard-object)
	(not (pcl::classp ,x))))

#+may-day
(defmacro INSTANCE-P (X)
  "predicate to determine if something is an INSTANCE: May day PCL."
  `(and (lisp:typep ,x 'pcl::standard-object)
	(not (pcl::classp ,x))))

#+vic-day
(defmacro INSTANCE-P (X)
  "predicate to determine if something is an INSTANCE: Victoria day PCL"
  `(lisp:typep ,x 'pcl::iwmc-class))

#+clos
(defmacro INSTANCE-P (X)
  "predicate to determine if something is an INSTANCE: LCL CLOS. (rev 4.0)"
  `(and (not (clos::classp ,x))(lisp:typep ,x 'clos::iwmc-class)))

#-clos
(defun ALL-SLOTS (instance &optional (all-allocations T))
  "returns the names of the slots in instance."
(let ((them (mapcar #'(lambda (slot)
			(pcl::slot-value slot 'pcl::name))
          (pcl::slots-to-inspect (pcl::class-of instance)
				 instance))))
(if all-allocations them 
	  (remove-if-not #'(lambda (slot)
          (equal (pcl::slotd-allocation slot) :instance))
			      them))))

#+clos
(defmethod ALL-SLOTS ((instance T) &optional (all-allocations T))
  "returns the names of the slots in instance, uses what MOP stuff is available."
(mapcar #'clos::slot-definition-name (clos::class-slots (clos::class-of instance))))

(defmethod COPY-INSTANCE ((instance T))
  "Provides shallow copying of any instance: returns a new copy of a given clos instance,
    writ as a method so youse gys can write ur own."
(let* ((copy (make-instance (instance-name instance)))
       (slots (all-slots instance)))
    (dolist (slot slots)
      (setf (slot-value copy slot)
	    (slot-value instance slot)))
    copy))

(defmethod ALL-SLOTS-AND-VALUES ((instance T))
"returns an alist of slot value pairs. NOTE: Each alist cell is a LIST, NOT a CONS!"
  (loop for slot in (all-slots instance) nconc
	(list slot (when (slot-boundp instance slot)
			 (slot-value instance slot)))))

;;; The main routine, SAVE-OBJECT.

(defun SAVE-OBJECT (object-instance filename &key
				    (compile nil)
				    (variable '*db-input*)
				    (if-exists :append)
				    (print-pretty nil)
				    (max-print-level 10000000)
				    (package nil) 
				    (if-does-not-exist :create))
  (setf *global-instance-count* 0)
  (let* ((*print-level*  max-print-level)
         (*print-pretty* print-pretty)
         (*print-length* 50000000)
         (*package*      (or (and package (find-package package))
			     *package*))
	 (pathname       filename)
	 (form           (get-fasd-form object-instance :reset t)))
    (setf (get '.%%SYMBOL-LABEL%%. 'namecounter) 0)
    (with-open-file (stream pathname :direction :output :if-exists if-exists
			    :if-does-not-exist if-does-not-exist)
      (format stream "~%~s"
	      `(in-package ',(read-from-string (package-name *package*))))
      (write-global-header stream 
			   '.%%SYMBOL-LABEL%%. 0
			   *global-instance-count*)
      (format stream "~%~s" `(setq ,variable ,form)))
    (format t "~& object saved to file: ~A" pathname)
    (when compile (format t "~% compiling file ~A" pathname)
          (compile-file pathname)
          (format t "~% done compiling file ~A" pathname))))

;;; ======= FASD forms. ===========

(defvar *global-instance-count* 0)
(setf *global-instance-count* 0)

(defmethod STRIP-PACKAGE ((x symbol))
"strip the package designator off the symbol, return the rest,
if keyword, return self.."
(if (keywordp x) x
  (intern (symbol-name x))))

(defun SLOT-EXISTS-P-ANY (instance name)
"returns t if the slotname exists with any package designator."
(let ((slots (mapcar #'strip-package (all-slots instance))))
  (member (strip-package name) slots :test #'equal)))

(defun QUOTED-SYMBOL-P (X)
"Predicate: returns t if the object is a quoted symbol."
(and (listp x)(equal (first x) 'quote)(symbolp (second x))))

(defun QUOTED-SYMBOL-FASD-FORM (instance)
"A FASD form for a quoted symbol."
`(QUOTE ,(second instance)))

(defun PAIR-SYMBOLS (instance)
(let ((slots (all-slots instance)))
 (pairlis (mapcar #'strip-package slots) slots)))

(defun FIND-PACKAGED-SLOTNAME (instance stripped)
"given the slotname without package, find the slotname WITH package."
  (let* ((choices (pair-symbols instance)))
    (rest (assoc stripped choices :test #'equal))))

(defun SLOT-VALUE-ANY (instance stripped)
"find the value of the real slot given the stripped name."
(setf stripped (strip-package stripped))
(let ((slotname (find-packaged-slotname instance stripped)))
  (when slotname (slot-value instance slotname))))

(defun GET-UNSAVEABLE-SLOTNAMES (instance)
  "Returns a list of the slotnames in instance, or the slotnames in the
   class of instance, which should not be saved."
  (slot-value-any instance 'unsaveable))

(defun GET-SLOT-VALUES (clos-instance)
  "given a pcl instance,constructs a plist of all the saveable
   slot/value pairs."
  (incf *global-instance-count*)
  (let ((slots (all-slots-and-values clos-instance))
	(unsaveable (get-unsaveable-slotnames clos-instance))
	(variable nil)
	(answers nil))
    (loop while slots do
	  (cond ((member (second slots) *seen* :test #'equal)
		 (setq variable (nth (position (second slots)
					       *seen*
					       :test #'equal)
				     *vars*))
		 (setq answers (append answers
			       `( ,(make-keyword (pop slots))
				  ',variable)))
		 (pop slots))
		((or (member (first slots) unsaveable :test #'equal)
       (member (first slots) *global-unsavable-slots* :test #'equal))
	       (pop slots)
	       (pop slots))
		(T (setq answers (append answers
				 `( ,(make-keyword (pop slots))
				    ,(get-fasd-form (pop slots))))))))
    answers))

(defun PUSHSYM (list &optional (label '.%%SYMBOL-LABEL%%.))
"label must match with special-marker-p, and must be upper-case."
  (push (newsym label) list))

(defun MAKESYMS (symbol min max &optional (pkg *package*))
(loop for count from min to max do (eval `(defvar
,(read-from-string (concatenate 'string (format nil "~A" symbol)
				(format nil "~A" count))
  pkg)))))

(defun WRITE-GLOBAL-HEADER (stream symbol min max
	   &optional (pkg-name (package-name *package*)))
(format stream (format nil "~%(EVAL-WHEN (COMPILE LOAD EVAL)
                       (DB:MAKESYMS '~A ~A ~A ~s))~%"
		       symbol min max pkg-name)))

(defun CONS-P (x)
  "ingenious predicate for testing whether something is a cons cell vs. a list.
   note that this returns nil for (LIST 'A 'B) whereas it returns T for (CONS 'A 'B)."
  (and (listp x) (null (listp (rest x)))))

(defun CONS-FASD-FORM (item)
  `(CONS ,(get-fasd-form (first item))
	 ,(get-fasd-form (rest item))))

(defun LIST-ARRAY (array)
  (list-array-aux array 0 nil))

(defun LIST-ARRAY-AUX (array level subscript-list)
  (let ((new-level (1+ level))
	(dims (array-dimensions array)))
    (loop for i from 0 to (1- (nth level dims))
	  collect
	  (cond ((equal level (1- (length dims)))
		 (let* ((aref-arg-list
			 (cons array (append subscript-list
					     (list i))))
			(array-val (apply #'aref aref-arg-list)))
		   (if (numberp array-val) array-val
		     (get-fasd-form array-val))))
		(T (list-array-aux array new-level
				   (append subscript-list (list i)))))
	  into temp finally (return (append '(list) temp)))))

(defun COERCE-2D-ARRAY (2d-array)
  (let ((rows (array-dimension 2d-array 0))
	(cols (array-dimension 2d-array 1)))
    (loop for x from 0 to (1- rows) collect
	  (loop for y from 0 to (1- cols) collect
		(aref 2d-array x y)) into answers
	  finally (return answers))))

(defun ARRAY-FASD-FORM (array)
  "this function return a make-array form."  
  (setf *print-array* T)
  `(make-array ,(get-fasd-form (array-dimensions array))
	       :element-type ',(array-element-type array)
	       :initial-contents ,(list-array array)))

;;; HASH TABLES...

(defun CREATE-HASH-TABLE (&key (test #'eql)
			       (size 67)
			       (rehash-size nil)
			       (rehash-threshold nil))
(let ((args (remove nil `(:size ,(get-fasd-form size)
	 :test ,test
		,@(when rehash-size (list :rehash-size (get-fasd-form rehash-size)))
		,@(when rehash-threshold (list :rehash-threshold (get-fasd-form rehash-threshold)))))))
  (apply #'make-hash-table args)))

(defun LOAD-HTAB (lst &key (test #'eql)
			       (size 67)
			       (rehash-size nil)
			       (rehash-threshold nil))
  (let ((htab (create-hash-table :test test
				    :size size
				    :rehash-size rehash-size
				    :rehash-threshold rehash-threshold)))
    (dolist (cell lst)
      (setf (gethash (first cell) htab)(eval (second cell))))
      htab))

(defun DESCRIBE-HTAB (htab)
"Utility to describe a hash table, printing out values."
  (describe htab)
  (maphash #'(lambda (key val)
	       (format t "~%~10t Key: ~A, ~45t Value: ~a.~%" key val))
	   htab))

(defun HTAB-FASD-FORM (htab)
"The dump form for hash tables."
  (let ((vals nil)
	(size (hash-table-size htab))
	(rehash-size (hash-table-rehash-size htab))
	(rehash-threshold (hash-table-rehash-threshold htab))
	(test (hash-table-test htab)))
    (maphash #'(lambda (key value)
		 (push (list key (GET-FASD-FORM value)) vals))
	     htab)
    (if (null vals) `(create-hash-table :size ,size
					     :rehash-size ,rehash-size
					     :test ',test
					     :rehash-threshold ,rehash-threshold)
      `(load-htab ',vals :size ,size 
		         :rehash-size ,rehash-size
			 :test ',test
			 :rehash-threshold ,rehash-threshold))))

;;; STRUCTURES (DEFSTRUCTS): MACHINE-DEPENDENT code!

;;;====================================ALLEGRO (MACINTOSH)  DEFSTRUCT ==========================================

;;; Beginning of the MAC

#+excl
(eval-when (eval compile load)
  
  (defun GET-DEFSTRUCT-TYPE (instance)
    (car (ccl::struct-ref instance 0)))
  
  (defun SET-DEFSTRUCT-VALUE (instance slotname value)
    (let* ((struct-slot-value-list (inspector::structure-slots instance))
           (slotname-position (1+ (position slotname 
                                            struct-slot-value-list 
                                            :key #'first))))
      (ccl::struct-set instance slotname-position value)))
  
  (defun GET-DEFSTRUCT-VALUE (instance slotname)
    "Given an instance of a defstruct, and the name of some slot, return the slots value."
    (let* ((struct-slot-value-list (inspector::structure-slots instance))
           (slotname-position (1+ (position slotname 
                                            struct-slot-value-list 
                                            :key #'first))))
      (ccl::struct-ref instance slotname-position)))
  
  (defsetf get-defstruct-value set-defstruct-value)

(defun GET-DEFSTRUCT-SLOT-ACCESSOR (instance slotname)
    ""
    (let* ((id (GET-DEFSTRUCT-TYPE instance)))
      (read-from-string (concatenate 'string (symbol-name id) "-"
				     (symbol-name slotname)))))
  
(defun GET-DEFSTRUCT-SLOTS-AND-VALS (instance)
  "Return a list of slots and values" ;Note that slots are not keyword names
  (labels ((interlock-lists (list1 list2 &optional interlocked-list)
                            (if (and list1 list2)
                              (cons (car list1) 
                                    (cons (car list2) 
                                          (interlock-lists (rest list1)
                                                           (rest list2)
                                                           interlocked-list)))
                              interlocked-list)))
    (let* ((struct-slot-value-list (inspector::structure-slots instance))
           (slot-list (mapcar #'first struct-slot-value-list))
           (vals-list '()))
      (dotimes (i (length slot-list))
        (push (ccl::struct-ref instance (1+ i)) vals-list))
      (setf vals-list (nreverse vals-list))
      (interlock-lists slot-list vals-list))))

(defun GET-DEFSTRUCT-SLOTS (instance)
  "Return a list of slots" ;Note that slots are not keyword names
  (let* ((struct-slot-value-list (inspector::structure-slots instance)))
    (mapcar #'first struct-slot-value-list)))
  
(defun STRUCTURE-FASD-FORM (instance)
  (labels ((interlock-lists (list1 list2 &optional interlocked-list)
                            (if (and list1 list2)
                              (cons (car list1) 
                                    (cons (car list2) 
                                          (interlock-lists (rest list1)
                                                           (rest list2)
                                                           interlocked-list)))
                              interlocked-list)))
    (let* ((ID (get-defstruct-type instance))
           (struct-slot-value-list (inspector::structure-slots instance))
           (slot-list (mapcar #'first struct-slot-value-list))
           (keyword-list (mapcar #'make-keyword slot-list))
           (vals-list '()))
      (dolist (slotname slot-list)
        (push (get-fasd-form (get-defstruct-value instance slotname)) vals-list))
      (setf vals-list (nreverse vals-list))
      (read-from-string (format nil "#S~S"
	`(,ID ,@(interlock-lists keyword-list vals-list)))))))
  
  ) ;;;; end of the mac eval-when....

;;;====================================LUCID DEFSTRUCT ==========================================
;;; Beginning of the Lucid eval-when....

#+lucid
(eval-when (eval compile load)

(defun STRUCTURE-P (x)
  (and (typep x 'structure)
       (NOT (VECTORP X))
       (not (typep x 'simple-vector))
       (not (typep x 'simple-array))
        (not (and (arrayp x)(> (array-rank x) 1)))))

(setf (symbol-function 'structurep) #'structure-p)

(defun STRUCTURE-INSTANCE-P (X)
""
(and (structurep x)(not (instance-p x))))

#+pcl
(defun STRUCTURE-TYPE-P (type)
  (let ((s-data (gethash type lucid::*defstructs*)))
 (or (and s-data (eq 'structure (system::structure-ref s-data 1 'defstruct)))
	(eq pcl::*structure-type* type))))

#+clos
(defun STRUCTURE-TYPE-P (type)
  (let ((s-data (gethash type lucid::*defstructs*)))
 (and s-data (eq 'structure (system::structure-ref s-data 1 'defstruct)))))

(defun STRUCTURE-TYPE-INCLUDED-TYPE-NAME (type)
  (let ((s-data (gethash type lucid::*defstructs*)))
    (and s-data (system:structure-ref s-data 6 'defstruct))))

(defun STRUCTURE-SLOTD-NAME (slotd)
  (first slotd))

(defun STRUCTURE-SLOTD-READER (slotd)
  (second slotd))

(defun STRUCTURE-SLOTD-WRITER (slotd)
  (third slotd))

(defun SET-DEFSTRUCT-VALUE (instance slotname value)
(EVAL `(setf (,(get-defstruct-slot-accessor instance slotname) ,instance) ,value)))

(defun GET-DEFSTRUCT-VALUE (instance slotname)
"Given an instance of a defstruct, and the name of some slot, return the slots value."
(apply (get-defstruct-slot-accessor instance slotname)(list instance)))

(defsetf get-defstruct-value set-defstruct-value)

(defun GET-DEFSTRUCT-SLOT-LOCATION (i name)
(position name (nreverse (get-defstruct-slots i))))

(defun GET-DEFSTRUCT-SLOT-ACCESSOR (instance slotname)
""
(let* ((id (type-of instance))
       (answer nil))
		     (multiple-value-bind (a accessor b c d)
		  (system:defstruct-slot-info id (get-defstruct-slot-location instance slotname))
			(setf answer accessor)
answer)))

(defun GET-DEFSTRUCT-SLOTS-AND-VALS (i)
  "given a defstruct instance, return a list of the slots and vals in that defstruct."
  (let ((id (type-of i)))
  (multiple-value-bind (indices a b c)
   (system:defstruct-info ID)
      (declare (ignore a b c))
      (let ((answers nil))
	(dotimes (count indices answers)
	  (multiple-value-bind (name d value e f)
	      (system:defstruct-slot-info ID count)
	      (declare (ignore d e f))
	      (push (cons name value) answers)
	      answers))))))

(defun GET-DEFSTRUCT-SLOTS (i)
  (let ((id (type-of i)))
  (multiple-value-bind (indices a b c)
   (system:defstruct-info ID)
      (declare (ignore a b c))
      (let ((answers nil))
	(dotimes (count indices answers)
	  (multiple-value-bind (name d value e f)
	      (system:defstruct-slot-info ID count)
	      (declare (ignore value d e f))
	      (push name answers)
	      answers))))))

(defun STRUCTURE-FASD-FORM (i)
"Lucid method of writing a fasd form for defstruct instances."
  (let ((id (type-of i)))
  (multiple-value-bind (indices a b c)
   (system:defstruct-info ID)
      (declare (ignore a b c))
      (let ((answers nil))
	(dotimes (count indices answers)
	  (multiple-value-bind (name accessor d e f)
	      (system:defstruct-slot-info ID count)
	      (declare (ignore d e f))
	      (push (make-keyword name) answers)
	      (push (get-fasd-form (funcall accessor i)) answers)))
     (read-from-string (format nil "#S~S" `(,ID ,@(nreverse answers))))))))

) ;;; end lucid eval-when....

;;;====================================SYMBOLICS DEFSTRUCT ==========================================
;;; beginning of symbolics eval-when....

#+Lispm
(eval-when (load eval compile)

(defun STRUCTURE-P (X)
(cli::structurep x))

(setf (symbol-function 'structurep) #'structure-p)

(defun STRUCTURE-INSTANCE-P (X)
"Predicate which returns t if the object is a STRUCTURE, NOT AN INSTANCE THEREOF!"
(and (structurep x)(not (instance-p x))))

(defun MAKE-DEFSTRUCT-BODY (slot-names slot-values)
  "makes a list of keyword value pairs appropriate for the body of a MAKE-x
   defstruct invokation. note the recursive call to fasd-form, which is where
   all the real work happens."
  (loop while (and slot-names slot-values) nconc
	(list (make-keyword (pop slot-names))
	      (get-fasd-form (pop slot-values)))
              into answers finally (return answers)))

(defun GET-DEFSTRUCT-DESCRIPTION (x)
(if (not (structurep x))(error "~A is not a structure! you lose...." x)
(si:get (or (AND (ARRAYP X) (si:NAMED-STRUCTURE-SYMBOL X))
			   (AND (LISTP X) (SYMBOLP (CAR X)) (CAR X)))
			       'si:DEFSTRUCT-DESCRIPTION)))
(defun STRUCTURE-FASD-FORM (x)
  "produces a list of the form (MAKE-x :A val1 :B val2 :C val3), note that the
   recursion to expand all these slots comes later in the fasd (typecase) expression."
  (let* ((desc (get-defstruct-description x))
	 (accessor-functions (mapcar #'(lambda (slot)
					 (first (reverse slot)))
				     (fourth desc)))
	 (slot-names (mapcar #'first (fourth desc)))
	 (maker (first (first (sixth desc))))
	 (slot-values (loop for acc in accessor-functions collect
			    (funcall acc x))))
    `(,maker ,@(make-defstruct-body slot-names slot-values))))

(defun GET-DEFSTRUCT-SLOTS-AND-VALS (x)
  "given a defstruct instance, return a list of the slots and vals in that defstruct."
  (let* ((desc (get-defstruct-description x))
	 (slot-names (mapcar #'first (fourth desc)))
	 (accessor-functions (mapcar #'(lambda (slot)
					 (first (reverse slot)))
				     (fourth desc)))
	 (slot-values (loop for acc in accessor-functions collect (funcall acc x))))
         (pairlis slot-names slot-values)))

(defun FORMATTED-DEFSTRUCT-SLOTS-AND-VALS (x)
  (let ((initial (get-defstruct-slots-and-vals x)))
    (loop for thang in initial nconc (list (make-keyword (first thang))
					   (get-fasd-form (rest thang))))))

(defun GET-DEFSTRUCT-SLOTS (x)
  "given a defstruct instance, return a list of the slots in that defstruct (no values)."
  (let* ((desc (get-defstruct-description x))
	 (slot-names (mapcar #'first (fourth desc))))
         slot-names))

(defun STRUCTURE-SLOTD-NAME (slotd)
  (first slotd))

(defun STRUCTURE-SLOTD-READER (slotd)
  (second slotd))

(defun STRUCTURE-SLOTD-WRITER (slotd)
  (third slotd))

)  ;;; end of the symbolics eval-when.,...

;;; machine independent defstruct functions...

;;;; Arrays & Vectors.

#+lucid
(defun %MAKE-ARRAY (dims &key (element-type T) initial-contents)
(make-array dims :element-type element-type
	         :initial-contents (eval initial-contents)))

#+lucid
(defun VECTOR-FASD-FORM (X)
  (let ((l (length x))
	(data (get-fasd-form (coerce x 'list))))
    `(%make-array ,l :element-type T
		  :initial-contents ',data)))

#+lispm
(defun VECTOR-FASD-FORM (array)
  `(make-array ,(get-fasd-form (array-dimensions array))
	       :element-type T
	       :initial-contents ,(list-array array)))

;;; Compiled functions.

#+ignore
(defun GET-COMPILED-FUNCTION-NAME (compiled-function)
(let ((ans nil))
(setq *readtable* (copy-readtable))
(set-dispatch-macro-character #\# #\' (function pseudo-quote-reader))
(set-dispatch-macro-character #\# #\< (function pseudo-quote-reader))
(setq ans (read-from-string (format nil "~A" compiled-function)))
(setq *readtable* (copy-readtable nil))
ans))

#+lucid
(defun FUNCTION-NAME (x)
"The 1th slot of the procedure struct is the function name in Lucid 4.0.
 i.e. SYS:PROCEDURE-SYMBOL <X>. SYS:PROCEDURE-SYMBOL is a constant, representing the
index to the function name within the procedures slots. (see wizard doc for 4.0 lucid."
(when (sys:procedurep x)(sys:procedure-ref x SYS:PROCEDURE-SYMBOL)))

#+lucid
(defun GET-COMPILED-FUNCTION-NAME (compiled-function)
(function-name compiled-function))

;;; Massive kludge for encountering READTABLES!!!

(defun READTABLE-FASD-FORM (i)
"Doesnt seem to be a good way to probe the internals of readtables, even
machine specific ways!!!!"
(declare (ignore i))
`(copy-readtable *readtable*))

;;; Massive kludge for pre-ansi hash table specs!!!!

#+lispm
(defun PARSE-HASH-TABLE-SPEC (htab)
(let ((ans nil))
(setq *readtable* (copy-readtable))
(set-dispatch-macro-character #\# #\' (function pseudo-quote-reader))
(set-dispatch-macro-character #\# #\< (function pseudo-quote-reader))
(setq ans (rest (butlast (read-from-string 
			   (concatenate 'string "(" (subseq (format nil "~a" htab) 8) ")")))))
(setq *readtable* (copy-readtable nil))
ans))

#+lispm
(defun HASH-TABLE-REHASH-SIZE (x)
(let ((spec (parse-hash-table-spec x)))
  (getf spec :rehash-size)))

#+lispm
(defun HASH-TABLE-REHASH-THRESHOLD (x)
(let ((spec (parse-hash-table-spec x)))
  (getf spec :rehash-threshold)))

;;; Functional FASD forms.....

;;; you may also need these....

#+class-save
(defun GENERIC-FUNCTION-FASD-FORM (instance)
"FASD Form for saving out generic functions..."
     (let ((name (generic-function-name instance))
	   (arglist (generic-function-lambda-list instance))
	   (documentation (generic-function-documentation instance)))
       `(OR (FIND-GENERIC-FUNCTION ',name)
	    (DEFGENERIC ,name ,arglist (:DOCUMENTATION ,(or documentation ""))))))

#+class-save
(defun METHOD-FASD-FORM (instance)
"Fasd form for saving out method objects."
     (LET* ((name (generic-function-name (method-generic-function instance)))
	    (qualifiers (method-qualifiers instance))
	    (specializers (method-specializers instance)))
	   `(FIND-METHOD (FUNCTION ,name)
			 (LIST ,@qualifiers)
			 (LIST ,@(DO-SPECIALIZERS specializers))
			 NIL)))

#+lucid
(defun COMPILED-FUNCTION-FASD-FORM (X)
  `(function ,(get-compiled-function-name x)))

#+lispm
(defun COMPILED-FUNCTION-FASD-FORM (X)
  (if (si:lexical-closure-p x) nil
  `(FUNCTION ,(si:compiled-function-name x))))

;;;; PCL/CLOS classes and instances.
;;;; NOTE: CLASS DEFINITIONS, WHEN READ IN, WILL OVERWRITE THE
;;;; CLASS DEFINITION PREVIOUSLY IN MEMORY. IF YOU DO NOT WANT THIS
;;;; TO HAPPEN, REPLACE 'DEFCLASS' BELOW WITH 'FIND CLASS' + the
;;;; APPROPRIATE ARGUMENTS!

#+class-save
(defun SAFE-CLASS-FASD-FORM (instance)
"This version of the class-fasd-form function WILL NOT overwrite 
 current class definitions with the same name. It is the one invoked
 by GET-FASD-FORM and SAVE-OBJECT."
 (let* ((name (class-name instance))
       (supertypes (get-class-superclasses instance))
       (slots (generate-class-slot-forms instance))
       (options (generate-class-options-form instance)))
`(OR (FIND-CLASS ',name)
     (DEFCLASS ,name ,supertypes ,slots ,@options))))

#+class-save
(defun CLASS-FASD-FORM (instance)
"This version of the class-fasd-form function WILL OVERWRITE 
 CURRENT CLASS DEFINITIONS WITH THE SAME NAME. Sunstitute a call to
 this one in GET-FASD-FORM and SAVE-OBJECT."
 (let* ((name (class-name instance))
       (supertypes (get-class-superclasses instance))
       (slots (generate-class-slot-forms instance))
       (options (generate-class-options-form instance)))
   (if (builtin-class-p instance) `(FIND-CLASS ',name)
     `(DEFCLASS ,name ,supertypes ,slots ,@options))))

(defun SYM< (a b)
"Predicate to see if symbol a is alphabetically before symbol b. T if a is."
(string< (format nil "~A" A)(format nil "~A" b)))

(defun SYMF< (a b)
"Predicate to see if symbol a is alphabetically before symbol b. T if a is."
(string< (format nil "~A" (FIRST A))(format nil "~A" (first b))))

(defun GET-ORDERED-SLOT-NAMES (I)
"Returns a list of the slot names of the instance, alphabetized."
(sort (all-slots i) #'sym<))

(defun FLATTEN (l)
(loop for cell in l nconc cell into answers
      finally (return answers)))

(defun PAIR-UP (l)
(loop while l collect (list (pop l)(pop l)) into answers
      finally (return (reverse answers))))

(defun ALPHABETIZE-BY-KEYWORD (lst)
(let ((alpha-cells (sort (pair-up lst) #'symf<)))
  (mapcar #'second alpha-cells)))

(defun GET-ORDERED-SLOT-VALUES (i)
"Gets the fasd forms out of the instance slot values, then alphabetizes them."
 (alphabetize-by-keyword (get-slot-values i)))

(defun %FILL-INSTANCE (i ordered-slot-values)
"Fills in the slots alphabetically. Assumes the slot values come into the function
 alphabetically ordered already: Returns the instance object."
(loop with osv = (copy-list ordered-slot-values)
      for name in (get-ordered-slot-names i) do
      (setf (slot-value i name)(EVAL (pop osv)))
      finally (return i)))

#+pcl
(defun %ALLOCATE-INSTANCE (class-object)
(pcl::allocate-instance class-object))

#+clos
(defun %ALLOCATE-INSTANCE (class-object)
(clos::allocate-instance class-object))

#+clos
(defun FILL-INSTANCE (classname &rest vals)
"New: allocates an instance given classname, the vals are the alphabetized list of
 slot values extracted from the target instance. returns the newly filled in instance."
(let* ((new (allocate-instance (find-class classname))))
  (%fill-instance new vals)
  new))

#+pcl
(defun FILL-INSTANCE (classname &rest vals)
"New: allocates an instance given classname, the vals are the alphabetized list of
 slot values extracted from the target instance. returns the newly filled in instance."
(let* ((new (pcl::allocate-instance (find-class classname))))
  (%fill-instance new vals)
  new))

(defun INSTANCE-FASD-FORM (instance)
"NEW VERSION. ATTEMPTS TO DEAL WITH SIRCULAR SLOT VALUE REFS,
Basic FASD form for clos/pcl instances. checks if the instance has a custom
 fasd form, binds it to a generated symbol name, recursively expands the
 instances contents."
(declare (special tmP))
  (if (has-fasd-form-p (instance-name instance))
      `(setq ,(first *vars*) ,(funcall #'(lambda (x)
					   (get-fasd-form x))
				       instance))
    `(let ((TMP (%allocate-instance (find-class  ',(instance-name instance)))))
    (setq ,(first *vars*) (%fill-instance TMP
	 (LIST ',@(get-ordered-slot-values instance))))
    (return-instance TMP))))

(defun EVAL-SLOTS (i)
"Goes through the local slots, insuring the contents of these slots have been
evaled properly. called by return-instance."
(loop for name in (all-slots i) do
(setf (slot-value i name)(eval (slot-value i name)))
finally (return i)))

(defun RETURN-INSTANCE (i)
"Called by the new instance-fasd-form: takes an instances, maps eval over its
slots, then returns the instance after having reassigned the slot values to
the evaled contents!"
(eval-slots i)
i)

;;; symbols.

(defun SPECIAL-MARKER-P (X &optional (label ".%%SYMBOL-LABEL%%."))
"label must match with pushsym, and must be upper-case."
  (search label (format nil "~A"  x) :test #'equal))

(defun SYMBOL-FASD-FORM (instance)
(read-from-string (format nil "'~s" instance)))

(defun REGULAR-FUNCTION-FASD-FORM (instance)
  `(FUNCTION ,instance))

(defun LONG-LIST-FASD-FORM (instance)
`(nconc ,@(make-list-forms (partition-long-list instance))))

(defun MAKE-LIST-FORMS (lists)
  (loop for list in lists collect (get-fasd-form list)))

(defun PARTITION-LONG-LIST (long-list &optional (limit 512))
"Some LISPs have a limit on the number of list components: this function partitions a
 list longer than the supplied limit appropriately for saving to file."
(loop while long-list collect
      (loop for count from 0 to (- limit 2) while long-list
	    collect (pop long-list))))

(defun CONSTANT-FASD-FORM (i)
  "anything that evals to itself, e.g. keywords, etc. just return that thing."
  i)

;;; the workhorse. NOTE: The case statement is very ORDER-DEPENDENT!
;;; If your version of CLOS supports specialization on ALL LISP types,
;;; you could write this as a set of FASD-FORM methods on the LISP types.
;;; This has not always been possible with PCL, thus the case statement.
;;; NOTE that a CONS is not necessarily a list! CONS-P distinguishes 
;;; between items such as (CONS 'A 'B) and (LIST 'A 'B).
;;;
;;; Notice that this version uses SAFE-CLASS-FASD-FORM to prevent class
;;; definition overwrite. Use CLASS-FASD-FORM below if you do not want this 
;;; behavior!

(defun GET-FASD-FORM (instance &key reset (CAL 512))
  (when reset (setf *seen* nil  *vars* nil))
  (cond ((null instance) nil)
	 ((equal instance T) T)
	((keywordp instance) instance)
;;;;;;	((quoted-symbol-p instance)(quoted-symbol-fasd-form instance))
	((symbolp instance)(symbol-fasd-form instance))
        #+class-save ((methodp instance)(method-fasd-form instance))
	#+class-save ((%generic-function-p instance)
		      (generic-function-fasd-form instance))
	((functionp instance) (compiled-function-fasd-form instance))
	((PATHNAMEP instance) (coerce instance 'string))
	#+class-save ((classp instance)(safe-class-fasd-form instance))
	((instance-p instance)
	 (if (member instance *seen* :test #'equal)
	     (symbol-fasd-form
	      (nth (position instance *seen* :test #'equal) *vars*))
	   (progn (push instance *seen*)
		  (setq *vars* (pushsym *vars*))
		  (instance-fasd-form instance))))
	((HASH-TABLE-P instance) (htab-fasd-form instance))
	((readtablep instance)(readtable-fasd-form instance))
	((STRUCTURE-P instance) (structure-fasd-form instance))
  	((or (CHARACTERP instance) (STRINGP instance))
	 instance)
	((typep instance 'VECTOR) (vector-fasd-form instance))
	((ARRAYP instance) (array-fasd-form instance))
	((CONS-P instance) (cons-fasd-form instance))
	((and (listp instance)(> (length instance) CAL))
	 (long-list-fasd-form instance))
	((LISTP instance)
	 `(list ,@(mapcar #'(lambda (thing)
			      (get-fasd-form thing)) instance)))
	((NUMBERP instance) instance)
	(T (progn (format t "couldn't parse ~A, with type ~A."
			  instance (type-of instance))
		  NIL))))

;;; ========= user defined fasd forms ==========

(defun HAS-FASD-FORM-P (class-name)
  "Predicate, returns t if a class has a user-defined FASD FORM method."
  (get class-name 'user::%%FASD-FORM-METHOD%%))

(defmacro DEFINE-FASD-FORM (class-name arglist &body body)
  "Macro to define a user-defined fasd-form for a given class-name.
   You could do this as two discrete steps, programmatically where you need it."
  `(progn (setf (get ',class-name 'user::%%fasd-form-method%%) T)
	  (defmethod FASD-FORM ,arglist ,@body)
	  ',class-name))

;;; ==================== Recursive reader for saved object files.... ==================

(defun READ-FILE (pathname &key
			   (variable '*db-input*))
  (cond ((load pathname :if-does-not-exist nil)
	 (setf *vars* nil
	       *seen* nil)
	 (setf (symbol-value variable)
	       (search-symbols (symbol-value variable)))
	 (eval variable))
	(T (format t  "the pathname ~a does not exist." pathname)
	   NIL)))

(defun SEARCH-SYMBOLS (instance)
  (cond ((null instance) nil)
	((symbolp instance)
	 ;; Now that we are keeping track of seen instances below,
	 ;; checking for variables may be redundant.
	 (cond ((not (special-marker-p instance)) instance)
	       ((member instance *vars* :test #'equal)
		(eval instance))
	       (T (push instance *vars*)
		  (search-symbols (eval instance)))
	       ))
	((functionp instance) instance)
	((pathnamep instance) instance)
	((instance-p instance)
	 ;; Searching symbols but not instances is not enough,
	 ;; since the root node (for example) is first seen
	 ;; as an object, not a special symbol.  You tend
	 ;; to get two copies of each node:  one for the
	 ;; (setq %%... (make-instance ...)), and one for
	 ;; the first (quote %%...).
	 (unless (member instance *seen* :test #'equal)
		 (push instance *seen*)
		 (let ((slots (all-slots-and-values instance)))
		   (loop while slots do
			 (setf (slot-value instance (pop slots))
			       (search-symbols (pop slots))))))
	 instance)
	((HASH-TABLE-P instance)
	 (maphash #'(lambda (key value)
		      (setf (gethash key instance)
			    (search-symbols value)))
		  instance) instance)
	((STRUCTURE-P instance)
	 #|
	 (dolist (slot (get-defstruct-slots instance))
		 (set-defstruct-slot instance slot
				     (search-symbols
				      (get-defstruct-value instance slot)
				      )))
         |#
	 instance
	 )
	((or (CHARACTERP instance)(STRINGP instance)) instance)
	((typep instance 'VECTOR)
	 (map 'vector #'(lambda (elt)
			  (search-symbols elt)) instance)
	 instance)
	((ARRAYP instance)
	 (let ((dims (array-dimensions instance)))
	   (case (length dims)
		 (1 (dotimes (i (first dims))
			     (setf (aref instance i)
				   (search-symbols (aref instance i)))))
		 (2 (dotimes (i (first dims))
			     (dotimes (j (second dims))
				      (setf (aref instance i j)
					    (search-symbols (aref instance i j))))))))
	 instance)
	((CONS-P instance)(cons (search-symbols (first instance)
						)
				(search-symbols (rest instance))))
	((LISTP instance)
	 (dotimes (count (length instance))
		  (setf (nth count instance)
			(search-symbols (nth count instance))))
	 instance)
	((NUMBERP instance) instance)
	(T (progn (format t "~%couldn't parse ~A, with type ~A.~%" instance
			  (type-of instance))
		  nil))))
  
;;; eof.

