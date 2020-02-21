;;;-*- Mode: Lisp; Package: (CONSTRAINT-MAINTENANCE) -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; load-cms.lisp
;;;
;;; Load this file and evaluate (cm::load-cms)
;;;
;;; Copyright 1994 Alfred J. Reich. All rights reserved.

;;; Permission is given to use, copy, and modify this software provided that
;;; this copyright notice is attached to all derivative works.  This
;;; software is provided "as is". Alfred J. Reich makes no warranty or
;;; representation, either express or implied, with respect to this
;;; software, its quality, accuracy, merchantability, or fitness for a
;;; particular purpose.


(defpackage "CONSTRAINT-MAINTENANCE"
  (:use "COMMON-LISP")
  (:nicknames "CM")
  (:export DEFINE-ALGEBRA FIND-ALGEBRA DELETE-ALGEBRA DUMP-ALGEBRA)
  (:import-from "INTERVAL-MATH" INT/ I<<= INT= INT& INT* MAKE-INTERVAL))

(in-package "CM")

(pushnew "cms:" *module-search-path* :test 'equalp)

(defun compile-if-needed (file &optional force)
  (let ((lisp (merge-pathnames file ".lisp"))
        (fasl (merge-pathnames file ".fasl")))
    (when (or force
              (not (probe-file fasl))
              (> (file-write-date lisp) (file-write-date fasl)))
      (compile-file lisp :verbose t))))

(defun compile-and-load (file &optional force-compile)
  (compile-if-needed file force-compile)
  (load file :verbose t))

(defparameter *wood-files*
  '("block-io-mcl" "split-lfun" "disk-cache" "woodequ" "disk-cache-accessors"
    "disk-cache-inspector" "persistent-heap" "btrees" "persistent-clos"
    "recovery"))

(defun load-wood (&optional force-compile)
  (with-compilation-unit ()
    (compile-if-needed "wood:load-wood")
    (dolist (file *wood-files*)
      (compile-and-load (merge-pathnames file "wood:") force-compile))))

; This should be called only after load-wood.
; It compiles the changed files
(defun compile-wood ()
  (with-compilation-unit ()
    (compile-if-needed "wood:load-wood")
    (dolist (file *wood-files*)
      (compile-if-needed (merge-pathnames file "wood:")))))


