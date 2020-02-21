(in-package "CL-USER")

;;; A script for creating the Temporal Constraint Manager application

(load "cms:load-file")

(pushnew :CECOM *FEATURES*)             ; Quit LISP when quiting TCM
                                        ;  (see "mac-interface.lisp")

(pushnew :DERIVE-CONSTRAINT-ALGEBRA *FEATURES*)         ; Load derivation code

(defvar *TCM* nil)                      ; Holds the TCM.

(defun START-TCM ()                     ; Starts the TCM running.
  (setf *TCM* (cm::tcm)))

(setf *LISP-STARTUP-FUNCTIONS* (list 'start-tcm))

(setf *LISTENER-WINDOW-SIZE* (make-point 500 100))      ; Small listener window

(save-application "cms:TCMgr" :excise-compiler t)       ; Builds the application
