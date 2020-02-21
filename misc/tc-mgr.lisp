;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                   TEMPORAL CONSTRAINT MANAGER (TCM)
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(in-package "CONSTRAINT-MAINTENANCE")


(defclass TEMPORAL-CONSTRAINT-MANAGER ()
  (
   (tcns
    :initarg :tcns
    :accessor tcns
    :initform nil
    :documentation "A list of temporal constraint networks.")
   (manager
    :initarg :manager
    :accessor manager
    :initform (create-tc-mgr-dialog)
    :documentation "The temporal constraint manager.")
   (editor
    :initarg :editor
    :accessor editor
    :initform nil
    :documentation "The temporal constraint editor.")
   (visualizer
    :initarg :visualizer
    :accessor visualizer
    :initform nil
    :documentation "The temporal constraint visualizer.")
   ))


(defun TCM ()                           ; Main start-up function
  "Creates a TEMPORAL-CONSTRAINT-NETWORK object, which in turn creates
   a TCM-DIALOG object, and then makes the latter point back to the former."
  (let* ((tcm-obj    (make-instance 'temporal-constraint-manager))
         (mgr-dialog (manager tcm-obj)))
    (setf (tcm-object mgr-dialog) tcm-obj)))


(defun FIND-CONSTRAINT-NETWORK (name tc-mgr)
  (find name (tcns tc-mgr) :test #'equal :key #'network-name))


(defun DELETE-CONSTRAINT-NETWORK (network tc-mgr)
  (setf (tcns tc-mgr)
        (remove (network-name network)
                (tcns tc-mgr)
                :test #'equal
                :key  #'network-name)))


(defun REGISTER-CONSTRAINT-NETWORK (network tc-mgr)
  "Install network in list of existing networks.  Removes previous versions."
  (delete-constraint-network network tc-mgr)
  (push network (tcns tc-mgr)))


(defmethod CREATE-TCN ((tc-mgr temporal-constraint-manager))
  (let* ((algebra-type (algebra-name-dialog))
         (network-name (network-name-dialog)))
    (if (equal algebra-type :closed)
      nil
      (setf (get-tcns tc-mgr)
            (push (initialize-constraint-network algebra-type)
                  (get-tcns tc-mgr))))))

#|
(defmethod CREATE-TCN ((tc-mgr temporal-constraint-manager))
  (let ((net nil))
    (until net
  

(network-name-dialog)))
       (network-name nil))
|#
