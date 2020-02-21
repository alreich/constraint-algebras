;;;-*- Mode: Lisp; Package: CONSTRAINT-MAINTENANCE -*-

;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;;                   TEMPORAL CONSTRAINT MANAGER (TCM)
;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(in-package "CONSTRAINT-MAINTENANCE")


;;;---------------------------------------------------------------------------

(defclass TCM-DIALOG (dialog)
  (
   (tcm-object
    :initarg :tcm-object
    :accessor tcm-object
    :initform nil
    :documentation "The temporal constraint network object that contains this dialog.")
   ))

(defmethod CLOSE-TC-MANAGER ((subview-item button-dialog-item))
  (if 
    (y-or-n-dialog
     "OK to quit the Temporal Constraint Manager?"
     :yes-text "Yes"
     :no-text "No"
     :cancel-text nil)
    #-CECOM
    (window-close (view-container subview-item))
    #+CECOM
    (progn
      (window-close (view-container subview-item))
      (quit))
    ))

;;;---------------------------------------------------------------------------

(defun TC-MANAGER-DIALOG ()          ; The top-level dialog
  (let* (
         (TCN-LIST
          (make-dialog-item
           'sequence-dialog-item
           #@(9 26) #@(282 124)
           "TCN List"
           'nil
           :cell-size #@(266 16)
           :selection-type :disjoint
           :table-hscrollp t
           :table-vscrollp t
           :table-sequence 'nil
           :table-print-function #'(lambda (tcn stream)
                                     (format stream "~A" (network-name tcn)))
           ))
         (TEXT-LABEL-1
          (make-dialog-item
           'static-text-dialog-item
           #@(45 7) #@(216 16)
           "Select networks from this list"
           'nil
           ))
         (COPY-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(115 194) #@(62 16)
           "Copy"
           #'(lambda (item) item (format t "~%Copy network action."))
           :default-button nil
           ))
         (CREATE-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(115 167) #@(61 16)
           "Create"
           #'(lambda (item)
               (set-table-sequence
                tcn-list
                (tcns 
                 (create-tcn (tc-mgr-of-subviews-container item))))
               )
           :default-button nil
           ))
         (MERGE-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(115 223) #@(62 16)
           "Merge"
           #'(lambda (item) item (format t "~%Merge networks action."))
           :dialog-item-enabled-p nil
           :default-button nil
           ))
         (EDIT-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(35 270) #@(62 16)
           "Edit"
           #'(lambda (item)
               (declare (ignore item))
               (tc-edit-dialog (selected-item tcn-list))
               )
           :view-nick-name 'edit
           :default-button t
           ))
         (SHOW-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(212 167) #@(62 16)
           "Show"
           #'(lambda (item) item (format t "~%Show constraints action."))
           :default-button nil
           ))
         (DELETE-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(212 194) #@(62 16)
           "Delete"
           #'(lambda (item) item (format t "~%Delete constraint action."))
           :dialog-item-enabled-p nil
           :default-button nil
           ))
         (QUIT-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(118 270) #@(62 16)
           "Quit"
           #'(lambda (item)
               (close-tc-manager item))
           :default-button nil
           ))
         (HELP-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(201 270) #@(62 16)
           "Help"
           #'(lambda (item)
               (format t "~%~S" (class-name item))
               (message-dialog
                "HELP ON MANAGING CONSTRAINT NETWORKS:
This dialog lets you create and load temporal constraint networks. To assert constraints in a network click \"Edit\".  To see a network's contents click \"Show\"."
                :ok-text
                "Quit Help"
                :size 9175375
                ))
           :default-button nil
           ))
         (SAVE-AS-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(18 223) #@(62 16)
           "Save As"
           #'(lambda (item) item (format t "~%Merge networks action."))
           :view-nick-name 'merge
           :dialog-item-enabled-p nil
           :default-button nil
           ))
         (LOAD-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(19 167) #@(61 16)
           "Load"
           #'(lambda (item) item (format t "~%Create network action."))
           :view-nick-name 'create
           :dialog-item-enabled-p nil
           :default-button nil
           ))
         (SAVE-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(18 194) #@(62 16)
           "Save"
           #'(lambda (item) item (format t "~%Copy network action."))
           :view-nick-name 'copy
           :dialog-item-enabled-p nil
           :default-button nil
           ))
         (RENAME-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(212 223) #@(62 16)
           "Rename"
           #'(lambda (item) item (format t "~%Rename network action."))
           :view-nick-name 'rename
           :default-button nil
           ))
         )
    (make-instance 'tcm-dialog
      :window-type :tool
      :window-title "Temporal Constraint Manager"
      ;; :view-position #@(337 130)
      :view-position #@(337 200)
      :view-size #@(300 300)
      :close-box-p nil
      :view-font '("Chicago" 12 :srcor :plain)
      :view-subviews
      (list TCN-LIST TEXT-LABEL-1 COPY-BUTTON CREATE-BUTTON MERGE-BUTTON
            EDIT-BUTTON SHOW-BUTTON DELETE-BUTTON QUIT-BUTTON HELP-BUTTON
            SAVE-AS-BUTTON LOAD-BUTTON SAVE-BUTTON RENAME-BUTTON
            ))))

;;;---------------------------------------------------------------------------

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
    :initform (tc-manager-dialog)
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


;;;---------------------------------------------------------------------------

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

;;;---------------------------------------------------------------------------

(defmethod SELECTED-ITEM ((item sequence-dialog-item))
  (let ((selected-cell (first (selected-cells item))))
    (if selected-cell
      (cell-contents item selected-cell))))

(defmethod SELECTED-ITEMS ((item sequence-dialog-item))
  (list (car (selected-item item))))

;;;---------------------------------------------------------------------------

(defun NETWORK-NAME-DIALOG (algebra-name)
  (let ((name
         (catch-cancel 
           (get-string-from-user
            "Give the constraint network a unique name, or use the default name below."
            :initial-string (string (gensym (concatenate 'string
                                                         (string algebra-name)
                                                         "-NET-")))
            :ok-text "OK"
            :cancel-text "Cancel"
            ))))
    (if (equal name :cancel) nil name)))

;;;---------------------------------------------------------------------------

(defun GET-TCN-ALGEBRA-NAME ()
  "Pop-up a dialog box that the user can select an algebra from.  Return the
   algebra's name, or return NIL if the user cancels the dialog."
  (let ((algebra-type (algebra-name-dialog)))
    (if (not (equal algebra-type :closed)) algebra-type)))
      

(defun ALGEBRA-NAME-DIALOG ()
  (let* (
         
         (ALGEBRA-LIST
          
          (make-dialog-item
           'sequence-dialog-item
           #@(7 21) #@(285 95)
           "Algebras"
           nil
           :view-nick-name 'alg-list
           :cell-size #@(269 16)
           :selection-type :single
           :table-hscrollp nil
           :table-vscrollp t
           :table-sequence (algebra-names)
           ))
         
         (TEXT-ITEM
          
          (make-dialog-item
           'static-text-dialog-item
           #@(21 3) #@(253 16)
           "Select an algebra from the list below"
           'nil
           ;; :part-color-list '(:text 1778431)
           ))
         
         (OK-BUTTON
          
          (make-dialog-item
           'button-dialog-item
           #@(20 126) #@(62 16)
           "OK"
           #'(lambda (item)
               (let ((sel-item (selected-item ALGEBRA-LIST)))
                 (when sel-item
                   (return-from-modal-dialog sel-item)
                   (window-close (view-container item)))))
           ;; :part-color-list '(:body 9764702 :text 0)
           :default-button t
           ))
         
         (CANCEL-BUTTON
          
          (make-dialog-item
           'button-dialog-item
           #@(122 126) #@(62 16)
           "Cancel"
           #'(lambda (item)
               (window-close (view-container item)))
           ;; :part-color-list '(:text 0 :body 16748950)
           :default-button nil
           ))
         
         (HELP-BUTTON
          
          (make-dialog-item
           'button-dialog-item
           #@(221 126) #@(62 16)
           "Help"
           #'(lambda (item)
               item
               (message-dialog
                "HELP ON ALGEBRA SELECTION:
This dialog lets you choose a constraint algebra to be used to create a temporal constraint network. Select one item in the list, then click \"OK\"; otherwise click \"Cancel\"."
                :ok-text "Quit Help"
                :size  #@(335 140))
               )
           :dialog-item-enabled-p t
           ;; :part-color-list '(:text 0 :body 6225407)
           :default-button nil
           )))
    
    (modal-dialog
     (make-instance 'dialog
       :window-type :double-edge-box
       :window-title "Constraint Algebras"
       :view-position #@(97 176)
       :view-size #@(300 150)
       :view-font '("Chicago" 12 :SRCOR :PLAIN)
       :view-subviews
       (list OK-BUTTON CANCEL-BUTTON HELP-BUTTON ALGEBRA-LIST TEXT-ITEM)))))

;;;---------------------------------------------------------------------------



(defun TC-EDIT-DIALOG (network)
  (let* ((NET-NAME (if network
                     (network-name network)
                     "---"))
         (ALG           (find-algebra (get-alg-name network)))
         (RELS          (get-algebra-rels alg))
         (ABBREVS       (get-algebra-abbrevs alg))
         (UNABBREV-RELS (mapcar #'(lambda (rel)
                                    (unabbrev-relation rel abbrevs))
                                rels))
         (REL-PAIRS     (pairlis rels unabbrev-rels))

         (SEL-NET-LABEL
          (make-dialog-item
           'static-text-dialog-item
           #@(8 8) #@(127 16)
           "Selected Network:"
           'NIL
           ))
         (SEL-NET-NAME
          (make-dialog-item
           'static-text-dialog-item
           #@(136 8) #@(352 13)
           net-name
           'nil
           ))
         (OBJ-1-LABEL
          (make-dialog-item
           'static-text-dialog-item
           #@(51 31) #@(52 16)
           "Object 1"
           'nil
           :view-font '("Chicago" 10 :SRCOR :PLAIN)
           ))
         (REL-LABEL
          (make-dialog-item
           'static-text-dialog-item
           #@(211 31) #@(67 16)
           "Relations"
           'nil
           :view-font '("Chicago" 10 :SRCOR :PLAIN)
           ))
         (OBJ-2-LABEL
          (make-dialog-item
           'static-text-dialog-item
           #@(380 31) #@(52 16)
           "Object 2"
           'nil
           :view-font '("Chicago" 10 :SRCOR :PLAIN)
           ))
         (OBJ-1-LIST
          (make-dialog-item
           'sequence-dialog-item
           #@(8 48) #@(148 172)
           "Object1"
           #'(lambda (item) item (format t "~%Object-1 List Action."))
           :view-font '("Chicago" 9 :srcor :plain)
           :cell-size #@(130 12)
           :selection-type :single
           :table-hscrollp t
           :table-vscrollp t
           :table-sequence
           '()
           ))
         (REL-LIST
          (make-dialog-item
           'sequence-dialog-item
           #@(172 48) #@(148 172)
           "Relations"
           #'(lambda (item) item (format t "~%Relation List Action."))
           :view-font '("Chicago" 9 :SRCOR :PLAIN)
           :cell-size #@(130 12)
           :selection-type :disjoint
           :table-hscrollp t
           :table-vscrollp t
           :table-sequence REL-PAIRS
           :table-print-function
           #'(lambda (rel-pair stream)
               (let ((short-name (car rel-pair))
                     (long-name  (cdr rel-pair)))
                 (if long-name
                   (format stream "~A  (~A)"
                           short-name long-name)
                   (format stream "~A" short-name))))
           ))
         (OBJ-2-LIST
          (make-dialog-item
           'sequence-dialog-item
           #@(338 48)  #@(148 172)
           "Object2"
           #'(lambda (item) item (format t "~%Object-2 List Action."))
           :view-font '("Chicago" 9 :SRCOR :PLAIN)
           :cell-size #@(130 12)
           :selection-type :single
           :table-hscrollp t
           :table-vscrollp t
           :table-sequence
           '()
           ))
         (ASSERT-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(16 236) #@(58 16)
           "Assert"
           #'(lambda (item)
               (declare (ignore item))
               (let* ((obj1 (selected-item  obj-1-list))
                      (obj2 (selected-item  obj-2-list))
                      (rels (selected-items rel-list))
                      (con  (list rels obj1 obj2)))
                 (assert-constraint con network :propagate-p nil)
                 ))
           :default-button t
           ))
         (NEW-OBJ-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(248 236)
           #@(85 16)
           "New Object"
           #'(lambda (item)
               (declare (ignore item))
               (new-object network)
               (let ((objs (get-constrained-objects network)))
                 (set-table-sequence obj-1-list objs)
                 (set-table-sequence obj-2-list objs)
                 ))
           :view-nick-name 'new-obj
           :default-button nil
           ))
         (PROP-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(157 236)
           #@(76 16)
           "Propagate"
           #'(lambda (item) item (format t "~%Propagate Action."))
           :view-nick-name 'propagate
           :default-button nil
           ))
         (CANCEL-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(350 236) #@(62 16)
           "Cancel"
           #'(lambda (item)
               (window-close (view-container item)))
           ;; :part-color-list '(:text 0 :body 16748950)
           :default-button nil
           ))
         (HELP-BUTTON
          (make-dialog-item
           'button-dialog-item
           #@(427 236) #@(62 16)
           "Help"
           #'(lambda (item)
               item
               (message-dialog
                "HELP ON CONSTRAINT ASSERTION:
This dialog lets you assert constraints.  Pick one object from each object list and pick one or more relations, then click \"ASSERT\"; otherwise click \"Cancel\"."
                :ok-text "Quit Help"
                :size  #@(335 140))
               )
           :dialog-item-enabled-p t
           ;; :part-color-list '(:text 0 :body 6225407)
           :default-button nil
           ))
         )
    (make-instance 'dialog
      :window-type :tool
      :window-title "Temporal Constraint Editor"
      :view-position '(:right 110)
      :view-size #@(500 270)
      :view-font '("Chicago" 12 :SRCOR :PLAIN)
      :close-box-p nil
      :view-subviews
      (list SEL-NET-LABEL SEL-NET-NAME
            OBJ-1-LABEL REL-LABEL OBJ-2-LABEL
            OBJ-1-LIST  REL-LIST  OBJ-2-LIST
            ASSERT-BUTTON NEW-OBJ-BUTTON PROP-BUTTON CANCEL-BUTTON HELP-BUTTON)
      )))

;;;---------------------------------------------------------------------------

(defun TC-MGR-OF-SUBVIEWS-CONTAINER (subview)
  "Given a subview of a TCM-DIALOG object, get the TEMPORAL-CONSTRAINT-MANAGER
   object that contains the TCM-DIALOG."
  (tcm-object (view-container subview)))


(defmethod CREATE-TCN ((tc-mgr temporal-constraint-manager))
  (let ((algebra-name (get-tcn-algebra-name)))
    (when algebra-name
      (let ((network-name (network-name-dialog algebra-name)))
        (when network-name
          (let ((net (initialize-constraint-network algebra-name)))
            (setf (network-name net) network-name)      ; A SIDE EFFECT !
            (register-constraint-network net tc-mgr))))))       ; ANOTHER, JEEZ !!!
  tc-mgr)

;;;---------------------------------------------------------------------------

(defun ELIMINATE-SYNONYMS (head tail)
  ;; See example below.
  (let ((datum (cdr head)))
    (remove-if #'(lambda (pair)
                   (= datum (cdr pair)))
               tail)))

;;; EXAMPLE
;;; (setf test '((a . 1) (b . 2) (c . 1) (d . 2) (e . 1)))
;;; (eliminate-synonyms (first test) (rest test))
;;; ==> ((B . 2) (D . 2))


(defmethod GET-OBJECT-TYPES ((tcn constraint-network))
  "Returns a list of the types of objects that the TCN knows how
   do deal with (e.g., interval, point).  Synonyms of types are
   not returned (e.g., int, period, per, pt, etc)."
  (let* ((equalities (get-algebra-equalities
                      (find-algebra
                       (get-alg-name tcn))))
         (obj-types  nil))
    (loop until (null equalities)
          do (let ((first-pair (first equalities)))
               (pushnew (car first-pair) obj-types)
               (setf equalities
                     (eliminate-synonyms first-pair (rest equalities)))
               ))
    obj-types))

;;;---------------------------------------------------------------------------

(defmethod GET-OBJECT-TYPE ((object-types list))
  "Pop-up a dialog box that the user can select an algebra from.  Return the
   algebra's name, or return NIL if the user cancels the dialog."
  (let ((object-type (object-type-dialog object-types)))
    (if (not (equal object-type :closed)) object-type)))


(defun OBJECT-TYPE-DIALOG (object-types)
  "Displays a list of object types for the user to select from."
  (let* (
         
         (OBJECT-LIST
          
          (make-dialog-item
           'sequence-dialog-item
           #@(7 21) #@(285 95)
           "Objects"
           nil
           :view-nick-name 'obj-type-list
           :cell-size #@(269 16)
           :selection-type :single
           :table-hscrollp nil
           :table-vscrollp t
           :table-sequence object-types
           ))
         
         (TEXT-ITEM
          
          (make-dialog-item
           'static-text-dialog-item
           ;; #@(21 3) #@(253 16)
           #@(50 3) #@(283 16)
           "Select a temporal object type"
           'nil
           ))
         
         (OK-BUTTON
          
          (make-dialog-item
           'button-dialog-item
           #@(20 126) #@(62 16)
           "OK"
           #'(lambda (item)
               (let ((sel-item (selected-item OBJECT-LIST)))
                 (when sel-item
                   (return-from-modal-dialog sel-item)
                   (window-close (view-container item)))))
           :default-button t
           ))
         
         (CANCEL-BUTTON
          
          (make-dialog-item
           'button-dialog-item
           #@(122 126) #@(62 16)
           "Cancel"
           #'(lambda (item)
               (window-close (view-container item)))
           :default-button nil
           ))
         
         (HELP-BUTTON
          
          (make-dialog-item
           'button-dialog-item
           #@(221 126) #@(62 16)
           "Help"
           #'(lambda (item)
               item
               (message-dialog
                "HELP ON OBJECT TYPE SELECTION:
This dialog lets you choose an object type to be used to create a temporal object. Select one item in the list, then click \"OK\"; otherwise click \"Cancel\"."
                :ok-text "Quit Help"
                :size  #@(335 140))
               )
           :dialog-item-enabled-p t
           :default-button nil
           )))
    
    (modal-dialog
     (make-instance 'dialog
       :window-type :double-edge-box
       :window-title "Temporal Object Types"
       :view-position #@(97 176)
       :view-size #@(300 150)
       :view-font '("Chicago" 12 :SRCOR :PLAIN)
       :view-subviews
       (list OK-BUTTON CANCEL-BUTTON HELP-BUTTON OBJECT-LIST TEXT-ITEM)))))

;;;---------------------------------------------------------------------------

(defun OBJECT-NAME-DIALOG (object-type)
  "Displays a dialog box for the user to enter an object name in.  Provides
   a default name based on the object's type."
  (let ((name
         (catch-cancel 
           (get-string-from-user
            "Give the temporal object a unique name, or use the default name below."
            :initial-string (string (gensym (concatenate 'string
                                                         (string object-type)
                                                         "-OBJ-")))
            :ok-text "OK"
            :cancel-text "Cancel"
            ))))
    (if (equal name :cancel) nil name)))

;;;---------------------------------------------------------------------------

(defmethod NEW-OBJECT ((tcn constraint-network))
  (let ((object-types (get-object-types tcn))
        (object-type  nil))
    (if (= (length object-types) 1)
      (setf object-type (first object-types))
      (setf object-type (get-object-type object-types)))
    (when object-type
      (let ((object-name (object-name-dialog object-type)))
        (when object-name
          (register-constrained-object object-name object-type tcn)
          )))))

;;;---------------------------------------------------------------------------
;;;                              END OF FILE
;;;---------------------------------------------------------------------------

