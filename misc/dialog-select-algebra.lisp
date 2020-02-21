(in-package "CM")

;;; This function is only used by the code in 'mac-interface.lisp'

(defun ALGEBRA-NAMES ()
  "Returns a list of all registered algebras."
  (mapcar #'(lambda (alg)
              (get-algebra-name alg))
          *algebras*))


(defmethod SELECTED-ITEM ((item sequence-dialog-item))
  (let ((selected-cell (first (selected-cells item))))
    (if selected-cell
      (cell-contents item selected-cell))))


(defun NETWORK-NAME-DIALOG ()
  (let ((name
         (catch-cancel 
           (get-string-from-user
            "Give the constraint network a unique name, or use the default name below."
            :initial-string (string (gensym "Constraint-Network-"))
            :ok-text "OK"
            :cancel-text "Cancel"
            ))))
    (if (equal name :cancel) nil name)))

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
           :table-hscrollp t
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