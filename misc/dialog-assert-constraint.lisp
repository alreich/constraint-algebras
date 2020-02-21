(in-package "CM")

#|
(MAKE-INSTANCE 'DIALOG
  :WINDOW-TYPE
  :tool
  :WINDOW-TITLE
  "Assert Constraints"
  :VIEW-POSITION
  '(:RIGHT 110)
  :VIEW-SIZE
  #@(500 270)
  :VIEW-FONT
  '("Chicago" 12 :SRCOR :PLAIN)
  :VIEW-SUBVIEWS
  (LIST (MAKE-DIALOG-ITEM
         'STATIC-TEXT-DIALOG-ITEM
         #@(8 8)
         #@(127 16)
         "Selected Network:"
         'NIL)
        (MAKE-DIALOG-ITEM
         'STATIC-TEXT-DIALOG-ITEM
         #@(136 8)
         #@(352 13)
         "--- No network has been selected ---"
         'NIL)
        (MAKE-DIALOG-ITEM
         'STATIC-TEXT-DIALOG-ITEM
         #@(51 31)
         #@(52 16)
         "Object 1"
         'NIL
         :VIEW-FONT
         '("Chicago" 10 :SRCOR :PLAIN))
        (MAKE-DIALOG-ITEM
         'STATIC-TEXT-DIALOG-ITEM
         #@(211 31)
         #@(67 16)
         "Relation(s)"
         'NIL
         :VIEW-FONT
         '("Chicago" 10 :SRCOR :PLAIN))
        (MAKE-DIALOG-ITEM
         'STATIC-TEXT-DIALOG-ITEM
         #@(380 31)
         #@(52 16)
         "Object 2"
         'NIL
         :VIEW-FONT
         '("Chicago" 10 :SRCOR :PLAIN))
        (MAKE-DIALOG-ITEM
         'SEQUENCE-DIALOG-ITEM
         #@(8 48)
         #@(147 172)
         "Object1"
         'NIL
         :VIEW-FONT
         '("Chicago" 9 :SRCOR :PLAIN)
         :CELL-SIZE
         #@(131 12)
         :SELECTION-TYPE
         :SINGLE
         :TABLE-HSCROLLP
         T
         :TABLE-VSCROLLP
         T
         :TABLE-SEQUENCE
         '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24))
        (MAKE-DIALOG-ITEM
         'SEQUENCE-DIALOG-ITEM
         #@(172 48)
         #@(148 172)
         "Relations"
         'NIL
         :VIEW-FONT
         '("Chicago" 9 :SRCOR :PLAIN)
         :CELL-SIZE
         #@(132 12)
         :SELECTION-TYPE
         :SINGLE
         :TABLE-HSCROLLP
         T
         :TABLE-VSCROLLP
         T
         :TABLE-SEQUENCE
         '(0 1 2))
        (MAKE-DIALOG-ITEM
         'SEQUENCE-DIALOG-ITEM
         #@(338 48)
         #@(148 172)
         "Object2"
         'NIL
         :VIEW-FONT
         '("Chicago" 9 :SRCOR :PLAIN)
         :CELL-SIZE
         #@(132 12)
         :SELECTION-TYPE
         :SINGLE
         :TABLE-HSCROLLP
         T
         :TABLE-VSCROLLP
         T
         :TABLE-SEQUENCE
         '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))
|#

(defun ASSERT-CONSTRAINT-DIALOG ()
  (let (
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
          "---"
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
          #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Object-1 List Action."))
          :view-font '("Chicago" 9 :SRCOR :PLAIN)
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
          :table-sequence
          '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
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
           #'(lambda (item) item (format t "~%Assert Constraint Action."))
           :default-button t
           ))

         (NEW-OBJ-BUTTON

          (make-dialog-item
           'button-dialog-item
           #@(248 236)
           #@(85 16)
           "New Object"
           #'(lambda (item) item (format t "~%New Object Action."))
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
      :window-title "Assert Constraints"
      :view-position '(:right 110)
      :view-size #@(500 270)
      :view-font '("Chicago" 12 :SRCOR :PLAIN)
      :close-box-p nil
      :view-subviews
      (list SEL-NET-LABEL SEL-NET-NAME
            OBJ-1-LABEL REL-LABEL OBJ-2-LABEL
            OBJ-1-LIST  REL-LIST  OBJ-2-LIST
            ASSERT-BUTTON NEW-OBJ-BUTTON PROP-BUTTON CANCEL-BUTTON HELP-BUTTON)
      )
    ))
