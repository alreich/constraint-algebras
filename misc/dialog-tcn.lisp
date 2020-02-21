(in-package "CM")

#|
(set-part-color tcm :frame 1842687)
(set-part-color tcm :title-bar 16775206)
|#


(setf tcm
      (MAKE-INSTANCE 'COLOR-DIALOG
        :WINDOW-TYPE
        :TOOL
        :WINDOW-TITLE
        "Temporal Constraint Manager"
        :VIEW-POSITION
        #@(420 198)
        :VIEW-SIZE
        #@(300 300)
        :CLOSE-BOX-P
        NIL
        :VIEW-FONT
        '("Chicago" 12 :SRCOR :PLAIN)
        :VIEW-SUBVIEWS
        (LIST (MAKE-DIALOG-ITEM
               'SEQUENCE-DIALOG-ITEM
               #@(9 26)
               #@(282 124)
               "Untitled"
               'NIL
               :CELL-SIZE
               #@(266 16)
               :SELECTION-TYPE
               :SINGLE
               :TABLE-HSCROLLP
               T
               :TABLE-VSCROLLP
               T
               :TABLE-SEQUENCE
               'NIL)
              (MAKE-DIALOG-ITEM
               'STATIC-TEXT-DIALOG-ITEM
               #@(45 7)
               #@(216 16)
               "Select network(s) from this list"
               'NIL
               :PART-COLOR-LIST
               '(:TEXT 1842687))
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(95 214)
               #@(62 16)
               "Copy"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Copy network action."))
               :PART-COLOR-LIST
               '(:BODY 16776732)
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(95 190)
               #@(61 16)
               "Create"
               #'(LAMBDA (ITEM)
                   ITEM
                   (FORMAT T "~%Create network action.")
                   (create-tcn *TC-MGR*))
               :PART-COLOR-LIST
               '(:BODY 16776732)
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(95 237)
               #@(62 16)
               "Merge"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Merge networks action."))
               :PART-COLOR-LIST
               '(:BODY 16776732)
               :DIALOG-ITEM-ENABLED-P
               NIL
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(212 190)
               #@(62 16)
               "Add"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Add constraint action."))
               :PART-COLOR-LIST
               '(:BODY 16776732)
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(212 237)
               #@(62 16)
               "Show"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Show constraints action."))
               :PART-COLOR-LIST
               '(:BODY 16776732)
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(212 213)
               #@(62 16)
               "Delete"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Delete constraint action."))
               :PART-COLOR-LIST
               '(:BODY 16776732)
               :DIALOG-ITEM-ENABLED-P
               NIL
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'STATIC-TEXT-DIALOG-ITEM
               #@(55 157)
               #@(72 25)
               "Network
Operations"
               'NIL
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN)
               :PART-COLOR-LIST
               '(:TEXT 1973759))
              (MAKE-DIALOG-ITEM
               'STATIC-TEXT-DIALOG-ITEM
               #@(211 157)
               #@(72 25)
               "Constraint
Operations"
               'NIL
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN)
               :PART-COLOR-LIST
               '(:TEXT 1711871))
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(69 274)
               #@(62 16)
               "Quit"
               #'(LAMBDA (ITEM) (WINDOW-CLOSE (VIEW-CONTAINER ITEM)))
               :PART-COLOR-LIST
               ;; '(:TEXT 0 :BODY 16748950)
               '(:BODY 16748950)
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(168 274)
               #@(62 16)
               "Help"
               #'(LAMBDA (ITEM)
                   ITEM
                   (MESSAGE-DIALOG
                    "HELP ON ALGEBRA SELECTION:
This dialog lets you choose a constraint algebra to be used to create a temporal constraint network. Select one item in the list, then click \"OK\"; otherwise click \"Cancel\"."
                    :OK-TEXT
                    "Quit Help"
                    :SIZE
                    9175375))
               :PART-COLOR-LIST
               '(:TEXT 0 :BODY 6225407)
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(18 237)
               #@(62 16)
               "Save As"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Merge networks action."))
               :PART-COLOR-LIST
               '(:BODY 16645915)
               :VIEW-NICK-NAME
               'MERGE
               :DIALOG-ITEM-ENABLED-P
               NIL
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(19 190)
               #@(61 16)
               "Load"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Create network action."))
               :VIEW-NICK-NAME
               'CREATE
               :DIALOG-ITEM-ENABLED-P
               NIL
               :PART-COLOR-LIST
               '(:BODY 16645915)
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(18 214)
               #@(62 16)
               "Save"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Copy network action."))
               :PART-COLOR-LIST
               '(:BODY 16645915)
               :VIEW-NICK-NAME
               'COPY
               :DIALOG-ITEM-ENABLED-P
               NIL
               :DEFAULT-BUTTON
               NIL))))