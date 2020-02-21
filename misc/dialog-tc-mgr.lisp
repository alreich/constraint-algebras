(in-package "CM")

#|
(set-part-color tcm :frame 1842687)
(set-part-color tcm :title-bar 16775206)
|#

(defclass TCM-DIALOG (dialog)
  (
   (tcm-object
    :initarg :tcm-object
    :accessor tcm-object
    :initform nil
    :documentation "The temporal constraint network object that contains this dialog.")
   )
  )

(defun CREATE-TC-MGR-DIALOG ()
  (MAKE-INSTANCE 'TCM-DIALOG
    :WINDOW-TYPE
    :TOOL
    :WINDOW-TITLE
    "Constraint Network Manager"
    :VIEW-POSITION
    #@(337 130)
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
           "Select networks from this list"
           'NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(115 194)
           #@(62 16)
           "Copy"
           #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Copy network action."))
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(115 167)
           #@(61 16)
           "Create"
           #'(LAMBDA (ITEM)
               ITEM
               (FORMAT T "~%Create network action.")
               (CREATE-TCN *TEMPORAL-CONSTRAINT-MANAGER*))
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(115 223)
           #@(62 16)
           "Merge"
           #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Merge networks action."))
           :DIALOG-ITEM-ENABLED-P
           NIL
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(35 270)
           #@(62 16)
           "Edit"
           #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Edit constraint action."))
           :VIEW-NICK-NAME
           'EDIT
           :DEFAULT-BUTTON
           T)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(212 167)
           #@(62 16)
           "Show"
           #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Show constraints action."))
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(212 194)
           #@(62 16)
           "Delete"
           #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Delete constraint action."))
           :DIALOG-ITEM-ENABLED-P
           NIL
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(118 270)
           #@(62 16)
           "Quit"
           #'(LAMBDA (ITEM) (WINDOW-CLOSE (VIEW-CONTAINER ITEM)))
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(201 270)
           #@(62 16)
           "Help"
           #'(LAMBDA (ITEM)
               ITEM
               (MESSAGE-DIALOG
                "HELP ON MANAGING CONSTRAINT NETWORKS:
This dialog lets you create and load temporal constraint networks. To assert constraints in a network click \"Edit\".  To see a network's contents click \"Show\"."
                :OK-TEXT
                "Quit Help"
                :SIZE
                9175375))
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(18 223)
           #@(62 16)
           "Save As"
           #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Merge networks action."))
           :VIEW-NICK-NAME
           'MERGE
           :DIALOG-ITEM-ENABLED-P
           NIL
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(19 167)
           #@(61 16)
           "Load"
           #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Create network action."))
           :VIEW-NICK-NAME
           'CREATE
           :DIALOG-ITEM-ENABLED-P
           NIL
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(18 194)
           #@(62 16)
           "Save"
           #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Copy network action."))
           :VIEW-NICK-NAME
           'COPY
           :DIALOG-ITEM-ENABLED-P
           NIL
           :DEFAULT-BUTTON
           NIL)
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(212 223)
           #@(62 16)
           "Rename"
           #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Rename network action."))
           :VIEW-NICK-NAME
           'RENAME
           :DEFAULT-BUTTON
           NIL))))
