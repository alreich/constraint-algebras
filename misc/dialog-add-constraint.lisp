
(setf add-con-dialog
      (MAKE-INSTANCE 'DIALOG
        :WINDOW-TYPE
        ;; :DOUBLE-EDGE-BOX
        :DOCUMENT
        :WINDOW-TITLE
        "Add Constraint"
        :VIEW-POSITION
        #@(141 151)
        :VIEW-SIZE
        #@(288 205)
        :VIEW-FONT
        '("Chicago" 12 :SRCOR :PLAIN)
        :VIEW-SUBVIEWS
        (LIST (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(12 30)
               #@(97 16)
               "X <before> Y"
               #'(LAMBDA (ITEM) ITEM)
               :VIEW-NICK-NAME
               'BEFORE
               ;; :CHECK-BOX-CHECKED-P
               ;; T
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(12 45)
               #@(102 16)
               "X <during> Y"
               #'(LAMBDA (ITEM)
                   (if (check-box-checked-p item)
                     (set-part-color item :text *red-color*)
                     (set-part-color item :text *black-color*)))
               :VIEW-NICK-NAME
               'DURING
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(12 60)
               #@(115 16)
               "X <finishes> Y"
               #'(LAMBDA (ITEM) ITEM)
               :VIEW-NICK-NAME
               'FINISHES
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(12 90)
               #@(100 16)
               "X <overlaps> Y"
               #'(LAMBDA (ITEM) ITEM)
               :VIEW-NICK-NAME
               'OVERLAPS
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(137 105)
               #@(115 16)
               "X <started-by> Y"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Started-by action."))
               ;; #'(LAMBDA (ITEM) (FORMAT T "~%Started-by action."))
               :VIEW-NICK-NAME
               'STARTED-BY
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(137 90)
               #@(134 16)
               "X <overlapped-by> Y"
               'NIL
               :VIEW-NICK-NAME
               'OVERLAPPED-BY
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(12 75)
               #@(91 16)
               "X <meets> Y"
               #'(LAMBDA (ITEM) ITEM)
               :VIEW-NICK-NAME
               'MEETS
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(137 60)
               #@(120 16)
               "X <finished-by> Y"
               'NIL
               :VIEW-NICK-NAME
               'FINISHED-BY
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(137 45)
               #@(102 16)
               "X <contains> Y"
               'NIL
               :VIEW-NICK-NAME
               'CONTAINS
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(137 30)
               #@(97 16)
               "X <after> Y"
               'NIL
               :VIEW-NICK-NAME
               'AFTER
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(137 75)
               #@(91 16)
               "X <met-by> Y"
               'NIL
               :VIEW-NICK-NAME
               'MET-BY
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(12 120)
               #@(89 16)
               "X <equals> Y"
               #'(LAMBDA (ITEM) ITEM)
               :VIEW-NICK-NAME
               'EQUALS
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(12 105)
               #@(89 16)
               "X <starts> Y"
               #'(LAMBDA (ITEM) ITEM)
               :VIEW-NICK-NAME
               'STARTS
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(12 159)
               #@(65 16)
               "X <in> Y"
               'NIL
               :VIEW-NICK-NAME
               'IN
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'STATIC-TEXT-DIALOG-ITEM
               #@(16 8)
               #@(15 16)
               "X:"
               'NIL
               :PART-COLOR-LIST
               '(:TEXT 1974527))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(12 144)
               #@(105 16)
               "X <unknown> Y"
               'NIL
               :VIEW-NICK-NAME
               'UNKNOWN
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(137 144)
               #@(105 16)
               "X <disjoint> Y"
               'NIL
               :VIEW-NICK-NAME
               'DISJOINT
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'CHECK-BOX-DIALOG-ITEM
               #@(137 159)
               #@(94 16)
               "X <around> Y"
               'NIL
               :VIEW-NICK-NAME
               'AROUND
               :VIEW-FONT
               '("Chicago" 10 :SRCOR :PLAIN))
              (MAKE-DIALOG-ITEM
               'EDITABLE-TEXT-DIALOG-ITEM
               #@(35 8)
               #@(94 13)
               "foo-interval"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%X interval name."))
               :VIEW-NICK-NAME
               'XINT
               :PART-COLOR-LIST
               '(:FRAME 1578959 :TEXT 1778431)
               :ALLOW-RETURNS
               NIL)
              (MAKE-DIALOG-ITEM
               'STATIC-TEXT-DIALOG-ITEM
               #@(144 8)
               #@(15 16)
               "Y:"
               'NIL
               :PART-COLOR-LIST
               '(:TEXT 1974527))
              (MAKE-DIALOG-ITEM
               'EDITABLE-TEXT-DIALOG-ITEM
               #@(163 8)
               #@(106 13)
               "bar-interval"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Y interval name."))
               :VIEW-NICK-NAME
               'YINT
               :PART-COLOR-LIST
               '(:FRAME 1578959 :TEXT 1778431)
               :ALLOW-RETURNS
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(49 181)
               #@(62 16)
               "Cancel"
               #'(LAMBDA (ITEM)
                   (window-close (view-container item)))
               :VIEW-NICK-NAME
               'CANCEL
               :PART-COLOR-LIST
               '(:TEXT 16719388)
               :DEFAULT-BUTTON
               NIL)
              (MAKE-DIALOG-ITEM
               'BUTTON-DIALOG-ITEM
               #@(162 181)
               #@(62 16)
               "Add"
               #'(LAMBDA (ITEM) ITEM (FORMAT T "~%OK Action."))
               :VIEW-NICK-NAME
               'ADD
               :PART-COLOR-LIST
               '(:TEXT 1621781)
               :DEFAULT-BUTTON
               NIL))))
