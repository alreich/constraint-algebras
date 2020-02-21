
(MAKE-INSTANCE 'COLOR-DIALOG
  :WINDOW-TYPE
  :TOOL
  :VIEW-POSITION
  '(:RIGHT 149)
  :VIEW-SIZE
  #@(300 150)
  :VIEW-FONT
  '("Chicago" 12 :SRCOR :PLAIN)
  :VIEW-SUBVIEWS
  (LIST (MAKE-DIALOG-ITEM
          'RADIO-BUTTON-DIALOG-ITEM
          #@(202 48)
          #@(72 16)
          "Untitled"
          'NIL)
        (MAKE-DIALOG-ITEM
          'CHECK-BOX-DIALOG-ITEM
          #@(102 48)
          #@(72 16)
          "Untitled"
          'NIL)
        (MAKE-DIALOG-ITEM
          'EDITABLE-TEXT-DIALOG-ITEM
          #@(114 9)
          #@(176 16)
          "Editable Text Goes Here"
          #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Edit text action."))
          :VIEW-NICK-NAME
          'ED_TXT
          :VIEW-FONT
          '("Helvetica" 14 :SRCOR :PLAIN)
          :PART-COLOR-LIST
          '(:BODY 6429951 :TEXT 0 :FRAME 2031392)
          :ALLOW-RETURNS
          T)
        (MAKE-DIALOG-ITEM
          'STATIC-TEXT-DIALOG-ITEM
          #@(9 17)
          #@(83 13)
          "Static Text"
          #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Static text action."))
          :VIEW-NICK-NAME
          'STAT_TXT
          :VIEW-FONT
          '("Helvetica" 12 :SRCOR :ITALIC :BOLD)
          :PART-COLOR-LIST
          '(:THUMB 4128540 :BODY 16718362 :TEXT 16775206 :FRAME 7998220))
        (MAKE-DIALOG-ITEM
          'BUTTON-DIALOG-ITEM
          #@(108 124)
          #@(62 16)
          "Button A"
          #'(LAMBDA (ITEM) ITEM (FORMAT T "~%Button A action."))
          :VIEW-NICK-NAME
          'A
          :VIEW-FONT
          '("Monaco" 12 :SRCOR :PLAIN)
          :DEFAULT-BUTTON
          T)
        (MAKE-DIALOG-ITEM
          'SEQUENCE-DIALOG-ITEM
          #@(10 38)
          #@(80 103)
          "Scroll-Window"
          #'(LAMBDA (ITEM) (FORMAT T "~%Scrolling window action, ~A." ITEM))
          :VIEW-NICK-NAME
          'SCROLL_WINDOW
          :VIEW-FONT
          '("Zapf Chancery" 10 :SRCOR :BOLD)
          :PART-COLOR-LIST
          '(:THUMB 16718610 :BODY 16580382 :TEXT 16718434 :FRAME 10820607)
          :CELL-SIZE
          #@(64 16)
          :SELECTION-TYPE
          :SINGLE
          :TABLE-HSCROLLP
          NIL
          :TABLE-VSCROLLP
          T
          :TABLE-SEQUENCE
          '(:ONE :TWO :THREE :FOUR :FIVE :SIX :SEVER))
        (MAKE-DIALOG-ITEM
          'RADIO-BUTTON-DIALOG-ITEM
          #@(202 71)
          #@(72 16)
          "Rad But"
          'NIL
          :RADIO-BUTTON-PUSHED-P
          T)
        (MAKE-DIALOG-ITEM
          'RADIO-BUTTON-DIALOG-ITEM
          #@(203 93)
          #@(72 16)
          "Untitled"
          'NIL)
        (MAKE-DIALOG-ITEM
          'CHECK-BOX-DIALOG-ITEM
          #@(102 71)
          #@(72 16)
          "Untitled"
          'NIL)
        (MAKE-DIALOG-ITEM
          'BUTTON-DIALOG-ITEM
          #@(179 124)
          #@(62 16)
          "Untitled"
          'NIL
          :DEFAULT-BUTTON
          NIL)))