---------------------------------------------------------------------------

(write-algebra-table ialg
		     :relations (sort (get-algebra-rels ialg) #'string<)
		     :reuse-abbrevs *reused-abbrevs-i*
		     :init-counter 8)

 ABBREVIATIONS:
       RI = {B, BI, D, DI, E, F, FI, M, MI, O, OI, S, SI}
       Z1   {B, D, M, O, S}
       Z2   {BI, D, F, MI, OI}
       Z3   {D, DI, E, F, FI, O, OI, S, SI}
       Z4   {DI, OI, SI}
       Z5   {DI, FI, O}
       Z6   {E, F, FI}
       Z7   {BI, MI, OI}
       Z8   {E, S, SI}

 MULTIPLICATION TABLE:
     X        B    BI     D    DI     E     F    FI     M    MI     O    OI     S    SI
     B        B    RI    Z1     B     B    Z1     B     B    Z1     B    Z1     B     B
    BI       RI    BI    Z2    BI    BI    BI    BI    Z2    BI    Z2    BI    Z2    BI
     D        B    BI     D    RI     D     D    Z1     B    BI    Z1    Z2     D    Z2
    DI      Z2V   Z1V    Z3    DI    DI    Z4    DI    Z5    Z4    Z5    Z4    Z5    DI
     E        B    BI     D    DI     E     F    FI     M    MI     O    OI     S    SI
     F        B    BI     D   Z1V     F     F    Z6     M    BI   Z4V    Z7     D    Z7
    FI        B   Z1V   Z4V    DI    FI    Z6    FI     M    Z4     O    Z4     O    DI
     M        B   Z1V   Z4V     B     M   Z4V     B     B    Z6     B   Z4V     M     M
    MI      Z2V    BI   Z5V    BI    MI    MI    MI    Z8    BI   Z5V    BI   Z5V    BI
     O        B   Z1V   Z4V   Z2V     O   Z4V   Z7V     B    Z4   Z7V    Z3     O    Z5
    OI      Z2V    BI   Z5V   Z1V    OI    OI    Z4    Z5    BI    Z3    Z7   Z5V    Z7
     S        B    BI     D   Z2V     S     D   Z7V     B    MI   Z7V   Z5V     S    Z8
    SI      Z2V    BI   Z5V    DI    SI    OI    DI    Z5    MI    Z5    OI    Z8    SI

---------------------------------------------------------------------------

(write-algebra-table ipalg
		     :relations '(:=B :=BI :=D :=DI :=E :=F :=FI :=M :=MI :=O :=OI :=S :=SI :=PE :=PF :=PFI :=PS :=PSI)
		     :reuse-abbrevs *reused-abbrevs-ip*
		     :init-counter 8)

 ABBREVIATIONS:
      RIP   {B, BI, D, DI, E, F, FI, M, MI, O, OI, S, SI, PE, PF, PFI, PS, PSI}
       Z9   {B, D, M, O, PS, S}
      Z10   {BI, D, F, MI, OI, PF}

 MULTIPLICATION TABLE:
     X        B    BI     D    DI     E     F    FI     M    MI     O    OI     S    SI    PE    PF   PFI    PS   PSI
     B        B   RIP    Z9     B     B    Z9     B     B    Z9     B    Z9     B     B     B    Z9     B     B     B
    BI      RIP    BI   Z10    BI    BI    BI    BI   Z10    BI   Z10    BI   Z10    BI    BI    BI    BI   Z10    BI
     D        B    BI     D   RIP     D     D    Z9     B    BI    Z9   Z10     D   Z10   ---   ---     B   ---    BI
    DI     Z10V   Z9V    Z3    DI    DI    Z4    DI    Z5    Z4    Z5    Z4    Z5    DI    DI    Z4    DI    Z5    DI
     E        B    BI     D    DI     E     F    FI     M    MI     O    OI     S    SI   ---   ---   PFI   ---   PSI
     F        B    BI     D   Z9V     F     F    Z6     M    BI   Z4V    Z7     D    Z7   ---   ---   PFI   ---    BI
    FI        B   Z9V   Z4V    DI    FI    Z6    FI     M    Z4     O    Z4     O    DI   ---   ---   PFI   ---    DI
     M        B   Z9V   Z4V     B     M   Z4V     B     B    Z6     B   Z4V     M     M   ---   ---     B   ---   PFI
    MI     Z10V    BI   Z5V    BI    MI    MI    MI    Z8    BI   Z5V    BI   Z5V    BI   ---   ---   PSI   ---    BI
     O        B   Z9V   Z4V  Z10V     O   Z4V   Z7V     B    Z4   Z7V    Z3     O    Z5   ---   ---     B   ---    DI
    OI     Z10V    BI   Z5V   Z9V    OI    OI    Z4    Z5    BI    Z3    Z7   Z5V    Z7   ---   ---    DI   ---    BI
     S        B    BI     D  Z10V     S     D   Z7V     B    MI   Z7V   Z5V     S    Z8   ---   ---     B   ---   PSI
    SI     Z10V    BI   Z5V    DI    SI    OI    DI    Z5    MI    Z5    OI    Z8    SI   ---   ---    DI   ---   PSI
    PE        B    BI     D   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---    PE    PF   ---    PS   ---
    PF        B    BI     D    BI    PF    PF    PF    PS    BI     D    BI     D    BI   ---   ---    PE   ---    BI
   PFI        B   Z9V   Z4V   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   PFI    Z6   ---     M   ---
    PS        B    BI     D     B    PS     D     B     B    PF     B     D    PS    PS   ---   ---     B   ---    PE
   PSI     Z10V    BI   Z5V   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   PSI    MI   ---    Z8   ---

---------------------------------------------------------------------------

(write-algebra-table rbalg
		     :relations '(:=<B :=<BI :=<D :=<DI :=<E :=<F :=<FI :=<M :=<MI :=<O :=<OI :=<S :=<SI
				  :=<PE :=<PF :=<PFI :=<PS :=<PSI
				  :=<RB :=<RBI :=<RO :=<ROI :=<RS :=<R~)
		     :reuse-abbrevs *reused-abbrevs-rb*
		     :init-counter 10)

 ABBREVIATIONS:
      RRB   {B, BI, D, DI, E, F, FI, M, MI, O, OI, S, SI, PE, PF, PFI, PS, PSI, RB, RBI, RO, ROI, RS, R~}
      Z11   {B, D, M, O, PS, RBI, RO, ROI, RS, S}
      Z12   {B, RB, R~}
      Z13   {BI, D, F, MI, OI, PF, RBI, ROI}
      Z14   {D, RBI, ROI}
      Z15   {D, DI, E, F, FI, O, OI, RO, ROI, RS, S, SI}
      Z16   {DI, FI, O, RO}
      Z17   {RBI, RO, ROI, RS}
      Z18   {RO, ROI, RS}
      Z19   {RB, R~}
      Z20   {RBI, ROI}
      Z21   {E, RS, S, SI}
      Z22   {B, RB}
      Z23   {B, M, O, RO}
      Z24   {D, O, RO, ROI, RS, S}
      Z25   {O, RO}
      Z26   {D, ROI}
      Z27   {RS, S}
      Z28   {D, DI, E, F, FI, O, OI, RB, RBI, RO, ROI, RS, S, SI}
      Z29   {B, DI, FI, M, O, PFI, RB, RO, R~}
      Z30   {DI, FI, O, RB, RO}

 MULTIPLICATION TABLE:
     X        B    BI     D    DI     E     F    FI     M    MI     O    OI     S    SI    PE    PF   PFI    PS   PSI    RB   RBI    RO   ROI    RS    R~
     B        B   RIP    Z9     B     B    Z9     B     B    Z9     B    Z9     B     B     B    Z9     B     B     B     B   Z11     B    Z9     B   Z12
    BI      RRB    BI   Z13    BI    BI    BI    BI   Z13    BI   Z13    BI   Z13    BI    BI    BI    BI   Z13    BI    R~   RBI   RBI   RBI   RBI    R~
     D        B    BI     D   RIP     D     D    Z9     B    BI    Z9   Z10     D   Z10   ---   ---     B   ---    BI   Z12   RBI   Z11   Z14   Z14    R~
    DI     Z13V   Z9V   Z15    DI    DI    Z4    DI   Z16    Z4   Z16    Z4   Z16    DI    DI    Z4    DI   Z16    DI    RB   Z17    RO   Z18    RO   Z19
     E        B    BI     D    DI     E     F    FI     M    MI     O    OI     S    SI   ---   ---   PFI   ---   PSI    RB   RBI    RO   ROI    RS    R~
     F        B    BI     D   Z9V     F     F    Z6     M    BI   Z4V    Z7     D    Z7   ---   ---   PFI   ---    BI   Z19   RBI   Z17   Z20   Z20    R~
    FI        B   Z9V   Z4V    DI    FI    Z6    FI     M    Z4     O    Z4     O    DI   ---   ---   PFI   ---    DI    RB   Z17    RO   Z18    RO   Z19
     M        B   Z9V   Z4V     B     M   Z4V     B     B    Z6     B   Z4V     M     M   ---   ---     B   ---   PFI     B   Z17     B   Z4V     M   Z19
    MI     Z13V    BI  Z16V    BI    MI    MI    MI   Z21    BI  Z16V    BI  Z16V    BI   ---   ---   PSI   ---    BI    R~   RBI   RBI   RBI   RBI    R~
     O        B   Z9V   Z4V  Z10V     O   Z4V   Z7V     B    Z4   Z7V    Z3     O    Z5   ---   ---     B   ---    DI   Z22   Z17   Z23   Z24   Z25   Z19
    OI     Z13V    BI  Z16V   Z9V    OI    OI    Z4   Z16    BI   Z15    Z7  Z16V    Z7   ---   ---    DI   ---    BI   Z19   RBI   Z17   Z20   Z20    R~
     S        B    BI     D  Z10V     S     D   Z7V     B    MI   Z7V   Z5V     S    Z8   ---   ---     B   ---   PSI   Z22   RBI   Z23   Z26   Z27    R~
    SI     Z13V    BI  Z16V    DI    SI    OI    DI   Z16    MI   Z16    OI   Z21    SI   ---   ---    DI   ---   PSI    RB   RBI    RO   ROI    RS    R~
    PE        B    BI     D   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---    PE    PF   ---    PS   ---   ---   RBI   ---   ---   ---    R~
    PF        B    BI     D    BI    PF    PF    PF    PS    BI     D    BI     D    BI   ---   ---    PE   ---    BI    R~   RBI   RBI   RBI   RBI    R~
   PFI        B   Z9V   Z4V   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   PFI    Z6   ---     M   ---   ---   Z17   ---   ---   ---   Z19
    PS        B    BI     D     B    PS     D     B     B    PF     B     D    PS    PS   ---   ---     B   ---    PE     B   RBI     B     D    PS    R~
   PSI     Z13V    BI  Z16V   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   PSI    MI   ---   Z21   ---   ---   RBI   ---   ---   ---    R~
    RB       RB  Z11V  Z17V    RB    RB  Z17V    RB    RB  Z17V    RB  Z17V    RB    RB    RB  Z17V    RB    RB    RB    RB   Z28    RB  Z17V    RB   Z29
   RBI       R~    BI   RBI  Z12V   RBI   RBI  Z19V    R~    BI  Z19V  Z22V   RBI  Z22V   ---   ---    R~   ---    BI   RRB   RBI  Z29V   Z13   Z13    R~
    RO       RB   Z9V   Z18  Z14V    RO   Z18  Z20V    RB    Z4  Z20V  Z24V    RO  Z26V   ---   ---    RB   ---    DI  Z13V   Z17   Z30   Z15   Z16   Z19
   ROI       RB    BI   ROI  Z11V   ROI   ROI  Z17V    RB    BI  Z17V  Z23V   ROI  Z23V   ---   ---    RB   ---    BI   Z29   RBI   Z28  Z30V  Z30V    R~
    RS       RB    BI   ROI  Z14V    RS   ROI  Z20V    RB    MI  Z20V  Z25V    RS  Z27V   ---   ---    RB   ---   PSI  Z13V   RBI   Z30  Z16V   Z21    R~
    R~       R~  Z12V  Z19V    R~    R~  Z19V    R~    R~  Z19V    R~  Z19V    R~    R~    R~  Z19V    R~    R~    R~    R~  Z29V    R~  Z19V    R~   RRB

---------------------------------------------------------------------------

(write-algebra-table (find-algebra 'LEFT-BRANCHING-PERIOD-&-POINT)
                     :relations '(:>=B :>=BI :>=D :>=DI :>=E :>=F :>=FI :>=M :>=MI :>=O :>=OI :>=S :>=SI
                                  :>=PE :>=PF :>=PFI :>=PS :>=PSI :>=LB :>=LBI :>=LO :>=LOI :>=LF :>=L~)
                     :reuse-abbrevs *REUSED-ABBREVS-LB*
                     :init-counter 30)

 RELATIONS:
        R = B BI D DI E F FI M MI O OI S SI PE PF PFI PS PSI LB LBI LO LOI LF L~

 ABBREVIATIONS:
      RLB   {B, BI, D, DI, E, F, FI, LB, LBI, LF, LO, LOI, L~, M, MI, O, OI, PE, PF, PFI, PS, PSI, S, SI}
      Z31   {B, D, LB, LO, M, O, PS, S}
      Z32   {BI, D, F, LB, LF, LO, LOI, MI, OI, PF}
      Z33   {BI, LBI, L~}
      Z34   {D, LB, LO}
      Z35   {D, DI, E, F, FI, LF, LO, LOI, O, OI, S, SI}
      Z36   {DI, LOI, OI, SI}
      Z37   {LB, LF, LO, LOI}
      Z38   {LF, LO, LOI}
      Z39   {LBI, L~}
      Z40   {BI, LBI}
      Z41   {D, LO}
      Z42   {BI, LOI, MI, OI}
      Z43   {F, LF}
      Z44   {E, F, FI, LF}
      Z45   {LB, LO}
      Z46   {D, F, LF, LO, LOI, OI}
      Z47   {LOI, OI}
      Z48   {B, D, LB, LO, L~, M, O, PS, S}
      Z49   {D, DI, E, F, FI, LB, LBI, LF, LO, LOI, O, OI, S, SI}
      Z50   {D, LB, LO, O, S}

 MULTIPLICATION TABLE:
     X        B    BI     D    DI     E     F    FI     M    MI     O    OI     S    SI    PE    PF   PFI    PS   PSI    LB   LBI    LO   LOI    LF    L~
     B        B   RLB   Z31     B     B   Z31     B     B   Z31     B   Z31     B     B     B   Z31     B     B     B    LB    L~    LB    LB    LB    L~
    BI      RIP    BI   Z10    BI    BI    BI    BI   Z10    BI   Z10    BI   Z10    BI    BI    BI    BI   Z10    BI   Z32    BI   Z10    BI    BI   Z33
     D        B    BI     D   RIP     D     D    Z9     B    BI    Z9   Z10     D   Z10   ---   ---     B   ---    BI    LB   Z33   Z34   Z32   Z34    L~
    DI     Z10V  Z31V   Z35    DI    DI   Z36    DI    Z5   Z36    Z5   Z36    Z5    DI    DI   Z36    DI    Z5    DI   Z37   LBI   Z38   LOI   LOI   Z39
     E        B    BI     D    DI     E     F    FI     M    MI     O    OI     S    SI   ---   ---   PFI   ---   PSI    LB   LBI    LO   LOI    LF    L~
     F        B    BI     D   Z9V     F     F    Z6     M    BI   Z4V    Z7     D    Z7   ---   ---   PFI   ---    BI    LB   Z40   Z41   Z42   Z43    L~
    FI        B  Z31V  Z36V    DI    FI   Z44    FI     M   Z36     O   Z36     O    DI   ---   ---   PFI   ---    DI    LB   LBI    LO   LOI    LF    L~
     M        B  Z31V  Z36V     B     M  Z36V     B     B   Z44     B  Z36V     M     M   ---   ---     B   ---   PFI    LB    L~    LB    LB    LB    L~
    MI     Z10V    BI   Z5V    BI    MI    MI    MI    Z8    BI   Z5V    BI   Z5V    BI   ---   ---   PSI   ---    BI   Z37    BI   Z5V    BI    MI   Z39
     O        B  Z31V  Z36V  Z10V     O  Z36V   Z7V     B   Z36   Z7V   Z35     O    Z5   ---   ---     B   ---    DI    LB   Z39   Z45   Z37   Z45    L~
    OI     Z10V    BI   Z5V   Z9V    OI    OI    Z4    Z5    BI    Z3    Z7   Z5V    Z7   ---   ---    DI   ---    BI   Z37   Z40   Z46   Z42   Z47   Z39
     S        B    BI     D  Z10V     S     D   Z7V     B    MI   Z7V   Z5V     S    Z8   ---   ---     B   ---   PSI    LB   Z39   Z45   Z37   Z45    L~
    SI     Z10V    BI   Z5V    DI    SI    OI    DI    Z5    MI    Z5    OI    Z8    SI   ---   ---    DI   ---   PSI   Z37   LBI   Z38   LOI   LOI   Z39
    PE        B    BI     D   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---    PE    PF   ---    PS   ---    LB   ---   ---   ---   ---    L~
    PF        B    BI     D    BI    PF    PF    PF    PS    BI     D    BI     D    BI   ---   ---    PE   ---    BI    LB    BI     D    BI    PF    L~
   PFI        B  Z31V  Z36V   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   PFI   Z44   ---     M   ---    LB   ---   ---   ---   ---    L~
    PS        B    BI     D     B    PS     D     B     B    PF     B     D    PS    PS   ---   ---     B   ---    PE    LB    L~    LB    LB    LB    L~
   PSI     Z10V    BI   Z5V   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   PSI    MI   ---    Z8   ---   Z37   ---   ---   ---   ---   Z39
    LB        B    L~    LB  Z33V    LB    LB  Z40V     B    L~  Z40V  Z39V    LB  Z39V   ---   ---     B   ---    L~    LB   RLB   Z31   Z48   Z31    L~
   LBI     Z32V   LBI  Z37V   LBI   LBI   LBI   LBI  Z37V   LBI  Z37V   LBI  Z37V   LBI   LBI   LBI   LBI  Z37V   LBI   Z49   LBI  Z37V   LBI   LBI  Z48V
    LO        B   LBI    LO  Z32V    LO    LO  Z42V     B   LBI  Z42V  Z37V    LO  Z37V   ---   ---     B   ---   LBI    LB  Z48V   Z50   Z49   Z50    L~
   LOI     Z10V   LBI   Z38  Z34V   LOI   LOI  Z41V    Z5   LBI  Z46V  Z45V   Z38  Z45V   ---   ---    DI   ---   LBI   Z37  Z31V   Z35  Z50V   Z36   Z39
    LF        B   LBI    LO  Z34V    LF    LF  Z43V     M   LBI  Z47V  Z45V    LO  Z45V   ---   ---   PFI   ---   LBI    LB  Z31V  Z36V  Z50V   Z44    L~
    L~     Z33V    L~  Z39V    L~    L~    L~    L~  Z39V    L~  Z39V    L~  Z39V    L~    L~    L~    L~  Z39V    L~   Z48    L~  Z39V    L~    L~   RLB

