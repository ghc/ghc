
#include <stg/MachRegs.h>

callerSaves :: GlobalReg -> Bool
#ifdef CALLER_SAVES_Base
callerSaves BaseReg           = True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg 1 _)  = True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg 2 _)  = True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg 3 _)  = True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg 4 _)  = True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg 5 _)  = True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg 6 _)  = True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg 7 _)  = True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg 8 _)  = True
#endif
#ifdef CALLER_SAVES_R9
callerSaves (VanillaReg 9 _)  = True
#endif
#ifdef CALLER_SAVES_R10
callerSaves (VanillaReg 10 _) = True
#endif
#ifdef CALLER_SAVES_F1
callerSaves (FloatReg 1)      = True
#endif
#ifdef CALLER_SAVES_F2
callerSaves (FloatReg 2)      = True
#endif
#ifdef CALLER_SAVES_F3
callerSaves (FloatReg 3)      = True
#endif
#ifdef CALLER_SAVES_F4
callerSaves (FloatReg 4)      = True
#endif
#ifdef CALLER_SAVES_D1
callerSaves (DoubleReg 1)     = True
#endif
#ifdef CALLER_SAVES_D2
callerSaves (DoubleReg 2)     = True
#endif
#ifdef CALLER_SAVES_L1
callerSaves (LongReg 1)       = True
#endif
#ifdef CALLER_SAVES_Sp
callerSaves Sp                = True
#endif
#ifdef CALLER_SAVES_SpLim
callerSaves SpLim             = True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp                = True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim             = True
#endif
#ifdef CALLER_SAVES_CCCS
callerSaves CCCS              = True
#endif
#ifdef CALLER_SAVES_CurrentTSO
callerSaves CurrentTSO        = True
#endif
#ifdef CALLER_SAVES_CurrentNursery
callerSaves CurrentNursery    = True
#endif
callerSaves _                 = False

