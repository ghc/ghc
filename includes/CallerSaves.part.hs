
#include <stg/MachRegs.h>

    platformCallerSaves :: GlobalReg -> Bool
#ifdef CALLER_SAVES_Base
    platformCallerSaves BaseReg     = True
#endif
#ifdef CALLER_SAVES_R1
    platformCallerSaves (VanillaReg 1 _)    = True
#endif
#ifdef CALLER_SAVES_R2
    platformCallerSaves (VanillaReg 2 _)    = True
#endif
#ifdef CALLER_SAVES_R3
    platformCallerSaves (VanillaReg 3 _)    = True
#endif
#ifdef CALLER_SAVES_R4
    platformCallerSaves (VanillaReg 4 _)    = True
#endif
#ifdef CALLER_SAVES_R5
    platformCallerSaves (VanillaReg 5 _)    = True
#endif
#ifdef CALLER_SAVES_R6
    platformCallerSaves (VanillaReg 6 _)    = True
#endif
#ifdef CALLER_SAVES_R7
    platformCallerSaves (VanillaReg 7 _)    = True
#endif
#ifdef CALLER_SAVES_R8
    platformCallerSaves (VanillaReg 8 _)    = True
#endif
#ifdef CALLER_SAVES_R9
    platformCallerSaves (VanillaReg 9 _)    = True
#endif
#ifdef CALLER_SAVES_R10
    platformCallerSaves (VanillaReg 10 _)   = True
#endif
#ifdef CALLER_SAVES_F1
    platformCallerSaves (FloatReg 1)    = True
#endif
#ifdef CALLER_SAVES_F2
    platformCallerSaves (FloatReg 2)    = True
#endif
#ifdef CALLER_SAVES_F3
    platformCallerSaves (FloatReg 3)    = True
#endif
#ifdef CALLER_SAVES_F4
    platformCallerSaves (FloatReg 4)    = True
#endif
#ifdef CALLER_SAVES_D1
    platformCallerSaves (DoubleReg 1)   = True
#endif
#ifdef CALLER_SAVES_D2
    platformCallerSaves (DoubleReg 2)   = True
#endif
#ifdef CALLER_SAVES_L1
    platformCallerSaves (LongReg 1)     = True
#endif
#ifdef CALLER_SAVES_Sp
    platformCallerSaves Sp          = True
#endif
#ifdef CALLER_SAVES_SpLim
    platformCallerSaves SpLim       = True
#endif
#ifdef CALLER_SAVES_Hp
    platformCallerSaves Hp          = True
#endif
#ifdef CALLER_SAVES_HpLim
    platformCallerSaves HpLim       = True
#endif
#ifdef CALLER_SAVES_CCCS
    platformCallerSaves CCCS                = True
#endif
#ifdef CALLER_SAVES_CurrentTSO
    platformCallerSaves CurrentTSO      = True
#endif
#ifdef CALLER_SAVES_CurrentNursery
    platformCallerSaves CurrentNursery  = True
#endif
    platformCallerSaves _           = False

