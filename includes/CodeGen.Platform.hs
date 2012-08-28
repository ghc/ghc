
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

activeStgRegs :: [GlobalReg]
activeStgRegs = [
#ifdef REG_Base
    BaseReg
#endif
#ifdef REG_Sp
    ,Sp
#endif
#ifdef REG_Hp
    ,Hp
#endif
#ifdef REG_R1
    ,VanillaReg 1 VGcPtr
#endif
#ifdef REG_R2
    ,VanillaReg 2 VGcPtr
#endif
#ifdef REG_R3
    ,VanillaReg 3 VGcPtr
#endif
#ifdef REG_R4
    ,VanillaReg 4 VGcPtr
#endif
#ifdef REG_R5
    ,VanillaReg 5 VGcPtr
#endif
#ifdef REG_R6
    ,VanillaReg 6 VGcPtr
#endif
#ifdef REG_R7
    ,VanillaReg 7 VGcPtr
#endif
#ifdef REG_R8
    ,VanillaReg 8 VGcPtr
#endif
#ifdef REG_R9
    ,VanillaReg 9 VGcPtr
#endif
#ifdef REG_R10
    ,VanillaReg 10 VGcPtr
#endif
#ifdef REG_SpLim
    ,SpLim
#endif
#ifdef REG_F1
    ,FloatReg 1
#endif
#ifdef REG_F2
    ,FloatReg 2
#endif
#ifdef REG_F3
    ,FloatReg 3
#endif
#ifdef REG_F4
    ,FloatReg 4
#endif
#ifdef REG_D1
    ,DoubleReg 1
#endif
#ifdef REG_D2
    ,DoubleReg 2
#endif
    ]

haveRegBase :: Bool
#ifdef REG_Base
haveRegBase = True
#else
haveRegBase = False
#endif

