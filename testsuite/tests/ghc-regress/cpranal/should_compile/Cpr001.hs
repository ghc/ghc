module Cpr001
    (intpInstr) where

import Cpr001_imp

-- -------------------------------------------------------------------

intpInstr	:: Instr -> MST ()

intpInstr (SysCall "exit")
    = setMTerminated

intpInstr (SysCall call)
    = setMSvc call

-- -------------------------------------------------------------------
