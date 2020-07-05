module GHC.CmmToAsm.ARM.Regs where

import GHC.Prelude
import GHC.Platform
import GHC.Platform.Reg

allocatableRegs :: Platform -> [RealReg]
allocatableRegs = const []
