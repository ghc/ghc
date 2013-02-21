
module SPARC.CodeGen.Gen32 (
        getSomeReg,
        getRegister
)

where

import SPARC.CodeGen.Base
import NCGMonad
import Reg

import Cmm

getSomeReg  :: CmmExpr -> NatM (Reg, InstrBlock)
getRegister :: CmmExpr -> NatM Register
