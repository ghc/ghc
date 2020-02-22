
module GHC.CmmToAsm.SPARC.CodeGen.Gen32 (
        getSomeReg,
        getRegister
)

where

import GHC.CmmToAsm.SPARC.CodeGen.Base
import GHC.CmmToAsm.Monad
import GHC.Platform.Reg

import GHC.Cmm

getSomeReg  :: CmmExpr -> NatM (Reg, InstrBlock)
getRegister :: CmmExpr -> NatM Register
