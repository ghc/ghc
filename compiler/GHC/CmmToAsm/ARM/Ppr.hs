module GHC.CmmToAsm.ARM.Ppr (pprNatCmmDecl) where

import GHC.Prelude
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.ARM.Instr
import GHC.Cmm
import GHC.Utils.Outputable

pprNatCmmDecl
  :: NCGConfig
  -> NatCmmDecl RawCmmStatics Instr
  -> SDoc
pprNatCmmDecl = undefined

