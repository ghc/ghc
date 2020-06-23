{-# LANGUAGE CPP, GADTs #-}
module GHC.CmmToAsm.ARM.CodeGen
  ( cmmTopCodeGen
  , generateJumpTableForInstr
  , InstrBlock
  ) where

#include "HsVersions.h"

import GHC.CmmToAsm.Monad (NatM)
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.ARM.Instr
import GHC.Prelude
import GHC.Cmm

cmmTopCodeGen :: RawCmmDecl -> NatM [NatCmmDecl RawCmmStatics Instr]
cmmTopCodeGen = undefined

generateJumpTableForInstr
  :: NCGConfig
  -> Instr
  -> Maybe (NatCmmDecl RawCmmStatics Instr)
generateJumpTableForInstr = undefined

data InstrBlock
