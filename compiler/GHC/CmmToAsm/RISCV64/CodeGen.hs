module GHC.CmmToAsm.RISCV64.CodeGen where

import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.RISCV64.Instr
import Prelude

cmmTopCodeGen
        :: RawCmmDecl
        -> NatM [NatCmmDecl RawCmmStatics Instr]
cmmTopCodeGen _ = error "TODO: cmmTopCodeGen"

generateJumpTableForInstr :: Instr
                          -> Maybe (NatCmmDecl RawCmmStatics Instr)
generateJumpTableForInstr _ = error "TODO: generateJumpTableForInstr"
