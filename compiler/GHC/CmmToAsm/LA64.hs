{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Native code generator for LoongArch64 architectures
module GHC.CmmToAsm.LA64 ( ncgLA64 ) where

import GHC.Prelude

import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Types
import GHC.Utils.Outputable (ftext)

import qualified GHC.CmmToAsm.LA64.CodeGen  as LA64
import qualified GHC.CmmToAsm.LA64.Instr    as LA64
import qualified GHC.CmmToAsm.LA64.Ppr      as LA64
import qualified GHC.CmmToAsm.LA64.RegInfo  as LA64
import qualified GHC.CmmToAsm.LA64.Regs     as LA64

ncgLA64 :: NCGConfig -> NcgImpl RawCmmStatics LA64.Instr LA64.JumpDest
ncgLA64 config =
  NcgImpl
    { ncgConfig                 = config,
      cmmTopCodeGen             = LA64.cmmTopCodeGen,
      generateJumpTableForInstr = LA64.generateJumpTableForInstr config,
      getJumpDestBlockId        = LA64.getJumpDestBlockId,
      canShortcut               = LA64.canShortcut,
      shortcutStatics           = LA64.shortcutStatics,
      shortcutJump              = LA64.shortcutJump,
      pprNatCmmDeclS            = LA64.pprNatCmmDecl config,
      pprNatCmmDeclH            = LA64.pprNatCmmDecl config,
      maxSpillSlots             = LA64.maxSpillSlots config,
      allocatableRegs           = LA64.allocatableRegs platform,
      ncgAllocMoreStack         = LA64.allocMoreStack platform,
      ncgMakeFarBranches        = \_p _i bs -> pure bs,
      extractUnwindPoints       = const [],
      invertCondBranches        = \_ _ -> id
    }
  where
    platform = ncgPlatform config

-- | `Instruction` instance for LA64
instance Instruction LA64.Instr where
  regUsageOfInstr       = LA64.regUsageOfInstr
  patchRegsOfInstr _    = LA64.patchRegsOfInstr
  isJumpishInstr        = LA64.isJumpishInstr
  canFallthroughTo      = LA64.canFallthroughTo
  jumpDestsOfInstr      = LA64.jumpDestsOfInstr
  patchJumpInstr        = LA64.patchJumpInstr
  mkSpillInstr          = LA64.mkSpillInstr
  mkLoadInstr           = LA64.mkLoadInstr
  takeDeltaInstr        = LA64.takeDeltaInstr
  isMetaInstr           = LA64.isMetaInstr
  mkRegRegMoveInstr _ _ = LA64.mkRegRegMoveInstr
  takeRegRegMoveInstr _ = LA64.takeRegRegMoveInstr
  mkJumpInstr           = LA64.mkJumpInstr
  mkStackAllocInstr     = LA64.mkStackAllocInstr
  mkStackDeallocInstr   = LA64.mkStackDeallocInstr
  mkComment             = pure . LA64.COMMENT . ftext
  pprInstr              = LA64.pprInstr
