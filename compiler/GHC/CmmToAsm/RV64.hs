{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Native code generator for RiscV64 architectures
module GHC.CmmToAsm.RV64
   ( ncgRV64 )
where

import GHC.Prelude

import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.Utils.Outputable (ftext)

import qualified GHC.CmmToAsm.RV64.Instr   as RV64
import qualified GHC.CmmToAsm.RV64.Ppr     as RV64
import qualified GHC.CmmToAsm.RV64.CodeGen as RV64
import qualified GHC.CmmToAsm.RV64.Regs    as RV64
import qualified GHC.CmmToAsm.RV64.RegInfo as RV64

ncgRV64 :: NCGConfig -> NcgImpl RawCmmStatics RV64.Instr RV64.JumpDest
ncgRV64 config
 = NcgImpl {
        ncgConfig                 = config
       ,cmmTopCodeGen             = RV64.cmmTopCodeGen
       ,generateJumpTableForInstr = RV64.generateJumpTableForInstr config
       ,getJumpDestBlockId        = RV64.getJumpDestBlockId
       ,canShortcut               = RV64.canShortcut
       ,shortcutStatics           = RV64.shortcutStatics
       ,shortcutJump              = RV64.shortcutJump
       ,pprNatCmmDeclS            = RV64.pprNatCmmDecl config
       ,pprNatCmmDeclH            = RV64.pprNatCmmDecl config
       ,maxSpillSlots             = RV64.maxSpillSlots config
       ,allocatableRegs           = RV64.allocatableRegs platform
       ,ncgAllocMoreStack         = RV64.allocMoreStack platform
       ,ncgMakeFarBranches        = RV64.makeFarBranches
       ,extractUnwindPoints       = const []
       ,invertCondBranches        = \_ _ -> id
  }
    where
      platform = ncgPlatform config

  -- | Instruction instance for RV64
instance Instruction RV64.Instr where
        regUsageOfInstr         = RV64.regUsageOfInstr
        patchRegsOfInstr        = RV64.patchRegsOfInstr
        isJumpishInstr          = RV64.isJumpishInstr
        jumpDestsOfInstr        = RV64.jumpDestsOfInstr
        patchJumpInstr          = RV64.patchJumpInstr
        mkSpillInstr            = RV64.mkSpillInstr
        mkLoadInstr             = RV64.mkLoadInstr
        takeDeltaInstr          = RV64.takeDeltaInstr
        isMetaInstr             = RV64.isMetaInstr
        mkRegRegMoveInstr _     = RV64.mkRegRegMoveInstr
        takeRegRegMoveInstr     = RV64.takeRegRegMoveInstr
        mkJumpInstr             = RV64.mkJumpInstr
        mkStackAllocInstr       = RV64.mkStackAllocInstr
        mkStackDeallocInstr     = RV64.mkStackDeallocInstr
        mkComment               = pure . RV64.COMMENT . ftext
        pprInstr                = RV64.pprInstr
