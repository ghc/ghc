{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Native code generator for AArch64 architectures
module GHC.CmmToAsm.AArch64
   ( ncgAArch64 )
where

import GHC.Prelude

import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.Utils.Outputable (ftext)

import qualified GHC.CmmToAsm.AArch64.Instr   as AArch64
import qualified GHC.CmmToAsm.AArch64.Ppr     as AArch64
import qualified GHC.CmmToAsm.AArch64.CodeGen as AArch64
import qualified GHC.CmmToAsm.AArch64.Regs    as AArch64
import qualified GHC.CmmToAsm.AArch64.RegInfo as AArch64

ncgAArch64 :: NCGConfig -> NcgImpl RawCmmStatics AArch64.Instr AArch64.JumpDest
ncgAArch64 config
 = NcgImpl {
        ncgConfig                 = config
       ,cmmTopCodeGen             = AArch64.cmmTopCodeGen
       ,generateJumpTableForInstr = AArch64.generateJumpTableForInstr config
       ,getJumpDestBlockId        = AArch64.getJumpDestBlockId
       ,canShortcut               = AArch64.canShortcut
       ,shortcutStatics           = AArch64.shortcutStatics
       ,shortcutJump              = AArch64.shortcutJump
       ,pprNatCmmDeclS            = AArch64.pprNatCmmDecl config
       ,pprNatCmmDeclH            = AArch64.pprNatCmmDecl config
       ,maxSpillSlots             = AArch64.maxSpillSlots config
       ,allocatableRegs           = AArch64.allocatableRegs platform
       ,ncgAllocMoreStack         = AArch64.allocMoreStack platform
       ,ncgMakeFarBranches        = AArch64.makeFarBranches
       ,extractUnwindPoints       = const []
       ,invertCondBranches        = \_ _ blocks -> blocks
  }
    where
      platform = ncgPlatform config

-- | Instruction instance for aarch64
instance Instruction AArch64.Instr where
        regUsageOfInstr         = AArch64.regUsageOfInstr
        patchRegsOfInstr _      = AArch64.patchRegsOfInstr
        isJumpishInstr          = AArch64.isJumpishInstr
        jumpDestsOfInstr        = AArch64.jumpDestsOfInstr
        canFallthroughTo        = AArch64.canFallthroughTo
        patchJumpInstr          = AArch64.patchJumpInstr
        mkSpillInstr            = AArch64.mkSpillInstr
        mkLoadInstr             = AArch64.mkLoadInstr
        takeDeltaInstr          = AArch64.takeDeltaInstr
        isMetaInstr             = AArch64.isMetaInstr
        mkRegRegMoveInstr _     = AArch64.mkRegRegMoveInstr
        takeRegRegMoveInstr _   = AArch64.takeRegRegMoveInstr
        mkJumpInstr             = AArch64.mkJumpInstr
        mkStackAllocInstr       = AArch64.mkStackAllocInstr
        mkStackDeallocInstr     = AArch64.mkStackDeallocInstr
        mkComment               = pure . AArch64.COMMENT . ftext
        pprInstr                = AArch64.pprInstr
