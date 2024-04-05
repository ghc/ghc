{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Native code generator for PPC architectures
module GHC.CmmToAsm.PPC
   ( ncgPPC
   )
where

import GHC.Prelude

import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types

import qualified GHC.CmmToAsm.PPC.Instr   as PPC
import qualified GHC.CmmToAsm.PPC.Ppr     as PPC
import qualified GHC.CmmToAsm.PPC.CodeGen as PPC
import qualified GHC.CmmToAsm.PPC.Regs    as PPC
import qualified GHC.CmmToAsm.PPC.RegInfo as PPC

ncgPPC :: NCGConfig -> NcgImpl RawCmmStatics PPC.Instr PPC.JumpDest
ncgPPC config = NcgImpl
   { ncgConfig                 = config
   , cmmTopCodeGen             = PPC.cmmTopCodeGen
   , generateJumpTableForInstr = PPC.generateJumpTableForInstr config
   , getJumpDestBlockId        = PPC.getJumpDestBlockId
   , canShortcut               = PPC.canShortcut
   , shortcutStatics           = PPC.shortcutStatics
   , shortcutJump              = PPC.shortcutJump
   , pprNatCmmDeclH            = PPC.pprNatCmmDecl config
   , pprNatCmmDeclS            = PPC.pprNatCmmDecl config
   , maxSpillSlots             = PPC.maxSpillSlots config
   , allocatableRegs           = PPC.allocatableRegs platform
   , ncgAllocMoreStack         = PPC.allocMoreStack platform
   , ncgMakeFarBranches        = PPC.makeFarBranches
   , extractUnwindPoints       = const []
   , invertCondBranches        = \_ _ -> id
   }
    where
      platform = ncgPlatform config

-- | Instruction instance for powerpc
instance Instruction PPC.Instr where
   regUsageOfInstr     = PPC.regUsageOfInstr
   patchRegsOfInstr    = PPC.patchRegsOfInstr
   isJumpishInstr      = PPC.isJumpishInstr
   jumpDestsOfInstr    = PPC.jumpDestsOfInstr
   canFallthroughTo    = PPC.canFallthroughTo
   patchJumpInstr      = PPC.patchJumpInstr
   mkSpillInstr        = PPC.mkSpillInstr
   mkLoadInstr         = PPC.mkLoadInstr
   takeDeltaInstr      = PPC.takeDeltaInstr
   isMetaInstr         = PPC.isMetaInstr
   mkRegRegMoveInstr _ = PPC.mkRegRegMoveInstr
   takeRegRegMoveInstr = PPC.takeRegRegMoveInstr
   mkJumpInstr         = PPC.mkJumpInstr
   mkStackAllocInstr   = PPC.mkStackAllocInstr
   mkStackDeallocInstr = PPC.mkStackDeallocInstr
   pprInstr            = PPC.pprInstr
   mkComment           = pure . PPC.COMMENT
