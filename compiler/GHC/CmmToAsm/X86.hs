{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Native code generator for x86 and x86-64 architectures
module GHC.CmmToAsm.X86
   ( ncgX86_64
   , ncgX86
   )
where

import GHC.Prelude

import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.Types.Basic (Alignment)

import qualified GHC.CmmToAsm.X86.Instr   as X86
import qualified GHC.CmmToAsm.X86.Ppr     as X86
import qualified GHC.CmmToAsm.X86.CodeGen as X86
import qualified GHC.CmmToAsm.X86.Regs    as X86

ncgX86 :: NCGConfig -> NcgImpl (Alignment, RawCmmStatics) X86.Instr X86.JumpDest
ncgX86 = ncgX86_64


ncgX86_64 :: NCGConfig -> NcgImpl (Alignment, RawCmmStatics) X86.Instr X86.JumpDest
ncgX86_64 config = NcgImpl
   { ncgConfig                 = config
   , cmmTopCodeGen             = X86.cmmTopCodeGen
   , generateJumpTableForInstr = X86.generateJumpTableForInstr config
   , getJumpDestBlockId        = X86.getJumpDestBlockId
   , canShortcut               = X86.canShortcut
   , shortcutStatics           = X86.shortcutStatics
   , shortcutJump              = X86.shortcutJump
   , pprNatCmmDeclS            = X86.pprNatCmmDecl config
   , pprNatCmmDeclH            = X86.pprNatCmmDecl config
   , maxSpillSlots             = X86.maxSpillSlots config
   , allocatableRegs           = X86.allocatableRegs platform
   , ncgAllocMoreStack         = X86.allocMoreStack platform
   , ncgMakeFarBranches        = \_p _i bs -> pure bs
   , extractUnwindPoints       = X86.extractUnwindPoints
   , invertCondBranches        = X86.invertCondBranches
   }
    where
      platform = ncgPlatform config

-- | Instruction instance for x86 instruction set.
instance Instruction X86.Instr where
   regUsageOfInstr         = X86.regUsageOfInstr
   patchRegsOfInstr        = X86.patchRegsOfInstr
   isJumpishInstr          = X86.isJumpishInstr
   jumpDestsOfInstr        = X86.jumpDestsOfInstr
   canFallthroughTo        = X86.canFallthroughTo
   patchJumpInstr          = X86.patchJumpInstr
   mkSpillInstr            = X86.mkSpillInstr
   mkLoadInstr             = X86.mkLoadInstr
   takeDeltaInstr          = X86.takeDeltaInstr
   isMetaInstr             = X86.isMetaInstr
   mkRegRegMoveInstr       = X86.mkRegRegMoveInstr
   takeRegRegMoveInstr     = X86.takeRegRegMoveInstr
   mkJumpInstr             = X86.mkJumpInstr
   mkStackAllocInstr       = X86.mkStackAllocInstr
   mkStackDeallocInstr     = X86.mkStackDeallocInstr
   pprInstr                = X86.pprInstr
   mkComment               = pure . X86.COMMENT
