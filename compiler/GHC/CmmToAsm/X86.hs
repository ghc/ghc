{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Native code generator for x86 and x86-64 architectures
module GHC.CmmToAsm.X86
   ( ncgX86_64
   , ncgX86
   )
where

import GHC.Prelude

import GHC.Types.Unique.Supply

import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.Types.Basic (Alignment)

import qualified GHC.CmmToAsm.X86.Instr   as X86
import qualified GHC.CmmToAsm.X86.Ppr     as X86
import qualified GHC.CmmToAsm.X86.CodeGen as X86
import qualified GHC.CmmToAsm.X86.Regs    as X86
import qualified GHC.CmmToAsm.Reg.Linear.X86Base    as X86Base
-- import qualified GHC.CmmToAsm.Reg.Linear.X86_64 as X86_64

import qualified GHC.CmmToAsm.Reg.Linear as Linear
import qualified GHC.CmmToAsm.Reg.Linear.FreeRegs as Linear
import GHC.CmmToAsm.Reg.Liveness (LiveCmmDecl)

{-# SPECIALIZE Linear.regAlloc :: X86Base.FreeRegsX86 -> NCGConfig
                      -> LiveCmmDecl statics X86.Instr
                      -> UniqSM
                           (NatCmmDecl statics X86.Instr,
                            Maybe Int,
                            Maybe Linear.RegAllocStats) #-}

{-# SPECIALIZE Linear.regAlloc :: X86Base.FreeRegsX86_64 -> NCGConfig
                      -> LiveCmmDecl statics X86.Instr
                      -> UniqSM
                           (NatCmmDecl statics X86.Instr,
                            Maybe Int,
                            Maybe Linear.RegAllocStats) #-}

linearRegAllocX86 :: NCGConfig
                      -> LiveCmmDecl statics X86.Instr
                      -> UniqSM
                           (NatCmmDecl statics X86.Instr,
                            Maybe Int,
                            Maybe Linear.RegAllocStats)
linearRegAllocX86 config = Linear.regAlloc
                              (Linear.frInitFreeRegs (ncgPlatform config) :: X86Base.FreeRegsX86)
                              config

linearRegAllocX86_64 :: NCGConfig
                      -> LiveCmmDecl statics X86.Instr
                      -> UniqSM
                           (NatCmmDecl statics X86.Instr,
                            Maybe Int,
                            Maybe Linear.RegAllocStats)
linearRegAllocX86_64 config = Linear.regAlloc
                                 (Linear.frInitFreeRegs (ncgPlatform config) :: X86Base.FreeRegsX86_64)
                                 config

ncgX86 :: NCGConfig -> NcgImpl (Alignment, RawCmmStatics) X86.Instr X86.JumpDest
ncgX86 config = (ncgX86_64 config) { linearRegAlloc = linearRegAllocX86 }

ncgX86_64 :: NCGConfig -> NcgImpl (Alignment, RawCmmStatics) X86.Instr X86.JumpDest
ncgX86_64 config = NcgImpl
   { ncgConfig                 = config
   , cmmTopCodeGen             = X86.cmmTopCodeGen
   , generateJumpTableForInstr = X86.generateJumpTableForInstr config
   , getJumpDestBlockId        = X86.getJumpDestBlockId
   , canShortcut               = X86.canShortcut
   , shortcutStatics           = X86.shortcutStatics
   , shortcutJump              = X86.shortcutJump
   , pprNatCmmDecl             = X86.pprNatCmmDecl config
   , maxSpillSlots             = X86.maxSpillSlots config
   , allocatableRegs           = X86.allocatableRegs platform
   , ncgAllocMoreStack         = X86.allocMoreStack platform
   , ncgExpandTop              = id
   , ncgMakeFarBranches        = const id
   , extractUnwindPoints       = X86.extractUnwindPoints
   , invertCondBranches        = X86.invertCondBranches
   , linearRegAlloc            = linearRegAllocX86_64
   }
    where
      platform = ncgPlatform config

-- | Instruction instance for x86 instruction set.
instance Instruction X86.Instr where
   regUsageOfInstr         = X86.regUsageOfInstr
   patchRegsOfInstr        = X86.patchRegsOfInstr
   isJumpishInstr          = X86.isJumpishInstr
   jumpDestsOfInstr        = X86.jumpDestsOfInstr
   patchJumpInstr          = X86.patchJumpInstr
   mkSpillInstr            = X86.mkSpillInstr
   mkLoadInstr             = X86.mkLoadInstr
   {-# INLINEABLE takeDeltaInstr #-}
   takeDeltaInstr          = X86.takeDeltaInstr
   isMetaInstr             = X86.isMetaInstr
   mkRegRegMoveInstr       = X86.mkRegRegMoveInstr
   {-# INLINEABLE takeRegRegMoveInstr #-}
   takeRegRegMoveInstr     = X86.takeRegRegMoveInstr
   {-# INLINEABLE mkJumpInstr #-}
   mkJumpInstr             = X86.mkJumpInstr
   {-# INLINEABLE mkStackAllocInstr #-}
   mkStackAllocInstr       = X86.mkStackAllocInstr
   {-# INLINEABLE mkStackDeallocInstr #-}
   mkStackDeallocInstr     = X86.mkStackDeallocInstr
   {-# INLINEABLE pprInstr #-}
   pprInstr                = X86.pprInstr

