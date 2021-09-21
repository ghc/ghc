{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Native code generator for SPARC architectures
module GHC.CmmToAsm.SPARC
   ( ncgSPARC
   )
where

import GHC.Prelude
import GHC.Utils.Panic

import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Instr

import qualified GHC.CmmToAsm.SPARC.Instr          as SPARC
import qualified GHC.CmmToAsm.SPARC.Ppr            as SPARC
import qualified GHC.CmmToAsm.SPARC.CodeGen        as SPARC
import qualified GHC.CmmToAsm.SPARC.CodeGen.Expand as SPARC
import qualified GHC.CmmToAsm.SPARC.Regs           as SPARC
import qualified GHC.CmmToAsm.SPARC.ShortcutJump   as SPARC


ncgSPARC :: NCGConfig -> NcgImpl RawCmmStatics SPARC.Instr SPARC.JumpDest
ncgSPARC config = NcgImpl
   { ncgConfig                 = config
   , cmmTopCodeGen             = SPARC.cmmTopCodeGen
   , generateJumpTableForInstr = SPARC.generateJumpTableForInstr platform
   , getJumpDestBlockId        = SPARC.getJumpDestBlockId
   , canShortcut               = SPARC.canShortcut
   , shortcutStatics           = SPARC.shortcutStatics
   , shortcutJump              = SPARC.shortcutJump
   , pprNatCmmDecl             = SPARC.pprNatCmmDecl config
   , maxSpillSlots             = SPARC.maxSpillSlots config
   , allocatableRegs           = SPARC.allocatableRegs
   , ncgExpandTop              = map SPARC.expandTop
   , ncgMakeFarBranches        = const id
   , extractUnwindPoints       = const []
   , invertCondBranches        = \_ _ -> id
   -- Allocating more stack space for spilling isn't currently supported for the
   -- linear register allocator on SPARC, hence the panic below.
   , ncgAllocMoreStack         = noAllocMoreStack
   }
    where
      platform = ncgPlatform config

      noAllocMoreStack amount _
        = panic $   "Register allocator: out of stack slots (need " ++ show amount ++ ")\n"
              ++  "   If you are trying to compile SHA1.hs from the crypto library then this\n"
              ++  "   is a known limitation in the linear allocator.\n"
              ++  "\n"
              ++  "   Try enabling the graph colouring allocator with -fregs-graph instead."
              ++  "   You can still file a bug report if you like.\n"


-- | instance for sparc instruction set
instance Instruction SPARC.Instr where
   regUsageOfInstr         = SPARC.regUsageOfInstr
   patchRegsOfInstr        = SPARC.patchRegsOfInstr
   isJumpishInstr          = SPARC.isJumpishInstr
   jumpDestsOfInstr        = SPARC.jumpDestsOfInstr
   patchJumpInstr          = SPARC.patchJumpInstr
   mkSpillInstr            = SPARC.mkSpillInstr
   mkLoadInstr             = SPARC.mkLoadInstr
   takeDeltaInstr          = SPARC.takeDeltaInstr
   isMetaInstr             = SPARC.isMetaInstr
   mkRegRegMoveInstr       = SPARC.mkRegRegMoveInstr
   takeRegRegMoveInstr     = SPARC.takeRegRegMoveInstr
   mkJumpInstr             = SPARC.mkJumpInstr
   pprInstr                = SPARC.pprInstr
   mkComment               = pure . SPARC.COMMENT
   mkStackAllocInstr       = panic "no sparc_mkStackAllocInstr"
   mkStackDeallocInstr     = panic "no sparc_mkStackDeallocInstr"
