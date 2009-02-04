{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Machine-specific parts of the register allocator
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------


module RegAllocInfo (
	-- shared code
	shortcutStatic,
	
	-- machine specific 
	RegUsage(..),
	noUsage,
	regUsage,
	patchRegs,
	jumpDests,
	isJumpish,
	patchJump,
	isRegRegMove,

        JumpDest, 
	canShortcut, 
	shortcutJump, 

	mkSpillInstr,
	mkLoadInstr,
	mkRegRegMoveInstr,
	mkBranchInstr,

	maxSpillSlots,
	spillSlotToOffset
    ) where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import BlockId
import Cmm
import CLabel
import Instrs
import Regs
import Outputable
import Constants	( rESERVED_C_STACK_BYTES )
import FastBool

#if   alpha_TARGET_ARCH
import Alpha.RegInfo

#elif i386_TARGET_ARCH || x86_64_TARGET_ARCH
import X86.RegInfo

#elif powerpc_TARGET_ARCH
import PPC.RegInfo

#elif sparc_TARGET_ARCH
import SPARC.RegInfo

#endif


-- Here because it knows about JumpDest
shortcutStatic :: (BlockId -> Maybe JumpDest) -> CmmStatic -> CmmStatic
shortcutStatic fn (CmmStaticLit (CmmLabel lab))
  | Just uq <- maybeAsmTemp lab 
  = CmmStaticLit (CmmLabel (shortBlockId fn (BlockId uq)))
shortcutStatic fn (CmmStaticLit (CmmLabelDiffOff lbl1 lbl2 off))
  | Just uq <- maybeAsmTemp lbl1
  = CmmStaticLit (CmmLabelDiffOff (shortBlockId fn (BlockId uq)) lbl2 off)
        -- slightly dodgy, we're ignoring the second label, but this
        -- works with the way we use CmmLabelDiffOff for jump tables now.
shortcutStatic fn other_static
        = other_static

shortBlockId fn blockid@(BlockId uq) =
   case fn blockid of
      Nothing -> mkAsmTempLabel uq
      Just (DestBlockId blockid')  -> shortBlockId fn blockid'
      Just (DestImm (ImmCLbl lbl)) -> lbl
      _other -> panic "shortBlockId"





