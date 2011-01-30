-----------------------------------------------------------------------------
--
-- Machine-specific parts of the register allocator
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------

module PPC.RegInfo (
        JumpDest( DestBlockId ), 
	canShortcut, 
	shortcutJump, 

	shortcutStatic
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import PPC.Regs
import PPC.Instr

import BlockId
import OldCmm
import CLabel

import Outputable
import Unique

data JumpDest = DestBlockId BlockId | DestImm Imm

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other


-- Here because it knows about JumpDest
shortcutStatic :: (BlockId -> Maybe JumpDest) -> CmmStatic -> CmmStatic

shortcutStatic fn (CmmStaticLit (CmmLabel lab))
  | Just uq <- maybeAsmTemp lab 
  = CmmStaticLit (CmmLabel (shortBlockId fn (mkBlockId uq)))

shortcutStatic fn (CmmStaticLit (CmmLabelDiffOff lbl1 lbl2 off))
  | Just uq <- maybeAsmTemp lbl1
  = CmmStaticLit (CmmLabelDiffOff (shortBlockId fn (mkBlockId uq)) lbl2 off)
        -- slightly dodgy, we're ignoring the second label, but this
        -- works with the way we use CmmLabelDiffOff for jump tables now.

shortcutStatic _ other_static
        = other_static

shortBlockId 
	:: (BlockId -> Maybe JumpDest)
	-> BlockId
	-> CLabel

shortBlockId fn blockid =
   case fn blockid of
      Nothing -> mkAsmTempLabel uq
      Just (DestBlockId blockid')  -> shortBlockId fn blockid'
      Just (DestImm (ImmCLbl lbl)) -> lbl
      _other -> panic "shortBlockId"
   where uq = getUnique blockid

