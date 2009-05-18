-----------------------------------------------------------------------------
--
-- Machine-specific parts of the register allocator
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------

module PPC.RegInfo (
	mkVReg,

        JumpDest, 
	canShortcut, 
	shortcutJump, 

	shortcutStatic,
	regDotColor
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import PPC.Regs
import PPC.Instr
import RegClass
import Reg
import Size

import BlockId
import Cmm
import CLabel

import Outputable
import Unique

mkVReg :: Unique -> Size -> Reg
mkVReg u size
   | not (isFloatSize size) = RegVirtual $ VirtualRegI u
   | otherwise
   = case size of
        FF32	-> RegVirtual $ VirtualRegD u
        FF64	-> RegVirtual $ VirtualRegD u
	_	-> panic "mkVReg"




data JumpDest = DestBlockId BlockId | DestImm Imm

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other


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

shortcutStatic _ other_static
        = other_static

shortBlockId 
	:: (BlockId -> Maybe JumpDest)
	-> BlockId
	-> CLabel

shortBlockId fn blockid@(BlockId uq) =
   case fn blockid of
      Nothing -> mkAsmTempLabel uq
      Just (DestBlockId blockid')  -> shortBlockId fn blockid'
      Just (DestImm (ImmCLbl lbl)) -> lbl
      _other -> panic "shortBlockId"



regDotColor :: Reg -> SDoc
regDotColor reg
 = case regClass reg of
 	RcInteger	-> text "blue"
	RcFloat		-> text "red"
	RcDouble	-> text "green"
