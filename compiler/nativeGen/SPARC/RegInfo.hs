
-----------------------------------------------------------------------------
--
-- Machine-specific parts of the register allocator
--
-- (c) The University of Glasgow 1996-2004
--
-----------------------------------------------------------------------------

module SPARC.RegInfo (
	mkVReg,

	riZero,
	fpRelEA,
	moveSp,
	fPair,

	shortcutStatic,
	regDotColor,

        JumpDest(..), 
	canShortcut, 
	shortcutJump,
)

where

import SPARC.Instr
import SPARC.Regs
import RegClass
import Reg
import Size

import Constants	(wORD_SIZE)
import Cmm
import CLabel
import BlockId
import Outputable
import Unique


-- | Make a virtual reg with this size.
mkVReg :: Unique -> Size -> Reg
mkVReg u size
	| not (isFloatSize size) 
	= VirtualRegI u

	| otherwise
	= case size of
		FF32    -> VirtualRegF u
		FF64	-> VirtualRegD u
		_ 	-> panic "mkVReg"


-- | Check if a RI represents a zero value.
--  	- a literal zero
--	- register %g0, which is always zero.
--
riZero :: RI -> Bool
riZero (RIImm (ImmInt 0))	    = True
riZero (RIImm (ImmInteger 0))	    = True
riZero (RIReg (RealReg 0))          = True
riZero _			    = False


-- | Calculate the effective address which would be used by the
-- 	corresponding fpRel sequence.  fpRel is in MachRegs.lhs,
-- 	alas -- can't have fpRelEA here because of module dependencies.
fpRelEA :: Int -> Reg -> Instr
fpRelEA n dst
   = ADD False False fp (RIImm (ImmInt (n * wORD_SIZE))) dst


-- | Code to shift the stack pointer by n words.
moveSp :: Int -> Instr
moveSp n
   = ADD False False sp (RIImm (ImmInt (n * wORD_SIZE))) sp


-- | Produce the second-half-of-a-double register given the first half.
fPair :: Reg -> Maybe Reg
fPair (RealReg n) 
	| n >= 32 && n `mod` 2 == 0  = Just (RealReg (n+1))

fPair (VirtualRegD u)
	= Just (VirtualRegHi u)

fPair _
	= trace ("MachInstrs.fPair: can't get high half of supposed double reg ") 
		Nothing

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

shortBlockId :: (BlockId -> Maybe JumpDest) -> BlockId -> CLabel
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



data JumpDest = DestBlockId BlockId | DestImm Imm

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other
