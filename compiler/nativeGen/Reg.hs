
-- | An architecture independent description of a register.
--	This needs to stay architecture independent because it is used
--	by NCGMonad and the register allocators, which are shared
--	by all architectures.
--
module Reg (
	RegNo,
	Reg(..),
	isRealReg,
	unRealReg,
	isVirtualReg,
	renameVirtualReg,
	getHiVRegFromLo
)

where

import Outputable
import Unique
import Panic

-- | An identifier for a real machine register.
type RegNo 
	= Int

-- RealRegs are machine regs which are available for allocation, in
-- 	the usual way.  We know what class they are, because that's part of
-- 	the processor's architecture.

-- VirtualRegs are virtual registers.  The register allocator will
-- 	eventually have to map them into RealRegs, or into spill slots.
--
-- 	VirtualRegs are allocated on the fly, usually to represent a single
-- 	value in the abstract assembly code (i.e. dynamic registers are
-- 	usually single assignment).  
--
--	With the new register allocator, the
-- 	single assignment restriction isn't necessary to get correct code,
-- 	although a better register allocation will result if single
-- 	assignment is used -- because the allocator maps a VirtualReg into
-- 	a single RealReg, even if the VirtualReg has multiple live ranges.

-- 	Virtual regs can be of either class, so that info is attached.
data Reg
	= RealReg      {-# UNPACK #-} !RegNo
	| VirtualRegI  {-# UNPACK #-} !Unique
	| VirtualRegHi {-# UNPACK #-} !Unique  -- High part of 2-word register
	| VirtualRegF  {-# UNPACK #-} !Unique
	| VirtualRegD  {-# UNPACK #-} !Unique
	deriving (Eq, Ord)


-- We like to have Uniques for Reg so that we can make UniqFM and UniqSets 
-- in the register allocator.
instance Uniquable Reg where
	getUnique (RealReg i)      = mkUnique 'C' i
	getUnique (VirtualRegI u)  = u
	getUnique (VirtualRegHi u) = u
	getUnique (VirtualRegF u)  = u
	getUnique (VirtualRegD u)  = u


-- | Print a reg in a generic manner
--	If you want the architecture specific names, then use the pprReg 
--	function from the appropriate Ppr module.
instance Outputable Reg where
	ppr reg
	 = case reg of
		RealReg i       -> text "%r" 	<> int i
		VirtualRegI  u  -> text "%vI_" 	<> pprUnique u
		VirtualRegHi u  -> text "%vHi_" <> pprUnique u
		VirtualRegF  u  -> text "%vF_" 	<> pprUnique u
		VirtualRegD  u  -> text "%vD_" 	<> pprUnique u



isRealReg :: Reg -> Bool
isRealReg = not . isVirtualReg

-- | Take the RegNo from a real reg
unRealReg :: Reg -> RegNo
unRealReg (RealReg i)	= i
unRealReg _		= panic "unRealReg on VirtualReg"

isVirtualReg :: Reg -> Bool
isVirtualReg (RealReg _)      = False
isVirtualReg (VirtualRegI _)  = True
isVirtualReg (VirtualRegHi _) = True
isVirtualReg (VirtualRegF _)  = True
isVirtualReg (VirtualRegD _)  = True


renameVirtualReg :: Unique -> Reg -> Reg
renameVirtualReg u r
 = case r of
 	RealReg _	-> error "renameVirtualReg: can't change unique on a real reg"
	VirtualRegI _	-> VirtualRegI  u
	VirtualRegHi _	-> VirtualRegHi u
	VirtualRegF _	-> VirtualRegF  u
	VirtualRegD _	-> VirtualRegD  u

-- Determine the upper-half vreg for a 64-bit quantity on a 32-bit platform
-- when supplied with the vreg for the lower-half of the quantity.
-- (NB. Not reversible).
getHiVRegFromLo :: Reg -> Reg
getHiVRegFromLo (VirtualRegI u) 
   = VirtualRegHi (newTagUnique u 'H') -- makes a pseudo-unique with tag 'H'

getHiVRegFromLo _ 
   = panic "RegsBase.getHiVRegFromLo"


