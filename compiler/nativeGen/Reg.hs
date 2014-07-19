
-- | An architecture independent description of a register.
--	This needs to stay architecture independent because it is used
--	by NCGMonad and the register allocators, which are shared
--	by all architectures.
--

{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module Reg (
	RegNo,
	Reg(..),
	regPair,
	regSingle,
	isRealReg,	takeRealReg,
	isVirtualReg,	takeVirtualReg,
	
	VirtualReg(..),
	renameVirtualReg,
	classOfVirtualReg,
	getHiVirtualRegFromLo,
	getHiVRegFromLo,

	RealReg(..),
	regNosOfRealReg,
	realRegsAlias,
	
	liftPatchFnToRegReg
)

where

import Outputable
import Unique
import RegClass
import Data.List

-- | An identifier for a primitive real machine register.
type RegNo 
	= Int

-- VirtualRegs are virtual registers.  The register allocator will
-- 	eventually have to map them into RealRegs, or into spill slots.
--
-- 	VirtualRegs are allocated on the fly, usually to represent a single
-- 	value in the abstract assembly code (i.e. dynamic registers are
-- 	usually single assignment).  
--
--	The  single assignment restriction isn't necessary to get correct code,
-- 	although a better register allocation will result if single
-- 	assignment is used -- because the allocator maps a VirtualReg into
-- 	a single RealReg, even if the VirtualReg has multiple live ranges.
--
-- 	Virtual regs can be of either class, so that info is attached.
--
data VirtualReg
	= VirtualRegI  {-# UNPACK #-} !Unique
	| VirtualRegHi {-# UNPACK #-} !Unique  -- High part of 2-word register
	| VirtualRegF  {-# UNPACK #-} !Unique
	| VirtualRegD  {-# UNPACK #-} !Unique
	| VirtualRegSSE {-# UNPACK #-} !Unique
	deriving (Eq, Show, Ord)

instance Uniquable VirtualReg where
	getUnique reg
	 = case reg of
	 	VirtualRegI u	-> u
		VirtualRegHi u	-> u
		VirtualRegF u	-> u
		VirtualRegD u	-> u
		VirtualRegSSE u	-> u

instance Outputable VirtualReg where
	ppr reg
	 = case reg of
		VirtualRegI  u  -> text "%vI_" 	<> pprUnique u
		VirtualRegHi u  -> text "%vHi_" <> pprUnique u
		VirtualRegF  u  -> text "%vF_" 	<> pprUnique u
		VirtualRegD  u  -> text "%vD_" 	<> pprUnique u
		VirtualRegSSE u -> text "%vSSE_" <> pprUnique u


renameVirtualReg :: Unique -> VirtualReg -> VirtualReg
renameVirtualReg u r
 = case r of
	VirtualRegI _	-> VirtualRegI  u
	VirtualRegHi _	-> VirtualRegHi u
	VirtualRegF _	-> VirtualRegF  u
	VirtualRegD _	-> VirtualRegD  u
	VirtualRegSSE _	-> VirtualRegSSE u


classOfVirtualReg :: VirtualReg -> RegClass
classOfVirtualReg vr
 = case vr of
 	VirtualRegI{}	-> RcInteger
	VirtualRegHi{}	-> RcInteger
	VirtualRegF{}	-> RcFloat
	VirtualRegD{}	-> RcDouble
	VirtualRegSSE{}	-> RcDoubleSSE


-- Determine the upper-half vreg for a 64-bit quantity on a 32-bit platform
-- when supplied with the vreg for the lower-half of the quantity.
-- (NB. Not reversible).
getHiVirtualRegFromLo :: VirtualReg -> VirtualReg
getHiVirtualRegFromLo reg
 = case reg of
	-- makes a pseudo-unique with tag 'H'
 	VirtualRegI u	-> VirtualRegHi (newTagUnique u 'H') 
	_		-> panic "Reg.getHiVirtualRegFromLo"

getHiVRegFromLo :: Reg -> Reg
getHiVRegFromLo reg
 = case reg of
 	RegVirtual  vr	-> RegVirtual (getHiVirtualRegFromLo vr)
	RegReal _	-> panic "Reg.getHiVRegFromLo"
 	

------------------------------------------------------------------------------------
-- | RealRegs are machine regs which are available for allocation, in
-- 	the usual way.  We know what class they are, because that's part of
-- 	the processor's architecture.
--
-- 	RealRegPairs are pairs of real registers that are allocated together
--	to hold a larger value, such as with Double regs on SPARC.
--
data RealReg
	= RealRegSingle	{-# UNPACK #-} !RegNo
	| RealRegPair	{-# UNPACK #-} !RegNo {-# UNPACK #-} !RegNo
	deriving (Eq, Show, Ord)

instance Uniquable RealReg where
	getUnique reg
	 = case reg of
	 	RealRegSingle i		-> mkRegSingleUnique i
		RealRegPair r1 r2	-> mkRegPairUnique (r1 * 65536 + r2)

instance Outputable RealReg where
	ppr reg
	 = case reg of
	 	RealRegSingle i		-> text "%r" 	<> int i
		RealRegPair r1 r2	-> text "%r(" <> int r1 <> text "|" <> int r2 <> text ")"

regNosOfRealReg :: RealReg -> [RegNo]
regNosOfRealReg rr
 = case rr of
 	RealRegSingle r1	-> [r1]
	RealRegPair   r1 r2	-> [r1, r2]
	

realRegsAlias :: RealReg -> RealReg -> Bool
realRegsAlias rr1 rr2
	= not $ null $ intersect (regNosOfRealReg rr1) (regNosOfRealReg rr2)

--------------------------------------------------------------------------------
-- | A register, either virtual or real
data Reg
	= RegVirtual !VirtualReg
	| RegReal    !RealReg
	deriving (Eq, Ord)

regSingle :: RegNo -> Reg
regSingle regNo		= RegReal $ RealRegSingle regNo

regPair :: RegNo -> RegNo -> Reg
regPair regNo1 regNo2	= RegReal $ RealRegPair regNo1 regNo2


-- We like to have Uniques for Reg so that we can make UniqFM and UniqSets 
-- in the register allocator.
instance Uniquable Reg where
	getUnique reg
	 = case reg of
	 	RegVirtual vr	-> getUnique vr
		RegReal    rr	-> getUnique rr
	
-- | Print a reg in a generic manner
--	If you want the architecture specific names, then use the pprReg 
--	function from the appropriate Ppr module.
instance Outputable Reg where
	ppr reg
	 = case reg of
	 	RegVirtual vr	-> ppr vr
		RegReal    rr	-> ppr rr


isRealReg :: Reg -> Bool
isRealReg reg 
 = case reg of
 	RegReal _	-> True
	RegVirtual _	-> False

takeRealReg :: Reg -> Maybe RealReg
takeRealReg reg
 = case reg of
 	RegReal rr	-> Just rr
	_		-> Nothing


isVirtualReg :: Reg -> Bool
isVirtualReg reg
 = case reg of
 	RegReal _	-> False
	RegVirtual _	-> True

takeVirtualReg :: Reg -> Maybe VirtualReg
takeVirtualReg reg
 = case reg of
 	RegReal _	-> Nothing
	RegVirtual vr	-> Just vr


-- | The patch function supplied by the allocator maps VirtualReg to RealReg
--	regs, but sometimes we want to apply it to plain old Reg.
--
liftPatchFnToRegReg  :: (VirtualReg -> RealReg) -> (Reg -> Reg)
liftPatchFnToRegReg patchF reg
 = case reg of
 	RegVirtual vr	-> RegReal (patchF vr)
	RegReal _	-> reg
	

