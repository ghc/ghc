
module RegsBase (
	RegNo,
	Reg(..), 
	isRealReg, 
	unRealReg,
	isVirtualReg, 
	renameVirtualReg,

        RegClass(..)
)

where

import Outputable	( Outputable(..) )
import qualified Outputable
import Panic
import Unique

-- ---------------------------------------------------------------------------
-- Registers

-- RealRegs are machine regs which are available for allocation, in
-- the usual way.  We know what class they are, because that's part of
-- the processor's architecture.

-- VirtualRegs are virtual registers.  The register allocator will
-- eventually have to map them into RealRegs, or into spill slots.
-- VirtualRegs are allocated on the fly, usually to represent a single
-- value in the abstract assembly code (i.e. dynamic registers are
-- usually single assignment).  With the new register allocator, the
-- single assignment restriction isn't necessary to get correct code,
-- although a better register allocation will result if single
-- assignment is used -- because the allocator maps a VirtualReg into
-- a single RealReg, even if the VirtualReg has multiple live ranges.

-- Virtual regs can be of either class, so that info is attached.

type RegNo 
	= Int

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


-- RegClass --------------------------------------------------------------------
data RegClass 
   = RcInteger 
   | RcFloat
   | RcDouble
     deriving Eq

instance Uniquable RegClass where
    getUnique RcInteger	= mkUnique 'L' 0
    getUnique RcFloat	= mkUnique 'L' 1
    getUnique RcDouble	= mkUnique 'L' 2

instance Outputable RegClass where
    ppr RcInteger	= Outputable.text "I"
    ppr RcFloat		= Outputable.text "F"
    ppr RcDouble	= Outputable.text "D"



