-- | An architecture independent description of a register.
--      This needs to stay architecture independent because it is used
--      by NCGMonad and the register allocators, which are shared
--      by all architectures.
--
module GHC.Platform.Reg (
        RegNo,
        Reg(..),
        regSingle,
        realRegSingle,
        isRealReg,      takeRealReg,
        isVirtualReg,   takeVirtualReg,

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

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Unique
import GHC.Builtin.Uniques
import GHC.Platform.Reg.Class

-- | An identifier for a primitive real machine register.
type RegNo
        = Int

-- VirtualRegs are virtual registers.  The register allocator will
--      eventually have to map them into RealRegs, or into spill slots.
--
--      VirtualRegs are allocated on the fly, usually to represent a single
--      value in the abstract assembly code (i.e. dynamic registers are
--      usually single assignment).
--
--      The  single assignment restriction isn't necessary to get correct code,
--      although a better register allocation will result if single
--      assignment is used -- because the allocator maps a VirtualReg into
--      a single RealReg, even if the VirtualReg has multiple live ranges.
--
--      Virtual regs can be of either class, so that info is attached.
--
data VirtualReg
        = VirtualRegI  {-# UNPACK #-} !Unique
        | VirtualRegHi {-# UNPACK #-} !Unique  -- High part of 2-word register
        | VirtualRegF  {-# UNPACK #-} !Unique
        | VirtualRegD  {-# UNPACK #-} !Unique

        deriving (Eq, Show)

-- This is laborious, but necessary. We can't derive Ord because
-- Unique doesn't have an Ord instance. Note nonDetCmpUnique in the
-- implementation. See Note [No Ord for Unique]
-- This is non-deterministic but we do not currently support deterministic
-- code-generation. See Note [Unique Determinism and code generation]
instance Ord VirtualReg where
  compare (VirtualRegI a) (VirtualRegI b) = nonDetCmpUnique a b
  compare (VirtualRegHi a) (VirtualRegHi b) = nonDetCmpUnique a b
  compare (VirtualRegF a) (VirtualRegF b) = nonDetCmpUnique a b
  compare (VirtualRegD a) (VirtualRegD b) = nonDetCmpUnique a b

  compare VirtualRegI{} _ = LT
  compare _ VirtualRegI{} = GT
  compare VirtualRegHi{} _ = LT
  compare _ VirtualRegHi{} = GT
  compare VirtualRegF{} _ = LT
  compare _ VirtualRegF{} = GT



instance Uniquable VirtualReg where
        getUnique reg
         = case reg of
                VirtualRegI u   -> u
                VirtualRegHi u  -> u
                VirtualRegF u   -> u
                VirtualRegD u   -> u

instance Outputable VirtualReg where
        ppr reg
         = case reg of
                VirtualRegI  u  -> text "%vI_"   <> pprUniqueAlways u
                VirtualRegHi u  -> text "%vHi_"  <> pprUniqueAlways u
                -- this code is kinda wrong on x86
                -- because float and double occupy the same register set
                -- namely SSE2 register xmm0 .. xmm15
                VirtualRegF  u  -> text "%vFloat_"   <> pprUniqueAlways u
                VirtualRegD  u  -> text "%vDouble_"   <> pprUniqueAlways u



renameVirtualReg :: Unique -> VirtualReg -> VirtualReg
renameVirtualReg u r
 = case r of
        VirtualRegI _   -> VirtualRegI  u
        VirtualRegHi _  -> VirtualRegHi u
        VirtualRegF _   -> VirtualRegF  u
        VirtualRegD _   -> VirtualRegD  u


classOfVirtualReg :: VirtualReg -> RegClass
classOfVirtualReg vr
 = case vr of
        VirtualRegI{}   -> RcInteger
        VirtualRegHi{}  -> RcInteger
        VirtualRegF{}   -> RcFloat
        VirtualRegD{}   -> RcDouble



-- Determine the upper-half vreg for a 64-bit quantity on a 32-bit platform
-- when supplied with the vreg for the lower-half of the quantity.
-- (NB. Not reversible).
getHiVirtualRegFromLo :: VirtualReg -> VirtualReg
getHiVirtualRegFromLo reg
 = case reg of
        -- makes a pseudo-unique with tag 'H'
        VirtualRegI u   -> VirtualRegHi (newTagUnique u 'H')
        _               -> panic "Reg.getHiVirtualRegFromLo"

getHiVRegFromLo :: Reg -> Reg
getHiVRegFromLo reg
 = case reg of
        RegVirtual  vr  -> RegVirtual (getHiVirtualRegFromLo vr)
        RegReal _       -> panic "Reg.getHiVRegFromLo"


------------------------------------------------------------------------------------
-- | RealRegs are machine regs which are available for allocation, in
--      the usual way.  We know what class they are, because that's part of
--      the processor's architecture.
--
newtype RealReg
        = RealRegSingle RegNo
        deriving (Eq, Show, Ord)

instance Uniquable RealReg where
        getUnique reg
         = case reg of
                RealRegSingle i         -> mkRegSingleUnique i

instance Outputable RealReg where
        ppr reg
         = case reg of
                RealRegSingle i         -> text "%r"  <> int i

regNosOfRealReg :: RealReg -> [RegNo]
regNosOfRealReg rr
 = case rr of
        RealRegSingle r1        -> [r1]


realRegsAlias :: RealReg -> RealReg -> Bool
realRegsAlias rr1 rr2 =
    -- used to be `not $ null $ intersect (regNosOfRealReg rr1) (regNosOfRealReg rr2)`
    -- but that resulted in some gnarly, gnarly, allocating code. So we manually
    -- write out all the cases which gives us nice non-allocating code.
    case rr1 of
        RealRegSingle r1 ->
            case rr2 of RealRegSingle r2 -> r1 == r2

--------------------------------------------------------------------------------
-- | A register, either virtual or real
data Reg
        = RegVirtual !VirtualReg
        | RegReal    !RealReg
        deriving (Eq, Ord, Show)

regSingle :: RegNo -> Reg
regSingle regNo = RegReal (realRegSingle regNo)

realRegSingle :: RegNo -> RealReg
realRegSingle regNo = RealRegSingle regNo


-- We like to have Uniques for Reg so that we can make UniqFM and UniqSets
-- in the register allocator.
instance Uniquable Reg where
        getUnique reg
         = case reg of
                RegVirtual vr   -> getUnique vr
                RegReal    rr   -> getUnique rr

-- | Print a reg in a generic manner
--      If you want the architecture specific names, then use the pprReg
--      function from the appropriate Ppr module.
instance Outputable Reg where
        ppr reg
         = case reg of
                RegVirtual vr   -> ppr vr
                RegReal    rr   -> ppr rr


isRealReg :: Reg -> Bool
isRealReg reg
 = case reg of
        RegReal _       -> True
        RegVirtual _    -> False

takeRealReg :: Reg -> Maybe RealReg
takeRealReg reg
 = case reg of
        RegReal rr      -> Just rr
        _               -> Nothing


isVirtualReg :: Reg -> Bool
isVirtualReg reg
 = case reg of
        RegReal _       -> False
        RegVirtual _    -> True

takeVirtualReg :: Reg -> Maybe VirtualReg
takeVirtualReg reg
 = case reg of
        RegReal _       -> Nothing
        RegVirtual vr   -> Just vr


-- | The patch function supplied by the allocator maps VirtualReg to RealReg
--      regs, but sometimes we want to apply it to plain old Reg.
--
liftPatchFnToRegReg  :: (VirtualReg -> RealReg) -> (Reg -> Reg)
liftPatchFnToRegReg patchF reg
 = case reg of
        RegVirtual vr   -> RegReal (patchF vr)
        RegReal _       -> reg
