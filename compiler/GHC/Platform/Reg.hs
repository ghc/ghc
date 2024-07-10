{-# LANGUAGE MagicHash #-}

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
        pprPlatformReg,

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
import GHC.Exts ( Int(I#), dataToTag# )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Unique
import GHC.Builtin.Uniques
import GHC.Platform.Reg.Class
import qualified GHC.Platform.Reg.Class.Unified   as Unified
import qualified GHC.Platform.Reg.Class.Separate  as Separate
import qualified GHC.Platform.Reg.Class.NoVectors as NoVectors
import GHC.Platform.ArchOS

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
   -- | Integer virtual register
   = VirtualRegI    { virtualRegUnique :: {-# UNPACK #-} !Unique }
   -- | High part of 2-word virtual register
   | VirtualRegHi   { virtualRegUnique :: {-# UNPACK #-} !Unique }
   -- | Double virtual register
   | VirtualRegD    { virtualRegUnique :: {-# UNPACK #-} !Unique }
   -- | 128-bit wide vector virtual register
   | VirtualRegV128 { virtualRegUnique :: {-# UNPACK #-} !Unique }
   deriving (Eq, Show)

-- We can't derive Ord, because Unique doesn't have an Ord instance.
-- Note nonDetCmpUnique in the implementation. See Note [No Ord for Unique].
-- This is non-deterministic but we do not currently support deterministic
-- code-generation. See Note [Unique Determinism and code generation]
instance Ord VirtualReg where
  compare vr1 vr2 =
    case compare (I# (dataToTag# vr1)) (I# (dataToTag# vr2)) of
      LT -> LT
      GT -> GT
      EQ -> nonDetCmpUnique (virtualRegUnique vr1) (virtualRegUnique vr2)

instance Uniquable VirtualReg where
        getUnique = virtualRegUnique

instance Outputable VirtualReg where
  ppr = pprVirtualReg

pprVirtualReg :: IsLine doc => VirtualReg -> doc
pprVirtualReg reg
        = case reg of
                VirtualRegI    u -> text "%vI_"   <> pprUniqueAlways u
                VirtualRegHi   u -> text "%vHi_"  <> pprUniqueAlways u
                VirtualRegD    u -> text "%vDouble_" <> pprUniqueAlways u
                VirtualRegV128 u -> text "%vV128_"   <> pprUniqueAlways u


renameVirtualReg :: Unique -> VirtualReg -> VirtualReg
renameVirtualReg u r = r { virtualRegUnique = u }

classOfVirtualReg :: Arch -> VirtualReg -> RegClass
classOfVirtualReg arch vr
  = case vr of
        VirtualRegI{} ->
          case regArch of
            Unified   ->   Unified.RcInteger
            Separate  ->  Separate.RcInteger
            NoVectors -> NoVectors.RcInteger
        VirtualRegHi{} ->
          case regArch of
            Unified   ->   Unified.RcInteger
            Separate  ->  Separate.RcInteger
            NoVectors -> NoVectors.RcInteger
        VirtualRegD{} ->
          case regArch of
            Unified   ->   Unified.RcFloatOrVector
            Separate  ->  Separate.RcFloat
            NoVectors -> NoVectors.RcFloat
        VirtualRegV128{} ->
          case regArch of
            Unified   ->  Unified.RcFloatOrVector
            Separate  -> Separate.RcVector
            NoVectors -> pprPanic "classOfVirtualReg VirtualRegV128"
                           ( text "arch:" <+> text ( show arch ) )
  where
    regArch = registerArch arch

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
        ppr = pprRealReg

pprRealReg :: IsLine doc => RealReg -> doc
pprRealReg reg = case reg of
  RealRegSingle i -> text "%r"  <> int i

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

-- | 'pprPlatformReg' specialised to SDoc
instance Outputable Reg where
  ppr = pprPlatformReg

-- | Print a reg in a generic manner
--      If you want the architecture specific names, then use the pprReg
--      function from the appropriate Ppr module.
pprPlatformReg :: IsLine doc => Reg -> doc
pprPlatformReg reg
 = case reg of
        RegVirtual vr   -> pprVirtualReg vr
        RegReal    rr   -> pprRealReg rr
{-# SPECIALIZE pprPlatformReg :: Reg -> SDoc #-}
{-# SPECIALIZE pprPlatformReg :: Reg -> HLine #-}

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
