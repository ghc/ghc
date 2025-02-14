{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHC.Cmm.Reg
    ( -- * Cmm Registers
      CmmReg(..)
    , cmmRegType
    , cmmRegWidth
      -- * Local registers
    , LocalReg(..)
    , localRegType
      -- * Global registers
    , GlobalReg(..), isArgReg, globalRegSpillType, pprGlobalReg
    , spReg, hpReg, spLimReg, hpLimReg, nodeReg
    , currentTSOReg, currentNurseryReg, hpAllocReg, cccsReg
    , node, baseReg
    , GlobalRegUse(..), pprGlobalRegUse

    , GlobalArgRegs(..)
    ) where

import GHC.Prelude

import GHC.Platform
import GHC.Utils.Outputable
import GHC.Types.Unique
import GHC.Cmm.Type

-----------------------------------------------------------------------------
--              Cmm registers
-----------------------------------------------------------------------------

{- Note [GlobalReg vs GlobalRegUse]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We distinguish GlobalReg, which describes registers in the STG abstract machine,
with GlobalRegUse, which describes an usage of such a register to store values
of a particular CmmType.

For example, we might want to load/store an 8-bit integer in a register that
can store 32-bit integers.

The width of the type must fit in the register, i.e. for a usage
@GlobalRegUse reg ty@ we must have that

  > typeWidth ty <= typeWidth (globalRegSpillType reg)

The restrictions about what categories of types can be stored in a given
register are less easily stated. Some examples are:

  - Vanilla registers can contain both pointers (gcWord) and non-pointers (bWord),
    as well as sub-word sized values (e.g. b16).
  - On x86_64, SIMD registers can be used to hold vectors of both floating
    and integral values (e.g. XmmReg may store 2 Double values or 4 Int32 values).
-}

-- | A use of a global register at a particular type.
--
-- While a 'GlobalReg' identifies a global register in the STG machine,
-- a 'GlobalRegUse' also contains information about the type we are storing
-- in the register.
--
-- See Note [GlobalReg vs GlobalRegUse] for more information.
data GlobalRegUse
  = GlobalRegUse
    { globalRegUse_reg  :: !GlobalReg
      -- ^ The underlying 'GlobalReg'
    , globalRegUse_type :: !CmmType
      -- ^ The 'CmmType' at which we are using the 'GlobalReg'.
      --
      -- Its width must be less than the width of the 'GlobalReg':
      --
      -- > typeWidth ty <= typeWidth (globalRegSpillType platform reg)
    }
  deriving Show

instance Outputable GlobalRegUse where
  ppr (GlobalRegUse reg _) = ppr reg

pprGlobalRegUse :: IsLine doc => GlobalRegUse -> doc
pprGlobalRegUse (GlobalRegUse reg _) = pprGlobalReg reg

-- TODO: these instances should be removed in favour
-- of more surgical uses of equality.
instance Eq GlobalRegUse where
  GlobalRegUse r1 _ == GlobalRegUse r2 _ = r1 == r2
instance Ord GlobalRegUse where
  GlobalRegUse r1 _ `compare` GlobalRegUse r2 _ = compare r1 r2

data CmmReg
  = CmmLocal  {-# UNPACK #-} !LocalReg
  | CmmGlobal GlobalRegUse
  deriving ( Eq, Ord, Show )

instance Outputable CmmReg where
    ppr e = pprReg e

pprReg :: CmmReg -> SDoc
pprReg r
   = case r of
        CmmLocal  local                   -> pprLocalReg  local
        CmmGlobal (GlobalRegUse global _) -> pprGlobalReg global

cmmRegType :: CmmReg -> CmmType
cmmRegType (CmmLocal  reg) = localRegType reg
cmmRegType (CmmGlobal reg) = globalRegUse_type reg

cmmRegWidth :: CmmReg -> Width
cmmRegWidth = typeWidth . cmmRegType

-----------------------------------------------------------------------------
--              Local registers
-----------------------------------------------------------------------------

data LocalReg
  = LocalReg {-# UNPACK #-} !Unique !CmmType
    -- ^ Parameters:
    --   1. Identifier
    --   2. Type
  deriving Show

instance Eq LocalReg where
  (LocalReg u1 _) == (LocalReg u2 _) = u1 == u2

instance Outputable LocalReg where
    ppr e = pprLocalReg e

-- This is non-deterministic but we do not currently support deterministic
-- code-generation. See Note [Unique Determinism and code generation]
-- See Note [No Ord for Unique]
instance Ord LocalReg where
  compare (LocalReg u1 _) (LocalReg u2 _) = nonDetCmpUnique u1 u2

instance Uniquable LocalReg where
  getUnique (LocalReg uniq _) = uniq

localRegType :: LocalReg -> CmmType
localRegType (LocalReg _ rep) = rep

--
-- We only print the type of the local reg if it isn't wordRep
--
pprLocalReg :: LocalReg -> SDoc
pprLocalReg (LocalReg uniq rep) =
--   = ppr rep <> char '_' <> ppr uniq
-- Temp Jan08
    char '_' <> pprUnique uniq <>
       (if isWord32 rep -- && not (isGcPtrType rep) -- Temp Jan08               -- sigh
                    then dcolon <> ptr <> ppr rep
                    else dcolon <> ptr <> ppr rep)
   where
     pprUnique unique = sdocOption sdocSuppressUniques $ \case
       True  -> text "_locVar_"
       False -> ppr unique
     ptr = empty
         --if isGcPtrType rep
         --      then doubleQuotes (text "ptr")
         --      else empty

-----------------------------------------------------------------------------
--              Global STG registers
-----------------------------------------------------------------------------
{-
Note [Overlapping global registers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The backend might not faithfully implement the abstraction of the STG
machine with independent registers for different values of type
GlobalReg. Specifically, certain pairs of registers (r1, r2) may
overlap in the sense that a store to r1 invalidates the value in r2,
and vice versa.

Currently this occurs only on the x86_64 architecture where FloatReg n
and DoubleReg n are assigned the same microarchitectural register, in
order to allow functions to receive more Float# or Double# arguments
in registers (as opposed to on the stack).

There are no specific rules about which registers might overlap with
which other registers, but presumably it's safe to assume that nothing
will overlap with special registers like Sp or BaseReg.

Use GHC.Cmm.Utils.regsOverlap to determine whether two GlobalRegs overlap
on a particular platform. The instance Eq GlobalReg is syntactic
equality of STG registers and does not take overlap into
account. However it is still used in UserOfRegs/DefinerOfRegs and
there are likely still bugs there, beware!
-}

-- | An abstract global register for the STG machine.
--
-- See also 'GlobalRegUse', which denotes a usage of a register at a particular
-- type (e.g. using a 32-bit wide register to store an 8-bit wide value), as per
-- Note [GlobalReg vs GlobalRegUse].
data GlobalReg
  -- Argument and return registers
  = VanillaReg                  -- pointers, unboxed ints and chars
        {-# UNPACK #-} !Int     -- its number

  | FloatReg            -- single-precision floating-point registers
        {-# UNPACK #-} !Int     -- its number

  | DoubleReg           -- double-precision floating-point registers
        {-# UNPACK #-} !Int     -- its number

  | LongReg             -- long int registers (64-bit, really)
        {-# UNPACK #-} !Int     -- its number

  -- I think we should redesign 'GlobalReg', for example instead of
  -- FloatReg/DoubleReg/XmmReg/YmmReg/ZmmReg we could have a single VecReg
  -- which also stores the type we are storing in it.
  --
  -- We might then be able to get rid of GlobalRegUse, as the type information
  -- would already be contained in a 'GlobalReg'.

  | XmmReg                      -- 128-bit SIMD vector register
        {-# UNPACK #-} !Int     -- its number

  | YmmReg                      -- 256-bit SIMD vector register
        {-# UNPACK #-} !Int     -- its number

  | ZmmReg                      -- 512-bit SIMD vector register
        {-# UNPACK #-} !Int     -- its number

  -- STG registers
  | Sp                  -- ^ Stack ptr; points to last occupied stack location.
  | SpLim               -- ^ Stack limit
  | Hp                  -- ^ Heap ptr; points to last occupied heap location.
  | HpLim               -- ^ Heap limit register
  | CCCS                -- ^ Current cost-centre stack
  | CurrentTSO          -- ^ pointer to current thread's TSO
  | CurrentNursery      -- ^ pointer to allocation area
  | HpAlloc             -- ^ allocation count for heap check failure

                -- We keep the address of some commonly-called
                -- functions in the register table, to keep code
                -- size down:
  | EagerBlackholeInfo  -- ^ address of stg_EAGER_BLACKHOLE_info
  | GCEnter1            -- ^ address of stg_gc_enter_1
  | GCFun               -- ^ address of stg_gc_fun

  -- | Base offset for the register table, used for accessing registers
  -- which do not have real registers assigned to them.  This register
  -- will only appear after we have expanded GlobalReg into memory accesses
  -- (where necessary) in the native code generator.
  | BaseReg

  -- | The register used by the platform for the C stack pointer. This is
  -- a break in the STG abstraction used exclusively to setup stack unwinding
  -- information.
  | MachSp

  -- | A dummy register used to indicate to the stack unwinder where
  -- a routine would return to.
  | UnwindReturnReg

  -- | Base Register for PIC (position-independent code) calculations.
  --
  -- Only used inside the native code generator. Its exact meaning differs
  -- from platform to platform (see module PositionIndependentCode).
  | PicBaseReg

  deriving( Eq, Ord, Show )
    -- NOTE: the Ord instance affects the tuple layout in GHCi, see
    --       Note [GHCi and native call registers]

instance Outputable GlobalReg where
    ppr e = pprGlobalReg e

instance OutputableP env GlobalReg where
    pdoc _ = ppr

pprGlobalReg :: IsLine doc => GlobalReg -> doc
pprGlobalReg gr
    = case gr of
        VanillaReg n   -> char 'R' <> int n
        FloatReg   n   -> char 'F' <> int n
        DoubleReg  n   -> char 'D' <> int n
        LongReg    n   -> char 'L' <> int n
        XmmReg     n   -> text "XMM" <> int n
        YmmReg     n   -> text "YMM" <> int n
        ZmmReg     n   -> text "ZMM" <> int n
        Sp             -> text "Sp"
        SpLim          -> text "SpLim"
        Hp             -> text "Hp"
        HpLim          -> text "HpLim"
        MachSp         -> text "MachSp"
        UnwindReturnReg-> text "UnwindReturnReg"
        CCCS           -> text "CCCS"
        CurrentTSO     -> text "CurrentTSO"
        CurrentNursery -> text "CurrentNursery"
        HpAlloc        -> text "HpAlloc"
        EagerBlackholeInfo -> text "stg_EAGER_BLACKHOLE_info"
        GCEnter1       -> text "stg_gc_enter_1"
        GCFun          -> text "stg_gc_fun"
        BaseReg        -> text "BaseReg"
        PicBaseReg     -> text "PicBaseReg"
{-# SPECIALIZE pprGlobalReg :: GlobalReg -> SDoc #-}
{-# SPECIALIZE pprGlobalReg :: GlobalReg -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


-- convenient aliases
baseReg, spReg, hpReg, spLimReg, hpLimReg, nodeReg,
  currentTSOReg, currentNurseryReg, hpAllocReg, cccsReg :: Platform -> CmmReg
baseReg           p = CmmGlobal (GlobalRegUse BaseReg        $ bWord p)
spReg             p = CmmGlobal (GlobalRegUse Sp             $ bWord p)
hpReg             p = CmmGlobal (GlobalRegUse Hp             $ gcWord p)
hpLimReg          p = CmmGlobal (GlobalRegUse HpLim          $ bWord p)
spLimReg          p = CmmGlobal (GlobalRegUse SpLim          $ bWord p)
nodeReg           p = CmmGlobal (GlobalRegUse (VanillaReg 1) $ gcWord p)
currentTSOReg     p = CmmGlobal (GlobalRegUse CurrentTSO     $ bWord p)
currentNurseryReg p = CmmGlobal (GlobalRegUse CurrentNursery $ bWord p)
hpAllocReg        p = CmmGlobal (GlobalRegUse HpAlloc        $ bWord p)
cccsReg           p = CmmGlobal (GlobalRegUse CCCS           $ bWord p)

node :: GlobalReg
node = VanillaReg 1

globalRegSpillType :: Platform -> GlobalReg -> CmmType
globalRegSpillType platform = \case
   VanillaReg _ -> gcWord platform
   FloatReg   _ -> cmmFloat W32
   DoubleReg  _ -> cmmFloat W64
   LongReg    _ -> cmmBits  W64

   -- TODO: improve the internal model of SIMD/vectorized registers
   -- the right design SHOULD improve handling of float and double code too.
   -- see remarks in Note [SIMD Design for the future] in GHC.StgToCmm.Prim
   XmmReg    _ -> cmmVec  4 (cmmBits W32)
   YmmReg    _ -> cmmVec  8 (cmmBits W32)
   ZmmReg    _ -> cmmVec 16 (cmmBits W32)

   Hp          -> gcWord platform -- The initialiser for all
                                  -- dynamically allocated closures
   _           -> bWord platform

isArgReg :: GlobalReg -> Bool
isArgReg (VanillaReg {}) = True
isArgReg (FloatReg {})   = True
isArgReg (DoubleReg {})  = True
isArgReg (LongReg {})    = True
isArgReg (XmmReg {})     = True
isArgReg (YmmReg {})     = True
isArgReg (ZmmReg {})     = True
isArgReg _               = False

-- --------------------------------------------------------------------------

-- | Global registers used for argument passing.
--
-- See Note [realArgRegsCover] in GHC.Cmm.CallConv.
data GlobalArgRegs
  -- | General-purpose (integer) argument-passing registers.
  = GP_ARG_REGS
  -- | Scalar (integer & floating-point) argument-passing registers.
  | SCALAR_ARG_REGS
  -- | 16 byte vector argument-passing registers, together with
  -- integer & floating-point argument-passing scalar registers.
  | V16_ARG_REGS
  -- | 32 byte vector argument-passing registers, together with
  -- integer & floating-point argument-passing scalar registers.
  | V32_ARG_REGS
  -- | 64 byte vector argument-passing registers, together with
  -- integer & floating-point argument-passing scalar registers.
  | V64_ARG_REGS
  deriving ( Show, Eq, Ord )
