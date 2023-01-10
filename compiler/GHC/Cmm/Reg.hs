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
    , GlobalReg(..), isArgReg, globalRegType
    , pprGlobalReg
    , spReg, hpReg, spLimReg, hpLimReg, nodeReg
    , currentTSOReg, currentNurseryReg, hpAllocReg, cccsReg
    , node, baseReg
    , VGcPtr(..)
    ) where

import GHC.Prelude

import GHC.Platform
import GHC.Utils.Outputable
import GHC.Types.Unique
import GHC.Cmm.Type

-----------------------------------------------------------------------------
--              Cmm registers
-----------------------------------------------------------------------------

data CmmReg
  = CmmLocal  {-# UNPACK #-} !LocalReg
  | CmmGlobal GlobalReg
  deriving( Eq, Ord, Show )

instance Outputable CmmReg where
    ppr e = pprReg e

pprReg :: CmmReg -> SDoc
pprReg r
   = case r of
        CmmLocal  local  -> pprLocalReg  local
        CmmGlobal global -> pprGlobalReg global

cmmRegType :: Platform -> CmmReg -> CmmType
cmmRegType _        (CmmLocal  reg) = localRegType reg
cmmRegType platform (CmmGlobal reg) = globalRegType platform reg

cmmRegWidth :: Platform -> CmmReg -> Width
cmmRegWidth platform = typeWidth . cmmRegType platform


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

data VGcPtr = VGcPtr | VNonGcPtr deriving( Eq, Show )

data GlobalReg
  -- Argument and return registers
  = VanillaReg                  -- pointers, unboxed ints and chars
        {-# UNPACK #-} !Int     -- its number
        VGcPtr

  | FloatReg            -- single-precision floating-point registers
        {-# UNPACK #-} !Int     -- its number

  | DoubleReg           -- double-precision floating-point registers
        {-# UNPACK #-} !Int     -- its number

  | LongReg             -- long int registers (64-bit, really)
        {-# UNPACK #-} !Int     -- its number

  | XmmReg                      -- 128-bit SIMD vector register
        {-# UNPACK #-} !Int     -- its number

  | YmmReg                      -- 256-bit SIMD vector register
        {-# UNPACK #-} !Int     -- its number

  | ZmmReg                      -- 512-bit SIMD vector register
        {-# UNPACK #-} !Int     -- its number

  -- STG registers
  | Sp                  -- Stack ptr; points to last occupied stack location.
  | SpLim               -- Stack limit
  | Hp                  -- Heap ptr; points to last occupied heap location.
  | HpLim               -- Heap limit register
  | CCCS                -- Current cost-centre stack
  | CurrentTSO          -- pointer to current thread's TSO
  | CurrentNursery      -- pointer to allocation area
  | HpAlloc             -- allocation count for heap check failure

                -- We keep the address of some commonly-called
                -- functions in the register table, to keep code
                -- size down:
  | EagerBlackholeInfo  -- stg_EAGER_BLACKHOLE_info
  | GCEnter1            -- stg_gc_enter_1
  | GCFun               -- stg_gc_fun

  -- Base offset for the register table, used for accessing registers
  -- which do not have real registers assigned to them.  This register
  -- will only appear after we have expanded GlobalReg into memory accesses
  -- (where necessary) in the native code generator.
  | BaseReg

  -- The register used by the platform for the C stack pointer. This is
  -- a break in the STG abstraction used exclusively to setup stack unwinding
  -- information.
  | MachSp

  -- The is a dummy register used to indicate to the stack unwinder where
  -- a routine would return to.
  | UnwindReturnReg

  -- Base Register for PIC (position-independent code) calculations
  -- Only used inside the native code generator. It's exact meaning differs
  -- from platform to platform (see module PositionIndependentCode).
  | PicBaseReg

  deriving( Show )

instance Eq GlobalReg where
   VanillaReg i _ == VanillaReg j _ = i==j -- Ignore type when seeking clashes
   FloatReg i == FloatReg j = i==j
   DoubleReg i == DoubleReg j = i==j
   LongReg i == LongReg j = i==j
   -- NOTE: XMM, YMM, ZMM registers actually are the same registers
   -- at least with respect to store at YMM i and then read from XMM i
   -- and similarly for ZMM etc.
   XmmReg i == XmmReg j = i==j
   YmmReg i == YmmReg j = i==j
   ZmmReg i == ZmmReg j = i==j
   Sp == Sp = True
   SpLim == SpLim = True
   Hp == Hp = True
   HpLim == HpLim = True
   CCCS == CCCS = True
   CurrentTSO == CurrentTSO = True
   CurrentNursery == CurrentNursery = True
   HpAlloc == HpAlloc = True
   EagerBlackholeInfo == EagerBlackholeInfo = True
   GCEnter1 == GCEnter1 = True
   GCFun == GCFun = True
   BaseReg == BaseReg = True
   MachSp == MachSp = True
   UnwindReturnReg == UnwindReturnReg = True
   PicBaseReg == PicBaseReg = True
   _r1 == _r2 = False

-- NOTE: this Ord instance affects the tuple layout in GHCi, see
--       Note [GHCi and native call registers]
instance Ord GlobalReg where
   compare (VanillaReg i _) (VanillaReg j _) = compare i j
     -- Ignore type when seeking clashes
   compare (FloatReg i)  (FloatReg  j) = compare i j
   compare (DoubleReg i) (DoubleReg j) = compare i j
   compare (LongReg i)   (LongReg   j) = compare i j
   compare (XmmReg i)    (XmmReg    j) = compare i j
   compare (YmmReg i)    (YmmReg    j) = compare i j
   compare (ZmmReg i)    (ZmmReg    j) = compare i j
   compare Sp Sp = EQ
   compare SpLim SpLim = EQ
   compare Hp Hp = EQ
   compare HpLim HpLim = EQ
   compare CCCS CCCS = EQ
   compare CurrentTSO CurrentTSO = EQ
   compare CurrentNursery CurrentNursery = EQ
   compare HpAlloc HpAlloc = EQ
   compare EagerBlackholeInfo EagerBlackholeInfo = EQ
   compare GCEnter1 GCEnter1 = EQ
   compare GCFun GCFun = EQ
   compare BaseReg BaseReg = EQ
   compare MachSp MachSp = EQ
   compare UnwindReturnReg UnwindReturnReg = EQ
   compare PicBaseReg PicBaseReg = EQ
   compare (VanillaReg _ _) _ = LT
   compare _ (VanillaReg _ _) = GT
   compare (FloatReg _) _     = LT
   compare _ (FloatReg _)     = GT
   compare (DoubleReg _) _    = LT
   compare _ (DoubleReg _)    = GT
   compare (LongReg _) _      = LT
   compare _ (LongReg _)      = GT
   compare (XmmReg _) _       = LT
   compare _ (XmmReg _)       = GT
   compare (YmmReg _) _       = LT
   compare _ (YmmReg _)       = GT
   compare (ZmmReg _) _       = LT
   compare _ (ZmmReg _)       = GT
   compare Sp _ = LT
   compare _ Sp = GT
   compare SpLim _ = LT
   compare _ SpLim = GT
   compare Hp _ = LT
   compare _ Hp = GT
   compare HpLim _ = LT
   compare _ HpLim = GT
   compare CCCS _ = LT
   compare _ CCCS = GT
   compare CurrentTSO _ = LT
   compare _ CurrentTSO = GT
   compare CurrentNursery _ = LT
   compare _ CurrentNursery = GT
   compare HpAlloc _ = LT
   compare _ HpAlloc = GT
   compare GCEnter1 _ = LT
   compare _ GCEnter1 = GT
   compare GCFun _ = LT
   compare _ GCFun = GT
   compare BaseReg _ = LT
   compare _ BaseReg = GT
   compare MachSp _ = LT
   compare _ MachSp = GT
   compare UnwindReturnReg _ = LT
   compare _ UnwindReturnReg = GT
   compare EagerBlackholeInfo _ = LT
   compare _ EagerBlackholeInfo = GT

instance Outputable GlobalReg where
    ppr e = pprGlobalReg e

instance OutputableP env GlobalReg where
    pdoc _ = ppr

pprGlobalReg :: IsLine doc => GlobalReg -> doc
pprGlobalReg gr
    = case gr of
        VanillaReg n _ -> char 'R' <> int n
-- Temp Jan08
--        VanillaReg n VNonGcPtr -> char 'R' <> int n
--        VanillaReg n VGcPtr    -> char 'P' <> int n
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
  currentTSOReg, currentNurseryReg, hpAllocReg, cccsReg  :: CmmReg
baseReg = CmmGlobal BaseReg
spReg = CmmGlobal Sp
hpReg = CmmGlobal Hp
hpLimReg = CmmGlobal HpLim
spLimReg = CmmGlobal SpLim
nodeReg = CmmGlobal node
currentTSOReg = CmmGlobal CurrentTSO
currentNurseryReg = CmmGlobal CurrentNursery
hpAllocReg = CmmGlobal HpAlloc
cccsReg = CmmGlobal CCCS

node :: GlobalReg
node = VanillaReg 1 VGcPtr

globalRegType :: Platform -> GlobalReg -> CmmType
globalRegType platform = \case
   (VanillaReg _ VGcPtr)    -> gcWord platform
   (VanillaReg _ VNonGcPtr) -> bWord platform
   (FloatReg _)             -> cmmFloat W32
   (DoubleReg _)            -> cmmFloat W64
   (LongReg _)              -> cmmBits W64
   -- TODO: improve the internal model of SIMD/vectorized registers
   -- the right design SHOULd improve handling of float and double code too.
   -- see remarks in Note [SIMD Design for the future] in GHC.StgToCmm.Prim
   (XmmReg _) -> cmmVec 4 (cmmBits W32)
   (YmmReg _) -> cmmVec 8 (cmmBits W32)
   (ZmmReg _) -> cmmVec 16 (cmmBits W32)

   Hp         -> gcWord platform -- The initialiser for all
                                 -- dynamically allocated closures
   _          -> bWord platform

isArgReg :: GlobalReg -> Bool
isArgReg (VanillaReg {}) = True
isArgReg (FloatReg {})   = True
isArgReg (DoubleReg {})  = True
isArgReg (LongReg {})    = True
isArgReg (XmmReg {})     = True
isArgReg (YmmReg {})     = True
isArgReg (ZmmReg {})     = True
isArgReg _               = False
