{-# LANGUAGE LambdaCase #-}

-- | CPU features and corresponding @-m\<feature\>@ flags.
--
-- See Note [CPU features].
module GHC.Platform.CpuFeatures
  ( -- * Feature flags
    CpuFeature (..)
  , cpuFeatureFlag

    -- * Enabled instruction sets
  , CpuFeatures (..)
  , initCpuFeatures
  , cpuFmaEnabled

    -- ** x86
  , X86Features (..)
  , SseAvxVersion (..)
  , AvxLevel (..)
  , x86SseVersion
  , x86Avx512Exts
  , sseAvxVersionIncludes
  , unionSseAvxVersion
  , sseAvxVectorRequirement
  , x86BaselineSseAvx
  , sseAvxVersionFlags
  , SseVersion (..)
  , Avx512Exts (..)
  , noAvx512Exts
  , BmiVersion (..)

    -- *** Querying x86 features
    -- | These are not independent; see Note [CPU features].
  , x86Ssse3Enabled
  , x86Sse4_1Enabled
  , x86Sse4_2Enabled
  , x86AvxEnabled
  , x86Avx2Enabled
  , x86FmaEnabled
  , x86Avx512FEnabled
  , x86Avx512DQEnabled
  , x86Avx512VLEnabled
  , x86Bmi2Enabled
  )
where

import GHC.Prelude

import GHC.Platform.ArchOS
import GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet

import Data.Maybe (isJust)

{- Note [CPU features]
~~~~~~~~~~~~~~~~~~~~~~
The -m<feature> flags enable CPU instruction set extensions. Many of them imply
others:

  1. AVX2 -> AVX -> SSE4.2 -> SSE4 -> SSSE3 -> SSE3 -> SSE2 -> SSE1
  2. BMI2 -> BMI1
  3. FMA -> AVX    (on X86)
  4. AVX512{BW,CD,DQ,ER,PF,VL} -> AVX512F -> AVX2 + FMA

We keep two representations:

  A. 'DynFlags' records the set of flags that were passed, as an
     'EnumSet' of 'CpuFeature'. Nothing is implied at this stage.

  B. 'initCpuFeatures' turns this set into a 'CpuFeatures' value, applying the
     implications and the baseline of the target architecture. This
     representation makes the implications structural: 'SseAvxVersion' is a
     point in the total order (1), FMA can only be recorded together with AVX,
     and AVX-512 sits above AVX2 and FMA.

Code generation and the driver only consult the second representation, so
whether an instruction set is enabled is decided in one place.
-}

-- | A CPU feature flag, @-m\<feature\>@.
data CpuFeature
  = Feat_SSE
  | Feat_SSE2
  | Feat_SSE3
  | Feat_SSSE3
  | Feat_SSE4
  | Feat_SSE4_2
  | Feat_AVX
  | Feat_AVX2
  | Feat_AVX512F
  | Feat_AVX512BW
  | Feat_AVX512CD
  | Feat_AVX512DQ
  | Feat_AVX512ER
  | Feat_AVX512PF
  | Feat_AVX512VL
  | Feat_FMA
  | Feat_BMI
  | Feat_BMI2
  | Feat_GFNI
  deriving (Eq, Show, Enum, Bounded)

-- | The name of the @-m\<feature\>@ flag for a 'CpuFeature' (without the
-- leading @m@).
--
-- Additionally returns an optional deprecation message.
cpuFeatureFlag :: CpuFeature -> (String, Maybe String)
cpuFeatureFlag = \case
  Feat_SSE      -> ("sse"      , Nothing)
  Feat_SSE2     -> ("sse2"     , Nothing)
  Feat_SSE3     -> ("sse3"     , Nothing)
  Feat_SSSE3    -> ("ssse3"    , Nothing)
  Feat_SSE4     -> ("sse4"     , Nothing)
  Feat_SSE4_2   -> ("sse4.2"   , Nothing)
  Feat_AVX      -> ("avx"      , Nothing)
  Feat_AVX2     -> ("avx2"     , Nothing)
  Feat_AVX512F  -> ("avx512f"  , Nothing)
  Feat_AVX512BW -> ("avx512bw" , Nothing)
  Feat_AVX512CD -> ("avx512cd" , Nothing)
  Feat_AVX512DQ -> ("avx512dq" , Nothing)
  Feat_AVX512ER -> ("avx512er" , Just "AVX-512ER was only available on Xeon Phi")
  Feat_AVX512PF -> ("avx512pf" , Just "AVX-512PF was only available on Xeon Phi")
  Feat_AVX512VL -> ("avx512vl" , Nothing)
  Feat_FMA      -> ("fma"      , Nothing)
  Feat_BMI      -> ("bmi"      , Nothing)
  Feat_BMI2     -> ("bmi2"     , Nothing)
  Feat_GFNI     -> ("gfni"     , Nothing)

-- | The CPU features that are enabled for the target.
--
-- See Note [CPU features].
data CpuFeatures
  -- | x86 and x86_64 CPU features.
  = X86CpuFeatures !X86Features
  -- | A generic set of CPU features.
  --
  -- For now, code generation for non-X86 architectures assumes a fixed
  -- baseline ISA, with the only exception of FMA support.
  --
  -- In the future, we can add architecture-specific constructors, e.g. for
  -- RISC-V (vector support, bit manipulation, etc).
  | GenericCpuFeatures
    { genericFma :: !Bool -- ^ are FMA operations enabled?
    }
  deriving (Eq, Show)

-- | The CPU features enabled by the given flags on the given architecture.
--
-- See Note [CPU features].
initCpuFeatures :: Arch -> EnumSet CpuFeature -> CpuFeatures
initCpuFeatures arch feats = case arch of
  ArchX86     -> X86CpuFeatures (initX86Features feats)
  ArchX86_64  -> X86CpuFeatures (initX86Features feats)
  -- AArch64 always has fused multiply-add instructions.
  ArchAArch64 -> GenericCpuFeatures { genericFma = True }
  _           -> GenericCpuFeatures { genericFma = EnumSet.member Feat_FMA feats }

-- | Whether fused multiply-add instructions are enabled.
cpuFmaEnabled :: CpuFeatures -> Bool
cpuFmaEnabled = \case
  X86CpuFeatures feats                    -> x86FmaEnabled feats
  GenericCpuFeatures { genericFma = fma } -> fma

--------------------------------------------------------------------------------
-- x86

-- | The x86 CPU features that are enabled.
data X86Features = X86Features
  { x86SseAvx :: !SseAvxVersion
  , x86Bmi    :: !(Maybe BmiVersion)
  , x86Gfni   :: !Bool
  }
  deriving (Eq, Show)

-- | Enabled SSE/AVX instruction sets, and whether FMA is enabled.
--
-- See Note [CPU features].
data SseAvxVersion
  -- | SSE, but no AVX (and thus no FMA).
  = Sse !SseVersion
  -- | AVX, and potentially more (AVX2, FMA, etc).
  | Avx !AvxLevel
  deriving (Eq, Show)
-- NB: no 'Ord' instance, as this is only a partial order.
-- See 'sseAvxVersionIncludes'.

-- | Level of enabled AVX instruction sets, and whether FMA is enabled.
data AvxLevel
  -- | AVX (with or without FMA).
  = Avx1 { avxFma :: !Bool }
  -- | AVX2 (with or without FMA).
  | Avx2 { avxFma :: !Bool }
  -- | AVX-512F (which includes AVX2 and FMA), with the given extensions.
  | Avx512 !Avx512Exts
  deriving (Eq, Show)

-- | x86 SSE instruction sets (each including the previous ones).
data SseVersion
  = SSE1
  | SSE2
  | SSE3
  | SSSE3
  | SSE4
  | SSE42
  deriving (Eq, Ord, Show)

-- | The AVX-512 extensions beyond AVX-512F.
data Avx512Exts = Avx512Exts
  { avx512bw :: !Bool -- ^ byte and word operations
  , avx512cd :: !Bool -- ^ conflict detection
  , avx512dq :: !Bool -- ^ doubleword and quadword operations
  , avx512er :: !Bool -- ^ exponential and reciprocal
  , avx512pf :: !Bool -- ^ prefetch
  , avx512vl :: !Bool -- ^ AVX-512 instructions on 128-bit and 256-bit vectors
  }
  deriving (Eq, Show)

noAvx512Exts :: Avx512Exts
noAvx512Exts = Avx512Exts False False False False False False

-- | x86 BMI (bit manipulation) instruction sets (BMI2 including BMI1).
data BmiVersion
  = BMI1
  | BMI2
  deriving (Eq, Ord, Show)


-- | The baseline ISA that we assume for both x86 and x86_64: SSE2.
--
-- Historical remark: prior to GHC 8.10, we used to not require SSE2 on 32-bit
-- x86 architectures. This resulted in defacto nondeterminism for rounding
-- behaviour due to oddities of the x87 stack-based floating-point implementation.
x86BaselineSseAvx :: SseAvxVersion
x86BaselineSseAvx = Sse SSE2

-- | Enable the user-specified features, on top of the X86 baseline feature set
-- ('x86BaselineSseAvx').
--
-- The implication structure of CPU feature flags is detailed in Note [CPU features].
initX86Features
  :: EnumSet CpuFeature -- ^ user-enabled features
  -> X86Features
initX86Features feats = X86Features
  { x86SseAvx = sseAvx
  , x86Bmi    = bmi
  , x86Gfni   = has Feat_GFNI
  }
  where
    has = (`EnumSet.member` feats)
    fma = has Feat_FMA
    avx512 = Avx512Exts
      { avx512bw = has Feat_AVX512BW
      , avx512cd = has Feat_AVX512CD
      , avx512dq = has Feat_AVX512DQ
      , avx512er = has Feat_AVX512ER
      , avx512pf = has Feat_AVX512PF
      , avx512vl = has Feat_AVX512VL
      }
    sseAvx
      | has Feat_AVX512F || avx512 /= noAvx512Exts = Avx (Avx512 avx512)
      | has Feat_AVX2                              = Avx (Avx2 fma)
      | has Feat_AVX || fma                        = Avx (Avx1 fma)
      | has Feat_SSE4_2                            = Sse SSE42
      | has Feat_SSE4                              = Sse SSE4
      | has Feat_SSSE3                             = Sse SSSE3
      | has Feat_SSE3                              = Sse SSE3
      | otherwise                                  = x86BaselineSseAvx
    bmi
      | has Feat_BMI2 = Just BMI2
      | has Feat_BMI  = Just BMI1
      | otherwise     = Nothing

x86SseVersion :: SseAvxVersion -> SseVersion
x86SseVersion = \case
  Sse v -> v
  Avx _ -> SSE42

x86Avx512Exts :: SseAvxVersion -> Maybe Avx512Exts
x86Avx512Exts = \case
  Avx (Avx512 exts) -> Just exts
  _                 -> Nothing

--------------------------------------------------------------------------------
-- Querying x86 features

-- NB: the predicates below are not independent, because there are implications
-- between feature flags (e.g. AVX2 implies SSE4.2).
--
-- See Note [CPU features].

x86Ssse3Enabled :: X86Features -> Bool
x86Ssse3Enabled = (>= SSSE3) . x86SseVersion . x86SseAvx

x86Sse4_1Enabled :: X86Features -> Bool
x86Sse4_1Enabled = (>= SSE4) . x86SseVersion . x86SseAvx

x86Sse4_2Enabled :: X86Features -> Bool
x86Sse4_2Enabled = (>= SSE42) . x86SseVersion . x86SseAvx

x86AvxEnabled :: X86Features -> Bool
x86AvxEnabled feats = case x86SseAvx feats of
  Sse _ -> False
  Avx _ -> True

x86Avx2Enabled :: X86Features -> Bool
x86Avx2Enabled feats = case x86SseAvx feats of
  Sse _          -> False
  Avx (Avx1 _)   -> False
  Avx (Avx2 _)   -> True
  Avx (Avx512 _) -> True

x86FmaEnabled :: X86Features -> Bool
x86FmaEnabled feats = case x86SseAvx feats of
  Sse _            -> False
  Avx (Avx1 fma)   -> fma
  Avx (Avx2 fma)   -> fma
  Avx (Avx512 _)   -> True

x86Avx512FEnabled :: X86Features -> Bool
x86Avx512FEnabled = isJust . x86Avx512Exts . x86SseAvx

x86Avx512DQEnabled :: X86Features -> Bool
x86Avx512DQEnabled = maybe False avx512dq . x86Avx512Exts . x86SseAvx

x86Avx512VLEnabled :: X86Features -> Bool
x86Avx512VLEnabled = maybe False avx512vl . x86Avx512Exts . x86SseAvx

x86Bmi2Enabled :: X86Features -> Bool
x86Bmi2Enabled = (>= Just BMI2) . x86Bmi

--------------------------------------------------------------------------------
-- Comparing x86 feature sets

-- | @enabled \`sseAvxVersionIncludes\` required@ answers whether the @enabled@
-- features provide everything required by @required@, taking into account
-- implications between instruction sets. See Note [CPU features].
--
-- This is a partial order, as FMA and the AVX-512 extensions are independent
-- of each other.
sseAvxVersionIncludes :: SseAvxVersion -> SseAvxVersion -> Bool
sseAvxVersionIncludes enabled required = case (enabled, required) of
  (Sse e, Sse r) -> e >= r
  (Avx _, Sse _) -> True
  (Sse _, Avx _) -> False
  (Avx e, Avx r) -> e `avxLevelIncludes` r
  where
    avxLevelIncludes :: AvxLevel -> AvxLevel -> Bool
    avxLevelIncludes e r = case (e, r) of
      (Avx512 ee, Avx512 re) -> ee `avx512ExtsInclude` re
      (Avx512 _, _)          -> True
      (_, Avx512 _)          -> False
      (Avx1 ef, Avx1 rf)     -> rf `implies` ef
      (Avx2 ef, Avx1 rf)     -> rf `implies` ef
      (Avx2 ef, Avx2 rf)     -> rf `implies` ef
      (Avx1 _, Avx2 _)       -> False

    avx512ExtsInclude :: Avx512Exts -> Avx512Exts -> Bool
    avx512ExtsInclude
      (Avx512Exts ebw ecd edq eer epf evl)
      (Avx512Exts rbw rcd rdq rer rpf rvl)
      =  rbw `implies` ebw
      && rcd `implies` ecd
      && rdq `implies` edq
      && rer `implies` eer
      && rpf `implies` epf
      && rvl `implies` evl

    implies :: Bool -> Bool -> Bool
    implies a b = b || not a

-- | The least 'SseAvxVersion' providing everything that both arguments do.
unionSseAvxVersion :: SseAvxVersion -> SseAvxVersion -> SseAvxVersion
unionSseAvxVersion v1 v2 = case (v1, v2) of
  (Sse a, Sse b) -> Sse (max a b)
  (Sse _, Avx _) -> v2
  (Avx _, Sse _) -> v1
  (Avx a, Avx b) -> Avx (a `avxLevelJoin` b)
  where
    avxLevelJoin :: AvxLevel -> AvxLevel -> AvxLevel
    avxLevelJoin a b = case (a, b) of
      (Avx512 ea, Avx512 eb) -> Avx512 (ea `avx512ExtsUnion` eb)
      (Avx512 _, _)          -> a
      (_, Avx512 _)          -> b
      (Avx1 fa, Avx1 fb)     -> Avx1 (fa || fb)
      (Avx1 fa, Avx2 fb)     -> Avx2 (fa || fb)
      (Avx2 fa, Avx1 fb)     -> Avx2 (fa || fb)
      (Avx2 fa, Avx2 fb)     -> Avx2 (fa || fb)
    avx512ExtsUnion (Avx512Exts bw1 cd1 dq1 er1 pf1 vl1) (Avx512Exts bw2 cd2 dq2 er2 pf2 vl2)
      = Avx512Exts (bw1 || bw2) (cd1 || cd2) (dq1 || dq2) (er1 || er2) (pf1 || pf2) (vl1 || vl2)

-- | The SSE/AVX instruction sets required to operate on vectors of the given
-- width in bits, with floating-point or integer elements.
--
-- 128-bit vectors need SSE2 (SSE1 only covers single-precision elements).
sseAvxVectorRequirement
  :: Bool -- ^ floating-point elements?
  -> Int  -- ^ width of the vector, in bits
  -> SseAvxVersion
sseAvxVectorRequirement float bits
  | bits <= 128 = Sse SSE2
  | bits <= 256 = Avx $ if float then Avx1 False else Avx2 False
  | otherwise   = Avx $ Avx512 noAvx512Exts

-- | A minimal set of flags that imply the given 'SseAvxVersion'.
sseAvxVersionFlags :: SseAvxVersion -> [String]
sseAvxVersionFlags = \case
  Sse v ->
    [ case v of
        SSE1  -> "-msse"
        SSE2  -> "-msse2"
        SSE3  -> "-msse3"
        SSSE3 -> "-mssse3"
        SSE4  -> "-msse4"
        SSE42 -> "-msse4.2"
    ]
  Avx (Avx1 fma)
    | fma       -> [ "-mfma" ] -- FMA implies AVX
    | otherwise -> [ "-mavx" ]
  Avx (Avx2 fma) -> "-mavx2" : [ "-mfma" | fma ]
  Avx (Avx512 (Avx512Exts bw cd dq er pf vl))
    -- Every AVX-512 extension implies AVX-512F
    | null exts -> [ "-mavx512f" ]
    | otherwise -> exts
    where
      exts =
        [ flag
        | (flag, enabled) <-
            [ ("-mavx512bw", bw)
            , ("-mavx512cd", cd)
            , ("-mavx512dq", dq)
            , ("-mavx512er", er)
            , ("-mavx512pf", pf)
            , ("-mavx512vl", vl)
            ]
        , enabled ]
