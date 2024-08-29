-----------------------------------------------------------------------------
--
-- Argument representations used in GHC.StgToCmm.Layout.
--
-- (c) The University of Glasgow 2013
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module GHC.StgToCmm.ArgRep (
        ArgRep(..), toArgRep, toArgRepOrV, argRepSizeW,

        argRepString, isNonV, idArgRep,

        slowCallPattern,

        ) where

import GHC.Prelude
import GHC.Platform

import GHC.StgToCmm.Closure    ( idPrimRep1 )
import GHC.Runtime.Heap.Layout ( WordOff )
import GHC.Types.Id            ( Id )
import GHC.Core.TyCon          ( PrimRep(..), PrimOrVoidRep(..), primElemRepSizeB )
import GHC.Types.Basic         ( RepArity )
import GHC.Settings.Constants  ( wORD64_SIZE, dOUBLE_SIZE )

import GHC.Utils.Outputable
import GHC.Data.FastString

-- I extricated this code as this new module in order to avoid a
-- cyclic dependency between GHC.StgToCmm.Layout and GHC.StgToCmm.Ticky.
--
-- NSF 18 Feb 2013

-------------------------------------------------------------------------
--      Classifying arguments: ArgRep
-------------------------------------------------------------------------

-- ArgRep is re-exported by GHC.StgToCmm.Layout, but only for use in the
-- byte-code generator which also needs to know about the
-- classification of arguments.

data ArgRep = P   -- GC Ptr
            | N   -- Word-sized non-ptr
            | L   -- 64-bit non-ptr (long)
            | V   -- Void
            | F   -- Float
            | D   -- Double
            | V16 -- 16-byte (128-bit) vectors of Float/Double/Int8/Word32/etc.
            | V32 -- 32-byte (256-bit) vectors of Float/Double/Int8/Word32/etc.
            | V64 -- 64-byte (512-bit) vectors of Float/Double/Int8/Word32/etc.
            deriving ( Eq, Ord )
instance Outputable ArgRep where ppr = text . argRepString

argRepString :: ArgRep -> String
argRepString P = "P"
argRepString N = "N"
argRepString L = "L"
argRepString V = "V"
argRepString F = "F"
argRepString D = "D"
argRepString V16 = "V16"
argRepString V32 = "V32"
argRepString V64 = "V64"

toArgRep :: Platform -> PrimRep -> ArgRep
toArgRep platform rep = case rep of
   BoxedRep _        -> P
   IntRep            -> N
   WordRep           -> N
   Int8Rep           -> N  -- Gets widened to native word width for calls
   Word8Rep          -> N  -- Gets widened to native word width for calls
   Int16Rep          -> N  -- Gets widened to native word width for calls
   Word16Rep         -> N  -- Gets widened to native word width for calls
   Int32Rep          -> N  -- Gets widened to native word width for calls
   Word32Rep         -> N  -- Gets widened to native word width for calls
   AddrRep           -> N
   Int64Rep          -> case platformWordSize platform of
                           PW4 -> L
                           PW8 -> N
   Word64Rep         -> case platformWordSize platform of
                           PW4 -> L
                           PW8 -> N
   FloatRep          -> F
   DoubleRep         -> D
   (VecRep len elem) -> case len*primElemRepSizeB platform elem of
                           16 -> V16
                           32 -> V32
                           64 -> V64
                           _  -> error "toArgRep: bad vector primrep"

toArgRepOrV :: Platform -> PrimOrVoidRep -> ArgRep
toArgRepOrV _ VoidRep = V
toArgRepOrV platform (NVRep rep) = toArgRep platform rep

isNonV :: ArgRep -> Bool
isNonV V = False
isNonV _ = True

argRepSizeW :: Platform -> ArgRep -> WordOff -- Size in words
argRepSizeW platform = \case
   N   -> 1
   P   -> 1
   F   -> 1
   L   -> wORD64_SIZE `quot` ws
   D   -> dOUBLE_SIZE `quot` ws
   V   -> 0
   V16 -> 16          `quot` ws
   V32 -> 32          `quot` ws
   V64 -> 64          `quot` ws
  where
   ws       = platformWordSizeInBytes platform

idArgRep :: Platform -> Id -> ArgRep
idArgRep platform = toArgRepOrV platform . idPrimRep1

-- This list of argument patterns should be kept in sync with at least
-- the following:
--
--  * GHC.StgToCmm.Layout.stdPattern maybe to some degree?
--
--  * the RTS_RET(stg_ap_*) and RTS_FUN_DECL(stg_ap_*_fast)
--  declarations in rts/include/stg/MiscClosures.h
--
--  * the SLOW_CALL_*_ctr declarations in rts/include/stg/Ticky.h,
--
--  * the TICK_SLOW_CALL_*() #defines in rts/include/Cmm.h,
--
--  * the PR_CTR(SLOW_CALL_*_ctr) calls in rts/Ticky.c,
--
--  * and the SymI_HasProto(stg_ap_*_{ret,info,fast}) calls and
--  SymI_HasProto(SLOW_CALL_*_ctr) calls in rts/Linker.c
--
-- There may be more places that I haven't found; I merely igrep'd for
-- pppppp and excluded things that seemed ghci-specific.
--
-- Also, it seems at the moment that ticky counters with void
-- arguments will never be bumped, but I'm still declaring those
-- counters, defensively.
--
-- NSF 6 Mar 2013

slowCallPattern :: [ArgRep] -> (FastString, RepArity)
-- Returns the generic apply function and arity
--
-- The first batch of cases match (some) specialised entries
-- The last group deals exhaustively with the cases for the first argument
--   (and the zero-argument case)
--
-- In 99% of cases this function will match *all* the arguments in one batch

slowCallPattern (P: P: P: P: P: P: _) = (fsLit "stg_ap_pppppp", 6)
slowCallPattern (P: P: P: P: P: _)    = (fsLit "stg_ap_ppppp", 5)
slowCallPattern (P: P: P: P: _)       = (fsLit "stg_ap_pppp", 4)
slowCallPattern (P: P: P: V: _)       = (fsLit "stg_ap_pppv", 4)
slowCallPattern (P: P: P: _)          = (fsLit "stg_ap_ppp", 3)
slowCallPattern (P: P: V: _)          = (fsLit "stg_ap_ppv", 3)
slowCallPattern (P: P: _)             = (fsLit "stg_ap_pp", 2)
slowCallPattern (P: V: _)             = (fsLit "stg_ap_pv", 2)
slowCallPattern (P: _)                = (fsLit "stg_ap_p", 1)
slowCallPattern (V: _)                = (fsLit "stg_ap_v", 1)
slowCallPattern (N: _)                = (fsLit "stg_ap_n", 1)
slowCallPattern (F: _)                = (fsLit "stg_ap_f", 1)
slowCallPattern (D: _)                = (fsLit "stg_ap_d", 1)
slowCallPattern (L: _)                = (fsLit "stg_ap_l", 1)
slowCallPattern (V16: _)              = (fsLit "stg_ap_v16", 1)
slowCallPattern (V32: _)              = (fsLit "stg_ap_v32", 1)
slowCallPattern (V64: _)              = (fsLit "stg_ap_v64", 1)
slowCallPattern []                    = (fsLit "stg_ap_0", 0)
