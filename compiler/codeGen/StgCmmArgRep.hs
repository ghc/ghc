-----------------------------------------------------------------------------
--
-- Argument representations used in StgCmmLayout.
--
-- (c) The University of Glasgow 2013
--
-----------------------------------------------------------------------------

module StgCmmArgRep (
        ArgRep(..), toArgRep, argRepSizeW,

        argRepString, isNonV, idArgRep,

        slowCallPattern,

        ) where

import StgCmmClosure    ( idPrimRep )

import SMRep            ( WordOff )
import Id               ( Id )
import TyCon            ( PrimRep(..), primElemRepSizeB )
import BasicTypes       ( RepArity )
import Constants        ( wORD64_SIZE )
import DynFlags

import Outputable
import FastString

-- I extricated this code as this new module in order to avoid a
-- cyclic dependency between StgCmmLayout and StgCmmTicky.
--
-- NSF 18 Feb 2013

-------------------------------------------------------------------------
--      Classifying arguments: ArgRep
-------------------------------------------------------------------------

-- ArgRep is re-exported by StgCmmLayout, but only for use in the
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

toArgRep :: PrimRep -> ArgRep
toArgRep VoidRep           = V
toArgRep PtrRep            = P
toArgRep IntRep            = N
toArgRep WordRep           = N
toArgRep AddrRep           = N
toArgRep Int64Rep          = L
toArgRep Word64Rep         = L
toArgRep FloatRep          = F
toArgRep DoubleRep         = D
toArgRep (VecRep len elem) = case len*primElemRepSizeB elem of
                               16 -> V16
                               32 -> V32
                               64 -> V64
                               _  -> error "toArgRep: bad vector primrep"

isNonV :: ArgRep -> Bool
isNonV V = False
isNonV _ = True

argRepSizeW :: DynFlags -> ArgRep -> WordOff                -- Size in words
argRepSizeW _      N   = 1
argRepSizeW _      P   = 1
argRepSizeW _      F   = 1
argRepSizeW dflags L   = wORD64_SIZE        `quot` wORD_SIZE dflags
argRepSizeW dflags D   = dOUBLE_SIZE dflags `quot` wORD_SIZE dflags
argRepSizeW _      V   = 0
argRepSizeW dflags V16 = 16                 `quot` wORD_SIZE dflags
argRepSizeW dflags V32 = 32                 `quot` wORD_SIZE dflags
argRepSizeW dflags V64 = 64                 `quot` wORD_SIZE dflags

idArgRep :: Id -> ArgRep
idArgRep = toArgRep . idPrimRep

-- This list of argument patterns should be kept in sync with at least
-- the following:
--
--  * StgCmmLayout.stdPattern maybe to some degree?
--
--  * the RTS_RET(stg_ap_*) and RTS_FUN_DECL(stg_ap_*_fast)
--  declarations in includes/stg/MiscClosures.h
--
--  * the SLOW_CALL_*_ctr declarations in includes/stg/Ticky.h,
--
--  * the TICK_SLOW_CALL_*() #defines in includes/Cmm.h,
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

-- These cases were found to cover about 99% of all slow calls:
slowCallPattern :: [ArgRep] -> (FastString, RepArity)
-- Returns the generic apply function and arity
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
