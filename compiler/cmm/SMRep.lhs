%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Storage manager representation of closures

\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMRep (
        -- * Words and bytes
        StgWord, fromStgWord, toStgWord,
        StgHalfWord, fromStgHalfWord, toStgHalfWord,
        hALF_WORD_SIZE, hALF_WORD_SIZE_IN_BITS,
        WordOff, ByteOff,
        roundUpToWords,

        -- * Closure repesentation
        SMRep(..), -- CmmInfo sees the rep; no one else does
        IsStatic,
        ClosureTypeInfo(..), ArgDescr(..), Liveness,
        ConstrDescription,

        -- ** Construction
        mkHeapRep, blackHoleRep, indStaticRep, mkStackRep, mkRTSRep,

        -- ** Predicates
        isStaticRep, isConRep, isThunkRep, isFunRep, isStaticNoCafCon,
        isStackRep,

        -- ** Size-related things
        heapClosureSize,
        fixedHdrSize, arrWordsHdrSize, arrPtrsHdrSize,
        profHdrSize, thunkHdrSize, nonHdrSize,

        -- ** RTS closure types
        rtsClosureType, rET_SMALL, rET_BIG,
        aRG_GEN, aRG_GEN_BIG,

        -- * Operations over [Word8] strings that don't belong here
        pprWord8String, stringToWord8s
    ) where

#include "../HsVersions.h"
#include "../includes/MachDeps.h"

import DynFlags
import Outputable
import Platform
import FastString

import Data.Array.Base
import Data.Char( ord )
import Data.Word
import Data.Bits
\end{code}


%************************************************************************
%*                                                                      *
                Words and bytes
%*                                                                      *
%************************************************************************

\begin{code}
type WordOff = Int -- Word offset, or word count
type ByteOff = Int -- Byte offset, or byte count

roundUpToWords :: DynFlags -> ByteOff -> ByteOff
roundUpToWords dflags n = (n + (wORD_SIZE dflags - 1)) .&. (complement (wORD_SIZE dflags - 1))
\end{code}

StgWord is a type representing an StgWord on the target platform.

\begin{code}
-- A Word64 is large enough to hold a Word for either a 32bit or 64bit platform
newtype StgWord = StgWord Word64
    deriving (Eq,
#if __GLASGOW_HASKELL__ < 706
              Num,
#endif
              Bits, IArray UArray)

fromStgWord :: StgWord -> Integer
fromStgWord (StgWord i) = toInteger i

toStgWord :: DynFlags -> Integer -> StgWord
toStgWord dflags i
    = case platformWordSize (targetPlatform dflags) of
      -- These conversions mean that things like toStgWord (-1)
      -- do the right thing
      4 -> StgWord (fromIntegral (fromInteger i :: Word32))
      8 -> StgWord (fromInteger i :: Word64)
      w -> panic ("toStgWord: Unknown platformWordSize: " ++ show w)

instance Outputable StgWord where
    ppr (StgWord i) = integer (toInteger i)

--

-- A Word32 is large enough to hold half a Word for either a 32bit or
-- 64bit platform
newtype StgHalfWord = StgHalfWord Word32
    deriving Eq

fromStgHalfWord :: StgHalfWord -> Integer
fromStgHalfWord (StgHalfWord w) = toInteger w

toStgHalfWord :: DynFlags -> Integer -> StgHalfWord
toStgHalfWord dflags i
    = case platformWordSize (targetPlatform dflags) of
      -- These conversions mean that things like toStgHalfWord (-1)
      -- do the right thing
      4 -> StgHalfWord (fromIntegral (fromInteger i :: Word16))
      8 -> StgHalfWord (fromInteger i :: Word32)
      w -> panic ("toStgHalfWord: Unknown platformWordSize: " ++ show w)

instance Outputable StgHalfWord where
    ppr (StgHalfWord w) = integer (toInteger w)

hALF_WORD_SIZE :: DynFlags -> ByteOff
hALF_WORD_SIZE dflags = platformWordSize (targetPlatform dflags) `shiftR` 1
hALF_WORD_SIZE_IN_BITS :: DynFlags -> Int
hALF_WORD_SIZE_IN_BITS dflags = platformWordSize (targetPlatform dflags) `shiftL` 2
\end{code}


%************************************************************************
%*                                                                      *
\subsubsection[SMRep-datatype]{@SMRep@---storage manager representation}
%*                                                                      *
%************************************************************************

\begin{code}
-- | A description of the layout of a closure.  Corresponds directly
-- to the closure types in includes/rts/storage/ClosureTypes.h.
data SMRep
  = HeapRep              -- GC routines consult sizes in info tbl
        IsStatic
        !WordOff         --  # ptr words
        !WordOff         --  # non-ptr words INCLUDING SLOP (see mkHeapRep below)
        ClosureTypeInfo  -- type-specific info

  | StackRep            -- Stack frame (RET_SMALL or RET_BIG)
        Liveness

  | RTSRep              -- The RTS needs to declare info tables with specific
        Int             -- type tags, so this form lets us override the default
        SMRep           -- tag for an SMRep.

-- | True <=> This is a static closure.  Affects how we garbage-collect it.
-- Static closure have an extra static link field at the end.
type IsStatic = Bool

-- From an SMRep you can get to the closure type defined in
-- includes/rts/storage/ClosureTypes.h. Described by the function
-- rtsClosureType below.

data ClosureTypeInfo
  = Constr        ConstrTag ConstrDescription
  | Fun           FunArity ArgDescr
  | Thunk
  | ThunkSelector SelectorOffset
  | BlackHole
  | IndStatic

type ConstrTag         = Int
type ConstrDescription = [Word8] -- result of dataConIdentity
type FunArity          = Int
type SelectorOffset    = Int

-------------------------
-- We represent liveness bitmaps as a Bitmap (whose internal
-- representation really is a bitmap).  These are pinned onto case return
-- vectors to indicate the state of the stack for the garbage collector.
--
-- In the compiled program, liveness bitmaps that fit inside a single
-- word (StgWord) are stored as a single word, while larger bitmaps are
-- stored as a pointer to an array of words.

type Liveness = [Bool]   -- One Bool per word; True  <=> non-ptr or dead
                         --                    False <=> ptr

-------------------------
-- An ArgDescr describes the argument pattern of a function

data ArgDescr
  = ArgSpec             -- Fits one of the standard patterns
        !Int            -- RTS type identifier ARG_P, ARG_N, ...

  | ArgGen              -- General case
        Liveness        -- Details about the arguments


-----------------------------------------------------------------------------
-- Construction

mkHeapRep :: DynFlags -> IsStatic -> WordOff -> WordOff -> ClosureTypeInfo
          -> SMRep
mkHeapRep dflags is_static ptr_wds nonptr_wds cl_type_info
  = HeapRep is_static
            ptr_wds
            (nonptr_wds + slop_wds)
            cl_type_info
  where
     slop_wds
      | is_static = 0
      | otherwise = max 0 (minClosureSize dflags - (hdr_size + payload_size))

     hdr_size     = closureTypeHdrSize dflags cl_type_info
     payload_size = ptr_wds + nonptr_wds

mkRTSRep :: Int -> SMRep -> SMRep
mkRTSRep = RTSRep

mkStackRep :: [Bool] -> SMRep
mkStackRep liveness = StackRep liveness

blackHoleRep :: SMRep
blackHoleRep = HeapRep False 0 0 BlackHole

indStaticRep :: SMRep
indStaticRep = HeapRep True 1 0 IndStatic

-----------------------------------------------------------------------------
-- Predicates

isStaticRep :: SMRep -> IsStatic
isStaticRep (HeapRep is_static _ _ _) = is_static
isStaticRep (StackRep {})             = False
isStaticRep (RTSRep _ rep)            = isStaticRep rep

isStackRep :: SMRep -> Bool
isStackRep StackRep{}     = True
isStackRep (RTSRep _ rep) = isStackRep rep
isStackRep _              = False

isConRep :: SMRep -> Bool
isConRep (HeapRep _ _ _ Constr{}) = True
isConRep _                        = False

isThunkRep :: SMRep -> Bool
isThunkRep (HeapRep _ _ _ Thunk{})         = True
isThunkRep (HeapRep _ _ _ ThunkSelector{}) = True
isThunkRep (HeapRep _ _ _ BlackHole{})     = True
isThunkRep (HeapRep _ _ _ IndStatic{})     = True
isThunkRep _                               = False

isFunRep :: SMRep -> Bool
isFunRep (HeapRep _ _ _ Fun{}) = True
isFunRep _                     = False

isStaticNoCafCon :: SMRep -> Bool
-- This should line up exactly with CONSTR_NOCAF_STATIC above
-- See Note [Static NoCaf constructors]
isStaticNoCafCon (HeapRep True 0 _ Constr{}) = True
isStaticNoCafCon _                           = False


-----------------------------------------------------------------------------
-- Size-related things

-- | Size of a closure header (StgHeader in includes/rts/storage/Closures.h)
fixedHdrSize :: DynFlags -> WordOff
fixedHdrSize dflags = sTD_HDR_SIZE dflags + profHdrSize dflags

-- | Size of the profiling part of a closure header
-- (StgProfHeader in includes/rts/storage/Closures.h)
profHdrSize  :: DynFlags -> WordOff
profHdrSize dflags
 | gopt Opt_SccProfilingOn dflags = pROF_HDR_SIZE dflags
 | otherwise                      = 0

-- | The garbage collector requires that every closure is at least as
--   big as this.
minClosureSize :: DynFlags -> WordOff
minClosureSize dflags = fixedHdrSize dflags + mIN_PAYLOAD_SIZE dflags

arrWordsHdrSize :: DynFlags -> ByteOff
arrWordsHdrSize dflags
 = fixedHdrSize dflags * wORD_SIZE dflags + sIZEOF_StgArrWords_NoHdr dflags

arrPtrsHdrSize :: DynFlags -> ByteOff
arrPtrsHdrSize dflags
 = fixedHdrSize dflags * wORD_SIZE dflags + sIZEOF_StgMutArrPtrs_NoHdr dflags

-- Thunks have an extra header word on SMP, so the update doesn't
-- splat the payload.
thunkHdrSize :: DynFlags -> WordOff
thunkHdrSize dflags = fixedHdrSize dflags + smp_hdr
        where smp_hdr = sIZEOF_StgSMPThunkHeader dflags `quot` wORD_SIZE dflags


nonHdrSize :: SMRep -> WordOff
nonHdrSize (HeapRep _ p np _) = p + np
nonHdrSize (StackRep bs)      = length bs
nonHdrSize (RTSRep _ rep)     = nonHdrSize rep

heapClosureSize :: DynFlags -> SMRep -> WordOff
heapClosureSize dflags (HeapRep _ p np ty)
 = closureTypeHdrSize dflags ty + p + np
heapClosureSize _ _ = panic "SMRep.heapClosureSize"

closureTypeHdrSize :: DynFlags -> ClosureTypeInfo -> WordOff
closureTypeHdrSize dflags ty = case ty of
                  Thunk{}         -> thunkHdrSize dflags
                  ThunkSelector{} -> thunkHdrSize dflags
                  BlackHole{}     -> thunkHdrSize dflags
                  IndStatic{}     -> thunkHdrSize dflags
                  _               -> fixedHdrSize dflags
        -- All thunks use thunkHdrSize, even if they are non-updatable.
        -- this is because we don't have separate closure types for
        -- updatable vs. non-updatable thunks, so the GC can't tell the
        -- difference.  If we ever have significant numbers of non-
        -- updatable thunks, it might be worth fixing this.

-----------------------------------------------------------------------------
-- deriving the RTS closure type from an SMRep

#include "../includes/rts/storage/ClosureTypes.h"
#include "../includes/rts/storage/FunTypes.h"
-- Defines CONSTR, CONSTR_1_0 etc

-- | Derives the RTS closure type from an 'SMRep'
rtsClosureType :: SMRep -> Int
rtsClosureType rep
    = case rep of
      RTSRep ty _ -> ty

      HeapRep False 1 0 Constr{} -> CONSTR_1_0
      HeapRep False 0 1 Constr{} -> CONSTR_0_1
      HeapRep False 2 0 Constr{} -> CONSTR_2_0
      HeapRep False 1 1 Constr{} -> CONSTR_1_1
      HeapRep False 0 2 Constr{} -> CONSTR_0_2
      HeapRep False _ _ Constr{} -> CONSTR

      HeapRep False 1 0 Fun{} -> FUN_1_0
      HeapRep False 0 1 Fun{} -> FUN_0_1
      HeapRep False 2 0 Fun{} -> FUN_2_0
      HeapRep False 1 1 Fun{} -> FUN_1_1
      HeapRep False 0 2 Fun{} -> FUN_0_2
      HeapRep False _ _ Fun{} -> FUN

      HeapRep False 1 0 Thunk{} -> THUNK_1_0
      HeapRep False 0 1 Thunk{} -> THUNK_0_1
      HeapRep False 2 0 Thunk{} -> THUNK_2_0
      HeapRep False 1 1 Thunk{} -> THUNK_1_1
      HeapRep False 0 2 Thunk{} -> THUNK_0_2
      HeapRep False _ _ Thunk{} -> THUNK

      HeapRep False _ _ ThunkSelector{} ->  THUNK_SELECTOR

      -- Approximation: we use the CONSTR_NOCAF_STATIC type for static
      -- constructors -- that have no pointer words only.
      HeapRep True 0 _ Constr{} -> CONSTR_NOCAF_STATIC  -- See isStaticNoCafCon below
      HeapRep True _ _ Constr{} -> CONSTR_STATIC
      HeapRep True _ _ Fun{}    -> FUN_STATIC
      HeapRep True _ _ Thunk{}  -> THUNK_STATIC

      HeapRep False _ _ BlackHole{} -> BLACKHOLE

      HeapRep False _ _ IndStatic{} -> IND_STATIC

      _ -> panic "rtsClosureType"

-- We export these ones
rET_SMALL, rET_BIG, aRG_GEN, aRG_GEN_BIG :: Int
rET_SMALL   = RET_SMALL
rET_BIG     = RET_BIG
aRG_GEN     = ARG_GEN
aRG_GEN_BIG = ARG_GEN_BIG
\end{code}

Note [Static NoCaf constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we know that a top-level binding 'x' is not Caffy (ie no CAFs are
reachable from 'x'), then a statically allocated constructor (Just x)
is also not Caffy, and the garbage collector need not follow its
argument fields.  Exploiting this would require two static info tables
for Just, for the two cases where the argument was Caffy or non-Caffy.

Currently we don't do this; instead we treat nullary constructors
as non-Caffy, and the others as potentially Caffy.


%************************************************************************
%*                                                                      *
             Pretty printing of SMRep and friends
%*                                                                      *
%************************************************************************

\begin{code}
instance Outputable ClosureTypeInfo where
   ppr = pprTypeInfo

instance Outputable SMRep where
   ppr (HeapRep static ps nps tyinfo)
     = hang (header <+> lbrace) 2 (ppr tyinfo <+> rbrace)
     where
       header = ptext (sLit "HeapRep")
                <+> if static then ptext (sLit "static") else empty
                <+> pp_n "ptrs" ps <+> pp_n "nonptrs" nps
       pp_n :: String -> Int -> SDoc
       pp_n _ 0 = empty
       pp_n s n = int n <+> text s

   ppr (StackRep bs) = ptext (sLit "StackRep") <+> ppr bs

   ppr (RTSRep ty rep) = ptext (sLit "tag:") <> ppr ty <+> ppr rep

instance Outputable ArgDescr where
  ppr (ArgSpec n) = ptext (sLit "ArgSpec") <+> ppr n
  ppr (ArgGen ls) = ptext (sLit "ArgGen") <+> ppr ls

pprTypeInfo :: ClosureTypeInfo -> SDoc
pprTypeInfo (Constr tag descr)
  = ptext (sLit "Con") <+>
    braces (sep [ ptext (sLit "tag:") <+> ppr tag
                , ptext (sLit "descr:") <> text (show descr) ])

pprTypeInfo (Fun arity args)
  = ptext (sLit "Fun") <+>
    braces (sep [ ptext (sLit "arity:") <+> ppr arity
                , ptext (sLit ("fun_type:")) <+> ppr args ])

pprTypeInfo (ThunkSelector offset)
  = ptext (sLit "ThunkSel") <+> ppr offset

pprTypeInfo Thunk     = ptext (sLit "Thunk")
pprTypeInfo BlackHole = ptext (sLit "BlackHole")
pprTypeInfo IndStatic = ptext (sLit "IndStatic")

-- XXX Does not belong here!!
stringToWord8s :: String -> [Word8]
stringToWord8s s = map (fromIntegral . ord) s

pprWord8String :: [Word8] -> SDoc
-- Debug printing.  Not very clever right now.
pprWord8String ws = text (show ws)
\end{code}
