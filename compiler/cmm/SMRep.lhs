%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Storage manager representation of closures

This is here, rather than in ClosureInfo, just to keep nhc happy.
Other modules should access this info through ClosureInfo.

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module SMRep (
        -- * Words and bytes
	StgWord, StgHalfWord, 
	hALF_WORD_SIZE, hALF_WORD_SIZE_IN_BITS,
	WordOff, ByteOff,
        roundUpToWords,

        -- * Closure repesentation
        SMRep(..),	-- CmmInfo sees the rep; no one else does
        IsStatic, 
        ClosureTypeInfo(..), ArgDescr(..), Liveness,
        ConstrDescription, 

        -- ** Construction
        mkHeapRep, blackHoleRep, mkStackRep, mkRTSRep,

        -- ** Predicates
        isStaticRep, isConRep, isThunkRep, isFunRep, isStaticNoCafCon,

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

import StaticFlags
import Constants
import Outputable
import FastString

import Data.Char( ord )
import Data.Word
import Data.Bits
\end{code}


%************************************************************************
%*									*
		Words and bytes
%*									*
%************************************************************************

\begin{code}
type WordOff = Int	-- Word offset, or word count
type ByteOff = Int	-- Byte offset, or byte count

roundUpToWords :: ByteOff -> ByteOff
roundUpToWords n = (n + (wORD_SIZE - 1)) .&. (complement (wORD_SIZE - 1))
\end{code}

StgWord is a type representing an StgWord on the target platform.

\begin{code}
#if SIZEOF_HSWORD == 4
type StgWord     = Word32
type StgHalfWord = Word16
hALF_WORD_SIZE :: ByteOff
hALF_WORD_SIZE = 2
hALF_WORD_SIZE_IN_BITS :: Int
hALF_WORD_SIZE_IN_BITS = 16
#elif SIZEOF_HSWORD == 8
type StgWord     = Word64
type StgHalfWord = Word32
hALF_WORD_SIZE :: ByteOff
hALF_WORD_SIZE = 4
hALF_WORD_SIZE_IN_BITS :: Int
hALF_WORD_SIZE_IN_BITS = 32
#else
#error unknown SIZEOF_HSWORD
#endif
\end{code}


%************************************************************************
%*									*
\subsubsection[SMRep-datatype]{@SMRep@---storage manager representation}
%*									*
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
        StgHalfWord     -- type tags, so this form lets us override the default
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

type ConstrTag         = StgHalfWord
type ConstrDescription = [Word8] -- result of dataConIdentity
type FunArity          = StgHalfWord
type SelectorOffset    = StgWord

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
  = ArgSpec		-- Fits one of the standard patterns
	!StgHalfWord	-- RTS type identifier ARG_P, ARG_N, ...

  | ArgGen	 	-- General case
	Liveness	-- Details about the arguments


-----------------------------------------------------------------------------
-- Construction

mkHeapRep :: IsStatic -> WordOff -> WordOff -> ClosureTypeInfo -> SMRep
mkHeapRep is_static ptr_wds nonptr_wds cl_type_info
  = HeapRep is_static
            ptr_wds
            (nonptr_wds + slop_wds)
            cl_type_info
  where
     slop_wds
      | is_static = 0
      | otherwise = max 0 (minClosureSize - (hdr_size + payload_size))

     hdr_size     = closureTypeHdrSize cl_type_info
     payload_size = ptr_wds + nonptr_wds

mkRTSRep :: StgHalfWord -> SMRep -> SMRep
mkRTSRep = RTSRep

mkStackRep :: [Bool] -> SMRep
mkStackRep liveness = StackRep liveness

blackHoleRep :: SMRep
blackHoleRep = HeapRep False 0 0 BlackHole

-----------------------------------------------------------------------------
-- Predicates

isStaticRep :: SMRep -> IsStatic
isStaticRep (HeapRep is_static _ _ _) = is_static
isStaticRep (StackRep {})             = False
isStaticRep (RTSRep _ rep)            = isStaticRep rep

isConRep :: SMRep -> Bool
isConRep (HeapRep _ _ _ Constr{}) = True
isConRep _                        = False

isThunkRep :: SMRep -> Bool
isThunkRep (HeapRep _ _ _ Thunk{})         = True
isThunkRep (HeapRep _ _ _ ThunkSelector{}) = True
isThunkRep (HeapRep _ _ _ BlackHole{})     = True
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
fixedHdrSize :: WordOff
fixedHdrSize = sTD_HDR_SIZE + profHdrSize

-- | Size of the profiling part of a closure header
-- (StgProfHeader in includes/rts/storage/Closures.h)
profHdrSize  :: WordOff
profHdrSize  | opt_SccProfilingOn   = pROF_HDR_SIZE
	     | otherwise	    = 0

-- | The garbage collector requires that every closure is at least as big as this.
minClosureSize :: WordOff
minClosureSize = fixedHdrSize + mIN_PAYLOAD_SIZE

arrWordsHdrSize   :: ByteOff
arrWordsHdrSize   = fixedHdrSize*wORD_SIZE + sIZEOF_StgArrWords_NoHdr

arrPtrsHdrSize    :: ByteOff
arrPtrsHdrSize    = fixedHdrSize*wORD_SIZE + sIZEOF_StgMutArrPtrs_NoHdr

-- Thunks have an extra header word on SMP, so the update doesn't 
-- splat the payload.
thunkHdrSize :: WordOff
thunkHdrSize = fixedHdrSize + smp_hdr
	where smp_hdr = sIZEOF_StgSMPThunkHeader `quot` wORD_SIZE


nonHdrSize :: SMRep -> WordOff
nonHdrSize (HeapRep _ p np _) = p + np
nonHdrSize (StackRep bs)      = length bs
nonHdrSize (RTSRep _ rep)     = nonHdrSize rep

heapClosureSize :: SMRep -> WordOff
heapClosureSize (HeapRep _ p np ty) = closureTypeHdrSize ty + p + np
heapClosureSize _ = panic "SMRep.heapClosureSize"

closureTypeHdrSize :: ClosureTypeInfo -> WordOff
closureTypeHdrSize ty = case ty of
                  Thunk{}         -> thunkHdrSize
                  ThunkSelector{} -> thunkHdrSize
                  BlackHole{}     -> thunkHdrSize
                  _               -> fixedHdrSize
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
rtsClosureType :: SMRep -> StgHalfWord
rtsClosureType (RTSRep ty _)  = ty

rtsClosureType (HeapRep False 1 0 Constr{}) = CONSTR_1_0
rtsClosureType (HeapRep False 0 1 Constr{}) = CONSTR_0_1
rtsClosureType (HeapRep False 2 0 Constr{}) = CONSTR_2_0
rtsClosureType (HeapRep False 1 1 Constr{}) = CONSTR_1_1
rtsClosureType (HeapRep False 0 2 Constr{}) = CONSTR_0_2
rtsClosureType (HeapRep False _ _ Constr{}) = CONSTR

rtsClosureType (HeapRep False 1 0 Fun{}) = FUN_1_0
rtsClosureType (HeapRep False 0 1 Fun{}) = FUN_0_1
rtsClosureType (HeapRep False 2 0 Fun{}) = FUN_2_0
rtsClosureType (HeapRep False 1 1 Fun{}) = FUN_1_1
rtsClosureType (HeapRep False 0 2 Fun{}) = FUN_0_2
rtsClosureType (HeapRep False _ _ Fun{}) = FUN

rtsClosureType (HeapRep False 1 0 Thunk{}) = THUNK_1_0
rtsClosureType (HeapRep False 0 1 Thunk{}) = THUNK_0_1
rtsClosureType (HeapRep False 2 0 Thunk{}) = THUNK_2_0
rtsClosureType (HeapRep False 1 1 Thunk{}) = THUNK_1_1
rtsClosureType (HeapRep False 0 2 Thunk{}) = THUNK_0_2
rtsClosureType (HeapRep False _ _ Thunk{}) = THUNK

rtsClosureType (HeapRep False _ _ ThunkSelector{}) =  THUNK_SELECTOR

-- Approximation: we use the CONSTR_NOCAF_STATIC type for static constructors
-- that have no pointer words only.
rtsClosureType (HeapRep True 0 _ Constr{}) = CONSTR_NOCAF_STATIC  -- See isStaticNoCafCon below
rtsClosureType (HeapRep True _ _ Constr{}) = CONSTR_STATIC
rtsClosureType (HeapRep True _ _ Fun{})    = FUN_STATIC
rtsClosureType (HeapRep True _ _ Thunk{})  = THUNK_STATIC

rtsClosureType (HeapRep False _ _ BlackHole{}) =  BLACKHOLE

rtsClosureType _ = panic "rtsClosureType"

-- We export these ones
rET_SMALL, rET_BIG, aRG_GEN, aRG_GEN_BIG :: StgHalfWord
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
%*									*
             Pretty printing of SMRep and friends
%*									*
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
  ppr (ArgSpec n) = ptext (sLit "ArgSpec") <+> integer (toInteger n)
  ppr (ArgGen ls) = ptext (sLit "ArgGen") <+> ppr ls
  
pprTypeInfo :: ClosureTypeInfo -> SDoc
pprTypeInfo (Constr tag descr)
  = ptext (sLit "Con") <+> 
    braces (sep [ ptext (sLit "tag:") <+> integer (toInteger tag)
                , ptext (sLit "descr:") <> text (show descr) ])

pprTypeInfo (Fun arity args)
  = ptext (sLit "Fun") <+> 
    braces (sep [ ptext (sLit "arity:") <+> integer (toInteger arity)
                , ptext (sLit ("fun_type:")) <+> ppr args ])

pprTypeInfo (ThunkSelector offset) 
  = ptext (sLit "ThunkSel") <+> integer (toInteger offset)

pprTypeInfo Thunk     = ptext (sLit "Thunk")
pprTypeInfo BlackHole = ptext (sLit "BlackHole")

-- XXX Does not belong here!!
stringToWord8s :: String -> [Word8]
stringToWord8s s = map (fromIntegral . ord) s

pprWord8String :: [Word8] -> SDoc
-- Debug printing.  Not very clever right now.
pprWord8String ws = text (show ws)
\end{code}
