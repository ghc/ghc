-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
--
-- Storage manager representation of closures

{-# LANGUAGE CPP,GeneralizedNewtypeDeriving #-}

module SMRep (
        -- * Words and bytes
        WordOff, ByteOff,
        wordsToBytes, bytesToWordsRoundUp,
        roundUpToWords, roundUpTo,

        StgWord, fromStgWord, toStgWord,
        StgHalfWord, fromStgHalfWord, toStgHalfWord,
        hALF_WORD_SIZE, hALF_WORD_SIZE_IN_BITS,

        -- * Closure repesentation
        SMRep(..), -- CmmInfo sees the rep; no one else does
        IsStatic,
        ClosureTypeInfo(..), ArgDescr(..), Liveness,
        ConstrDescription,

        -- ** Construction
        mkHeapRep, blackHoleRep, indStaticRep, mkStackRep, mkRTSRep, arrPtrsRep,
        smallArrPtrsRep, arrWordsRep,

        -- ** Predicates
        isStaticRep, isConRep, isThunkRep, isFunRep, isStaticNoCafCon,
        isStackRep,

        -- ** Size-related things
        heapClosureSizeW,
        fixedHdrSizeW, arrWordsHdrSize, arrWordsHdrSizeW, arrPtrsHdrSize,
        arrPtrsHdrSizeW, profHdrSize, thunkHdrSize, nonHdrSize, nonHdrSizeW,
        smallArrPtrsHdrSize, smallArrPtrsHdrSizeW, hdrSize, hdrSizeW,
        fixedHdrSize,

        -- ** RTS closure types
        rtsClosureType, rET_SMALL, rET_BIG,
        aRG_GEN, aRG_GEN_BIG,

        -- ** Arrays
        card, cardRoundUp, cardTableSizeB, cardTableSizeW,

        -- * Operations over [Word8] strings that don't belong here
        pprWord8String, stringToWord8s
    ) where

#include "../HsVersions.h"
#include "../includes/MachDeps.h"

import GhcPrelude

import BasicTypes( ConTagZ )
import DynFlags
import Outputable
import Platform
import FastString

import Data.Char( ord )
import Data.Word
import Data.Bits

{-
************************************************************************
*                                                                      *
                Words and bytes
*                                                                      *
************************************************************************
-}

-- | Word offset, or word count
type WordOff = Int

-- | Byte offset, or byte count
type ByteOff = Int

-- | Round up the given byte count to the next byte count that's a
-- multiple of the machine's word size.
roundUpToWords :: DynFlags -> ByteOff -> ByteOff
roundUpToWords dflags n = roundUpTo n (wORD_SIZE dflags)

-- | Round up @base@ to a multiple of @size@.
roundUpTo :: ByteOff -> ByteOff -> ByteOff
roundUpTo base size = (base + (size - 1)) .&. (complement (size - 1))

-- | Convert the given number of words to a number of bytes.
--
-- This function morally has type @WordOff -> ByteOff@, but uses @Num
-- a@ to allow for overloading.
wordsToBytes :: Num a => DynFlags -> a -> a
wordsToBytes dflags n = fromIntegral (wORD_SIZE dflags) * n
{-# SPECIALIZE wordsToBytes :: DynFlags -> Int -> Int #-}
{-# SPECIALIZE wordsToBytes :: DynFlags -> Word -> Word #-}
{-# SPECIALIZE wordsToBytes :: DynFlags -> Integer -> Integer #-}

-- | First round the given byte count up to a multiple of the
-- machine's word size and then convert the result to words.
bytesToWordsRoundUp :: DynFlags -> ByteOff -> WordOff
bytesToWordsRoundUp dflags n = (n + word_size - 1) `quot` word_size
 where word_size = wORD_SIZE dflags
-- StgWord is a type representing an StgWord on the target platform.
-- A Word64 is large enough to hold a Word for either a 32bit or 64bit platform
newtype StgWord = StgWord Word64
    deriving (Eq, Bits)

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

{-
************************************************************************
*                                                                      *
\subsubsection[SMRep-datatype]{@SMRep@---storage manager representation}
*                                                                      *
************************************************************************
-}

-- | A description of the layout of a closure.  Corresponds directly
-- to the closure types in includes/rts/storage/ClosureTypes.h.
data SMRep
  = HeapRep              -- GC routines consult sizes in info tbl
        IsStatic
        !WordOff         --  # ptr words
        !WordOff         --  # non-ptr words INCLUDING SLOP (see mkHeapRep below)
        ClosureTypeInfo  -- type-specific info

  | ArrayPtrsRep
        !WordOff        -- # ptr words
        !WordOff        -- # card table words

  | SmallArrayPtrsRep
        !WordOff        -- # ptr words

  | ArrayWordsRep
        !WordOff        -- # bytes expressed in words, rounded up

  | StackRep            -- Stack frame (RET_SMALL or RET_BIG)
        Liveness

  | RTSRep              -- The RTS needs to declare info tables with specific
        Int             -- type tags, so this form lets us override the default
        SMRep           -- tag for an SMRep.

-- | True <=> This is a static closure.  Affects how we garbage-collect it.
-- Static closure have an extra static link field at the end.
-- Constructors do not have a static variant; see Note [static constructors]
type IsStatic = Bool

-- From an SMRep you can get to the closure type defined in
-- includes/rts/storage/ClosureTypes.h. Described by the function
-- rtsClosureType below.

data ClosureTypeInfo
  = Constr        ConTagZ ConstrDescription
  | Fun           FunArity ArgDescr
  | Thunk
  | ThunkSelector SelectorOffset
  | BlackHole
  | IndStatic

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

arrPtrsRep :: DynFlags -> WordOff -> SMRep
arrPtrsRep dflags elems = ArrayPtrsRep elems (cardTableSizeW dflags elems)

smallArrPtrsRep :: WordOff -> SMRep
smallArrPtrsRep elems = SmallArrayPtrsRep elems

arrWordsRep :: DynFlags -> ByteOff -> SMRep
arrWordsRep dflags bytes = ArrayWordsRep (bytesToWordsRoundUp dflags bytes)

-----------------------------------------------------------------------------
-- Predicates

isStaticRep :: SMRep -> IsStatic
isStaticRep (HeapRep is_static _ _ _) = is_static
isStaticRep (RTSRep _ rep)            = isStaticRep rep
isStaticRep _                         = False

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
-- This should line up exactly with CONSTR_NOCAF below
-- See Note [Static NoCaf constructors]
isStaticNoCafCon (HeapRep _ 0 _ Constr{}) = True
isStaticNoCafCon _                        = False


-----------------------------------------------------------------------------
-- Size-related things

fixedHdrSize :: DynFlags -> ByteOff
fixedHdrSize dflags = wordsToBytes dflags (fixedHdrSizeW dflags)

-- | Size of a closure header (StgHeader in includes/rts/storage/Closures.h)
fixedHdrSizeW :: DynFlags -> WordOff
fixedHdrSizeW dflags = sTD_HDR_SIZE dflags + profHdrSize dflags

-- | Size of the profiling part of a closure header
-- (StgProfHeader in includes/rts/storage/Closures.h)
profHdrSize  :: DynFlags -> WordOff
profHdrSize dflags
 | gopt Opt_SccProfilingOn dflags = pROF_HDR_SIZE dflags
 | otherwise                      = 0

-- | The garbage collector requires that every closure is at least as
--   big as this.
minClosureSize :: DynFlags -> WordOff
minClosureSize dflags = fixedHdrSizeW dflags + mIN_PAYLOAD_SIZE dflags

arrWordsHdrSize :: DynFlags -> ByteOff
arrWordsHdrSize dflags
 = fixedHdrSize dflags + sIZEOF_StgArrBytes_NoHdr dflags

arrWordsHdrSizeW :: DynFlags -> WordOff
arrWordsHdrSizeW dflags =
    fixedHdrSizeW dflags +
    (sIZEOF_StgArrBytes_NoHdr dflags `quot` wORD_SIZE dflags)

arrPtrsHdrSize :: DynFlags -> ByteOff
arrPtrsHdrSize dflags
 = fixedHdrSize dflags + sIZEOF_StgMutArrPtrs_NoHdr dflags

arrPtrsHdrSizeW :: DynFlags -> WordOff
arrPtrsHdrSizeW dflags =
    fixedHdrSizeW dflags +
    (sIZEOF_StgMutArrPtrs_NoHdr dflags `quot` wORD_SIZE dflags)

smallArrPtrsHdrSize :: DynFlags -> ByteOff
smallArrPtrsHdrSize dflags
 = fixedHdrSize dflags + sIZEOF_StgSmallMutArrPtrs_NoHdr dflags

smallArrPtrsHdrSizeW :: DynFlags -> WordOff
smallArrPtrsHdrSizeW dflags =
    fixedHdrSizeW dflags +
    (sIZEOF_StgSmallMutArrPtrs_NoHdr dflags `quot` wORD_SIZE dflags)

-- Thunks have an extra header word on SMP, so the update doesn't
-- splat the payload.
thunkHdrSize :: DynFlags -> WordOff
thunkHdrSize dflags = fixedHdrSizeW dflags + smp_hdr
        where smp_hdr = sIZEOF_StgSMPThunkHeader dflags `quot` wORD_SIZE dflags

hdrSize :: DynFlags -> SMRep -> ByteOff
hdrSize dflags rep = wordsToBytes dflags (hdrSizeW dflags rep)

hdrSizeW :: DynFlags -> SMRep -> WordOff
hdrSizeW dflags (HeapRep _ _ _ ty)    = closureTypeHdrSize dflags ty
hdrSizeW dflags (ArrayPtrsRep _ _)    = arrPtrsHdrSizeW dflags
hdrSizeW dflags (SmallArrayPtrsRep _) = smallArrPtrsHdrSizeW dflags
hdrSizeW dflags (ArrayWordsRep _)     = arrWordsHdrSizeW dflags
hdrSizeW _ _                          = panic "SMRep.hdrSizeW"

nonHdrSize :: DynFlags -> SMRep -> ByteOff
nonHdrSize dflags rep = wordsToBytes dflags (nonHdrSizeW rep)

nonHdrSizeW :: SMRep -> WordOff
nonHdrSizeW (HeapRep _ p np _) = p + np
nonHdrSizeW (ArrayPtrsRep elems ct) = elems + ct
nonHdrSizeW (SmallArrayPtrsRep elems) = elems
nonHdrSizeW (ArrayWordsRep words) = words
nonHdrSizeW (StackRep bs)      = length bs
nonHdrSizeW (RTSRep _ rep)     = nonHdrSizeW rep

-- | The total size of the closure, in words.
heapClosureSizeW :: DynFlags -> SMRep -> WordOff
heapClosureSizeW dflags (HeapRep _ p np ty)
 = closureTypeHdrSize dflags ty + p + np
heapClosureSizeW dflags (ArrayPtrsRep elems ct)
 = arrPtrsHdrSizeW dflags + elems + ct
heapClosureSizeW dflags (SmallArrayPtrsRep elems)
 = smallArrPtrsHdrSizeW dflags + elems
heapClosureSizeW dflags (ArrayWordsRep words)
 = arrWordsHdrSizeW dflags + words
heapClosureSizeW _ _ = panic "SMRep.heapClosureSize"

closureTypeHdrSize :: DynFlags -> ClosureTypeInfo -> WordOff
closureTypeHdrSize dflags ty = case ty of
                  Thunk{}         -> thunkHdrSize dflags
                  ThunkSelector{} -> thunkHdrSize dflags
                  BlackHole{}     -> thunkHdrSize dflags
                  IndStatic{}     -> thunkHdrSize dflags
                  _               -> fixedHdrSizeW dflags
        -- All thunks use thunkHdrSize, even if they are non-updatable.
        -- this is because we don't have separate closure types for
        -- updatable vs. non-updatable thunks, so the GC can't tell the
        -- difference.  If we ever have significant numbers of non-
        -- updatable thunks, it might be worth fixing this.

-- ---------------------------------------------------------------------------
-- Arrays

-- | The byte offset into the card table of the card for a given element
card :: DynFlags -> Int -> Int
card dflags i = i `shiftR` mUT_ARR_PTRS_CARD_BITS dflags

-- | Convert a number of elements to a number of cards, rounding up
cardRoundUp :: DynFlags -> Int -> Int
cardRoundUp dflags i =
  card dflags (i + ((1 `shiftL` mUT_ARR_PTRS_CARD_BITS dflags) - 1))

-- | The size of a card table, in bytes
cardTableSizeB :: DynFlags -> Int -> ByteOff
cardTableSizeB dflags elems = cardRoundUp dflags elems

-- | The size of a card table, in words
cardTableSizeW :: DynFlags -> Int -> WordOff
cardTableSizeW dflags elems =
  bytesToWordsRoundUp dflags (cardTableSizeB dflags elems)

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

      -- See Note [static constructors]
      HeapRep _     1 0 Constr{} -> CONSTR_1_0
      HeapRep _     0 1 Constr{} -> CONSTR_0_1
      HeapRep _     2 0 Constr{} -> CONSTR_2_0
      HeapRep _     1 1 Constr{} -> CONSTR_1_1
      HeapRep _     0 2 Constr{} -> CONSTR_0_2
      HeapRep _     0 _ Constr{} -> CONSTR_NOCAF
           -- See Note [Static NoCaf constructors]
      HeapRep _     _ _ Constr{} -> CONSTR

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

{-
Note [static constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~

We used to have a CONSTR_STATIC closure type, and each constructor had
two info tables: one with CONSTR (or CONSTR_1_0 etc.), and one with
CONSTR_STATIC.

This distinction was removed, because when copying a data structure
into a compact region, we must copy static constructors into the
compact region too.  If we didn't do this, we would need to track the
references from the compact region out to the static constructors,
because they might (indirectly) refer to CAFs.

Since static constructors will be copied to the heap, if we wanted to
use different info tables for static and dynamic constructors, we
would have to switch the info pointer when copying the constructor
into the compact region, which means we would need an extra field of
the static info table to point to the dynamic one.

However, since the distinction between static and dynamic closure
types is never actually needed (other than for assertions), we can
just drop the distinction and use the same info table for both.

The GC *does* need to distinguish between static and dynamic closures,
but it does this using the HEAP_ALLOCED() macro which checks whether
the address of the closure resides within the dynamic heap.
HEAP_ALLOCED() doesn't read the closure's info table.

Note [Static NoCaf constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we know that a top-level binding 'x' is not Caffy (ie no CAFs are
reachable from 'x'), then a statically allocated constructor (Just x)
is also not Caffy, and the garbage collector need not follow its
argument fields.  Exploiting this would require two static info tables
for Just, for the two cases where the argument was Caffy or non-Caffy.

Currently we don't do this; instead we treat nullary constructors
as non-Caffy, and the others as potentially Caffy.


************************************************************************
*                                                                      *
             Pretty printing of SMRep and friends
*                                                                      *
************************************************************************
-}

instance Outputable ClosureTypeInfo where
   ppr = pprTypeInfo

instance Outputable SMRep where
   ppr (HeapRep static ps nps tyinfo)
     = hang (header <+> lbrace) 2 (ppr tyinfo <+> rbrace)
     where
       header = text "HeapRep"
                <+> if static then text "static" else empty
                <+> pp_n "ptrs" ps <+> pp_n "nonptrs" nps
       pp_n :: String -> Int -> SDoc
       pp_n _ 0 = empty
       pp_n s n = int n <+> text s

   ppr (ArrayPtrsRep size _) = text "ArrayPtrsRep" <+> ppr size

   ppr (SmallArrayPtrsRep size) = text "SmallArrayPtrsRep" <+> ppr size

   ppr (ArrayWordsRep words) = text "ArrayWordsRep" <+> ppr words

   ppr (StackRep bs) = text "StackRep" <+> ppr bs

   ppr (RTSRep ty rep) = text "tag:" <> ppr ty <+> ppr rep

instance Outputable ArgDescr where
  ppr (ArgSpec n) = text "ArgSpec" <+> ppr n
  ppr (ArgGen ls) = text "ArgGen" <+> ppr ls

pprTypeInfo :: ClosureTypeInfo -> SDoc
pprTypeInfo (Constr tag descr)
  = text "Con" <+>
    braces (sep [ text "tag:" <+> ppr tag
                , text "descr:" <> text (show descr) ])

pprTypeInfo (Fun arity args)
  = text "Fun" <+>
    braces (sep [ text "arity:" <+> ppr arity
                , ptext (sLit ("fun_type:")) <+> ppr args ])

pprTypeInfo (ThunkSelector offset)
  = text "ThunkSel" <+> ppr offset

pprTypeInfo Thunk     = text "Thunk"
pprTypeInfo BlackHole = text "BlackHole"
pprTypeInfo IndStatic = text "IndStatic"

-- XXX Does not belong here!!
stringToWord8s :: String -> [Word8]
stringToWord8s s = map (fromIntegral . ord) s

pprWord8String :: [Word8] -> SDoc
-- Debug printing.  Not very clever right now.
pprWord8String ws = text (show ws)
