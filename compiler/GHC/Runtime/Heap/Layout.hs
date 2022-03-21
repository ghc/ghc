-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
--
-- Storage manager representation of closures

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Runtime.Heap.Layout (
        -- * Words and bytes
        WordOff, ByteOff,
        wordsToBytes, bytesToWordsRoundUp,
        roundUpToWords, roundUpTo,

        StgWord, fromStgWord, toStgWord,
        StgHalfWord, fromStgHalfWord, toStgHalfWord,
        halfWordSize, halfWordSizeInBits,

        -- * Closure representation
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
        card, cardRoundUp, cardTableSizeB, cardTableSizeW
    ) where

import GHC.Prelude

import GHC.Types.Basic( ConTagZ )
import GHC.Platform
import GHC.Platform.Profile

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Word
import Data.ByteString (ByteString)

{-
************************************************************************
*                                                                      *
                Words and bytes
*                                                                      *
************************************************************************
-}

-- | Byte offset, or byte count
type ByteOff = Int

-- | Word offset, or word count
type WordOff = Int

-- | Round up the given byte count to the next byte count that's a
-- multiple of the machine's word size.
roundUpToWords :: Platform -> ByteOff -> ByteOff
roundUpToWords platform n = roundUpTo n (platformWordSizeInBytes platform)

-- | Round up @base@ to a multiple of @size@.
roundUpTo :: ByteOff -> ByteOff -> ByteOff
roundUpTo base size = (base + (size - 1)) .&. (complement (size - 1))

-- | Convert the given number of words to a number of bytes.
--
-- This function morally has type @WordOff -> ByteOff@, but uses @Num
-- a@ to allow for overloading.
wordsToBytes :: Num a => Platform -> a -> a
wordsToBytes platform n = fromIntegral (platformWordSizeInBytes platform) * n
{-# SPECIALIZE wordsToBytes :: Platform -> Int -> Int #-}
{-# SPECIALIZE wordsToBytes :: Platform -> Word -> Word #-}
{-# SPECIALIZE wordsToBytes :: Platform -> Integer -> Integer #-}

-- | First round the given byte count up to a multiple of the
-- machine's word size and then convert the result to words.
bytesToWordsRoundUp :: Platform -> ByteOff -> WordOff
bytesToWordsRoundUp platform n = (n + word_size - 1) `quot` word_size
 where word_size = platformWordSizeInBytes platform
-- StgWord is a type representing an StgWord on the target platform.
-- A Word64 is large enough to hold a Word for either a 32bit or 64bit platform
newtype StgWord = StgWord Word64
    deriving (Eq, Bits)

fromStgWord :: StgWord -> Integer
fromStgWord (StgWord i) = toInteger i

toStgWord :: Platform -> Integer -> StgWord
toStgWord platform i
    = case platformWordSize platform of
      -- These conversions mean that things like toStgWord (-1)
      -- do the right thing
      PW4 -> StgWord (fromIntegral (fromInteger i :: Word32))
      PW8 -> StgWord (fromInteger i)

instance Outputable StgWord where
    ppr (StgWord i) = integer (toInteger i)

--

-- A Word32 is large enough to hold half a Word for either a 32bit or
-- 64bit platform
newtype StgHalfWord = StgHalfWord Word32
    deriving Eq

fromStgHalfWord :: StgHalfWord -> Integer
fromStgHalfWord (StgHalfWord w) = toInteger w

toStgHalfWord :: Platform -> Integer -> StgHalfWord
toStgHalfWord platform i
    = case platformWordSize platform of
      -- These conversions mean that things like toStgHalfWord (-1)
      -- do the right thing
      PW4 -> StgHalfWord (fromIntegral (fromInteger i :: Word16))
      PW8 -> StgHalfWord (fromInteger i :: Word32)

instance Outputable StgHalfWord where
    ppr (StgHalfWord w) = integer (toInteger w)

-- | Half word size in bytes
halfWordSize :: Platform -> ByteOff
halfWordSize platform = platformWordSizeInBytes platform `div` 2

halfWordSizeInBits :: Platform -> Int
halfWordSizeInBits platform = platformWordSizeInBits platform `div` 2

{-
************************************************************************
*                                                                      *
\subsubsection[SMRep-datatype]{@SMRep@---storage manager representation}
*                                                                      *
************************************************************************
-}

-- | A description of the layout of a closure.  Corresponds directly
-- to the closure types in includes\/rts\/storage\/ClosureTypes.h.
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
  deriving Eq

-- | True \<=> This is a static closure.  Affects how we garbage-collect it.
-- Static closure have an extra static link field at the end.
-- Constructors do not have a static variant; see Note [static constructors]
type IsStatic = Bool

-- From an SMRep you can get to the closure type defined in
-- rts/include/rts/storage/ClosureTypes.h. Described by the function
-- rtsClosureType below.

data ClosureTypeInfo
  = Constr        ConTagZ ConstrDescription
  | Fun           FunArity ArgDescr
  | Thunk
  | ThunkSelector SelectorOffset
  | BlackHole
  | IndStatic
  deriving Eq

type ConstrDescription = ByteString -- result of dataConIdentity
type FunArity          = Int
type SelectorOffset    = Int

-- | We represent liveness bitmaps as a Bitmap (whose internal representation
-- really is a bitmap).  These are pinned onto case return vectors to indicate
-- the state of the stack for the garbage collector.
--
-- In the compiled program, liveness bitmaps that fit inside a single word
-- (StgWord) are stored as a single word, while larger bitmaps are stored as a
-- pointer to an array of words.

type Liveness = [Bool]   -- One Bool per word; True  <=> non-ptr or dead
                         --                    False <=> ptr

--------------------------------------------------------------------------------
-- | An ArgDescr describes the argument pattern of a function

data ArgDescr
  = ArgSpec             -- Fits one of the standard patterns
        !Int            -- RTS type identifier ARG_P, ARG_N, ...

  | ArgGen              -- General case
        Liveness        -- Details about the arguments

  | ArgUnknown          -- For imported binds.
                        -- Invariant: Never Unknown for binds of the module
                        -- we are compiling.
  deriving (Eq)

instance Outputable ArgDescr where
  ppr (ArgSpec n) = text "ArgSpec" <+> ppr n
  ppr (ArgGen ls) = text "ArgGen" <+> ppr ls
  ppr ArgUnknown = text "ArgUnknown"

-----------------------------------------------------------------------------
-- Construction

mkHeapRep :: Profile -> IsStatic -> WordOff -> WordOff -> ClosureTypeInfo
          -> SMRep
mkHeapRep profile is_static ptr_wds nonptr_wds cl_type_info
  = HeapRep is_static
            ptr_wds
            (nonptr_wds + slop_wds)
            cl_type_info
  where
     slop_wds
      | is_static = 0
      | otherwise = max 0 (minClosureSize profile - (hdr_size + payload_size))

     hdr_size     = closureTypeHdrSize profile cl_type_info
     payload_size = ptr_wds + nonptr_wds

mkRTSRep :: Int -> SMRep -> SMRep
mkRTSRep = RTSRep

mkStackRep :: [Bool] -> SMRep
mkStackRep liveness = StackRep liveness

blackHoleRep :: SMRep
blackHoleRep = HeapRep False 0 0 BlackHole

indStaticRep :: SMRep
indStaticRep = HeapRep True 1 0 IndStatic

arrPtrsRep :: Platform -> WordOff -> SMRep
arrPtrsRep platform elems = ArrayPtrsRep elems (cardTableSizeW platform elems)

smallArrPtrsRep :: WordOff -> SMRep
smallArrPtrsRep elems = SmallArrayPtrsRep elems

arrWordsRep :: Platform -> ByteOff -> SMRep
arrWordsRep platform bytes = ArrayWordsRep (bytesToWordsRoundUp platform bytes)

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
isThunkRep (HeapRep _ _ _ Thunk)           = True
isThunkRep (HeapRep _ _ _ ThunkSelector{}) = True
isThunkRep (HeapRep _ _ _ BlackHole)       = True
isThunkRep (HeapRep _ _ _ IndStatic)       = True
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

fixedHdrSize :: Profile -> ByteOff
fixedHdrSize profile = wordsToBytes (profilePlatform profile) (fixedHdrSizeW profile)

-- | Size of a closure header (StgHeader in includes\/rts\/storage\/Closures.h)
fixedHdrSizeW :: Profile -> WordOff
fixedHdrSizeW profile = pc_STD_HDR_SIZE (profileConstants profile) + profHdrSize profile

-- | Size of the profiling part of a closure header
-- (StgProfHeader in includes\/rts\/storage\/Closures.h)
profHdrSize :: Profile -> WordOff
profHdrSize profile =
   if profileIsProfiling profile
      then pc_PROF_HDR_SIZE (profileConstants profile)
      else 0

-- | The garbage collector requires that every closure is at least as
--   big as this.
minClosureSize :: Profile -> WordOff
minClosureSize profile
 = fixedHdrSizeW profile
   + pc_MIN_PAYLOAD_SIZE (profileConstants profile)

arrWordsHdrSize :: Profile -> ByteOff
arrWordsHdrSize profile
 = fixedHdrSize profile
   + pc_SIZEOF_StgArrBytes_NoHdr (profileConstants profile)

arrWordsHdrSizeW :: Profile -> WordOff
arrWordsHdrSizeW profile
 = fixedHdrSizeW profile
   + (pc_SIZEOF_StgArrBytes_NoHdr (profileConstants profile) `quot`
      platformWordSizeInBytes (profilePlatform profile))

arrPtrsHdrSize :: Profile -> ByteOff
arrPtrsHdrSize profile
 = fixedHdrSize profile
   + pc_SIZEOF_StgMutArrPtrs_NoHdr (profileConstants profile)

arrPtrsHdrSizeW :: Profile -> WordOff
arrPtrsHdrSizeW profile
 = fixedHdrSizeW profile
   + (pc_SIZEOF_StgMutArrPtrs_NoHdr (profileConstants profile) `quot`
      platformWordSizeInBytes (profilePlatform profile))

smallArrPtrsHdrSize :: Profile -> ByteOff
smallArrPtrsHdrSize profile
 = fixedHdrSize profile
   + pc_SIZEOF_StgSmallMutArrPtrs_NoHdr (profileConstants profile)

smallArrPtrsHdrSizeW :: Profile -> WordOff
smallArrPtrsHdrSizeW profile
 = fixedHdrSizeW profile
   + (pc_SIZEOF_StgSmallMutArrPtrs_NoHdr (profileConstants profile) `quot`
      platformWordSizeInBytes (profilePlatform profile))

-- Thunks have an extra header word on SMP, so the update doesn't
-- splat the payload.
thunkHdrSize :: Profile -> WordOff
thunkHdrSize profile = fixedHdrSizeW profile + smp_hdr
        where
         platform = profilePlatform profile
         smp_hdr  = pc_SIZEOF_StgSMPThunkHeader (platformConstants platform) `quot`
                         platformWordSizeInBytes platform

hdrSize :: Profile -> SMRep -> ByteOff
hdrSize profile rep = wordsToBytes (profilePlatform profile) (hdrSizeW profile rep)

hdrSizeW :: Profile -> SMRep -> WordOff
hdrSizeW profile (HeapRep _ _ _ ty)    = closureTypeHdrSize profile ty
hdrSizeW profile (ArrayPtrsRep _ _)    = arrPtrsHdrSizeW profile
hdrSizeW profile (SmallArrayPtrsRep _) = smallArrPtrsHdrSizeW profile
hdrSizeW profile (ArrayWordsRep _)     = arrWordsHdrSizeW profile
hdrSizeW _ _                           = panic "GHC.Runtime.Heap.Layout.hdrSizeW"

nonHdrSize :: Platform -> SMRep -> ByteOff
nonHdrSize platform rep = wordsToBytes platform (nonHdrSizeW rep)

nonHdrSizeW :: SMRep -> WordOff
nonHdrSizeW (HeapRep _ p np _) = p + np
nonHdrSizeW (ArrayPtrsRep elems ct) = elems + ct
nonHdrSizeW (SmallArrayPtrsRep elems) = elems
nonHdrSizeW (ArrayWordsRep words) = words
nonHdrSizeW (StackRep bs)      = length bs
nonHdrSizeW (RTSRep _ rep)     = nonHdrSizeW rep

-- | The total size of the closure, in words.
heapClosureSizeW :: Profile -> SMRep -> WordOff
heapClosureSizeW profile rep = case rep of
   HeapRep _ p np ty       -> closureTypeHdrSize profile ty + p + np
   ArrayPtrsRep elems ct   -> arrPtrsHdrSizeW profile + elems + ct
   SmallArrayPtrsRep elems -> smallArrPtrsHdrSizeW profile + elems
   ArrayWordsRep words     -> arrWordsHdrSizeW profile + words
   _                       -> panic "GHC.Runtime.Heap.Layout.heapClosureSize"

closureTypeHdrSize :: Profile -> ClosureTypeInfo -> WordOff
closureTypeHdrSize profile ty = case ty of
                  Thunk           -> thunkHdrSize profile
                  ThunkSelector{} -> thunkHdrSize profile
                  BlackHole       -> thunkHdrSize profile
                  IndStatic       -> thunkHdrSize profile
                  _               -> fixedHdrSizeW profile
        -- All thunks use thunkHdrSize, even if they are non-updatable.
        -- this is because we don't have separate closure types for
        -- updatable vs. non-updatable thunks, so the GC can't tell the
        -- difference.  If we ever have significant numbers of non-
        -- updatable thunks, it might be worth fixing this.

-- ---------------------------------------------------------------------------
-- Arrays

-- | The byte offset into the card table of the card for a given element
card :: Platform -> Int -> Int
card platform i = i `shiftR` pc_MUT_ARR_PTRS_CARD_BITS (platformConstants platform)

-- | Convert a number of elements to a number of cards, rounding up
cardRoundUp :: Platform -> Int -> Int
cardRoundUp platform i =
  card platform (i + ((1 `shiftL` pc_MUT_ARR_PTRS_CARD_BITS (platformConstants platform)) - 1))

-- | The size of a card table, in bytes
cardTableSizeB :: Platform -> Int -> ByteOff
cardTableSizeB platform elems = cardRoundUp platform elems

-- | The size of a card table, in words
cardTableSizeW :: Platform -> Int -> WordOff
cardTableSizeW platform elems =
  bytesToWordsRoundUp platform (cardTableSizeB platform elems)

-----------------------------------------------------------------------------
-- deriving the RTS closure type from an SMRep

#include "ClosureTypes.h"
#include "FunTypes.h"
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

      HeapRep False 1 0 Thunk -> THUNK_1_0
      HeapRep False 0 1 Thunk -> THUNK_0_1
      HeapRep False 2 0 Thunk -> THUNK_2_0
      HeapRep False 1 1 Thunk -> THUNK_1_1
      HeapRep False 0 2 Thunk -> THUNK_0_2
      HeapRep False _ _ Thunk -> THUNK

      HeapRep False _ _ ThunkSelector{} ->  THUNK_SELECTOR

      HeapRep True _ _ Fun{}      -> FUN_STATIC
      HeapRep True _ _ Thunk      -> THUNK_STATIC
      HeapRep False _ _ BlackHole -> BLACKHOLE
      HeapRep False _ _ IndStatic -> IND_STATIC

      StackRep _ -> STACK

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

pprTypeInfo :: ClosureTypeInfo -> SDoc
pprTypeInfo (Constr tag descr)
  = text "Con" <+>
    braces (sep [ text "tag:" <+> ppr tag
                , text "descr:" <> text (show descr) ])

pprTypeInfo (Fun arity args)
  = text "Fun" <+>
    braces (sep [ text "arity:"    <+> ppr arity
                , text "fun_type:" <+> ppr args ])

pprTypeInfo (ThunkSelector offset)
  = text "ThunkSel" <+> ppr offset

pprTypeInfo Thunk     = text "Thunk"
pprTypeInfo BlackHole = text "BlackHole"
pprTypeInfo IndStatic = text "IndStatic"
