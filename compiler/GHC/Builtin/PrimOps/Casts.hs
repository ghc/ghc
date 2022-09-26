{-
This module contains helpers to cast variables
between different Int/WordReps in StgLand.

-}

module GHC.Builtin.PrimOps.Casts
    ( getCasts )
where

import GHC.Prelude

import GHC.Core.TyCon
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Types.RepType
import GHC.Core.Type
import GHC.Builtin.Types.Prim

import GHC.Builtin.PrimOps
import GHC.Plugins (HasDebugCallStack)

{- Note [PrimRep based casting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module contains a number of utility functions useful when
converting between variables of differing PrimReps.

The general pattern is:
* We have two primReps `from_rep` and `to_rep`.
* We want a list of PrimOps we can apply to a variable of rep `from_rep`.
Applying the list of primOps in order takes us to `to_rep` from `from_rep` giving
us a variable of the returned type at each step.

E.g. we call `getCasts from_rep to_rep` and get back [(op1#,ty1),(op2#,ty2)].
We can use this result to construct a function of type
`StgExpr -> StgExpr` by construction an expression

    case op1# <from> of (x' :: ty1) -> case op2# x' of x' -> <rhs_hole>

Ideally backends will compile the sequence of PrimOps to a no-op. E.g. by reusing
the same register but just relabeling it as another width.
However this is might not always be possible or the required optimizations
simply not implemented in the backend. This means currently many of these casts
will be cheap but not all of them will be completely zero-cost.

-}

-- | `getCasts from_rep to_rep` gives us a list of primops which when applied in order convert from_rep to to_rep.
-- See Note [PrimRep based casting]
getCasts :: PrimRep -> PrimRep -> [(PrimOp,Type)]
getCasts from_rep to_rep
  -- No-op
  | -- pprTrace "getCasts" (ppr (from_rep,to_rep)) $
    to_rep == from_rep
  = []

  -- Float <-> Double
  | to_rep == FloatRep =
    assertPpr (from_rep == DoubleRep) (ppr from_rep <+> ppr to_rep) $
    [(DoubleToFloatOp,floatPrimTy)]
  | to_rep == DoubleRep =
    assertPpr (from_rep == FloatRep) (ppr from_rep <+> ppr to_rep) $
    [(FloatToDoubleOp,doublePrimTy)]

  -- Addr <-> Word/Int
  | to_rep == AddrRep = wordOrIntToAddrRep from_rep
  | from_rep == AddrRep = addrToWordOrIntRep to_rep

  -- Int* -> Int*
  | primRepIsInt from_rep
  , primRepIsInt to_rep
  = sizedIntToSizedInt from_rep to_rep

  -- Word* -> Word*
  | primRepIsWord from_rep
  , primRepIsWord to_rep
  = sizedWordToSizedWord from_rep to_rep

  -- Word* -> Int*
  | primRepIsWord from_rep
  , primRepIsInt to_rep
  = let (op1,r1) = wordToIntRep from_rep
    in (op1,primRepToType r1):sizedIntToSizedInt r1 to_rep

  -- Int* -> Word*
  | primRepIsInt from_rep
  , primRepIsWord to_rep
  = let (op1,r1) = intToWordRep from_rep
    in (op1,primRepToType r1):sizedWordToSizedWord r1 to_rep

  | otherwise = pprPanic "getCasts:Unexpect rep combination"
                          (ppr (from_rep,to_rep))

wordOrIntToAddrRep :: HasDebugCallStack => PrimRep -> [(PrimOp,Type)]
wordOrIntToAddrRep AddrRep = [] -- No-op argument is already AddrRep
wordOrIntToAddrRep IntRep = [(IntToAddrOp, addrPrimTy)]
wordOrIntToAddrRep WordRep = [(WordToIntOp,intPrimTy), (IntToAddrOp,addrPrimTy)]
wordOrIntToAddrRep r
    | primRepIsInt r = (intToMachineInt r,intPrimTy):[(IntToAddrOp,addrPrimTy)]
    | primRepIsWord r =
        let (op1,r1) = wordToIntRep r
        in (op1, primRepToType r1):[(intToMachineInt r1,intPrimTy), (IntToAddrOp,addrPrimTy)]
    | otherwise = pprPanic "Rep not word or int rep" (ppr r)

addrToWordOrIntRep :: HasDebugCallStack => PrimRep -> [(PrimOp,Type)]
-- Machine sizes
addrToWordOrIntRep IntRep = [(AddrToIntOp, intPrimTy)]
addrToWordOrIntRep WordRep = [(AddrToIntOp,intPrimTy), (IntToWordOp,wordPrimTy)]
-- Explicitly sized reps
addrToWordOrIntRep r
    | primRepIsWord r = (AddrToIntOp,intPrimTy) : (IntToWordOp,wordPrimTy) : sizedWordToSizedWord WordRep r
    | primRepIsInt r = (AddrToIntOp,intPrimTy) : sizedIntToSizedInt IntRep r
    | otherwise = pprPanic "Target rep not word or int rep" (ppr r)


-- WordX# -> IntX# (same size), argument is source rep
wordToIntRep :: HasDebugCallStack => PrimRep -> (PrimOp,PrimRep)
wordToIntRep rep
    = case rep of
        (WordRep) -> (WordToIntOp, IntRep)
        (Word8Rep) -> (Word8ToInt8Op, Int8Rep)
        (Word16Rep) -> (Word16ToInt16Op, Int16Rep)
        (Word32Rep) -> (Word32ToInt32Op, Int32Rep)
        (Word64Rep) -> (Word64ToInt64Op, Int64Rep)
        _ -> pprPanic "Rep not a wordRep" (ppr rep)

-- IntX# -> WordX#, argument is source rep
intToWordRep :: HasDebugCallStack => PrimRep -> (PrimOp,PrimRep)
intToWordRep rep
    = case rep of
        (IntRep) -> (IntToWordOp, WordRep)
        (Int8Rep) -> (Int8ToWord8Op, Word8Rep)
        (Int16Rep) -> (Int16ToWord16Op, Word16Rep)
        (Int32Rep) -> (Int32ToWord32Op, Word32Rep)
        (Int64Rep) -> (Int64ToWord64Op, Word64Rep)
        _ -> pprPanic "Rep not a wordRep" (ppr rep)

-- Casts between any size int to any other size of int
sizedIntToSizedInt :: HasDebugCallStack => PrimRep -> PrimRep -> [(PrimOp,Type)]
sizedIntToSizedInt r1 r2
    | r1 == r2 = []
-- Cast to Int#
sizedIntToSizedInt r IntRep = [(intToMachineInt r,intPrimTy)]
-- Cast from Int#
sizedIntToSizedInt IntRep r = [(intFromMachineInt r,primRepToType r)]
-- Sized to differently sized must go over machine word.
sizedIntToSizedInt r1 r2 = (intToMachineInt r1,intPrimTy) : [(intFromMachineInt r2,primRepToType r2)]

-- Casts between any size Word to any other size of Word
sizedWordToSizedWord :: HasDebugCallStack => PrimRep -> PrimRep -> [(PrimOp,Type)]
sizedWordToSizedWord r1 r2
    | r1 == r2 = []
-- Cast to Word#
sizedWordToSizedWord r WordRep = [(wordToMachineWord r,wordPrimTy)]
-- Cast from Word#
sizedWordToSizedWord WordRep r = [(wordFromMachineWord r, primRepToType r)]
-- Conversion between different non-machine sizes must go via machine word.
sizedWordToSizedWord r1 r2 = (wordToMachineWord r1,wordPrimTy) : [(wordFromMachineWord r2, primRepToType r2)]


-- Prefer the definitions above this line if possible
----------------------


-- Int*# to Int#
{-# INLINE intToMachineInt #-}
intToMachineInt :: HasDebugCallStack => PrimRep -> PrimOp
intToMachineInt r =
    assertPpr (primRepIsInt r) (ppr r) $
    case r of
        (Int8Rep) -> Int8ToIntOp
        (Int16Rep) -> Int16ToIntOp
        (Int32Rep) -> Int32ToIntOp
        (Int64Rep) -> Int64ToIntOp
        _ -> pprPanic "Source rep not int" $ ppr r

-- Int# to Int*#
{-# INLINE intFromMachineInt #-}
intFromMachineInt :: HasDebugCallStack => PrimRep -> PrimOp
intFromMachineInt r =
    assertPpr (primRepIsInt r) (ppr r) $
    case r of
        Int8Rep -> IntToInt8Op
        Int16Rep -> IntToInt16Op
        Int32Rep -> IntToInt32Op
        Int64Rep -> IntToInt64Op
        _ -> pprPanic "Dest rep not sized int" $ ppr r

-- Word# to Word*#
{-# INLINE wordFromMachineWord #-}
wordFromMachineWord :: HasDebugCallStack => PrimRep -> PrimOp
wordFromMachineWord r =
    assert (primRepIsWord r) $
    case r of
        Word8Rep -> WordToWord8Op
        Word16Rep -> WordToWord16Op
        Word32Rep -> WordToWord32Op
        Word64Rep -> WordToWord64Op
        _ -> pprPanic "Dest rep not sized word" $ ppr r

-- Word*# to Word#
{-# INLINE wordToMachineWord #-}
wordToMachineWord :: HasDebugCallStack => PrimRep -> PrimOp
wordToMachineWord r =
    assertPpr (primRepIsWord r) (text "Not a word rep:" <> ppr r) $
    case r of
        Word8Rep -> Word8ToWordOp
        Word16Rep -> Word16ToWordOp
        Word32Rep -> Word32ToWordOp
        Word64Rep -> Word64ToWordOp
        _ -> pprPanic "Dest rep not sized word" $ ppr r