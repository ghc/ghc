{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}

-- | Range analysis
--
-- See Note [Value range analysis]
module GHC.Core.Opt.Range
  ( Comparison(..)
  , Range(..)
  , noRange
  , valueRange
  , rangeIntersect
  , rangeCastFrom
  , inRange
  , rangeSize
  , rangeCmp
  , rangeLe
  , rangeGe
  , rangeLt
  , rangeGt
  , rangeOf
  , rangeWord
  , rangeWord8
  , rangeWord16
  , rangeWord32
  , rangeWord64
  , rangeChar
  , rangeInt
  , rangeInt8
  , rangeInt16
  , rangeInt32
  , rangeInt64
  )
where

import GHC.Prelude
import GHC.Platform
import GHC.Core
import GHC.Builtin.PrimOps ( PrimOp(..) )
import GHC.Builtin.PrimOps.Ids (primOpId)
import GHC.Types.Literal
import GHC.Types.Id
#if defined(DEBUG)
import GHC.Utils.Outputable
import GHC.Utils.Panic
#endif

import Data.Word
import Data.Int
import Data.Char (ord)
import Data.Maybe

{-
Note [Value range analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
To perform some constant-folding optimisations, it is sometimes enough to know
the range (interval) of what an expression will evaluate to. For example,
consider the following expression:

  word8ToWord x < 256

Even without knowing the value of 'x', we know that:

  - x is in range [0,255] because it is of type Word8
  - `word8ToWord x` is in range [0,255] despite being of type Word
  - `word8ToWord x < 256` is always `True`

When a comparison primop is applied to two expressions (e.g. `ltWord# e1 e2`),
we infer the ranges of `e1` and `e2` and use them to try to statically compute
the result of the comparison (see `rangeCmp` function in this module).

Value range analysis consists in inferring the range of a CoreExpr.
    valueRange :: Platform -> CoreExpr -> Range
is the workhorse function doing this analysis in this module. It traverses a
CoreExpr recursively to infer a range as precise as possible. It takes into
account:

  - literals: a literal 'n' implies a range [n,n]
  - narrowing primops (e.g. Narrow8IntOp)
  - conversion primops (e.g. WordToIntOp)
  - addition and subtraction primops (e.g. Word32AddOp)
  - logical AND primops
  - variable unfoldings: for example, consider:
        case word8ToWord# x of y { ... case ltWord# y 256## { ... }}
    the value range analysis is triggered in a rewrite-rule for ltWord# on
    both `y` and `256##`. `y` is a variable so the analysis looks into its
    unfolding (here `word8ToWord# x`) to infer its range. It's obviously only
    possible when the variable has an unfolding. This unfolding, however, isn't
    required to be expandable as we're not expanding/inlining the unfolding,
    just computing its value range.

VRA1: Filtering unreachable alternatives based on range analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following example:

  case word8ToWord# x of
    123456# -> ...
    ...

By applying value range analysis to the scrutinee, GHC infers that the 123456#
alternative is unreachable because it's out of the range [0,255] of the
scrutinee, hence it will never match.

This filtering is done in GHC.Core.Opt.ConstantFold.caseRules2. See also T25718b


Limitation 1: no disjoint ranges
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The current implementation doesn't support disjoint ranges hence it can't
precisely track some ranges. In these cases it falls back to a wider range.
For example: range analysis for `word8ToInt8 x` where x is in [100,130] would
need a disjoint range union to be represented as [100,127] U [-128,-126] because
of the overflow (130 > 127). See `rangeCastFrom` function in this module for the
implementation.

Similarly, additions and subtractions may overflow/underflow. In these cases, we
would also need disjoint ranges to represent the resulting range. For example,
`x+1` where (x :: Word8) and x is in [100,255] would require a disjoint range
union: [0,0] U [101,255].
We use `rangeCastFrom` to handle these cases too: e.g. `x+1` (as described
above) is first computed to be in range [101,256] (ignoring the Word8 type), but
this range isn't in Word8's range [0,255], so conservatively we assume a range
[0,255] for `x+1` expression

Limitation 2: limited top-down range information in alternatives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The value range analysis for a variable currently consists in applying the
analysis to the variable's unfolding (when it is available). This approach
doesn't return the most precise range possible in some cases. For example
consider:

  case x ># 0# of
    1# -> {- Here we should know that x's range is > 0 -}
    0# -> {- ... and here <= 0 -}

In both alternatives `x`'s unfolding (if any) is the same, yet we should be able
to infer a different range for `x` as a consequence of the pattern matching on
the comparison operator application.

Possible future work: we could imagine extending the unfolding information to
include range information. Then a top-down pass could compute and store more
precise range information in cases like this one. Implementing this should
change the output of the T25718a test.

-}

data Comparison = Gt | Ge | Lt | Le deriving (Show)


-- | A range (minBound,maxBound)
--
-- Bounds may not be known.
data Range
  = MkRange {-# UNPACK #-} !(Maybe Integer) -- ^ Lower bound: Nothing means unbounded below
            {-# UNPACK #-} !(Maybe Integer) -- ^ Upper bound: Nothing means unbounded above
  deriving (Eq,Show)

pattern Range :: Maybe Integer -> Maybe Integer -> Range
pattern Range x y <- MkRange x y
  where
    Range x y = mkRange x y

{-# COMPLETE Range #-}

mkRange :: Maybe Integer -> Maybe Integer -> Range
mkRange = \cases
#if defined(DEBUG)
  -- check that the bounds of the range are well ordered.
  (Just x) (Just y) | x > y -> pprPanic "Invalid range" (ppr (x,y))
#endif
  x y -> MkRange x y


-- | Compute the intersection of two overlapping ranges.
--
-- If the two ranges don't overlap, result is undefined.
rangeIntersect :: Range -> Range -> Range
rangeIntersect (Range mi1 ma1) (Range mi2 ma2) = Range mi ma
  where
    merge op = \cases
      Nothing  Nothing  -> Nothing
      Nothing  v        -> v
      v        Nothing  -> v
      (Just a) (Just b) -> Just (op a b)
    mi = merge max mi1 mi2
    ma = merge min ma1 ma2

-- | Used for casts that share representation over a sub-range (e.g. [0,127] for
-- Word8# ([0,255]) and Int8# ([-128,127]))
--
-- If from_range isn't fully included into to_range, then we return to_range.
-- That's because for now we don't have a way to track disjoined ranges.
--    E.g. if we wanted to cast [-1,10] (Int8) into [0,255] (Word8), we would
--    need to represent a range union: [0,10] U [255,255] (Word8)
--
-- We also use this function to ensure that the result of an arithmetic
-- operation on ranges didn't overflow/underflow.
rangeCastFrom :: Range -> Range -> Range
rangeCastFrom to_range from_range = case from_range of
  Range (Just mi) (Just ma)
    | mi `inRange` to_range
    , ma `inRange` to_range
    -> from_range
  _ -> to_range

inRange :: Integer -> Range -> Bool
inRange x = \case
  Range Nothing   Nothing   -> True
  Range (Just mi) Nothing   -> x >= mi
  Range (Just mi) (Just ma) -> x >= mi && x <= ma
  Range Nothing   (Just ma) -> x <= ma

rangeSize :: Range -> Maybe Integer
rangeSize = \case
  Range (Just mi) (Just ma) -> Just (ma-mi+1)
  _                         -> Nothing

-- | Addition of two ranges
rangeAdd :: Range -> Range -> Range
rangeAdd (Range mi1 ma1) (Range mi2 ma2) = Range (add mi1 mi2) (add ma1 ma2)
  where
    add = \cases
      (Just v) (Just u) -> Just (v+u)
      _        _        -> Nothing

-- | Subtraction of two ranges
rangeSub :: Range -> Range -> Range
rangeSub (Range mi1 ma1) (Range mi2 ma2) = Range (sub mi1 ma2) (sub ma1 mi2)
  where
    sub = \cases
      (Just v) (Just u) -> Just (v-u)
      _        _        -> Nothing

-- | Logical AND of two ranges at the given type
{-# NOINLINE rangeAnd #-}
rangeAnd :: forall a. (FiniteBits a, Bounded a, Integral a) => Range -> Range -> Range
rangeAnd (Range mi1' ma1') (Range mi2' ma2') = final_range
  where
    -- convert the Integer range bounds into the given type with FiniteBits
    -- because we need to work on the actual representation, not on the Integer
    -- value. We also take the minBound/maxBound when a bound is missing.
    mi1, mi2, ma1, ma2 :: a
    mi1 = fromMaybe minBound (fromInteger <$> mi1')
    mi2 = fromMaybe minBound (fromInteger <$> mi2')
    ma1 = fromMaybe maxBound (fromInteger <$> ma1')
    ma2 = fromMaybe maxBound (fromInteger <$> ma2')

    -- we compute the minimal number of leading zeros for each range.
    -- Then we take the maximum of both values: this is the number of leading
    -- bits that will always be set to zero in the resulting range.
    clz1 = min (countLeadingZeros mi1) (countLeadingZeros ma1)
    clz2 = min (countLeadingZeros mi2) (countLeadingZeros ma2)
    clzr = max clz1 clz2

    -- we generate a mask for the bits that might be set to 1 in the resulting
    -- range and apply it to every bound. It may reorder the bounds: e.g.
    --    ([-1,1] :: Int8) .&. 0xF ==> [15,1] ==> [1,15]
    -- so we have to be careful when we reconstruct the final range
    mask :: a
    mask = if
      | clzr == finiteBitSize mi1 -> zeroBits
      | clzr == 0                 -> complement zeroBits
      | otherwise                 -> (1 `unsafeShiftL` (finiteBitSize mi1 - clzr)) - 1
              -- we can't use: complement zeroBits `shiftR` clzr
              -- because shiftR performs sign-extension for signed types
    mk_final_bound x = toInteger (x .&. mask)

    fmi1 = mk_final_bound mi1
    fmi2 = mk_final_bound mi2
    fma1 = mk_final_bound ma1
    fma2 = mk_final_bound ma2
    fmi = fmi1 `min` fmi2 `min` fma1 `min` fma2
    fma = fmi1 `max` fmi2 `max` fma1 `max` fma2
    final_range = Range (Just fmi) (Just fma)


noRange :: Range
noRange = Range Nothing Nothing

rangeOf :: forall a. (Bounded a, Integral a) => Range
rangeOf = Range (Just (toInteger (minBound :: a))) (Just (toInteger (maxBound :: a)))

rangeWord8, rangeWord16, rangeWord32, rangeWord64 :: Range
rangeWord8  = rangeOf @Word8
rangeWord16 = rangeOf @Word16
rangeWord32 = rangeOf @Word32
rangeWord64 = rangeOf @Word64

rangeInt8, rangeInt16, rangeInt32, rangeInt64 :: Range
rangeInt8  = rangeOf @Int8
rangeInt16 = rangeOf @Int16
rangeInt32 = rangeOf @Int32
rangeInt64 = rangeOf @Int64

rangeWord, rangeInt, rangeChar :: Platform -> Range
rangeWord p = case platformWordSize p of
  PW4 -> rangeWord32
  PW8 -> rangeWord64
rangeInt p = case platformWordSize p of
  PW4 -> rangeInt32
  PW8 -> rangeInt64

rangeChar = rangeInt -- a Char# is represented internally as an Int#

-- | Compare two ranges
--
-- See Note [Value range analysis]
rangeCmp :: Comparison -> Range -> Range -> Maybe Bool
rangeCmp cmp rx ry = case cmp of
  Gt -> rx `rangeGt` ry
  Ge -> rx `rangeGe` ry
  Le -> rx `rangeLe` ry
  Lt -> rx `rangeLt` ry

rangeGt :: Range -> Range -> Maybe Bool
rangeGt = \cases
  (Range (Just mi) _) (Range _ (Just ma)) | mi > ma  -> Just True
  (Range _ (Just ma)) (Range (Just mi) _) | ma <= mi -> Just False
  _ _ -> Nothing

rangeLt :: Range -> Range -> Maybe Bool
rangeLt = \cases
  (Range _ (Just ma)) (Range (Just mi) _) | ma < mi  -> Just True
  (Range (Just mi) _) (Range _ (Just ma)) | mi >= ma -> Just False
  _ _ -> Nothing

rangeGe :: Range -> Range -> Maybe Bool
rangeGe = \cases
  (Range (Just mi) _) (Range _ (Just ma)) | mi >= ma -> Just True
  (Range _ (Just ma)) (Range (Just mi) _) | ma < mi  -> Just False
  _ _ -> Nothing

rangeLe :: Range -> Range -> Maybe Bool
rangeLe = \cases
  (Range _ (Just ma)) (Range (Just mi) _) | ma <= mi -> Just True
  (Range (Just mi) _) (Range _ (Just ma)) | mi > ma  -> Just False
  _ _ -> Nothing


-- | Return the Integer range of an expression
valueRange :: Platform -> CoreExpr -> Range
valueRange platform = value_range 10
  where
    -- we use some fuel value to avoid recursing infinitely
    value_range :: Word -> CoreExpr -> Range
    value_range fuel expr
      | fuel == 0 = noRange
      | otherwise = case expr of
          Lit (LitNumber _ n) -> Range (Just n) (Just n)
          Lit (LitChar c) -> let n = fromIntegral (ord c) in Range (Just n) (Just n)
          Lit _ -> noRange
          Var v
            | Just ebody <- expandUnfolding_always (idUnfolding v)
            -> value_range (fuel-1) ebody
            | otherwise
            -> noRange
          PrimOpVar op `App` x ->
            let sub_range = value_range (fuel-1) x
            in case op of
              Word8ToWordOp  -> rangeWord8  `rangeIntersect` sub_range
              Word16ToWordOp -> rangeWord16 `rangeIntersect` sub_range
              Word32ToWordOp -> rangeWord32 `rangeIntersect` sub_range
              Word64ToWordOp -> rangeWord platform `rangeIntersect` sub_range
              WordToWord8Op  -> rangeWord8  `rangeIntersect` sub_range
              WordToWord16Op -> rangeWord16 `rangeIntersect` sub_range
              WordToWord32Op -> rangeWord32 `rangeIntersect` sub_range
              WordToWord64Op -> rangeWord platform `rangeIntersect` sub_range

              Word8ToInt8Op   -> rangeInt8  `rangeCastFrom` (rangeWord8  `rangeIntersect` sub_range)
              Word16ToInt16Op -> rangeInt16 `rangeCastFrom` (rangeWord16 `rangeIntersect` sub_range)
              Word32ToInt32Op -> rangeInt32 `rangeCastFrom` (rangeWord32 `rangeIntersect` sub_range)
              Word64ToInt64Op -> rangeInt64 `rangeCastFrom` (rangeWord64 `rangeIntersect` sub_range)
              WordToIntOp     -> rangeInt platform `rangeCastFrom` (rangeWord platform  `rangeIntersect` sub_range)

              Int8ToWord8Op   -> rangeWord8  `rangeCastFrom` (rangeInt8  `rangeIntersect` sub_range)
              Int16ToWord16Op -> rangeWord16 `rangeCastFrom` (rangeInt16 `rangeIntersect` sub_range)
              Int32ToWord32Op -> rangeWord32 `rangeCastFrom` (rangeInt32 `rangeIntersect` sub_range)
              Int64ToWord64Op -> rangeWord64 `rangeCastFrom` (rangeInt64 `rangeIntersect` sub_range)
              IntToWordOp     -> rangeWord platform `rangeCastFrom` (rangeInt platform `rangeIntersect` sub_range)

              Narrow8IntOp   -> rangeInt8 `rangeIntersect` sub_range
              Narrow16IntOp  -> rangeInt16 `rangeIntersect` sub_range
              Narrow32IntOp  -> rangeInt32 `rangeIntersect` sub_range
              Narrow8WordOp  -> rangeWord8 `rangeIntersect` sub_range
              Narrow16WordOp -> rangeWord16 `rangeIntersect` sub_range
              Narrow32WordOp -> rangeWord32 `rangeIntersect` sub_range

              OrdOp          -> sub_range
              ChrOp          -> sub_range

              _              -> noRange

          PrimOpVar op `App` x `App` y ->
            let range_x = value_range (fuel-1) x
                range_y = value_range (fuel-1) y
            in case op of
              Word8AddOp  -> rangeWord8 `rangeCastFrom` rangeAdd range_x range_y
              Word16AddOp -> rangeWord16 `rangeCastFrom` rangeAdd range_x range_y
              Word32AddOp -> rangeWord32 `rangeCastFrom` rangeAdd range_x range_y
              Word64AddOp -> rangeWord64 `rangeCastFrom` rangeAdd range_x range_y
              WordAddOp   -> rangeWord platform `rangeCastFrom` rangeAdd range_x range_y
              Int8AddOp   -> rangeInt8 `rangeCastFrom` rangeAdd range_x range_y
              Int16AddOp  -> rangeInt16 `rangeCastFrom` rangeAdd range_x range_y
              Int32AddOp  -> rangeInt32 `rangeCastFrom` rangeAdd range_x range_y
              Int64AddOp  -> rangeInt64 `rangeCastFrom` rangeAdd range_x range_y
              IntAddOp    -> rangeInt platform `rangeCastFrom` rangeAdd range_x range_y

              Word8SubOp  -> rangeWord8 `rangeCastFrom` rangeSub range_x range_y
              Word16SubOp -> rangeWord16 `rangeCastFrom` rangeSub range_x range_y
              Word32SubOp -> rangeWord32 `rangeCastFrom` rangeSub range_x range_y
              Word64SubOp -> rangeWord64 `rangeCastFrom` rangeSub range_x range_y
              WordSubOp   -> rangeWord platform `rangeCastFrom` rangeSub range_x range_y
              Int8SubOp   -> rangeInt8 `rangeCastFrom` rangeSub range_x range_y
              Int16SubOp  -> rangeInt16 `rangeCastFrom` rangeSub range_x range_y
              Int32SubOp  -> rangeInt32 `rangeCastFrom` rangeSub range_x range_y
              Int64SubOp  -> rangeInt64 `rangeCastFrom` rangeSub range_x range_y
              IntSubOp    -> rangeInt platform `rangeCastFrom` rangeSub range_x range_y

              IntAndOp -> case platformWordSize platform of
                PW4 -> rangeAnd @Int32 range_x range_y
                PW8 -> rangeAnd @Int64 range_x range_y
              WordAndOp -> case platformWordSize platform of
                PW4 -> rangeAnd @Word32 range_x range_y
                PW8 -> rangeAnd @Word64 range_x range_y
              Word8AndOp  -> rangeAnd @Word8  range_x range_y
              Word16AndOp -> rangeAnd @Word16 range_x range_y
              Word32AndOp -> rangeAnd @Word32 range_x range_y
              Word64AndOp -> rangeAnd @Word64 range_x range_y

              -- TODO: shifts, or, clz, ctz, negate...
              _ -> noRange

          App {}      -> noRange
          Lam {}      -> noRange
          Let {}      -> noRange
          Case {}     -> noRange
          Cast e _    -> value_range fuel e
          Tick _ e    -> value_range fuel e
          Type {}     -> noRange
          Coercion {} -> noRange

-- | Match a primop
pattern PrimOpVar:: PrimOp -> Arg CoreBndr
pattern PrimOpVar op <- Var (isPrimOpId_maybe -> Just op) where
   PrimOpVar op = Var (primOpId op)

