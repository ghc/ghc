{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Types.Literal.Floating
  ( LitFloating
  , LitFloatingType(..)
  , ConstantFoldingPrecision(..)

  , floatToLitFloating, doubleToLitFloating, rationalToLitFloating
  , unsafeLitFloatingToRational, litFloatingToHostFloat, litFloatingToHostDouble
  , pprLitFloating

  -- ** Arithmetic on floating-point literals
  , litRationalToFloatOp
  , litFloatingUnaryOp, litFloatingBinaryOp, litFloatingTernaryOp
  , litFloatingComparisonOp
  , truncateLitFloating, decodeLitFloating
  , encodeLitFloat, encodeLitDouble

  , isZeroLF, isPositiveZeroLF, isPositiveLF, isOneLF, isFiniteLF
  , isPositiveZero, litFloatingIsNonStandardNaN
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Utils.Binary
import GHC.Utils.Panic
import GHC.Utils.Outputable

import Control.DeepSeq ( NFData(..) )
import Data.Data ( Data )
import Data.Function
import GHC.Exts ( dataToTag#, isTrue#, (<#) )
import GHC.Float
import Numeric ( showHex )

-- For now, this assumes that Float/Double arithmetic on the target
-- machine is the same as that on the host machine. And that's OK.

-- | Represents a known @Float#@ or @Double#@ literal on the target machine.
data LitFloating
  = LitFloatingF !Float
  | LitFloatingD !Double
  | LitFloatingR !Rational
  deriving (Data, Show)

instance NFData LitFloating where
  rnf = \case
    LitFloatingF f -> rnf f
    LitFloatingD d -> rnf d
    LitFloatingR r -> rnf r

data LitFloatingType = LitFloat | LitDouble
  deriving stock (Data, Eq, Ord, Show)

instance NFData LitFloatingType where
  rnf !_ = ()

data ConstantFoldingPrecision
  = FloatPrecision
  | DoublePrecision
  | ExcessPrecision

litFloatingIsNonStandardNaN :: LitFloatingType -> LitFloating -> Bool
litFloatingIsNonStandardNaN ty lit =
  case ty of
    LitFloat -> isNaN f && w /= 0x7FC00000
      where f = litFloatingToHostFloat lit
            w = castFloatToWord32 f
    LitDouble -> isNaN f && w /= 0x7FF8000000000000
      where f = litFloatingToHostDouble lit
            w = castDoubleToWord64 f

pprLitFloating :: LitFloatingType -> LitFloating -> SDoc
pprLitFloating ty lit = getPprDebug $ \ dbg -> getPprStyle $ \ sty ->
  case ty of
    LitFloat ->
      if
        | isNaN f
        , w /= 0x7FC00000 -- not equal to the standard NaN
        -> case sty of

            -- For assembly data sections, print the corresponding unsigned integer literal.
            PprCode {}
              -> ppr w

            -- For the user (and without -debug-ppr), just print "NaN" regardless of sign/payload.
            PprUser {}
              | not dbg
              -> text "NaN"

            -- For debugging, print the NaN including its sign and payload.
            _ -> format_non_standard_NaN (testBit w 31) (testBit w 22) (w .&. 0x3FFFFF)
        | otherwise
        -> ppr f
        where
          f = litFloatingToHostFloat lit
          w = castFloatToWord32 f
    LitDouble ->
      if
        | isNaN f
        , w /= 0x7FF8000000000000 -- not equal to the standard NaN
        -> case sty of
            PprCode {}
              -> ppr w
            PprUser {}
              | not dbg
              -> text "NaN"
            _ -> format_non_standard_NaN (testBit w 63) (testBit w 51) (w .&. 0x7FFFFFFFFFFFF)
        | otherwise
        -> ppr f
        where
          f = litFloatingToHostDouble lit
          w = castDoubleToWord64 f

format_non_standard_NaN :: (Integral a, Show a) => Bool -> Bool -> a -> SDoc
format_non_standard_NaN neg is_quiet payload =
  hcat
    [ if neg then text "-" else empty
    , if is_quiet then text "qNaN" else text "sNaN"
    , if payload == 0
      then empty
      else parens (text "0x" <> text (showHex payload ""))
    ]

instance Binary LitFloating where
  put_ bh (LitFloatingF f) = putByte bh 0 >> put_ bh f
  put_ bh (LitFloatingD d) = putByte bh 1 >> put_ bh d
  put_ bh (LitFloatingR r) = putByte bh 2 >> put_ bh r
  get bh = do
    h <- getByte bh
    case  h  of
      0 -> LitFloatingF <$> get bh
      1 -> LitFloatingD <$> get bh
      2 -> LitFloatingR <$> get bh
      _ -> pprPanic "Binary:LitFloating" (int (fromIntegral h))

-- | This instance intentionally disagrees with the equality predicate
-- @(==)@ on @Float@ or @Double@ values: It is reflexive even on NaNs and
-- distinguishes between positive and negative zero.
-- Use 'litFloatingComparisonOp' if you need to match @Float@ or @Double@.
instance Eq LitFloating where
  v1 == v2 = v1 `compare` v2 == EQ

-- | The ordering represented by this instance is pretty arbitrary and
-- may change without notice.  Do not rely on its behavior!
instance Ord LitFloating where
  v1 `compare` v2 = case  (canonicalizeLF v1, canonicalizeLF v2)  of
    (LitFloatingF f1, LitFloatingF f2)
      -> (compare `on` castFloatToWord32 ) f1 f2
    (LitFloatingD d1, LitFloatingD d2)
      -> (compare `on` castDoubleToWord64) d1 d2
    (LitFloatingR r1, LitFloatingR r2) -> compare r1 r2
    (cv1, cv2) | isTrue# (dataToTag# cv1 <# dataToTag# cv2) -> LT
               | otherwise -> GT

-- | Canonicalize a 'LitFloating' to allow comparison.
--
--  - NaNs remain as they are (whether 'Float' or 'Double')
--  - infinities and negative zero are represented as 'Double'
--  - finite values (including subnormals) are represented as 'Rational'
canonicalizeLF :: LitFloating -> LitFloating
-- allow different representations of the same value to compare equal
canonicalizeLF v = case  v  of
  LitFloatingF f
    | isNaN f -> v -- (double2Float . float2Double) destroys NaN payloads
    | isInfinite f || isNegativeZero f -> LitFloatingD (float2Double f)
    | otherwise -> LitFloatingR (toRational f)
  LitFloatingD d
    | isNaN d || isInfinite d || isNegativeZero d -> v
    | otherwise -> LitFloatingR (toRational d)
  LitFloatingR _ -> v

floatToLitFloating :: Float -> LitFloating
floatToLitFloating = LitFloatingF
doubleToLitFloating :: Double -> LitFloating
doubleToLitFloating = LitFloatingD
rationalToLitFloating :: Rational -> LitFloating
rationalToLitFloating = LitFloatingR

litFloatingToHostFloat :: LitFloating -> Float
litFloatingToHostFloat v = case  v  of
  LitFloatingF f -> f
  LitFloatingD d -> double2Float d
  LitFloatingR r -> fromRational r

litFloatingToHostDouble :: LitFloating -> Double
litFloatingToHostDouble v = case  v  of
  LitFloatingF f -> float2Double f
  LitFloatingD d -> d
  LitFloatingR r -> fromRational r

-- | Attempts to convert a 'LitFloating' to a rational number.
--
-- Returns nonsense if its argument is a NaN or an infinity.
-- Equates @0.0@ with @-0.0@.
unsafeLitFloatingToRational :: LitFloating -> Rational
unsafeLitFloatingToRational v = case  v  of
  LitFloatingF f -> toRational f
  LitFloatingD d -> toRational d
  LitFloatingR r -> r

-- | Losslessly convert a 'LitFloating' to a 'Rational'.
--
-- Returns 'Nothing' precisely for 'NaN', infinities and negative zero.
litFloatingRational_maybe :: LitFloating -> Maybe Rational
litFloatingRational_maybe v
  | isFiniteLF v
  , not (isNegativeZeroLF v)
  = Just $ unsafeLitFloatingToRational v
  | otherwise
  = Nothing

isZeroLF :: LitFloating -> Bool
isZeroLF v = case  v  of
  LitFloatingF f -> f == 0
  LitFloatingD d -> d == 0
  LitFloatingR r -> r == 0

isNegativeZeroLF :: LitFloating -> Bool
isNegativeZeroLF v = case  v  of
  LitFloatingF f -> isNegativeZero f
  LitFloatingD d -> isNegativeZero d
  LitFloatingR _ -> False

isPositiveZero :: RealFloat a => a -> Bool
isPositiveZero x = x == 0 && not (isNegativeZero x)
{-# INLINEABLE isPositiveZero #-}

isPositiveZeroLF :: LitFloating -> Bool
isPositiveZeroLF v = case  v  of
  LitFloatingF f -> isPositiveZero f
  LitFloatingD d -> isPositiveZero d
  LitFloatingR r -> r == 0

isOneLF :: LitFloating -> Bool
isOneLF v = case  v  of
  LitFloatingF f -> f == 1
  LitFloatingD d -> d == 1
  LitFloatingR r -> r == 1

-- | Returns True if its argument represents a real number,
-- and False if its argument represents a NaN or an infinity.
isFiniteLF :: LitFloating -> Bool
isFiniteLF v = case v of
  LitFloatingF f -> not (isInfinite f || isNaN f)
  LitFloatingD d -> not (isInfinite d || isNaN d)
  LitFloatingR _ -> True

-- | Is this a positive floating-point value?
--
-- Handles negative zero, infinities and signed NaNs.
isPositiveLF :: LitFloating -> Bool
isPositiveLF v =
  case v of
    LitFloatingF f -> not $ testBit (castFloatToWord32  f) 31
    LitFloatingD d -> not $ testBit (castDoubleToWord64 d) 63
    LitFloatingR q -> q >= 0

-- Constant folding helpers for LitFloating

litRationalToFloatOp
  :: ConstantFoldingPrecision
  -> Rational -> LitFloating
litRationalToFloatOp prec rat = case  prec  of
  ExcessPrecision -> LitFloatingR rat
  FloatPrecision  -> LitFloatingF (fromRational rat)
  DoublePrecision -> LitFloatingD (fromRational rat)

litFloatingUnaryOp
  :: ConstantFoldingPrecision
  -> (forall t. Fractional t => t -> t)
  -> LitFloating -> LitFloating
litFloatingUnaryOp prec op lit = case  prec  of
  ExcessPrecision
    | Just r <- litFloatingRational_maybe lit -> LitFloatingR (op r)
    | otherwise -> dres
  FloatPrecision -> LitFloatingF (op (litFloatingToHostFloat  lit))
  DoublePrecision -> dres
  where  dres = LitFloatingD (op (litFloatingToHostDouble lit))

litFloatingBinaryOp
  :: ConstantFoldingPrecision
  -> (forall t. Fractional t => t -> t -> t)
  -> LitFloating -> LitFloating -> LitFloating
litFloatingBinaryOp prec op lit1 lit2 = case  prec  of
  ExcessPrecision
    | Just res <- (liftA2 op `on` litFloatingRational_maybe) lit1 lit2 -> LitFloatingR res
    | otherwise -> dres
  FloatPrecision
    -> LitFloatingF ((op `on` litFloatingToHostFloat)  lit1 lit2)
  DoublePrecision -> dres
  where  dres = LitFloatingD ((op `on` litFloatingToHostDouble) lit1 lit2)

litFloatingTernaryOp
  :: ConstantFoldingPrecision
  -> (forall t. Fractional t => t -> t -> t -> t)
  -> LitFloating -> LitFloating -> LitFloating -> LitFloating
litFloatingTernaryOp prec op lit1 lit2 lit3 = case  prec  of
  ExcessPrecision
    | Just l1' <- litFloatingRational_maybe lit1
    , Just l2' <- litFloatingRational_maybe lit2
    , Just l3' <- litFloatingRational_maybe lit3
    -> LitFloatingR (op l1' l2' l3')
    | otherwise
    -> dres
  FloatPrecision
    -> LitFloatingF $
         op (litFloatingToHostFloat lit1)
            (litFloatingToHostFloat lit2)
            (litFloatingToHostFloat lit3)
  DoublePrecision -> dres
  where
    dres =
      LitFloatingD $
         op (litFloatingToHostDouble lit1)
            (litFloatingToHostDouble lit2)
            (litFloatingToHostDouble lit3)

litFloatingComparisonOp
  :: ConstantFoldingPrecision -> (forall t. Ord t => t -> t -> res)
  -> LitFloating -> LitFloating -> res
litFloatingComparisonOp prec op lit1 lit2 = case  prec  of
  ExcessPrecision
    | Just res <- (liftA2 op `on` litFloatingRational_maybe) lit1 lit2 -> res
    | otherwise -> dres
  FloatPrecision -> (op `on` litFloatingToHostFloat)  lit1 lit2
  DoublePrecision -> dres
  where  dres = (op `on` litFloatingToHostDouble) lit1 lit2

truncateLitFloating :: ConstantFoldingPrecision -> LitFloating -> Integer
truncateLitFloating prec lit = case  prec  of
  ExcessPrecision
    | Just r <- litFloatingRational_maybe lit -> truncate r
    | otherwise -> dres
  FloatPrecision  -> truncate (litFloatingToHostFloat  lit)
  DoublePrecision -> dres
  where  dres = truncate (litFloatingToHostDouble lit)

decodeLitFloating :: LitFloatingType -> Platform -> LitFloating -> (Integer, Int)
decodeLitFloating LitFloat  _ lit = decodeFloat (litFloatingToHostFloat  lit)
decodeLitFloating LitDouble _ lit = decodeFloat (litFloatingToHostDouble lit)

encodeLitFloat, encodeLitDouble
  :: Integer -> Int -> LitFloating
encodeLitFloat  m e
  = LitFloatingF (encodeFloat @Float  m e)
encodeLitDouble m e
  = LitFloatingD (encodeFloat @Double m e)
