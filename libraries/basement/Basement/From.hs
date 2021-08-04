{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Basement.From
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- Flexible Type convertion
--
-- From is multi parameter type class that allow converting
-- from a to b.
--
-- Only type that are valid to convert to another type
-- should be From instance; otherwise TryFrom should be used.
--
-- Into (resp TryInto) allows the contrary instances to be able
-- to specify the destination type before the source. This is
-- practical with TypeApplication
module Basement.From
    ( From(..)
    , Into
    , TryFrom(..)
    , TryInto
    , into
    , tryInto
    ) where

import           Basement.Compat.Base

-- basic instances
import           GHC.Types
import           GHC.Prim
import           GHC.Int
import           GHC.Word
import           Basement.Numerical.Number
import           Basement.Numerical.Conversion
import qualified Basement.Block as Block
import qualified Basement.BoxedArray as BoxArray
import           Basement.Cast (cast)
import qualified Basement.UArray as UArray
import qualified Basement.String as String
import qualified Basement.Types.AsciiString as AsciiString
import           Basement.Types.Word128 (Word128(..))
import           Basement.Types.Word256 (Word256(..))
import qualified Basement.Types.Word128 as Word128
import qualified Basement.Types.Word256 as Word256
import           Basement.These
import           Basement.PrimType (PrimType, PrimSize)
import           Basement.Types.OffsetSize
import           Basement.Compat.Natural
import qualified Prelude (fromIntegral)

-- nat instances
#if __GLASGOW_HASKELL__ >= 800
import           Basement.Nat
import qualified Basement.Sized.Block as BlockN
import           Basement.Bounded
#endif

-- | Class of things that can be converted from a to b.
--
-- In a valid instance, the source should be always representable by the destination,
-- otherwise the instance should be using 'TryFrom'
class From a b where
    from :: a -> b

type Into b a = From a b

-- | Same as from but reverse the type variable so that the destination type can be specified first
--
-- e.g. converting:
--
-- from @_ @Word (10 :: Int)
--
-- into @Word (10 :: Int)
--
into :: Into b a => a -> b
into = from

-- | Class of things that can mostly be converted from a to b, but with possible error cases.
class TryFrom a b where
    tryFrom :: a -> Maybe b

type TryInto b a = TryFrom a b

-- | same as tryFrom but reversed
tryInto :: TryInto b a => a -> Maybe b
tryInto = tryFrom

instance From a a where
    from = id

instance IsNatural n => From n Natural where
    from = toNatural
instance IsIntegral n => From n Integer where
    from = toInteger

instance From Int8 Int16 where
    from (I8# i) = I16# i
instance From Int8 Int32 where
    from (I8# i) = I32# i
instance From Int8 Int64 where
    from (I8# i) = intToInt64 (I# i)
instance From Int8 Int where
    from (I8# i) = I# i

instance From Int16 Int32 where
    from (I16# i) = I32# i
instance From Int16 Int64 where
    from (I16# i) = intToInt64 (I# i)
instance From Int16 Int where
    from (I16# i) = I# i

instance From Int32 Int64 where
    from (I32# i) = intToInt64 (I# i)
instance From Int32 Int where
    from (I32# i) = I# i

instance From Int Int64 where
    from = intToInt64

instance From Word8 Word16 where
    from (W8# i) = W16# i
instance From Word8 Word32 where
    from (W8# i) = W32# i
instance From Word8 Word64 where
    from (W8# i) = wordToWord64 (W# i)
instance From Word8 Word128 where
    from (W8# i) = Word128 0 (wordToWord64 $ W# i)
instance From Word8 Word256 where
    from (W8# i) = Word256 0 0 0 (wordToWord64 $ W# i)
instance From Word8 Word where
    from (W8# i) = W# i
instance From Word8 Int16 where
    from (W8# w) = I16# (word2Int# w)
instance From Word8 Int32 where
    from (W8# w) = I32# (word2Int# w)
instance From Word8 Int64 where
    from (W8# w) = intToInt64 (I# (word2Int# w))
instance From Word8 Int where
    from (W8# w) = I# (word2Int# w)

instance From Word16 Word32 where
    from (W16# i) = W32# i
instance From Word16 Word64 where
    from (W16# i) = wordToWord64 (W# i)
instance From Word16 Word128 where
    from (W16# i) = Word128 0 (wordToWord64 $ W# i)
instance From Word16 Word256 where
    from (W16# i) = Word256 0 0 0 (wordToWord64 $ W# i)
instance From Word16 Word where
    from (W16# i) = W# i
instance From Word16 Int32 where
    from (W16# w) = I32# (word2Int# w)
instance From Word16 Int64 where
    from (W16# w) = intToInt64 (I# (word2Int# w))
instance From Word16 Int where
    from (W16# w) = I# (word2Int# w)

instance From Word32 Word64 where
    from (W32# i) = wordToWord64 (W# i)
instance From Word32 Word128 where
    from (W32# i) = Word128 0 (wordToWord64 $ W# i)
instance From Word32 Word256 where
    from (W32# i) = Word256 0 0 0 (wordToWord64 $ W# i)
instance From Word32 Word where
    from (W32# i) = W# i
instance From Word32 Int64 where
    from (W32# w) = intToInt64 (I# (word2Int# w))
instance From Word32 Int where
    from (W32# w) = I# (word2Int# w)

instance From Word64 Word128 where
    from w = Word128 0 w
instance From Word64 Word256 where
    from w = Word256 0 0 0 w

instance From Word Word64 where
    from = wordToWord64

-- Simple prelude types
instance From (Maybe a) (Either () a) where
    from (Just x) = Right x
    from Nothing  = Left ()

-- basic basement types
instance From (CountOf ty) Int where
    from (CountOf n) = n
instance From (CountOf ty) Word where
    -- here it is ok to cast the underlying `Int` held by `CountOf` to a `Word`
    -- as the `Int` should never hold a negative value.
    from (CountOf n) = cast n
instance From Word (Offset ty) where
    from w = Offset (cast w)
instance TryFrom Int (Offset ty) where
    tryFrom i
        | i < 0     = Nothing
        | otherwise = Just (Offset i)
instance TryFrom Int (CountOf ty) where
    tryFrom i
        | i < 0     = Nothing
        | otherwise = Just (CountOf i)
instance From Word (CountOf ty) where
    from w = CountOf (cast w)

instance From (Either a b) (These a b) where
    from (Left a) = This a
    from (Right b) = That b

instance From Word128 Word256 where
    from (Word128 a b) = Word256 0 0 a b

-- basement instances

-- uarrays
instance PrimType ty => From (Block.Block ty) (UArray.UArray ty) where
    from = UArray.fromBlock
instance PrimType ty => From (BoxArray.Array ty) (UArray.UArray ty) where
    from = BoxArray.mapToUnboxed id

-- blocks
instance PrimType ty => From (UArray.UArray ty) (Block.Block ty) where
    from = UArray.toBlock
instance PrimType ty => From (BoxArray.Array ty) (Block.Block ty) where
    from = UArray.toBlock . BoxArray.mapToUnboxed id

-- boxed array
instance PrimType ty => From (UArray.UArray ty) (BoxArray.Array ty) where
    from = BoxArray.mapFromUnboxed id


instance From String.String (UArray.UArray Word8) where
    from = String.toBytes String.UTF8

instance From AsciiString.AsciiString String.String where
    from = String.fromBytesUnsafe . UArray.unsafeRecast . AsciiString.toBytes
instance From AsciiString.AsciiString (UArray.UArray Word8) where
    from = UArray.unsafeRecast . AsciiString.toBytes

instance TryFrom (UArray.UArray Word8) String.String where
    tryFrom arr = case String.fromBytes String.UTF8 arr of
                    (s, Nothing, _) -> Just s
                    (_, Just _, _)  -> Nothing

#if __GLASGOW_HASKELL__ >= 800
instance From (BlockN.BlockN n ty) (Block.Block ty) where
    from = BlockN.toBlock
instance (PrimType a, PrimType b, KnownNat n, KnownNat m, ((PrimSize b) Basement.Nat.* m) ~ ((PrimSize a) Basement.Nat.* n))
      => From (BlockN.BlockN n a) (BlockN.BlockN m b) where
    from = BlockN.cast
instance (NatWithinBound Int n, PrimType ty) => From (BlockN.BlockN n ty) (UArray.UArray ty) where
    from = UArray.fromBlock . BlockN.toBlock
instance (NatWithinBound Int n, PrimType ty) => From (BlockN.BlockN n ty) (BoxArray.Array ty) where
    from = BoxArray.mapFromUnboxed id . UArray.fromBlock . BlockN.toBlock

instance (NatWithinBound (CountOf ty) n, KnownNat n, PrimType ty)
      => TryFrom (Block.Block ty) (BlockN.BlockN n ty) where
    tryFrom = BlockN.toBlockN
instance (NatWithinBound (CountOf ty) n, KnownNat n, PrimType ty)
      => TryFrom (UArray.UArray ty) (BlockN.BlockN n ty) where
    tryFrom = BlockN.toBlockN . UArray.toBlock
instance (NatWithinBound (CountOf ty) n, KnownNat n, PrimType ty)
      => TryFrom (BoxArray.Array ty) (BlockN.BlockN n ty) where
    tryFrom = BlockN.toBlockN . UArray.toBlock . BoxArray.mapToUnboxed id

instance (KnownNat n, NatWithinBound Word8 n) => From (Zn64 n) Word8 where
    from = narrow . unZn64 where narrow (W64# w) = W8# (narrow8Word# (word64ToWord# w))
instance (KnownNat n, NatWithinBound Word16 n) => From (Zn64 n) Word16 where
    from = narrow . unZn64 where narrow (W64# w) = W16# (narrow16Word# (word64ToWord# w))
instance (KnownNat n, NatWithinBound Word32 n) => From (Zn64 n) Word32 where
    from = narrow . unZn64 where narrow (W64# w) = W32# (narrow32Word# (word64ToWord# w))
instance From (Zn64 n) Word64 where
    from = unZn64
instance From (Zn64 n) Word128 where
    from = from . unZn64
instance From (Zn64 n) Word256 where
    from = from . unZn64

instance (KnownNat n, NatWithinBound Word8 n) => From (Zn n) Word8 where
    from = narrow . naturalToWord64 . unZn where narrow (W64# w) = W8# (narrow8Word# (word64ToWord# w))
instance (KnownNat n, NatWithinBound Word16 n) => From (Zn n) Word16 where
    from = narrow . naturalToWord64 . unZn where narrow (W64# w) = W16# (narrow16Word# (word64ToWord# w))
instance (KnownNat n, NatWithinBound Word32 n) => From (Zn n) Word32 where
    from = narrow . naturalToWord64 . unZn where narrow (W64# w) = W32# (narrow32Word# (word64ToWord# w))
instance (KnownNat n, NatWithinBound Word64 n) => From (Zn n) Word64 where
    from = naturalToWord64 . unZn
instance (KnownNat n, NatWithinBound Word128 n) => From (Zn n) Word128 where
    from = Word128.fromNatural . unZn
instance (KnownNat n, NatWithinBound Word256 n) => From (Zn n) Word256 where
    from = Word256.fromNatural . unZn

instance (KnownNat n, NatWithinBound Word64 n) => From (Zn n) (Zn64 n) where
    from = zn64 . naturalToWord64 . unZn
instance KnownNat n => From (Zn64 n) (Zn n) where
    from = zn . from . unZn64

naturalToWord64 :: Natural -> Word64
naturalToWord64 = Prelude.fromIntegral
#endif
