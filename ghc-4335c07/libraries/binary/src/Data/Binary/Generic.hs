{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, KindSignatures,
    ScopedTypeVariables, TypeOperators, TypeSynonymInstances #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Generic
-- Copyright   : Bryan O'Sullivan
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Bryan O'Sullivan <bos@serpentine.com>
-- Stability   : unstable
-- Portability : Only works with GHC 7.2 and newer
--
-- Instances for supporting GHC generics.
--
-----------------------------------------------------------------------------
module Data.Binary.Generic
    (
    ) where

import Control.Applicative
import Data.Binary.Class
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Word
import Data.Monoid ((<>))
import GHC.Generics
import Prelude -- Silence AMP warning.

-- Type without constructors
instance GBinaryPut V1 where
    gput _ = pure ()

instance GBinaryGet V1 where
    gget   = return undefined

-- Constructor without arguments
instance GBinaryPut U1 where
    gput U1 = pure ()

instance GBinaryGet U1 where
    gget    = return U1

-- Product: constructor with parameters
instance (GBinaryPut a, GBinaryPut b) => GBinaryPut (a :*: b) where
    gput (x :*: y) = gput x <> gput y

instance (GBinaryGet a, GBinaryGet b) => GBinaryGet (a :*: b) where
    gget = (:*:) <$> gget <*> gget

-- Metadata (constructor name, etc)
instance GBinaryPut a => GBinaryPut (M1 i c a) where
    gput = gput . unM1

instance GBinaryGet a => GBinaryGet (M1 i c a) where
    gget = M1 <$> gget

-- Constants, additional parameters, and rank-1 recursion
instance Binary a => GBinaryPut (K1 i a) where
    gput = put . unK1

instance Binary a => GBinaryGet (K1 i a) where
    gget = K1 <$> get

-- Borrowed from the cereal package.

-- The following GBinary instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define PUTSUM(WORD) GUARD(WORD) = putSum (0 :: WORD) (fromIntegral size)
#define GETSUM(WORD) GUARD(WORD) = (get :: Get WORD) >>= checkGetSum (fromIntegral size)

instance ( GSumPut  a, GSumPut  b
         , SumSize    a, SumSize    b) => GBinaryPut (a :+: b) where
    gput | PUTSUM(Word8) | PUTSUM(Word16) | PUTSUM(Word32) | PUTSUM(Word64)
         | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)

instance ( GSumGet  a, GSumGet  b
         , SumSize    a, SumSize    b) => GBinaryGet (a :+: b) where
    gget | GETSUM(Word8) | GETSUM(Word16) | GETSUM(Word32) | GETSUM(Word64)
         | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)

sizeError :: Show size => String -> size -> error
sizeError s size =
    error $ "Can't " ++ s ++ " a type with " ++ show size ++ " constructors"

------------------------------------------------------------------------

checkGetSum :: (Ord word, Num word, Bits word, GSumGet f)
            => word -> word -> Get (f a)
checkGetSum size code | code < size = getSum code size
                      | otherwise   = fail "Unknown encoding for constructor"
{-# INLINE checkGetSum #-}

class GSumGet f where
    getSum :: (Ord word, Num word, Bits word) => word -> word -> Get (f a)

class GSumPut f where
    putSum :: (Num w, Bits w, Binary w) => w -> w -> f a -> Put

instance (GSumGet a, GSumGet b) => GSumGet (a :+: b) where
    getSum !code !size | code < sizeL = L1 <$> getSum code           sizeL
                       | otherwise    = R1 <$> getSum (code - sizeL) sizeR
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance (GSumPut a, GSumPut b) => GSumPut (a :+: b) where
    putSum !code !size s = case s of
                             L1 x -> putSum code           sizeL x
                             R1 x -> putSum (code + sizeL) sizeR x
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance GBinaryGet a => GSumGet (C1 c a) where
    getSum _ _ = gget

instance GBinaryPut a => GSumPut (C1 c a) where
    putSum !code _ x = put code <> gput x

------------------------------------------------------------------------

class SumSize f where
    sumSize :: Tagged f Word64

newtype Tagged (s :: * -> *) b = Tagged {unTagged :: b}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) +
                       unTagged (sumSize :: Tagged b Word64)

instance SumSize (C1 c a) where
    sumSize = Tagged 1
