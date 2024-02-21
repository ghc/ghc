{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.Ord
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Orderings
--
-----------------------------------------------------------------------------

module GHC.Internal.Data.Ord (
   Ord(..),
   Ordering(..),
   Down(..),
   comparing,
   clamp,
 ) where

import GHC.Internal.Data.Bits (Bits, FiniteBits, complement)
import GHC.Internal.Foreign.Storable (Storable)
import GHC.Internal.Ix (Ix)
import GHC.Internal.Base
import GHC.Internal.Enum (Bounded(..), Enum(..))
import GHC.Internal.Float (Floating, RealFloat)
import GHC.Internal.Num
import GHC.Internal.Read
import GHC.Internal.Real (Fractional, Real, RealFrac)
import GHC.Internal.Show

-- $setup
-- >>> import Prelude

-- |
-- > comparing p x y = compare (p x) (p y)
--
-- Useful combinator for use in conjunction with the @xxxBy@ family
-- of functions from "Data.List", for example:
--
-- >   ... sortBy (comparing fst) ...
comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

-- |
-- > clamp (low, high) a = min high (max a low)
--
-- Function for ensuring the value @a@ is within the inclusive bounds given by
-- @low@ and @high@. If it is, @a@ is returned unchanged. The result
-- is otherwise @low@ if @a <= low@, or @high@ if @high <= a@.
--
-- When clamp is used at Double and Float, it has NaN propagating semantics in
-- its second argument. That is, @clamp (l,h) NaN = NaN@, but @clamp (NaN, NaN)
-- x = x@.
--
-- >>> clamp (0, 10) 2
-- 2
--
-- >>> clamp ('a', 'm') 'x'
-- 'm'
--
-- @since base-4.16.0.0
clamp :: (Ord a) => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)

-- | The 'Down' type allows you to reverse sort order conveniently.  A value of type
-- @'Down' a@ contains a value of type @a@ (represented as @'Down' a@).
--
-- If @a@ has an @'Ord'@ instance associated with it then comparing two
-- values thus wrapped will give you the opposite of their normal sort order.
-- This is particularly useful when sorting in generalised list comprehensions,
-- as in: @then sortWith by 'Down' x@.
--
-- >>> compare True False
-- GT
--
-- >>> compare (Down True) (Down False)
-- LT
--
-- If @a@ has a @'Bounded'@ instance then the wrapped instance also respects
-- the reversed ordering by exchanging the values of @'minBound'@ and
-- @'maxBound'@.
--
-- >>> minBound :: Int
-- -9223372036854775808
--
-- >>> minBound :: Down Int
-- Down 9223372036854775807
--
-- All other instances of @'Down' a@ behave as they do for @a@.
--
-- @since base-4.6.0.0
newtype Down a = Down
    { getDown :: a -- ^ @since base-4.14.0.0
    }
    deriving
      ( Eq        -- ^ @since base-4.6.0.0
      , Num       -- ^ @since base-4.11.0.0
      , Semigroup -- ^ @since base-4.11.0.0
      , Monoid    -- ^ @since base-4.11.0.0
      , Bits       -- ^ @since base-4.14.0.0
      , FiniteBits -- ^ @since base-4.14.0.0
      , Floating   -- ^ @since base-4.14.0.0
      , Fractional -- ^ @since base-4.14.0.0
      , Ix         -- ^ @since base-4.14.0.0
      , Real       -- ^ @since base-4.14.0.0
      , RealFrac   -- ^ @since base-4.14.0.0
      , RealFloat  -- ^ @since base-4.14.0.0
      , Storable   -- ^ @since base-4.14.0.0
      )

-- | This instance would be equivalent to the derived instances of the
-- 'Down' newtype if the 'getDown' field were removed
--
-- @since base-4.7.0.0
instance (Read a) => Read (Down a) where
    readsPrec d = readParen (d > 10) $ \ r ->
        [(Down x,t) | ("Down",s) <- lex r, (x,t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Down' newtype if the 'getDown' field were removed
--
-- @since base-4.7.0.0
instance (Show a) => Show (Down a) where
    showsPrec d (Down x) = showParen (d > 10) $
        showString "Down " . showsPrec 11 x

-- | @since base-4.6.0.0
instance Ord a => Ord (Down a) where
    compare (Down x) (Down y) = y `compare` x
    Down x < Down y = y < x
    Down x > Down y = y > x
    Down x <= Down y = y <= x
    Down x >= Down y = y >= x
    min (Down x) (Down y) = Down (max y x)
    max (Down x) (Down y) = Down (min y x)

-- | Swaps @'minBound'@ and @'maxBound'@ of the underlying type.
--
-- @since base-4.14.0.0
instance Bounded a => Bounded (Down a) where
    minBound = Down maxBound
    maxBound = Down minBound

-- | Swaps @'succ'@ and @'pred'@ of the underlying type.
--
-- @since base-4.18.0.0
instance (Enum a, Bounded a, Eq a) => Enum (Down a) where
    succ = fmap pred
    pred = fmap succ

    -- Here we use the fact that 'comparing (complement @Int)' behaves
    -- as an order-swapping `compare @Int`.
    fromEnum = complement . fromEnum . getDown
    toEnum = Down . toEnum . complement

    enumFrom (Down x)
        | x == minBound
        = [Down x] -- We can't rely on 'enumFromThen _ (pred @a minBound)` behaving nicely,
                   -- since 'enumFromThen _' might be strict and 'pred minBound' might throw
        | otherwise
        = coerce $ enumFromThen x (pred x)
    enumFromThen (Down x) (Down y) = coerce $ enumFromThen x y

-- | @since base-4.11.0.0
instance Functor Down where
    fmap = coerce

-- | @since base-4.11.0.0
instance Applicative Down where
    pure = Down
    (<*>) = coerce

-- | @since base-4.11.0.0
instance Monad Down where
    Down a >>= k = k a
