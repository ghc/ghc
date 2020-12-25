{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ord
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

module Data.Ord (
   Ord(..),
   Ordering(..),
   Down(..),
   comparing,
   clamp,
 ) where

import Data.Bits (Bits, FiniteBits)
import Foreign.Storable (Storable)
import GHC.Ix (Ix)
import GHC.Base
import GHC.Enum (Bounded(..))
import GHC.Float (Floating, RealFloat)
import GHC.Num
import GHC.Read
import GHC.Real (Fractional, Real, RealFrac)
import GHC.Show

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
-- Function for ensursing the value @a@ is within the inclusive bounds given by
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
-- @since 4.6.0.0
newtype Down a = Down
    { getDown :: a -- ^ @since 4.14.0.0
    }
    deriving
      ( Eq        -- ^ @since 4.6.0.0
      , Num       -- ^ @since 4.11.0.0
      , Semigroup -- ^ @since 4.11.0.0
      , Monoid    -- ^ @since 4.11.0.0
      , Bits       -- ^ @since 4.14.0.0
      , FiniteBits -- ^ @since 4.14.0.0
      , Floating   -- ^ @since 4.14.0.0
      , Fractional -- ^ @since 4.14.0.0
      , Ix         -- ^ @since 4.14.0.0
      , Real       -- ^ @since 4.14.0.0
      , RealFrac   -- ^ @since 4.14.0.0
      , RealFloat  -- ^ @since 4.14.0.0
      , Storable   -- ^ @since 4.14.0.0
      )

-- | This instance would be equivalent to the derived instances of the
-- 'Down' newtype if the 'getDown' field were removed
--
-- @since 4.7.0.0
instance (Read a) => Read (Down a) where
    readsPrec d = readParen (d > 10) $ \ r ->
        [(Down x,t) | ("Down",s) <- lex r, (x,t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Down' newtype if the 'getDown' field were removed
--
-- @since 4.7.0.0
instance (Show a) => Show (Down a) where
    showsPrec d (Down x) = showParen (d > 10) $
        showString "Down " . showsPrec 11 x

-- | @since 4.6.0.0
instance Ord a => Ord (Down a) where
    compare (Down x) (Down y) = y `compare` x

-- | Swaps @'minBound'@ and @'maxBound'@ of the underlying type.
--
-- @since 4.14.0.0
instance Bounded a => Bounded (Down a) where
    minBound = Down maxBound
    maxBound = Down minBound

-- | @since 4.11.0.0
instance Functor Down where
    fmap = coerce

-- | @since 4.11.0.0
instance Applicative Down where
    pure = Down
    (<*>) = coerce

-- | @since 4.11.0.0
instance Monad Down where
    Down a >>= k = k a
