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
 ) where

import GHC.Base
import GHC.Show
import GHC.Read
import GHC.Num

-- | 
-- > comparing p x y = compare (p x) (p y)
--
-- Useful combinator for use in conjunction with the @xxxBy@ family
-- of functions from "Data.List", for example:
--
-- >   ... sortBy (comparing fst) ...
comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

-- | The 'Down' type allows you to reverse sort order conveniently.  A value of type
-- @'Down' a@ contains a value of type @a@ (represented as @'Down' a@).
-- If @a@ has an @'Ord'@ instance associated with it then comparing two
-- values thus wrapped will give you the opposite of their normal sort order.
-- This is particularly useful when sorting in generalised list comprehensions,
-- as in: @then sortWith by 'Down' x@
--
-- @since 4.6.0.0
newtype Down a = Down a
    deriving
      ( Eq        -- ^ @since 4.6.0.0
      , Show      -- ^ @since 4.7.0.0
      , Read      -- ^ @since 4.7.0.0
      , Num       -- ^ @since 4.11.0.0
      , Semigroup -- ^ @since 4.11.0.0
      , Monoid    -- ^ @since 4.11.0.0
      )

-- | @since 4.6.0.0
instance Ord a => Ord (Down a) where
    compare (Down x) (Down y) = y `compare` x

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
