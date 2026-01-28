{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- |
--
-- Module      :  Data.Ord.Down
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The @Down@ datatype
--

module Data.Ord.Down
    ( Down(..)
    ) where

import GHC.Base
    ( otherwise, ($), coerce, Eq((==)), Monad((>>=)), Functor(fmap)
    , Ord(..), Applicative(pure, (<*>)), Semigroup, Monoid, IO(IO)
    , (.), liftM2 )
import GHC.Internal.Control.Monad.Zip ( MonadZip(mzipWith) )
import GHC.Internal.Data.Bits ( Bits, FiniteBits, complement )
import GHC.Internal.Data.Data ( Data )
import GHC.Internal.Data.Foldable ( Foldable )
import GHC.Internal.Data.Traversable ( Traversable )
import GHC.Internal.Enum ( Bounded(..), Enum(..) )
import GHC.Internal.Float ( Floating, RealFloat )
import GHC.Internal.Foreign.Storable ( Storable )
import GHC.Internal.Generics ( Generic, Generic1 )
import GHC.Internal.Ix ( Ix )
import GHC.Internal.Num ( Num )
import GHC.Internal.Read ( Read(readsPrec), lex, readParen )
import GHC.Internal.Real ( Fractional, Real, RealFrac )
import GHC.Internal.Show ( Show(showsPrec), showParen, showString )

-- $setup
-- >>> import Prelude

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
      , Generic    -- ^ @since base-4.12.0.0
      )

-- | @since base-4.12.0.0
deriving instance Generic1 Down

-- | @since base-4.12.0.0
deriving instance Foldable Down

-- | @since base-4.12.0.0
deriving instance Traversable Down

-- | @since 4.12.0.0
instance MonadZip Down where
    mzipWith = liftM2

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

-- | @since base-4.12.0.0
deriving instance Data a => Data (Down a)
