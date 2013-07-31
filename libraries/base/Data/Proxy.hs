{-# LANGUAGE NoImplicitPrelude, Trustworthy #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE PolyKinds #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Proxy
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Definition of a Proxy type (poly-kinded in GHC)
--
-----------------------------------------------------------------------------

module Data.Proxy
  (
        Proxy(..), asProxyTypeOf
#ifdef __GLASGOW_HASKELL__
      , KProxy(..)
#endif
  ) where

import Data.Monoid

import GHC.Base
import GHC.Show
import GHC.Read
import GHC.Enum
import GHC.Arr

-- | A concrete, poly-kinded proxy type
data Proxy t = Proxy

#ifdef __GLASGOW_HASKELL__
-- | A concrete, promotable proxy type, for use at the kind level
-- There are no instances for this because it is intended at the kind level only
data KProxy (t :: *) = KProxy
#endif

instance Eq (Proxy s) where
  _ == _ = True

instance Ord (Proxy s) where
  compare _ _ = EQ

instance Show (Proxy s) where
  showsPrec _ _ = showString "Proxy"

instance Read (Proxy s) where
  readsPrec d = readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])

instance Enum (Proxy s) where
    succ _               = error "Proxy.succ"
    pred _               = error "Proxy.pred"
    fromEnum _           = 0
    toEnum 0             = Proxy
    toEnum _             = error "Proxy.toEnum: 0 expected"
    enumFrom _           = [Proxy]
    enumFromThen _ _     = [Proxy]
    enumFromThenTo _ _ _ = [Proxy]
    enumFromTo _ _       = [Proxy]

instance Ix (Proxy s) where
    range _           = [Proxy]
    index _ _         = 0
    inRange _ _       = True
    rangeSize _       = 1
#ifdef __GLASGOW_HASKELL__
    unsafeIndex _ _   = 0
    unsafeRangeSize _ = 1
#endif

instance Bounded (Proxy s) where
    minBound = Proxy
    maxBound = Proxy

instance Functor Proxy where
    fmap _ _ = Proxy
    {-# INLINE fmap #-}

instance Monoid (Proxy s) where
    mempty = Proxy
    {-# INLINE mempty #-}
    mappend _ _ = Proxy
    {-# INLINE mappend #-}
    mconcat _ = Proxy
    {-# INLINE mconcat #-}

instance Monad Proxy where
    return _ = Proxy
    {-# INLINE return #-}
    _ >>= _ = Proxy
    {-# INLINE (>>=) #-}

-- | 'asProxyTypeOf' is a type-restricted version of 'const'.
-- It is usually used as an infix operator, and its typing forces its first
-- argument (which is usually overloaded) to have the same type as the tag
-- of the second.
asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}