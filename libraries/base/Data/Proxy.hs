{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Proxy
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Definition of a Proxy type (poly-kinded in GHC)
--
-- @since 4.7.0.0
-----------------------------------------------------------------------------

module Data.Proxy
  (
        Proxy(..), asProxyTypeOf
      , KProxy(..)
  ) where

import GHC.Base
import GHC.Show
import GHC.Read
import GHC.Enum
import GHC.Arr

-- $setup
-- >>> import Data.Void
-- >>> import Prelude

-- | 'Proxy' is a type that holds no data, but has a phantom parameter of
-- arbitrary type (or even kind). Its use is to provide type information, even
-- though there is no value available of that type (or it may be too costly to
-- create one).
--
-- Historically, @'Proxy' :: 'Proxy' a@ is a safer alternative to the
-- @'undefined' :: a@ idiom.
--
-- >>> Proxy :: Proxy (Void, Int -> Int)
-- Proxy
--
-- Proxy can even hold types of higher kinds,
--
-- >>> Proxy :: Proxy Either
-- Proxy
--
-- >>> Proxy :: Proxy Functor
-- Proxy
--
-- >>> Proxy :: Proxy complicatedStructure
-- Proxy
data Proxy t = Proxy deriving ( Bounded -- ^ @since 4.7.0.0
                              , Read    -- ^ @since 4.7.0.0
                              )

-- | A concrete, promotable proxy type, for use at the kind level.
-- There are no instances for this because it is intended at the kind level only
data KProxy (t :: Type) = KProxy

-- It's common to use (undefined :: Proxy t) and (Proxy :: Proxy t)
-- interchangeably, so all of these instances are hand-written to be
-- lazy in Proxy arguments.

-- | @since 4.7.0.0
instance Eq (Proxy s) where
  _ == _ = True

-- | @since 4.7.0.0
instance Ord (Proxy s) where
  compare _ _ = EQ

-- | @since 4.7.0.0
instance Show (Proxy s) where
  showsPrec _ _ = showString "Proxy"

-- | @since 4.7.0.0
instance Enum (Proxy s) where
    succ _               = errorWithoutStackTrace "Proxy.succ"
    pred _               = errorWithoutStackTrace "Proxy.pred"
    fromEnum _           = 0
    toEnum 0             = Proxy
    toEnum _             = errorWithoutStackTrace "Proxy.toEnum: 0 expected"
    enumFrom _           = [Proxy]
    enumFromThen _ _     = [Proxy]
    enumFromThenTo _ _ _ = [Proxy]
    enumFromTo _ _       = [Proxy]

-- | @since 4.7.0.0
instance Ix (Proxy s) where
    range _           = [Proxy]
    index _ _         = 0
    inRange _ _       = True
    rangeSize _       = 1
    unsafeIndex _ _   = 0
    unsafeRangeSize _ = 1

-- | @since 4.9.0.0
instance Semigroup (Proxy s) where
    _ <> _ = Proxy
    sconcat _ = Proxy
    stimes _ _ = Proxy

-- | @since 4.7.0.0
instance Monoid (Proxy s) where
    mempty = Proxy
    mconcat _ = Proxy

-- | @since 4.7.0.0
instance Functor Proxy where
    fmap _ _ = Proxy
    {-# INLINE fmap #-}

-- | @since 4.7.0.0
instance Applicative Proxy where
    pure _ = Proxy
    {-# INLINE pure #-}
    _ <*> _ = Proxy
    {-# INLINE (<*>) #-}

-- | @since 4.9.0.0
instance Alternative Proxy where
    empty = Proxy
    {-# INLINE empty #-}
    _ <|> _ = Proxy
    {-# INLINE (<|>) #-}

-- | @since 4.7.0.0
instance Monad Proxy where
    _ >>= _ = Proxy
    {-# INLINE (>>=) #-}

-- | @since 4.9.0.0
instance MonadPlus Proxy

-- | 'asProxyTypeOf' is a type-restricted version of 'const'.
-- It is usually used as an infix operator, and its typing forces its first
-- argument (which is usually overloaded) to have the same type as the tag
-- of the second.
--
-- >>> import Data.Word
-- >>> :type asProxyTypeOf 123 (Proxy :: Proxy Word8)
-- asProxyTypeOf 123 (Proxy :: Proxy Word8) :: Word8
--
-- Note the lower-case @proxy@ in the definition. This allows any type
-- constructor with just one argument to be passed to the function, for example
-- we could also write
--
-- >>> import Data.Word
-- >>> :type asProxyTypeOf 123 (Just (undefined :: Word8))
-- asProxyTypeOf 123 (Just (undefined :: Word8)) :: Word8
asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}

