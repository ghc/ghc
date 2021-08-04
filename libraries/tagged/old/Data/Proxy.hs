{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Proxy
-- Copyright  : 2009-2013 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------
module Data.Proxy
    (
    -- * Proxy values
      Proxy(..)
    , asProxyTypeOf
    , KProxy(..)
    ) where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (MonadPlus(..))
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(..))
#endif
#ifdef MIN_VERSION_deepseq
import Control.DeepSeq (NFData(..))
#endif
#ifdef MIN_VERSION_transformers
import Data.Functor.Classes (Eq1(..), Ord1(..), Read1(..), Show1(..))
#endif
import Data.Traversable (Traversable(..))
import Data.Foldable (Foldable(..))
import Data.Ix (Ix(..))
import Data.Monoid
#ifdef __GLASGOW_HASKELL__
import GHC.Arr (unsafeIndex, unsafeRangeSize)
import Data.Data
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics hiding (Fixity(..))
#endif
#endif

#if __GLASGOW_HASKELL__ >= 707
deriving instance Typeable Proxy
#else
data Proxy s = Proxy
#if __GLASGOW_HASKELL__ >= 702
  deriving Generic

-- We have to implement the Generic1 instance manually due to an old
-- bug in GHC 7.6. This is mostly copied from the output of
--
-- deriving instance Generic1 Proxy
--
-- Compiled with -ddump-deriv on a more recent GHC.
instance Generic1 Proxy where
  type Rep1 Proxy = D1 ProxyMetaData (C1 ProxyMetaCons U1)
  from1 Proxy = M1 (M1 U1)
  to1 (M1 (M1 U1)) = Proxy

data ProxyMetaData
data ProxyMetaCons

instance Datatype ProxyMetaData where
  datatypeName _ = "Proxy"
  moduleName   _ = "Data.Proxy"

instance Constructor ProxyMetaCons where
  conName _ = "Proxy"
#endif
#endif

instance Eq (Proxy s) where
  _ == _ = True

instance Ord (Proxy s) where
  compare _ _ = EQ

instance Show (Proxy s) where
  showsPrec _ _ = showString "Proxy"

instance Read (Proxy s) where
  readsPrec _ = readParen False (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 707
instance Typeable1 Proxy where
  typeOf1 _ = mkTyConApp proxyTyCon []

proxyTyCon :: TyCon
#if __GLASGOW_HASKELL__ < 704
proxyTyCon = mkTyCon "Data.Proxy.Proxy"
#else
proxyTyCon = mkTyCon3 "tagged" "Data.Proxy" "Proxy"
#endif
{-# NOINLINE proxyTyCon #-}
#endif

instance Data s => Data (Proxy s) where
  gfoldl _ z _ = z Proxy
  toConstr _ = proxyConstr
  gunfold _ z c = case constrIndex c of
    1 -> z Proxy
    _ -> error "gunfold"
  dataTypeOf _ = proxyDataType
  dataCast1 f = gcast1 f

proxyConstr :: Constr
proxyConstr = mkConstr proxyDataType "Proxy" [] Prefix
{-# NOINLINE proxyConstr #-}

proxyDataType :: DataType
proxyDataType = mkDataType "Data.Proxy.Proxy" [proxyConstr]
{-# NOINLINE proxyDataType #-}
#endif

instance Enum (Proxy s) where
    succ _ = error "Proxy.succ"
    pred _ = error "Proxy.pred"
    fromEnum _ = 0
    toEnum 0 = Proxy
    toEnum _ = error "Proxy.toEnum: 0 expected"
    enumFrom _ = [Proxy]
    enumFromThen _ _ = [Proxy]
    enumFromThenTo _ _ _ = [Proxy]
    enumFromTo _ _ = [Proxy]

instance Ix (Proxy s) where
    range _ = [Proxy]
    index _ _ = 0
    inRange _ _ = True
    rangeSize _ = 1
#ifdef __GLASGOW_HASKELL__
    unsafeIndex _ _ = 0
    unsafeRangeSize _ = 1
#endif

instance Bounded (Proxy s) where
    minBound = Proxy
    maxBound = Proxy

#ifdef MIN_VERSION_deepseq
instance NFData (Proxy s) where
    rnf Proxy = ()
#endif

#ifdef MIN_VERSION_transformers
# if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
instance Eq1 Proxy where
  eq1 = (==)

instance Ord1 Proxy where
  compare1 = compare

instance Read1 Proxy where
  readsPrec1 = readsPrec

instance Show1 Proxy where
  showsPrec1 = showsPrec
# else
instance Eq1 Proxy where
  liftEq _ _ _ = True

instance Ord1 Proxy where
  liftCompare _ _ _ = EQ

instance Show1 Proxy where
  liftShowsPrec _ _ _ _ = showString "Proxy"

instance Read1 Proxy where
  liftReadsPrec _ _ _ =
    readParen False (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])
# endif
#endif

instance Functor Proxy where
    fmap _ _ = Proxy
    {-# INLINE fmap #-}

instance Applicative Proxy where
    pure _ = Proxy
    {-# INLINE pure #-}
    _ <*> _ = Proxy
    {-# INLINE (<*>) #-}

instance Alternative Proxy where
    empty = Proxy
    {-# INLINE empty #-}
    _ <|> _ = Proxy
    {-# INLINE (<|>) #-}

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

instance MonadPlus Proxy where
    mzero = Proxy
    {-# INLINE mzero #-}
    mplus _ _ = Proxy
    {-# INLINE mplus #-}

#if MIN_VERSION_base(4,4,0)
instance MonadZip Proxy where
    mzipWith _ _ _ = Proxy
    {-# INLINE mzipWith #-}
#endif

instance Foldable Proxy where
    foldMap _ _ = mempty
    {-# INLINE foldMap #-}
    fold _ = mempty
    {-# INLINE fold #-}
    foldr _ z _ = z
    {-# INLINE foldr #-}
    foldl _ z _ = z
    {-# INLINE foldl #-}
    foldl1 _ _ = error "foldl1: Proxy"
    {-# INLINE foldl1 #-}
    foldr1 _ _ = error "foldr1: Proxy"
    {-# INLINE foldr1 #-}

instance Traversable Proxy where
    traverse _ _ = pure Proxy
    {-# INLINE traverse #-}
    sequenceA _ = pure Proxy
    {-# INLINE sequenceA #-}
    mapM _ _ = return Proxy
    {-# INLINE mapM #-}
    sequence _ = return Proxy
    {-# INLINE sequence #-}

-- | 'asProxyTypeOf' is a type-restricted version of 'const'.
-- It is usually used as an infix operator, and its typing forces its first
-- argument (which is usually overloaded) to have the same type as the tag
-- of the second.
asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}

-- | A concrete, promotable proxy type, for use at the kind level
-- There are no instances for this because it is intended at the kind level only
data KProxy
#if __GLASGOW_HASKELL__ >= 706
            (t :: *)
#else
            t
#endif
    = KProxy
#if defined(LANGUAGE_DeriveDataTypeable)
  deriving Typeable
#endif
