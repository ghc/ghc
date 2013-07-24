{-# LANGUAGE PolyKinds, DeriveDataTypeable, NoImplicitPrelude,
             DeriveGeneric #-}

module Data.Proxy
  (
        Proxy(..), KProxy(..)
  ) where

import Data.Data
import Data.Monoid
import Data.Traversable
import Data.Foldable

import Control.Applicative

import GHC.Base
import GHC.Show
import GHC.Read
import GHC.Enum
import GHC.Arr
import qualified GHC.Generics as Generics

-- | A concrete, poly-kinded proxy type
data Proxy t = Proxy
  deriving (Typeable, Generics.Generic)

-- | A concrete, promotable proxy type, for use at the kind level
-- There are no instances for this because it is intended at the kind level only
data KProxy (t :: *) = KProxy

instance Eq (Proxy s) where
  _ == _ = True

instance Ord (Proxy s) where
  compare _ _ = EQ

instance Show (Proxy s) where
  showsPrec _ _ = showString "Proxy"

instance Read (Proxy s) where
  readsPrec d = readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])

proxyConstr :: Constr
proxyConstr = mkConstr proxyDataType "Proxy" [] Prefix

proxyDataType :: DataType
proxyDataType = mkDataType "Data.Proxy.Proxy" [proxyConstr]

instance (Data t) => Data (Proxy t) where
  gfoldl _ z Proxy  = z Proxy
  toConstr Proxy  = proxyConstr
  gunfold _ z c = case constrIndex c of
                    1 -> z Proxy
                    _ -> error "Data.Data.gunfold(Proxy)"
  dataTypeOf _ = proxyDataType
  dataCast1 f  = gcast1 f

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

instance Applicative Proxy where
    pure _ = Proxy
    {-# INLINE pure #-}
    _ <*> _ = Proxy
    {-# INLINE (<*>) #-}

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