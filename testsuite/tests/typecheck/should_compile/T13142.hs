{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module T13142 () where

import Data.Foldable (foldl')
import Data.Proxy
import Data.Word
import Foreign.Storable (Storable, sizeOf)
import GHC.Generics
import qualified Data.Kind as K
import GHC.TypeLits

newtype Type = VarT Name
  deriving Generic

data Name = Name OccName NameFlavour
  deriving Generic

data NameFlavour
   = NameS
   | NameQ ModName
   | NameU !Int
   | NameL !Int
   | NameG PkgName ModName
   deriving Generic

newtype ModName = ModName String
  deriving Generic

newtype PkgName = PkgName String
  deriving Generic

newtype OccName = OccName String
  deriving Generic

instance Store Type
instance Store Name
instance Store NameFlavour
instance Store ModName
instance Store OccName
instance Store PkgName

instance Store Char where
  {-# INLINE size #-}
  size = sizeStorableTy "Foreign.Storable.Storable GHC.Types.Char"

instance Store Int where
  {-# INLINE size #-}
  size = undefined

instance Store a => Store [a] where
    size = sizeSequence
    {-# INLINE size #-}

sizeSequence :: forall a. Store a => Size [a]
sizeSequence = VarSize $ \t ->
    case size :: Size a of
        ConstSize n -> n * (length t) + sizeOf (undefined :: Int)
        VarSize f -> foldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeSequence #-}

class Store a where
    size :: Size a

    default size :: (Generic a, GStoreSize (Rep a)) => Size a
    size = genericSize
    {-# INLINE size #-}

data Size a
    = VarSize (a -> Int)
    | ConstSize !Int

getSizeWith :: Size a -> a -> Int
getSizeWith (VarSize f) x = f x
getSizeWith (ConstSize n) _ = n
{-# INLINE getSizeWith #-}

contramapSize :: (a -> b) -> Size b -> Size a
contramapSize f (VarSize g) = VarSize (g . f)
contramapSize _ (ConstSize n) = ConstSize n
{-# INLINE contramapSize #-}

combineSizeWith :: forall a b c. (c -> a) -> (c -> b) -> Size a -> Size b -> Size c
combineSizeWith toA toB sizeA sizeB =
    case (sizeA, sizeB) of
        (VarSize f, VarSize g) -> VarSize (\x -> f (toA x) + g (toB x))
        (VarSize f, ConstSize m) -> VarSize (\x -> f (toA x) + m)
        (ConstSize n, VarSize g) -> VarSize (\x -> n + g (toB x))
        (ConstSize n, ConstSize m) -> ConstSize (n + m)
{-# INLINE combineSizeWith #-}

sizeStorableTy :: forall a. Storable a => String -> Size a
sizeStorableTy ty = ConstSize (sizeOf (error msg :: a))
  where
    msg = "In Data.Store.storableSize: " ++ ty ++ "'s sizeOf evaluated its argument."
{-# INLINE sizeStorableTy #-}

genericSize :: (Generic a, GStoreSize (Rep a)) => Size a
genericSize = contramapSize from gsize
{-# INLINE genericSize #-}

type family SumArity (a :: K.Type -> K.Type) :: Nat where
    SumArity (C1 c a) = 1
    SumArity (x :+: y) = SumArity x + SumArity y

class GStoreSize f where gsize :: Size (f a)

instance GStoreSize f => GStoreSize (M1 i c f) where
    gsize = contramapSize unM1 gsize
    {-# INLINE gsize #-}

instance Store a => GStoreSize (K1 i a) where
    gsize = contramapSize unK1 size
    {-# INLINE gsize #-}

instance GStoreSize U1 where
    gsize = ConstSize 0
    {-# INLINE gsize #-}

instance GStoreSize V1 where
    gsize = ConstSize 0
    {-# INLINE gsize #-}

instance (GStoreSize a, GStoreSize b) => GStoreSize (a :*: b) where
    gsize = combineSizeWith (\(x :*: _) -> x) (\(_ :*: y) -> y) gsize gsize
    {-# INLINE gsize #-}

instance (SumArity (a :+: b) <= 255, GStoreSizeSum 0 (a :+: b))
         => GStoreSize (a :+: b) where
    gsize = VarSize $ \x -> sizeOf (undefined :: Word8) + gsizeSum x (Proxy :: Proxy 0)
    {-# INLINE gsize #-}

class KnownNat n => GStoreSizeSum (n :: Nat) (f :: K.Type -> K.Type) where
    gsizeSum :: f a -> Proxy n -> Int

instance (GStoreSizeSum n a, GStoreSizeSum (n + SumArity a) b, KnownNat n)
         => GStoreSizeSum n (a :+: b) where
    gsizeSum (L1 l) _ = gsizeSum l (Proxy :: Proxy n)
    gsizeSum (R1 r) _ = gsizeSum r (Proxy :: Proxy (n + SumArity a))
    {-# INLINE gsizeSum #-}

instance (GStoreSize a, KnownNat n) => GStoreSizeSum n (C1 c a) where
    gsizeSum x _ = getSizeWith gsize x
    {-# INLINE gsizeSum #-}
