{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
module T28123 where
import GHC.TypeNats
import Data.Kind

f1 :: Int -> forall (n :: Natural) -> Int -> Int
f1 x n y = x
{-# INLINE f1 #-}

f2 :: forall a . Int -> forall (b :: Type) -> a -> a
f2 x t y = y
{-# INLINE f2 #-}

f3 :: forall a . forall (b :: Type) -> a -> a
f3 t = id
{-# INLINE f3 #-}

f4 :: forall a . forall (b :: Type) -> a -> a
f4 t y = y
{-# INLINE f4 #-}

f5 :: Int -> forall (n :: Natural) -> KnownNat n => Int
f5 x _ = x
{-# INLINE f5 #-}

