{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module T23026 where

import Data.Kind (Type)

data Sing (a :: k)
data SingInstance (a :: k) = SingInstance (Sing a)

app :: (Sing a -> SingInstance a) -> Sing a -> SingInstance a
app f x = f x
{-# NOINLINE app #-}

withSomeSing
  :: forall k2 k1 (f :: k2 -> k1 -> Type) a2 a1.
     (Sing a2, Sing a1)
  -> f a2 a1
  -> (forall b2 b1. f b2 b1 -> Int)
  -> Int
withSomeSing (sa2, sa1) x g =
  case app SingInstance sa2 of
    SingInstance _ ->
      case app SingInstance sa1 of
        SingInstance _ -> g x
{-# INLINABLE withSomeSing #-}
