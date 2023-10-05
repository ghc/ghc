{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T15305 where

import Data.Void

-- Example 1

data (:+:) f g a = Inl !(f a) | Inr !(g a)

data A
data B

data Foo l where
  Foo :: Foo A

data Bar l where
  Bar :: Bar B

type Sig = Foo :+: Bar

fun :: Sig B -> Int
fun (Inr Bar) = 1

-- Example 2

data GhcPass c
type family XXHsImplicitBndrs x
type instance XXHsImplicitBndrs (GhcPass _) = Void

data HsImplicitBndrs pass
  = UsefulConstructor
  | XHsImplicitBndrs !(XXHsImplicitBndrs pass)

fun2 :: HsImplicitBndrs (GhcPass pass) -> ()
fun2 UsefulConstructor = ()

fun2' :: (p ~ GhcPass pass) => HsImplicitBndrs p -> ()
fun2' UsefulConstructor = ()

-- Example 3

data Abyss = MkAbyss !Abyss

stareIntoTheAbyss :: Abyss -> a
stareIntoTheAbyss x = case x of {}
{-
Alas, this function is marked non-exhaustive, since the way GHC solves
NonVoid constraints at the moment isn't sophisticated enough to handle
recursive strict argument types like MkAbyss exhibits. Maybe some day.
-}
