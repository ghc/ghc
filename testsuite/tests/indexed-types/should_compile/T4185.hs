{-# LANGUAGE DeriveFunctor, StandaloneDeriving, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving #-}
module T4185 where

data family Foo k :: * -> *

------------- Generalised newtype deriving of user class -----------
class Bar f where
	bar :: f a -> Int
        woo :: f a -> f a

instance Bar Maybe where
	bar Nothing = 0
	bar Just{} = 1
        woo x = x

-- Deriving clause
newtype instance Foo Int a = FooInt (Maybe a) deriving (Bar)

-- Standalone deriving
newtype instance Foo Char a = FooChar (Maybe a) 
deriving instance Bar (Foo Char)

{-
dBarMaybe :: Bar Maybe

newtype FooInt a = FooInt (Maybe a)
axiom ax7 a : Foo Int a ~ FooInt a   -- Family axiom
axiom ax7   : FooInt ~ Maybe         -- Newtype axiom

dBarFooInt :: Bar (Foo Int)
dBarFooInt = dBarMaybe |> Bar ax7
-}

------------- Deriving on data types for Functor -----------

-- Deriving clause
data instance Foo Bool a = FB1 a | FB2 a deriving( Functor )

-- Standalone deriving
data instance Foo Float a = FB3 a
deriving instance Functor (Foo Float)


--instance Functor (Foo Bool) where
--  fmap f (FB1 x) = FB1 (f x)
--  fmap f (FB2 y) = FB2 (f y)