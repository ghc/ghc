{-# LANGUAGE TypeFamilies, GADTs, RankNTypes, ScopedTypeVariables #-}

module ShouldFail where

type family Const a
type instance Const a = ()

data T a where T :: a -> T (Const a)

coerce :: forall a b . a -> b
coerce x = case T x :: T (Const b) of
           T y -> y
