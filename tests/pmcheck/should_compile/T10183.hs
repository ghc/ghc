{-# LANGUAGE GADTs, DataKinds, TypeOperators, UnicodeSyntax #-}

module Foo where

import GHC.TypeLits

data List l t where
     Nil  ∷ List 0 t
     (:-) ∷ t → List l t → List (l+1) t

head' ∷ (1<=l) ⇒ List l t → t
head' (x :- _) = x

data T a where
  TT :: T Bool
  TF :: T Int

f :: T Bool -> Bool
f TT = True

g :: (a ~ Bool) => T a -> Bool
g TT = True
