{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module T19622 where

import Data.Kind (Type)

data A
data B

data ElemKind k where
  ElemKindA :: ElemKind A
  ElemKindB :: ElemKind B

class KnownElemKind (xs :: [k]) where
  getKind :: TypedList f xs -> ElemKind k

data TypedList (f :: (k -> Type)) (xs :: [k]) where
  Nil :: TypedList f '[]
  Cons :: f x -> TypedList f xs -> TypedList f (x ': xs)

data Dim (x :: k)

pattern DimA :: forall k (xs :: [k]) . KnownElemKind xs => (k ~ A) => TypedList Dim xs
pattern DimA <- (getKind -> ElemKindA)

{-# COMPLETE DimA #-}
{-# COMPLETE Nil, Cons #-}

f :: forall (xns :: [B]) . TypedList Dim xns -> TypedList Dim xns -> Bool
f Nil Nil = True
f (Cons _ _) (Cons _ _) = True

g :: forall (xns :: [B]) . TypedList Dim xns -> Bool
g Nil = True
g (Cons _ _) = True

h :: forall (xns :: [A]) . TypedList Dim xns -> Bool
h Nil = True
h (Cons _ _) = True

i :: forall (xns :: [A]) . TypedList Dim xns -> TypedList Dim xns -> Bool
i Nil Nil = True
i (Cons _ _) (Cons _ _) = True

j :: forall k (xns :: [k]) . TypedList Dim xns -> TypedList Dim xns -> Bool
j Nil Nil = True
j (Cons _ _) (Cons _ _) = True

l :: forall (xns :: [A]) . KnownElemKind xns => TypedList Dim xns -> Bool
l DimA = True
