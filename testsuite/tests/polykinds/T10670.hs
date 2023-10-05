{-# LANGUAGE ScopedTypeVariables, RankNTypes, GADTs, PolyKinds #-}

module T10670 where

import Unsafe.Coerce
import Data.Kind (Type)

data TypeRepT (a::k) where
  TRCon :: TypeRepT a

data G2 c a where
  G2 :: TypeRepT a -> TypeRepT b -> G2 c (c a b)

getT2 :: TypeRepT (c :: k2 -> k1 -> k) -> TypeRepT (a :: k) -> Maybe (G2 c a)
{-# NOINLINE getT2 #-}
getT2 c t = Nothing

tyRepTArr :: TypeRepT (->)
{-# NOINLINE tyRepTArr #-}
tyRepTArr = TRCon

s :: forall a x. TypeRepT (a :: Type) -> Maybe x
s tf = case getT2 tyRepTArr tf :: Maybe (G2 (->) a) of
          Just (G2 _ _) -> Nothing
          _ -> Nothing
