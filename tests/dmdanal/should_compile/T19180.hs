{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module T14270 (mkTrApp) where

import Data.Kind (Type)

data TypeRep a = TypeRep

mkTrApp :: TypeRep a -> TypeRep a
mkTrApp (x :: TypeRep x)
  | Just _ <- isTYPE (typeRepKind x)
  = undefined
mkTrApp x = TypeRep

typeRepKind :: TypeRep (a :: k) -> TypeRep k
typeRepKind = if sum [0..100] == 10 then undefined else const TypeRep

isTYPE :: TypeRep (a :: Type) -> Maybe a
isTYPE _ = if sum [0..100] == 10 then Nothing else undefined
{-# NOINLINE isTYPE #-}
