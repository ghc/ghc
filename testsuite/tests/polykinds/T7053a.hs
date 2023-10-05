{-# LANGUAGE PolyKinds, GADTs #-}

module T7053a where

import Data.Kind (Type)

type TypeRep :: k -> Type
data TypeRep a where
   TyApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)

