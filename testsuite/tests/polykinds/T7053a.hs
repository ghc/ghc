{-# LANGUAGE PolyKinds, GADTs, CUSKs #-}

module T7053a where

import Data.Kind (Type)

-- This time with a fully-specified kind signature
data TypeRep (a :: k) :: Type where
   TyApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)

