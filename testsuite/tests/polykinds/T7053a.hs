{-# LANGUAGE PolyKinds, GADTs #-}

module T7053a where

-- This time with a fully-specified kind signature
data TypeRep (a :: k) :: * where
   TyApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)

