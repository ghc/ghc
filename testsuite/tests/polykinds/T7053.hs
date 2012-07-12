{-# LANGUAGE PolyKinds #-}

module T7053 where

data TypeRep (a :: k) where
   TyApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)

