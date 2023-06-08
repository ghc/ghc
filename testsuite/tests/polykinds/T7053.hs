{-# LANGUAGE PolyKinds, GADTs #-}

module T7053 where

-- No standalone kind signature or return kind annotation.
-- We are testing that GHC does not panic without either of those.
data TypeRep (a :: k) where
   TyApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)

