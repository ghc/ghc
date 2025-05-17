{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Example5 where

-- Current behavior
type family F a where
  F (a -> _) = Maybe a

-- With proposed change
{-
type family F' a where
  F' ((a :: Type) -> _) = Maybe a
-}