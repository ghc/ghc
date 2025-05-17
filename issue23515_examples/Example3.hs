{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Example3 where

-- Current behavior
type family N' a where
  N' (t a) = [a]
  N' a     = ()

-- With proposed change
{-
type family N'' a where
  N'' (t (a :: Type)) = [a]
  N'' a               = ()
-}