{-# language DataKinds, PolyKinds, GADTs, TypeFamilies, RankNTypes,
             TypeOperators, ConstraintKinds, UnliftedNewtypes #-}

module T25611a where

import Data.Kind

-- Enhanced kind inference for data family instance in !13767
-- this is the h98 newtype instance case

data family Fix0 :: (k -> Type) -> k
newtype instance Fix0 f = In0 { out0 :: f (Fix0 f) }

-- This is the GADT newtype instance case
-- currently not enabled since !9116 (closed) impose `A newtype must not be a GADT`
-- data family Fix2 :: (k -> Type) -> k
-- newtype instance Fix2 f where In2 :: f (Fix2 f) -> Fix2 f
