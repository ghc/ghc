{-# language DataKinds, PolyKinds, GADTs, TypeFamilies, RankNTypes,
             TypeOperators, ConstraintKinds #-}

module DataInstanceKindsDefaults where

import Data.Kind

-- This test checks if we default the kind of the data instance correctly without UnliftedNewtypes
-- or UnliftedDatatypes.
-- Assumptions:
-- If we default the result kind of the data instance to `TYPE r`,
-- then `checkNewDataCon` would through the error since the result kind of the data instance
-- should be `Type` without UnliftedNewtypes or UnliftedDatatypes.

data family A :: k -> k
newtype instance A a = MkA a

data family B :: k -> k
data instance B a = MkB a

data family C :: k -> k
data instance C a where MkC :: a -> C a

data family D :: k -> k
newtype instance D a where MkD :: a -> D a

