{-# LANGUAGE DataKinds, UnliftedNewtypes, TypeFamilies, PolyKinds, MagicHash #-}

module T25647 where

import GHC.Exts
import Data.Kind

-- Rejected because in the type signature for In2 we default
-- the runtime-rep variable to LiftedRep, and that makes In2
-- into a GADT
newtype Fix2 f :: TYPE r where
   In2 :: forall ff. ff (Fix2 ff) -> Fix2 ff

-- Rejected for the same reason
type Fix4a :: forall r. (TYPE r -> TYPE r) -> TYPE r
newtype Fix4a f where
  In4a :: ff (Fix4a ff) -> Fix4a ff
