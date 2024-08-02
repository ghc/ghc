
{-# LANGUAGE GADTs, RankNTypes, PolyKinds, TypeApplications,
             TypeAbstractions, ScopedTypeVariables #-}

-- Derived from T18986a by replacing tyvar binders with wildcards
module T23501b where

import Data.Kind (Type)

data T where
  MkT :: forall (f :: forall k. k -> Type).
    f Int -> f Maybe -> T

g1 :: T -> ()
g1 (MkT @(_ :: forall k. k -> Type) _ _) = ()

g2 :: T -> ()
g2 (MkT @_ _ _) = ()
