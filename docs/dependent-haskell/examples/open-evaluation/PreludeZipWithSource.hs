{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures, TypeFamilies #-}
module PreludeZipWithSource (Zipped, PairHeads) where

import GHC.Exts (U)
import Prelude (Bool)
import qualified Prelude

type Zipped :: forall a b c. (a %U -> b %U -> c) -> [a] -> [b] -> [c]
type Zipped f xs ys = Prelude.zipWith f xs ys

type family Pair (x :: Bool) (y :: Bool) :: (Bool, Bool) where
    Pair x y = '(x, y)

type PairHeads = Pair
