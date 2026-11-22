{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures, TypeFamilies #-}
module PreludeZipWithSource (Zipped, PairHeads, Minimum) where

import GHC.Exts (U)
import Prelude (Bool)
import qualified Prelude
import qualified PreludeMapLengthSize as S

-- | Name the checked minimum for clients without dependent type syntax.
type Minimum :: S.Size -> S.Size -> S.Size
type Minimum m n = S.minSize m n

type Zipped :: forall a b c. (a %U -> b %U -> c) -> [a] -> [b] -> [c]
type Zipped f xs ys = Prelude.zipWith f xs ys

type family Pair (x :: Bool) (y :: Bool) :: (Bool, Bool) where
    Pair x y = '(x, y)

type PairHeads = Pair
