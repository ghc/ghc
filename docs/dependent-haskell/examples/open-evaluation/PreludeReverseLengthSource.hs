{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module PreludeReverseLengthSource (Reversed, ReversedOnto) where

import qualified GHC.Internal.List as L
import qualified Prelude as P

-- | Refer to the checked equations of Prelude reverse.
type Reversed :: forall a. [a] -> [a]
type Reversed xs = P.reverse xs

-- | Refer to the checked equations of the library's accumulator worker.
type ReversedOnto :: forall a. [a] -> [a] -> [a]
type ReversedOnto xs acc = L.reverseOnto xs acc
