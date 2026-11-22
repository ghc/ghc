{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module PreludeZipSource (ZipPairs) where

import qualified Prelude

type ZipPairs :: forall a b. [a] -> [b] -> [(a, b)]
type ZipPairs xs ys = Prelude.zip xs ys
