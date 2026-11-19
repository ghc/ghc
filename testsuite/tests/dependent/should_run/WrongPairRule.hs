{-# LANGUAGE DataKinds, KindSignatures, NoExperimentalDependentTypes, TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module WrongPairRule where

import Data.Proxy (Proxy)
import Numeric.Natural (Natural)
import PreludePairExport (get, secondIndex, swapIndex)

-- The rule below changes the selected field at these two kinds.
bad :: forall (p :: (Natural, Natural)). Proxy p -> ()
bad p = same (get (swapIndex p)) (secondIndex p)
  where
    same :: Proxy (n :: Natural) -> Proxy n -> ()
    same _ _ = ()

{-# RULES "pair-composition/first" [~1] forall a b. Prelude.fst @Natural @Natural (a, b) = b #-}
