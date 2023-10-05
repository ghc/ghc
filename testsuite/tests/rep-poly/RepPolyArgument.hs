{-# LANGUAGE ExplicitForAll, PolyKinds, StandaloneKindSignatures, TypeApplications, TypeFamilies #-}

module RepPolyArgument where

import GHC.Exts

type R :: forall k. k
data family R

foo = undefined (undefined @(R @RuntimeRep))
